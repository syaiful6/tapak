#!/usr/bin/env bash
# Whole-stack HTTP benchmark: tapak-server-bench driven by zrk.
#
# For every scenario it records, per run:
#   rps              achieved requests/second (zrk)
#   p50_us, p99_us   latency (zrk; coordinated-omission corrected with -R)
#   cpu_ns_per_req   server CPU time per request   (perf task-clock / requests)
#   cycles_per_req   server user cycles per request (perf cycles:u / requests)
#   bytes_per_req    server allocation per request  (Gc counters / requests)
# and writes the median over runs to <outdir>/<label>.json.
#
# The "control" scenario hits a handler that skips the router, so
#   router cost ~= metric(scenario) - metric(control).
#
# Compare two results with bench/compare.sh. To compare commits, build the
# other commit's server in a worktree and pass both binaries; runs are
# interleaved (A B A B ...) so drift affects both equally:
#
#   git worktree add ../tapak-base main
#   (cd ../tapak-base && dune build --profile release bench/server_bench.exe)
#   dune build --profile release bench/server_bench.exe
#   bench/http_bench.sh \
#     -b base=../tapak-base/_build/default/bench/server_bench.exe \
#     -b head=_build/default/bench/server_bench.exe
#   bench/compare.sh bench/results/base.json bench/results/head.json

set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 [options]
  -b LABEL=PATH   server binary to benchmark; repeatable. Default: build this
                  tree in release, labelled with the git short sha
  -r RUNS         measured runs per binary, interleaved (default 3)
  -d SECONDS      measured duration per scenario (default 15)
  -w SECONDS      discarded warm-up per scenario (default 3)
  -c CONNS        zrk connections (default 64)
  -t THREADS      zrk threads (default: number of client CPUs)
  -D DOMAINS      server domains (default: number of server CPUs)
  -R RATE         fixed-rate mode at RATE req/s, for latency comparisons
                  (default: closed loop, which finds maximum throughput)
  -s REGEX        only run scenarios whose name matches REGEX
  -o DIR          results directory (default bench/results)
  -p PORT         base port; uses PORT..PORT+2 (default 18080)
  -S CPUS         CPUs for the server, taskset list (default: first half)
  -C CPUS         CPUs for zrk, taskset list (default: second half)
  -h              show this help
EOF
}

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

binaries=()
runs=3
duration=15
warmup=3
conns=64
threads=""
domains=""
rate=""
filter=""
outdir=bench/results
port=18080

ncpu=$(nproc)
half=$((ncpu / 2))
if ((ncpu >= 2)); then
  server_cpus="0-$((half - 1))"
  client_cpus="$half-$((ncpu - 1))"
else
  server_cpus=""
  client_cpus=""
fi

while getopts "b:r:d:w:c:t:D:R:s:o:p:S:C:h" opt; do
  case $opt in
    b) binaries+=("$OPTARG") ;;
    r) runs=$OPTARG ;;
    d) duration=$OPTARG ;;
    w) warmup=$OPTARG ;;
    c) conns=$OPTARG ;;
    t) threads=$OPTARG ;;
    D) domains=$OPTARG ;;
    R) rate=$OPTARG ;;
    s) filter=$OPTARG ;;
    o) outdir=$OPTARG ;;
    p) port=$OPTARG ;;
    S) server_cpus=$OPTARG ;;
    C) client_cpus=$OPTARG ;;
    h) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done

cpu_count() { # count CPUs in a taskset list such as "0-1,4"
  local n=0 part
  IFS=, read -ra parts <<<"$1"
  for part in "${parts[@]}"; do
    if [[ $part == *-* ]]; then n=$((n + ${part#*-} - ${part%-*} + 1)); else n=$((n + 1)); fi
  done
  echo "$n"
}

if [[ -n $server_cpus ]]; then
  : "${domains:=$(cpu_count "$server_cpus")}"
  : "${threads:=$(cpu_count "$client_cpus")}"
fi
: "${domains:=1}"
: "${threads:=2}"

for tool in zrk jq curl; do
  command -v "$tool" >/dev/null || { echo "error: $tool not found" >&2; exit 1; }
done
have_perf=0
if command -v perf >/dev/null && perf stat -e cycles:u -x, true >/dev/null 2>&1; then
  have_perf=1
else
  echo "warning: perf unavailable; cpu_ns_per_req and cycles_per_req will be null" >&2
fi

pin() { # run "$@" on the given CPU list, if any
  local cpus=$1; shift
  if [[ -n $cpus ]]; then taskset -c "$cpus" "$@"; else "$@"; fi
}

if ((${#binaries[@]} == 0)); then
  echo "building server_bench (release)..." >&2
  dune build --profile release bench/server_bench.exe 2>/dev/null
  label=$(git rev-parse --short HEAD)
  git diff --quiet HEAD -- src pkg bench 2>/dev/null || label="$label-dirty"
  binaries=("$label=_build/default/bench/server_bench.exe")
fi
for spec in "${binaries[@]}"; do
  [[ $spec == *=* && -x ${spec#*=} ]] || { echo "error: bad -b '$spec' (want LABEL=PATH to an executable)" >&2; exit 2; }
done

# Scenarios: name | port offset | method | path | expected status | headers?
# Path scenarios mirror route_bench; input/* send the same headers, cookie and
# query as route_bench's input benchmarks.
query="q=alice&limit=25&offset=10"
scenarios=(
  "control|1|GET|/|200|0"
  "path/static|0|GET|/people|200|0"
  "path/2-params|0|GET|/people/me/activities/public|200|0"
  "path/203-route-trie|0|GET|/repos/julienschmidt/httprouter/pulls/1/files|200|0"
  "input/baseline|0|GET|/baseline/users?$query|200|1"
  "input/query|0|GET|/query/users?$query|200|1"
  "input/headers|0|GET|/headers/users?$query|200|1"
  "input/cookies|0|GET|/cookies/users?$query|200|1"
  "input/all|0|GET|/all/users?$query|200|1"
  "miss/404|0|GET|/no/such/route|404|0"
  "miss/405|0|DELETE|/people|405|0"
)
input_headers=(
  "Accept: application/json"
  "Accept-Encoding: gzip, deflate, br"
  "Accept-Language: en-US,en;q=0.9"
  "User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20100101"
  "X-Forwarded-For: 203.0.113.7"
  "X-Api-Key: secret-key"
  "X-Request-Id: request-123"
  "Cookie: session=abcdef0123456789; theme=dark"
)

selected=()
for sc in "${scenarios[@]}"; do
  name=${sc%%|*}
  if [[ -z $filter || $name =~ $filter ]]; then selected+=("$sc"); fi
done
((${#selected[@]} > 0)) || { echo "error: no scenario matches '$filter'" >&2; exit 2; }

work=$(mktemp -d)
server_pid=""
cleanup() {
  if [[ -n $server_pid ]]; then kill "$server_pid" 2>/dev/null || true; wait "$server_pid" 2>/dev/null || true; fi
  rm -rf "$work"
}
trap cleanup EXIT

port_in_use() {
  local p
  for p in "$port" $((port + 1)) $((port + 2)); do
    (exec 3<>"/dev/tcp/127.0.0.1/$p") 2>/dev/null && return 0
  done
  return 1
}

start_server() {
  local bin=$1
  # Started directly, not through [pin]: a backgrounded shell function runs in
  # a subshell, and $! would then be the subshell, not the server that perf
  # must attach to. taskset execs the server, so $! is the server here.
  if port_in_use; then
    echo "error: port $port..$((port + 2)) already in use; is another server running?" >&2
    exit 1
  fi
  if [[ -n $server_cpus ]]; then
    taskset -c "$server_cpus" "$bin" --port "$port" --domains "$domains" >"$work/server.log" 2>&1 &
  else
    "$bin" --port "$port" --domains "$domains" >"$work/server.log" 2>&1 &
  fi
  server_pid=$!
  for _ in $(seq 50); do
    curl -sf "http://127.0.0.1:$((port + 2))/gc" >/dev/null 2>&1 && return 0
    kill -0 "$server_pid" 2>/dev/null || break
    sleep 0.1
  done
  echo "error: server did not start:" >&2; cat "$work/server.log" >&2; exit 1
}

stop_server() {
  kill "$server_pid" 2>/dev/null || true
  wait "$server_pid" 2>/dev/null || true
  server_pid=""
  for _ in $(seq 50); do port_in_use || return 0; sleep 0.1; done
  echo "error: server on port $port did not shut down" >&2
  exit 1
}

gc_bytes() { curl -sf "http://127.0.0.1:$((port + 2))/gc" | jq '.allocated_bytes'; }

# Fails loudly if a scenario does not return its expected status, so a broken
# route is never benchmarked as a fast error path.
check_status() {
  local name=$1 url=$2 method=$3 expected=$4 with_headers=$5 args=() h code
  if ((with_headers)); then for h in "${input_headers[@]}"; do args+=(-H "$h"); done; fi
  code=$(curl -s -o /dev/null -w '%{http_code}' -X "$method" "${args[@]}" "$url")
  [[ $code == "$expected" ]] || { echo "error: $name: expected $expected, got $code ($method $url)" >&2; exit 1; }
}

zrk_args() { # common zrk arguments for one scenario
  local method=$1 with_headers=$2 h
  printf '%s\0' -t "$threads" -c "$conns" -m "$method" --plain
  if [[ -n $rate ]]; then printf '%s\0' -R "$rate"; else printf '%s\0' --closed; fi
  if ((with_headers)); then for h in "${input_headers[@]}"; do printf '%s\0' -H "$h"; done; fi
}

measure() { # label run scenario -> appends one NDJSON row to $work/rows.ndjson
  local label=$1 run=$2 sc=$3
  local name off method path expected with_headers url args
  IFS='|' read -r name off method path expected with_headers <<<"$sc"
  url="http://127.0.0.1:$((port + off))$path"
  check_status "$name" "$url" "$method" "$expected" "$with_headers"
  mapfile -d '' args < <(zrk_args "$method" "$with_headers")

  pin "$client_cpus" zrk "${args[@]}" -d "${warmup}s" -o /dev/null "$url" >/dev/null 2>&1 || true

  local before after perf_pid="" cycles=null task_ms=null
  before=$(gc_bytes)
  if ((have_perf)); then
    perf stat -x, -e cycles:u,task-clock -p "$server_pid" -o "$work/perf.csv" -- sleep "$duration" 2>"$work/perf.err" &
    perf_pid=$!
  fi
  local status=0
  pin "$client_cpus" zrk "${args[@]}" -d "${duration}s" --format json -o "$work/zrk.json" "$url" >/dev/null 2>&1 || status=$?
  if [[ -n $perf_pid ]]; then
    wait "$perf_pid" || true
    cycles=$(awk -F, '$3 ~ /^cycles/ {print $1}' "$work/perf.csv")
    task_ms=$(awk -F, '$3 ~ /^task-clock/ {print $1}' "$work/perf.csv")
    [[ $cycles =~ ^[0-9.]+$ ]] || cycles=null
    [[ $task_ms =~ ^[0-9.]+$ ]] || task_ms=null
  fi
  after=$(gc_bytes)
  [[ -s $work/zrk.json ]] || { echo "error: zrk failed for $name (exit $status)" >&2; exit 1; }

  jq -c \
    --arg label "$label" --argjson run "$run" --arg name "$name" \
    --argjson expected "$expected" \
    --argjson before "$before" --argjson after "$after" \
    --argjson cycles "$cycles" --argjson task_ms "$task_ms" '
    def per_req(x): if x == null or .requests == 0 then null else x / .requests end;
    {label: $label, run: $run, scenario: $name,
     requests: .requests,
     rps: .achieved_rate,
     p50_us: .latency_us.p50,
     p99_us: .latency_us.p99,
     cpu_ns_per_req: per_req(if $task_ms == null then null else $task_ms * 1e6 end),
     cycles_per_req: per_req($cycles),
     bytes_per_req: per_req($after - $before),
     unexpected: (.requests
                  - (if $expected < 300 then .status_codes["2xx"] else .status_codes["4xx"] end)
                  + ([.errors | to_entries[] | select(.key != "non_2xx_3xx") | .value] | add))}
    ' "$work/zrk.json" >>"$work/rows.ndjson"

  jq -r '"  \(.scenario | .+ " " * (22 - length)) \(.rps | floor) req/s  p99 \(.p99_us)us  "
          + "cpu \(.cpu_ns_per_req // "-" | if type == "number" then floor else . end) ns/req  "
          + "\(.bytes_per_req | floor) B/req"
          + (if .unexpected > 0 then "  UNEXPECTED: \(.unexpected)" else "" end)' \
    <<<"$(tail -n1 "$work/rows.ndjson")"
}

echo "server cpus=${server_cpus:-any} domains=$domains | zrk cpus=${client_cpus:-any} threads=$threads conns=$conns" \
  "| ${rate:+rate=$rate req/s}${rate:-closed loop} | ${duration}s x $runs runs (+${warmup}s warm-up)" >&2
gov=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor 2>/dev/null || echo unknown)
[[ $gov == performance ]] || echo "note: CPU governor is '$gov'; 'performance' gives steadier numbers" >&2

for run in $(seq "$runs"); do
  for spec in "${binaries[@]}"; do
    label=${spec%%=*}
    echo "run $run/$runs: $label" >&2
    start_server "${spec#*=}"
    for sc in "${selected[@]}"; do measure "$label" "$run" "$sc"; done
    stop_server
  done
done

mkdir -p "$outdir"
for spec in "${binaries[@]}"; do
  label=${spec%%=*}
  jq -s \
    --arg label "$label" --arg binary "${spec#*=}" \
    --arg sha "$(git rev-parse HEAD)" --arg date "$(date -Iseconds)" \
    --arg kernel "$(uname -r)" \
    --arg cpu "$(awk -F': ' '/model name/ {print $2; exit}' /proc/cpuinfo)" \
    --arg governor "$gov" --arg zrk "$(zrk --version 2>&1)" \
    --arg ocamlrunparam "${OCAMLRUNPARAM:-}" \
    --argjson runs "$runs" --argjson duration "$duration" --argjson conns "$conns" \
    --argjson threads "$threads" --argjson domains "$domains" \
    --arg rate "$rate" --arg server_cpus "$server_cpus" --arg client_cpus "$client_cpus" '
    def median: map(select(. != null)) | sort | if length == 0 then null else .[length / 2 | floor] end;
    def spread: map(select(. != null)) | if length < 2 then 0 else (max - min) / (sort | .[length / 2 | floor]) end;
    map(select(.label == $label)) as $rows
    | {meta: {label: $label, binary: $binary, git_sha: $sha, date: $date,
              kernel: $kernel, cpu: $cpu, governor: $governor, zrk: $zrk,
              ocamlrunparam: $ocamlrunparam,
              runs: $runs, duration_s: $duration, connections: $conns,
              zrk_threads: $threads, server_domains: $domains,
              mode: (if $rate == "" then "closed" else "rate" end),
              rate: (if $rate == "" then null else ($rate | tonumber) end),
              server_cpus: $server_cpus, client_cpus: $client_cpus},
       scenarios: ($rows
         | reduce (.[].scenario) as $n ([]; if index([$n]) then . else . + [$n] end)
         | map(. as $n | $rows | map(select(.scenario == $n)))
         | map({
         key: .[0].scenario,
         value: {rps: (map(.rps) | median),
                 rps_spread: (map(.rps) | spread),
                 p50_us: (map(.p50_us) | median),
                 p99_us: (map(.p99_us) | median),
                 cpu_ns_per_req: (map(.cpu_ns_per_req) | median),
                 cycles_per_req: (map(.cycles_per_req) | median),
                 bytes_per_req: (map(.bytes_per_req) | median),
                 unexpected: (map(.unexpected) | add),
                 runs: map(del(.label, .scenario))}}) | from_entries)}
    ' "$work/rows.ndjson" >"$outdir/$label.json"
  echo "wrote $outdir/$label.json" >&2
done

# Summary of the last label, with the router's share over the control floor.
last=${binaries[-1]%%=*}
jq -r '
  .scenarios as $s
  | ($s.control // null) as $c
  | def f(x): if x == null then "-" else (x | floor | tostring) end;
    def d(x; y): if x == null or y == null then "-" else ((x - y) | floor | tostring) end;
    ["scenario", "req/s", "spread", "p50us", "p99us", "cpu ns", "cycles", "B/req", "cpu ns-ctl", "B-ctl"],
    ($s | to_entries[] | .key as $k | .value as $v |
      [$k, f($v.rps), "\($v.rps_spread * 100 | floor)%", f($v.p50_us), f($v.p99_us),
       f($v.cpu_ns_per_req), f($v.cycles_per_req), f($v.bytes_per_req),
       d($v.cpu_ns_per_req; $c.cpu_ns_per_req), d($v.bytes_per_req; $c.bytes_per_req)])
  | @tsv' "$outdir/$last.json" | column -t -s $'\t'

failed=0
for spec in "${binaries[@]}"; do
  f="$outdir/${spec%%=*}.json"
  if jq -e '[.scenarios[].unexpected] | any(. > 0)' "$f" >/dev/null; then
    echo "error: requests returned an unexpected status or failed; see 'unexpected' in $f" >&2
    failed=1
  fi
done
exit "$failed"
