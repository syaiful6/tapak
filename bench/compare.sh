#!/usr/bin/env bash
# Compare two bench/http_bench.sh results and flag regressions.
#
#   bench/compare.sh [-t RPS_PCT] [-l P99_PCT] [-u CPU_PCT] BASE.json NEW.json
#
# Thresholds are percentages (defaults: req/s 5, p99 10, cpu ns/req 5). A
# change only counts as a regression when it exceeds the threshold AND the
# run-to-run spread of both results, so noisy scenarios are reported as such
# instead of failing. Bytes/request is deterministic, so any increase of more
# than 1% is flagged. Exits 1 if any regression was found.
#
# For the numbers to mean anything, both results should come from the same
# machine and, ideally, the same interleaved http_bench.sh invocation.

set -euo pipefail

rps_pct=5
p99_pct=10
cpu_pct=5
while getopts "t:l:u:h" opt; do
  case $opt in
    t) rps_pct=$OPTARG ;;
    l) p99_pct=$OPTARG ;;
    u) cpu_pct=$OPTARG ;;
    h) sed -n '2,13p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))
(($# == 2)) || { echo "usage: $0 [-t RPS_PCT] [-l P99_PCT] [-u CPU_PCT] BASE.json NEW.json" >&2; exit 2; }
base=$1
new=$2

table=$(jq -n -r --slurpfile b "$base" --slurpfile n "$new" \
  --argjson rps_pct "$rps_pct" --argjson p99_pct "$p99_pct" --argjson cpu_pct "$cpu_pct" '
  $b[0] as $b | $n[0] as $n
  | def pct(old; new): if old == null or new == null or old == 0 then null else (new - old) / old * 100 end;
    def fmt(x): if x == null then "-" else ((x * 10 | round) / 10 | tostring) + "%" end;
    # higher_is_better: rps. For the others an increase is worse.
    def verdict(p; limit; noise; higher_is_better):
      if p == null then ""
      elif (if higher_is_better then -p else p end) > limit then
        (if (p | fabs) <= noise then "noisy" else "REGRESSION" end)
      elif (if higher_is_better then p else -p end) > limit and (p | fabs) > noise then "improved"
      else "" end;
    ($b.meta | "base: \(.label) (\(.git_sha[0:10]), \(.date))"),
    ($n.meta | "new:  \(.label) (\(.git_sha[0:10]), \(.date))"),
    (if ($b.meta | del(.label, .binary, .git_sha, .date))
        != ($n.meta | del(.label, .binary, .git_sha, .date))
     then "warning: results were taken with different settings or on a different machine" else empty end),
    "",
    ("\u0001" + (["scenario", "req/s", "Δ", "p99us", "Δ", "cpu ns/req", "Δ", "B/req", "Δ", "verdict"] | @tsv)),
    ([$n.scenarios | to_entries[] | .key as $k | .value as $v | ($b.scenarios[$k] // null) as $o
      | select($o != null)
      | (([$o.rps_spread, $v.rps_spread] | max) * 100) as $noise
      | pct($o.rps; $v.rps) as $rps
      | pct($o.p99_us; $v.p99_us) as $p99
      | pct($o.cpu_ns_per_req; $v.cpu_ns_per_req) as $cpu
      | pct($o.bytes_per_req; $v.bytes_per_req) as $bytes
      | [ verdict($rps; $rps_pct; $noise; true),
          verdict($p99; $p99_pct; $noise; false),
          verdict($cpu; $cpu_pct; $noise; false),
          (if $bytes != null and $bytes > 1 then "REGRESSION"
           elif $bytes != null and $bytes < -1 then "improved" else "" end) ] as $vs
      | [ $k, ($v.rps | floor), fmt($rps), $v.p99_us, fmt($p99),
          ($v.cpu_ns_per_req // null | if . == null then "-" else floor end), fmt($cpu),
          ($v.bytes_per_req | floor), fmt($bytes),
          (if any($vs[]; . == "REGRESSION") then "REGRESSION"
           elif any($vs[]; . == "noisy") then "noisy"
           elif any($vs[]; . == "improved") then "improved" else "ok" end) ]
     ] | .[] | "\u0001" + @tsv)
' | awk '/^\001/ { print substr($0, 2) | "column -t -s \"\t\""; next } { print }')
echo "$table"
if grep -q REGRESSION <<<"$table"; then exit 1; fi
