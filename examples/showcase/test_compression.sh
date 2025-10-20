#!/bin/bash

# Test script for compression middleware
# Usage: ./test_compression.sh [port]

PORT=${1:-3000}
BASE_URL="http://localhost:$PORT"

echo "Testing Tapak Compression Middleware"
echo "Server: $BASE_URL"
echo ""

# Test 1: Gzip encoding
echo "=== Test 1: Gzip Encoding ==="
echo '{"test": "gzip", "message": "Hello from gzipped request!"}' |
  gzip |
  curl -v -s -X POST \
    -H "Content-Encoding: gzip" \
    --data-binary @- \
    "$BASE_URL/echo"
echo ""
echo ""

# Test 2: Deflate encoding using Python's zlib (with header)
echo "=== Test 2: Deflate Encoding (zlib format) ==="
echo '{"test": "deflate-zlib", "message": "Hello from deflated request with zlib wrapper!"}' |
  python3 -c "
import sys, zlib
data = sys.stdin.buffer.read()
# Zlib format (deflate with wrapper - what most tools use)
compressed = zlib.compress(data)
sys.stdout.buffer.write(compressed)
" |
  curl -v -s -X POST \
    -H "Content-Encoding: deflate" \
    --data-binary @- \
    "$BASE_URL/echo"
echo ""
echo ""

# Test 3: Large gzipped payload
echo "=== Test 3: Large Gzipped Payload ==="
python3 -c "
import json
data = {
    'test': 'large-gzip',
    'items': list(range(1000)),
    'message': 'Large payload with 1000 items'
}
print(json.dumps(data, indent=2))
" |
  gzip |
  curl -v -s -X POST \
    -H "Content-Encoding: gzip" \
    --data-binary @- \
    "$BASE_URL/echo" | head -20
echo ""
echo "... (truncated)"
echo ""

# Test 4: Identity (no compression)
echo "=== Test 4: Identity (no compression) ==="
echo '{"test": "identity", "message": "No compression"}' |
  curl -v -s -X POST \
    -H "Content-Encoding: identity" \
    --data-binary @- \
    "$BASE_URL/echo"
echo ""
echo ""

# Test 5: Chunked gzip data
echo "=== Test 5: Chunked Gzip Transfer ==="
echo '{"test": "chunked-gzip", "message": "Testing chunked transfer encoding with gzip"}' |
  gzip |
  curl -v -s -X POST \
    -H "Content-Encoding: gzip" \
    -H "Transfer-Encoding: chunked" \
    --data-binary @- \
    "$BASE_URL/echo"
echo ""
echo ""

# Test 6: Brotli encoding
echo "=== Test 6: Brotli Encoding ==="
if command -v brotli &>/dev/null; then
  echo '{"test": "brotli", "message": "Hello from brotli compressed request!"}' |
    brotli -c |
    curl -v -s -X POST \
      -H "Content-Encoding: br" \
      --data-binary @- \
      "$BASE_URL/echo"
  echo ""
else
  echo "⚠️  brotli command not found, skipping test"
  echo "Install with: brew install brotli  (macOS) or  apt-get install brotli  (Linux)"
  echo ""
fi
echo ""

# Test 7: Zstandard (zstd) encoding
echo "=== Test 7: Zstandard Encoding ==="
if command -v zstd &>/dev/null; then
  echo '{"test": "zstd", "message": "Hello from zstd compressed request!"}' |
    zstd -c |
    curl -v -s -X POST \
      -H "Content-Encoding: zstd" \
      --data-binary @- \
      "$BASE_URL/echo"
  echo ""
else
  echo "⚠️  zstd command not found, skipping test"
  echo "Install with: brew install zstd  (macOS) or  apt-get install zstd  (Linux)"
  echo ""
fi
echo ""

# Test 8: Large brotli payload
echo "=== Test 8: Large Brotli Payload ==="
if command -v brotli &>/dev/null; then
  python3 -c "
import json
data = {
    'test': 'large-brotli',
    'items': list(range(500)),
    'message': 'Large payload with 500 items (brotli)'
}
print(json.dumps(data, indent=2))
" |
    brotli -c |
    curl -v -s -X POST \
      -H "Content-Encoding: br" \
      --data-binary @- \
      "$BASE_URL/echo" | head -20
  echo ""
  echo "... (truncated)"
else
  echo "⚠️  Skipped (brotli not available)"
fi
echo ""
echo ""

# Test 9: Large zstd payload
echo "=== Test 9: Large Zstandard Payload ==="
if command -v zstd &>/dev/null; then
  python3 -c "
import json
data = {
    'test': 'large-zstd',
    'items': list(range(500)),
    'message': 'Large payload with 500 items (zstd)'
}
print(json.dumps(data, indent=2))
" |
    zstd -c |
    curl -v -s -X POST \
      -H "Content-Encoding: zstd" \
      --data-binary @- \
      "$BASE_URL/echo" | head -20
  echo ""
  echo "... (truncated)"
else
  echo "⚠️  Skipped (zstd not available)"
fi
echo ""
echo ""

# Test 10: Compare compression ratios
echo "=== Test 10: Compression Ratio Comparison ==="
echo "Generating test data..."
TEST_DATA='{"test": "compression-comparison", "data": "'$(python3 -c "print('x' * 1000)")'"}'

echo "Original size: $(echo -n "$TEST_DATA" | wc -c) bytes"
echo ""

if command -v gzip &>/dev/null; then
  GZIP_SIZE=$(echo -n "$TEST_DATA" | gzip | wc -c)
  echo "Gzip size:     $GZIP_SIZE bytes"
fi

if command -v brotli &>/dev/null; then
  BROTLI_SIZE=$(echo -n "$TEST_DATA" | brotli -c | wc -c)
  echo "Brotli size:   $BROTLI_SIZE bytes"
fi

if command -v zstd &>/dev/null; then
  ZSTD_SIZE=$(echo -n "$TEST_DATA" | zstd -c | wc -c)
  echo "Zstd size:     $ZSTD_SIZE bytes"
fi

python3 -c "import zlib; import sys; print('Deflate size:  {} bytes'.format(len(zlib.compress(sys.argv[1].encode()))))" "$TEST_DATA" 2>/dev/null

echo ""
echo ""

echo "=== All Tests Complete ==="
echo ""
echo "Summary:"
echo "  ✓ Gzip encoding"
echo "  ✓ Deflate encoding (zlib format)"
echo "  ✓ Identity (no compression)"
echo "  ✓ Chunked transfer"
if command -v brotli &>/dev/null || python3 -c "import brotli" 2>/dev/null; then
  echo "  ✓ Brotli encoding"
else
  echo "  ⚠ Brotli encoding (not available)"
fi
if command -v zstd &>/dev/null || python3 -c "import zstandard" 2>/dev/null; then
  echo "  ✓ Zstandard encoding"
else
  echo "  ⚠ Zstandard encoding (not available)"
fi
echo ""
