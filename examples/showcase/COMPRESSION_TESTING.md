# Testing Compression Middleware

This guide shows how to test the compression/decompression middleware with various encoding formats.

## Quick Start

```bash
# Start the showcase server
dune exec --profile=dev tapak-showcase

# In another terminal, run the test script
./examples/showcase/test_compression.sh
```

## Manual Testing

### 1. Gzip Encoding

The most common encoding, used by browsers and curl by default:

```bash
# Simple gzip test
echo '{"message": "Hello!"}' | gzip | \
  curl -X POST \
    -H "Content-Encoding: gzip" \
    --data-binary @- \
    http://localhost:3000/echo

# From a file
gzip -c myfile.json | \
  curl -X POST \
    -H "Content-Encoding: gzip" \
    --data-binary @- \
    http://localhost:3000/echo
```

### 2. Deflate Encoding

Deflate is the raw DEFLATE compressed data format (RFC 1951). However, there's ambiguity: some implementations use raw DEFLATE, others use DEFLATE wrapped in zlib format (RFC 1950).

#### Option A: Raw Deflate (using Python)

```bash
# Raw DEFLATE format (no wrapper)
echo '{"message": "Hello from deflate!"}' | \
  python3 -c "
import sys, zlib
data = sys.stdin.buffer.read()
# Strip zlib header (2 bytes) and trailer (4 bytes) for raw deflate
compressed = zlib.compress(data)[2:-4]
sys.stdout.buffer.write(compressed)
" | \
  curl -X POST \
    -H "Content-Encoding: deflate" \
    --data-binary @- \
    http://localhost:3000/echo
```

#### Option B: Zlib Format (more common in practice)

```bash
# Zlib-wrapped DEFLATE (what most tools actually use for "deflate")
echo '{"message": "Hello from deflate!"}' | \
  python3 -c "
import sys, zlib
data = sys.stdin.buffer.read()
compressed = zlib.compress(data)  # Includes zlib header/footer
sys.stdout.buffer.write(compressed)
" | \
  curl -X POST \
    -H "Content-Encoding: deflate" \
    --data-binary @- \
    http://localhost:3000/echo
```

**Note:** The Tapak implementation uses `Zlib.inflate_init false` which handles zlib-wrapped deflate format (the more common interpretation).

### 3. Testing with Different Tools

#### Using curl with pre-compressed files:

```bash
# Compress a file
gzip -c data.json > data.json.gz

# Send it
curl -X POST \
  -H "Content-Encoding: gzip" \
  --data-binary @data.json.gz \
  http://localhost:3000/echo
```

#### Using httpie (if installed):

```bash
# httpie doesn't support sending compressed data directly
# but you can use --compress flag (though it's for response compression)
```

#### Using Python requests:

```python
import requests
import gzip
import json

data = {"message": "Hello from Python!"}
json_data = json.dumps(data).encode('utf-8')

# Gzip compression
compressed = gzip.compress(json_data)

response = requests.post(
    'http://localhost:3000/echo',
    data=compressed,
    headers={'Content-Encoding': 'gzip'}
)

print(response.text)
```

### 4. Large Payload Testing

Test with a large payload to ensure streaming works:

```bash
# Generate large JSON (1000 items)
python3 -c "
import json
data = {'items': list(range(1000)), 'message': 'Large payload'}
print(json.dumps(data))
" | gzip | \
  curl -X POST \
    -H "Content-Encoding: gzip" \
    --data-binary @- \
    http://localhost:3000/echo
```

## Expected Behavior

When the decompression middleware successfully processes a request:

1. The `Content-Encoding` header is **removed** from the request
2. The `Content-Length` header is **removed** (since the decompressed size differs)
3. The request body is decompressed and available to handlers as plain text
4. The echo handler should show `Content-Encoding: removed after decompression`

## Debugging

If you encounter issues:

1. **Check the server logs** - they will show errors during decompression
2. **Verify compression format**:
   ```bash
   # Check if data is actually gzipped
   echo "test" | gzip | xxd | head -1
   # Should start with: 1f8b (gzip magic bytes)
   ```
3. **Test with known-good data**:
   ```bash
   # Create a reference gzip file
   echo "test" | gzip > /tmp/test.gz
   curl -X POST -H "Content-Encoding: gzip" \
     --data-binary @/tmp/test.gz http://localhost:3000/echo
   ```

### 5. Brotli Encoding

Brotli is a modern compression algorithm with better compression ratios than gzip.

#### Using brotli command:

```bash
# Simple brotli test
echo '{"message": "Hello from brotli!"}' | \
  brotli -c | \
  curl -X POST \
    -H "Content-Encoding: br" \
    --data-binary @- \
    http://localhost:3000/echo

# Install brotli if needed:
# macOS: brew install brotli
# Ubuntu/Debian: apt-get install brotli
```

#### Using Python:

```bash
echo '{"message": "Hello from brotli!"}' | \
  python3 -c "
import sys, brotli
data = sys.stdin.buffer.read()
compressed = brotli.compress(data)
sys.stdout.buffer.write(compressed)
" | \
  curl -X POST \
    -H "Content-Encoding: br" \
    --data-binary @- \
    http://localhost:3000/echo

# Install Python brotli: pip3 install brotli
```

### 6. Zstandard (Zstd) Encoding

Zstandard offers excellent compression speed and ratio, increasingly popular for HTTP.

#### Using zstd command:

```bash
# Simple zstd test
echo '{"message": "Hello from zstd!"}' | \
  zstd -c | \
  curl -X POST \
    -H "Content-Encoding: zstd" \
    --data-binary @- \
    http://localhost:3000/echo

# Install zstd if needed:
# macOS: brew install zstd
# Ubuntu/Debian: apt-get install zstd
```

#### Using Python:

```bash
echo '{"message": "Hello from zstd!"}' | \
  python3 -c "
import sys, zstandard as zstd
data = sys.stdin.buffer.read()
cctx = zstd.ZstdCompressor()
compressed = cctx.compress(data)
sys.stdout.buffer.write(compressed)
" | \
  curl -X POST \
    -H "Content-Encoding: zstd" \
    --data-binary @- \
    http://localhost:3000/echo

# Install Python zstandard: pip3 install zstandard
```

## Format Details

### Gzip Format
- Magic bytes: `1f 8b`
- Contains: header (10+ bytes) + deflate data + footer (8 bytes)
- Used by: `gzip` command, web browsers, most HTTP clients
- Compression ratio: Good
- Speed: Fast

### Deflate Format (Raw)
- No magic bytes
- Just raw DEFLATE compressed stream
- Rarely used in practice

### Deflate Format (Zlib-wrapped)
- Magic bytes: `78 9c` (default compression) or `78 01` (no compression) or `78 da` (best compression)
- Contains: zlib header (2 bytes) + deflate data + adler32 checksum (4 bytes)
- More common interpretation of "deflate" in HTTP
- Compression ratio: Good
- Speed: Fast

### Brotli Format
- Magic bytes: None (starts with compressed data)
- Designed specifically for HTTP compression
- Better compression ratio than gzip (10-20% smaller)
- Speed: Slower compression, similar decompression
- Supported by: Modern browsers, CDNs
- Content-Encoding header: `br`

### Zstandard Format
- Magic bytes: `28 b5 2f fd`
- Modern compression algorithm by Facebook
- Excellent compression ratio and speed
- Tunable compression levels (1-22)
- Speed: Very fast compression and decompression
- Supported by: Growing adoption in browsers and servers
- Content-Encoding header: `zstd`

### Identity
- No compression, data sent as-is
- Content-Encoding can be "identity" or omitted

## Compression Comparison

For typical JSON data (~1KB with repetitive content):

| Format | Compressed Size | Compression Speed | Decompression Speed |
|--------|----------------|-------------------|---------------------|
| **Gzip** | ~200 bytes | Fast | Fast |
| **Deflate** | ~200 bytes | Fast | Fast |
| **Brotli** | ~180 bytes | Slower | Fast |
| **Zstd** | ~190 bytes | Very Fast | Very Fast |

**Recommendation:**
- **Gzip**: Best compatibility, good all-around choice
- **Brotli**: Best compression ratio for static assets
- **Zstd**: Best balance of speed and compression for dynamic content
- **Deflate**: Rarely used in practice (ambiguous spec)
