# Compression Testing - Quick Reference

Quick one-liners for testing all supported compression formats.

## Prerequisites

```bash
# Start the server
dune exec --profile=dev tapak-showcase

# Install compression tools (optional)
brew install brotli zstd              # macOS
# or
apt-get install brotli zstd           # Ubuntu/Debian

# Install Python modules (optional)
pip3 install brotli zstandard
```

## One-Liner Tests

### Gzip

```bash
echo '{"test": "gzip"}' | gzip | curl -X POST -H "Content-Encoding: gzip" --data-binary @- http://localhost:3000/echo
```

### Deflate (zlib format)

```bash
echo '{"test": "deflate"}' | python3 -c "import sys,zlib; sys.stdout.buffer.write(zlib.compress(sys.stdin.buffer.read()))" | curl -X POST -H "Content-Encoding: deflate" --data-binary @- http://localhost:3000/echo
```

### Brotli (command)

```bash
echo '{"test": "brotli"}' | brotli -c | curl -X POST -H "Content-Encoding: br" --data-binary @- http://localhost:3000/echo
```

### Brotli (Python)

```bash
echo '{"test": "brotli"}' | python3 -c "import sys,brotli; sys.stdout.buffer.write(brotli.compress(sys.stdin.buffer.read()))" | curl -X POST -H "Content-Encoding: br" --data-binary @- http://localhost:3000/echo
```

### Zstd (command)

```bash
echo '{"test": "zstd"}' | zstd -c | curl -X POST -H "Content-Encoding: zstd" --data-binary @- http://localhost:3000/echo
```

### Zstd (Python)

```bash
echo '{"test": "zstd"}' | python3 -c "import sys,zstandard as z; sys.stdout.buffer.write(z.ZstdCompressor().compress(sys.stdin.buffer.read()))" | curl -X POST -H "Content-Encoding: zstd" --data-binary @- http://localhost:3000/echo
```

### Identity (no compression)

```bash
echo '{"test": "identity"}' | curl -X POST -H "Content-Encoding: identity" --data-binary @- http://localhost:3000/echo
```

## Full Test Suite

```bash
./examples/showcase/test_compression.sh
```

## Content-Encoding Headers

| Format | Header Value | Command | Python Module |
|--------|-------------|---------|---------------|
| Gzip | `gzip` | `gzip` | `gzip` |
| Deflate | `deflate` | - | `zlib` |
| Brotli | `br` | `brotli` | `brotli` |
| Zstandard | `zstd` | `zstd` | `zstandard` |
| None | `identity` | - | - |

## Expected Response

When compression is working correctly:

```json
Received 15 bytes
Content-Encoding: removed after decompression

Body:
{"test": "gzip"}
```

The middleware:

- ✅ Decompresses the body
- ✅ Removes `Content-Encoding` header
- ✅ Removes `Content-Length` header
- ✅ Passes decompressed body to handler
