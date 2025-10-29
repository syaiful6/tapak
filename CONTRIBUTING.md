# Contributing to Tapak

Thank you for your interest in contributing to Tapak!

## Development Setup

### Using Nix (Recommended)

If you're using Nix with flakes:

```bash
# Enter the development shell
nix develop

# Or with direnv
direnv allow
```

The development shell includes all necessary dependencies including odoc for documentation.

### Using Opam

```bash
# Install dependencies
opam install . --deps-only --with-doc --with-test

# Build the project
dune build

# Run tests
dune test
```

## Building Documentation

### Locally

```bash
dune build @doc

# View documentation
open _build/default/_doc/_html/index.html
```

### Serving Locally

```bash
cd _build/default/_doc/_html
python3 -m http.server 8000
# Then open http://localhost:8000
```

## Documentation Guidelines

Documentation is written in `.mld` (Markup Language Documentation) files located in the `doc/` directory.

### Adding New Documentation

1. Create a new `.mld` file in `doc/`
2. Add it to `doc/dune` in the `mld_files` list
3. Reference it from `doc/index.mld` or other pages

### Documentation Syntax

odoc uses a variant of Markdown. Key syntax:

- `{0 Title}` - Page title
- `{1 Section}` - Section heading
- `{2 Subsection}` - Subsection heading
- `{[code]}` - Code block
- `{!module:Module_name}` - Link to module
- `{!page-page_name}` - Link to page
- `{{:url} Link text}` - External link
- `{b bold}` - Bold text
- `{i italic}` - Italic text

## Code Style

The project uses ocamlformat for code formatting:

```bash
# Format all files
dune build @fmt --auto-promote
```

## Pull Request Process

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Update documentation if needed
6. Ensure all tests pass
7. Submit a pull request

## Questions?

Feel free to open an issue for questions or discussions!
