# Documentation Setup

This guide explains how to build and deploy Tapak's documentation.

## Prerequisites

The documentation system uses:

- **odoc**: OCaml documentation generator
- **dune**: Build system with documentation support

## Installation

### Option 1: Using Nix (Recommended)

Odoc is included in the development shell:

```bash
nix develop
# or if using direnv
direnv allow
```

### Option 2: Using Opam

```bash
# Install odoc
opam install odoc

# Verify installation
odoc --version
```

## Building Documentation

### Quick Build

```bash
./scripts/build-docs.sh
```

### Manual Build

```bash
dune build @doc
```

The generated HTML will be in `_build/default/_doc/_html/`.

## Viewing Documentation Locally

### Option 1: Direct File Access

```bash
open _build/default/_doc/_html/index.html
# or on Linux
xdg-open _build/default/_doc/_html/index.html
```

### Option 2: Local Server

```bash
cd _build/default/_doc/_html
python3 -m http.server 8000
```

Then visit <http://localhost:8000>

## Documentation Structure

```
doc/
├── dune           # Dune configuration for docs
├── index.mld      # Main landing page
├── kernel.mld     # Tapak Kernel overview
├── router.mld     # Router guide
└── middleware.mld # Middleware guide
```

## Writing Documentation

Documentation files use the `.mld` format (Markup Language Documentation).

### Basic Syntax

```ocaml
{0 Page Title}

{1 Section Heading}

Regular paragraph text.

{2 Subsection}

{[ 
  (* Code block *)
  let example = "OCaml code"
]}

{b Bold text} and {i italic text}.

- List item 1
- List item 2

{!module:Module_name} - Link to a module
{!page-page_name} - Link to another page
{{:https://example.com} External link}
```

### Adding a New Documentation Page

1. Create `doc/your-page.mld`:

```ocaml
{0 Your Page Title}

Content goes here...
```

2. Add it to `doc/dune`:

```lisp
(documentation
 (package tapak)
 (mld_files index kernel router middleware your-page))
```

3. Link to it from `doc/index.mld`:

```ocaml
- {!page-your-page} - Description
```

## GitHub Pages Deployment

### Automatic Deployment

The repository includes a GitHub Actions workflow (`.github/workflows/docs.yml`) that automatically:

1. Builds documentation on push to `main`
2. Deploys to GitHub Pages

### Enabling GitHub Pages

1. Go to your repository on GitHub
2. Navigate to Settings → Pages
3. Under "Build and deployment":
   - Source: **GitHub Actions**
4. Push to `main` branch
5. Documentation will be available at: `https://your-username.github.io/tapak/`

For this repository: <https://syaiful6.github.io/tapak/>

### Manual Deployment

If you want to deploy manually:

```bash
# Build documentation
dune build @doc

# Create deployment directory
mkdir -p gh-pages
cp -r _build/default/_doc/_html/* gh-pages/

# Switch to gh-pages branch
git checkout -b gh-pages

# Add and commit
git add gh-pages/
git commit -m "Update documentation"

# Push to GitHub
git push origin gh-pages:gh-pages --force
```

## Troubleshooting

### "Program odoc not found"

Install odoc:

```bash
opam install odoc
```

### Documentation not updating

Clear the build cache:

```bash
dune clean
dune build @doc
```

### Links not working

Ensure:

- Module names are correct in `{!module:Name}` links
- Page names match filenames (without `.mld`) in `{!page-name}` links
- All referenced pages are listed in `doc/dune`

## Resources

- [odoc Documentation](https://ocaml.github.io/odoc/)
- [Dune Documentation Generation](https://dune.readthedocs.io/en/stable/documentation.html)
- [GitHub Pages Documentation](https://docs.github.com/en/pages)
