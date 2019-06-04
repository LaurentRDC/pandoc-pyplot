# Examples of pandoc-pyplot usage

This folder contains everything you need to get started with two examples of pandoc-pyplot. These examples showcase a few features of pandoc-pyplot:

* Regular captions;
* Markdown captions with LaTeX math;
* Include scripts;
* Interaction with other Pandoc filters;
* YAML configuration.

## Example 1: simple pandoc-pyplot plotting

The first example is located in file `residuals.md`. Note that default values are determined bu the `.pandoc-pyplot.yml` config file.

The easiest way to compile this example is to HTML:

```
pandoc --filter pandoc-pyplot -i residuals.md -o residuals.html
```

If you have a LaTeX toolchain installed, you can generate a PDF as well:

```
pandoc --filter pandoc-pyplot -i residuals.md -o residuals.pdf
```

## Example 2: pandoc-pyplot and pandoc-crossref together

The second example showcases the interactions between pandoc-pyplot and pandoc-crossref. It is located in the file `crossref.md`. For this example to work, pandoc-pyplot must be used __first__. Note that default values are determined by the `.pandoc-pyplot.yml` config file.

The easiest way to compile this example is to HTML:

```bash
pandoc --filter pandoc-pyplot --filter pandoc-crossref -i crossref.md -o crossref.html
```

If you have a LaTeX toolchain installed, you can generate a PDF as well:

```
pandoc --filter pandoc-pyplot --filter pandoc-crossref -i crossref.md -o crossref.pdf
```