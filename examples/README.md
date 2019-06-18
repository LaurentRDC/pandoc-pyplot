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

## Example 3 : pandoc-pyplot and LaTeX

This third example demonstrates how pandoc-pyplot can be included in a LaTeX pipeline. It is recommended that the figures first be rendered "in-place":

```bash
pandoc --filter pandoc-pyplot -i latex.tex -o latex_with_figures.tex
```

and then your usual LaTeX -> PDF rendering happens. The intermediate file will not be human-readable, most probably.

To label a figure, you can use raw TeX macros in captions (requires pandoc-pyplot > 2.1.4.0):

```latex
\begin{minted}[caption=This is an example\label{example} include=style.py, format=png]{pyplot}
...
\end{minted}

... as seen in Figure \ref{example}.
```