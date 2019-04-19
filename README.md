# pandoc-pyplot - A Pandoc filter to generate Matplotlib figures directly in documents

[![Hackage version](https://img.shields.io/hackage/v/pandoc-pyplot.svg)](http://hackage.haskell.org/package/pandoc-pyplot) [![Stackage version (LTS)](http://stackage.org/package/pandoc-pyplot/badge/lts)](http://stackage.org/nightly/package/pandoc-pyplot) [![Stackage version (nightly)](http://stackage.org/package/pandoc-pyplot/badge/nightly)](http://stackage.org/nightly/package/pandoc-pyplot) [![Build status](https://ci.appveyor.com/api/projects/status/qbmq9cyks5jup48e?svg=true)](https://ci.appveyor.com/project/LaurentRDC/pandoc-pyplot) ![GitHub](https://img.shields.io/github/license/LaurentRDC/pandoc-pyplot.svg)

`pandoc-pyplot` turns Python code present in your documents to embedded Matplotlib figures.

## Usage

The filter recognizes code blocks with the `.pyplot` class present. It will run the script in the associated code block in a Python interpreter and capture the generated Matplotlib figure.

Here is a basic example using the scripting `matplotlib.pyplot` API:

```markdown
    ```{.pyplot}
    import matplotlib.pyplot as plt

    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```
```

Putting the above in `input.md`, we can then generate the plot and embed it:

```bash
pandoc --filter pandoc-pyplot input.md --output output.html
```

or

```bash
pandoc --filter pandoc-pyplot input.md --output output.pdf
```

or any other output format you want.

There are more examples in the [source repository](https://github.com/LaurentRDC/pandoc-pyplot), in the `\examples` directory.

## Features

### No wasted work

`pandoc-pyplot` minimizes work, only generating figures if it absolutely must. Therefore, you can confidently run the filter on very large documents containing dozens of figures --- like a book or a thesis --- and only the figures which have recently changed will be re-generated.

### Link to source code and high-resolution figure

In case of an output format that supports links (e.g. HTML), the embedded image generated by `pandoc-pyplot` will be a link to the source code which was used to generate the file. Therefore, other people can see what Python code was used to create your figures. A high resolution image will be made available in a caption link.

### Captions

You can also specify a caption for your image. This is done using the optional `caption` parameter:

```markdown
    ```{.pyplot caption="This is a simple figure"}
    import matplotlib.pyplot as plt

    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```
```

Caption formatting is either plain text or Markdown. LaTeX-style math is also support in captions (using dollar signs $...$).

### Including scripts

If you find yourself always repeating some steps, inclusion of scripts is possible using the `include` parameter. For example, if you want all plots to have the [`ggplot`](https://matplotlib.org/tutorials/introductory/customizing.html#sphx-glr-tutorials-introductory-customizing-py) style, you can write a very short preamble `style.py` like so:

```python
import matplotlib.pyplot as plt
plt.style.use('ggplot')
```

and include it in your document as follows:

```markdown
    ```{.pyplot include=style.py}
    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```
```

Which is equivalent to writing the following markdown:

```markdown
    ```{.pyplot}
    import matplotlib.pyplot as plt
    plt.style.use('ggplot')

    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```
```

This `include` parameter is perfect for longer documents with many plots. Simply define the style you want in a separate script! You can also import packages this way, or define functions you often use.

### Compatibility with pandoc-crossref

[`pandoc-crossref`](https://github.com/lierdakil/pandoc-crossref) is a pandoc filter that makes it effortless to cross-reference objects on documents. 

You can use `pandoc-crossref` in conjunction with `pandoc-pyplot` for the ultimate figure-making pipeline. You can combine both in a figure like so:

```markdown
    ```{#fig:myexample .pyplot caption="This is a caption"}
    # Insert figure script here
    ```

    As you can see in @fig:myexample, ...
```

If the above source is located in file `myfile.md`, you can render the figure and references by applying `pandoc-pyplot` **first**, and then `pandoc-crossref`. For example:

```bash
pandoc --filter pandoc-pyplot --filter pandoc-crossref -i myfile.md -o myfile.html
```

### Configurable

*New in version 2.1.0.0*

To avoid repetition, `pandoc-pyplot` can be configured using simple YAML syntax. `pandoc-pyplot` will look for a `pandoc-pyplot.yml` file in the current working directory:

```yaml
# You can specify any or all of the following parameters
interpreter: python36
directory: mydirectory/
include: mystyle.py
format: jpeg
dpi: 150
```

These values override the default values, which are equivalent to:

```yaml
interpreter: python
directory: generated/
format: png
dpi: 80
```

## Installation

### Binaries

Windows binaries are available on [GitHub](https://github.com/LaurentRDC/pandoc-pyplot/releases). Place the executable in a location that is in your PATH to be able to call it.

### Installers

Windows installers are made available thanks to [Inno Setup](http://www.jrsoftware.org/isinfo.php). You can download them from the [release page](https://github.com/LaurentRDC/pandoc-pyplot/releases/latest).

### From Hackage/Stackage

`pandoc-pyplot` is available on Hackage. Using the [`cabal-install`](https://www.haskell.org/cabal/) tool:

```bash
cabal update
cabal install pandoc-pyplot
```

Similarly, `pandoc-pyplot` is available on Stackage:

```bash
stack update
stack install pandoc-pyplot
```

### From source

Building from source can be done using [`stack`](https://docs.haskellstack.org/en/stable/README/) or [`cabal`](https://www.haskell.org/cabal/):

```bash
git clone https://github.com/LaurentRDC/pandoc-pyplot
cd pandoc-pylot
stack install # Alternatively, `cabal install`
```

## Running the filter

### Requirements

This filter only works with the Matplotlib plotting library. Therefore, you a Python interpreter and at least [Matplotlib](https://matplotlib.org/) installed. The name of the Python interpreter to use can be specified in a `.pandoc-pyplot.yml` file; by default, `pandoc-pyplot` will use the `"python"` name.

You can use the filter with Pandoc as follows:

```bash
pandoc --filter pandoc-pyplot input.md --output output.html
```

In which case, the output is HTML. Another example with PDF output:

```bash
pandoc --filter pandoc-pyplot input.md --output output.pdf
```

Python exceptions will be printed to screen in case of a problem.

`pandoc-pyplot` has a limited command-line interface. Take a look at the help available using the `-h` or `--help` argument:

```bash
pandoc-pyplot --help
```

## Usage as a Haskell library

To include the functionality of `pandoc-pyplot` in a Haskell package, you can use the `makePlot :: Block -> IO Block` function (for single blocks) or `plotTransform :: Pandoc -> IO Pandoc` function (for entire documents).

### Usage with Hakyll

This filter was originally designed to be used with [Hakyll](https://jaspervdj.be/hakyll/). In case you want to use the filter with your own Hakyll setup, you can use a transform function that works on entire documents:

```haskell
import Text.Pandoc.Filter.Pyplot (plotTransform)

import Hakyll

-- Unsafe compiler is required because of the interaction
-- in IO (i.e. running an external Python script).
makePlotPandocCompiler :: Compiler (Item String)
makePlotPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . plotTransform)
```

The `plotTransformWithConfig` is also available for a more configurable set-up.

## Warning

Do not run this filter on unknown documents. There is nothing in `pandoc-pyplot` that can stop a Python script from performing **evil actions**.
