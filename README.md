# pandoc-pyplot

_A Pandoc filter for generating figures with Matplotlib from code directly in documents_

Inspired by [sphinx](https://sphinxdoc.org)'s `plot_directive`, `pandoc-pyplot` helps turn Python code present in your documents to embedded Matplotlib figures. 

## Usage

The filter recognizes code blocks with the `plot_target` attribute present. It will run the script in the associated code block in a Python interpreter and capture the generated Matplotlib figure. This captured figure will be saved in the located specific by `plot_target`.

### Basic example

Here is a basic example using the scripting `matplotlib.pyplot` API:

    ```{plot_target=my_figure.jpg}
    import matplotlib.pyplot as plt

    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```

`pandoc-pyplot` will determine whether the `plot_target` is a relative or absolute path. In case of a relative path (like above), all paths will be considered relative to the current working directory.

We can control the format of the output file by changing the `plot_target` file extension. All formats supported by Matplotlib on your machine are available.

Putting the above in `input.md`, we can then generate the plot and embed it:

```bash
pandoc --filter pandoc-pyplot input.md --output output.html
```

or

```bash
pandoc --filter pandoc-pyplot input.md output.pdf
```

or any other output format you want. There are more examples in the source repository, in the `\examples` directory.

### Link to source code

In case of an output format that supports links (e.g. HTML), the embedded image generated by `pandoc-pyplot` will be a link to the source code which was used to generate the file. Therefore, other people can see what Python code was used to create your figures.

### Alternate text

You can also specify some alternate text for your image. This is done using the optional `plot_alt` parameter:

    ```{plot_target=my_figure.jpg, plot_alt="This is a simple figure"}
    import matplotlib.pyplot as plt

    plt.figure()
    plt.plot([0,1,2,3,4], [1,2,3,4,5])
    plt.title('This is an example figure')
    ```

## Install

## Requirements

This filter only works with the Matplotlib plotting library. Therefore, you need [Matplotlib](matplotlib.org) and a Python interpreter. The python interpreter is expected to be discoverable using the name `"python"` (as opposed to `"python3"`, for example)

## Running the filter

The filter program must be in your `PATH`. In case it is, you can use the filter with Pandoc as follows:

```bash
pandoc --filter pandoc-pyplot input.md output.html
```

Another example with PDF output:

```bash
pandoc --filter pandoc-pyplot input.md output.pdf
```

Python exceptions will be printed to screen in case of a problem.

## Usage as a Haskell library

To include the functionality of `pandoc-pyplot` in a Haskell package, you can use the `makePlot` function:

```haskell
-- From pandoc-types
import Text.Pandoc.Walk         (walkM)
import Text.Pandoc.Definition   (Pandoc)
-- From pandoc-pyplot
import Text.Pandoc.Filter.Pyplot (makePlot)

transformDocument :: Pandoc -> IO Pandoc
transformDocument = walkM makePlot
```

## Usage with Hakyll

This filter was originally designed to be used with [Hakyll](https://jaspervdj.be/hakyll/). In case you want to use the filter with your own Hakyll setup, you must create a transform function first:

```haskell
-- From pandoc-types
import Text.Pandoc          (Pandoc)
import Text.Pandoc.Walk     (walkM)

-- from pandoc-pyplot
import Text.Pandoc.Filter.IncludePyplot (includePlot)

import Hakyll

plotTransform :: Pandoc -> IO Pandoc
plotTransform = walkM . includePlot

-- Unsafe compiler is required because of the interaction
-- in IO (i.e. running an external Python script).
includePlotPandocCompiler :: Compiler (Item String)
includePlotPandocCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (unsafeCompiler . plotTransform)
```

## Aknowledgements

This package is inspired from [`pandoc-include-code`](https://github.com/owickstrom/pandoc-include-code).