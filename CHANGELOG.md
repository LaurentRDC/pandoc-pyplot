# Change log

pandoc-pyplot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

Release 2.1.4.0
---------------

* Added examples and documentation on how to use `pandoc-pyplot` on LaTeX documents.
* Allowed raw LaTeX macros in figure captions. This is required to label figures in LaTeX. E.g.:
  
  ```latex
  \begin{minted}[caption=myCaption\label{myfig}]{pyplot}
  
  \end{minted}
  ```

* `with-links` key changed to `links`. I'm sorry. Pandoc doesn't support LaTeX tokens with `-`.

Release 2.1.3.0
---------------

* Switched to using [optparse-applicative](https://github.com/pcapriotti/optparse-applicative#arguments) for command-line argument parsing.
* Added a command-line options, "--write-example-config", which will write a config file ".pandoc-pyplot.yml" to show all available configuration options.
* Links to source code and high-res images can be suppressed using `{.pyplot with-links=false ...}` (or via the configuration file with `with-links: false`). This is to get cleaner output in technical documentation (e.g. PDF). Example:

  ```markdown
    ```{.pyplot caption="This is a caption" with-links=false}
    import matplotlib.pyplot as plt
    plt.figure()
    plt.plot([1,2,3,4,5],[1,2,3,4,5])
    ```
  ```
* Added automated builds on macOS and Linux via Azure-Pipelines. Windows build will stay on Appveyor for now.

Release 2.1.2.0
---------------

* Added the "flags" configuration option, which allows to pass command-line flags to the Python interpreter. For example, warnings can be suppressed using the `-Wignore` flag.
* Refactoring of the script check mechanism. It will be much easier to extend in the future.
* Updated the command-line help with an example combining pandoc-pyplot and pandoc-crossref
* Default Python interpreter is now "python" on Windows and __"python3" otherwise__.

Release 2.1.1.1
---------------

* Fixed a critical bug where pandoc-pyplot would interpret input from pandoc as a malformed command-line flag.

Release 2.1.1.0
---------------

* Added a command-line option to open the HTML manual in the default web browser.
* Added documentation regarding compatibility with pandoc-crossref. This was always supported but not explicitly documented.

Release 2.1.0.1
---------------

* Fixed outdated documentation (referencing "target" parameter)
* Fixed types required to build Configuration values that were not exported (SaveFormat, PythonScript)

Release 2.1.0.0
---------------

* Added support for config files ".pandoc-pyplot.yml", which specify different default values. This is mirrored in the new `Configuration` type and new functions, `makePlotWithConfig` and `plotTransformWithConfig`.
* Added the ability to specify a different Python interpreter to use.
* Added support for GIF and TIF files.
* Added the "-f"/"--formats" command to show supported output figure formats.
* Added support for GHC 8.2
* Moved internal modules to `Text.Pandoc.Filter.Pyplot.Internal` module.

Release 2.0.1.0
---------------

* Support for Markdown formatting in figure captions, including LaTeX math.

Release 2.0.0.0
---------------

Many **breaking changes** in this release:

* `pandoc-pyplot` will now determine the filename based on hashing the figure content. Therefore, figures will only be re-generated if necessary.
* Removed the ability to control the filename and format directly using the `plot_target=...` attribute.
* Added the ability to control the directory in which figures will be saved using the `directory=...` attribute.
* Added the possibility to control the figures dots-per-inch (i.e. pixel density) with the `dpi=...` attribute.
* Added the ability to control the figure format with the `format=...` attribute. Possible values are currently `"png"`, `"svg"`, `"pdf"`, `"jpg"`/`"jpeg"` and `"eps"`.
* The confusing `plot_alt=...` attribute has been renamed to `caption=...` for obvious reasons.
* The `plot_include=...` attribute has been renamed to `include=...`.
* Added the generation of a higher resolution figure for every figure `pandoc-pyplot` understands.

Release 1.1.0.0
---------------

* Added the ability to include Python files before code using the `plot_include=script.py` attribute.
* Added a test suite.

Release 1.0.3.0
---------------

* Fixed an issue where `pandoc-pyplot` would not build with base < 4.9 (#1)

Release 1.0.2.0
---------------

* Added support for captions using the `plot_alt=...` attribute. For example:

  ```markdown
    ```{plot_target=test.png plot_alt="This is a caption"}
    import matplotlib.pyplot as plt
    plt.figure()
    plt.plot([1,2,3,4,5],[1,2,3,4,5])
    ```
  ```

Release 1.0.1.0
---------------

* Added `plotTransform :: Pandoc -> IO Pandoc` function to transform entire documents. This makes it easier to integrate `pandoc-pyplot` into Hakyll-based sites!

Release 1.0.0.1
---------------

* Updated README with fixes and warnings
* Added top-level package documentation compatible with Haddock
* Added Unsafe language extension, as this filter will run arbitrary Python scripts.

Release 1.0.0.0
---------------

Initial release.

See documentation on [Hackage](https://hackage.haskell.org/package/pandoc-pyplot)
