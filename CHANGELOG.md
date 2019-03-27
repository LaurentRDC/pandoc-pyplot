# Change log

pandoc-pyplot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

Release 2.0.0.0
---------------

Many **breaking changes** in this release:

* `pandoc-pyplot` will now determine the filename based on hashing the figure content. Therefore, figures will only be re-generated if necessary.
* Removed the ability to control the filename and format directly using the `plot_target=...` attribute.
* Added the ability to control the directory in which figures will be saved using the `directory=...` attribute.
* Added the possibility to control the figures dots-per-inch (i.e. pixel density) with the `dpi=...` attribute.
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