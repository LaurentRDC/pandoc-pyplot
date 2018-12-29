# Change log

pandoc-pyplot uses [Semantic Versioning](http://semver.org/spec/v2.0.0.html)

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