## Test environments

* local
    * R version 4.2.1 (2022-06-23)
    * macOS Monterey 12.5.1
    * x86_64, darwin17.0

* win-builder

## Notes

This is a relatively modest update, fixing bug and expanding one function (makeFun) to handle 
additional cases (without changing existing behavior) to support new features in
mosaicCalc (which will be submitted soon).

Adding that expanded version of makeFun() breaks a test in the mosaic package since
that use case used to throw an error and now does something useful.  So the mosaic
package will be submitted in tandem with this package.

