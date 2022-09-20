## Test environments

* local
    * R version 4.2.1 (2022-06-23)
    * macOS Monterey 12.5.1
    * x86_64, darwin17.0

* win-builder

## Notes

CRAN reports new issue with a test failure (appaerntly only under openBLAS). The test uses
randomly generated data, and a call to nls() seems to be failing.  Since this doesn't fail
elsewhere and I've been unable to recreate, I'm adding a call to set.seed() to at least freeze
the data used in the test.