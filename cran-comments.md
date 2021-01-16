## Test environments

* local


  * R version 4.0.3 (2020-10-10)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Catalina 10.15.6
  * devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_` = "true"))
  
* win-builder

## Mosaic suite udpates

This update mainly handles a breaking change in dplyr which now exports tally() and count() as generics.
Also handles some URL issues, and exports one new function. This should make it possible to update 
the mosaic package, which will follow soon.



