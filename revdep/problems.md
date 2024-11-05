# fr

<details>

* Version: 0.5.1
* GitHub: https://github.com/cole-brokamp/fr
* Source code: https://github.com/cran/fr
* Date/Publication: 2023-11-30 20:00:02 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "fr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_data_frame
    > ### Title: Coerce a 'fr_tdr' object into a data frame
    > ### Aliases: as_data_frame
    > 
    > ### ** Examples
    > 
    > as_fr_tdr(mtcars, name = "mtcars") |>
    +   as_data_frame()
    Error: <fr::fr_schema> object is invalid:
    - all items in @fields should be fr_field objects
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       5.     └─fr:::fr_schema(fields = purrr::imap(x, as_fr_field))
       6.       └─S7::new_object(...)
       7.         └─S7::validate(object, recursive = !parent_validated)
      
      [ FAIL 23 | WARN 0 | SKIP 2 | PASS 9 ]
      Deleting unused snapshots:
      • write_fr_tdr/my_mtcars.csv
      • write_fr_tdr/tabular-data-resource.yaml
      Error: Test failures
      Execution halted
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    summary:
      function(object, ...)
    summary.fr::fr_tdr:
      function(x, ...)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘creating_a_tabular-data-resource.Rmd’ using rmarkdown
    
    Quitting from lines 48-57 [unnamed-chunk-4] (creating_a_tabular-data-resource.Rmd)
    Error: processing vignette 'creating_a_tabular-data-resource.Rmd' failed with diagnostics:
    <fr::fr_schema> object is invalid:
    - all items in @fields should be fr_field objects
    --- failed re-building ‘creating_a_tabular-data-resource.Rmd’
    
    ...
    Error: processing vignette 'read_fr_tdr.Rmd' failed with diagnostics:
    <fr::fr_schema> object is invalid:
    - all items in @fields should be fr_field objects
    --- failed re-building ‘read_fr_tdr.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘creating_a_tabular-data-resource.Rmd’ ‘read_fr_tdr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘creating_a_tabular-data-resource.Rmd’
      ...
    3 A03   2013-08-15    15.6 best        19 TRUE 
    
    > d_tdr <- as_fr_tdr(d, name = "types_example", version = "0.1.0", 
    +     title = "Example Data with Types", homepage = "https://geomarker.io", 
    +     .... [TRUNCATED] 
    
      When sourcing ‘creating_a_tabular-data-resource.R’:
    ...
    
    > d_fr <- read_fr_tdr(fs::path_package("fr", "hamilton_poverty_2020"))
    
      When sourcing ‘read_fr_tdr.R’:
    Error: <fr::fr_schema> object is invalid:
    - all items in @fields should be fr_field objects
    Execution halted
    
      ‘creating_a_tabular-data-resource.Rmd’ using ‘UTF-8’... failed
      ‘read_fr_tdr.Rmd’ using ‘UTF-8’... failed
    ```

