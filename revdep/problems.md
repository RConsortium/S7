# ale (0.5.3)

* GitHub: <https://github.com/tripartio/ale>
* Email: <mailto:Chitu.Okoli@skema.edu>
* GitHub mirror: <https://github.com/cran/ale>

Run `revdepcheck::cloud_details(, "ale")` for more info

## Newly broken

*   checking whether package ‘ale’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ale’
     See ‘/tmp/workdir/ale/new/ale.Rcheck/00install.out’ for details.
     ```

# apa7 (0.1.3)

* GitHub: <https://github.com/wjschne/apa7>
* Email: <mailto:w.joel.schneider@gmail.com>
* GitHub mirror: <https://github.com/cran/apa7>

Run `revdepcheck::cloud_details(, "apa7")` for more info

## Newly broken

*   checking whether package ‘apa7’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘apa7’
     See ‘/tmp/workdir/apa7/new/apa7.Rcheck/00install.out’ for details.
     ```

# bidsr (0.1.1)

* GitHub: <https://github.com/dipterix/bidsr>
* Email: <mailto:dipterix.wang@gmail.com>
* GitHub mirror: <https://github.com/cran/bidsr>

Run `revdepcheck::cloud_details(, "bidsr")` for more info

## Newly broken

*   checking whether package ‘bidsr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/bidsr/new/bidsr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘bidsr’ ...
** this is package ‘bidsr’ version ‘0.1.1’
** package ‘bidsr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in S7::`method<-`(`*tmp*`, list(x = BIDSMap, name = S7::class_any),  : 
  `signature` must be length 1.
Error: unable to load R code in package ‘bidsr’
Execution halted
ERROR: lazy loading failed for package ‘bidsr’
* removing ‘/tmp/workdir/bidsr/new/bidsr.Rcheck/bidsr’


```
### CRAN

```
* installing *source* package ‘bidsr’ ...
** this is package ‘bidsr’ version ‘0.1.1’
** package ‘bidsr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bidsr)


```
# btw (1.5.0)

* GitHub: <https://github.com/posit-dev/btw>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/btw>

Run `revdepcheck::cloud_details(, "btw")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       6. └─btw::btw("@news dplyr join_by", clipboard = FALSE)
       7.   ├─btw::btw_this(...)
       8.   └─btw:::btw_this.environment(...)
       9.     └─btw:::btw_tool_env_describe_environment_impl(...)
      10.       └─btw:::map(...)
      11.         └─base::lapply(.x, .f, ...)
      12.           └─btw (local) FUN(X[[i]], ...)
      13.             ├─btw::btw_this(item, caller_env = environment)
      14.             └─btw:::btw_this.character(item, caller_env = environment)
      15.               └─btw:::dispatch_at_command(cmd, caller_env)
      16.                 └─btw (local) btw_this_cmd(cmd$args)
      17.                   ├─base::I(...)
      18.                   │ └─base::unique.default(c("AsIs", oldClass(x)))
      19.                   └─btw:::btw_tool_docs_package_news_impl(...)
      20.                     └─btw:::package_news_search(...)
      21.                       └─btw:::map_chr(news$HTML, extract_relevant_news, search_term = search_term)
      22.                         └─btw:::.rlang_purrr_map_mold(.x, .f, character(1), ...)
      23.                           └─base::vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
      24.                             └─btw (local) FUN(X[[i]], ...)
      25.                               └─btw:::map_chr(all_elements[has_text], as.character)
      26.                                 └─btw:::.rlang_purrr_map_mold(.x, .f, character(1), ...)
      27.                                   └─rlang::as_function(.f, env = global_env())
      28.                                     └─rlang:::abort_coercion(x, "a function", arg = arg, call = call)
      29.                                       └─rlang::abort(msg, call = call)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        11.                     \-rlang::abort(msg, call = call)
       -- Error ('test-tool-docs-news.R:136:3'): btw_tool_docs_package_news() when no news is found --
       Error in `(S7::as_class(ellmer::ContentText))(text = text, ...)`: unused argument ("")
       Backtrace:
            x
         1. +-testthat::expect_message(...) at test-tool-docs-news.R:136:3
         2. | \-testthat:::expect_condition_matching_(...)
         3. |   \-testthat:::quasi_capture(...)
         4. |     +-testthat (local) .capture(...)
         5. |     | \-base::withCallingHandlers(...)
         6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         7. +-testthat::expect_s3_class(btw(package_news("R")), "btw::btw")
         8. | \-testthat::quasi_label(enquo(object))
         9. |   \-rlang::eval_bare(expr, quo_get_env(quo))
        10. \-btw::btw(package_news("R"))
        11.   \-btw:::BTW("")
        12.     \-S7::new_object(...)
        13.       \-S7:::check_parent(`_parent`, class)
        14.         \-S7:::class_inherits(parent, parent_class)
       
       [ FAIL 15 | WARN 2 | SKIP 154 | PASS 2082 ]
       Error:
       ! Test failures.
       Execution halted
       Ran 8/8 deferred expressions
     ```

*   checking R code for possible problems ... NOTE
     ```
     wrap_built_in_tools : <anonymous>: no visible global function
       definition for ‘BtwToolBuiltIn’
     Undefined global functions or variables:
       BtwToolBuiltIn
     ```

# caugi (1.3.0)

* GitHub: <https://github.com/frederikfabriciusbjerre/caugi>
* Email: <mailto:frederik@fabriciusbjerre.dk>
* GitHub mirror: <https://github.com/cran/caugi>

Run `revdepcheck::cloud_details(, "caugi")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘caugi-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: read_graphml
     > ### Title: Read GraphML File to caugi Graph
     > ### Aliases: read_graphml
     > 
     > ### ** Examples
     > 
     > # Create and export a graph
     > cg <- caugi(
     +   A %-->% B,
     +   B %-->% C,
     +   class = "DAG"
     + )
     > 
     > tmp <- tempfile(fileext = ".graphml")
     > write_graphml(cg, tmp)
     Error in S7::new_object(caugi_export, content = content, format = "graphml") : 
       `_parent` must be an instance of <caugi::caugi_export>, not <S7_class>.
     Calls: write_graphml ... caugi_graphml -> <Anonymous> -> check_parent -> stop2
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        4.       └─S7:::check_parent(`_parent`, class)
        5.         └─S7:::stop2(msg, call = call)
       ── Error ('test-format-mermaid.R:166:3'): to_mermaid handles --o edges (#307) ──
       <error/condition>
       Error in `S7::new_object(caugi_export, content = content, format = "mermaid")`: `_parent` must be an instance of <caugi::caugi_export>, not <S7_class>.
       Backtrace:
           ▆
        1. └─caugi::to_mermaid(cg) at test-format-mermaid.R:166:3
        2.   └─caugi::caugi_mermaid(content = result)
        3.     └─S7::new_object(caugi_export, content = content, format = "mermaid")
        4.       └─S7:::check_parent(`_parent`, class)
        5.         └─S7:::stop2(msg, call = call)
       ── Error ('test-plot-composition.R:203:3'): plot composition branches are covered ──
       <error/condition>
       Error in `S7::method(S7:::as_generic(`+`), list(caugi_plot, caugi_plot))`: `generic` must be a <S7_generic>, not a S3<S7_generic_sentinel/S7_external_generic>.
       Backtrace:
           ▆
        1. └─S7::method(S7:::as_generic(`+`), list(caugi_plot, caugi_plot)) at test-plot-composition.R:203:3
        2.   └─S7::check_is_S7(generic, S7_generic)
        3.     └─S7:::stop2(msg, call = call)
       
       [ FAIL 38 | WARN 0 | SKIP 0 | PASS 3390 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# cohortBuilder (1.0.0)

* GitHub: <https://github.com/r-world-devs/cohortBuilder>
* Email: <mailto:krystian8207@gmail.com>
* GitHub mirror: <https://github.com/cran/cohortBuilder>

Run `revdepcheck::cloud_details(, "cohortBuilder")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     ℹ With name: iris.Sepal.Length.
     Caused by error in `S7::new_object()` at cohortBuilder/R/filter.R:188:5:
     ! `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Backtrace:
          ▆
       1. ├─cohortBuilder::autofilter(set_source(tblist(iris = iris)))
       2. ├─cohortBuilder:::autofilter.tblist(set_source(tblist(iris = iris))) at cohortBuilder/R/source_methods.R:582:3
       3. │ ├─base::unname(...) at cohortBuilder/R/source_tblist.R:1588:3
       4. │ └─purrr::map(...) at cohortBuilder/R/source_tblist.R:1588:3
       5. │   └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
       6. │     ├─purrr:::with_indexed_errors(...)
       7. │     │ └─base::withCallingHandlers(...)
       8. │     ├─purrr:::call_with_cleanup(...)
       9. │     └─cohortBuilder (local) .f(.x[[i]], ...)
      10. │       ├─base::do.call(cohortBuilder::filter, .)
      11. │       └─cohortBuilder (local) `<fn>`(...)
      12. │         └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
      13. │           └─S7::new_object(...) at cohortBuilder/R/filter.R:188:5
      14. │             └─S7:::check_parent(`_parent`, class)
      15. │               └─S7:::stop2(msg, call = call)
      16. │                 └─base::stop(...)
      17. └─purrr (local) `<fn>`(`<error>`)
      18.   └─cli::cli_abort(...)
      19.     └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        2.   └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
        3.     └─S7::new_object(...) at cohortBuilder/R/filter.R:125:5
        4.       └─S7:::check_parent(`_parent`, class)
        5.         └─S7:::stop2(msg, call = call)
       
       [ FAIL 78 | WARN 1 | SKIP 0 | PASS 133 ]
       Deleting unused snapshots:
       'source_tblist/date-range-breaks-argument-is-passed-properly.svg',
       'source_tblist/date-range-extra-args-work.svg',
       'source_tblist/date-range-no-data-case-works.svg',
       'source_tblist/datetime-range-breaks-arg-works.svg',
       'source_tblist/datetime-range-default-breaks-work.svg',
       'source_tblist/datetime-range-extra-args-work.svg',
       'source_tblist/datetime-range-no-data-case-works.svg',
       'source_tblist/discrete-extra-args-work.svg',
       'source_tblist/discrete-no-data-case-works.svg',
       'source_tblist/multi-discrete-extra-args-work.svg',
       'source_tblist/multi-discrete-no-data-case-works.svg',
       'source_tblist/multi-discrete-standard-call-works.svg',
       'source_tblist/range-breaks-argument-is-passed-properly.svg',
       'source_tblist/range-extra-args-work.svg', and
       'source_tblist/range-no-data-case-works.svg'
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
       7.       │ └─base::withCallingHandlers(...)
       8.       ├─purrr:::call_with_cleanup(...)
       9.       └─cohortBuilder (local) .f(.x[[i]], ...)
      10.         ├─base::do.call(cohortBuilder::filter, .)
      11.         └─cohortBuilder (local) `<fn>`(...)
      12.           └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
      13.             └─S7::new_object(...) at cohortBuilder/R/filter.R:188:5
      14.               └─S7:::check_parent(`_parent`, class)
      15.                 └─S7:::stop2(msg, call = call)
      16.                   └─base::stop(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'source-intelligence.Rmd' failed with diagnostics:
     ℹ In index: 1.
     ℹ With name: iris.Sepal.Length.
     Caused by error in `S7::new_object()` at cohortBuilder/R/filter.R:188:5:
     ! `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     --- failed re-building ‘source-intelligence.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘binding-keys.Rmd’ ‘cohort-configuration.Rmd’ ‘cohortBuilder.Rmd’
       ‘custom-filters.Rmd’ ‘managing-cohort.Rmd’ ‘source-intelligence.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# covr (3.6.5)

* GitHub: <https://github.com/r-lib/covr>
* Email: <mailto:james.f.hester@gmail.com>
* GitHub mirror: <https://github.com/cran/covr>

Run `revdepcheck::cloud_details(, "covr")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > test-print.R: TestPrint Coverage: 0.00%
       > test-print.R: TestPrint Coverage: 0.00%
       > test-print.R: TestPrint Coverage: 66.67%
       [ FAIL 1 | WARN 0 | SKIP 12 | PASS 367 ]
       
       ══ Skipped tests (12) ══════════════════════════════════════════════════════════
       • On CRAN (10): 'test-Compiled.R:2:3', 'test-Compiled.R:35:3',
         'test-Compiled.R:48:3', 'test-Compiled.R:64:3', 'test-Compiled.R:80:3',
         'test-Compiled.R:87:3', 'test-Compiled.R:97:3', 'test-S7.R:1:1',
         'test-record_tests.R:165:3', 'test-utils.R:44:3'
       • empty test (1):
       • getRversion() >= "4.0.0" is TRUE (1): 'test-utils.R:35:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-S7.R:5:3'): S7 coverage is reported ──────────────────────────
       Expected `cov$value` to equal `c(1, 1, 1, 2, 5, 0, 5, 0, 5, 1, 1, 2, 1, 1, 0)`.
       Differences:
         `actual[1:13]`: 0.0 0.0 0.0 2.0 0.0 0.0 0.0 0.0 0.0 0.0 and 3 more...
       `expected[1:13]`: 1.0 1.0 1.0 2.0 5.0 0.0 5.0 0.0 5.0 1.0           ...
       
       
       [ FAIL 1 | WARN 0 | SKIP 12 | PASS 367 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# dcmstan (0.1.0)

* GitHub: <https://github.com/r-dcm/dcmstan>
* Email: <mailto:wjakethompson@gmail.com>
* GitHub mirror: <https://github.com/cran/dcmstan>

Run `revdepcheck::cloud_details(, "dcmstan")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     4     0     0     1
     5     1     1     0
     6     1     0     1
     7     0     1     1
     8     1     1     1
     > 
     > create_profiles(5)
     # A tibble: 32 × 5
         att1  att2  att3  att4  att5
        <int> <int> <int> <int> <int>
      1     0     0     0     0     0
      2     1     0     0     0     0
      3     0     1     0     0     0
      4     0     0     1     0     0
      5     0     0     0     1     0
      6     0     0     0     0     1
      7     1     1     0     0     0
      8     1     0     1     0     0
      9     1     0     0     1     0
     10     1     0     0     0     1
     # ℹ 22 more rows
     > 
     > create_profiles(unconstrained(), attributes = c("att1", "att2"))
     Error: @model is read-only
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        3. │   └─S7::S7_inherits(x, class)
        4. │     └─S7:::class_inherits(x, class)
        5. ├─dcmstan::dino()
        6. │ └─dcmstan:::DINO(model = "dino")
        7. │   └─S7::new_object(...)
        8. │     └─S7::`prop<-`(`*tmp*`, name, check = FALSE, value = prop_setter_vals[[name]])
        9. └─dcmstan (local) `<DINO>@model`(`<dc::DINO>`, "dino")
       ── Error ('test-zzz-methods-stan-data.R:179:3'): hdcm data objects are correct ──
       Error: @model is read-only
       Backtrace:
           ▆
        1. ├─dcmstan::dcm_specify(...) at test-zzz-methods-stan-data.R:179:3
        2. │ └─S7::check_is_S7(measurement_model, measurement)
        3. │   └─S7::S7_inherits(x, class)
        4. │     └─S7:::class_inherits(x, class)
        5. ├─dcmstan::lcdm()
        6. │ └─dcmstan:::LCDM(model = "lcdm", list(max_interaction = max_interaction))
        7. │   └─S7::new_object(...)
        8. │     └─S7::`prop<-`(`*tmp*`, name, check = FALSE, value = prop_setter_vals[[name]])
        9. └─dcmstan (local) `<LCDM>@model`(`<dc::LCDM>`, "lcdm")
       
       [ FAIL 62 | WARN 0 | SKIP 4 | PASS 76 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Error:
     ! @model is read-only
     ---
     Backtrace:
         ▆
      1. ├─dcmstan::dcm_specify(...)
      2. │ └─S7::check_is_S7(measurement_model, measurement)
      3. │   └─S7::S7_inherits(x, class)
      4. │     └─S7:::class_inherits(x, class)
      5. ├─dcmstan::dina()
      6. │ └─dcmstan:::DINA(model = "dina")
      7. │   └─S7::new_object(...)
      8. │     └─S7::`prop<-`(`*tmp*`, name, check = FALSE, value = prop_setter_vals[[name]])
      9. └─dcmstan (local) `<DINA>@model`(`<dc::DINA>`, "dina")
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'dcmstan.Rmd' failed with diagnostics:
     @model is read-only
     --- failed re-building ‘dcmstan.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘dcmstan.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# deltapif (0.4.5)

* Email: <mailto:rzepeda17@gmail.com>
* GitHub mirror: <https://github.com/cran/deltapif>

Run `revdepcheck::cloud_details(, "deltapif")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘deltapif-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: as.data.frame
     > ### Title: Transform an object into a data.frame
     > ### Aliases: as.data.frame
     > 
     > ### ** Examples
     > 
     > #Transform one pif
     > my_pif <- pif(p = 0.5, p_cft = 0.25, beta = 1.3, var_p = 0.1,
     +     var_beta = 0.2, label = "My pif")
     Error in S7::new_object(S7::S7_object(), conf_level = conf_level, type = type,  : 
       `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
     Calls: pif ... pif_atomic_class -> <Anonymous> -> check_parent -> stop2
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           ▆
        1. └─deltapif (local) make_paf(label = "te1") at test_covariance_generics_2.R:311:3
        2.   └─deltapif::paf(...) at test_covariance_generics_2.R:14:3
        3.     └─deltapif::pif(...)
        4.       └─deltapif:::pif_atomic_class(...)
        5.         └─S7::new_object(...)
        6.           └─S7:::check_parent(`_parent`, class)
        7.             └─S7:::stop2(msg, call = call)
       ── Error ('test_covariance_generics_2.R:318:3'): covariance is consistent across hierarchy levels ──
       <error/condition>
       Error in `S7::new_object(S7::S7_object(), conf_level = conf_level, type = type, label = label, link = link, link_inv = link_inv, link_deriv = link_deriv, p = p, p_cft = p_cft, beta = beta, var_p = var_p, var_beta = var_beta, rr_link = rr_link, rr_link_deriv = rr_link_deriv, upper_bound_p = upper_bound_p, upper_bound_beta = upper_bound_beta)`: `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
       Backtrace:
           ▆
        1. └─deltapif (local) make_paf(label = "h1") at test_covariance_generics_2.R:318:3
        2.   └─deltapif::paf(...) at test_covariance_generics_2.R:14:3
        3.     └─deltapif::pif(...)
        4.       └─deltapif:::pif_atomic_class(...)
        5.         └─S7::new_object(...)
        6.           └─S7:::check_parent(`_parent`, class)
        7.             └─S7:::stop2(msg, call = call)
       
       [ FAIL 271 | WARN 0 | SKIP 0 | PASS 305 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Quitting from Introduction.Rmd:61-65 [unnamed-chunk-2]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `S7::new_object()`:
     ! `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
     ---
     Backtrace:
         ▆
      1. └─deltapif::paf(p = 0.085, beta = log(1.59), quiet = TRUE)
      2.   └─deltapif::pif(...)
      3.     └─deltapif:::pif_atomic_class(...)
      4.       └─S7::new_object(...)
      5.         └─S7:::check_parent(`_parent`, class)
      6.           └─S7:::stop2(msg, call = call)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
     `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
     --- failed re-building ‘Introduction.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘Examples.Rmd’ ‘Introduction.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

*   checking R code for possible problems ... NOTE
     ```
     change_link: no visible global function definition for ‘coef’
     pif: no visible global function definition for ‘coef’
     pif_ensemble: no visible global function definition for ‘coef’
     pif_total: no visible global function definition for ‘coef’
     weighted_adjusted_fractions : <anonymous>: no visible global function
       definition for ‘coef’
     weighted_adjusted_fractions: no visible global function definition for
       ‘coef’
     Undefined global functions or variables:
       coef
     Consider adding
       importFrom("stats", "coef")
     to your NAMESPACE file.
     ```

# ellmer (0.5.0)

* GitHub: <https://github.com/tidyverse/ellmer>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/ellmer>

Run `revdepcheck::cloud_details(, "ellmer")` for more info

## Newly broken

*   checking whether package ‘ellmer’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See ‘/tmp/workdir/ellmer/new/ellmer.Rcheck/00install.out’ for details.
     ```

*   checking for code/documentation mismatches ... WARNING
     ```
     Codoc mismatches from Rd file 'Turn.Rd':
     AssistantPartialTurn
       Code: function(..., reason = "interrupted")
       Docs: function(contents = list(), json = list(), tokens = c(NA_real_,
                      NA_real_, NA_real_), cost = NA_real_, duration =
                      NA_real_, finish_reason = NA_character_, reason =
                      "interrupted")
       Argument names in code not in docs:
         ...
       Argument names in docs not in code:
         contents json tokens cost duration finish_reason
       Mismatches in argument names:
         Position: 1 Code: ... Docs: contents
         Position: 2 Code: reason Docs: json
     ```

## In both

*   checking tests ... ERROR
     ```
     ...
         7.             ├─coro::is_exhausted(new <- .x())
         8.             └─coro (local) .x()
         9.               └─base::evalq(...)
        10.                 └─base::evalq(...)
        11.                   └─base::evalq(...)
        12.                     └─base::evalq(...)
        13.                       ├─coro::is_exhausted(elt <- iterator())
        14.                       └─coro (local) iterator()
        15.                         └─base::evalq(...)
        16.                           └─base::evalq(...)
        17.                             ├─base::evalq(...)
        18.                             │ └─base::evalq(...)
        19.                             │   └─coro (local) user(...)
        20.                             │     ├─.last_value <<- eval_bare(substitute(expr), user_env)
        21.                             │     │ └─rlang::env_poke(env, lhs, value, inherit = TRUE, create = FALSE)
        22.                             │     └─rlang::eval_bare(substitute(expr), user_env)
        23.                             └─ellmer:::chat_perform(...)
        24.                               └─httr2::req_perform(req)
        25.                                 └─httr2:::handle_resp(req, resp, error_call = error_call)
        26.                                   └─rlang::cnd_signal(resp)
       
       [ FAIL 11 | WARN 0 | SKIP 290 | PASS 1253 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# filtro (0.2.0)

* GitHub: <https://github.com/tidymodels/filtro>
* Email: <mailto:franceslinyc@gmail.com>
* GitHub mirror: <https://github.com/cran/filtro>

Run `revdepcheck::cloud_details(, "filtro")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     + 
     + # Arrange score
     + ames_aov_pval_res |> arrange_score()
     + ## Don't show: 
     + }) # examplesIf
     > library(dplyr)
     
     Attaching package: ‘dplyr’
     
     The following objects are masked from ‘package:stats’:
     
         filter, lag
     
     The following objects are masked from ‘package:base’:
     
         intersect, setdiff, setequal, union
     
     > ames_subset <- dplyr::select(modeldata::ames, Sale_Price, MS_SubClass, 
     +     MS_Zoning, Lot_Frontage, Lot_Area, Street)
     > ames_subset <- dplyr::mutate(ames_subset, Sale_Price = log10(Sale_Price))
     > ames_aov_pval_res <- fit(score_aov_pval, Sale_Price ~ ., data = ames_subset)
     Error in fit(score_aov_pval, Sale_Price ~ ., data = ames_subset) : 
       could not find function "fit"
     Calls: <Anonymous> -> source -> withVisible -> eval -> eval
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
       
       
       processing file: forestimp.qmd
       [31mError in `fit()`:[39m
       ! could not find function "fit"
       
       Quitting from forestimp.qmd:86-94 [unnamed-chunk-5]
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[31m~~~~~~~~~~~[39m
       <error/rlang_error>
       Error in `fit()`:
       ! could not find function "fit"
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[31m~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[39m
       
       [31mExecution halted[39m
       [33mWARN: Error encountered when rendering files[39m
       
     Caused by error in `processx::run()`:
     ! System command 'quarto' failed
     --- failed re-building ‘forestimp.qmd’
     
     SUMMARY: processing the following files failed:
       ‘filtro.qmd’ ‘forestimp.qmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# fr (0.5.2)

* GitHub: <https://github.com/cole-brokamp/fr>
* Email: <mailto:cole@colebrokamp.com>
* GitHub mirror: <https://github.com/cran/fr>

Run `revdepcheck::cloud_details(, "fr")` for more info

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
     Error in (function (.data = list(), row.names = NULL, name = character(0),  : 
       <fr_tdr> object is invalid:
     - All columns and row names must have the same length
     Calls: as_data_frame ... <Anonymous> -> <Anonymous> -> validate_from -> stop2
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        3.     └─S7::new_object(...)
        4.       └─S7:::validate_from(...)
        5.         └─S7:::stop2(msg, call = call, class = "S7_error_validation_failed")
       ── Error ('test-write_fr_tdr.R:3:3'): write_fr_tdr works ───────────────────────
       <S7_error_validation_failed/error/condition>
       Error in `(structure(function (.data = list(), row.names = NULL, name = character(0), path = character(0), version = character(0), title = character(0), homepage = character(0), description = character(0), schema = (function (fields = list(), missingValues = character(0), primaryKey = character(0), foreignKeys = character(0))  {     fields     missingValues     primaryKey     foreignKeys     S7::new_object(S7::S7_object(), fields = fields, missingValues = missingValues, primaryKey = primaryKey, foreignKeys = foreignKeys) })())  S7::new_object(new_data.frame(.data = .data, row.names = row.names), name = name, path = path, version = version, title = title, homepage = homepage, description = description, schema = schema), class = c("S7_class", "S7_object"), name = "fr_tdr", parent = structure(list(class = "data.frame", constructor = structure(function (.data = list(), row.names = NULL)  {     if (is.null(row.names)) {         list2DF(.data)     }     else {         out <- list2DF(.data, length(row.names))         attr(out, "row.names") <- row.names         out     } }, class = "S7_constructor"), validator = function (self)  {     if (!is.list(self)) {         return("Underlying data must be a <list>")     }     if (length(self) >= 1) {         col_lengths <- vapply(self, NROW, integer(1L), USE.NAMES = FALSE)         ns <- unique(c(col_lengths, .row_names_info(self, 2L)))         if (length(ns) > 1) {             return("All columns and row names must have the same length")         }         if (is.null(names(self))) {             return("Underlying data must be named")         }     } }, abstract = FALSE, default = data.frame(), attributes = c("names", "row.names")), class = "S7_S3_class"), properties = list(name = structure(list(name = "name", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), path = structure(list(name = "path", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), version = structure(list(name = "version", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), title = structure(list(name = "title", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), homepage = structure(list(name = "homepage", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), description = structure(list(name = "description", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), schema = structure(list(name = "schema", class = structure(function (fields = list(), missingValues = character(0), primaryKey = character(0), foreignKeys = character(0))  {     fields     missingValues     primaryKey     foreignKeys     S7::new_object(S7::S7_object(), fields = fields, missingValues = missingValues, primaryKey = primaryKey, foreignKeys = foreignKeys) }, class = c("S7_class", "S7_object"), name = "fr_schema", parent = structure(function ()  {     .Call(S7_object_) }, name = "S7_object", properties = list(), abstract = FALSE, constructor = function ()  {     .Call(S7_object_) }, validator = function (self)  {     if (!is_S7_type(self)) {         "Underlying data is corrupt"     } }, S7_class_name = "S7_object", S7_dispatch = "S7_object", S7_version = 1L, class = c("S7_class", "S7_object")), properties = list(fields = structure(list(name = "fields", class = structure(list(class = "list", constructor_name = "list", constructor = function (.data = list())  .data, validator = function (object)  if (base_class(object) != "list") {     sprintf("Underlying data must be <%s> not <%s>", "list", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), missingValues = structure(list(name = "missingValues", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), primaryKey = structure(list(name = "primaryKey", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property"), foreignKeys = structure(list(name = "foreignKeys", class = structure(list(class = "character", constructor_name = "character", constructor = function (.data = character(0))  .data, validator = function (object)  if (base_class(object) != "character") {     sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object)) }), class = "S7_base_class"), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = structure(function (fields = list(), missingValues = character(0), primaryKey = character(0), foreignKeys = character(0))  {     fields     missingValues     primaryKey     foreignKeys     S7::new_object(S7::S7_object(), fields = fields, missingValues = missingValues, primaryKey = primaryKey, foreignKeys = foreignKeys) }, class = "S7_constructor"), validator = function (self)  {     if (!length(self@fields) > 0) {         "@fields must have at least one field"     }     else if (!all(sapply(self@fields, is_fr_field))) {         "all items in @fields should be fr_field objects"     } }, S7_class_name = "fr_schema", S7_dispatch = c("fr_schema", "S7_object"), S7_version = 1L), getter = NULL, setter = NULL, validator = NULL, default = NULL), class = "S7_property")), abstract = FALSE, constructor = structure(function (.data = list(), row.names = NULL, name = character(0), path = character(0), version = character(0), title = character(0), homepage = character(0), description = character(0), schema = (function (fields = list(), missingValues = character(0), primaryKey = character(0),      foreignKeys = character(0))  {     fields     missingValues     primaryKey     foreignKeys     S7::new_object(S7::S7_object(), fields = fields, missingValues = missingValues, primaryKey = primaryKey, foreignKeys = foreignKeys) })())  S7::new_object(new_data.frame(.data = .data, row.names = row.names), name = name, path = path, version = version, title = title, homepage = homepage, description = description, schema = schema), class = "S7_constructor"), validator = function (self)  {     if (length(self@name) != 1) {         "@name must be length 1"     }     else if (!all(names(self@schema@fields) %in% names(S7::S7_data(self)))) {         "not all fields in the schema are columns in the data frame"     }     else if (!all(names(S7::S7_data(self)) %in% names(self@schema@fields))) {         "not all columns in the data frame are fields in the schema"     } }, S7_class_name = "fr_tdr", S7_dispatch = c("fr_tdr", "data.frame", "S7_object"), S7_version = 1L))(structure(list(mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 19.7, 15, 21.4), cyl = c(6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8, 8, 8, 8, 4, 4, 4, 8, 6, 8, 4), disp = c(160, 160, 108, 258, 360, 225, 360, 146.7, 140.8, 167.6, 167.6, 275.8, 275.8, 275.8,  472, 460, 440, 78.7, 75.7, 71.1, 120.1, 318, 304, 350, 400, 79, 120.3, 95.1, 351, 145, 301, 121), hp = c(110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150, 150, 245, 175, 66, 91, 113, 264, 175, 335, 109), drat = c(3.9, 3.9, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92, 3.07, 3.07, 3.07, 2.93, 3, 3.23, 4.08, 4.93, 4.22, 3.7, 2.76, 3.15, 3.73, 3.08, 4.08, 4.43, 3.77, 4.22, 3.62, 3.54, 4.11), wt = c(2.62, 2.875, 2.32, 3.215, 3.44, 3.46, 3.57,  3.19, 3.15, 3.44, 3.44, 4.07, 3.73, 3.78, 5.25, 5.424, 5.345, 2.2, 1.615, 1.835, 2.465, 3.52, 3.435, 3.84, 3.845, 1.935, 2.14, 1.513, 3.17, 2.77, 3.57, 2.78), qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20, 22.9, 18.3, 18.9, 17.4, 17.6, 18, 17.98, 17.82, 17.42, 19.47, 18.52, 19.9, 20.01, 16.87, 17.3, 15.41, 17.05, 18.9, 16.7, 16.9, 14.5, 15.5, 14.6, 18.6), vs = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1), am = c(1, 1, 1, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), gear = c(4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 5, 5, 5, 5, 5, 4), carb = c(4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2, 2, 4, 2, 1, 2, 2, 4, 6, 8, 2)), row.names = c(NA, -32L), class = c("tbl_df", "tbl", "data.frame")), name = "my_mtcars", schema = <object>)`: <fr_tdr> object is invalid:
       - All columns and row names must have the same length
       Backtrace:
           ▆
        1. ├─fr::write_fr_tdr(as_fr_tdr(mtcars, name = "my_mtcars"), dir = tempdir()) at test-write_fr_tdr.R:3:3
        2. └─fr::as_fr_tdr(mtcars, name = "my_mtcars")
        3.   ├─S7::S7_dispatch()
        4.   └─fr (local) `method(as_fr_tdr, new_S3_class("data.frame"))`(...)
        5.     ├─base::do.call(fr_tdr, dots)
        6.     └─fr (local) `<S7_class>`(`<tibble[,11]>`, name = "my_mtcars", schema = `<fr_schem>`)
        7.       └─S7::new_object(...)
        8.         └─S7:::validate_from(...)
        9.           └─S7:::stop2(msg, call = call, class = "S7_error_validation_failed")
       
       [ FAIL 13 | WARN 0 | SKIP 2 | PASS 20 ]
       Deleting unused snapshots: 'write_fr_tdr/my_mtcars.csv' and
       'write_fr_tdr/tabular-data-resource.yaml'
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `fr_tdr()`:
     ! <fr_tdr> object is invalid:
     - All columns and row names must have the same length
     ---
     Backtrace:
         ▆
      1. └─fr::read_fr_tdr(fs::path_package("fr", "hamilton_poverty_2020"))
      2.   └─fr:::fr_tdr(...)
      3.     └─S7::new_object(...)
      4.       └─S7:::validate_from(...)
      5.         └─S7:::stop2(msg, call = call, class = "S7_error_validation_failed")
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'read_fr_tdr.Rmd' failed with diagnostics:
     <fr_tdr> object is invalid:
     - All columns and row names must have the same length
     --- failed re-building ‘read_fr_tdr.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘creating_a_tabular-data-resource.Rmd’ ‘read_fr_tdr.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
     ```
       Missing dependency on R >= 4.1.0 because package code uses the pipe
       |> or function shorthand \(...) syntax added in R 4.1.0.
       File(s) using such syntax:
         ‘as_data_frame.Rd’ ‘as_list.Rd’ ‘dplyr_methods.Rd’ ‘fr_schema.R’
         ‘fr_tdr.R’ ‘read_fr_tdr.R’ ‘update_field.R’ ‘update_field.Rd’
     ```

# GGally (2.4.0)

* GitHub: <https://github.com/ggobi/ggally>
* Email: <mailto:schloerke@gmail.com>
* GitHub mirror: <https://github.com/cran/GGally>

Run `revdepcheck::cloud_details(, "GGally")` for more info

## Newly broken

*   checking whether package ‘GGally’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘GGally’
     See ‘/tmp/workdir/GGally/new/GGally.Rcheck/00install.out’ for details.
     ```

# ggarrow (0.2.0)

* GitHub: <https://github.com/teunbrand/ggarrow>
* Email: <mailto:tahvdbrand@gmail.com>
* GitHub mirror: <https://github.com/cran/ggarrow>

Run `revdepcheck::cloud_details(, "ggarrow")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     +   geom_path() +
     +   theme(
     +     # Proper arrow with variable width for x-axis line
     +     axis.line.x = element_arrow(
     +       arrow_head = "head_wings", linewidth_head = 2, linewidth_fins = 0
     +     ),
     +     # Just a variable width line for the y-axis line
     +     axis.line.y = element_arrow(linewidth_head = 0, linewidth_fins = 5,
     +                                 lineend = "round"),
     +     # Arrows for the y-axis ticks
     +     axis.ticks.y = element_arrow(arrow_fins = arrow_head_line(angle = 45)),
     +     # Variable width lines for the x-axis ticks
     +     axis.ticks.x = element_arrow(linewidth_head = 3, linewidth_fins = 0),
     +     axis.ticks.length = unit(0.5, 'cm'),
     +     # Arrows for major panel grid
     +     panel.grid.major = element_arrow(
     +       arrow_head = "head_wings", arrow_fins = "fins_feather", length = 10
     +     ),
     +     # Shortened lines for the minor panel grid
     +     panel.grid.minor = element_arrow(resect = 20)
     +   )
     Error in S7::new_object(.parent = parent, linewidth_head = linewidth_head,  : 
       argument "_parent" is missing, with no default
     Calls: theme -> find_args -> mget -> element_arrow -> <Anonymous>
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         'test-linejoins.R:30:1', 'test-linejoins.R:47:1', 'test-linejoins.R:63:1',
         'test-scale_arrow.R:103:1', 'test-scale_arrow.R:117:1',
         'test-scale_arrow.R:132:1', 'test-scale_arrow.R:147:1',
         'test-scale_arrow.R:163:1', 'test-scale_arrow.R:180:1',
         'test-scale_arrow.R:197:1', 'test-shaping.R:11:1', 'test-shaping.R:23:1',
         'test-shaping.R:37:1', 'test-shaping.R:51:1', 'test-shaping.R:67:1',
         'test-shaping.R:87:1', 'test-shaping.R:102:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test-theme_elements.R:2:3'): arrow theme elements looks like they should ──
       <getvarError/missingArgError/error/condition>
       Error in `S7::new_object(.parent = parent, linewidth_head = linewidth_head, linewidth_fins = linewidth_fins, stroke_colour = stroke_colour, stroke_width = stroke_width, arrow_head = arrow_head, arrow_fins = arrow_fins, arrow_mid = arrow_mid, length = length, length_head = length_head, length_mid = length_mid, length_fins = length_fins, resect = resect, resect_head = resect_head, resect_fins = resect_fins, justify = justify, force_arrow = force_arrow, mid_place = mid_place, linemitre = linemitre, distort = distort)`: argument "_parent" is missing, with no default
       Backtrace:
           ▆
        1. ├─ggplot2::theme(...) at test-theme_elements.R:2:3
        2. │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
        3. │   └─base::mget(args, envir = env)
        4. └─ggarrow::element_arrow(linewidth_head = 3, linewidth_fins = 0)
        5.   └─S7::new_object(...)
       
       [ FAIL 1 | WARN 0 | SKIP 35 | PASS 39 ]
       Deleting unused snapshots: 'theme_elements/theme-lines-as-arrows.svg'
       Error:
       ! Test failures.
       Execution halted
     ```

# ggdiagram (0.2.0)

* GitHub: <https://github.com/wjschne/ggdiagram>
* Email: <mailto:w.joel.schneider@gmail.com>
* GitHub mirror: <https://github.com/cran/ggdiagram>

Run `revdepcheck::cloud_details(, "ggdiagram")` for more info

## Newly broken

*   checking whether package ‘ggdiagram’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/ggdiagram/new/ggdiagram.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘ggdiagram’ ...
** this is package ‘ggdiagram’ version ‘0.2.0’
** package ‘ggdiagram’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in S7::new_property(class = ob_point, default = ob_point(0, 0)) :
  `default` should be a scalar or a quoted call, not a <ggdiagram::ob_point>.
* Did you mean `default = quote(ob_point(0, 0))`?
* This warning will become an error in a future release.
Error : .onLoad failed in loadNamespace() for 'ggforce', details:
  call: S7::S7_data(x)
  error: `object` must be an <S7_object>, not a S3<ggplot2::mapping/uneval/gg/S7_object>.
Error: unable to load R code in package ‘ggdiagram’
Execution halted
ERROR: lazy loading failed for package ‘ggdiagram’
* removing ‘/tmp/workdir/ggdiagram/new/ggdiagram.Rcheck/ggdiagram’


```
### CRAN

```
* installing *source* package ‘ggdiagram’ ...
** this is package ‘ggdiagram’ version ‘0.2.0’
** package ‘ggdiagram’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggdiagram)


```
# gglogger (0.1.8)

* GitHub: <https://github.com/pwwang/gglogger>
* Email: <mailto:pwwang@pwwang.com>
* GitHub mirror: <https://github.com/cran/gglogger>

Run `revdepcheck::cloud_details(, "gglogger")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
           ▆
        1. └─p$logs$evaluate(env) at test-gglogger.R:101:5
        2.   └─base::lapply(logs, function(log) log$evaluate(envir))
        3.     └─gglogger (local) FUN(X[[i]], ...)
        4.       └─log$evaluate(envir)
        5.         └─base::eval(parse(text = code), envir = envir)
        6.           └─base::eval(parse(text = code), envir = envir)
       ── Failure ('test-gglogger.R:117:5'): gglogger gen_code works ──────────────────
       Expected `code` to equal "library(ggplot2)\n\nggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))\n".
       Differences:
       1/1 mismatches
       x[1]: "library(ggplot2)\n\nggplot2::ggplot(ggplot2::mpg) +\n  e2\n"
       y[1]: "library(ggplot2)\n\nggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x =
       y[1]:  displ, y = hwy))\n"
       ── Failure ('test-gglogger.R:124:5'): gglogger stringify works ─────────────────
       Expected `code` to equal "ggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))".
       Differences:
       1/1 mismatches
       x[1]: "ggplot2::ggplot(ggplot2::mpg) +\n  e2"
       y[1]: "ggplot2::ggplot(ggplot2::mpg) +\n  geom_point(aes(x = displ, y = hwy))"
       
       [ FAIL 25 | WARN 0 | SKIP 0 | PASS 33 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# ggpath (1.1.1)

* GitHub: <https://github.com/mrcaseb/ggpath>
* Email: <mailto:mrcaseb@gmail.com>
* GitHub mirror: <https://github.com/cran/ggpath>

Run `revdepcheck::cloud_details(, "ggpath")` for more info

## Newly broken

*   checking whether package ‘ggpath’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/ggpath/new/ggpath.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘ggpath’ ...
** this is package ‘ggpath’ version ‘1.1.1’
** package ‘ggpath’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in S7::new_class("element_path", parent = element_text, properties = list(alpha = S7::new_property(S7::class_numeric),  : 
  <element_path>@size must narrow <ggplot2::element_text>@size.
- <ggplot2::element_text>@size is <NULL>, <integer>, or <double>.
- <element_path>@size is <integer>, <double>, or S3<simpleUnit/unit/unit_v2>.
Error: unable to load R code in package ‘ggpath’
Execution halted
ERROR: lazy loading failed for package ‘ggpath’
* removing ‘/tmp/workdir/ggpath/new/ggpath.Rcheck/ggpath’


```
### CRAN

```
* installing *source* package ‘ggpath’ ...
** this is package ‘ggpath’ version ‘1.1.1’
** package ‘ggpath’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggpath)


```
# ggplot2 (4.0.3)

* GitHub: <https://github.com/tidyverse/ggplot2>
* Email: <mailto:thomas.pedersen@posit.co>
* GitHub mirror: <https://github.com/cran/ggplot2>

Run `revdepcheck::cloud_details(, "ggplot2")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       
       [ FAIL 1 | WARN 0 | SKIP 270 | PASS 1781 ]
       Deleting unused snapshots:
       'geom-dotplot/2-na-values-bin-along-y-stack-center.svg',
       'geom-dotplot/2-na-values-dot-density-binning-binwidth-4.svg',
       'geom-dotplot/3-stackgroups-bin-y-histodot.svg',
       'geom-dotplot/3-stackgroups-dot-density-with-aligned-bins.svg',
       'geom-dotplot/3-stackgroups-histodot.svg',
       'geom-dotplot/basic-dotplot-with-dot-density-binning-binwidth-4.svg',
       'geom-dotplot/bin-along-y-stack-center.svg',
       'geom-dotplot/bin-along-y-stack-centerwhole-histodot.svg',
       'geom-dotplot/bin-along-y-stack-centerwhole.svg',
       'geom-dotplot/bin-x-three-y-groups-stack-centerwhole.svg',
       'geom-dotplot/bin-y-continous-x-axis-grouping-by-x.svg',
       'geom-dotplot/bin-y-continous-x-axis-single-x-group.svg',
       'geom-dotplot/bin-y-dodged-coord-flip.svg', 'geom-dotplot/bin-y-dodged.svg',
       'geom-dotplot/bin-y-dodging-3-stackgroups-histodot.svg',
       'geom-dotplot/bin-y-three-x-groups-bins-aligned-across-groups.svg',
       'geom-dotplot/bin-y-three-x-groups-bins-aligned-coord-flip.svg',
       'geom-dotplot/bin-y-three-x-groups-fill-and-dodge.svg', …,
       'theme/vertical-legends-placed-apart.svg', and
       'theme/width-is-3-times-height.svg'
       Error:
       ! Test failures.
       Execution halted
     ```

# ggplotplus (0.5.7)

* GitHub: <https://github.com/MAISRC/ggplotplus>
* Email: <mailto:bajcz003@umn.edu>
* GitHub mirror: <https://github.com/cran/ggplotplus>

Run `revdepcheck::cloud_details(, "ggplotplus")` for more info

## Newly broken

*   checking whether package ‘ggplotplus’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/ggplotplus/new/ggplotplus.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘ggplotplus’ ...
** this is package ‘ggplotplus’ version ‘0.5.7’
** package ‘ggplotplus’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error : Package 'ggplot2' must export `ggplot` as an S7 class.
Error: unable to load R code in package ‘ggplotplus’
Execution halted
ERROR: lazy loading failed for package ‘ggplotplus’
* removing ‘/tmp/workdir/ggplotplus/new/ggplotplus.Rcheck/ggplotplus’


```
### CRAN

```
* installing *source* package ‘ggplotplus’ ...
** this is package ‘ggplotplus’ version ‘0.5.7’
** package ‘ggplotplus’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggplotplus)


```
# ggside (0.4.1)

* GitHub: <https://github.com/jtlandis/ggside>
* Email: <mailto:jtlandis314@gmail.com>
* GitHub mirror: <https://github.com/cran/ggside>

Run `revdepcheck::cloud_details(, "ggside")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ── Error ('test_add_gg.R:51:3'): add_gg errors ─────────────────────────────────
       Error in ``method(update_ggplot, list(class_any, ggplot2::ggplot))`(object = "", plot = <object>, ...)`: Can't add `e2` to a <ggplot> object.
       Backtrace:
            ▆
         1. ├─testthat::expect_error(p + "", "Can't add `\"\"` to a") at test_add_gg.R:51:3
         2. │ └─testthat:::expect_condition_matching_(...)
         3. │   └─testthat:::quasi_capture(...)
         4. │     ├─testthat (local) .capture(...)
         5. │     │ └─base::withCallingHandlers(...)
         6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         7. └─S7:::Ops.S7_object(p, "")
         8.   └─ggplot2 (local) `method(+, list(ggplot2::ggplot, class_any))`(...)
         9.     └─ggplot2:::add_ggplot(e1, e2, e2name)
        10.       ├─ggplot2::ggplot_add(object, p, objectname)
        11.       └─ggplot2:::ggplot_add.default(object, p, objectname)
        12.         └─ggplot2::update_ggplot(object = object, plot = plot, ...)
        13.           ├─S7::S7_dispatch()
        14.           └─ggplot2 (local) `method(update_ggplot, list(class_any, ggplot2::ggplot))`(object = `<chr>`, plot = `<ggplt2::>`, ...)
        15.             └─cli::cli_abort("Can't add {.var {object_name}} to a {.cls ggplot} object.")
        16.               └─rlang::abort(...)
       
       [ FAIL 1 | WARN 0 | SKIP 36 | PASS 115 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# ggtime (1.0.0)

* GitHub: <https://github.com/mitchelloharawild/ggtime>
* Email: <mailto:mail@mitchelloharawild.com>
* GitHub mirror: <https://github.com/cran/ggtime>

Run `revdepcheck::cloud_details(, "ggtime")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > library(ggplot2)
     > 
     > 
     > # Basic time line plot of a random walk (no timezone changes)
     > df_ts <- data.frame(
     +   time = as.POSIXct("2023-03-11", tz = "Australia/Melbourne") + 0:11 * 3600,
     +   value = cumsum(rnorm(12, 2))
     + )
     > ggplot(df_ts, aes(time, value)) +
     +   geom_time_line()
     > 
     > # Random walk with a backward timezone change (DST ends)
     > df_tz_back <- data.frame(
     +   time = as.POSIXct("2023-04-02", tz = "Australia/Melbourne") + 0:11 * 3600,
     +   value = cumsum(rnorm(12, 2))
     + )
     > # Naive/local time (`tz = NA`) shows the DST transition as a dashed jump
     > ggplot(df_tz_back, aes(time, value)) +
     +   geom_time_line() +
     +   scale_x_mixtime(time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA))
     Error in `method(chronon_format_linear, list(mixtime::tu_hour, class_any))`(x = <object>,  : 
       argument "cal" is missing, with no default
     Calls: <Anonymous> ... method(chronon_format_linear, list(mixtime::tu_hour, class_any)) -> paste -> chronon_format_linear -> <Anonymous>
     Execution halted
     ```

## In both

*   checking tests ... ERROR
     ```
     ...
        21.                                     └─self$scale$get_labels(breaks)
        22.                                       └─ggtime (local) get_labels(..., self = self)
        23.                                         └─ggproto_parent(ScaleContinuous, labelled)$get_labels(breaks)
        24.                                           └─ggplot2 (local) get_labels(..., self = self)
        25.                                             └─self$labels(breaks)
        26.                                               └─ggtime (local) labels(...)
        27.                                                 └─ggtime:::time_labels_at(x, chronon = granule)
        28.                                                   ├─base::format(inject(mixtime::mixtime(!!!args)), attr = FALSE)
        29.                                                   └─vecvec (local) `format.vecvec::vecvec`(inject(mixtime::mixtime(!!!args)), attr = FALSE)
        30.                                                     ├─vctrs::vec_c(!!!lapply(x@x, format, ...), .ptype = character())
        31.                                                     │ └─rlang::list2(...) at vctrs/R/c.R:83:3
        32.                                                     └─base::lapply(x@x, format, ...)
        33.                                                       ├─base (local) FUN(X[[i]], ...)
        34.                                                       └─mixtime (local) `format.mixtime::mt_time`(X[[i]], ...)
        35.                                                         └─mixtime:::time_format_impl(x, ..., attr = attr)
        36.                                                           ├─mixtime:::mt_glue_fmt(format, env = env)
        37.                                                           └─mixtime:::time_format_default(x, attr = attr)
        38.                                                             └─mixtime::chronon_format_linear(chronon)
        39.                                                               ├─S7::S7_dispatch()
        40.                                                               └─mixtime (local) `method(chronon_format_linear, list(mixtime::tu_day, class_any))`(...)
       
       [ FAIL 15 | WARN 2 | SKIP 9 | PASS 578 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# GitAI (0.1.3)

* GitHub: <https://github.com/r-world-devs/GitAI>
* Email: <mailto:kamil.wais@gmail.com>
* GitHub mirror: <https://github.com/cran/GitAI>

Run `revdepcheck::cloud_details(, "GitAI")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
        2.   ├─rlang::exec(provider_method, !!!provider_args)
        3.   └─GitAI (local) `<fn>`(model = "gpt-4o-mini", params = NULL, echo = "none")
        4.     └─GitAI:::mock_chat_method(...) at ./setup.R:50:3
        5.       └─ChatMocked$new(provider = provider, echo = echo) at ./setup.R:26:3
        6.         └─ellmer (local) initialize(...)
        7.           └─cli::cli_abort("{.arg model} is required.")
        8.             └─rlang::abort(...)
       ── Error ('test-set_llm.R:100:3'): setting system prompt ───────────────────────
       Error in `initialize(...)`: `model` is required.
       Backtrace:
           ▆
        1. ├─GitAI::set_prompt(set_llm(my_project), system_prompt = "You always return only 'Hi there!'") at test-set_llm.R:100:3
        2. └─GitAI::set_llm(my_project)
        3.   ├─rlang::exec(provider_method, !!!provider_args)
        4.   └─GitAI (local) `<fn>`(model = "gpt-4o-mini", params = NULL, echo = "none")
        5.     └─GitAI:::mock_chat_method(...) at ./setup.R:50:3
        6.       └─ChatMocked$new(provider = provider, echo = echo) at ./setup.R:26:3
        7.         └─ellmer (local) initialize(...)
        8.           └─cli::cli_abort("{.arg model} is required.")
        9.             └─rlang::abort(...)
       
       [ FAIL 4 | WARN 4 | SKIP 6 | PASS 36 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# iAR (1.3.4)

* GitHub: <https://github.com/NA/NA>
* Email: <mailto:felipe.elorrieta@usach.cl>
* GitHub mirror: <https://github.com/cran/iAR>

Run `revdepcheck::cloud_details(, "iAR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘iAR-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: BiAR
     > ### Title: 'BiAR' Class
     > ### Aliases: BiAR
     > 
     > ### ** Examples
     > 
     > times=gentime(n=200, distribution = "expmixture",
     +              lambda1 = 130, lambda2 = 6.5, p1 = 0.15, p2 = 0.85)@times
     Error: Can't find method for `gentime(MISSING)`.
     Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- re-building ‘getting-started.Rmd’ using rmarkdown
     
     Quitting from getting-started.Rmd:53-62 [gentime]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error:
     ! Can't find method for `gentime(MISSING)`.
     ---
     Backtrace:
         ▆
      1. ├─iAR::gentime(n = 100)
      2. │ └─S7::S7_dispatch()
      3. └─S7:::method_lookup_error("gentime", `<list>`)
      4.   └─S7:::stop2(msg, call = NULL, class = "S7_error_method_not_found")
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
     Can't find method for `gentime(MISSING)`.
     --- failed re-building ‘getting-started.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘getting-started.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# imply (0.1.0)

* GitHub: <https://github.com/jonclayden/imply>
* Email: <mailto:code@clayden.org>
* GitHub mirror: <https://github.com/cran/imply>

Run `revdepcheck::cloud_details(, "imply")` for more info

## Newly broken

*   checking whether package ‘imply’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/imply/new/imply.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘imply’ ...
** this is package ‘imply’ version ‘0.1.0’
** package ‘imply’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
...
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o imply.so RcppExports.o apply.o geometry.o narrow.o raster.o reduce.o sparse.o -fopenmp -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/imply/new/imply.Rcheck/00LOCK-imply/00new/imply/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error : Class union has not been registered with S4; please call S4_register(new_union(class_logical, class_integer, class_double, class_complex, class_character, class_raw)).
Error: unable to load R code in package ‘imply’
Execution halted
ERROR: lazy loading failed for package ‘imply’
* removing ‘/tmp/workdir/imply/new/imply.Rcheck/imply’


```
### CRAN

```
* installing *source* package ‘imply’ ...
** this is package ‘imply’ version ‘0.1.0’
** package ‘imply’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
...
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (imply)


```
# joinery (1.0.1)

* GitHub: <https://github.com/edubruell/joinery>
* Email: <mailto:eduard.bruell@zew.de>
* GitHub mirror: <https://github.com/cran/joinery>

Run `revdepcheck::cloud_details(, "joinery")` for more info

## Newly broken

*   checking whether package ‘joinery’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘joinery’
     See ‘/tmp/workdir/joinery/new/joinery.Rcheck/00install.out’ for details.
     ```

# marquee (1.2.1)

* GitHub: <https://github.com/r-lib/marquee>
* Email: <mailto:thomas.pedersen@posit.co>
* GitHub mirror: <https://github.com/cran/marquee>

Run `revdepcheck::cloud_details(, "marquee")` for more info

## Newly broken

*   checking dependencies in R code ... NOTE
     ```
     Namespace in Imports field not imported from: ‘S7’
       All declared Imports should be used.
     ```

# measr (2.0.1)

* GitHub: <https://github.com/r-dcm/measr>
* Email: <mailto:wjakethompson@gmail.com>
* GitHub mirror: <https://github.com/cran/measr>

Run `revdepcheck::cloud_details(, "measr")` for more info

## Newly broken

*   checking for code/documentation mismatches ... WARNING
     ```
     Codoc mismatches from Rd file 'measrdcm.Rd':
     measrdcm
       Code: function(model_spec = dcmstan::dcm_specification(), data =
                      list(), stancode = character(0), method =
                      stanmethod(), algorithm = character(0), backend =
                      stanbackend(), model = list(), respondent_estimates =
                      list(), fit = list(), criteria = list(), reliability =
                      list(), file = character(0), version = list())
       Docs: function(model_spec = NULL, data = list(), stancode =
                      character(0), method = stanmethod(), algorithm =
                      character(0), backend = stanbackend(), model = list(),
                      respondent_estimates = list(), fit = list(), criteria
                      = list(), reliability = list(), file = character(0),
                      version = list())
       Mismatches in argument default values:
         Name: 'model_spec' Code: dcmstan::dcm_specification() Docs: NULL
     ```

# medfit (0.3.2)

* GitHub: <https://github.com/data-wise/medfit>
* Email: <mailto:dtofighi@gmail.com>
* GitHub mirror: <https://github.com/cran/medfit>

Run `revdepcheck::cloud_details(, "medfit")` for more info

## Newly broken

*   checking whether package ‘medfit’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/medfit/new/medfit.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘medfit’ ...
** this is package ‘medfit’ version ‘0.3.2’
** package ‘medfit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error : Class union has not been registered with S4; please call S4_register(new_union(class_integer, class_double, NULL)).
Error: unable to load R code in package ‘medfit’
Execution halted
ERROR: lazy loading failed for package ‘medfit’
* removing ‘/tmp/workdir/medfit/new/medfit.Rcheck/medfit’


```
### CRAN

```
* installing *source* package ‘medfit’ ...
** this is package ‘medfit’ version ‘0.3.2’
** package ‘medfit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (medfit)


```
# mighty.metadata (0.1.0)

* GitHub: <https://github.com/NovoNordisk-OpenSource/mighty.metadata>
* Email: <mailto:oath@novonordisk.com>
* GitHub mirror: <https://github.com/cran/mighty.metadata>

Run `revdepcheck::cloud_details(, "mighty.metadata")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘mighty.metadata-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: columns
     > ### Title: Update columns in your metadata
     > ### Aliases: columns list_columns remove_columns add_column move_column
     > ###   select_column update_column
     > 
     > ### ** Examples
     > 
     > # Load example configuration
     > x <- mighty_domain(
     +   file = system.file("examples", "advs.yml", package = "mighty.metadata")
     + )
     Error in S7::new_object(.parent = S7::S7_object(), context = ctx) : 
       argument "_parent" is missing, with no default
     Calls: mighty_domain ... validate_yaml.character -> use_validator -> validator -> <Anonymous>
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
           ▆
        1. └─mighty.metadata::mighty_study(test_path("test_study")) at test-z_resolve.R:150:3
        2.   └─mighty.metadata:::find_yml(path = path, name = "_mighty", schema = mighty_schema)
        3.     ├─S7schema::validate_yaml(files, schema)
        4.     └─S7schema:::validate_yaml.character(files, schema)
        5.       ├─S7schema:::use_validator(...)
        6.       └─S7schema:::validator(schema = schema)
        7.         └─S7::new_object(.parent = S7::S7_object(), context = ctx)
       ── Error ('test-z_resolve.R:177:3'): resolve_includes() - drops domains if not included (expression) ──
       <getvarError/missingArgError/error/condition>
       Error in `S7::new_object(.parent = S7::S7_object(), context = ctx)`: argument "_parent" is missing, with no default
       Backtrace:
           ▆
        1. └─mighty.metadata::mighty_study(test_path("test_study")) at test-z_resolve.R:177:3
        2.   └─mighty.metadata:::find_yml(path = path, name = "_mighty", schema = mighty_schema)
        3.     ├─S7schema::validate_yaml(files, schema)
        4.     └─S7schema:::validate_yaml.character(files, schema)
        5.       ├─S7schema:::use_validator(...)
        6.       └─S7schema:::validator(schema = schema)
        7.         └─S7::new_object(.parent = S7::S7_object(), context = ctx)
       
       [ FAIL 63 | WARN 0 | SKIP 1 | PASS 40 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
      2.   ├─S7::new_object(...)
      3.   │ └─S7:::collect_dots(...)
      4.   └─S7schema::S7schema(...)
      5.     ├─S7schema::validate_yaml(file, schema)
      6.     └─S7schema:::validate_yaml.character(file, schema)
      7.       ├─S7schema:::use_validator(...)
      8.       └─S7schema:::validator(schema = schema)
      9.         └─S7::new_object(.parent = S7::S7_object(), context = ctx)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'mighty-metadata.Rmd' failed with diagnostics:
     argument "_parent" is missing, with no default
     --- failed re-building ‘mighty-metadata.Rmd’
     
     --- re-building ‘mighty-schema.Rmd’ using rmarkdown
     --- finished re-building ‘mighty-schema.Rmd’
     
     --- re-building ‘study-schema.Rmd’ using rmarkdown
     --- finished re-building ‘study-schema.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘mighty-metadata.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# mixtime (0.3.0)

* GitHub: <https://github.com/mitchelloharawild/mixtime>
* Email: <mailto:mail@mitchelloharawild.com>
* GitHub mirror: <https://github.com/cran/mixtime>

Run `revdepcheck::cloud_details(, "mixtime")` for more info

## Newly broken

*   checking whether package ‘mixtime’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/mixtime/new/mixtime.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘mixtime’ ...
** this is package ‘mixtime’ version ‘0.3.0’
** package ‘mixtime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c cpp11.cpp -o cpp11.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c format.cpp -o format.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c timeastro.cpp -o timeastro.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c timeastro_lunar.cpp -o timeastro_lunar.o
...
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o mixtime.so cpp11.o format.o timeastro.o timeastro_lunar.o timeastro_solar.o timezone-info.o -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/mixtime/new/mixtime.Rcheck/00LOCK-mixtime/00new/mixtime/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error : Package 'vecvec' must export `vecvec` as an S7 class.
Error: unable to load R code in package ‘mixtime’
Execution halted
ERROR: lazy loading failed for package ‘mixtime’
* removing ‘/tmp/workdir/mixtime/new/mixtime.Rcheck/mixtime’


```
### CRAN

```
* installing *source* package ‘mixtime’ ...
** this is package ‘mixtime’ version ‘0.3.0’
** package ‘mixtime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c cpp11.cpp -o cpp11.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c format.cpp -o format.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c timeastro.cpp -o timeastro.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/cpp11/include' -I'/tmp/r-deps/tzdb/include' -I/usr/local/include    -fpic  -g -O2   -c timeastro_lunar.cpp -o timeastro_lunar.o
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (mixtime)


```
# myTAI (2.3.7)

* GitHub: <https://github.com/drostlab/myTAI>
* Email: <mailto:hajk-georg.drost@tuebingen.mpg.de>
* GitHub mirror: <https://github.com/cran/myTAI>

Run `revdepcheck::cloud_details(, "myTAI")` for more info

## Newly broken

*   checking whether package ‘myTAI’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/myTAI/new/myTAI.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘myTAI’ ...
** this is package ‘myTAI’ version ‘2.3.7’
** package ‘myTAI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c null_txis.cpp -o null_txis.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c sc_txi.cpp -o sc_txi.o
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o myTAI.so RcppExports.o null_txis.o sc_txi.o -latomic -L/opt/R/4.6.0/lib/R/lib -lR
...
Warning: namespace ‘myTAI’ is not available and has been replaced
by .GlobalEnv when processing object ‘example_phyex_set_sc’
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggforce', details:
  call: S7::S7_data(x)
  error: `object` must be an <S7_object>, not a S3<ggplot2::mapping/uneval/gg/S7_object>.
Execution halted
ERROR: lazy loading failed for package ‘myTAI’
* removing ‘/tmp/workdir/myTAI/new/myTAI.Rcheck/myTAI’


```
### CRAN

```
* installing *source* package ‘myTAI’ ...
** this is package ‘myTAI’ version ‘2.3.7’
** package ‘myTAI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c null_txis.cpp -o null_txis.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppThread/include' -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c sc_txi.cpp -o sc_txi.o
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o myTAI.so RcppExports.o null_txis.o sc_txi.o -latomic -L/opt/R/4.6.0/lib/R/lib -lR
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (myTAI)


```
# nflplotR (1.7.0)

* GitHub: <https://github.com/nflverse/nflplotR>
* Email: <mailto:mrcaseb@gmail.com>
* GitHub mirror: <https://github.com/cran/nflplotR>

Run `revdepcheck::cloud_details(, "nflplotR")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > 
     > library(nflplotR)
     > library(ggplot2)
     > 
     > team_abbr <- valid_team_names()
     > # remove conference logos from this example
     > team_abbr <- team_abbr[!team_abbr %in% c("AFC", "NFC", "NFL")]
     > 
     > df <- data.frame(
     +   random_value = runif(length(team_abbr), 0, 1),
     +   teams = team_abbr
     + )
     > 
     > # use logos for x-axis
     > # note that the plot is assigned to the object "p"
     > p <- ggplot(df, aes(x = teams, y = random_value)) +
     +   geom_col(aes(color = teams, fill = teams), width = 0.5) +
     +   scale_color_nfl(type = "secondary") +
     +   scale_fill_nfl(alpha = 0.4) +
     +   theme_minimal() +
     +   theme(axis.text.x = element_nfl_logo())
     Error in S7::new_object(S7::S7_object(), alpha = alpha, colour = color %||%  : 
       `_parent` must be an instance of <ggplot2::element_text>, not <S7_object>.
     Calls: theme ... <Anonymous> -> <Anonymous> -> check_parent -> stop2
     Execution halted
     ```

*   checking for code/documentation mismatches ... WARNING
     ```
     ...
         alpha colour hjust vjust color angle size
       Mismatches in argument names:
         Position: 1 Code: ... Docs: alpha
     element_nfl_wordmark
       Code: function(...)
       Docs: function(alpha = 1L, colour = NA_character_, hjust = 0.5, vjust
                      = 0.5, color = NULL, angle = 0, size = grid::unit(0.5,
                      "cm"))
       Argument names in code not in docs:
         ...
       Argument names in docs not in code:
         alpha colour hjust vjust color angle size
       Mismatches in argument names:
         Position: 1 Code: ... Docs: alpha
     element_nfl_headshot
       Code: function(...)
       Docs: function(alpha = 1L, colour = NA_character_, hjust = 0.5, vjust
                      = 0.5, color = NULL, angle = 0, size = grid::unit(0.5,
                      "cm"))
       Argument names in code not in docs:
         ...
       Argument names in docs not in code:
         alpha colour hjust vjust color angle size
       Mismatches in argument names:
         Position: 1 Code: ... Docs: alpha
     ```

# PFIM (8.0)

* Email: <mailto:pfim@inserm.fr>
* GitHub mirror: <https://github.com/cran/PFIM>

Run `revdepcheck::cloud_details(, "PFIM")` for more info

## Newly broken

*   checking whether package ‘PFIM’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/PFIM/new/PFIM.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘PFIM’ ...
** this is package ‘PFIM’ version ‘8.0’
** package ‘PFIM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -DARMA_WARN_LEVEL=1 -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2   -c FedorovWynnAlgorithm.cpp -o FedorovWynnAlgorithm.o
...
* This warning will become an error in a future release.
Warning in new_property(class_list, default = list()) :
  `default` should be a scalar or a quoted call, not a <list>.
* Did you mean `default = quote(list())`?
* This warning will become an error in a future release.
Error : Class union has not been registered with S4; please call S4_register(new_union(NULL, PFIM::Fim)).
Error: unable to load R code in package ‘PFIM’
Execution halted
ERROR: lazy loading failed for package ‘PFIM’
* removing ‘/tmp/workdir/PFIM/new/PFIM.Rcheck/PFIM’


```
### CRAN

```
* installing *source* package ‘PFIM’ ...
** this is package ‘PFIM’ version ‘8.0’
** package ‘PFIM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -DARMA_WARN_LEVEL=1 -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2   -c FedorovWynnAlgorithm.cpp -o FedorovWynnAlgorithm.o
...
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (PFIM)


```
# querychat (0.4.1)

* GitHub: <https://github.com/posit-dev/querychat>
* Email: <mailto:garrick@posit.co>
* GitHub mirror: <https://github.com/cran/querychat>

Run `revdepcheck::cloud_details(, "querychat")` for more info

## Newly broken

*   checking S3 generic/method consistency ... WARNING
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See section ‘Generic functions and methods’ in the ‘Writing R
     Extensions’ manual.
     ```

*   checking for code/documentation mismatches ... WARNING
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking dependencies in R code ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking foreign function calls ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See chapter ‘System and foreign language interfaces’ in the ‘Writing R
     Extensions’ manual.
     ```

*   checking R code for possible problems ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking Rd \usage sections ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     The \usage entries for S3 methods should use the \method markup and not
     their full name.
     See chapter ‘Writing R documentation files’ in the ‘Writing R
     Extensions’ manual.
     ```

# rtemis (1.2.7)

* GitHub: <https://github.com/rtemis-org/rtemis>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis>

Run `revdepcheck::cloud_details(, "rtemis")` for more info

## Newly broken

*   checking whether package ‘rtemis’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/rtemis/new/rtemis.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking tests ... ERROR
     ```
     ...
       
       2026-10-09 13:04:45 [0m[1;38;2;15;106;102m✓[0m Done in 0.14 seconds.[0m [train]
       [ FAIL 1 | WARN 4 | SKIP 4 | PASS 356 ]
       
       ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
       • For local testing only; requires CSV file (3): 'test_ClusterConfig.R:19:3',
         'test_DecomposeConfig.R:19:3', 'test_SuperConfig.R:48:3'
       • empty test (1):
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_Clustering.R:96:3'): cluster_DBSCAN() succeeds ─────────────────
       Error: approx must be a single, finite, nonnegative number.
       Backtrace:
           ▆
        1. └─rtemis::cluster(...) at test_Clustering.R:96:3
        2.   └─rtemis:::cluster_(config = config, x = x, verbosity = verbosity)
        3.     ├─S7::S7_dispatch()
        4.     └─rtemis (local) `method(cluster_, rtemis::DBSCANConfig)`(...)
        5.       └─dbscan::dbscan(...)
        6.         └─dbscan:::.validate_nonnegative_scalar(extra$approx %||% 0, "approx")
       
       [ FAIL 1 | WARN 4 | SKIP 4 | PASS 356 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## Installation

### Devel

```
* installing *source* package ‘rtemis’ ...
** this is package ‘rtemis’ version ‘1.2.7’
** package ‘rtemis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis’
Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.core’
Error in new_class(name = "StratSubConfig", parent = ResamplerConfig,  : 
  <StratSubConfig>@n must narrow <rtemis::ResamplerConfig>@n.
- <rtemis::ResamplerConfig>@n is <integer>.
- <StratSubConfig>@n is <integer> or <NULL>.
Error: unable to load R code in package ‘rtemis’
Execution halted
ERROR: lazy loading failed for package ‘rtemis’
* removing ‘/tmp/workdir/rtemis/new/rtemis.Rcheck/rtemis’


```
### CRAN

```
* installing *source* package ‘rtemis’ ...
** this is package ‘rtemis’ version ‘1.2.7’
** package ‘rtemis’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rtemis)


```
# rtemis.a3 (0.5.3)

* GitHub: <https://github.com/rtemis-org/a3>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis.a3>

Run `revdepcheck::cloud_details(, "rtemis.a3")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘rtemis.a3-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: create_A3
     > ### Title: Create an A3 object from sequence, annotations, and metadata
     > ### Aliases: create_A3
     > 
     > ### ** Examples
     > 
     > # Minimal: sequence only
     > a <- create_A3("MAEPRQEFEVMEDHAGTYGLGDRK")
     Error in new_object(Metadata, uniprot_id = uniprot_id, description = description,  : 
       `_parent` must be an instance of <rtemis.a3::Metadata>, not <S7_class>.
     Calls: create_A3 ... A3 -> A3Metadata -> new_object -> check_parent -> stop2
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
       Error in `new_object(Metadata, uniprot_id = uniprot_id, description = description, reference = reference, organism = organism)`: `_parent` must be an instance of <rtemis.a3::Metadata>, not <S7_class>.
       Backtrace:
           ▆
        1. └─rtemis.a3:::A3from_json(lst) at test_A3.R:674:3
        2.   ├─rtemis.a3:::A3(...)
        3.   └─rtemis.a3:::A3Metadata(...)
        4.     └─S7::new_object(...)
        5.       └─S7:::check_parent(`_parent`, class)
        6.         └─S7:::stop2(msg, call = call)
       ── Error ('test_A3.R:681:3'): read_A3json reports valid A3 version ─────────────
       <error/condition>
       Error in `new_object(Metadata, uniprot_id = uniprot_id, description = description, reference = reference, organism = organism)`: `_parent` must be an instance of <rtemis.a3::Metadata>, not <S7_class>.
       Backtrace:
           ▆
        1. └─rtemis.a3::create_A3(...) at test_A3.R:681:3
        2.   ├─rtemis.a3:::A3(...)
        3.   └─rtemis.a3:::A3Metadata(...)
        4.     └─S7::new_object(...)
        5.       └─S7:::check_parent(`_parent`, class)
        6.         └─S7:::stop2(msg, call = call)
       
       [ FAIL 9 | WARN 0 | SKIP 1 | PASS 57 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking whether package ‘rtemis.a3’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.a3’
     See ‘/tmp/workdir/rtemis.a3/new/rtemis.a3.Rcheck/00install.out’ for details.
     ```

# rtemis.core (0.4.6)

* GitHub: <https://github.com/rtemis-org/rtemis.core>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis.core>

Run `revdepcheck::cloud_details(, "rtemis.core")` for more info

## Newly broken

*   checking whether package ‘rtemis.core’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.core’
     See ‘/tmp/workdir/rtemis.core/new/rtemis.core.Rcheck/00install.out’ for details.
     ```

# rtemis.llm (0.8.7)

* GitHub: <https://github.com/rtemis-org/llm>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis.llm>

Run `revdepcheck::cloud_details(, "rtemis.llm")` for more info

## Newly broken

*   checking whether package ‘rtemis.llm’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.llm’
     See ‘/tmp/workdir/rtemis.llm/new/rtemis.llm.Rcheck/00install.out’ for details.
     ```

# S7schema (0.1.2)

* GitHub: <https://github.com/NovoNordisk-OpenSource/S7schema>
* Email: <mailto:oath@novonordisk.com>
* GitHub mirror: <https://github.com/cran/S7schema>

Run `revdepcheck::cloud_details(, "S7schema")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘S7schema-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: S7schema
     > ### Title: Work with valid configurations
     > ### Aliases: S7schema
     > 
     > ### ** Examples
     > 
     > # Work with yaml configuration file:
     > S7schema(
     +   file = system.file("examples/config.yml", package = "S7schema"),
     +   schema = system.file("examples/schema.json", package = "S7schema")
     + )
     Error in S7::new_object(.parent = S7::S7_object(), context = ctx) : 
       argument "_parent" is missing, with no default
     Calls: S7schema ... validate_yaml.character -> use_validator -> validator -> <Anonymous>
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        6.       └─S7::new_object(.parent = S7::S7_object(), context = ctx)
       ── Failure ('test-z_write.R:52:3'): object created in memory ───────────────────
       Expected `S7schema(...)` not to throw any conditions.
       Actually got a <getvarError> with message:
         argument "_parent" is missing, with no default
       ── Error ('test-z_write.R:58:3'): object created in memory ─────────────────────
       <S7_error_method_not_found/error/condition>
       Error: Can't find method for `write_config(<NULL>)`.
       Backtrace:
            ▆
         1. ├─testthat::expect_error(write_config(x), "`path` must be provided") at test-z_write.R:58:3
         2. │ └─testthat:::expect_condition_matching_(...)
         3. │   └─testthat:::quasi_capture(...)
         4. │     ├─testthat (local) .capture(...)
         5. │     │ └─base::withCallingHandlers(...)
         6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
         7. ├─S7schema::write_config(x)
         8. │ └─S7::S7_dispatch()
         9. └─S7:::method_lookup_error("write_config", `<named list>`)
        10.   └─S7:::stop2(msg, call = NULL, class = "S7_error_method_not_found")
       
       [ FAIL 27 | WARN 0 | SKIP 1 | PASS 61 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Error in `S7::new_object()`:
     ! argument "_parent" is missing, with no default
     ---
     Backtrace:
         ▆
      1. └─my_config_class(file = config_path)
      2.   ├─S7::new_object(...)
      3.   │ └─S7:::collect_dots(...)
      4.   └─S7schema::S7schema(...)
      5.     ├─S7schema::validate_yaml(file, schema)
      6.     └─S7schema:::validate_yaml.character(file, schema)
      7.       ├─S7schema:::use_validator(...)
      8.       └─S7schema:::validator(schema = schema)
      9.         └─S7::new_object(.parent = S7::S7_object(), context = ctx)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'use-in-package.Rmd' failed with diagnostics:
     argument "_parent" is missing, with no default
     --- failed re-building ‘use-in-package.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘S7schema.Rmd’ ‘use-in-package.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# shinychat (0.5.0)

* GitHub: <https://github.com/posit-dev/shinychat>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/shinychat>

Run `revdepcheck::cloud_details(, "shinychat")` for more info

## Newly broken

*   checking for code/documentation mismatches ... WARNING
     ```
     Codoc mismatches from Rd file 'ContentSlashCommand.Rd':
     ContentSlashCommand
       Code: function(..., command = character(0), user_text = "")
       Docs: function(text = stop("Required"), command = character(0),
                      user_text = "")
       Argument names in code not in docs:
         ...
       Argument names in docs not in code:
         text
       Mismatches in argument names:
         Position: 1 Code: ... Docs: text
     ```

# shinyCohortBuilder (1.0.0)

* GitHub: <https://github.com/r-world-devs/shinyCohortBuilder>
* Email: <mailto:krystian8207@gmail.com>
* GitHub mirror: <https://github.com/cran/shinyCohortBuilder>

Run `revdepcheck::cloud_details(, "shinyCohortBuilder")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
     > ### Title: Return GUI layer methods for filter of specified type
     > ### Aliases: gui-filter-layer .gui_filter
     > 
     > ### ** Examples
     > 
     > library(cohortBuilder)
     
     Attaching package: ‘cohortBuilder’
     
     The following objects are masked from ‘package:stats’:
     
         filter, step
     
     > librarian_source <- set_source(as.tblist(librarian))
     > coh <- cohort(
     +   librarian_source,
     +   filter(
     +     "range", id = "copies", name = "Copies", dataset = "books",
     +     variable = "copies", range = c(5, 12)
     +   )
     + ) |> run()
     Error in S7::new_object(S7::S7_object(), type = "range", id = id, name = name,  : 
       `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Calls: run ... constructor -> <Anonymous> -> check_parent -> stop2
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
        10. │       ├─base::do.call(cohortBuilder::filter, .)
        11. │       └─cohortBuilder (local) `<fn>`(...)
        12. │         └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
        13. │           └─S7::new_object(...) at cohortBuilder/R/filter.R:125:5
        14. │             └─S7:::check_parent(`_parent`, class)
        15. │               └─S7:::stop2(msg, call = call)
        16. │                 └─base::stop(...)
        17. └─purrr (local) `<fn>`(`<error>`)
        18.   └─cli::cli_abort(...)
        19.     └─rlang::abort(...)
       ── Error ('test-source_tblist.R:115:3'): .available_filters_choices.tblist returns prepared choices ──
       <error/condition>
       Error in `S7::new_object(S7::S7_object(), type = "range", id = id, name = name, variable = variable, range = range, dataset = dataset, keep_na = keep_na, active = active, description = description, domain = domain, extra = list(...), private = list(input_param = "range"))`: `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
       Backtrace:
           ▆
        1. └─cohortBuilder::filter(...) at test-source_tblist.R:115:3
        2.   └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
        3.     └─S7::new_object(...) at cohortBuilder/R/filter.R:188:5
        4.       └─S7:::check_parent(`_parent`, class)
        5.         └─S7:::stop2(msg, call = call)
       
       [ FAIL 65 | WARN 1 | SKIP 69 | PASS 254 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
       9.       └─cohortBuilder (local) .f(.x[[i]], ...)
      10.         ├─base::do.call(cohortBuilder::filter, .)
      11.         └─cohortBuilder (local) `<fn>`(...)
      12.           └─cohortBuilder (local) constructor(...) at cohortBuilder/R/filter.R:905:3
      13.             └─S7::new_object(...) at cohortBuilder/R/filter.R:188:5
      14.               └─S7:::check_parent(`_parent`, class)
      15.                 └─S7:::stop2(msg, call = call)
      16.                   └─base::stop(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'shinyCohortBuilder.Rmd' failed with diagnostics:
     ℹ In index: 1.
     ℹ With name: iris.Sepal.Length.
     Caused by error in `S7::new_object()` at cohortBuilder/R/filter.R:188:5:
     ! `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     --- failed re-building ‘shinyCohortBuilder.Rmd’
     
     --- re-building ‘updating-source.Rmd’ using rmarkdown
     --- finished re-building ‘updating-source.Rmd’
     
     SUMMARY: processing the following files failed:
       ‘gui-filter-layer.Rmd’ ‘shinyCohortBuilder.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# shinyfilters (0.3.1)

* GitHub: <https://github.com/joshwlivingston/shinyfilters>
* Email: <mailto:joshwlivingston@gmail.com>
* GitHub mirror: <https://github.com/cran/shinyfilters>

Run `revdepcheck::cloud_details(, "shinyfilters")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         3. ├─shiny::shinyApp(...)
         4. │ └─shiny:::uiHttpHandler(ui, uiPattern)
         5. │   └─base::force(ui)
         6. ├─shiny::fluidPage(...)
         7. │ ├─shiny::bootstrapPage(...)
         8. │ │ └─rlang::list2(...)
         9. │ └─htmltools::div(class = "container-fluid", ...)
        10. │   └─rlang::dots_list(...)
        11. ├─shiny::sidebarLayout(...)
        12. ├─shiny::sidebarPanel(...)
        13. │ ├─htmltools::div(...)
        14. │ │ └─rlang::dots_list(...)
        15. │ └─tags$form(class = "well", role = "complementary", ...)
        16. │   └─rlang::dots_list(...)
        17. └─shinyfilters::filterInput(...)
        18.   ├─S7::S7_dispatch()
        19.   └─shinyfilters (local) `method(filterInput, class_list)`(x = `<list>`, ...)
        20.     └─shinyfilters:::s7_check_is_valid_list_dispatch(x, function_name = "filterInput")
        21.       ├─base::stop(...)
        22.       └─base::sprintf(...)
       
       [ FAIL 14 | WARN 51 | SKIP 2 | PASS 306 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Quitting from filter-input-catalog.Rmd:142-150 [ex-radioButtons-list]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `cls@name`:
     ! no applicable method for `@` applied to an object of class "S7_base_class"
     ---
     Backtrace:
         ▆
      1. └─shinyfilters::filterInput(...)
      2.   ├─S7::S7_dispatch()
      3.   └─shinyfilters (local) `method(filterInput, class_list)`(x = `<list>`, ...)
      4.     └─shinyfilters:::s7_check_is_valid_list_dispatch(x, function_name = "filterInput")
      5.       ├─base::stop(...)
      6.       └─base::sprintf(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'filter-input-catalog.Rmd' failed with diagnostics:
     no applicable method for `@` applied to an object of class "S7_base_class"
     --- failed re-building ‘filter-input-catalog.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘filter-input-catalog.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# shinyOAuth (0.6.1)

* GitHub: <https://github.com/lukakoning/shinyOAuth>
* Email: <mailto:koningluka@gmail.com>
* GitHub mirror: <https://github.com/cran/shinyOAuth>

Run `revdepcheck::cloud_details(, "shinyOAuth")` for more info

## Newly broken

*   checking whether package ‘shinyOAuth’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘shinyOAuth’
     See ‘/tmp/workdir/shinyOAuth/new/shinyOAuth.Rcheck/00install.out’ for details.
     ```

# statim (0.1.0)

* GitHub: <https://github.com/s7-stats/statim>
* Email: <mailto:joshua.marie.k@gmail.com>
* GitHub mirror: <https://github.com/cran/statim>

Run `revdepcheck::cloud_details(, "statim")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘statim-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: LINEAR_REG
     > ### Title: Linear regression
     > ### Aliases: LINEAR_REG
     > 
     > ### ** Examples
     > 
     > # via rel()
     > cars |>
     +     define_model(rel(speed, dist)) |>
     +     prepare_model(LINEAR_REG) |>
     +     conclude()
     Error in class_lm_object(terms = fit$terms, fitted = unname(fit$fitted.values),  : 
       unused argument (family = "gaussian")
     Calls: conclude ... inject_and_run -> <Anonymous> -> <Anonymous> -> lm_to_lm_object
     Execution halted
     ```

*   checking tests ... ERROR
     ```
     ...
         7.         └─statim (local) `method(conclude, statim::model_lazy)`(.x = `<sttm::m_>`, ...)
         8.           └─statim:::inject_and_run(...)
         9.             ├─rlang::exec(fn, !!!injected, !!!extra)
        10.             └─statim (local) `<fn>`(.proc = `<named list>`)
        11.               └─statim:::lm_to_lm_object(stats::lm(formula, data = data, ...))
       ── Error ('test-write-models.R:189:5'): write_models() mixed var_id types each result is a cld_exec ──
       Error in `class_lm_object(terms = fit$terms, fitted = unname(fit$fitted.values), residuals = unname(fit$residuals), beta = coef_tbl[, 1], std_beta = coef_tbl[, 2], df_residual = df_res, deviance = rss, dispersion = rss/df_res, family = "gaussian", x_mat = as.numeric(mm), x_assign = attr(mm, "assign"), x_levels = xlev)`: unused argument (family = "gaussian")
       Backtrace:
            ▆
         1. └─statim::conclude(...) at test-write-models.R:189:5
         2.   ├─S7::S7_dispatch()
         3.   └─statim (local) `method(conclude, statim::multi_lazy)`(.x = `<sttm::m_>`, ...)
         4.     └─base::lapply(.x@models, conclude)
         5.       └─statim (local) FUN(X[[i]], ...)
         6.         ├─S7::S7_dispatch()
         7.         └─statim (local) `method(conclude, statim::model_lazy)`(.x = `<sttm::m_>`, ...)
         8.           └─statim:::inject_and_run(...)
         9.             ├─rlang::exec(fn, !!!injected, !!!extra)
        10.             └─statim (local) `<fn>`(.proc = `<named list>`)
        11.               └─statim:::lm_to_lm_object(stats::lm(formula, data = data, ...))
       
       [ FAIL 103 | WARN 0 | SKIP 0 | PASS 1039 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     Error in `class_lm_object()`:
     ! unused argument (family = "gaussian")
     ---
     Backtrace:
         ▆
      1. ├─statim::tidy(...)
      2. │ └─S7::S7_dispatch()
      3. └─statim::conclude(prepare(define_model(mtcars, mpg ~ .), LINEAR_REG))
      4.   ├─S7::S7_dispatch()
      5.   └─statim (local) `method(conclude, statim::model_lazy)`(.x = `<sttm::m_>`, ...)
      6.     └─statim:::inject_and_run(...)
      7.       ├─rlang::exec(fn, !!!injected, !!!extra)
      8.       └─statim (local) `<fn>`(.proc = `<named list>`)
      9.         └─statim:::lm_to_lm_object(stats::lm(formula, data = data, ...))
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'statim.Rmd' failed with diagnostics:
     unused argument (family = "gaussian")
     --- failed re-building ‘statim.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘statim.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

*   checking whether package ‘statim’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Note: possible error in 'class_lm_object(terms = fit$terms, ': unused argument (family = "gaussian") 
     See ‘/tmp/workdir/statim/new/statim.Rcheck/00install.out’ for details.
     Information on the location(s) of code generating the ‘Note’s can be
     obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
     to ‘yes’.
     ```

*   checking R code for possible problems ... NOTE
     ```
     lm_to_lm_object: possible error in class_lm_object(terms = fit$terms,
       fitted = unname(fit$fitted.values), residuals =
       unname(fit$residuals), beta = coef_tbl[, 1], std_beta = coef_tbl[,
       2], df_residual = df_res, deviance = rss, dispersion = rss/df_res,
       family = "gaussian", x_mat = as.numeric(mm), x_assign = attr(mm,
       "assign"), x_levels = xlev): unused argument (family = "gaussian")
     ```

# tidyllm (0.7.0)

* GitHub: <https://github.com/edubruell/tidyllm>
* Email: <mailto:eduard.bruell@zew.de>
* GitHub mirror: <https://github.com/cran/tidyllm>

Run `revdepcheck::cloud_details(, "tidyllm")` for more info

## Newly broken

*   checking S3 generic/method consistency ... WARNING
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See section ‘Generic functions and methods’ in the ‘Writing R
     Extensions’ manual.
     ```

*   checking for code/documentation mismatches ... WARNING
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking dependencies in R code ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking foreign function calls ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See chapter ‘System and foreign language interfaces’ in the ‘Writing R
     Extensions’ manual.
     ```

*   checking R code for possible problems ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     ```

*   checking Rd \usage sections ... NOTE
     ```
     Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     The \usage entries for S3 methods should use the \method markup and not
     their full name.
     See chapter ‘Writing R documentation files’ in the ‘Writing R
     Extensions’ manual.
     ```

# typedjson (0.1.1)

* GitHub: <https://github.com/nbenn/typedjson>
* Email: <mailto:nicolas@cynkra.com>
* GitHub mirror: <https://github.com/cran/typedjson>

Run `revdepcheck::cloud_details(, "typedjson")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Differences:
       `actual`:   "s7/generator" "s7/generator-validator" "s7/generator-constructor"
       `expected`:                                                                   
       
       ── Failure ('test-shapes.R:59:3'): a value survives in every position it can occupy ──
       Expected `failed` to be identical to `character()`.
       Differences:
            actual                                   | expected                
        [1] "s7/generator @ root"                    -                         
        [2] "s7/generator @ object_value"            -                         
        [3] "s7/generator @ array_element"           -                         
        [4] "s7/generator @ two_object_values"       -                         
        [5] "s7/generator @ two_array_elements"      -                         
        [6] "s7/generator @ deep"                    -                         
        [7] "s7/generator @ attribute"               -                         
        [8] "s7/generator-validator @ root"          -                         
        [9] "s7/generator-validator @ object_value"  -                         
       [10] "s7/generator-validator @ array_element" -                         
        ... ...                                        ...      and 11 more ...
       
       
       [ FAIL 29 | WARN 0 | SKIP 0 | PASS 1754 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# vecvec (1.3.0)

* GitHub: <https://github.com/mitchelloharawild/vecvec>
* Email: <mailto:mail@mitchelloharawild.com>
* GitHub mirror: <https://github.com/cran/vecvec>

Run `revdepcheck::cloud_details(, "vecvec")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       Error in `get(as.character(FUN), mode = "function", envir = envir)`: object '.f' of mode 'function' was not found
       Backtrace:
            ▆
         1. ├─testthat::expect_all_true(is.na(vec)) at test-vctrs.R:22:3
         2. │ └─testthat::quasi_label(enquo(object))
         3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
         4. ├─base::is.na(vec)
         5. └─vecvec (local) `is.na.vecvec::vecvec`(vec)
         6.   ├─vecvec::unvecvec(vecvec_apply(x, is.na), ptype = logical())
         7.   │ └─vecvec::is_vecvec(x)
         8.   │   └─S7::S7_inherits(x, class_vecvec)
         9.   │     └─S7:::class_inherits(x, class)
        10.   └─vecvec::vecvec_apply(x, is.na)
        11.     ├─vecvec:::vecvec_flatten_adj(lapply(x@x, .f, ...))
        12.     │ └─vecvec::is_vecvec(x)
        13.     │   └─S7::S7_inherits(x, class_vecvec)
        14.     │     └─S7:::class_inherits(x, class)
        15.     └─base::lapply(x@x, .f, ...)
        16.       └─base::match.fun(FUN)
        17.         └─base::get(as.character(FUN), mode = "function", envir = envir)
       
       [ FAIL 12 | WARN 0 | SKIP 8 | PASS 495 ]
       Error:
       ! Test failures.
       Execution halted
     ```

