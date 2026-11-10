# ale (0.5.3)

* GitHub: <https://github.com/tripartio/ale>
* Email: <mailto:Chitu.Okoli@skema.edu>
* GitHub mirror: <https://github.com/cran/ale>

Run `revdepcheck::revdep_details(, "ale")` for more info

## Newly broken

*   checking whether package ‘ale’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ale’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ale/new/ale.Rcheck/00install.out’ for details.
     ```

## In both

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       +   6  19.84566 1.197411e-14     1    -1 -Inf  Inf 1.197373e-14 1.197457e-14     NA
       and 53046 more ...
       * Run `testthat::snapshot_accept("ALE-categorical", "testthat")` to accept the change.
       * Run `testthat::snapshot_review("ALE-categorical", "testthat")` to review the change.
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 30 | WARN 663 | SKIP 0 | PASS 92 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# anansi (1.2.0)

* GitHub: <https://github.com/thomazbastiaanssen/anansi>
* Email: <mailto:thomazbastiaanssen@gmail.com>

Run `revdepcheck::revdep_details(, "anansi")` for more info

## Newly broken

*   checking whether package ‘anansi’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/anansi/new/anansi.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking running R code from vignettes ...
     ```
       ‘adjacency_matrices.Rmd’ using ‘UTF-8’... failed
       ‘anansi.Rmd’ using ‘UTF-8’... OK
       ‘differential_associations.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘adjacency_matrices.Rmd’
       ...
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial Narrow' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial Narrow' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Arial Narrow' not found in PostScript font database
     
       When sourcing ‘adjacency_matrices.R’:
     Error: invalid font type
     Execution halted
     ```

## Installation

### Devel

```
* installing *source* package ‘anansi’ ...
** this is package ‘anansi’ version ‘1.2.0’
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: .onLoad failed in loadNamespace() for 'ggforce', details:
  call: S7::S7_data(x)
  error: `object` must be an <S7_object>, not a S3<ggplot2::mapping/uneval/gg/S7_object>.
Execution halted
ERROR: lazy loading failed for package ‘anansi’
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/anansi/new/anansi.Rcheck/anansi’


```
### CRAN

```
* installing *source* package ‘anansi’ ...
** this is package ‘anansi’ version ‘1.2.0’
** using staged installation
** R
** data
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
* DONE (anansi)


```
# apa7 (0.1.3)

* GitHub: <https://github.com/wjschne/apa7>
* Email: <mailto:w.joel.schneider@gmail.com>
* GitHub mirror: <https://github.com/cran/apa7>

Run `revdepcheck::revdep_details(, "apa7")` for more info

## Newly broken

*   checking whether package ‘apa7’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘apa7’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/apa7/new/apa7.Rcheck/00install.out’ for details.
     ```

## In both

*   checking running R code from vignettes ...
     ```
       ‘apa7.qmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘apa7.qmd’
       ...
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Roboto Condensed' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Roboto Condensed' not found in PostScript font database
     Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
       font family 'Roboto Condensed' not found in PostScript font database
     
       When sourcing ‘apa7.R’:
     Error: invalid font type
     Execution halted
     ```

# bidsr (0.1.1)

* GitHub: <https://github.com/dipterix/bidsr>
* Email: <mailto:dipterix.wang@gmail.com>
* GitHub mirror: <https://github.com/cran/bidsr>

Run `revdepcheck::revdep_details(, "bidsr")` for more info

## Newly broken

*   checking whether package ‘bidsr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/bidsr/new/bidsr.Rcheck/00install.out’ for details.
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/bidsr/new/bidsr.Rcheck/bidsr’


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

Run `revdepcheck::revdep_details(, "btw")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         2.   \-btw:::map(...)
         3.     \-base::lapply(.x, .f, ...)
         4.       \-btw (local) FUN(X[[i]], ...)
         5.         +-base::paste(map_chr(children, as.character), collapse = "\n")
         6.         \-btw:::map_chr(children, as.character)
         7.           \-btw:::.rlang_purrr_map_mold(.x, .f, character(1), ...)
         8.             \-rlang::as_function(.f, env = global_env())
         9.               \-rlang:::abort_coercion(x, "a function", arg = arg, call = call)
        10.                 \-rlang::abort(msg, call = call)
       
       [ FAIL 16 | WARN 0 | SKIP 68 | PASS 2373 ]
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

Run `revdepcheck::revdep_details(, "caugi")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

Run `revdepcheck::revdep_details(, "cohortBuilder")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

*   checking running R code from vignettes ...
     ```
     ...
     Execution halted
     when running code in ‘managing-cohort.Rmd’
       ...
     
     
     > librarian_source <- set_source(as.tblist(librarian))
     
     > librarian_cohort <- cohort(librarian_source, step(filter("discrete", 
     +     id = "author", dataset = "books", variable = "author", value = "Dan Brow ..." ... [TRUNCATED] 
     
       When sourcing ‘managing-cohort.R’:
     Error: `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Execution halted
     when running code in ‘source-intelligence.Rmd’
       ...
     > labelled_source <- autofilter(set_source(tblist(iris = iris), 
     +     description = list(iris = list(Species = describe("the species of iris", 
     +     .... [TRUNCATED] 
     
       When sourcing ‘source-intelligence.R’:
     Error: ℹ In index: 1.
     ℹ With name: iris.Sepal.Length.
     Caused by error in `S7::new_object()` at cohortBuilder/R/filter.R:188:5:
     ! `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Execution halted
     ```

# dcmstan (0.1.0)

* GitHub: <https://github.com/r-dcm/dcmstan>
* Email: <mailto:wjakethompson@gmail.com>
* GitHub mirror: <https://github.com/cran/dcmstan>

Run `revdepcheck::revdep_details(, "dcmstan")` for more info

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

*   checking running R code from vignettes ...
     ```
       ‘dcmstan.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘dcmstan.Rmd’
       ...
     10 8c                 0                      0               1
     # ℹ 17 more rows
     # ℹ 1 more variable: multiplicative_comparison <dbl>
     
     > spec <- dcm_specify(qmatrix = dtmr_qmatrix, identifier = "item", 
     +     measurement_model = dina(), structural_model = bayesnet())
     
       When sourcing ‘dcmstan.R’:
     Error: @model is read-only
     Execution halted
     ```

## In both

*   checking tests ...
     ```
       Running ‘spelling.R’
       Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
       
       [ FAIL 64 | WARN 0 | SKIP 0 | PASS 88 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# deltapif (0.4.5)

* Email: <mailto:rzepeda17@gmail.com>
* GitHub mirror: <https://github.com/cran/deltapif>

Run `revdepcheck::revdep_details(, "deltapif")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘Examples.Rmd’
       ...
     10 Physical inactivity 0.3293037 0.092961816  62.8     68.6  56.6  73.2  61.3
     11            Diabetes 0.4317824 0.075776055  28.6     41.0  44.1  37.2  25.4
     12       Air pollution 0.0861777 0.009362766  22.8     44.4  55.2  41.3  17.2
     
     > paf_hispanic <- paf(p = 0.069, beta = 0.463734, var_beta = 0.0273858, 
     +     var_p = 0, rr_link = exp, label = "Hispanic")
     
       When sourcing ‘Examples.R’:
     Error: `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
     Execution halted
     when running code in ‘Introduction.Rmd’
       ...
     
     > library(deltapif)
     
     > library(deltapif)
     
     > paf(p = 0.085, beta = log(1.59), quiet = TRUE)
     
       When sourcing ‘Introduction.R’:
     Error: `_parent` must be an instance of <deltapif::pif_class>, not <S7_object>.
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

Run `revdepcheck::revdep_details(, "ellmer")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
        32.                                       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        33.                                       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
        34.                                       │ └─base::force(expr)
        35.                                       └─rlang::abort(...)
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 8 | WARN 0 | SKIP 86 | PASS 1668 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘prompt-design.Rmd’
       ...
     > chat <- chat_anthropic()
     Using model = "claude-sonnet-5".
     
     > chat$chat(question)
     
       When sourcing ‘prompt-design.R’:
     Error: HTTP 400 Bad Request.
     ℹ Your credit balance is too low to access the Anthropic API. Please go to
       Plans & Billing to upgrade or purchase credits. [invalid_request_error]
     Execution halted
     when running code in ‘structured-data.Rmd’
       ...
     > chat <- chat_anthropic("Extract all characteristics of supplied character")
     Using model = "claude-sonnet-5".
     
     > chat$chat_structured(text, type = type_characteristics)
     
       When sourcing ‘structured-data.R’:
     Error: HTTP 400 Bad Request.
     ℹ Your credit balance is too low to access the Anthropic API. Please go to
       Plans & Billing to upgrade or purchase credits. [invalid_request_error]
     Execution halted
     ```

*   checking whether package ‘ellmer’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘ellmer’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ellmer/new/ellmer.Rcheck/00install.out’ for details.
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

## Newly fixed

*   R CMD check timed out


# filtro (0.2.0)

* GitHub: <https://github.com/tidymodels/filtro>
* Email: <mailto:franceslinyc@gmail.com>
* GitHub mirror: <https://github.com/cran/filtro>

Run `revdepcheck::revdep_details(, "filtro")` for more info

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

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘filtro.qmd’
       ...
     > ames <- modeldata::ames
     
     > ames <- dplyr::mutate(ames, Sale_Price = log10(Sale_Price))
     
     > ames_aov_pval_res <- fit(score_aov_pval, Sale_Price ~ 
     +     ., data = ames)
     
       When sourcing ‘filtro.R’:
     Error: could not find function "fit"
     Execution halted
     when running code in ‘forestimp.qmd’
       ...
     > cells_subset <- dplyr::slice(modeldata::cells, 1:50)
     
     > cells_subset$case <- NULL
     
     > cells_imp_rf_res <- fit(score_imp_rf, class ~ ., data = cells_subset, 
     +     seed = 42)
     
       When sourcing ‘forestimp.R’:
     Error: could not find function "fit"
     Execution halted
     ```

# fr (0.5.2)

* GitHub: <https://github.com/cole-brokamp/fr>
* Email: <mailto:cole@colebrokamp.com>
* GitHub mirror: <https://github.com/cran/fr>

Run `revdepcheck::revdep_details(, "fr")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
        1. ├─testthat::expect_s3_class(...) at test-read_fr_tdr.R:14:3
        2. │ └─testthat::quasi_label(enquo(object))
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─fr::read_fr_tdr("https://raw.githubusercontent.com/cole-brokamp/fr/main/inst/hamilton_poverty_2020/tabular-data-resource.yaml")
        5.   └─fr:::fr_tdr(...)
        6.     └─S7::new_object(...)
        7.       └─S7:::validate_from(...)
        8.         └─S7:::stop2(msg, call = call, class = "S7_error_validation_failed")
       
       [ FAIL 14 | WARN 0 | SKIP 0 | PASS 22 ]
       Deleting unused snapshots: 'write_fr_tdr/my_mtcars.csv' and
       'write_fr_tdr/tabular-data-resource.yaml'
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘creating_a_tabular-data-resource.Rmd’
       ...
     3 A03   2013-08-15    15.6 best        19 TRUE 
     
     > d_tdr <- as_fr_tdr(d, name = "types_example", version = "0.1.0", 
     +     title = "Example Data with Types", homepage = "https://geomarker.io", 
     +     .... [TRUNCATED] 
     
       When sourcing ‘creating_a_tabular-data-resource.R’:
     Error: <fr_tdr> object is invalid:
     - All columns and row names must have the same length
     Execution halted
     when running code in ‘read_fr_tdr.Rmd’
       ...
     /Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/fr/new/fr.Rcheck/fr/hamilton_poverty_2020
     ├── hamilton_poverty_2020.csv
     └── tabular-data-resource.yaml
     
     > d_fr <- read_fr_tdr(fs::path_package("fr", "hamilton_poverty_2020"))
     
       When sourcing ‘read_fr_tdr.R’:
     Error: <fr_tdr> object is invalid:
     - All columns and row names must have the same length
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

Run `revdepcheck::revdep_details(, "GGally")` for more info

## Newly broken

*   checking whether package ‘GGally’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘GGally’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/GGally/new/GGally.Rcheck/00install.out’ for details.
     ```

## In both

*   checking dependencies in R code ... NOTE
     ```
     The operation couldn’t be completed. Unable to locate a Java Runtime.
     Please visit http://www.java.com for information on installing Java.
     ```

# ggarrow (0.2.0)

* GitHub: <https://github.com/teunbrand/ggarrow>
* Email: <mailto:tahvdbrand@gmail.com>
* GitHub mirror: <https://github.com/cran/ggarrow>

Run `revdepcheck::revdep_details(, "ggarrow")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       <getvarError/missingArgError/error/condition>
       Error in `S7::new_object(.parent = parent, linewidth_head = linewidth_head, linewidth_fins = linewidth_fins, stroke_colour = stroke_colour, stroke_width = stroke_width, arrow_head = arrow_head, arrow_fins = arrow_fins, arrow_mid = arrow_mid, length = length, length_head = length_head, length_mid = length_mid, length_fins = length_fins, resect = resect, resect_head = resect_head, resect_fins = resect_fins, justify = justify, force_arrow = force_arrow, mid_place = mid_place, linemitre = linemitre, distort = distort)`: argument "_parent" is missing, with no default
       Backtrace:
           ▆
        1. ├─ggplot2::theme(...) at test-theme_elements.R:2:3
        2. │ └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
        3. │   └─base::mget(args, envir = env)
        4. └─ggarrow::element_arrow(linewidth_head = 3, linewidth_fins = 0)
        5.   └─S7::new_object(...)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 79 ]
       Deleting unused snapshots: 'theme_elements/theme-lines-as-arrows.svg'
       Error:
       ! Test failures.
       Execution halted
     ```

# ggdiagram (0.2.0)

* GitHub: <https://github.com/wjschne/ggdiagram>
* Email: <mailto:w.joel.schneider@gmail.com>
* GitHub mirror: <https://github.com/cran/ggdiagram>

Run `revdepcheck::revdep_details(, "ggdiagram")` for more info

## Newly broken

*   checking whether package ‘ggdiagram’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggdiagram/new/ggdiagram.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking tests ...
     ```
       Running ‘spelling.R’
       Comparing ‘spelling.Rout’ to ‘spelling.Rout.save’ ... OK
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         7. │       ├─purrr:::call_with_cleanup(...)
         8. │       └─ggdiagram (local) .f(...)
         9. │         └─pdftools::pdf_pagesize(f_pdf)
        10. │           ├─pdftools:::poppler_pdf_pagesize(loadfile(pdf), opw, upw)
        11. │           └─pdftools:::loadfile(pdf)
        12. │             └─base::normalizePath(pdf, mustWork = TRUE)
        13. └─base::.handleSimpleError(...)
        14.   └─purrr (local) h(simpleError(msg, call))
        15.     └─cli::cli_abort(...)
        16.       └─rlang::abort(...)
       
       [ FAIL 1 | WARN 1 | SKIP 0 | PASS 2089 ]
       Error:
       ! Test failures.
       Execution halted
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggdiagram/new/ggdiagram.Rcheck/ggdiagram’


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

Run `revdepcheck::revdep_details(, "gglogger")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

Run `revdepcheck::revdep_details(, "ggpath")` for more info

## Newly broken

*   checking whether package ‘ggpath’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggpath/new/ggpath.Rcheck/00install.out’ for details.
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggpath/new/ggpath.Rcheck/ggpath’


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
# ggplotplus (0.5.7)

* GitHub: <https://github.com/MAISRC/ggplotplus>
* Email: <mailto:bajcz003@umn.edu>
* GitHub mirror: <https://github.com/cran/ggplotplus>

Run `revdepcheck::revdep_details(, "ggplotplus")` for more info

## Newly broken

*   checking whether package ‘ggplotplus’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggplotplus/new/ggplotplus.Rcheck/00install.out’ for details.
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/ggplotplus/new/ggplotplus.Rcheck/ggplotplus’


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
# ggtime (1.0.0)

* GitHub: <https://github.com/mitchelloharawild/ggtime>
* Email: <mailto:mail@mitchelloharawild.com>
* GitHub mirror: <https://github.com/cran/ggtime>

Run `revdepcheck::revdep_details(, "ggtime")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
       
       [ FAIL 14 | WARN 2 | SKIP 0 | PASS 589 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# iAR (1.3.4)

* GitHub: <https://github.com/NA/NA>
* Email: <mailto:felipe.elorrieta@usach.cl>
* GitHub mirror: <https://github.com/cran/iAR>

Run `revdepcheck::revdep_details(, "iAR")` for more info

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

*   checking running R code from vignettes ...
     ```
       ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘getting-started.Rmd’
       ...
     
     > library(iAR)
     
     > set.seed(2847)
     
     > times <- gentime(n = 100)@times
     
       When sourcing ‘getting-started.R’:
     Error: Can't find method for `gentime(MISSING)`.
     Execution halted
     ```

# imply (0.1.0)

* GitHub: <https://github.com/jonclayden/imply>
* Email: <mailto:code@clayden.org>
* GitHub mirror: <https://github.com/cran/imply>

Run `revdepcheck::revdep_details(, "imply")` for more info

## Newly broken

*   checking whether package ‘imply’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/imply/new/imply.Rcheck/00install.out’ for details.
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
clang++ -arch arm64 -std=gnu++20 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.6/Resources/lib -L/opt/R/arm64/lib -o imply.so RcppExports.o apply.o geometry.o narrow.o raster.o reduce.o sparse.o -F/Library/Frameworks/R.framework/Versions/4.6 -framework R
installing to /Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/imply/new/imply.Rcheck/00LOCK-imply/00new/imply/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error : Class union has not been registered with S4; please call S4_register(new_union(class_logical, class_integer, class_double, class_complex, class_character, class_raw)).
Error: unable to load R code in package ‘imply’
Execution halted
ERROR: lazy loading failed for package ‘imply’
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/imply/new/imply.Rcheck/imply’


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

Run `revdepcheck::revdep_details(, "joinery")` for more info

## Newly broken

*   checking whether package ‘joinery’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘joinery’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/joinery/new/joinery.Rcheck/00install.out’ for details.
     ```

# marquee (1.2.1)

* GitHub: <https://github.com/r-lib/marquee>
* Email: <mailto:thomas.pedersen@posit.co>
* GitHub mirror: <https://github.com/cran/marquee>

Run `revdepcheck::revdep_details(, "marquee")` for more info

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

Run `revdepcheck::revdep_details(, "measr")` for more info

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

## In both

*   R CMD check timed out


# medfit (0.3.2)

* GitHub: <https://github.com/data-wise/medfit>
* Email: <mailto:dtofighi@gmail.com>
* GitHub mirror: <https://github.com/cran/medfit>

Run `revdepcheck::revdep_details(, "medfit")` for more info

## Newly broken

*   checking whether package ‘medfit’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/medfit/new/medfit.Rcheck/00install.out’ for details.
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/medfit/new/medfit.Rcheck/medfit’


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

Run `revdepcheck::revdep_details(, "mighty.metadata")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
        4.     └─S7schema:::validate_yaml.character(files, schema)
        5.       ├─S7schema:::use_validator(...)
        6.       └─S7schema:::validator(schema = schema)
        7.         └─S7::new_object(.parent = S7::S7_object(), context = ctx)
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 64 | WARN 0 | SKIP 0 | PASS 40 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
       ‘adam-schema.Rmd’ using ‘UTF-8’... OK
       ‘mighty-metadata.Rmd’ using ‘UTF-8’... failed
       ‘mighty-schema.Rmd’ using ‘UTF-8’... OK
       ‘study-schema.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘mighty-metadata.Rmd’
       ...
     
     > library(mighty.metadata)
     
     > path <- system.file("examples", "advs.yml", package = "mighty.metadata")
     
     > advs <- mighty_domain(path)
     
       When sourcing ‘mighty-metadata.R’:
     Error: argument "_parent" is missing, with no default
     Execution halted
     ```

# mixtime (0.3.0)

* GitHub: <https://github.com/mitchelloharawild/mixtime>
* Email: <mailto:mail@mitchelloharawild.com>
* GitHub mirror: <https://github.com/cran/mixtime>

Run `revdepcheck::revdep_details(, "mixtime")` for more info

## Newly broken

*   checking whether package ‘mixtime’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/mixtime/new/mixtime.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘mixtime’ ...
** this is package ‘mixtime’ version ‘0.3.0’
** package ‘mixtime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using SDK: ‘MacOSX27.0.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cpp11.cpp -o cpp11.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c format.cpp -o format.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c timeastro.cpp -o timeastro.o
...
clang++ -arch arm64 -std=gnu++20 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/Library/Frameworks/R.framework/Versions/4.6/Resources/lib -L/opt/R/arm64/lib -o mixtime.so cpp11.o format.o timeastro.o timeastro_lunar.o timeastro_solar.o timezone-info.o -F/Library/Frameworks/R.framework/Versions/4.6 -framework R
installing to /Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/mixtime/new/mixtime.Rcheck/00LOCK-mixtime/00new/mixtime/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error : Package 'vecvec' must export `vecvec` as an S7 class.
Error: unable to load R code in package ‘mixtime’
Execution halted
ERROR: lazy loading failed for package ‘mixtime’
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/mixtime/new/mixtime.Rcheck/mixtime’


```
### CRAN

```
* installing *source* package ‘mixtime’ ...
** this is package ‘mixtime’ version ‘0.3.0’
** package ‘mixtime’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using SDK: ‘MacOSX27.0.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c cpp11.cpp -o cpp11.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c format.cpp -o format.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/cpp11/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/mixtime/tzdb/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c timeastro.cpp -o timeastro.o
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

Run `revdepcheck::revdep_details(, "myTAI")` for more info

## Newly broken

*   checking whether package ‘myTAI’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/myTAI/new/myTAI.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘myTAI’ ...
** this is package ‘myTAI’ version ‘2.3.7’
** package ‘myTAI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using SDK: ‘MacOSX27.0.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c null_txis.cpp -o null_txis.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c sc_txi.cpp -o sc_txi.o
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/myTAI/new/myTAI.Rcheck/myTAI’


```
### CRAN

```
* installing *source* package ‘myTAI’ ...
** this is package ‘myTAI’ version ‘2.3.7’
** package ‘myTAI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using SDK: ‘MacOSX27.0.sdk’
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c null_txis.cpp -o null_txis.o
clang++ -arch arm64 -std=gnu++20 -I"/Library/Frameworks/R.framework/Versions/4.6/Resources/include" -DNDEBUG  -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppArmadillo/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/RcppThread/include' -I'/Users/tomasz/github/RConsortium/S7/revdep/library.noindex/myTAI/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2   -c sc_txi.cpp -o sc_txi.o
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

Run `revdepcheck::revdep_details(, "nflplotR")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         3. │   └─base::mget(args, envir = env)
         4. └─nflplotR::element_nfl_logo()
         5.   ├─S7::new_object((S7::as_class(ggpath::element_path))(...))
         6.   │ └─S7:::check_parent(`_parent`, class)
         7.   │   └─S7:::class_inherits(parent, parent_class)
         8.   └─(S7::as_class(ggpath::element_path))(...)
         9.     └─S7::new_object(...)
        10.       └─S7:::check_parent(`_parent`, class)
        11.         └─S7:::stop2(msg, call = call)
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 20 ]
       Deleting unused snapshots: 'theme-elements/p1.svg' and 'theme-elements/p2.svg'
       Error:
       ! Test failures.
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

# parsermd (0.2.0)

* GitHub: <https://github.com/rundel/parsermd>
* Email: <mailto:rundel@gmail.com>
* GitHub mirror: <https://github.com/cran/parsermd>

Run `revdepcheck::revdep_details(, "parsermd")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       - "Caused by error:"
       + "Caused by error in `<rmd_chunk>@engine`:"
         "! <rmd_chunk>@engine must be <character>, not <double>"
       
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 14 | SKIP 3 | PASS 7262 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking whether package ‘parsermd’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1/__fwd/string.h:45:41: warning: 'char_traits<unsigned char>' is deprecated: char_traits<T> for T not equal to char, wchar_t, char8_t, char16_t or char32_t is non-standard and is provided for a temporary period. It will be removed in a future release, so please migrate off of it. [-Wdeprecated-declarations]
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/parsermd/new/parsermd.Rcheck/00install.out’ for details.
     ```

# PFIM (8.0)

* Email: <mailto:pfim@inserm.fr>
* GitHub mirror: <https://github.com/cran/PFIM>

Run `revdepcheck::revdep_details(, "PFIM")` for more info

## Newly broken

*   checking whether package ‘PFIM’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/PFIM/new/PFIM.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       • src/init.c absent (installed tarball) (1): 'test-cpp-kernels.R:27:3'
       • tests_PFIM/resultats_de_references not found (1):
         'test-eval-opt-references.R:45:3'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-example-pk-mm.R:130:3'): Model PK 1cpt : MichaelisMenten1BolusSingleDose_VmKm ──
       Expected `detPopulationFim` to equal `valueDetPopulationFim`.
       Differences:
       actual != expected but don't know how to show the difference
       
       
       [ FAIL 1 | WARN 0 | SKIP 7 | PASS 1216 ]
       Error:
       ! Test failures.
       Execution halted
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
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++17
using SDK: ‘MacOSX27.0.sdk’
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/PFIM/new/PFIM.Rcheck/PFIM’


```
### CRAN

```
* installing *source* package ‘PFIM’ ...
** this is package ‘PFIM’ version ‘8.0’
** package ‘PFIM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++17
using SDK: ‘MacOSX27.0.sdk’
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
# plyxp (1.6.1)

* GitHub: <https://github.com/jtlandis/plyxp>
* Email: <mailto:jtlandis314@gmail.com>

Run `revdepcheck::revdep_details(, "plyxp")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     ...
       1. ├─dplyr::mutate(...)
       2. ├─plyxp:::mutate.PlySummarizedExperiment(...)
       3. │ └─plyxp::plyxp(.data, mutate_se_impl, ...)
       4. │   ├─rlang::try_fetch(...)
       5. │   │ ├─base::tryCatch(...)
       6. │   │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       7. │   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       8. │   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
       9. │   │ └─base::withCallingHandlers(...)
      10. │   └─plyxp (local) .f(se(.data), ...)
      11. │     └─mask$results()
      12. │       └─self$apply(function(m) m$results(), .on_masks = .from_masks)
      13. │         └─base::lapply(private$.masks[.on_masks], .f, ...)
      14. │           └─plyxp (local) FUN(X[[i]], ...)
      15. │             └─m$results()
      16. │               └─base::lapply(added, self$unchop)
      17. │                 └─plyxp (local) FUN(X[[i]], ...)
      18. │                   └─plyxp::list_unchop(lapply(data, as.vector), indices = private$.indices)
      19. │                     ├─S7::S7_dispatch()
      20. │                     └─plyxp (local) `method(list_unchop, list(class_list, class_any))`(...)
      21. │                       └─vctrs::list_unchop(x, indices = indices, ptype = ptype)
      22. └─rlang (local) `<fn>`(`<evalErrr>`) at vctrs/R/list-unchop.R:85:3
      23.   └─handlers[[2L]](cnd)
      24.     └─rlang::abort(...)
     Execution halted
     ```

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         2.   └─self$apply(function(m) m$results(), .on_masks = .from_masks)
         3.     └─base::lapply(private$.masks[.on_masks], .f, ...)
         4.       └─plyxp (local) FUN(X[[i]], ...)
         5.         └─m$results()
         6.           └─base::lapply(added, self$unchop)
         7.             └─plyxp (local) FUN(X[[i]], ...)
         8.               └─plyxp::list_unchop(lapply(data, as.vector), indices = private$.indices)
         9.                 ├─S7::S7_dispatch()
        10.                 └─plyxp (local) `method(list_unchop, list(class_list, class_any))`(...)
        11.                   └─vctrs::list_unchop(x, indices = indices, ptype = ptype)
       
       [ FAIL 18 | WARN 0 | SKIP 0 | PASS 151 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking running R code from vignettes ...
     ```
       ‘plyxp.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘plyxp.Rmd’
       ...
     > xp <- new_plyxp(airway)
     
     > mutate(xp, log_counts = log1p(counts), cols(treated = dex == 
     +     "trt"), rows(new_id = paste0("gene-", gene_name)))
     
       When sourcing ‘plyxp.R’:
     Error: 
     Caused by error in `method(list_unchop, list(class_list, class_any))`:
     ! argument "ptype" is missing, with no default
     Execution halted
     ```

# querychat (0.4.1)

* GitHub: <https://github.com/posit-dev/querychat>
* Email: <mailto:garrick@posit.co>
* GitHub mirror: <https://github.com/cran/querychat>

Run `revdepcheck::revdep_details(, "querychat")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       -   ! Can't construct an object from abstract class <HandoffGalleryItem>
       +   ! Can't construct an object from abstract class <HandoffGalleryItem>.
       * Run `testthat::snapshot_accept("handoff_types", "testthat")` to accept the change.
       * Run `testthat::snapshot_review("handoff_types", "testthat")` to review the change.
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 21 | SKIP 1 | PASS 2193 ]
       Error:
       ! Test failures.
       Execution halted
     ```

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

## In both

*   R CMD check timed out


# quickr (0.3.0)

* GitHub: <https://github.com/t-kalinowski/quickr>
* Email: <mailto:tomasz@posit.co>
* GitHub mirror: <https://github.com/cran/quickr>

Run `revdepcheck::revdep_details(, "quickr")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         5.     └─quickr:::new_fortran_subroutine(name, e)
         6.       ├─quickr:::scope_set(scope, "return_names", unique(unname(closure_return_var_names(closure))))
         7.       │ └─base::assign(name, value, envir = st)
         8.       ├─base::unique(unname(closure_return_var_names(closure)))
         9.       ├─base::unname(closure_return_var_names(closure))
        10.       └─quickr:::closure_return_var_names(closure)
        11.         └─quickr:::map_chr(args, as.character)
        12.           └─base::vapply(X = .x, FUN = .f, FUN.VALUE = "", ...)
        13.             └─base::match.fun(FUN)
        14.               └─base::get(as.character(FUN), mode = "function", envir = envir)
       
       [ FAIL 26 | WARN 0 | SKIP 0 | PASS 1650 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# RMediation (1.6.1)

* GitHub: <https://github.com/data-wise/rmediation>
* Email: <mailto:dtofighi@gmail.com>
* GitHub mirror: <https://github.com/cran/RMediation>

Run `revdepcheck::revdep_details(, "RMediation")` for more info

## Newly broken

*   checking running R code from vignettes ...
     ```
       ‘getting-started.Rmd’ using ‘UTF-8’... OK
       ‘methods-comparison.Rmd’ using ‘UTF-8’... OK
       ‘serial-mediation-with-medfit.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘serial-mediation-with-medfit.Rmd’
       ...
     > fit <- lavaan::sem(model, data = dat)
     
     > mu <- medfit::extract_mediation(fit, treatment = "X", 
     +     mediator = c("M1", "M2"), outcome = "Y")
     
       When sourcing ‘serial-mediation-with-medfit.R’:
     Error: .onLoad failed in loadNamespace() for 'medfit', details:
       call: NULL
       error: Class union has not been registered with S4; please call S4_register(new_union(class_integer, class_double)).
     Execution halted
     ```

# roxygen2 (8.1.1)

* GitHub: <https://github.com/r-lib/roxygen2>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/roxygen2>

Run `revdepcheck::revdep_details(, "roxygen2")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
           [test.R:  1] @test 'a {' {parsed}
         Code
       * Run `testthat::snapshot_accept("tag-parser", "testthat")` to accept the change.
       * Run `testthat::snapshot_review("tag-parser", "testthat")` to review the change.
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 0 | SKIP 1 | PASS 1260 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# rtemis (1.2.7)

* GitHub: <https://github.com/rtemis-org/rtemis>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis>

Run `revdepcheck::revdep_details(, "rtemis")` for more info

## Newly broken

*   checking whether package ‘rtemis’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/rtemis/new/rtemis.Rcheck/00install.out’ for details.
     ```

## Newly fixed

*   checking examples ... ERROR
     ```
     ...
     > res <- resample(dat)
     2026-10-09 09:54:38 [0mInput contains more than one column; stratifying on last.[0m [resample]
     2026-10-09 09:54:38 [0mUsing max n bins possible = 2.[0m [kfold]
     > dat$Species <- factor(dat$Species)
     > dat_train <- dat[res[[1]], ]
     > dat_test <- dat[-res[[1]], ]
     > 
     > # Train GLM on a training/test split
     > mod_c_glm <- train(
     +   x = dat_train,
     +   dat_test = dat_test,
     +   algorithm = "glm"
     + )
     2026-10-09 09:54:38 [0mChecking data is ready for training... ✔ [check_supervised]
     2026-10-09 09:54:38 [0m▶[0m [train]
     2026-10-09 09:54:38 [0mTraining set: 90 cases x 4 features.[0m [summarize_supervised]
     2026-10-09 09:54:38 [0m    Test set: 10 cases x 4 features.[0m [summarize_supervised]
     2026-10-09 09:54:38 [0m// Max workers: c(`_R_CHECK_LIMIT_CORES_` = 1) { Algorithm: 1; Tuning: 1; Outer Resampling: 1 }[0m [get_n_workers]
     2026-10-09 09:54:38 [0mTraining GLM Classification...[0m [train]
     2026-10-09 09:54:38 [0mChecking data is ready for training... ✔ [check_supervised]
     2026-10-09 09:54:38 ✖ rtemis_dependency_error[0m [auc]
     Error in auc() : Please install the following dependency:
         - lightAUC 
     Calls: train ... classification_metrics -> auc -> check_dependencies -> abort
     Execution halted
     ```

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       
       Backtrace:
           ▆
        1. └─rtemis::train(x = datc, algorithm = "glm") at test_to_json.R:70:1
        2.   └─rtemis:::make_Supervised(...)
        3.     └─rtemis:::Classification(...)
        4.       └─rtemis::classification_metrics(...)
        5.         └─rtemis:::auc(...)
        6.           └─rtemis.core::check_dependencies("lightAUC")
        7.             └─rtemis.core::abort(...)
       
       [ FAIL 5 | WARN 0 | SKIP 4 | PASS 203 ]
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
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/rtemis/new/rtemis.Rcheck/rtemis’


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

Run `revdepcheck::revdep_details(, "rtemis.a3")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/rtemis.a3/new/rtemis.a3.Rcheck/00install.out’ for details.
     ```

# rtemis.core (0.4.6)

* GitHub: <https://github.com/rtemis-org/rtemis.core>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis.core>

Run `revdepcheck::revdep_details(, "rtemis.core")` for more info

## Newly broken

*   checking whether package ‘rtemis.core’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.core’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/rtemis.core/new/rtemis.core.Rcheck/00install.out’ for details.
     ```

# rtemis.llm (0.8.7)

* GitHub: <https://github.com/rtemis-org/llm>
* Email: <mailto:gennatas@gmail.com>
* GitHub mirror: <https://github.com/cran/rtemis.llm>

Run `revdepcheck::revdep_details(, "rtemis.llm")` for more info

## Newly broken

*   checking whether package ‘rtemis.llm’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘data.table:::=’ when loading ‘rtemis.llm’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/rtemis.llm/new/rtemis.llm.Rcheck/00install.out’ for details.
     ```

# S7schema (0.1.2)

* GitHub: <https://github.com/NovoNordisk-OpenSource/S7schema>
* Email: <mailto:oath@novonordisk.com>
* GitHub mirror: <https://github.com/cran/S7schema>

Run `revdepcheck::revdep_details(, "S7schema")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
       
       [ FAIL 27 | WARN 0 | SKIP 0 | PASS 63 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘S7schema.Rmd’
       ...
     
     > ex_config <- system.file("examples/config.yml", package = "S7schema")
     
     > ex_schema <- system.file("examples/schema.json", package = "S7schema")
     
     > validate_yaml(file = ex_config, schema = ex_schema)
     
       When sourcing ‘S7schema.R’:
     Error: argument "_parent" is missing, with no default
     Execution halted
     when running code in ‘use-in-package.Rmd’
       ...
     +         S7::new_obje .... [TRUNCATED] 
     
     > config_path <- system.file("examples/config.yml", 
     +     package = "S7schema")
     
     > x <- my_config_class(file = config_path)
     
       When sourcing ‘use-in-package.R’:
     Error: argument "_parent" is missing, with no default
     Execution halted
     ```

# shinychat (0.5.0)

* GitHub: <https://github.com/posit-dev/shinychat>
* Email: <mailto:garrick@adenbuie.com>
* GitHub mirror: <https://github.com/cran/shinychat>

Run `revdepcheck::revdep_details(, "shinychat")` for more info

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

Run `revdepcheck::revdep_details(, "shinyCohortBuilder")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       The following object is masked from ‘package:shiny’:
       
           code
       
       The following objects are masked from ‘package:stats’:
       
           filter, step
       
       Error in S7::new_object(S7::S7_object(), type = "discrete", id = id, name = name,  : 
         `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
       
       [ FAIL 121 | WARN 1 | SKIP 13 | PASS 254 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
     ...
     Errors in running code in vignettes:
     when running code in ‘gui-filter-layer.Rmd’
       ...
     
     
     > iris_source <- set_source(tblist(iris = iris))
     
     > species_filter <- filter(type = "discrete", id = "species", 
     +     dataset = "iris", variable = "Species", value = "setosa")
     
       When sourcing ‘gui-filter-layer.R’:
     Error: `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Execution halted
     when running code in ‘shinyCohortBuilder.Rmd’
       ...
     > options(tibble.print_min = 5)
     
     > iris_source <- autofilter(set_source(tblist(iris = iris)))
     
       When sourcing ‘shinyCohortBuilder.R’:
     Error: ℹ In index: 1.
     ℹ With name: iris.Sepal.Length.
     Caused by error in `S7::new_object()` at cohortBuilder/R/filter.R:188:5:
     ! `_parent` must be an instance of <cohortBuilder::CbFilter>, not <S7_object>.
     Execution halted
     ```

# shinyfilters (0.3.1)

* GitHub: <https://github.com/joshwlivingston/shinyfilters>
* Email: <mailto:joshwlivingston@gmail.com>
* GitHub mirror: <https://github.com/cran/shinyfilters>

Run `revdepcheck::revdep_details(, "shinyfilters")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
       
       [ FAIL 14 | WARN 51 | SKIP 0 | PASS 308 ]
       Error:
       ! Test failures.
       Execution halted
     ```

*   checking running R code from vignettes ...
     ```
       ‘customizing-shinyfilters.Rmd’ using ‘UTF-8’... OK
       ‘filter-input-catalog.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘filter-input-catalog.Rmd’
       ...
         </label>
       </div>
     </div>
     
     > filterInput(x = as.list(letters[1:10]), inputId = "id", 
     +     label = "Pick a letter:", inline = TRUE, radio = TRUE)
     
       When sourcing ‘filter-input-catalog.R’:
     Error: no applicable method for `@` applied to an object of class "S7_base_class"
     Execution halted
     ```

# shinyOAuth (0.6.1)

* GitHub: <https://github.com/lukakoning/shinyOAuth>
* Email: <mailto:koningluka@gmail.com>
* GitHub mirror: <https://github.com/cran/shinyOAuth>

Run `revdepcheck::revdep_details(, "shinyOAuth")` for more info

## Newly broken

*   checking whether package ‘shinyOAuth’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: replacing previous import ‘S7:::=’ by ‘rlang:::=’ when loading ‘shinyOAuth’
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/shinyOAuth/new/shinyOAuth.Rcheck/00install.out’ for details.
     ```

# statim (0.1.0)

* GitHub: <https://github.com/s7-stats/statim>
* Email: <mailto:joshua.marie.k@gmail.com>
* GitHub mirror: <https://github.com/cran/statim>

Run `revdepcheck::revdep_details(, "statim")` for more info

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

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

*   checking running R code from vignettes ...
     ```
       ‘statim.Rmd’ using ‘UTF-8’... failed
      ERROR
     Errors in running code in vignettes:
     when running code in ‘statim.Rmd’
       ...
       group estimate t_stat    df  p_val lower_90 upper_90
       <chr>    <dbl>  <dbl> <dbl>  <dbl>    <dbl>    <dbl>
     1 group    -1.58  -1.86  17.8 0.0794    -3.05   -0.107
     
     > tidy(conclude(prepare(define_model(mtcars, mpg ~ .), 
     +     LINEAR_REG)))
     
       When sourcing ‘statim.R’:
     Error: unused argument (family = "gaussian")
     Execution halted
     ```

*   checking whether package ‘statim’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Note: possible error in 'class_lm_object(terms = fit$terms, ': unused argument (family = "gaussian") 
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/statim/new/statim.Rcheck/00install.out’ for details.
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

Run `revdepcheck::revdep_details(, "tidyllm")` for more info

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

Run `revdepcheck::revdep_details(, "typedjson")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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

Run `revdepcheck::revdep_details(, "vecvec")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
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
       
       [ FAIL 12 | WARN 0 | SKIP 0 | PASS 503 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# waldo (0.6.2)

* GitHub: <https://github.com/r-lib/waldo>
* Email: <mailto:hadley@posit.co>
* GitHub mirror: <https://github.com/cran/waldo>

Run `revdepcheck::revdep_details(, "waldo")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       +   `attr(new, '_S7_class')$class@parent@name`: "A"        
       +   
       and 30 more ...
       
       
       ── Snapshots ───────────────────────────────────────────────────────────────────
       To review and process snapshots locally:
       * Locate check directory.
       * Copy 'tests/testthat/_snaps' to local package.
       * Run `testthat::snapshot_accept()` to accept all changes.
       * Run `testthat::snapshot_review()` to review all changes.
       [ FAIL 1 | WARN 1 | SKIP 0 | PASS 182 ]
       Error:
       ! Test failures.
       Execution halted
     ```

