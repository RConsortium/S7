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
# bregr ()

* GitHub: <https://github.com/RConsortium/S7>
* Email: <mailto:hadley@posit.co>

Run `revdepcheck::revdep_details(, "bregr")` for more info

## Error before installation

### Devel

```

  There are binary versions available but the source versions are later:
                 binary source needs_compilation
enrichplot       1.32.0 1.32.1             FALSE
statsExpressions  2.1.1  2.1.2             FALSE



installing the source packages ‘enrichplot’, ‘GO.db’, ‘statsExpressions’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status


```
### CRAN

```

  There are binary versions available but the source versions are later:
                 binary source needs_compilation
enrichplot       1.32.0 1.32.1             FALSE
statsExpressions  2.1.1  2.1.2             FALSE



installing the source packages ‘enrichplot’, ‘GO.db’, ‘statsExpressions’

Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘enrichplot’ had non-zero exit status


```
# dataquieR (2.8.15)

* Email: <mailto:stephan.struckmann@uni-greifswald.de>
* GitHub mirror: <https://github.com/cran/dataquieR>

Run `revdepcheck::revdep_details(, "dataquieR")` for more info

## In both

*   R CMD check timed out


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
# gridmicrotex (0.2.0)

* GitHub: <https://github.com/adayim/gridmicrotex>
* Email: <mailto:ad938@cam.ac.uk>
* GitHub mirror: <https://github.com/cran/gridmicrotex>

Run `revdepcheck::revdep_details(, "gridmicrotex")` for more info

## In both

*   checking whether package ‘gridmicrotex’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/new/gridmicrotex.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘gridmicrotex’ ...
** this is package ‘gridmicrotex’ version ‘0.2.0’
** package ‘gridmicrotex’ successfully unpacked and MD5 sums checked
** using staged installation
configure: fribidi found; bidirectional reordering enabled.
** libs
specified C++17
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++17
using SDK: ‘MacOSX27.0.sdk’
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘gridmicrotex’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/new/gridmicrotex.Rcheck/00LOCK-gridmicrotex/00new/gridmicrotex/libs/gridmicrotex.so':
  dlopen(/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/new/gridmicrotex.Rcheck/00LOCK-gridmicrotex/00new/gridmicrotex/libs/gridmicrotex.so, 0x0006): symbol not found in flat namespace '_png_create_info_struct'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/new/gridmicrotex.Rcheck/gridmicrotex’


```
### CRAN

```
* installing *source* package ‘gridmicrotex’ ...
** this is package ‘gridmicrotex’ version ‘0.2.0’
** package ‘gridmicrotex’ successfully unpacked and MD5 sums checked
** using staged installation
configure: fribidi found; bidirectional reordering enabled.
** libs
specified C++17
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.3.34.2)’
using C++17
using SDK: ‘MacOSX27.0.sdk’
...
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘gridmicrotex’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/old/gridmicrotex.Rcheck/00LOCK-gridmicrotex/00new/gridmicrotex/libs/gridmicrotex.so':
  dlopen(/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/old/gridmicrotex.Rcheck/00LOCK-gridmicrotex/00new/gridmicrotex/libs/gridmicrotex.so, 0x0006): symbol not found in flat namespace '_png_create_info_struct'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/tomasz/github/RConsortium/S7/revdep/checks.noindex/gridmicrotex/old/gridmicrotex.Rcheck/gridmicrotex’


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


# risk.assessr (4.1.3)

* GitHub: <https://github.com/pharmaverse/risk.assessr>
* Email: <mailto:edward.gillian-ext@sanofi.com>
* GitHub mirror: <https://github.com/cran/risk.assessr>

Run `revdepcheck::revdep_details(, "risk.assessr")` for more info

## In both

*   R CMD check timed out


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
