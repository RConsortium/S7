## R CMD check results

The November 10 checks of the development version at commit `432d4d1d` completed with 0 errors and 0 warnings on:

- Linux: R-devel and R 4.2, 4.3, 4.4, 4.5, and 4.6.
- macOS and Windows: R 4.6.2.

R 4.6 and R-devel report 1 NOTE for the non-API call to `Rf_findVarInFrame`. This is the known issue covered by our existing arrangement with CRAN; a replacement is not yet available.

Check results: https://github.com/RConsortium/S7/actions/runs/42848257951

## revdepcheck results

An initial cloud run checked 121 reverse dependencies. We investigated the new problems and prepared fixes in S7 and compatibility patches for downstream packages. These checks were run during release preparation and have not been repeated after the compatibility updates.

Maintainers on 41 downstream pull requests were notified on November 2 of the planned November 16 submission. Many compatibility updates are now on CRAN. As of November 16, corresponding updates are still pending on CRAN for ale, apa7, btw, cohortBuilder, dcmstan, filtro, imply, joinery, measr, medfit, mixtime, myTAI, shinychat, statim, typedjson, and vecvec.

The maintainers of imply and measr plan to release after S7 1.0.0 is available on CRAN. The myTAI maintainer reports that CRAN prefers S7 to be submitted first.

Downstream patches, notifications, and release plans: https://github.com/RConsortium/S7/issues/786
