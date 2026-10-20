# Reverse dependency investigation

Investigated all 48 unique packages in the committed reports: 37 packages with new problems and 11 that failed to check. This includes installation failures repeated in problems.md. Source observations and cited log excerpts are included in [the evidence appendix](investigation/evidence.md). Findings include source pointers, proposed fix owners, rough effort estimates, and validation limits.

The report is based on cloud run 9d2b3da3-5ec9-42ac-a8c0-487f25463f7a, which reports 121 reverse dependencies. Local checks.noindex and data.sqlite belong to a separate partial run and sometimes include additional environmental failures. The per-package cloud statuses below were independently read from cloud old/new 00check.log files. Focused local reproductions compare installed S7 0.2.2 and 0.2.2.9000 in separate R sessions; they do not establish that any proposed fixes pass full package checks.

This is the investigation snapshot recorded before implementing fixes. It is shared for discussion and is not intended to be merged. No S7 implementation changes are included. Effort estimates describe likely patch scope and are provisional; dependency rebuilds and cross-version checks can add time. Source status statements describe the checkouts inspected during the investigation, not a fresh upstream review. The [later local reports](local-results/README.md) are a separate snapshot; their additional failures have not all been investigated here.

The simplified legacy-mapping reproduction establishes an old/new behavior change, but does not define the full valid legacy-object contract. A compatibility repair still needs to inspect and test the actual saved object from the installed package.

## Fix order

| Work | Packages affected | Proposed repair | Scope |
|:---|:---|:---|:---|
| Defaulted dispatch arguments | iAR, ggtime through mixtime | Distinguish an omitted argument with a default from a truly missing argument in S7 dispatch. | Focused S7 patch and public regressions; hours. |
| Foreign parent exported under an alias | ggplotplus, mixtime | Preserve the exported class binding when generating a runtime parent reference. Explicit new_external_class() using the exported alias is a verified workaround. | Focused S7 patch with package-boundary regression; hours. |
| data.frame validation | fr | Inspect underlying columns without invoking the subclass metadata as.list() method. | Focused S7 patch and regression; hours. |
| Legacy saved mapping | anansi, ggdiagram, myTAI through ggforce | Preserve compatibility with valid saved pre-versioned mappings; test the actual installed ggforce object. | S7 compatibility investigation/patch; hours, with dependent installation checks. |
| Namespace and constructor migrations | btw, filtro, S7schema, mighty.metadata, ggarrow, caugi, cohortBuilder, shinyCohortBuilder, deltapif, rtemis.a3, dcmstan | Apply documented naming/build-hook changes, pass actual parent objects, and avoid replaying immutable initialization through setters. | Mostly small package patches; dcmstan/cohortBuilder need several constructors checked. |
| S4-facing union registration | imply, medfit, PFIM | Explicitly register union property types before S4 class registration. | Small package migrations; verify registration order and fresh-library installation. |
| Import collisions and documentation | ale, apa7, ellmer, GGally, joinery, rtemis.core, rtemis.llm, shinyOAuth, querychat, tidyllm, shinychat, measr, marquee | Make imports explicit/exclude conflicting :=; update forwarded constructor signatures and property-default docs. Fix dependency warnings once and rerun affected consumers. | Mostly small namespace/docs changes. |
| Remaining behavior/integration work | ggpath, nflplotR, rtemis, statim, shinyfilters, gglogger, ggplot2, ggside, GitAI, covr, typedjson, vecvec | Respect parent property types; preserve captured expressions; update test mocks and class graph/coverage integrations. | Hours for local migrations; roughly 1–3 days for serialization/instrumentation work. |

The exact typedjson 0.1.1 round-trip change and vecvec 1.3.0 dimension loss are reproduced. The precise typedjson decoder identity repair, covr live-closure instrumentation repair, and vecvec callback evaluation cause still need implementation-level isolation. The vecvec callback error occurs under both S7 versions on local R 4.6.1, while it is new-only in the cloud comparison; do not silently treat that local result as differential evidence.

## Reproductions

These scripts and outputs are archived as recorded, including the original local library paths. They are evidence from the investigation, not a portable test suite. Structured findings are available in [findings.json](investigation/findings.json).

- [s7-revdep-constructors-repro-new.txt](investigation/reproductions/s7-revdep-constructors-repro-new.txt)
- [s7-revdep-constructors-repro-old.txt](investigation/reproductions/s7-revdep-constructors-repro-old.txt)
- [s7-revdep-constructors-repro.R](investigation/reproductions/s7-revdep-constructors-repro.R)
- [s7-revdep-dcmstan-custom3-new.txt](investigation/reproductions/s7-revdep-dcmstan-custom3-new.txt)
- [s7-revdep-dcmstan-new.txt](investigation/reproductions/s7-revdep-dcmstan-new.txt)
- [s7-revdep-dcmstan-old.txt](investigation/reproductions/s7-revdep-dcmstan-old.txt)
- [s7-revdep-dcmstan-repro-custom3.R](investigation/reproductions/s7-revdep-dcmstan-repro-custom3.R)
- [s7-revdep-dcmstan-repro.R](investigation/reproductions/s7-revdep-dcmstan-repro.R)
- [s7-revdep-inherits-repro.txt](investigation/reproductions/s7-revdep-inherits-repro.txt)
- [s7-revdep-root-reproductions.txt](investigation/reproductions/s7-revdep-root-reproductions.txt)

## Package findings

### ale 0.5.3

New install WARNING is a namespace collision: ale imports all of S7 and explicitly imports rlang:::=; S7 0.2.2.9000 newly exports :=, so rlang replaces the earlier S7 binding during namespace load. Old check is OK.

**Fix:** Exclude := from ale's S7 import (or replace the broad import with explicit S7 imports). Small NAMESPACE/roxygen edit, about 10–20 minutes plus package check.

**Owner:** ale. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘ale’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current ale source still has both imports; no package check rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/ale/new/ale.Rcheck/00check.log:22](investigation/evidence.md#evidence-070)
- [clone/ale/NAMESPACE:11](investigation/evidence.md#evidence-004)
- [clone/ale/NAMESPACE:36](investigation/evidence.md#evidence-004)
- [S7/revdep/library.noindex/S7/new/S7/NAMESPACE:44](investigation/evidence.md#evidence-153)

### anansi 1.2.0

The cloud report has an empty pre-installation failure, so its original preparation failure cannot be diagnosed from the downloaded cloud log. The later local new run gives a concrete installation failure in ggforce .onLoad(): S7_data() rejects a saved class-vector-only ggplot2 mapping while updating GeomArc0 defaults. The local old run gets past installation and instead has an unrelated font/vignette error. Public old/new S7 API comparisons reproduce acceptance versus rejection of the legacy mapping representation.

**Fix:** S7 should add backward compatibility for the pre-versioned serialized object representation in S7_data()/check_is_S7(), with a public regression covering an old-format ggplot2 mapping; expected effort 2–4 hours including representation review and regression coverage. Rebuilding ggforce against the new S7 is a short-term workaround, but not a package-specific root fix. The font failure needs an environment-independent vignette font choice.

**Owner:** S7 for legacy serialized-object compatibility; ggforce rebuild as a short-term workaround; anansi for the unrelated font-sensitive vignette. **Confidence:** medium-high.

**Cloud checks:** old: no completed check log; new: no completed check log.

**Validation:** Cloud old/new completed check logs are unavailable for anansi. The local new 00install.out identifies ggforce and S7_data; the local old install succeeds. Public old/new legacy-representation comparison is saved with the reproductions. The original cloud preparation failure remains unexplained.

**Available source:** No anansi-side change found in current Bioconductor clone. Current ggforce source still defines GeomArc0 default_aes with aes() and registers geom defaults at load time; the check uses ggforce 0.5.0 with ggplot2 4.0.3 and S7 0.2.2.9000. A current ggforce rebuild would construct fresh defaults, but the checked artifact fails while loading its saved legacy default mapping.

**Evidence:**

- [S7/revdep/checks.noindex/anansi/new/anansi.Rcheck/00install.out:7](investigation/evidence.md#evidence-061)
- [S7/revdep/checks.noindex/anansi/new/anansi.Rcheck/00install.out:10](investigation/evidence.md#evidence-061)
- [S7/revdep/checks.noindex/anansi/old/anansi.Rcheck/00check.log:23](investigation/evidence.md#evidence-062)
- [S7/revdep/checks.noindex/anansi/old/anansi.Rcheck/00check.log:69](investigation/evidence.md#evidence-062)
- [S7/R/inherits.R:49](investigation/evidence.md#evidence-058)
- [S7/R/data.R:27](investigation/evidence.md#evidence-055)
- [clone/ggforce/R/zzz.R:17](investigation/evidence.md#evidence-018)
- [saved/evidence/s7-revdep-inherits-repro.txt:1](investigation/evidence.md#evidence-128)
- [saved/evidence/s7-revdep-inherits-repro.txt:4](investigation/evidence.md#evidence-128)
- [clone/ggforce/R/zzz.R:18](investigation/evidence.md#evidence-018)
- [clone/ggforce/R/arc.R:200](investigation/evidence.md#evidence-017)
- [clone/ggforce/R/arc.R:202](investigation/evidence.md#evidence-017)
- [aes.R:108](investigation/evidence.md#evidence-158)
- [aes.R:153](investigation/evidence.md#evidence-158)
- [aes.R:168](investigation/evidence.md#evidence-158)
- [all-classes.R:281](investigation/evidence.md#evidence-159)
- [all-classes.R:282](investigation/evidence.md#evidence-159)
- [geom-update-defaults.R:142](investigation/evidence.md#evidence-160)
- [geom-update-defaults.R:150](investigation/evidence.md#evidence-160)

### apa7 0.1.3

New install WARNING is the same S7/rlang := collision as ale: broad import(S7) plus importFrom(rlang, ':='). Old check is OK.

**Fix:** Exclude := from the S7 import or use explicit imports. Small namespace/documentation update, about 10–20 minutes plus package check.

**Owner:** apa7. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘apa7’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current source retains both imports; no package check rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/apa7/new/apa7.Rcheck/00check.log:22](investigation/evidence.md#evidence-071)
- [clone/apa7/NAMESPACE:44](investigation/evidence.md#evidence-005)
- [clone/apa7/NAMESPACE:45](investigation/evidence.md#evidence-005)
- [S7/revdep/library.noindex/S7/new/S7/NAMESPACE:44](investigation/evidence.md#evidence-153)

### bidsr 0.1.1

New installation failure: S7 rejects the two-entry signature registered for extract_bracket.generic; the failing generic dispatches only on x. CRAN/old installs and tests passed.

**Fix:** Register the method on BIDSClassBase alone (remove the name dimension) and audit sibling bracket registrations. Small package patch, roughly 1–2 hours.

**Owner:** bidsr. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘bidsr’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check Status: OK; new fails at lazy loading with signature must be length 1. Source confirms the two-dimensional signature.

**Available source:** Fetched current bidsr HEAD retains the signature; no upstream fix visible.

**Evidence:**

- [S7/revdep/problems.md:35](investigation/evidence.md#evidence-154)
- [cloud/bidsr/new/bidsr.Rcheck/00install.out:1](investigation/evidence.md#evidence-073)
- [cloud/bidsr/new/bidsr.Rcheck/00check.log:1](investigation/evidence.md#evidence-072)
- [saved/tested-sources/bidsr/R/class002-bids_class_base.R:312](investigation/evidence.md#evidence-136)

### btw 1.5.0

Two direct package incompatibilities are confirmed from exact tested btw 1.5.0 source. First, registration of method(as.character, BTW) without S7_on_build leaves an external-generic sentinel in the btw namespace; the vendored map_chr helper passes that list to rlang::as_function and fails. Second, btw() no-news branch calls BTW("") positionally; with current S7 constructor formals (..., text, settings), the empty string is forwarded to ellmer::ContentText and errors as unused. Both are reproducible old-pass/new-fail with the tested source.

**Fix:** Add S7::S7_on_build() after method registrations to replace external-generic sentinels at package build time; change the no-news constructor call to BTW(text = ""). Small migration, roughly 1–3 hours plus package checks.

**Owner:** btw migration; no core change indicated. **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs, 1 NOTE.

**New-run check findings:** R code for possible problems: NOTE; examples: ERROR; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Installed exact btw_1.5.0 tarball into isolated old/new libraries. as.character in btw namespace is base builtin old and S7_generic_sentinel list new; BTW("") succeeds old and errors unused argument new, while BTW(text = "") succeeds new. Cloud new log has test/example failures; exact no-news call is present in source.

**Available source:** Fetched local btw clone retains both source patterns; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:93](investigation/evidence.md#evidence-154)
- [cloud/btw/new/btw.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-074)
- [saved/tested-sources/btw/R/btw.R:105](investigation/evidence.md#evidence-137)
- [saved/tested-sources/btw/R/btw.R:134](investigation/evidence.md#evidence-137)
- [saved/tested-sources/btw/R/import-standalone-purrr.R:61](investigation/evidence.md#evidence-138)
- [S7/R/hooks.R:141](investigation/evidence.md#evidence-057)
- [S7/R/constructor.R:219](investigation/evidence.md#evidence-054)

### caugi 1.3.0

Old check passed; new examples and 37 of 38 tests fail because export constructors call S7::new_object(caugi_export, ...) with a class definition where the API requires a parent object. One additional test directly uses S7:::as_generic and internal generic sentinel details.

**Fix:** Construct with an instance of the parent class (for example caugi_export()) and replace tests’ internal S7 calls with public generic/method APIs. Small-to-moderate package patch, 2–4 hours.

**Owner:** caugi. **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check passed. Cloud new example and most tests fail at export; one separate test calls S7:::as_generic. Local partial logs agree on those failure families.

**Available source:** Fetched current caugi HEAD still passes the class definition to new_object; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:171](investigation/evidence.md#evidence-154)
- [cloud/caugi/new/caugi.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-076)
- [cloud/caugi/new/caugi.Rcheck/00check.log:1](investigation/evidence.md#evidence-075)
- [saved/tested-sources/caugi/R/all-classes.R:36](investigation/evidence.md#evidence-139)
- [saved/tested-sources/caugi/R/format-dot.R:25](investigation/evidence.md#evidence-140)

### cohortBuilder 1.0.0

Cloud old check passed; new check has examples, tests, and vignette errors from custom child constructors for CbFilter subclasses passing S7::S7_object() as _parent. Current S7 requires an instance of the declared parent. The many test errors share this one constructor defect.

**Fix:** Small to medium (hours): update each custom filter constructor to construct/forward a CbFilter parent object before calling new_object(); check all seven constructors and rerun package checks.

**Owner:** cohortBuilder. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories (examples, tests, vignette rebuilding). No rerun performed.

**Available source:** The available clone is still 1.0.0 and contains the failing S7_object() parent calls; no fix is evident.

**Evidence:**

- [cloud/cohortBuilder/new/cohortBuilder.Rcheck/00check.log:78](investigation/evidence.md#evidence-077)
- [clone/cohortBuilder/R/filter.R:78](investigation/evidence.md#evidence-006)
- [clone/cohortBuilder/R/filter.R:125](investigation/evidence.md#evidence-006)
- [S7/R/class.R:364](investigation/evidence.md#evidence-053)

### covr 3.6.5

Cloud old check passes; new adds one S7 coverage failure: coverage counts for property getter, setter, and validator code are zero where expected counts are nonzero. Isolated package_coverage() with exact covr 3.6.5 and the check fixture reproduces old expected counts and new zeros. covr traverses class/property metadata, while new S7 stores instance class identity through a class-reference environment. The exact missed-link mechanism (class-ref reachability versus closure replacement of copied metadata) remains to isolate.

**Fix:** Update covr S7 instrumentation to instrument the closures reached by instance property access, then validate the existing TestS7 fixture against both S7 libraries. Likely 0.5–2 days.

**Owner:** covr integration; S7 only if the class-ref representation breaks an established instrumentation contract. **Confidence:** high for the new coverage regression; medium for class-ref as the exact cause.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old is OK; cloud new fails test-S7.R:5. Local exact-package reproduction gives old counts 1,1,1,2,5,0,5,0,5,1,1,2,1,1,0 and new gives zero for property getter/setter/validator lines. This is a local reproduction; no compiled/gcov failures are inherited in the cloud comparison.

**Available source:** Current covr clone has S7 traversal in R/S7.R:46; no fix assessed.

**Evidence:**

- [S7/revdep/problems.md:337](investigation/evidence.md#evidence-154)
- [cloud/covr/new/covr.Rcheck/tests/testthat.Rout.fail:53](investigation/evidence.md#evidence-078)
- [S7/revdep/checks.noindex/covr/new/covr.Rcheck/tests/testthat/test-S7.R:1](investigation/evidence.md#evidence-064)
- [S7/revdep/checks.noindex/covr/new/covr.Rcheck/tests/testthat/TestS7/R/foo.R:3](investigation/evidence.md#evidence-063)
- [clone/covr/R/S7.R:46](investigation/evidence.md#evidence-007)
- [S7/R/class.R:190](investigation/evidence.md#evidence-053)
- [S7/src/prop.c:41](investigation/evidence.md#evidence-156)

### dcmstan 0.1.0

Cloud old check passes; development S7 adds examples, tests, and vignette failures at the custom @model setter. dcmstan defines measurement/structural parent classes with a writable model property, then overrides that property on 12 child model classes with a setter that errors once self@model is non-NULL. Development S7's generated child constructor first constructs the parent with the requested model and then passes model again to new_object(), which invokes the child setter on an already initialized value. An isolated public S7 reproduction with the same parent property and setter passes on 0.2.2 and fails on 0.2.2.9000; a custom parent-once constructor passes on dev and later mutation still errors.

**Fix:** Small to medium (hours): add family-specific custom constructors for measurement and structural child classes. Each should call its parent constructor once with the final model/model_args, then promote/build the child with new_object(parent_object) without replaying model through the child setter. This preserves the strict read-only-after-initialization invariant. Verify all public factories (lcdm, dina, dino, crum, nida, nido, ncrum; unconstrained, independent, loglinear, hdcm, bayesnet), and assert post-construction model assignment still errors.

**Owner:** dcmstan. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check Status OK. Cloud new has 3 ERROR categories: examples, tests, and vignette rebuilding. Repro script /tmp/s7-revdep-dcmstan-repro.R: old constructor succeeds; dev generated constructor errors; custom parent-once constructor succeeds on dev and a later model mutation is still rejected. No dcmstan source was changed.

**Available source:** The current dcmstan clone is 0.1.0.9000 and retains the same setter and child property overrides as tested 0.1.0; no constructor workaround is present.

**Evidence:**

- [cloud/dcmstan/old/dcmstan.Rcheck/00check.log:63](investigation/evidence.md#evidence-080)
- [cloud/dcmstan/new/dcmstan.Rcheck/00check.log:95](investigation/evidence.md#evidence-079)
- [cloud/dcmstan/new/dcmstan.Rcheck/00check.log:187](investigation/evidence.md#evidence-079)
- [cloud/dcmstan/new/dcmstan.Rcheck/00check.log:953](investigation/evidence.md#evidence-079)
- [saved/tested-sources/dcmstan/R/zzz-class-model-components.R:491](investigation/evidence.md#evidence-141)
- [saved/tested-sources/dcmstan/R/zzz-class-model-components.R:501](investigation/evidence.md#evidence-141)
- [saved/tested-sources/dcmstan/R/zzz-class-model-components.R:508](investigation/evidence.md#evidence-141)
- [saved/tested-sources/dcmstan/R/zzz-class-model-components.R:551](investigation/evidence.md#evidence-141)
- [saved/tested-sources/dcmstan/R/zzz-class-model-components.R:579](investigation/evidence.md#evidence-141)
- [clone/dcmstan/R/zzz-class-model-components.R:491](investigation/evidence.md#evidence-008)
- [S7/R/constructor.R:132](investigation/evidence.md#evidence-054)
- [S7/R/constructor.R:145](investigation/evidence.md#evidence-054)
- [S7/NEWS.md:17](investigation/evidence.md#evidence-050)

### deltapif 0.4.5

Cloud old check passed; new examples, tests, and vignette rebuilding fail because child classes such as pif_atomic_class call new_object(S7_object()) while declaring pif_class as their parent. The new parent check rejects the unrelated base S7_object. A separate R-code NOTE reports unimported stats::coef and is not the constructor cause.

**Fix:** Medium (hours to a day): audit the five custom class constructors in 01-classes.R and pass the actual parent object; add stats::coef to Imports/NAMESPACE as appropriate to clear the separate NOTE.

**Owner:** deltapif. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs, 1 NOTE.

**New-run check findings:** R code for possible problems: NOTE; examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories plus 1 NOTE.

**Available source:** The available 0.4.5 clone retains the S7_object() parent constructors; no fix is evident.

**Evidence:**

- [cloud/deltapif/new/deltapif.Rcheck/00check.log:85](investigation/evidence.md#evidence-081)
- [clone/deltapif/R/01-classes.R:354](investigation/evidence.md#evidence-009)
- [clone/deltapif/R/01-classes.R:548](investigation/evidence.md#evidence-009)
- [S7/R/class.R:364](investigation/evidence.md#evidence-053)
- [cloud/deltapif/new/deltapif.Rcheck/00check.log:44](investigation/evidence.md#evidence-081)

### ellmer 0.5.0

Two newly broken check categories: install WARNING from import(S7) colliding with import(rlang) on :=; codoc WARNING because AssistantPartialTurn now has code formals (..., reason) while Turn.Rd documents all inherited AssistantTurn properties explicitly. The 11 test failures are inherited: both old and new runs fail provider tests on HTTP requests/cassette lookup, unrelated to S7.

**Fix:** Exclude := from the S7 import. Update the class documentation to show ... forwarded to AssistantTurn and re-document. Together this is a small NAMESPACE/docs patch, roughly 30–60 minutes. The provider-test errors need network/cassette test-environment work, not an S7 fix.

**Owner:** ellmer for namespace/docs; ellmer test harness or external provider environment for inherited provider failures. **Confidence:** high.

**Cloud checks:** old: 1 ERROR; new: 1 ERROR, 2 WARNINGs.

**New-run check findings:** whether package ‘ellmer’ can be installed: WARNING; for code/documentation mismatches: WARNING; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old/new full logs show new install/codoc warnings; tests fail in both runs (11 HTTP/cassette errors). Current clone retains the import collision and docs mismatch. No rerun.

**Available source:** Not fixed in current ellmer clone.

**Evidence:**

- [cloud/ellmer/new/ellmer.Rcheck/00check.log:22](investigation/evidence.md#evidence-082)
- [cloud/ellmer/new/ellmer.Rcheck/00check.log:52](investigation/evidence.md#evidence-082)
- [cloud/ellmer/old/ellmer.Rcheck/00check.log:61](investigation/evidence.md#evidence-083)
- [cloud/ellmer/new/ellmer.Rcheck/00check.log:128](investigation/evidence.md#evidence-082)
- [clone/ellmer/NAMESPACE:143](investigation/evidence.md#evidence-010)
- [clone/ellmer/NAMESPACE:145](investigation/evidence.md#evidence-010)
- [clone/ellmer/R/turns.R:179](investigation/evidence.md#evidence-011)
- [clone/ellmer/man/Turn.Rd:26](investigation/evidence.md#evidence-012)

### filtro 0.2.0

Old check passes; new examples and two rebuilt vignettes fail with could not find function fit. filtro imports fit from generics, registers six S7 methods on that external generic, and its .onLoad only calls methods_register(), not S7_on_build(). The source evidence points to the S7 external-generic sentinel migration: the fit binding is missing from the installed evaluation namespace. This is a package namespace migration finding, distinct from a generic documentation mistake.

**Fix:** Call S7::S7_on_build() from the package build hook/namespace setup so the external generic sentinel is stripped and the imported generics::fit binding is visible; verify installed exports and rerun examples/vignettes. Small migration, 1–3 hours.

**Owner:** filtro migration; S7 sentinel lifecycle if S7_on_build does not restore the imported binding. **Confidence:** medium-high.

**Cloud checks:** old: OK; new: 2 ERRORs.

**New-run check findings:** examples: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old passes; new examples/vignettes error on unqualified fit. NAMESPACE imports generics::fit; source registers S7::method(fit, ...) across score classes; .onLoad only calls methods_register. No package source changes or package load reproduction in this last pass.

**Available source:** Fetched local filtro HEAD retains these registrations and lacks S7_on_build; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:650](investigation/evidence.md#evidence-154)
- [cloud/filtro/new/filtro.Rcheck/00check.log:111](investigation/evidence.md#evidence-084)
- [clone/filtro/NAMESPACE:41](investigation/evidence.md#evidence-013)
- [clone/filtro/R/score-aov.R:157](investigation/evidence.md#evidence-014)
- [clone/filtro/R/zzz.R:1](investigation/evidence.md#evidence-015)
- [S7/R/hooks.R:143](investigation/evidence.md#evidence-057)

### fr 0.5.2

The cloud run has three new error categories: examples, tests, and vignettes. S7 data.frame validation uses vapply(self, NROW, ...), which coerces the classed object through its as.list() method. fr_tdr defines as.list() to return frictionless metadata, so validation counts metadata entries rather than data-frame columns. This is a confirmed S7 validation defect, rather than malformed compact row names.

**Fix:** Small S7 change (hours): validate the underlying data-frame columns without dispatching to the subclass as.list() method. Add a public-API regression using a data.frame subclass with a metadata as.list() method; verify fr::as_fr_tdr() and its examples after the fix.

**Owner:** S7. **Confidence:** high.

**Cloud checks:** old: 1 NOTE; new: 3 ERRORs, 1 NOTE.

**New-run check findings:** DESCRIPTION meta-information: NOTE; examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Exact fr 0.5.2 source loaded with pkgload: fr::as_fr_tdr(data.frame(x = 1:3), name = "example") succeeds with S7 0.2.2 and fails with development S7. A minimal public S7 subclass with a metadata as.list() override produces the same old/new difference.

**Available source:** Local fr HEAD still defines fr_tdr as class_data.frame subclass and same as_fr_tdr construction; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:720](investigation/evidence.md#evidence-154)
- [cloud/fr/new/fr.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-086)
- [cloud/fr/new/fr.Rcheck/00check.log:1](investigation/evidence.md#evidence-085)
- [saved/tested-sources/fr/R/fr_tdr.R:1](investigation/evidence.md#evidence-142)
- [saved/tested-sources/fr/R/fr_tdr.R:40](investigation/evidence.md#evidence-142)
- [S7/R/S3.R:266](investigation/evidence.md#evidence-051)
- [saved/evidence/s7-revdep-root-reproductions.txt:1](investigation/evidence.md#evidence-129)

### GGally 2.4.0

New install WARNING is caused by import(S7) and importFrom(rlang, ':='); the new S7 export collides with rlang's binding. Old check is OK.

**Fix:** Exclude := from the S7 import or use explicit imports. Small namespace change, about 10–20 minutes plus check.

**Owner:** GGally. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘GGally’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current clone retains both imports; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/GGally/new/GGally.Rcheck/00check.log:23](investigation/evidence.md#evidence-065)
- [clone/GGally/NAMESPACE:140](investigation/evidence.md#evidence-001)
- [clone/GGally/NAMESPACE:189](investigation/evidence.md#evidence-001)
- [S7/revdep/library.noindex/S7/new/S7/NAMESPACE:44](investigation/evidence.md#evidence-153)

### ggarrow 0.2.0

Old check passed; new examples and test fail because element_arrow calls new_object(.parent = parent), but current API formal is _parent; .parent is ignored by ... and required _parent stays missing.

**Fix:** Rename `.parent` to `_parent` in constructor; add focused constructor test. Tiny package patch, under an hour.

**Owner:** ggarrow. **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check passed; example and one test fail at same missing _parent argument.

**Available source:** Fetched current ggarrow HEAD still uses .parent at theme_elements.R:322; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:838](investigation/evidence.md#evidence-154)
- [cloud/ggarrow/new/ggarrow.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-088)
- [cloud/ggarrow/new/ggarrow.Rcheck/00check.log:1](investigation/evidence.md#evidence-087)
- [saved/tested-sources/ggarrow/R/theme_elements.R:321](investigation/evidence.md#evidence-143)
- [S7/R/class.R:411](investigation/evidence.md#evidence-053)

### ggdiagram 0.2.0

New package installation fails through ggforce .onLoad() while updating GeomArc0 default aesthetics. The mapping stored in the installed ggforce lazy-load database is a legacy class-vector-only ggplot2 mapping; development S7 rejects it in S7_data(), while old S7 accepts that representation. ggdiagram also has a separate warning for an unquoted ob_point property default.

**Fix:** Address legacy serialized-object compatibility in S7, using the installed ggforce mapping as the public regression case. Rebuilding ggforce is a workaround to verify, not a root repair. Focused compatibility work (hours), followed by ggdiagram, myTAI, and anansi installation checks. Also quote the ob_point property default.

**Owner:** S7 legacy-object compatibility; ggdiagram default warning. **Confidence:** high for shared failure; compatibility repair requires actual saved-object regression.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘ggdiagram’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR. The failing call is in ggforce .onLoad, not ggdiagram code.

**Available source:** The 0.2.0 clone retains the unquoted default. ggforce source was not among the assigned clones; no dependency fix was verified.

**Evidence:**

- [cloud/ggdiagram/new/ggdiagram.Rcheck/00install.out:12](investigation/evidence.md#evidence-089)
- [clone/ggdiagram/R/labels.R:483](investigation/evidence.md#evidence-016)
- [S7/revdep/problems.md:936](investigation/evidence.md#evidence-154)
- [all-classes.R:282](investigation/evidence.md#evidence-159)
- [geom-update-defaults.R:142](investigation/evidence.md#evidence-160)
- [geom-update-defaults.R:150](investigation/evidence.md#evidence-160)

### gglogger 0.1.8

Cloud old tests pass; new has 25 failures in captured plot/theme expressions and their later evaluation. The methods use substitute(e2, env = caller_env(2)). Development S7 changes the operator dispatch stack and now preserves the original expression directly in substitute(e2); the fixed caller depth instead retrieves the symbol e2.

**Fix:** Small package change (hours): use direct substitute(e2) in both ggplot and theme + methods. Retain expression-capture and evaluation tests. If both S7 generations must remain supported, make that compatibility requirement explicit before selecting an implementation.

**Owner:** gglogger. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** A public operator method reproduces the reversal: old S7 gives e2 for direct substitute() and the original expression at caller_env(2); development S7 gives the original expression directly and e2 at that fixed frame depth.

**Available source:** Fetched current gglogger HEAD still uses caller_env(2) and substitute(e2); no fix visible.

**Evidence:**

- [S7/revdep/problems.md:972](investigation/evidence.md#evidence-154)
- [cloud/gglogger/new/gglogger.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-091)
- [cloud/gglogger/new/gglogger.Rcheck/00check.log:1](investigation/evidence.md#evidence-090)
- [saved/tested-sources/gglogger/R/zzz.R:14](investigation/evidence.md#evidence-144)
- [S7/R/method-ops.R:30](investigation/evidence.md#evidence-059)
- [saved/evidence/s7-revdep-root-reproductions.txt:1](investigation/evidence.md#evidence-129)

### ggpath 1.1.1

Cloud old check passed; new install fails for two related S7 parent-contract violations: element_path widens inherited element_text@size to include grid units, and its custom constructor passes S7_object() instead of an element_text parent object. The property override is not a narrowing of the declared parent property. This terminal failure is property-type narrowing during class definition, not a #364 external-generic placeholder failure; ggpath declares no new_external_generic.

**Fix:** Medium (one to several days): redesign element_path's size representation so it respects element_text's property type while preserving unit sizing at render time, and construct from a valid element_text parent. This is more than renaming an argument. ggpath has a separate S7_on_build gap only if it retains external-generic registrations after class construction is fixed; this is not the reported install blocker.

**Owner:** ggpath. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘ggpath’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR.

**Available source:** Available 1.1.1 clone still widens size to a unit union and passes S7_object() to new_object(); no fix is evident.

**Evidence:**

- [cloud/ggpath/new/ggpath.Rcheck/00install.out:8](investigation/evidence.md#evidence-092)
- [saved/tested-sources/ggpath/R/theme_element.R:95](investigation/evidence.md#evidence-145)
- [saved/tested-sources/ggpath/R/theme_element.R:104](investigation/evidence.md#evidence-145)
- [saved/tested-sources/ggpath/R/theme_element.R:116](investigation/evidence.md#evidence-145)
- [S7/R/class.R:364](investigation/evidence.md#evidence-053)
- [S7/NEWS.md:13](investigation/evidence.md#evidence-050)

### ggplot2 4.0.3

The cloud run has old Status OK and exactly one new failed test: S7_data(aes) has class gg while the expected list is bare. Development S7 deliberately preserves the S3 parent class in S7_data(), as documented and covered by S7 tests. The separate later local run additionally has inherited graphics failures, operator-expression snapshot differences, and a changed element_text condition snapshot; those are not cloud failures.

**Fix:** Small package update (hours): make the aes expectation assert the intended underlying values and retained S3 parent class. For the additional local operator failures, migrate plot/theme methods from caller_env(2) to direct substitute(e2), preserving user expressions. Review the element_text condition snapshot against the new diagnostic contract; handle graphics/rendering differences separately.

**Owner:** ggplot2. **Confidence:** high for cloud cause; local condition change needs snapshot review.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check passes; cloud new test-aes.R:69 is the sole failed test. A public S7 subclass of an S3 parent returns a bare list from S7_data() on 0.2.2 and a list with the S3 parent class on development S7.

**Available source:** Current local ggplot2 checkout inspected; these checks are for tested 4.0.3 and no current-head validation run.

**Evidence:**

- [S7/revdep/problems.md:1072](investigation/evidence.md#evidence-154)
- [cloud/ggplot2/new/ggplot2.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-094)
- [cloud/ggplot2/new/ggplot2.Rcheck/00check.log:1](investigation/evidence.md#evidence-093)
- [saved/tested-sources/ggplot2/tests/testthat/test-aes.R:69](investigation/evidence.md#evidence-147)
- [saved/tested-sources/ggplot2/tests/testthat/test-plot.R:11](investigation/evidence.md#evidence-148)
- [saved/tested-sources/ggplot2/tests/testthat/test-theme.R:570](investigation/evidence.md#evidence-149)
- [S7/R/data.R:27](investigation/evidence.md#evidence-055)
- [S7/tests/testthat/test-data.R:80](investigation/evidence.md#evidence-157)
- [saved/evidence/s7-revdep-root-reproductions.txt:1](investigation/evidence.md#evidence-129)

### ggplotplus 0.5.7

Cloud old check passed; new install fails when ggplotplus subclasses ggplot2::class_ggplot. S7 resolves the external class by its internal name ggplot, but ggplot2 exports the class object as class_ggplot and exports ggplot as a function, so the required binding is not an S7 class. This is not the #364 temporary external-generic placeholder issue: neither package declares new_external_generic for the failing class construction, and ggplotplus has no S7_on_build() call; mixtime has external generics but class construction fails first.

**Fix:** Small (hours): work around the S7 alias-resolution regression with explicit S7 external-class references to the exported aliases class_ggplot and class_ggplot_built as parent classes, then verify package installation and plot-building APIs.

**Owner:** S7 (package workaround available). **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘ggplotplus’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR. Diagnosis follows the exported binding names in source and S7's external-class resolver. The reported terminal error is external class name/export resolution, not a generic-placeholder sentinel; S7_on_build() would not address this first failure. Public package-boundary repro with ggplot2 class_ggplot passes on S7 0.2.2, fails on 0.2.2.9000, and an explicit exported-alias reference passes on dev.

**Available source:** The 0.5.7 clone still uses class_ggplot and class_ggplot_built directly as parents; no alias-aware reference is present.

**Evidence:**

- [cloud/ggplotplus/new/ggplotplus.Rcheck/00install.out:9](investigation/evidence.md#evidence-095)
- [clone/ggplotplus/R/S7Classes.R:84](investigation/evidence.md#evidence-020)
- [clone/ggplotplus/R/S7Classes.R:100](investigation/evidence.md#evidence-020)
- [S7/R/external-class.R:156](investigation/evidence.md#evidence-056)
- [S7/NEWS.md:24](investigation/evidence.md#evidence-050)

### ggside 0.4.1

Cloud old check passes; new has one failure in the add_gg error message: the operand is rendered as e2 instead of the expected caller expression. ggplot2 plot/theme + methods use a fixed caller_env(2) depth, which no longer identifies the original operand after the S7 operator stack change. The additional helper/rendering failures in the later local run are separate.

**Fix:** Small dependency migration (hours): fix expression capture in ggplot2 plot/theme + methods using direct substitute(e2), then rerun the ggside error-message test. Preserve the assertion that the user sees the actual operand.

**Owner:** ggplot2; verify ggside afterward. **Confidence:** high for observed stack change and source; dependent rerun needed.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old Status OK, cloud new one test failure. The public operator reproduction confirms why the fixed depth retrieves e2 under development S7.

**Available source:** Fetched current ggside HEAD has same test expectation; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:1171](investigation/evidence.md#evidence-154)
- [cloud/ggside/new/ggside.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-097)
- [cloud/ggside/new/ggside.Rcheck/00check.log:1](investigation/evidence.md#evidence-096)
- [saved/tested-sources/ggside/tests/testthat/test_add_gg.R:51](investigation/evidence.md#evidence-150)
- [saved/tested-sources/ggplot2/R/plot-construction.R:70](investigation/evidence.md#evidence-146)
- [saved/evidence/s7-revdep-root-reproductions.txt:1](investigation/evidence.md#evidence-129)

### ggtime 1.0.0

Cloud old check has one unrelated DST test failure and examples pass. New adds an examples ERROR and 14 additional test failures (15 total): mixtime::chronon_format_linear() dispatches on x and cal, with cal = time_calendar(x), but current S7 dispatch sees the omitted defaulted cal as class_missing and no calendar-specific method applies. The same exact old/new defaulted-dispatch behavior was reproduced directly. Thus the new failures are an S7 defaulted-argument dispatch regression exposed through mixtime; the old DST assertion remains inherited.

**Fix:** S7 fix (small to medium, hours): preserve default evaluation for omitted dispatch arguments, then rerun mixtime and ggtime. A mixtime workaround must explicitly supply cal at every omitted-cal call or add suitable class_missing dispatch methods; estimate medium (hours to a day) due many formatter call paths. Keep the pre-existing DST failure separate.

**Owner:** S7 (mixtime workaround available). **Confidence:** high.

**Cloud checks:** old: 1 ERROR; new: 2 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old: examples OK, tests have one pre-existing DST failure. Cloud new: examples ERROR plus 15 test failures; 14 test failures share missing cal and the one DST failure is unchanged. Isolated Rscript with the exact two-dispatch-argument generic/default pattern passed omitted-cal dispatch on old S7 and failed as MISSING on dev; explicit cal works.

**Available source:** Current mixtime clone 0.3.0.9000 retains cal = time_calendar(x) and default call chronon_format_linear(chronon); no workaround is evident.

**Evidence:**

- [cloud/ggtime/old/ggtime.Rcheck/00check.log:52](investigation/evidence.md#evidence-099)
- [cloud/ggtime/old/ggtime.Rcheck/00check.log:80](investigation/evidence.md#evidence-099)
- [cloud/ggtime/new/ggtime.Rcheck/00check.log:52](investigation/evidence.md#evidence-098)
- [cloud/ggtime/new/ggtime.Rcheck/00check.log:83](investigation/evidence.md#evidence-098)
- [clone/mixtime/R/01_chronon_format.R:26](investigation/evidence.md#evidence-031)
- [clone/mixtime/R/format.R:63](investigation/evidence.md#evidence-032)
- [S7/src/method-dispatch.c:217](investigation/evidence.md#evidence-155)

### GitAI 0.1.3

Cloud old check passes; cloud new has four set_llm test errors because its ChatMocked subclass reaches ellmer initialization without the required model. The later local run also has PAT-dependent failures in both versions; those are not inherited failures in the cloud report.

**Fix:** Supply the required model in ChatMocked initialization or update the mock to current ellmer constructor requirements; PAT failures are inherited environment setup. Small test-only patch, 1–2 hours.

**Owner:** GitAI tests/ellmer API. **Confidence:** high for failure site; medium for full constructor interaction.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old Status OK; cloud new has four failed tests. Source defines the affected ChatMocked test subclass. Exact mock construction across both versions has not been isolated beyond the check/source evidence.

**Available source:** Fetched current GitAI HEAD still has ChatMocked$new(provider, echo) without model; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:1283](investigation/evidence.md#evidence-154)
- [cloud/GitAI/new/GitAI.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-067)
- [cloud/GitAI/new/GitAI.Rcheck/00check.log:1](investigation/evidence.md#evidence-066)
- [saved/tested-sources/GitAI/tests/testthat/setup.R:4](investigation/evidence.md#evidence-132)

### iAR 1.3.4

Confirmed new S7 dispatch behavior breaks gentime(n=...): its custom generic declares x = NULL and registers only a NULL method, but development S7 dispatches an omitted defaulted x as class_missing. Exact old/new isolated public-generic reproduction returns the NULL method under S7 0.2.2 and fails with gentime(MISSING) under 0.2.2.9000; explicitly passing NULL works on both. The package API documents omission as equivalent to NULL. This is a direct S7 regression exposed by iAR, not an unchanged no-default missing-dispatch case.

**Fix:** Small (hours): fix S7 dispatch to evaluate a declared default before dispatch (while preserving true required-missing dispatch); alternatively iAR can add a class_missing method forwarding to the NULL path as a workaround. Verify gentime(), gentime(NULL, ...), and required-missing generics on old/dev S7.

**Owner:** S7 (iAR workaround available). **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs.

**New-run check findings:** examples: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old: examples/tests/vignettes pass. Cloud new: examples and vignette fail at gentime(n=...) while tests pass. Isolated Rscript using the exact custom generic signature and NULL method reproduced old pass/new MISSING failure; f(NULL) passed both.

**Available source:** Current iAR clone is 1.3.4 and retains x=NULL plus only a NULL method. No iAR fix is evident; a minimal workaround is a class_missing method.

**Evidence:**

- [cloud/iAR/old/iAR.Rcheck/00check.log:71](investigation/evidence.md#evidence-101)
- [cloud/iAR/new/iAR.Rcheck/00check.log:81](investigation/evidence.md#evidence-100)
- [clone/iAR/R/09_gentime.R:43](investigation/evidence.md#evidence-021)
- [clone/iAR/R/09_gentime.R:44](investigation/evidence.md#evidence-021)
- [S7/src/method-dispatch.c:217](investigation/evidence.md#evidence-155)
- [S7/NEWS.md:89](investigation/evidence.md#evidence-050)

### imply 0.1.0

New install fails while registering sparseImage with S4: its property uses S7::class_atomic, an S7 class union, and S4 registration traverses that union but reports it unregistered. The source does call S7::S4_register(sparseImage); the concrete missing registration is the union itself. Old CRAN install passed.

**Fix:** Small package migration (hours): register S7::class_atomic with S4 before registering sparseImage, and audit packedImage for the same requirement. S7 now exposes properties as S4 slots and documents explicit union registration; validate fresh-library installation and S4 methods.

**Owner:** imply. **Confidence:** high for missing union registration.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘imply’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Old install passed; new fails during lazy load with class union not registered. The class_atomic property and class registration calls are visible in source.

**Available source:** Fetched current imply HEAD still uses class_atomic and registers only sparseImage/packedImage; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:1380](investigation/evidence.md#evidence-154)
- [cloud/imply/new/imply.Rcheck/00install.out:1](investigation/evidence.md#evidence-103)
- [cloud/imply/new/imply.Rcheck/00check.log:1](investigation/evidence.md#evidence-102)
- [saved/tested-sources/imply/R/sparse.R:32](investigation/evidence.md#evidence-151)
- [saved/tested-sources/imply/R/sparse.R:118](investigation/evidence.md#evidence-151)
- [S7/R/S4.R:20](investigation/evidence.md#evidence-052)

### joinery 1.0.1

New install WARNING is an import collision between broad imports of S7 and data.table, both of which export :=. rlang already excludes :=. Old check is OK.

**Fix:** Exclude := from import(S7), matching the existing import(rlang, except = :=) pattern. Small namespace edit, about 10–20 minutes plus check.

**Owner:** joinery. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘joinery’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current clone retains S7/data.table broad imports; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/joinery/new/joinery.Rcheck/00check.log:23](investigation/evidence.md#evidence-104)
- [clone/joinery/NAMESPACE:93](investigation/evidence.md#evidence-022)
- [clone/joinery/NAMESPACE:94](investigation/evidence.md#evidence-022)
- [clone/joinery/NAMESPACE:96](investigation/evidence.md#evidence-022)

### marquee 1.2.1

New NOTE says S7 is declared in DESCRIPTION Imports but is not imported in NAMESPACE. Code calls S7:: functions explicitly, so this is a dependency declaration/NAMESPACE consistency note, not a runtime failure.

**Fix:** Declare the needed S7 imports in NAMESPACE (or otherwise make the Imports declaration consistent with package policy). Small metadata edit, about 10–20 minutes plus check.

**Owner:** marquee. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 NOTE.

**New-run check findings:** dependencies in R code: NOTE. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one NOTE. Current source still declares S7 in Imports and has no S7 NAMESPACE import; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/marquee/new/marquee.Rcheck/00check.log:46](investigation/evidence.md#evidence-105)
- [clone/marquee/DESCRIPTION:19](investigation/evidence.md#evidence-023)
- [clone/marquee/NAMESPACE:63](investigation/evidence.md#evidence-024)
- [clone/marquee/R/element_marquee.R:240](investigation/evidence.md#evidence-025)

### measr 2.0.1

New codoc WARNING: generated measrdcm() code has model_spec = dcmstan::dcm_specification(), while Rd says NULL. Source declares a typed property and default = NULL; under S7, NULL means use the property's class constructor default, so the generated formal matches current S7 behavior. This is a downstream documentation/default-intent mismatch.

**Fix:** If the class constructor default is intended, update roxygen/Rd to dcmstan::dcm_specification() and re-document. If literal NULL is intended, use default = quote(NULL) and check the constructor behavior. Small docs/code fix, 15–30 minutes.

**Owner:** measr. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** for code/documentation mismatches: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one codoc WARNING. Current clone retains Rd NULL with the same S7 class default; no rerun.

**Available source:** Not fixed in current clone; current dev code and docs preserve the mismatch.

**Evidence:**

- [cloud/measr/new/measr.Rcheck/00check.log:55](investigation/evidence.md#evidence-106)
- [clone/measr/R/zzz-class-dcm-estimate.R:454](investigation/evidence.md#evidence-026)
- [clone/measr/R/zzz-class-dcm-estimate.R:456](investigation/evidence.md#evidence-026)
- [clone/measr/man/measrdcm.Rd:8](investigation/evidence.md#evidence-027)
- [S7/R/property.R:218](investigation/evidence.md#evidence-060)

### medfit 0.3.2

Cloud old check passed; new install fails because an S7 union (class_integer | class_double | NULL) used by an S4-facing class is not registered with S4. Registering the enclosing classes does not register the union type itself.

**Fix:** Small to medium (hours): identify S4-facing union property types and register each union with S4 before methods_register(), or replace those slots with an S4-compatible type. Validate install from a fresh library.

**Owner:** medfit. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘medfit’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR.

**Available source:** Current clone is 0.5.0 and still defines nullable union properties in classes registered with S4; its .onLoad registers classes but no union, so a fix is not evident. Not reinstalled.

**Evidence:**

- [cloud/medfit/new/medfit.Rcheck/00install.out:6](investigation/evidence.md#evidence-107)
- [saved/tested-sources/medfit/R/classes.R:96](investigation/evidence.md#evidence-152)
- [saved/tested-sources/medfit/R/classes.R:201](investigation/evidence.md#evidence-152)
- [clone/medfit/R/zzz.R:39](investigation/evidence.md#evidence-028)

### mighty.metadata 0.1.0

Cloud old check passed; new examples, tests, and vignette rebuilding fail because S7schema's internal constructors call new_object(.parent=...), but this S7 version renamed the first argument to _parent. mighty.metadata also uses .parent in its own constructors, so fixing only S7schema may expose the same error downstream. list_columns(<NULL>) failures are cascades from failed object creation.

**Fix:** Small (hours): migrate .parent to _parent throughout S7schema and mighty.metadata, then rerun both packages. S7's NEWS documents this intentional argument rename.

**Owner:** S7schema and mighty.metadata. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories (examples, tests, vignette rebuilding).

**Available source:** mighty.metadata clone is 0.1.0.9015 and still contains .parent calls; S7schema clone is 0.1.2 and still contains .parent calls. No migration is evident.

**Evidence:**

- [cloud/mighty.metadata/new/mighty.metadata.Rcheck/00check.log:70](investigation/evidence.md#evidence-108)
- [clone/S7schema/R/validator.R:23](investigation/evidence.md#evidence-002)
- [clone/mighty.metadata/R/mighty_domain.R:63](investigation/evidence.md#evidence-029)
- [S7/NEWS.md:29](investigation/evidence.md#evidence-050)

### mixtime 0.3.0

Cloud old check passed; new install fails while mixtime subclasses vecvec::class_vecvec. The class object has internal name vecvec but is exported as class_vecvec; S7's external-class resolution looks for vecvec and finds the constructor function instead. This is the same exported-alias pattern as ggplotplus. This is not the #364 temporary external-generic placeholder issue: neither package declares new_external_generic for the failing class construction, and ggplotplus has no S7_on_build() call; mixtime has external generics but class construction fails first.

**Fix:** Small (hours): refer to vecvec's exported class binding explicitly with an external-class reference to class_vecvec, or adjust vecvec's exported class identity contract; verify downstream ggtime after installation.

**Owner:** S7 (package workaround available). **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘mixtime’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR. The reported terminal error is external class name/export resolution, not a generic-placeholder sentinel; S7_on_build() would not address this first failure. Public package-boundary repro with ggplot2 class_ggplot passes on S7 0.2.2, fails on 0.2.2.9000, and an explicit exported-alias reference passes on dev.

**Available source:** mixtime clone 0.3.0.9000 still parents from class_vecvec; vecvec clone exports vecvec as a function and class_vecvec as the S7 class binding.

**Evidence:**

- [cloud/mixtime/new/mixtime.Rcheck/00install.out:18](investigation/evidence.md#evidence-109)
- [clone/mixtime/R/00_classes.R:198](investigation/evidence.md#evidence-030)
- [clone/vecvec/R/00_S7_classes.R:34](investigation/evidence.md#evidence-045)
- [clone/vecvec/NAMESPACE:25](investigation/evidence.md#evidence-044)
- [S7/R/external-class.R:156](investigation/evidence.md#evidence-056)
- [S7/NEWS.md:24](investigation/evidence.md#evidence-050)

### myTAI 2.3.7

New package installation fails through ggforce .onLoad() while updating GeomArc0 default aesthetics. The mapping stored in the installed ggforce lazy-load database is a legacy class-vector-only ggplot2 mapping; development S7 rejects it in S7_data(), while old S7 accepts that representation.

**Fix:** Address legacy serialized-object compatibility in S7, using the installed ggforce mapping as the public regression case. Rebuilding ggforce is a workaround to verify, not a root repair. Focused compatibility work (hours), followed by ggdiagram, myTAI, and anansi installation checks.

**Owner:** S7 legacy-object compatibility. **Confidence:** high for shared failure; compatibility repair requires actual saved-object regression.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘myTAI’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR. Error call belongs to ggforce .onLoad.

**Available source:** myTAI clone is 2.3.7; no package-side fix is indicated. ggforce source was not among the assigned clones.

**Evidence:**

- [cloud/myTAI/new/myTAI.Rcheck/00install.out:30](investigation/evidence.md#evidence-110)
- [clone/myTAI/DESCRIPTION:16](investigation/evidence.md#evidence-033)
- [S7/revdep/problems.md:1768](investigation/evidence.md#evidence-154)
- [all-classes.R:282](investigation/evidence.md#evidence-159)
- [geom-update-defaults.R:142](investigation/evidence.md#evidence-160)
- [geom-update-defaults.R:150](investigation/evidence.md#evidence-160)

### nflplotR 1.7.0

Cloud old check passed; new examples fail through ggpath::element_path(), whose custom constructor passes S7_object() despite element_text being its parent. This is inherited from ggpath. Separately, the code/documentation WARNING says generated element_nfl_* constructors take ... while their docs list element_path arguments.

**Fix:** Medium (one to several days): fix ggpath's element_path constructor/type contract first; nflplotR should then rerun examples. Small follow-up: update element_nfl_* usage docs or provide matching constructors to clear the codoc warning.

**Owner:** ggpath for example error; nflplotR for docs warning. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR, 1 WARNING.

**New-run check findings:** for code/documentation mismatches: WARNING; examples: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 example ERROR and 1 codoc WARNING.

**Available source:** nflplotR clone remains 1.7.0 with inherited ggpath use and stale docs; no package-side error fix is evident.

**Evidence:**

- [cloud/nflplotR/new/nflplotR.Rcheck/00check.log:118](investigation/evidence.md#evidence-111)
- [cloud/nflplotR/new/nflplotR.Rcheck/00check.log:48](investigation/evidence.md#evidence-111)
- [clone/ggpath/R/theme_element.R:116](investigation/evidence.md#evidence-019)
- [clone/nflplotR/R/theme-elements.R:130](investigation/evidence.md#evidence-034)

### PFIM 8.0

Cloud old check passed; new install fails because S4-registered PFIM classes use a union property type NULL | Fim without registering that union with S4. The diagnostic names new_union(NULL, PFIM::Fim). There is also a nonfatal warning that list() should be quoted as a property default.

**Fix:** Small to medium (hours): register the union type used by the S4-facing properties before S4 method registration (or choose an S4-compatible property type), then quote list() defaults and rerun install/check.

**Owner:** PFIM. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** whether package ‘PFIM’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 1 install ERROR. Install output also contains the list-default warning.

**Available source:** The 8.0 clone still uses NULL | Fim in properties and replays S4 class registration without registering that union; no fix is evident.

**Evidence:**

- [cloud/PFIM/new/PFIM.Rcheck/00install.out:254](investigation/evidence.md#evidence-068)
- [saved/tested-sources/PFIM/R/Arm.R:49](investigation/evidence.md#evidence-133)
- [saved/tested-sources/PFIM/R/Design.R:31](investigation/evidence.md#evidence-134)
- [saved/tested-sources/PFIM/R/zzz.R:23](investigation/evidence.md#evidence-135)

### querychat 0.4.1

All six new check categories (2 WARNINGs, 4 NOTEs) repeat the same namespace warning while loading ellmer: S7:::= is replaced by rlang:::=. They are cascade diagnostics, not six independent querychat defects.

**Fix:** Fix ellmer's S7/rlang import collision; no querychat source change is indicated by these logs. This is covered by the small ellmer namespace patch.

**Owner:** ellmer (dependency). **Confidence:** high.

**Cloud checks:** old: OK; new: 2 WARNINGs, 4 NOTEs.

**New-run check findings:** dependencies in R code: NOTE; S3 generic/method consistency: WARNING; foreign function calls: NOTE; R code for possible problems: NOTE; for code/documentation mismatches: WARNING; Rd \usage sections: NOTE. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new log shows the same ellmer warning under dependencies, S3 consistency, foreign calls, possible problems, codoc and Rd usage. Current ellmer source still imports both namespaces.

**Available source:** Await ellmer namespace fix; querychat itself has no demonstrated issue.

**Evidence:**

- [cloud/querychat/new/querychat.Rcheck/00check.log:39](investigation/evidence.md#evidence-112)
- [cloud/querychat/new/querychat.Rcheck/00check.log:41](investigation/evidence.md#evidence-112)
- [cloud/querychat/new/querychat.Rcheck/00check.log:46](investigation/evidence.md#evidence-112)
- [cloud/querychat/new/querychat.Rcheck/00check.log:50](investigation/evidence.md#evidence-112)
- [cloud/querychat/new/querychat.Rcheck/00check.log:56](investigation/evidence.md#evidence-112)
- [cloud/querychat/new/querychat.Rcheck/00check.log:58](investigation/evidence.md#evidence-112)
- [clone/ellmer/NAMESPACE:143](investigation/evidence.md#evidence-010)
- [clone/ellmer/NAMESPACE:145](investigation/evidence.md#evidence-010)

### rtemis 1.2.7

New install ERROR is a class property type violation: tested StratSubConfig declares @n as integer or NULL while parent ResamplerConfig@n is integer, so the child widens rather than narrows the parent contract. Old run instead failed one DBSCAN test because dbscan rejected approx; the report marks that old test failure newly fixed, while installation is newly broken.

**Fix:** At the 1.2.7 source, align the parent/child property types: decide whether n is nullable and declare that consistently, or remove NULL from the child. Add/retain a class-construction check. Likely a small schema change (30–90 minutes). Current clone has moved to a different resampler schema, so no equivalent patch is obvious there.

**Owner:** rtemis. **Confidence:** high for tested-version cause; medium for current-version status.

**Cloud checks:** old: 1 ERROR; new: 1 ERROR.

**New-run check findings:** whether package ‘rtemis’ can be installed: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old/new logs distinguish old test ERROR from new install ERROR. Current GitHub clone is substantially newer (1.4.2 series) and defines n_resamples rather than the reported n property; no check rerun.

**Available source:** Likely resolved by later schema redesign in current clone; verify with a check before treating as confirmed.

**Evidence:**

- [cloud/rtemis/new/rtemis.Rcheck/00install.out:12](investigation/evidence.md#evidence-116)
- [cloud/rtemis/old/rtemis.Rcheck/00check.log:58](investigation/evidence.md#evidence-117)
- [S7/revdep/problems.md:1997](investigation/evidence.md#evidence-154)
- [clone/rtemis/R/100_Resampler.R:193](investigation/evidence.md#evidence-038)
- [clone/rtemis/R/100_Resampler.R:196](investigation/evidence.md#evidence-038)
- [clone/rtemis/DESCRIPTION:3](investigation/evidence.md#evidence-037)

### rtemis.a3 0.5.3

New examples and tests fail because the custom A3Metadata constructor calls new_object(Metadata, ...), passing the S7 class object as _parent instead of an instance. The new public contract requires an instance of the declared parent. A separate install WARNING is the S7/data.table := import collision.

**Fix:** Pass Metadata() as _parent (or use the default parent construction) and exclude := from the broad S7 import. One-line constructor correction plus a public constructor test and small namespace edit; about 30–60 minutes.

**Owner:** rtemis.a3. **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs, 1 WARNING.

**New-run check findings:** whether package ‘rtemis.a3’ can be installed: WARNING; examples: ERROR; tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check has an install WARNING, example ERROR, and test ERROR. Current clone still passes the class object to new_object; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/rtemis.a3/new/rtemis.a3.Rcheck/00check.log:22](investigation/evidence.md#evidence-113)
- [cloud/rtemis.a3/new/rtemis.a3.Rcheck/00check.log:55](investigation/evidence.md#evidence-113)
- [clone/rtemis.a3/R/R/a3.R:338](investigation/evidence.md#evidence-035)
- [clone/rtemis.a3/R/R/a3.R:353](investigation/evidence.md#evidence-035)
- [clone/rtemis.a3/R/R/a3.R:354](investigation/evidence.md#evidence-035)
- [S7/R/class.R:50](investigation/evidence.md#evidence-053)
- [S7/R/class.R:364](investigation/evidence.md#evidence-053)

### rtemis.core 0.4.6

New install WARNING is the S7/data.table := collision from broad imports. No other new category is reported for this package.

**Fix:** Exclude := from import(S7) (data.table remains the intended binding). Small namespace edit, about 10–20 minutes plus check.

**Owner:** rtemis.core. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘rtemis.core’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current clone retains both broad imports; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/rtemis.core/new/rtemis.core.Rcheck/00check.log:22](investigation/evidence.md#evidence-114)
- [clone/rtemis.core/NAMESPACE:161](investigation/evidence.md#evidence-036)
- [clone/rtemis.core/NAMESPACE:162](investigation/evidence.md#evidence-036)

### rtemis.llm 0.8.7

New install WARNING is the S7/data.table := collision reported while loading rtemis.core. It is inherited through the rtemis.core dependency; this log shows no separate runtime failure.

**Fix:** Exclude := from rtemis.core's broad S7 import; no independent rtemis.llm fix is indicated. Small dependency namespace patch.

**Owner:** rtemis.core (dependency). **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘rtemis.llm’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING through rtemis.core. No rerun.

**Available source:** Await rtemis.core namespace fix; no independent llm source defect found.

**Evidence:**

- [cloud/rtemis.llm/new/rtemis.llm.Rcheck/00check.log:22](investigation/evidence.md#evidence-115)
- [clone/rtemis.core/NAMESPACE:161](investigation/evidence.md#evidence-036)
- [clone/rtemis.core/NAMESPACE:162](investigation/evidence.md#evidence-036)

### S7schema 0.1.2

Cloud old check passed; new examples, tests, and vignette rebuilding fail at construct_validator() because it calls S7::new_object(.parent=...). The current argument is _parent. The write_config(<NULL>) test error follows failed construction and is downstream.

**Fix:** Small (hours): change all S7schema new_object(.parent=...) calls to _parent and rerun its public examples/tests; then verify mighty.metadata callers.

**Owner:** S7schema. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories (examples, tests, vignette rebuilding).

**Available source:** The available 0.1.2 clone retains .parent calls; no fix is evident.

**Evidence:**

- [cloud/S7schema/new/S7schema.Rcheck/00check.log:73](investigation/evidence.md#evidence-069)
- [clone/S7schema/R/validator.R:23](investigation/evidence.md#evidence-002)
- [clone/S7schema/R/y_S7schema.R:104](investigation/evidence.md#evidence-003)
- [S7/NEWS.md:29](investigation/evidence.md#evidence-050)

### shinychat 0.5.0

New codoc WARNING: generated ContentSlashCommand formals are (..., command, user_text), while its Rd topic documents text, command, and user_text. This matches S7's forwarding-constructor shape for a subclass of ellmer::ContentText; parent arguments are accepted through ... .

**Fix:** Document ... as forwarded to ContentText and re-document. Small docs-only correction, about 15–30 minutes.

**Owner:** shinychat. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** for code/documentation mismatches: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports only one codoc WARNING. Current clone still has the stale Rd usage; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/shinychat/new/shinychat.Rcheck/00check.log:50](investigation/evidence.md#evidence-120)
- [clone/shinychat/pkg-r/R/content-slash-command.R:59](investigation/evidence.md#evidence-040)
- [clone/shinychat/pkg-r/R/content-slash-command.R:61](investigation/evidence.md#evidence-040)
- [clone/shinychat/pkg-r/man/ContentSlashCommand.Rd:7](investigation/evidence.md#evidence-041)
- [S7/R/constructor.R:166](investigation/evidence.md#evidence-054)

### shinyCohortBuilder 1.0.0

Cloud old check passed; new examples, tests, and vignette rebuilding fail while shinyCohortBuilder calls cohortBuilder::filter(). The underlying failure is cohortBuilder's invalid CbFilter parent construction, so this is inherited rather than a direct shinyCohortBuilder defect.

**Fix:** Small for shinyCohortBuilder: no direct change is indicated by the trace. Fix cohortBuilder's parent constructors, then rerun these dependent examples/tests; add a compatibility update only if failures remain.

**Owner:** cohortBuilder. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs.

**New-run check findings:** examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories (examples, tests, vignette rebuilding). The stack enters cohortBuilder::filter().

**Available source:** The available clone remains 1.0.0; no shinyCohortBuilder-specific change is indicated by the failing stack.

**Evidence:**

- [cloud/shinyCohortBuilder/new/shinyCohortBuilder.Rcheck/00check.log:89](investigation/evidence.md#evidence-118)
- [S7/revdep/problems.md:2346](investigation/evidence.md#evidence-154)
- [clone/cohortBuilder/R/filter.R:125](investigation/evidence.md#evidence-006)

### shinyfilters 0.3.1

Old tests passed; new has 14 list-input errors and vignette failure. All point to an error-message path dereferencing cls@name when cls is now an S7_base_class, which has no @ method.

**Fix:** Replace cls@name with S7::S7_class_desc(cls) in the shared diagnostic helper. Small package patch, about 1 hour.

**Owner:** shinyfilters. **Confidence:** high.

**Cloud checks:** old: OK; new: 2 ERRORs.

**New-run check findings:** tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old tests passed (320); cloud new has 14 errors with cls@name and the vignette reaches the same helper. Local checks.noindex omitted shinyfilters.

**Available source:** Fetched current shinyfilters HEAD still contains cls@name; no fix visible.

**Evidence:**

- [S7/revdep/problems.md:2412](investigation/evidence.md#evidence-154)
- [cloud/shinyfilters/new/shinyfilters.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-122)
- [cloud/shinyfilters/new/shinyfilters.Rcheck/00check.log:1](investigation/evidence.md#evidence-121)
- [clone/shinyfilters/R/utils.R:42](investigation/evidence.md#evidence-042)

### shinyOAuth 0.6.1

New install WARNING is the S7/rlang := import collision; old check is OK.

**Fix:** Exclude := from shinyOAuth's broad S7 import or use explicit imports. Small namespace edit, about 10–20 minutes plus check.

**Owner:** shinyOAuth. **Confidence:** high.

**Cloud checks:** old: OK; new: 1 WARNING.

**New-run check findings:** whether package ‘shinyOAuth’ can be installed: WARNING. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check reports one install WARNING. Current clone still broadly imports S7; no rerun.

**Available source:** Not fixed in current clone.

**Evidence:**

- [cloud/shinyOAuth/new/shinyOAuth.Rcheck/00check.log:22](investigation/evidence.md#evidence-119)
- [clone/shinyOAuth/NAMESPACE:67](investigation/evidence.md#evidence-039)
- [S7/revdep/library.noindex/S7/new/S7/NAMESPACE:44](investigation/evidence.md#evidence-153)

### statim 0.1.0

Cloud old check passed; new examples, tests, and vignette rebuilding fail because lm_to_lm_object() passes family="gaussian" to class_lm_object(), whose properties do not include family and whose constructor rejects the argument. This is the same root cause as the package-install WARNING and code NOTE; it is a statim caller/class mismatch, not a separate failure.

**Fix:** Small (hours): remove the unsupported family argument if unused, or add a family property and validate/store it if behavior depends on it; check all lm conversion paths.

**Owner:** statim. **Confidence:** high.

**Cloud checks:** old: OK; new: 3 ERRORs, 1 WARNING, 1 NOTE.

**New-run check findings:** whether package ‘statim’ can be installed: WARNING; R code for possible problems: NOTE; examples: ERROR; tests: ERROR; re-building of vignette outputs: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Cloud full check: old Status OK; new has 3 ERROR categories, 1 install WARNING, and 1 NOTE, all traced to the unsupported family argument.

**Available source:** Current clone is 0.1.0.9000 and still passes family="gaussian" at the conversion call; no fix is evident.

**Evidence:**

- [cloud/statim/new/statim.Rcheck/00check.log:68](investigation/evidence.md#evidence-123)
- [clone/statim/R/model-linear-reg.R:148](investigation/evidence.md#evidence-043)
- [clone/statim/R/model-linear-reg.R:356](investigation/evidence.md#evidence-043)
- [S7/revdep/problems.md:2591](investigation/evidence.md#evidence-154)

### tidyllm 0.7.0

All six new check categories (2 WARNINGs, 4 NOTEs) repeat the same := collision while loading ellmer. They are dependency-propagated diagnostics, not six independent tidyllm defects.

**Fix:** Fix ellmer's broad S7/rlang import collision; no tidyllm source change is indicated by these logs. This is the same small ellmer patch needed for querychat.

**Owner:** ellmer (dependency). **Confidence:** high.

**Cloud checks:** old: OK; new: 2 WARNINGs, 4 NOTEs.

**New-run check findings:** dependencies in R code: NOTE; S3 generic/method consistency: WARNING; foreign function calls: NOTE; R code for possible problems: NOTE; for code/documentation mismatches: WARNING; Rd \usage sections: NOTE. These include unchanged findings where the old check also reports them.

**Validation:** Cloud old check OK; new check shows the same ellmer warning across dependency/S3/Rd checks. Current ellmer source still has broad imports; no rerun.

**Available source:** Await ellmer namespace fix; no independent tidyllm defect found.

**Evidence:**

- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:40](investigation/evidence.md#evidence-124)
- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:42](investigation/evidence.md#evidence-124)
- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:47](investigation/evidence.md#evidence-124)
- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:51](investigation/evidence.md#evidence-124)
- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:57](investigation/evidence.md#evidence-124)
- [cloud/tidyllm/new/tidyllm.Rcheck/00check.log:59](investigation/evidence.md#evidence-124)
- [clone/ellmer/NAMESPACE:143](investigation/evidence.md#evidence-010)
- [clone/ellmer/NAMESPACE:145](investigation/evidence.md#evidence-010)

### typedjson 0.1.1

Cloud old tests pass; new tested typedjson 0.1.1 run has 29 failures around S7 class/object serialization. Exact tested source is now available from the CRAN mirror tag 0.1.1. With that exact source and an unscoped class, JSON round-trip is identical under old S7 but not new, although all.equal() and S7_data() agree. The new serialized graph includes .S7_class_ref inside the constructor closure environment, pointing back to the class, so the decoder reconstructs a different class identity.

**Fix:** Preserve the shared class-reference, constructor-environment, and class identity during decode, or rebuild through a public S7 class API that rebinds the reference. Validate the full 0.1.1 test-objects.R suite. Likely 1–3 days.

**Owner:** typedjson/S7 interface; typedjson serialization most likely. **Confidence:** high for the focused tested-version reproduction; medium for broader 29-failure scope.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Fetched CRAN mirror tag 0.1.1 into a separate local tag ref, archived to /tmp, and installed in isolated old/new libraries. Foo <- new_class("Foo", properties=list(x=class_double)); json_write_str/json_read_str is identical old and not new, all.equal and S7_data agree; new object stores _S7_class as an environment and serialized JSON contains .S7_class_ref. Exact 0.1.1 source confirmed; the focused roundtrip was not the full package suite.

**Available source:** Exact tested typedjson 0.1.1 source retrieved from CRAN mirror tag; current clone remains 0.1.2. No fix assessed.

**Evidence:**

- [S7/revdep/problems.md:2659](investigation/evidence.md#evidence-154)
- [cloud/typedjson/new/typedjson.Rcheck/tests/testthat.Rout.fail:1](investigation/evidence.md#evidence-125)
- [saved/evidence/s7.R:72](investigation/evidence.md#evidence-130)
- [saved/evidence/environments.R:136](investigation/evidence.md#evidence-127)
- [saved/evidence/test-objects.R:34](investigation/evidence.md#evidence-131)
- [S7/R/class.R:190](investigation/evidence.md#evidence-053)

### vecvec 1.3.0

Cloud old tests pass and new tested 1.3.0 run has 12 new failures: five array [<- operations lose dim and seven is.na/vctrs callback paths error because .f is not found. Exact v1.3.0 tag source installed in isolated old/new libraries reproduces dim retention old and dim=NULL new. `is.na(x)` reproduces `.f` lookup failure on the new S7 build, while direct `vecvec_apply(x, is.na)` succeeds; local old build also fails the nested lookup on R 4.6.1, so that subfailure is cloud-confirmed but not locally differential. The likely promise-context mechanism is S7 `@` invoking the C property getter before base::lapply resolves its symbolic FUN; forcing/matching `.f` before evaluating `x@x` is the focused package-side validation/fix candidate.

**Fix:** For dimensions, preserve the dim attribute across the S7 `[<-`/`@<-` update path and add the five observed array assignment cases. For callback lookup, eagerly resolve `.f` within vecvec_apply before the S7-backed x@x property access (for example match.fun(.f)), then run is.na and vctrs public tests. Moderate package work, roughly 0.5–2 days; investigate S7 call-stack semantics only if forcing .f does not restore the public contract.

**Owner:** vecvec adaptation likely; S7 property/call semantics if callback forcing cannot preserve contract. **Confidence:** high for dim behavior and cloud failure grouping; medium for callback mechanism.

**Cloud checks:** old: OK; new: 1 ERROR.

**New-run check findings:** tests: ERROR. These include unchanged findings where the old check also reports them.

**Validation:** Installed exact vecvec 1.3.0 source from its v1.3.0 git tag in isolated libraries. On R 4.6.1, x <- array(vecvec(1:6), dim=c(2,3)); x[1,1] <- 99L retains dim old and drops it new. is.na(x) fails `.f` lookup new, but also fails locally old on this R; direct vecvec_apply(x,is.na) succeeds both. Cloud R check independently confirms the seven callback errors are new-only. Current clone v1.3.0.9000 was not used for this claim.

**Available source:** Exact tested tag v1.3.0 inspected and installed; current clone is ahead at 1.3.0.9000. No fix assessed.

**Evidence:**

- [S7/revdep/problems.md:2699](investigation/evidence.md#evidence-154)
- [cloud/vecvec/new/vecvec.Rcheck/tests/testthat.Rout.fail:57](investigation/evidence.md#evidence-126)
- [clone/vecvec/R/apply.R:15](investigation/evidence.md#evidence-046)
- [clone/vecvec/R/predicates.R:12](investigation/evidence.md#evidence-047)
- [clone/vecvec/R/replacement.R:1](investigation/evidence.md#evidence-048)
- [clone/vecvec/R/vecvec.R:287](investigation/evidence.md#evidence-049)
- [S7/src/prop.c:522](investigation/evidence.md#evidence-156)
