# Reverse dependency results

**Do not merge.** This branch shares the reverse dependency reports and the investigation into their causes. It contains no S7 implementation changes.

Start with the [investigation](investigation.md), which covers all 48 packages flagged by the original cloud reports: 37 with new problems and 11 that failed to check. It includes proposed repairs, confidence levels, evidence, and unresolved questions.

The reports below are from cloud run `9d2b3da3-5ec9-42ac-a8c0-487f25463f7a`, comparing S7 0.2.2 with development S7 0.2.2.9000 across 121 reverse dependencies. The [CRAN summary](cran.md) excludes the Bioconductor package anansi from its failed-to-check count.

The [later local reports](local-results/README.md) cover 122 reverse dependencies and include additional environmental failures. They are retained separately and should not be treated as the same run or as validation of proposed fixes. Their generated CRAN summary likewise excludes Bioconductor packages.

The original cloud preparation failure for anansi has empty logs and remains unexplained. Its later local failure through ggforce is a separate observation. Parts of the covr, typedjson, and vecvec investigations also remain unresolved; see the per-package validation limits.

## Original cloud results

# Revdeps

## Failed to check (11)

|package    |version |error     |warning |note |
|:----------|:-------|:---------|:-------|:----|
|anansi     |?       |          |        |     |
|[bidsr](failures.md#bidsr)|0.1.1   |__+1__    |        |     |
|[ggdiagram](failures.md#ggdiagram)|0.2.0   |__+1__    |        |     |
|[ggpath](failures.md#ggpath)|1.1.1   |__+1__    |        |     |
|[ggplotplus](failures.md#ggplotplus)|0.5.7   |__+1__    |        |     |
|[imply](failures.md#imply)|0.1.0   |__+1__    |        |     |
|[medfit](failures.md#medfit)|0.3.2   |__+1__    |        |     |
|[mixtime](failures.md#mixtime)|0.3.0   |__+1__    |        |     |
|[myTAI](failures.md#mytai)|2.3.7   |__+1__    |        |     |
|[PFIM](failures.md#pfim)|8.0     |__+1__    |        |     |
|[rtemis](failures.md#rtemis)|1.2.7   |-1 __+1__ |        |     |

## New problems (37)

|package            |version |error    |warning |note   |
|:------------------|:-------|:--------|:-------|:------|
|[ale](problems.md#ale)|0.5.3   |         |__+1__  |       |
|[apa7](problems.md#apa7)|0.1.3   |         |__+1__  |       |
|[btw](problems.md#btw)|1.5.0   |__+2__   |        |__+1__ |
|[caugi](problems.md#caugi)|1.3.0   |__+2__   |        |       |
|[cohortBuilder](problems.md#cohortbuilder)|1.0.0   |__+3__   |        |       |
|[covr](problems.md#covr)|3.6.5   |__+1__   |        |       |
|[dcmstan](problems.md#dcmstan)|0.1.0   |__+3__   |        |       |
|[deltapif](problems.md#deltapif)|0.4.5   |__+3__   |        |__+1__ |
|[ellmer](problems.md#ellmer)|0.5.0   |1        |__+2__  |       |
|[filtro](problems.md#filtro)|0.2.0   |__+2__   |        |       |
|[fr](problems.md#fr)|0.5.2   |__+3__   |        |1      |
|[GGally](problems.md#ggally)|2.4.0   |         |__+1__  |       |
|[ggarrow](problems.md#ggarrow)|0.2.0   |__+2__   |        |       |
|[gglogger](problems.md#gglogger)|0.1.8   |__+1__   |        |       |
|[ggplot2](problems.md#ggplot2)|4.0.3   |__+1__   |        |       |
|[ggside](problems.md#ggside)|0.4.1   |__+1__   |        |       |
|[ggtime](problems.md#ggtime)|1.0.0   |1 __+1__ |        |       |
|[GitAI](problems.md#gitai)|0.1.3   |__+1__   |        |       |
|[iAR](problems.md#iar)|1.3.4   |__+2__   |        |       |
|[joinery](problems.md#joinery)|1.0.1   |         |__+1__  |       |
|[marquee](problems.md#marquee)|1.2.1   |         |        |__+1__ |
|[measr](problems.md#measr)|2.0.1   |         |__+1__  |       |
|[mighty.metadata](problems.md#mightymetadata)|0.1.0   |__+3__   |        |       |
|[nflplotR](problems.md#nflplotr)|1.7.0   |__+1__   |__+1__  |       |
|[querychat](problems.md#querychat)|0.4.1   |         |__+2__  |__+4__ |
|[rtemis.a3](problems.md#rtemisa3)|0.5.3   |__+2__   |__+1__  |       |
|[rtemis.core](problems.md#rtemiscore)|0.4.6   |         |__+1__  |       |
|[rtemis.llm](problems.md#rtemisllm)|0.8.7   |         |__+1__  |       |
|[S7schema](problems.md#s7schema)|0.1.2   |__+3__   |        |       |
|[shinychat](problems.md#shinychat)|0.5.0   |         |__+1__  |       |
|[shinyCohortBuilder](problems.md#shinycohortbuilder)|1.0.0   |__+3__   |        |       |
|[shinyfilters](problems.md#shinyfilters)|0.3.1   |__+2__   |        |       |
|[shinyOAuth](problems.md#shinyoauth)|0.6.1   |         |__+1__  |       |
|[statim](problems.md#statim)|0.1.0   |__+3__   |__+1__  |__+1__ |
|[tidyllm](problems.md#tidyllm)|0.7.0   |         |__+2__  |__+4__ |
|[typedjson](problems.md#typedjson)|0.1.1   |__+1__   |        |       |
|[vecvec](problems.md#vecvec)|1.3.0   |__+1__   |        |       |

