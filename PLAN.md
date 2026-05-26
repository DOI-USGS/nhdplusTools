# hydrogeofetch migration plan

nhdplusTools 1.5.0 is the last feature release under the old name. This plan covers the rename to hydrogeofetch, the deprecation shim, and the eventual CRAN archive of nhdplusTools. Each milestone ends with a gate — review or communication work that should finish before moving on.


## 1. Close out nhdplusTools 1.5.0 ✓

*Complete.* nhdplusTools v1.5.0 released on CRAN, tagged and released on both GitHub and GitLab (May 2026). pkgdown site live. README points to the rename plan.


## 2. Build hydrogeofetch v1.0

Rename all internal references so the package installs and loads as `hydrogeofetch`. Function names and signatures stay identical except where the package name is embedded in the function name itself (`nhdplusTools_data_dir`, `nhdplusTools_cache_settings`). New hex sticker is already in place.

### 2a. Package identity

- [ ] DESCRIPTION: `Package:`, `Title:`, `Description:` fields (leave `URL:` and `BugReports:` pointing at current repo until milestone 3)
- [ ] NEWS.md: add hydrogeofetch 1.0.0 header; keep nhdplusTools history below it
- [ ] README.Rmd: update package name in title, heading, and narrative text. Leave badge URLs, `install.packages()`, and `remotes::install_github()` pointing at nhdplusTools/current repo — those update after the repo rename (milestone 3) and CRAN acceptance (milestone 6). Re-knit README.md.

Deferred to later milestones:
- inst/CITATION: update after DOI is minted (milestone 4)
- code.json: update after USGS software release process begins (milestone 4)
- _pkgdown.yml `url:`: update after repo rename (milestone 6)
- DESCRIPTION `URL:`/`BugReports:`: update after repo rename (milestone 6)
- README badge URLs and install instructions: update after repo rename (milestone 6) and CRAN acceptance (milestone 5)

### 2b. R source — package internals

- [ ] Rename `R/A_nhdplusTools.R` to `R/A_hydrogeofetch.R`
- [ ] Internal environment: `nhdplusTools_env` → `hydrogeofetch_env`
- [ ] Debug env var: `debug_nhdplusTools` → `debug_hydrogeofetch`
- [ ] Cache directory: `tools::R_user_dir("nhdplusTools")` → `tools::R_user_dir("hydrogeofetch")`
- [ ] Rename exported functions:
  - `nhdplusTools_data_dir()` → `hydrogeofetch_data_dir()`
  - `nhdplusTools_cache_settings()` → `hydrogeofetch_cache_settings()`

### 2c. Environment variables (user-facing)

`NHDPLUSTOOLS_MEMOISE_CACHE` and `NHDPLUSTOOLS_MEMOISE_TIMEOUT` are set by users in `.Renviron`. Rename to `HYDROGEOFETCH_MEMOISE_CACHE` and `HYDROGEOFETCH_MEMOISE_TIMEOUT`, but check the old names as a fallback with a one-time `message()` telling the user to update. Remove the fallback in v2.0.

### 2d. Mechanical find-replace across R/, tests/, vignettes/, inst/extdata/

These are high-count, low-risk changes. Do them after 2b so the function renames are in place.

- [ ] `system.file(..., package = "nhdplusTools")` → `package = "hydrogeofetch"` (~87 occurrences)
- [ ] `library(nhdplusTools)` → `library(hydrogeofetch)`
- [ ] `nhdplusTools::` → `hydrogeofetch::` (qualified calls in examples and extdata scripts)
- [ ] Roxygen prose referencing "nhdplusTools" by name
- [ ] Deprecation warning strings (e.g., `R/rebuild_topology.R`)
- [ ] `test_check("nhdplusTools")` in `tests/testthat.R`

### 2e. File renames

- [ ] `vignettes/nhdplusTools.Rmd` → `vignettes/hydrogeofetch.Rmd` (update VignetteIndexEntry inside)
- [ ] `tests/testthat/test_nhdplusTools.R` → `tests/testthat/test_hydrogeofetch.R`
- [ ] Delete generated files that will be rebuilt: `man/nhdplusTools_*.Rd`, `inst/doc/nhdplusTools.*`, `docs/` directory

### 2f. Rebuild and check

- [ ] `devtools::document()` — regenerates NAMESPACE, man pages
- [ ] `devtools::build_readme()` — re-knit README.md
- [ ] `devtools::check()` — full R CMD check
- [ ] `pkgdown::build_site()` locally to confirm it builds — deploy after repo rename (milestone 3)

### 2g. Validate

Run these checks to make sure nothing was missed:

- [ ] `grep -r "nhdplusTools" R/ tests/ vignettes/ man/ DESCRIPTION NAMESPACE` — should return zero hits outside of NEWS.md history, PLAN.md, inst/CITATION (deferred), and README install/badge lines (deferred)
- [ ] `grep -r "NHDPLUSTOOLS_" R/` — should only appear in the env var fallback code from 2c
- [ ] Confirm `library(hydrogeofetch)` loads without errors
- [ ] Confirm `hydrogeofetch_data_dir()` and `hydrogeofetch_cache_settings()` work
- [ ] Confirm `?hydrogeofetch_data_dir` and `?hydrogeofetch_cache_settings` render help pages
- [ ] Confirm all vignettes build: `devtools::build_vignettes()`
- [ ] Confirm `example(get_nldi)` (or another web-service example) runs
- [ ] R CMD check: 0 errors, 0 warnings, no NOTEs about the rename
- [ ] Install from local source and run `testthat::test_local()` in a clean session

**Done when:** all items above checked off, R CMD check clean.

**Gate:** Internal review of the renamed package. Share with a few colleagues to install and test before anything goes public.


## 3. Dependency and code modernization

Clean up legacy dependencies and bring the codebase to a consistent modern style.

**HTTP client:** Replace httr with httr2. Adopt httptest2 for web service tests so they run against recorded fixtures in CI and live against real services on demand. Binary downloads (GeoPackage files) stay as live-only tests behind `skip_on_cran()`.

**Dependency cleanup:** Drop R.utils (one `gunzip()` call, replaceable with base R), tidyr (one `unnest()` call), and unused arrow import. Move maptiles/mapsf to Suggests. Replace fst with arrow/parquet for VAA table caching.

**Code style:** Convert all `%>%` to native `|>` and drop the magrittr import. Apply current style conventions across the full codebase — alignment, spacing, and any other holdovers from earlier eras of the package.

**Test fragility found during milestone 2:** Several tests crash or fail when web services are unavailable or misbehaving. These need attention during the httptest2 migration:
- `test_get_vaa.R` and `test_02_subset_extras.R`: R subprocess segfaults under parallel testing. Parallel testing disabled (`Config/testthat/parallel: false`) as a workaround. Investigate whether fst/arrow memory use is the root cause; may resolve itself once fst is replaced with arrow/parquet.
- `test_01_get_nldi.R`: `get_nldi_index()` test fails when NLDI pygeoapi returns server errors. Added `skip_if(is.null(...))` as a stopgap. With httptest2 fixtures this test should run deterministically.
- `get_geoconnex_reference` `\donttest` examples: fail on TLS certificate errors or when geoconnex.us is unreachable. Consider converting to `\dontrun` or recording fixtures.

**Done when:** No remaining httr, magrittr, R.utils, tidyr, or fst usage. arrow wired up for VAA caching. httptest2 fixtures recorded for JSON/GeoJSON service calls. Tests pass in both mock and live modes. R CMD check clean. Re-enable parallel testing once subprocess crashes are resolved.

**Gate:** Run the full test suite live to confirm service compatibility. Review httptest2 fixture sizes — if any are too large to check in, simplify or move those tests to live-only.


## 4. USGS software release

Create a new entry on code.usgs.gov for hydrogeofetch. Mint a new DOI. The existing nhdplusTools DOI stays pointed at a historical release page.

**Done when:** DOI assigned for hydrogeofetch.

**Gate:** Update the items deferred from milestone 2:
- inst/CITATION: title, textVersion, citHeader, DOI
- code.json: `name`, URLs, version entries
- DESCRIPTION: DOI in any relevant fields
Confirm the plan for the old nhdplusTools DOI redirect with the DOI coordinator.


## 5. hydrogeofetch on CRAN

Submit hydrogeofetch v1.0 to CRAN. The repo is still named nhdplusTools at this point — that's fine, CRAN only cares about the package tarball.

**Release workflow:** Push an `rc/1.0.0` branch to code.usgs.gov. The GitLab CI pipeline runs a lightweight structural check, builds the source tarball, uploads it to the GitLab package registry, and runs `R CMD check --as-cran` on that tarball. Download the verified tarball from the registry and submit it to CRAN. See `.gitlab-ci.yml` and the README "Build and release" section for details.

**Done when:** Package accepted and available on CRAN.

**Gate:** Update README.Rmd `install.packages()` line to `install.packages("hydrogeofetch")`. Announce hydrogeofetch v1.0. Notify reverse-dependency maintainers (amadeus, elfgen, StreamCatTools) that the deprecation shim is coming and they should plan to switch their imports.


## 6. Rename the GitHub repo

Rename doi-usgs/nhdplusTools to doi-usgs/hydrogeofetch. GitHub maintains redirects from the old URL. Requires org admin access — this milestone can move earlier if access becomes available.

**Done when:** The repo is accessible at doi-usgs/hydrogeofetch and old nhdplusTools URLs redirect.

**Gate:** Verify redirects work for clone URLs, issue links, and pkgdown. Then update the items deferred from milestone 2:
- DESCRIPTION `URL:` and `BugReports:` → new repo URL
- _pkgdown.yml `url:` → new GitHub Pages URL
- README.Rmd badge URLs and `remotes::install_github()` → new repo name
- CI config and any other hardcoded URLs that GitHub won't redirect (e.g., raw.githubusercontent.com paths)
- Re-knit README.md, deploy pkgdown site


## 7. nhdplusTools deprecation shim

Build a shim version of nhdplusTools (maintained as a branch in the hydrogeofetch repo). Every exported function becomes a `.Deprecated()` wrapper that forwards to `hydrogeofetch::*`. Submit to CRAN.

**Done when:** Shim accepted on CRAN. Installing nhdplusTools gives deprecation warnings that name hydrogeofetch as the replacement.

**Gate:** Confirm reverse-dep maintainers have a migration path and timeline. Announce the deprecation and the October 2028 archive date.


## 8. Archive nhdplusTools — October 2028

Request CRAN archive after the two-year sunset. Archived packages remain installable from the CRAN archive, so existing code pinned to nhdplusTools keeps working.

**Done when:** nhdplusTools archived on CRAN.

**Gate:** Verify no remaining reverse dependencies on CRAN. Post a final announcement. Confirm the archived package installs cleanly from the archive.
