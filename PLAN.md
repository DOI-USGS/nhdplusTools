# hydrogeofetch migration plan

nhdplusTools 1.5.0 is the last feature release under the old name. This plan covers the rename to hydrogeofetch, the deprecation shim, and the eventual CRAN archive of nhdplusTools. Each milestone ends with a gate — review or communication work that should finish before moving on.


## 1. Close out nhdplusTools 1.5.0

Deploy the pkgdown site and confirm CRAN acceptance. This is the last version of nhdplusTools that carries new functionality.

**Done when:** pkgdown site live, package on CRAN, announcement posted.

**Gate:** Announce 1.5.0 as the final feature release. The README already points to the rename plan; confirm it reads clearly after the pkgdown rebuild.


## 2. Build hydrogeofetch v1.0

Rename all internal references: DESCRIPTION, roxygen tags, tests, vignettes, pkgdown config, .Rproj. New hex sticker. No changes to function names or signatures — this is a pure rename of the package identity.

**Done when:** R CMD check passes under the hydrogeofetch name with no NOTEs related to the rename.

**Gate:** Internal review of the renamed package. Share with a few colleagues to install and test before anything goes public.


## 3. Rename the GitHub repo

Rename doi-usgs/nhdplusTools to doi-usgs/hydrogeofetch. GitHub maintains redirects from the old URL.

**Done when:** The repo is accessible at doi-usgs/hydrogeofetch and old nhdplusTools URLs redirect.

**Gate:** Verify redirects work for clone URLs, issue links, and pkgdown. Update any hardcoded URLs in CI config or docs that GitHub won't redirect automatically (e.g., badge URLs, raw.githubusercontent.com paths).


## 4. Dependency and code modernization

Clean up legacy dependencies and bring the codebase to a consistent modern style.

**HTTP client:** Replace httr with httr2. Adopt httptest2 for web service tests so they run against recorded fixtures in CI and live against real services on demand. Binary downloads (GeoPackage files) stay as live-only tests behind `skip_on_cran()`.

**Dependency cleanup:** Drop R.utils (one `gunzip()` call, replaceable with base R), tidyr (one `unnest()` call), and unused arrow import. Move maptiles/mapsf to Suggests. Replace fst with arrow/parquet for VAA table caching.

**Code style:** Convert all `%>%` to native `|>` and drop the magrittr import. Apply current style conventions across the full codebase — alignment, spacing, and any other holdovers from earlier eras of the package.

**Done when:** No remaining httr, magrittr, R.utils, tidyr, or fst usage. arrow wired up for VAA caching. httptest2 fixtures recorded for JSON/GeoJSON service calls. Tests pass in both mock and live modes. R CMD check clean.

**Gate:** Run the full test suite live to confirm service compatibility. Review httptest2 fixture sizes — if any are too large to check in, simplify or move those tests to live-only.


## 5. USGS software release

Create a new entry on code.usgs.gov for hydrogeofetch. Mint a new DOI. The existing nhdplusTools DOI stays pointed at a historical release page.

**Done when:** DOI assigned for hydrogeofetch.

**Gate:** Update DESCRIPTION and the citation file with the new DOI. Confirm the plan for the old nhdplusTools DOI redirect with the DOI coordinator.


## 6. hydrogeofetch on CRAN

Submit hydrogeofetch v1.0 to CRAN from the renamed repo.

**Done when:** Package accepted and available on CRAN.

**Gate:** Announce hydrogeofetch v1.0. Notify reverse-dependency maintainers (amadeus, elfgen, StreamCatTools) that the deprecation shim is coming and they should plan to switch their imports.


## 7. nhdplusTools deprecation shim

Build a shim version of nhdplusTools (maintained as a branch in the hydrogeofetch repo). Every exported function becomes a `.Deprecated()` wrapper that forwards to `hydrogeofetch::*`. Submit to CRAN.

**Done when:** Shim accepted on CRAN. Installing nhdplusTools gives deprecation warnings that name hydrogeofetch as the replacement.

**Gate:** Confirm reverse-dep maintainers have a migration path and timeline. Announce the deprecation and the October 2028 archive date.


## 8. Archive nhdplusTools — October 2028

Request CRAN archive after the two-year sunset. Archived packages remain installable from the CRAN archive, so existing code pinned to nhdplusTools keeps working.

**Done when:** nhdplusTools archived on CRAN.

**Gate:** Verify no remaining reverse dependencies on CRAN. Post a final announcement. Confirm the archived package installs cleanly from the archive.
