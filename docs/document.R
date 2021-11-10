# This builds documentation for the package in Markdown format.

Rd2md::ReferenceManual(outdir = "docs/")

text <- readLines("docs/Reference_Manual_nhdplusTools.md")

headings <- text[grepl("^# `.*", text)]

headings <- gsub("`", "", gsub("# `", "", headings))

toc <- paste0("- [", headings, "](#", headings, ")")

text <- c("# Vignettes",
          "- [Getting Started](nhdplusTools.md)",
          "- [Indexing Data to NHDPlus](point_indexing.md)",
          "- [Working with NHDPlusHR](nhdplushr.md)",
          "- [Plotting with nhdplusTools](plot_nhdplus.md)",
          "",
          "# Function Reference",
          "",
          toc,
          "",
          text[2:length(text)])

error_line <- which(grepl(".*```$", text) & !grepl("^```,*", text))

text[error_line] <- gsub("```", "", text[error_line])

text <- c(text[1:error_line], "```", text[error_line + 1:length(text)])

text <- text[!is.na(text)]

writeLines(text, "docs/Reference_Manual_nhdplusTools.md")

rmarkdown::render("vignettes/nhdplushr.Rmd", "md_document", output_file = "../docs/nhdplushr.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/plot_nhdplus.Rmd", "md_document", output_file = "../docs/plot_nhdplus.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/point_indexing.Rmd", "md_document", output_file = "../docs/point_indexing.md")
rstudioapi::restartSession()
rmarkdown::render("vignettes/nhdplusTools.Rmd", "md_document", output_file = "../docs/nhdplusTools.md")
rstudioapi::restartSession()
