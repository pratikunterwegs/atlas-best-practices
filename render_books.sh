#!/bin/bash
Rscript --slave -e 'bookdown::render_book("index.rmd")'
Rscript --slave -e 'bookdown::render_book("index.rmd", "rmarkdown::pdf_document"); warnings()'

# to render source files
Rscript --slave -a 'lapply(list.files(pattern = "\\d{2}\\w+.Rmd"), function(x) knitr::purl(x, output = sprintf("R/%s", gsub(".{4}$", ".R", x))))'
