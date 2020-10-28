#!/bin/bash
Rscript --slave -e 'bookdown::render_book("index.rmd")'
Rscript --slave -e 'bookdown::render_book("index.rmd", "rmarkdown::pdf_document"); warnings()'
