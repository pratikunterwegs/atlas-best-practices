#!/bin/bash

Rscript --slave -e 'bookdown::render_book("index.rmd", "bookdown::pdf_document2")'

# to render source files
Rscript --slave -e 'lapply(list.files(pattern = "\\d{2}\\w+.Rmd"), function(x) knitr::purl(x, output = sprintf("R/%s", gsub(".{4}$", ".R", x))))'

# copy pdfs and name correctly
cp -p atlas-manuscript/arxiv_article.pdf docs/ms_atlas_preproc_main_text`date -I`.pdf
cp -p docs/supplementary_material.pdf docs/ms_atlas_preproc_supplementary_material_`date -I`.pdf
rm docs/supplementary_material.pdf

# build atlastools manual
Rscript --slave -e 'devtools::build_manual(pkg = "../atlastools", path = "docs/")'
