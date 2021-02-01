#!/bin/bash

# remove old tex files and pdfs
rm docs/*.pdf
rm docs/*.tex

Rscript --slave -e 'bookdown::render_book("index.rmd", "bookdown::pdf_document2")'

# to render source files
Rscript --slave -e 'lapply(list.files(pattern = "\\d{2}\\w+.Rmd"), function(x) knitr::purl(x, output = sprintf("R/%s", gsub(".{4}$", ".R", x))))'

# copy pdfs and name correctly
# remove old pdfs


cp -p atlas-manuscript/arxiv_article.pdf docs/ms_atlas_preproc_main_text`date -I`.pdf
mv docs/supplementary_material.pdf docs/ms_atlas_preproc_supplementary_material_`date -I`.pdf

# copy tex and replace old tex
cp -p atlas-manuscript/arxiv_article.tex docs/ms_atlas_preproc_main_text_`date -I`.tex

mv docs/supplementary_material.tex docs/ms_atlas_preproc_supplement_`date -I`.tex

# build atlastools manual
Rscript --slave -e 'devtools::build_manual(pkg = "../atlastools", path = "docs/")'
