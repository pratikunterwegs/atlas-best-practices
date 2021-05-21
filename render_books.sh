#!/bin/bash
# remove old tex files and pdfs

# style files
Rscript --vanilla --slave -e 'styler::style_dir("scripts", filetype = "R", recursive = FALSE)'
Rscript --vanilla --slave -e 'styler::style_dir("supplement", filetype = "Rmd", recursive = FALSE)'

# render supplement
Rscript --vanilla --slave -e 'bookdown::render_book("supplement/index.rmd", "bookdown::pdf_document2", output_dir = "docs", config_file = "supplement/_bookdown.yml")'

# to render source files
Rscript --slave -e 'lapply(list.files(pattern = "supplement\\/\\d{2}\\w+.Rmd"), function(x) knitr::purl(x, output = sprintf("R/%s", gsub(".{4}$", ".R", x))))'

mv docs/supplementary_material.pdf docs/ms_atlas_preproc_supplementary_material_`date -I`.pdf

# build atlastools manual
Rscript --slave -e 'devtools::build_manual(pkg = "../atlastools", path = "docs/")'

pandoc atlas-manuscript/arxiv_article.tex --bibliography=atlas-manuscript/references.bib -o docs/ms_preprocessing_`date -I`.docx
