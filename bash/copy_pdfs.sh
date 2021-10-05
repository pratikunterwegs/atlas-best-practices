#!/bin/bash
# remove old tex files and pdfs

cd ../docs
rm *main*.pdf

cd ..

# copy pdfs and name correctly
cp -p atlas-manuscript/arxiv_article.pdf docs/ms_atlas_preproc_main_text_`date -I`.pdf

pandoc atlas-manuscript/arxiv_article.tex --bibliography=atlas-manuscript/references.bib -o docs/ms_preprocessing_`date -I`.docx
