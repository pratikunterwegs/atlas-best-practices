# Source Code and Supplementary Material for: _A Guide to Pre-processing High-throughput Animal Tracking Data_

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4314810.svg)](https://doi.org/10.5281/zenodo.4314810)


This is source code for a manuscript that sets out a pipeline for pre-processing data from ATLAS systems, but which can be applied to any high-throughput tracking data.

## Source Code

Most of the source code may be found in the folder `R`, with numbers indicating the order in which it is to be run.
This source code is generated from the `.Rmd` files in the top-level directory using `knitr::purl`.
See the `render_books.sh` file for details of the conversion from `.Rmd` to `.R`.

### R/

- _01_background.R: Simulating data using the `R` package `smoove` for the section _Pipeline Overview, Getting `atlastools`, and Simulating Data_.
Simulated data is saved into the `data` folder as `data_sim.csv` (uniform correlated velocity model data), and `data_for_patch.csv` (three rotational/advection correlated velocity model datasets). 
No seed was set for `data_sim.csv`, but `data_for_res_patch.csv` was generated using a set seed.

- _02_filtering_data.R: Source code for figures that support the sections _Spatio-Temporal Filtering_ and _Filtering to Reduce Location Errors_.
Also adds error to the UCVM dataset (`data_sim.csv`) using functions defined in `R/helper_functions.R`.
Makes Figures 2 -- 3.
Writes cleaned data to `data/data_no_reflections.csv`.

- _03_smoothing_tracks.R: Code to make Figures 4 -- 5 in the section _Smoothing and Thinning Data_.

- _04_residence_patch.R: Code to make Figure 6 in the section _Synthesising Movement Tracks into Residence Patches_.

- 05_calibration_data.R: Code for Figure 7 in the main text subsection _Validating the Residence Patch Method_. 
Shows a fully worked out example of pre-processing ATLAS data, as well as the linear model validating the residence patch method.
Also used in Supplementary Material 01 (`docs/ms_atlas_preproc_supplementary_material_DATE.pdf`) section _Validating the Residence Patch Method with Calibration Data_.

- 06_bat_data.R: Shows a fully worked out example of pre-processing ATLAS tracking data from fruit bats in the Hula Valley, Israel, which forms the main text section _Worked out Example on Animal Tracking Data_.
Also used in Supplementary Material 01 (`docs/ms_atlas_preproc_supplementary_material_DATE.pdf`) section _Processing Egyptian Fruit Bat Tracks_.

- helper_functions.R: Script with helpful functions to simulate errors in data.

### Top level

- `.Rmd` files corresponding to the `.R` files described above.
The `xx_references.Rmd` file ensures a _References_ section in Supplementary Material 01.

- `srtm_hula.ipynb`: A Jupyter notebook with Python code to retrieve SRTM 30m resolution data for the Hula Valley Israel.
This is used solely as a background for Figure 8 in the main text.

- `render_books.sh`: A shell script with helper commands to generate the Supplementary Material and the source code as `R` scripts from the `Rmd` files.

- `atlas_best_practices.qgz`: A QGIS 3 project file to generate Figure 8 in the main text.

- `references.bib`: References for the main text and Supplementary Material 01.

- `.git*`: Files for git repository organisation. The submodule `atlas-manuscript` contains the `TEX` source of the main text and is private.

## Supplementary Material

Supplementary Material 01 is rendered into the `docs/` folder as `docs/ms_atlas_preproc_supplementary_material_DATE.pdf`, where `DATE` is the date of rendering.
The rendering code using the `R` package `bookdown` is run from the shell script `render_books.sh`.
The `TEX` version is retained as `supplementary_material.tex`.

Supplementary Material 02 is the PDF manual for the `R` package `atlastools`, which is rendered from the installed version of `atlastools` using `devtools::build_manual`.
The rendered output is `atlastools_VERSION.pdf`.
Versioning is not implemented in the naming.
This command is also run from the shell script `render_books.sh`.

The latest version of `atlastools` may be found at https://github.com/pratikunterwegs/atlastools.

## Main Text

The main text is also copied from a local source into the `docs/` as a PDF.

## Data 

The `data/` folder contains the following main datasets.

### Simulated Movement Data

1. `data_sim.csv`: Data simulated from `R/_01_background.R`. An UCVM track simulated using `smoove`.

2. `data_for_res_patch.csv`: Data simulated from `R/_01_background.R`. Three RACVM tracks simulated using `smoove`.

### Empirical Movement Data

1. `atlas1060_allTrials_annotated.csv`: A hand held calibration ATLAS track collected in August 2020 in the Dutch Wadden Sea.
Used to make Figure 7 in the main text subsection _Validating the Residence Patch Method_, and in Supplementary Material 01 (`docs/ms_atlas_preproc_supplementary_material_DATE.pdf`), section _Validating the Residence Patch Method with Calibration Data_.

2. `Three_example_bats.sql`: Tracks from three Egyptian fruit bats (_Rousettus aegyptiacus_)from the Hula Valley, Israel.
Used to make Figure 8 in the main text section _Worked out Example on Animal Tracking Data_, and for the Supplementary Material 01 (`docs/ms_atlas_preproc_supplementary_material_DATE.pdf`) section _Processing Egyptian Fruit Bat Tracks_.

3. `bat_data.csv`: The above data saved as a `csv` file.

### Empirical Spatial Data

1. `griend_polygon/`: A shapefile with the polygon outline of the Dutch Wadden Sea island of Griend, where the calibration data was collected.

2. `hula_valley/`: A shapefile of the extent of the study site of the Hula Valley, Israel.

3. `griend_hut.gpkg`: The location of the field station on Griend.

4. `Roosts.csv`: The locations of bat roosts in the Hula Valley.

5. `trees_update_clusters_Aug2020.csv`: The locations of fruit trees in the Hula Valley, Israel.

6. `srtm_30.tif`: A digital elevation model (DEM) of the Hula Valley, Israel.

### Processed Data

All other data are processed forms of the simulated or empirical movement data and not described further.

## Other Directories

Other directories such as `figures/` or `_bookdown_files/` are self-explanatory.

## `atlastools` Status

<!-- badges: start -->
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4033154.svg)](https://doi.org/10.5281/zenodo.4033154)
  [![R build status](https://github.com/pratikunterwegs/atlastools/workflows/R-CMD-check/badge.svg)](https://github.com/pratikunterwegs/atlastools/actions)
  [![codecov.io](https://codecov.io/github/pratikunterwegs/atlastools/coverage.svg?branch=master)](https://codecov.io/github/pratikunterwegs/atlastools/branch/master)
<!-- badges: end -->
