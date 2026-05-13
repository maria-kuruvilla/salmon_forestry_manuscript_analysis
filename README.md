# Salmon Forestry Manuscript Analysis

This repository contains the code and data used for the final analysis of the coast wide forestry effects on Pacific salmon in BC.

-   `data_processing_sst`: Contains the extended reconstructed sea-surface temperature (ERSST) data used for the analysis, code to process the ERSST and to add it to the existing salmon-forestry dataset

    -   `data`: Contains ERSST in `sst_ersst_df.csv`. [CM_CU_SITES_En.csv, PKE_CU_SITES_En.csv, PKO_CU_SITES_En.csv datasets](https://open.canada.ca/data/en/dataset/1ac00a39-4770-443d-8a6b-9656c06df6a3) contain information about watershed GFE ID, latitude, longitude.
    -   `ersst_chum`: Input `salmon_forestry_data_analysis/data/chum_SR_20_hat_yr.csv`. Calculates the shortest distance between the watershed and the SST data location, Joins appropriate ERSST timeseries data to dataset. Output in `salmon_forestry_data_anlaysis/data/chum_SR_20_hat_yr_w_ersst.csv`.
    -   `ersst_pink`: Input `salmon_forestry_data_analysis/data/pke_SR_10_hat_yr_reduced_VRI90.csv` and `salmon_forestry_data_analysis/data/PKO_SR_10_hat_yr_reduced_VRI90.csv`. Calculates the shortest distance between the watershed and the SST data location. Joins appropriate ERSST timeseries data to dataset. Output in `salmon_forestry_data_anlaysis/data/pke_SR_10_hat_yr_w_ersst.csv` and `salmon_forestry_data_anlaysis/data/pko_SR_10_hat_yr_w_ersst.csv`.
    -   `sst_ersst`: Downloads and processes spring ERSST data. Output in `data_processing_sst/data/sst_ersst_df.csv`.

-   `forestry_data_compilation`:

    -   `inputs`:

    -   `forestry_data`:

    -   `salmon_datasets_plotting`:

-   `manuscript_figures_tables`: Contains code to create figures and tables in the manuscript. Output in `output_figures_tables`.

-   `output_figures_tables`: Contains figures and tables in the manuscript.

-   `salmon_data_compilation`: Contains the data, code, and documentation for the assembly of spawner-recruitment (S-R) data for wild pink and chum salmon populations in British Columbia (excluding the Fraser River watershed). The data are assembled at a river- or system-level rather than an aggregate spatial scale.

    -   `\2019-NCC-SR-assembly`: contains materials (inputs, code, outputs) for previously assembled river-level S-R data by PSF for the North & Central Coast.
    -   `\Ecofish-SCVI-assembly`: contains materials (inputs, code, outputs) for assembling the coastwide S-R data, including the output from the NCC S-R assembly combined with S-R data for the south coast assembled drawing on data from PSF and DFO.

-   `salmon_forestry_data_analysis`:

    -   `data`:

        -   `chum_SR_20_hat_yr.csv`
        -   `chum_SR_20_hat_yr_w_ersst.csv`
        -   `pke_SR_10_hat_yr_reduced_VRI90.csv`
        -   `PKO_SR_10_hat_yr_reduced_VRI90.csv`
        -   `pke_SR_10_hat_yr_w_ersst.csv`
        -   `pko_SR_10_hat_yr_w_ersst.csv`
        -   `salmon_datasets_plotting`: Contains datasets necessary to generate maps with salmon watersheds.

    -   `stan models`:

        -   `code`
            -   `chum`: Contains stan file with model for chum with R file to fit model to data.
            -   `pink`: Contains stan file with model for pink with R file to fit model to data.
            -   funcs.R
        -   `outs`
            -   `fits`: Contains model objects.
            -   `posterior`: Contains models results.
            -   `summary`: Contains model summaries.
