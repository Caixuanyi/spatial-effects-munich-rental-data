# Spatial Effects in Munich Rental Data

This repository contains the R code, model results, and figures accompanying the bachelor thesis **"Spatial Effects in Munich Rental Data"**.

## Repository Structure

### `R_code/`

This folder contains all R scripts used for data preparation, model estimation, sensitivity analysis, and spatial visualisation.

Please read the `README.md` file inside the `R_code/` folder before running the scripts. It explains the required execution order and provides additional information on how the code should be used.

### `model_results/`

This folder contains the numerical model output for three specifications:

- the default basis-dimension specification,
- the main specification with a spatial basis dimension of `k = 60`,
- the sensitivity analysis with a spatial basis dimension of `k = 120`.

The files include the relevant model summaries, ANOVA comparisons, and AIC results.

### `figures/`

This folder contains the spatial visualisations corresponding to the three model specifications:

- default basis dimensions,
- the main `k = 60` specification,
- the `k = 120` sensitivity specification.

## Data

The original Mietspiegel data are not included in this repository.

The `R_code/README.md` file provides information on how the local data files should be loaded before running the analysis.
