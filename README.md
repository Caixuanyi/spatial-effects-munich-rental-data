# spatial-effects-munich-rental-data
R code accompanying the bachelor thesis "Spatial Effects in Munich Rental Data"
## Code

The R scripts should be run in the following order:

1. **Data Preparation and Descriptive Analysis.R**  
   Loads the three Mietspiegel datasets, prepares the variables, performs basic data-quality checks, calculates descriptive statistics, and produces the exploratory plots.

2. **k30.R**  
   Fits the models using the default basis dimensions in `mgcv` (`k = 10` for the one-dimensional smooths and `k = 30` for the spatial smooths). These models are used as the initial specification.

3. **k60.R**  
   Fits the main model specification used in the thesis. The basis dimensions are increased to `k = 20` for living area and building age and `k = 60` for the main spatial smooths. The difference smooths in Model 3 use `k = 20`.

4. **k120.R**  
   Fits the sensitivity-analysis models in which the main spatial basis dimension is increased to `k = 120`. The other basis dimensions remain unchanged.

5. **spatial_visualizations.R**  
   Checks the plausibility of the coordinate reference system (CRS), obtains and processes the Munich district boundaries, constructs the spatial prediction grid and spatial masks, and produces the spatial-effect maps for the default, main (`k = 60`), and sensitivity (`k = 120`) model specifications.
   
## Data

The data are not included in this repository.

Before running `Data Preparation and Descriptive Analysis.R`, replace the file paths in the three `load()` statements with the local paths to the corresponding 2021, 2023, and 2025 Mietspiegel data files.

The subsequent code assumes that the loaded R objects are named `daten_cai.21`, `daten_cai.23`, and `daten_cai.25`. These object names should not be changed unless the corresponding references in the subsequent code are changed accordingly.

## Execution order

```text
Data Preparation and Descriptive Analysis.R
        ↓
k30.R
        ↓
k60.R
        ↓
k120.R
        ↓
spatial_visualizations.R
