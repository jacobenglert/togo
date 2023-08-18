Togo Research Project

Contents:
Section #1: Data description
Section #2: R Code Summary
Section #3: Other

Section #1

There are two datasets and two subfolders containing datasets in the Data
subfolder. 

The Raw subfolder contains the data as provided by the Togolese.
This folder also contains a folder with highlighted versions of the raw data
that depict the issues that need to be addressed. 

The Supplementary subfolder contains Sea Surface Temperature, El Nino Southern
Oscillation, and geographic data for the various Togolese cities.

The Cleaned.csv and Imputed.csv files are created using the Cleaning.R script
as described in Section #2. These files contain information from both the Raw
and Supplementary subfolders in a format that is useful for analyzing and
modeling.


Section #2: R Code Summary

There are four R scripts located within the R subfolder. The following
provides a description of the purpose of each one. Brief descriptions are
also included in the program headers.

Cleaning.R
 - Imports raw Excel sheet data as provided by the Togolese.
 - Imports supplementary data of interest (SST, Latitude, Longitude, etc.)
 - Outputs two datasets:
	- Cleaned.csv: This file contains 12312 observations of the following
	14 variables:
		- Year
		- DecYear (calculated as Year + (MonthNum - 0.5)/12)
		- Month
		- MonthNum: Numeric month (i.e. January = 1)
		- ENSO: El Nino Southern Oscillation
		- SST: Sea Surface Temperature
		- Location: Togolese city
		- Longitude: Longitude of Location
		- Latitude: Latitude of Location
		- Elevation: Elevation of Location
		- Type: Observation Type (Minimum or Maximum)
		- Temperature: Observed value
		- ExcelAvg: Was this observation an Excel mean in the raw data?
		- Outlier: Is this observation an outlier, within Location and
				Month, using the IQR method?

	- Imputed.csv: This file is identical to Cleaned.csv except for the fact
		that the Temperature variable now consists of an imputation for
		observations that were either an Excel mean or an outlier using
		the methodology described above. An additional variable "Imputed"
		serves as a flag showing which observations were imputed. The
		final models determined by the Spring 2017 section of Applied
		Mathematical Models at Northern Kentucky University were used to
		impute these values.

SSA.R
 - Implements Singular Spectrum Analysis for each individual Location and type
	of temperature (maximum and minimum). 
 - SSA function defined within the program takes the following parameters:
	- city: enquoted Location
	- var: enquoted Type
	- n: number of singular values (and corresponding vectors) to keep
 - SSA function outputs the following (.png) to the Images subfolder:
	- Scree Plot (Including trend)
	- Scree Plot (Excluding trend)
	- W-Correlation Matrix
	- Plot of left singular vectors
	- Plot of paired left singular vectors
	- Plot of time-series decomposition
	- Plot of 2100 projection using R's vforecast

Modeling.R
 - Creates two linear regression models (one for each temperature set)
 - Visualizes the models and examines some diagnostics
 - Saves images of the model fits to the Images subfolder
 - Includes a 3D visualization of the effect of latitude and longitude on
	temperature

ConversionFile.R
 - Serves as a workspace script for developing features and functions to use
	in the other R scripts. This program does NOT include anything the
	others don't. The primary purpose of this program was in migrating old
	analyses and programs into the new, more concise format.


Section #3: Other

Other items included in this Project are materials and presentations used for
various talks and poster presentations and LaTex files for a research paper in
progress.
	