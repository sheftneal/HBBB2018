# Robust relationship between air quality and infant mortality in Africa


Replication materials for _Heft-Neal, Burney, Bendavid & Burke (2018)_.

The materials in this repository allow users to reproduce the figures, tables, and calculations appearing in the main text and extended data of the paper.

If you find a meaningful error in the code or have questions or suggestions, please contact Sam Heft-Neal at sheftneal@stanford.edu.

## Organization of repository

* **scripts**: scripts for replication of figures, tables, and calculations.
* **figures/published**: published versions of the figures.
* **figures/raw**: scripts 03-12 will generate pdf figures in this directory.
* **data/inputs**: data inputs for analysis.
* **data/figure_data**: data inputs for figures.
* **data/RR_curve_HBBB2018**: Relative Risk curve published in the paper. Includes CSV and RData versions.
* **HBBB2018_replication.Rproj**: organizes the replication materials into an RStudio Project. 

## Instructions
The repository is 115Mb.

Users can manage replication through the R project "HBBB2018_replication.Rproj". Alternatively users can set working directory to HBBB2018 and run scripts independently.

Script 01 derives the Relative Risk (RR) curve used in the paper. The final RR curve is also available for download in CSV or RData versions in data/.

Script 02 replicates the calculations cited in the paper.

Scripts 03-06 generate the figures in the main text of the paper and writes them to figures/raw. The figures produced by these scripts will be slightly visually different than the published figures because post-processing was done in Adobe Illustrator. Published versions of the figures are available in figures/published.

Scripts 07-12 generate the figures in the extended data section of the paper. 

Script 13 estimates the results shown in Table ED1.

Fig ED1 is not included in the replication materials because it's based on data from other studies. Fig ED2 is not included because it cannot be generated without including the geo-coordinates of DHS households which we cannot release due to the user agreement for DHS data.

Scripts 01, 04, and 12 take more than 1 hour each to run. Scripts 07 and 13 each take 1-2 minutes. All other scripts should run within a few seconds.


## R packages required
* **classInt**
* **fields**
* **lfe**
* **multcomp**
* **maptools**
* **plotrix**
* **splines**
* **tidyverse**

Scripts were written in R 3.5.0.

Users can run the following command to install the most recent versions of these packages:

```R
install.packages(c('classInt', 'fields', 'lfe', 'multcomp', 'maptools', 'plotrix', 'splines', 'tidyverse'), dependencies = T)
```
