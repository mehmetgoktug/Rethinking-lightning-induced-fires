# Rethinking-lightning-induced-fires

Due to legal restrictions, the datasets used in this study cannot be publicly shared. Researchers interested in accessing these datasets are advised to directly contact the relevant authorities. The lightning occurrence data utilized in this research can be acquired from the Turkish State Meteorological Service, while the fire occurrence data and forest administrative boundary data can be obtained from the General Directorate of Forestry.

## Software Versions

- R = 4.3.3
- tidyverse = 2.0.0 
- data.table = 1.14.10
- sf = 1.0.14 
- geos = 0.2.4 
- stars = 0.6.4 
- terra = 1.7.55
- spdep = 1.3.1

## Reproduction

These instructions will guide users on how to replicate the analysis using RStudio or any text editors like VSCode, VIM, Emacs, etc.

- **RStudio Users**: Open the `rethinking_lightning_induced_fires.Rproj` project file in RStudio.
- **Non-RStudio Users**: Open the working directory in your preferred text editor or terminal.

### Reproduction Steps

1. **Get Data**
   1. Run `./R/get_geoboundaries.R` to download geoboundaries data for defining the study area.

2. **Data Wrangling**
   1. Run `./R/edit_forest_adm.R` to process administrative forest boundaries data for Türkiye. This includes converting to lowercase, replacing Turkish characters with Latin ASCII characters, standardizing attributes of spatial vector data, and transforming the spatial projection.
   2. Run `./R/tidy_lightning.R` to clean and standardize lightning data, including necessary spatial transformations.
   3. Run `./R/tidy_fire.R` to clean and standardize wildfire data, including necessary spatial transformations.
   4. Run `./R/edit_values.R` to manually adjust wildfire data to ensure correct joins.
   5. Run `./R/calc_lightning_count.R` to generate 1x1 km grids and calculate the lightning count for each grid cell.

3. **Results: Figures and Tables**
   1. Run `./R/plot/fig1a.R` to generate Figure 1a, showing the spatial variability of lightning occurrence.
   2. Run `./R/plot/fig1b.R` to generate Figure 1b, illustrating the spatial variability of lightning-induced fire percentages by local unit.
   3. Run `./R/plot/fig1c.R` to generate Figure 1c, depicting the distribution of the total number of fires by local unit.
   4. Run `./R/plot/fig2.R` to generate Figure 2, which displays the total number of recorded fires and their relative proportions by cause (human-caused, lightning-induced, and unknown origin) across regional units.
   5. Run `./R/plot/fig3.R` to generate Figure 3, presenting representative examples of major trends in fire causes across regional units.
   6. Run `./R/plot/fig4.R` to generate Figure 4, showing annual trends in fire causes from 2002 to 2022 for the regional units depicted in Figure 3.
   7. Run `./R/gmoran.R` to calculate Global Moran's *I*, a measure of spatial autocorrelation.
   8. Run `./R/plot/fig5.R` to calculate Local Moran's *I* and generate Figure 5, which shows the distribution of lightning-induced fire clusters across Türkiye.
   9. Run `./R/table1.R` to generate Table 1 and Supplement Table 1, displaying the percentage of fire incidents and burned area by fire size class in selected regions and across all of Türkiye.
   10. Run `./R/plot/supp_fig1.R` to generate Supplement Figure 1, detailing the monthly distribution of wildfires by origin in Türkiye.
   11. Run `./R/supp_table2.R` to generate Supplement Table 2, showing the top-10 local units with the highest percentage of lightning-induced fires.
