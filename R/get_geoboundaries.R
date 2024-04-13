################################################################################
# title: Get geoBoundaries and Transform 
# author: Mehmet Göktuğ Öztürk
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# dir paths
main_dir <- "/home/quercus/github/first_paper/"
sub_dir <- "data/input/geoBoundaries/"
target_dir <- "data/output/geoBoundaries/"

# enter into the thesis directory
setwd(main_dir)

# create directory
dir.create(sub_dir)
dir.create(target_dir)

# download boundary data 
download.file(
  "https://github.com/wmgeolab/geoBoundaries/raw/4499140/releaseData/gbOpen/TUR/ADM0/geoBoundaries-TUR-ADM0-all.zip",
  destfile = paste0(sub_dir, "tr_adm0.zip")  
)
download.file(
  "https://github.com/wmgeolab/geoBoundaries/raw/4499140/releaseData/gbOpen/TUR/ADM1/geoBoundaries-TUR-ADM1-all.zip",
  destfile = paste0(sub_dir, "tr_adm1.zip")  
)
download.file(
  "https://github.com/wmgeolab/geoBoundaries/raw/4499140/releaseData/gbOpen/TUR/ADM2/geoBoundaries-TUR-ADM2-all.zip",
  destfile = paste0(sub_dir, "tr_adm2.zip") 
)

# unzip files
zip_files <- list.files(sub_dir, full.names = TRUE)
for (i in zip_files) {
  unzip(i, exdir = "./data/input/geoBoundaries")
}

# remove zip files
system("rm ./data/input/geoBoundaries/*.zip")

# list shp files and convert to gpkg
shp_files_full <- list.files(
  sub_dir, 
  pattern = "shp",
  recursive = TRUE, 
  full.names = TRUE
)

shp_files <- list.files(
  sub_dir, 
  pattern = "shp",
  recursive = TRUE
)

shp_files <- gsub(".shp", "", shp_files)

counter <- 1
for (i in shp_files_full) {
  sf::read_sf(i) |>
    sf::st_transform("EPSG:3035") |>
    sf::write_sf(paste0(target_dir, shp_files[counter], ".gpkg"))
  counter <- counter + 1
}

# change working directory
setwd(target_dir)

# rename of new files
system("mv geoBoundaries-TUR-ADM0.gpkg tr_adm0.gpkg")
system("mv geoBoundaries-TUR-ADM0_simplified.gpkg tr_adm0_simplified.gpkg")
system("mv geoBoundaries-TUR-ADM1.gpkg tr_adm1.gpkg")
system("mv geoBoundaries-TUR-ADM1_simplified.gpkg tr_adm1_simplified.gpkg")
system("mv geoBoundaries-TUR-ADM2.gpkg tr_adm2.gpkg")
system("mv geoBoundaries-TUR-ADM2_simplified.gpkg tr_adm2_simplified.gpkg")
