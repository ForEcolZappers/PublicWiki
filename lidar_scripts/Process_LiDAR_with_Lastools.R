
# I need to go through this script and make it simpler with some functions
# Namings should be standardized from base_name
# Final raster products should be separate with parameters in filenames
# Parameters in folder names for sensitivity analysis
# I also want to check with CHM-CHM=DSM-DSM


library(raster)
library(foreach)
library(doParallel)
library(lidR)
library(rgeos)
library(sp)
library(stringr)

# The paths & functions
OneDrive='C:/Users/User/OneDrive - University Of Cambridge/'
source(paste0(OneDrive,'R/LiDAR_basics/create_CHM_pitfree.R'))
source(paste0(OneDrive,'R/LiDAR_basics/create_DSM_pitfree.R'))
source(paste0(OneDrive,'R/LiDAR_basics/LAStools_functions.R'))
LAStools_path <- "C:/Users/User/Documents/LAStools/bin"
n_cores <- 4




########################################################
# Set up the basics

# The raw data
setwd("~/Data/ALS_Data/")
setwd("C:/Users/User/Documents/Data/ALS_Data/")
base_folder=
raster_output_name="Sepilok_2020"
epsg_code=32650 # This is the one for sABAH
raster_resolution=1
subcircle=0.2

small_tiles_path = file.path(base_folder,"DSM_small_tiles")
small_tiles_pattern=file.path(small_tiles_path, "*.laz")
small_tiles_list = list.files(small_tiles_path, full.names = TRUE, pattern = ".laz$")

#####################
#Done, RIL, Sepilok 2020,
#
base_path='C:/Users/User/Documents/Data/ALS_Data/Malaysia_ALS/SBE/2020/'
raster_output_name="SBE_2020"
raster_resolution=1
tin_resolution=10
subcircle=0.2
h_seq=c(seq(from = 1, to = 60, by = 5),seq(60,120,2))
epsg_code=32650
LAStools_path="C:/Users/User/Documents/LAStools/bin"

cl <- makeCluster(4)
registerDoParallel(cl)
do_everything(base_path=base_path,raster_output_name=raster_output_name,raster_resolution=raster_resolution,
              tin_resolution=tin_resolution,subcircle=subcircle,h_seq=h_seq,epsg_code=epsg_code,LAStools_path=LAStools_path)
stopCluster(cl)
  
mosaic_all_rasters(base_path=base_path,raster_output_name=raster_output_name,raster_resolution=raster_resolution,
                            tin_resolution=tin_resolution,subcircle=subcircle,epsg_code=epsg_code)
  
  
  

#___
####################
# CREATE PITFREE DSM (test)
tmp_dsm_path=paste0(DSM_raster_path,"/tmp_dsm")
dir.create(tmp_dsm_path)
#DSM_raster_path
create_DSM_pitfree(small_tiles_list,
                   raster_resolution,
                   subcircle,
                   kill=2,
                   DSM_raster_path,
                   tmp_dsm_path,
                   bbtype, 
                   LAStools_path)
mosaic_rasters(raster_path=DSM_raster_path,
               raster_resolution=1,
               raster_type="DSM",
               raster_output_name='RIL2020',
               num_chunks=10,
               epsg_code=epsg_code)








####################################################################

# CREATE SMALL TILES - this is the first step
##########
DSM_path="Malaysia_ALS/SBE/2020/DSM_las/"
DSM_files=list.files(DSM_path, full.names = TRUE,pattern = ".las$")
small_tiles_path = file.path(base_folder, "DSM_small_tiles")
dir.create(small_tiles_path)

# Create small tiles (could turn this into a single function)
# DSM_path, small_tiles_path, size, buffer, epsg_code

# Index the tiles - creates the .lax files
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(DSM_files)[1], "*.las"),
  paste("-cores", n_cores)
))


#Create small tiles (250m)
system(
  paste(
    file.path(LAStools_path, "lastile"),
    "-i",
    file.path(dirname(DSM_files)[1], "*.las"),
    "-tile_size 250",
    "-buffer 20",
    paste("-epsg", epsg_code),
    paste("-odir", small_tiles_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)

# Index the small tiles
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(small_tiles_list)[1], "*.laz"),
  paste("-cores", n_cores)
))

