# This script creates rasters (DSM, DTM and CHM) from point cloud data using lastools functions
# The functions are all contained in 'LAStools_functions.R'
# Adjust lines 16-27 to your data + choose parameters and it should run

# It is a bit brittle w.r.t the file system. It will create new folders (you can ignore the warnings)
# for each of the DSM, DTM and CHM, with subfolders for the parameters (tin_resolution, subcircle, and raster_resolution)

# This script is fast, but pretty inefficient in terms of storage. The las files are duplicated a few times
# I usually delete all intermediate files once I'm happy with the rasters
# A better solution would be to replace the DSM_small_tiles/*laz files each time
# Classifying the ground and adding a normalized height should not alter the original data
# This would remove the need for DSM_small_tiles/ground/ and DSM_small_tiles/chm/


library(raster)
library(ggplot2)
#library(foreach)
#library(doParallel)
#library(lidR)
#library(rgeos)
#library(sp)
#library(stringr)

# Set up the paths & functions
OneDrive='C:/Users/User/OneDrive - University Of Cambridge/'
source(paste0(OneDrive,'R/LiDAR_basics/functions_tools/LAStools_functions.R'))

##################### Set up basics
general_parameters=data.frame(
  LAStools_path="C:/Users/User/Documents/LAStools/bin",
  base_path='C:/Users/User/Documents/Data/ALS_Data/Brazil_ALS/BON_A01/2013/',
  file_type='*.laz',
  bbtype="tile", # This should be "tile" or "orig"
  raster_output_name="BON_A01_2013",
  raster_resolution=1,
  n_cores=4,
  epsg_code=4326
)

# WGS84 =4326
# Sabah=32650
# Tanzania = 32736
# French Guiana = 
# Bialowieza=2180

tin_resolution=10 # This is the resolution (in pixels) of the DTM tin generation
subcircle=0.2     # This effectively 'expands' the points to a finite size (in pixels)


# CREATE SMALL TILES - this just helps with the memory - 
create_small_tiles(general_parameters, 
                   large_tiles_folder="", 
                   tile_size=250,
                   buffer_size=20)

# following functions rely on finding 'DSM_small_tiles/*.laz' as created above
# If you want to skip the above step, put your files in base_path/DSM_small_tiles/*

# Make DSM and DTM - this function does all the work
#make_DSM_DTM_tiled_rasters(base_path,raster_output_name,raster_resolution,tin_resolution,subcircle,epsg_code,LAStools_path)
# If the above fails, try these individually
make_dsm_full(general_parameters,tin_resolution,subcircle)
make_dtm_full(general_parameters,tin_resolution)
CCCHM=mosaic_DSM_DTM_CCCHM(general_parameters,tin_resolution,subcircle,num_chunks=5)
# Inspect the DSM and DTM to see if they are OK.  

# Histogram of heights in the cheap and cheerful CHM (DSM-DTM) to set h_seq.
#hist(CCCHM)
CCCHM_heights=data.frame(height=getValues(CCCHM),version="Cheap & cheerful")
max(CCCHM_heights,na.rm=TRUE)
ggplot(CCCHM_heights, aes(x=height))+geom_histogram(alpha=0.2, position="identity")+xlim(0,100)

# Focus on the top bit: 
top_bit=dplyr::filter(CCCHM_heights,height>50)
ggplot(top_bit, aes(x=height))+geom_histogram(alpha=0.2, position="identity")+xlim(0,100)

# Adjust h_seq to the CHM profile. We use multiple layers to avoid noise, but this also makes it slow
#h_seq=c(seq(from = 1, to = 60, by = 5),seq(60,120,2))
h_seq=c(seq(from = 1, to = 30, by = 3),seq(30,60,5))

make_chm_las_files(general_parameters,tin_resolution,subcircle,h_seq)

# Make the CHM tiled rasters - This is the slowest part, it loops over h_seq
make_pitfree_chm_tiled_rasters(general_parameters,tin_resolution,subcircle,h_seq)
CHM=mosaic_CHM_rasters(general_parameters,tin_resolution,subcircle,num_chunks=5)


# Compare the CHMs
#hist(CHM)
CHM_heights=data.frame(height=getValues(CHM),version="Pitfree")
max(CHM_heights,na.rm=TRUE)
dat=dplyr::bind_rows(CCCHM_heights,CHM_heights)
ggplot(dat, aes(x=height, fill=version)) + geom_histogram(alpha=0.2, position="identity")+xlim(0,70)

  