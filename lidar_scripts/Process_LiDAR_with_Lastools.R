# This script creates rasters (DSM, DTM and CHM) from point cloud data using lastools functions
# The functions are all contained in 'LAStools_functions.R'
# Adjust lines 24-36 to your data + choose parameters and it should run

# It is a bit brittle w.r.t the file structure. It will create new folders (you can ignore the warnings)
# for each of the DSM, DTM and CHM, with subfolders for the parameters (tin_resolution, subcircle, and raster_resolution)

# This script is fast, but inefficient in terms of storage. The las files are duplicated a few times
# I usually delete all intermediate files once I'm happy with the rasters
# A better solution would be to replace the DSM_small_tiles/*laz files each time
# Classifying the ground and adding a normalized height should not alter the original data
# This would remove the need for DSM_small_tiles/ground/ and DSM_small_tiles/chm/

library(raster) # These functions rely on the raster package which is being superceded by terra
library(ggplot2)
library(foreach)
library(doParallel)

# Set up the paths & functions
OneDrive='C:/Users/User/OneDrive - University Of Cambridge/'
source(paste0(OneDrive,'R/LiDAR_basics/functions_tools/LAStools_functions.R'))

##################### Set up basics
general_parameters=data.frame(
  LAStools_path="C:/Users/User/Documents/LAStools/bin",
  base_path=paste0('C:/Users/User/Documents/Data/ALS_Data/Brazil_ALS/JAR_A01b/2017/'),
  #base_path=paste0('C:/Users/User/Documents/Data/ALS_Data/Malaysia_ALS/Sepilok/2020/'),
  #base_path=paste0('C:/Users/User/Documents/Data/ALS_Data/Poland_ALS/2019/'),
  file_type='*.laz',
  bbtype="tile", # This should be "tile" or "orig"
  #raster_output_name=paste0("Miombo",i,"_2014"),
  raster_output_name="Jarib_2020",
  raster_resolution=0.2,
  n_cores=4,
  epsg_code=4326
)


# CREATE SMALL TILES - this just helps with the memory - 
create_small_tiles(general_parameters, 
                   large_tiles_folder="", 
                   tile_size=250,
                   buffer_size=0)

#general_parameters$file_type="*.laz"
# following functions rely on finding 'DSM_small_tiles/*.laz' as created above
# If you want to skip the above step, put your files in base_path/DSM_small_tiles/*

make_point_density_raster_full(general_parameters,num_chunks=9)
make_pulse_density_raster_full(general_parameters,num_chunks=5)


tin_resolution=10 # This is the resolution (in pixels) of the DTM tin generation
subcircle=0.2     # This effectively 'expands' the points to a finite size (in pixels)

DSM=make_random_dsm_full(general_parameters,subcircle,9) # this calculates the mean of first returns
make_dsm_full(general_parameters,subcircle) # this finds the highest first return

make_dtm_full(general_parameters,tin_resolution)
CCCHM=mosaic_DSM_DTM_CCCHM(general_parameters,tin_resolution,subcircle,num_chunks=20)
# Inspect the DSM and DTM to see if they are OK.  

# Histogram of heights in the cheap and cheerful CHM (DSM-DTM) to set h_seq.
hist(CCCHM)

# Adjust h_seq to the CHM profile. We use multiple layers to avoid noise, but this also makes it slow
#subcircle=0..05
h_seq=c(seq(from = 1, to = 110, by = 10))
make_chm_las_files(general_parameters,tin_resolution,subcircle,h_seq)

# Make the CHM tiled rasters - This is the slowest part, it loops over h_seq
make_pitfree_chm_tiled_rasters(general_parameters,tin_resolution,subcircle,h_seq)
CHM=mosaic_CHM_rasters(general_parameters,tin_resolution,subcircle,num_chunks=5)

# thin versions
make_thin_chm_las_files(general_parameters,tin_resolution,subcircle,h_seq,pd=4)
make_pitfree_thin_chm_tiled_rasters(general_parameters,tin_resolution,subcircle,h_seq)
CHM=mosaic_thin_CHM_rasters(general_parameters,tin_resolution,subcircle,num_chunks=5)
