# This is a script which is coded up from the lastools tutorial 
# https://rapidlasso.com/2014/11/04/rasterizing-perfect-canopy-height-models-from-lidar/
# accessed 5/10/2016
# By Tom Swinfield
# Script produced 17-01-31

# Script adapted by Laura Bentley 05/11/2019
# For the ID of ground points from a structure-from-motion derrived point cloud
# Using RGB fitering to remove vegetation and noise
# Info on processing photogrammetry can be found here:
#https://rapidlasso.com/2019/05/01/clean-dtm-from-agisoft-photogrammetric-points-of-urban-scene/
#https://rapidlasso.com/2018/12/27/scripting-lastools-to-create-a-clean-dtm-from-noisy-photogrammetric-point-cloud/
#https://rapidlasso.com/2016/11/22/creating-a-better-dtm-from-photogrammetic-points-by-avoiding-shadows/

rm(list=ls())


library(raster)
library(foreach)
library(doParallel)
library(lidR)
library(rgeos)
library(sp)

setwd("C:/Users/Laura/OneDrive - University Of Cambridge/Documents/Carbon Analysis/Data/")


# Funtions ----------------------------------------------------------------

make_dtm <- function(input, step, kill, out_path, bbtype, LAStools_path) {
  dir.create(out_path)
  
  # Make dtm
  system(paste(
    file.path(LAStools_path, "blast2dem"),
    "-i",
    input,
    "-keep_class 2",
    paste("-step", step),
    paste("-kill", kill),
    paste("-use_", bbtype, "_bb", sep = ""),
    paste("-odir", out_path),
    "-otif"
  ))
  return(NULL)
}

make_mosaic<-function(files, fun, na.rm, crs, filename){
  
  # Loads in all the rasters:
  myrasts<-lapply(files, function(x) raster(x))
  
  names(myrasts)[1:2] <- c('x', 'y')
  # names(myrasts)<-paste("r", 1:length(myrasts), sep="")
  myrasts$fun <- fun
  myrasts$na.rm <- na.rm
  myrasts_mosaic <- do.call(mosaic, myrasts)
  proj4string(myrasts_mosaic)<-crs
  
  #plot(myrasts_mosaic)
  
  writeRaster(myrasts_mosaic, filename, overwrite = TRUE)
  
  unlink(list.files(rasterOptions()$tmpdir, full.names = TRUE))
  rm(myrasts)
  return(myrasts_mosaic)
}

#_______________________________________

# Set up data  ----

#_______________________________________

input <-
  list.files("SFM_OrthostaticMap/orthomosaics/Laura_ES17_126-170-camera-20180222/point_cloud/las1.2/",
             full.names = TRUE,
             pattern = ".laz$"
  )

out_path <- "DTM"
dir.create(out_path)
LAStools_path <- "C:/Users/Laura/Documents/PHD_Programs/LAStools/LAStools/bin"

n_cores <- 4


#Index the tiles
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(input)[1], "*.laz"),
  paste("-cores", n_cores)
))


small_path <- file.path(dirname(input)[1], "small_tiles")
dir.create(small_path)

tiled_path <- file.path(small_path, "base_tiles")
dir.create(tiled_path)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(dirname(input)[1], "*.laz"),
    "-tile_size 500",
    "-buffer 20",
    "-epsg 25830",
    paste("-odir",tiled_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)

# Now index these:
input <-
  list.files(
    "SFM_OrthostaticMap/orthomosaics/Laura_ES17_126-170-camera-20180222/point_cloud/las1.2/small_tiles/base_tiles/",
    full.names = TRUE,
    pattern = ".laz$"
  )

# # Index the tiles
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(input)[1], "*.laz"),
  paste("-cores", n_cores)
))

#_____________________________________________________

# Classify Shadow Points 

#_____________________________________________________
shadow_path <- file.path(small_path, "DTM_1_shadow_R45G60B70")
dir.create(shadow_path)
#Record of shadows
#Pick colour thresholds based on careful inspection of point cloud

system(
  paste(
    file.path(LAStools_path, "las2las"),
     "-i",
    file.path(tiled_path, "*.laz"),
    # las2las only permits dropping / keeping points at this stage
    # all other points will be dropped
    # Setting a new classification can be useful when combining files together post ground point classification
    "-keep_RGB_red 0 11520",
    "-keep_RGB_green 0 14080",
    "-keep_RGB_blue 0 17920",
    "-set_classification 7",
    paste("-epsg 25830"),
    paste("-odir", shadow_path),
    paste("-odix", "shadow"),
    paste("-o",".laz"),
    paste("-cores", n_cores)
  )
)

noshadow_path <- file.path(small_path, "DTM_1_noshadow_R45G60B70")
dir.create(noshadow_path)

# Remove shadows (high noise) from files for classificaiton

system(
  paste(
    file.path(LAStools_path, "las2las"),
    "-i",
    file.path(tiled_path, "*.laz"),
    # las2las only permits dropping / keeping points at this stage
    # all other points will be dropped
    # Setting classification can be useful when combining files together post ground point classification
    "-drop_RGB_red 0 11520",
    "-drop_RGB_green 0 14080",
    "-drop_RGB_blue 0 17920",
    paste("-epsg 25830"),
    paste("-odir", noshadow_path),
    paste("-odix", "noshadow"),
    paste("-o",".laz"),
    paste("-cores", n_cores)
  )
)

#_____________________________________________________

# Remove vegetation

#_____________________________________________________

veg_path <- file.path(small_path, "DTM_2b_veg_G140_nsR45")
dir.create(veg_path)

system(
  paste(
    file.path(LAStools_path, "las2las"),
    "-i",
    file.path(tiled_path, "*.laz"),
    # las2las only permits dropping / keeping points at this stage
    # all other points will be dropped
    # Setting classification can be useful when combining files together post ground point classification
    "-keep_RGB_green 0 35840",
    paste("-epsg 25830"),
    paste("-odir", veg_path),
    paste("-odix", "veg"),
    paste("-o",".laz"),
    paste("-cores", n_cores)
  )
)

# Remove vegetation prior to classification
# How appropriate this is will vary with landscape

noveg_path <- file.path(small_path, "DTM_2b_novegG140_nsR45")
dir.create(noveg_path)

system(
  paste(
    file.path(LAStools_path, "las2las"),
    "-i",
    file.path(tiled_path, "*.laz"),
    # las2las only permits dropping / keeping points at this stage
    # all other points will be dropped
    # Setting classification can be useful when combining files together post ground point classification
    "-drop_RGB_green 0 35840",
    paste("-epsg 25830"),
    paste("-odir", noveg_path),
    paste("-odix", "noveg"),
    paste("-o",".laz"),
    paste("-cores", n_cores)
  )
)


#_____________________________________________________

# Thin points for DTM

#_____________________________________________________

thin_path <- file.path(small_path, "DTM_3b_thinned_0.5_step")
dir.create(thin_path)


system(
  paste(
    file.path(LAStools_path, "lasthin"),
    "-i",
    file.path(noveg_path, "*.laz"),
    "-step 0.5",
    # At a 0.5m resolution, give point at 50th percentile in height if a min of 20 points
    "-lowest",
    "-classify_as 8", #label these points as 8,
    paste("-epsg 25830"),
    paste("-odir", thin_path),
    paste("-odix", "_thinned"),
    paste("-o .laz"),
    paste("-cores", n_cores)
  )
)


#_____________________________________________________

# Label ground points for DSM

#_____________________________________________________

ground_path <- file.path(small_path, "DTM_4_ground30B1.5")
dir.create(ground_path)

system(
  paste(
    file.path(LAStools_path, "lasground_new"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(thin_path, "*.laz"),
    #"-wilderness", # Setting this to wilderness will actually reduce the step size to 3 m, which will
    # prevent low points from being detected.
    "-step 30",
    "-ultra_fine",
    # This is used to ensure that the algorithm works hard to find the low points
    # it is actually designed for use on steep slopes but my hunch is that it will work well
    # for dense forest
    "-drop_class 0",
    "-bulge 1.5",
    #"-spike 0.2",
    # "-buffered 20", # Use this line if the tiles need to be buffered on the fly
    "-remain_buffered",
    paste("-epsg 25830"),
    paste("-odir", ground_path),
    paste("-odix", "ground"),
    paste("-olaz"),
    paste("-cores", n_cores)
  )
)



#_______________________________________

# Remove buffers                    ----

#_______________________________________

dtm_nobuff_path <- file.path(small_path, "DTM_5_NoBuffer_b5")
dir.create(dtm_nobuff_path)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    "-i",
    file.path(ground_path, "*.laz"),
    "-remove_buffer",
    "-epsg 25830",
    paste("-odir", dtm_nobuff_path),
    paste("-odix","nobuff"),
    "-olaz",
    paste("-cores", n_cores)
  )
)

#_______________________________________

# Make DTM

#_______________________________________

input <- list.files(dtm_nobuff_path, full.names = TRUE, pattern = ".laz$")
#setwd(out_path) - not necessary for laura's set up

bbtype = "tile"

dtm_path <- file.path(out_path,"DTM_23_10_2019_k50_t500_b5")
dir.create(dtm_path)


make_dtm(
  input = file.path(dtm_nobuff_path, "*.laz"),
  step = 1,
  kill = 50,
  out_path = dtm_path,
  bbtype = bbtype, # bbtype not applicable because using tiles not made with lastools... i think
  LAStools_path = LAStools_path
)


# Mosaic the DTM
A<-list.files(dtm_path, pattern = ".tif$", full.names = TRUE)
chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
A<-chunker(A, 9)

Mosaicdtm_path <- file.path(out_path,"Mosaic_DTM_23_10_2019_k50_t500_b5")
dir.create(Mosaicdtm_path)

count<-1
mymosaics <- lapply(A, function(x) {
  make_mosaic(
    files = x,
    fun = mean,
    na.rm = TRUE,
    crs = CRS("+init=EPSG:25830"),
    filename = paste(Mosaicdtm_path,"/Sponforest_DTM_", count, ".tif", sep = "")
  )
  count<<-count+1
})

make_mosaic(
  files = list.files(Mosaicdtm_path, pattern = "Sponforest_DTM_[[:digit:]].tif$", full.names = TRUE),
  fun = mean,
  na.rm = TRUE,
  crs = CRS("+init=EPSG:25830"),
  filename = paste(Mosaicdtm_path,"/Sponforest_DTM.tif", sep = "")
)
