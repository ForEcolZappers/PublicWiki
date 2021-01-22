# This is a script which is coded up from the lastools tutorial 
# https://rapidlasso.com/2014/11/04/rasterizing-perfect-canopy-height-models-from-lidar/
# accessed 5/10/2016
# 
# The code will produce a series of space filling DEMs excluding which are used to produce very neat canopy 
# height model rasters.
#
#  by Tom Swinfield
#
# Script produced 17-01-31

rm(list=ls())


library(raster)
library(foreach)
library(doParallel)
library(lidR)
library(rgeos)
library(sp)
source("C:/Users/Forecol/ownCloud/LiDAR_canopy/R/Functions/CanProf_funs.R")
setwd("C:/Users/Laura/OneDrive - University Of Cambridge/Documents/Carbon Analysis/")


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

chm_pitfree <- function(input, step, h_seq, subcircle, kill, out_path, bbtype, LAStools_path) {
  tmp_laz_path<-file.path(out_path, "tmp_laz")
  tmp_chm_path<-file.path(out_path, "tmp_chm")
  
  #dir.create(tmp_laz_path)
  #dir.create(tmp_chm_path)
  
  foreach(i = input,
          .packages = c("raster"),
          .inorder = TRUE) %dopar% {
            #  for (i in input) {
            
            # Make ground layer
            system(paste(
              file.path(LAStools_path, "blast2dem"),
              "-i",
              i,
              paste("-drop_z_above", 0.1),
              paste("-step", step),
              paste("-kill", 20),
              paste("-use_", bbtype, "_bb", sep=""),
              "-o",
              file.path(tmp_chm_path, paste(
                gsub(".laz", "", basename(i)), "_chm_ground.bil", sep = ""
              ))
            ))
            
            
            # Thin the laz file
            system(paste(
              file.path(LAStools_path, "lasthin"),
              "-i",
              i,
              paste("-subcircle", subcircle),
              paste("-step", step / 2),
              "-highest",
              "-o",
              file.path(tmp_laz_path, paste(
                gsub(".laz", "", basename(i)), "_temp.laz", sep = ""
              ))
            ))
            
            # Seqentially build the CHM for each layer
            for (j in h_seq) {
              system(
                paste(
                  file.path(LAStools_path, "blast2dem"),
                  "-i",
                  file.path("tmp_laz", paste(
                    gsub(".laz", "", basename(i)), "_temp.laz", sep = ""
                  )),
                  paste("-drop_z_below", j),
                  paste("-step", step),
                  paste("-kill", kill),
                  paste("-use_", bbtype, "_bb", sep=""),
                  "-o",
                  file.path(tmp_chm_path, paste(
                    gsub(".laz", "", basename(i)), "_chm_", j, ".bil", sep = ""
                  ))
                )
              )
            }
            
            # Merge the layers
            
            system(
              paste(
                file.path(LAStools_path, "lasgrid"),
                "-i",
                file.path("tmp_chm", paste(
                  gsub(".laz", "", basename(i)), "_chm_*.bil", sep = ""
                )),
                "-merged",
                paste("-step", step),
                "-highest",
                #paste("-set_min_max", -2, max(h_seq)),
                paste("-drop_z_below", 0), # drop any points below 0
                paste("-drop_z_above", max(h_seq)), # drop any points above the max height
                # "-use_bb",
                "-o",
                file.path(out_path, paste(
                  gsub(".laz", "", basename(i)), "_r.tif", sep = ""
                ))
              )
            )
            # Clean up the temp files:
            system(paste("rm", file.path("tmp_chm", paste(
              gsub(".laz", "", basename(i)), "*", sep = ""
            ))))
            system(paste("rm", file.path("tmp_laz", paste(
              gsub(".laz", "", basename(i)), "*", sep = ""
            ))))
          }
  # remove temporary folders
  #system(paste("rm -rf", "tmp_laz"))
  #system(paste("rm -rf", "tmp_chm"))
  
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
  
  plot(myrasts_mosaic)
  
  writeRaster(myrasts_mosaic, filename, overwrite = TRUE)
  
  unlink(list.files(rasterOptions()$tmpdir, full.names = TRUE))
  rm(myrasts)
  return(myrasts_mosaic)
}




#_______________________________________

# Set up data  ----

#_______________________________________

input <-
  list.files("Data/.......",
    full.names = TRUE,
    pattern = ".las$"
  )

out_path <- "G:/Data/Thetford/2017/Processed/CHM_raster_step10m"
#dir.create(out_path)
LAStools_path <- "C:/Users/Laura/Documents/PHD_Programs/LAStools/LAStools/bin"

n_cores <- 4

# Index the tiles
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(input)[1], "*.las"),
  paste("-cores", n_cores)
))


small_path <- file.path(dirname(input)[1], "small_tiles")
dir.create(small_path)


system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(dirname(input)[1], "*.las"),
    "-tile_size 250",
    "-buffer 20",
    "-epsg 27700",
    paste("-odir", small_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)

# Now index these:
input <-
  list.files(
    "G:/Data/Thetford/2017/small_tiles",
    full.names = TRUE,
    pattern = ".laz$"
  )

# Index the tiles
system(paste(
  file.path(LAStools_path, "lasindex"),
  "-i",
  file.path(dirname(input)[1], "*.laz"),
  paste("-cores", n_cores)
))

#_______________________________________

# Classify ground in raw Las tiles  ----

#_______________________________________

ground_path <- file.path(dirname(input)[1], "ground")
dir.create(ground_path)


system(
  paste(
    file.path(LAStools_path, "lasground_new"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(dirname(input)[1], "*.laz"),
    #"-wilderness", # Setting this to wilderness will actually reduce the step size to 3 m, which will
    # prevent low points from being detected.
    "-step 5",
    "-extra_fine",
    # This is used to ensure that the algorithm works hard to find the low points
    # it is actually designed for use on steep slopes but my hunch is that it will work well
    # for dense forest
    # "-buffered 20", # Use this line if the tiles need to be buffered on the fly
    "-remain_buffered",
    paste("-epsg 27700"),
    paste("-odir", ground_path),
    paste("-o", "ground.laz"),
    paste("-cores", n_cores)
  )
)



#_______________________________________

# Normalize heights from ground     ----

#_______________________________________

chm_path <- file.path(dirname(input)[1], "chm")
dir.create(chm_path)

system(
  paste(
    file.path(LAStools_path, "lasheight"),
    "-i",
    file.path(ground_path, "*.laz"),
    paste("-drop_above", 60),
    # Set height to maximum expected height
    "-replace_z",
    # "-buffered 20", # Use this line if the tiles need to be buffered on the fly
    "-remain_buffered",
    paste("-odir", chm_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)


# system(paste(file.path(LAStools_path, "lasinfo"),
#              "-i",
#              i))

#_______________________________________

# Remove buffers                    ----

#_______________________________________

chm_nobuff_path <- file.path(dirname(input)[1], "chm_nobuff")
dir.create(chm_nobuff_path)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(chm_path, "*.laz"),
    "-remove_buffer",
    "-epsg 27700",
    paste("-odir", chm_nobuff_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)

#_______________________________________

# Make DTM                          ----

#_______________________________________


input <- list.files(chm_path, full.names = TRUE, pattern = ".laz$")
setwd(out_path)

step <- 0.5
h_seq <- c(2, seq(from = 5, to = 60, by = 5))
subcircle <- 0.1
kill <- 1.0
bbtype = "tile"

dtm_path <- file.path(out_path, "dtm")

make_dtm(
  input = file.path(ground_path, "*.laz"),
  step = 5,
  kill = 20,
  out_path = dtm_path,
  bbtype = bbtype, # bbtype not applicable because using tiles not made with lastools... i think
  LAStools_path = LAStools_path
)

# Mosaic the DTM
A<-list.files(dtm_path, pattern = ".tif$", full.names = TRUE)
chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
A<-chunker(A, 5)

count<-1
mymosaics <- lapply(A, function(x) {
  make_mosaic(
    files = x,
    fun = mean,
    na.rm = TRUE,
    crs = CRS("+init=EPSG:27700"),
    filename = paste("dtm/Thetford_DTM_10m_", count, ".tif", sep = "")
  )
  count<<-count+1
})

make_mosaic(
  files = list.files(dtm_path, pattern = "10m_[[:digit:]].tif$", full.names = TRUE),
  fun = mean,
  na.rm = TRUE,
  crs = CRS("+init=EPSG:27700"),
  filename = "dtm/Thetford_DTM_10m.tif"
)


#_______________________________________

# Make Pitfree CHM                  ----

#_______________________________________

input <- list.files(chm_path, full.names = TRUE, pattern = ".laz$")
out_path <- "G:/Data/Thetford/2017/Processed/CHM_raster_step10m"
setwd(out_path)


cl <- makeCluster(n_cores)
registerDoParallel(cl)
chm_pitfree(input,
            step,
            h_seq,
            subcircle,
            kill,
            out_path,
            bbtype, # bbtype not applicable because using tiles not made with lastools... i think
            LAStools_path)
stopCluster(cl)

# Mosaic the CHM

A<-list.files(out_path, pattern = ".tif$", full.names = TRUE)
chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
A<-chunker(A, 5)

count<-1
mymosaics <- lapply(A, function(x) {
  make_mosaic(
    files = x,
    fun = mean,
    na.rm = TRUE,
    crs = CRS("+init=EPSG:27700"),
    filename = paste("Thetford_CHM_10m_", count, ".tif", sep = "")
  )
  count<<-count+1
})

make_mosaic(
  files = list.files(out_path, pattern = "10m_[[:digit:]].tif$", full.names = TRUE),
  fun = mean,
  na.rm = TRUE,
  crs = CRS("+init=EPSG:27700"),
  filename = "Thetford_CHM_10m.tif"
)

unlink(rasterOptions()$tmpdir, recursive = TRUE) # Deletes the flightline temporary directory

