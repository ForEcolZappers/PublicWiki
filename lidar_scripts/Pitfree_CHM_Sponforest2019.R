# This is a script which is coded up from the lastools tutorial 
# https://rapidlasso.com/2014/11/04/rasterizing-perfect-canopy-height-models-from-lidar/
# accessed 5/10/2016
# 
# The code will produce a series of space filling DEMs excluding which are used to produce very neat canopy 
# height model rasters.
# by Tom Swinfield
# Script produced 17-01-31

# Script adapted by Laura Bentley 05/11/2019
# To use ground points IDed by the Pitfree_DTM_Sponforest2019 script from a structure-from-motion derrived point cloud
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
library(stringr)

setwd("C:/Users/Laura/OneDrive - University Of Cambridge/Documents/Carbon Analysis/Data/")


# Functions ---------------------------------------------------------------

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
  
  dir.create(tmp_laz_path)
  dir.create(tmp_chm_path)
  
  foreach(i = input,
          .packages = c("raster"),
          .inorder = TRUE) %dopar% {
            #  for (i in input) {
            
            # Make ground layer
            system(paste(
              file.path(LAStools_path, "blast2dem"),
              "-i",
              i,
              paste("-keep_class", 2),
              paste("-step", step),
              paste("-kill", 40),
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
            ## add code to drop green pixels 
            for (j in h_seq) {
              system(
                paste(
                  file.path(LAStools_path, "blast2dem"),
                  "-i",
                  file.path(paste(out_path,"tmp_laz",sep="/"), paste(
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
                file.path(paste(out_path,"tmp_chm",sep="/"), paste(
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
            system(paste("rm", file.path(tmp_chm_path, paste(
              gsub(".laz", "", basename(i)), "*", sep = ""
            ))))
            system(paste("rm", file.path(tmp_laz_path, paste(
              gsub(".laz", "", basename(i)), "*", sep = ""
            ))))
          }
  # remove temporary folders
  system(paste("rm -rf", "tmp_laz"))
  system(paste("rm -rf", "tmp_chm"))
  
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

pointrestore <- function(path_A,path_B,out_path,LAStools_path,char){
  
  small_Input_A <- unlist(lapply(list.files(path_A,full.names = F,pattern = ".laz$"),substr,start = 1,stop = char))
  
  Input_A <- list.files(path_A,full.names = T,pattern = ".laz$")
  Input_B <- list.files(path_B,full.names = T,pattern = ".laz$")
  
  tmp_path <- file.path(out_path,"tmp_pairs")        
  dir.create(tmp_path)
  
  cat('Set-up complete!\n')
  
  foreach(i = small_Input_A,
          .packages = c("raster"),
          .inorder = TRUE) %dopar% {
          
    A <- Input_A[grep(pattern = i,x = Input_A)]
    B <- Input_B[grep(pattern = i,x = Input_B)]
    
    
    file.copy(A,tmp_path)
    file.copy(B,tmp_path)
   
    cat('Files copied\n')
    
    system(
      paste(
        file.path(LAStools_path,"lasmerge"), # recombine veg and ground points
        "-i",
        file.path(tmp_path, "*.laz"),
        paste("-odir",out_path),
        paste("-odix",paste(i,"merged",sep="_")),
        paste("-o",".laz")
        #paste('-cores',n_cores)
      )
    )
    
    cat('Files Merged\n')
    
    file.remove(list.files(path = tmp_path,full.names = T))
    
  }
  
}

copyclassification <- function(path_A,path_B,out_path,LAStools_path){
  
  small_Input_A <- paste(str_split(list.files(path_A,full.names = F,pattern = ".laz$"),pattern = "_",simplify = T)[,1],
                         str_split(list.files(path_A,full.names = F,pattern = ".laz$"),pattern = "_",simplify = T)[,2],
                         sep = "_")
  
  Input_A <- list.files(path_A,full.names = T,pattern = ".laz$")
  Input_B <- list.files(path_B,full.names = T,pattern = ".laz$")
  
  cat('Set-up complete!')
  
  foreach(i = small_Input_A,
          .packages = c("raster"),
          .inorder = TRUE) %dopar% {
            
            A <- Input_A[grep(pattern = i,x = Input_A)]
            B <- Input_B[grep(pattern = i,x = Input_B)]
            
           system(
              paste(
                file.path(LAStools_path,"lascopy"), # recombine veg and ground points
                "-i",A,
                "-i",B,
                "-classification",
                paste("-odir",out_path),
                paste("-odix",paste(i,"merged",sep="_")),
                paste("-o",".laz")
                #paste('-cores',n_cores)
              )
            )
            
            
          }
  
}

#_______________________________________

# Set up data  ----

#_______________________________________

input <-
  list.files("SFM_OrthostaticMap/orthomosaics/Laura_ES17_126-170-camera-20180222/point_cloud/las1.2/",
    full.names = TRUE,
    pattern = ".laz$"
  )


#dir.create(out_path)
LAStools_path <- "C:/Users/Laura/Documents/PHD_Programs/LAStools/LAStools/bin"

n_cores <- 4


# Index the tiles
# system(paste(
#   file.path(LAStools_path, "lasindex"),
#   "-i",
#   file.path(dirname(input)[1], "*.laz"),
#   paste("-cores", n_cores)
# ))


small_path <- file.path(dirname(input)[1], "small_tiles")
tiled_path <- file.path(small_path, "base_tiles")
#dir.create(small_path)

# 
# system(
#   paste(
#     file.path(LAStools_path, "lastile"),
#     # Using lasground_new sets the default step size to 25 m
#     # This greatly improves true ground point detection
#     # lasground has a default step size of 5 m
#     "-i",
#     file.path(dirname(input)[1], "*.laz"),
#     "-tile_size 250",
#     "-buffer 20",
#     "-epsg 25830",
#     paste("-odir", small_path),
#     "-olaz",
#     paste("-cores", n_cores)
#   )
# )

# Now index these:
# input <-
#   list.files(
#     "SFM_OrthostaticMap/orthomosaics/Laura_ES17_126-170-camera-20180222/point_cloud/las1.2/small_tiles/base_tiles/",
#     full.names = TRUE,
#     pattern = ".laz$"
#   )

# # Index the tiles
# system(paste(
#   file.path(LAStools_path, "lasindex"),
#   "-i",
#   file.path(dirname(input)[1], "*.laz"),
#   paste("-cores", n_cores)
# ))

#_____________________________________________________

# Add veg points to ground points from DTP

#_____________________________________________________


ground_path <- file.path(small_path,"DTM_4_ground30B1")
veg_path <- file.path(small_path, "DTM_2_veg_G140_nsR45")
shadow_path <- file.path(small_path, "DTM_1_shadow_R45G60B70")


# Remove buffer from ground and veg paths
# Buffered tiles produce error when merged
ground_nobuff_path <- file.path(small_path, "CHM_1_groundnobuffer")
dir.create(ground_nobuff_path)

veg_nobuff_path <- file.path(small_path, "CHM_1_vegnobuffer")
dir.create(veg_nobuff_path)

# shadow_nobuff_path <- file.path(small_path, "CHM_1_shadownobuffer")
# dir.create(shadow_nobuff_path)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(ground_path, "*.laz"),
    "-remove_buffer",
    "-epsg 25830",
    paste("-odir", ground_nobuff_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(veg_path, "*.laz"),
    "-remove_buffer",
    "-epsg 25830",
    paste("-odir", veg_nobuff_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)



merged_path <- file.path(small_path, "CHM_2_Merged")
dir.create(merged_path)

# Combine classified GPs and 'vegetation' points

pointrestore(path_A = ground_nobuff_path,path_B = veg_nobuff_path,
             out_path = merged_path,LAStools_path = LAStools_path)



# Re-tile! 
# Merging drops tile info

tiled_merge_path <- file.path(small_path, "CHM_3_tiledmerged")
dir.create(tiled_merge_path)

system(
  paste(
    file.path(LAStools_path, "lastile"),
    # Using lasground_new sets the default step size to 25 m
    # This greatly improves true ground point detection
    # lasground has a default step size of 5 m
    "-i",
    file.path(merged_path, "*.laz"),
    "-tile_size 500",
    "-epsg 25830",
    paste("-odir", tiled_merge_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)



#_______________________________________

# Normalize heights from ground     ----

#_______________________________________

normalised_path <- file.path(small_path, "CHM_4_normalised")
dir.create(normalised_path)

system(
  paste(
    file.path(LAStools_path, "lasheight"),
    "-i",
    file.path(tiled_merge_path, "*.laz"),
    paste("-drop_above", 50),
    # Set height to maximum expected height
    "-replace_z",
    # "-buffered 20", # Use this line if the tiles need to be buffered on the fly
    "-remain_buffered",
    paste("-odir",normalised_path),
    "-olaz",
    paste("-cores", n_cores)
  )
)


#_______________________________________

# Make Pitfree CHM                  ----

#_______________________________________

#out_path <- "CHM"

step <- 1
h_seq <- c(seq(from = 0.5, to = 20, by = 0.5),seq(from = 20,to = 40,by = 1))
subcircle <- 0.2
kill <- 2.0
bbtype = "tile"

input <- list.files(normalised_path, full.names = TRUE, pattern = ".laz$")
out_path <- "CHM/CHM_24_10_2019_k2_s2"
dir.create(out_path)
#setwd(out_path)

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
out_path <- "CHM/Mosaic_CHM_24_10_2019_k2_s2"
dir.create(out_path)

count<-1
mymosaics <- lapply(A, function(x) {
  make_mosaic(
    files = x,
    fun = mean,
    na.rm = TRUE,
    crs = CRS("+init=EPSG:25830"),
    filename = paste(out_path,"/Sponforest_CHM_", count, ".tif", sep = "")
  )
  count<<-count+1
})

make_mosaic(
  files = list.files(out_path, pattern = "CHM_[[:digit:]].tif$", full.names = TRUE),
  fun = mean,
  na.rm = TRUE,
  crs = CRS("+init=EPSG:25830"),
  filename = paste(out_path,"/Sponforest_CHM.tif",sep="")
)

unlink(rasterOptions()$tmpdir, recursive = TRUE) # Deletes the flightline temporary directory

