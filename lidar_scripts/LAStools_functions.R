# These functions create DTMs and CHMs from LiDAR data using Lastools with system calls
# They come from Tom Swinfield, updated by Laura Bentley

classify_ground_points=function(input_file_pattern,  out_path,step, epsg_code, n_cores, LAStools_path){
  system(
    paste(
      file.path(LAStools_path, "lasground_new"),
      # Using lasground_new sets the default step size to 25 m
      # This greatly improves true ground point detection
      # lasground has a default step size of 5 m
      "-i",
      input_file_pattern,
      paste("-odir", out_path),
      #paste0("-odix ","_ground"),
      "-olaz",
      #"-wilderness", # Setting this to wilderness will actually reduce the step size to 3 m, which will
      # prevent low points from being detected.
      paste("-step", step),
      "-extra_fine",
      # This is used to ensure that the algorithm works hard to find the low points
      # it is actually designed for use on steep slopes but my hunch is that it will work well
      # for dense forest
      # "-buffered 20", # Use this line if the tiles need to be buffered on the fly
      "-remain_buffered",
      "-compute_height",
      paste("-epsg", epsg_code),
      paste("-cores", n_cores)
  ))
}

make_dtm <- function(input_file_pattern, step, kill, out_path, bbtype, LAStools_path) {
  dir.create(out_path)
  
  # Make dtm
  system(paste(
    file.path(LAStools_path, "blast2dem"),
    "-i",
    input_file_pattern,
    "-keep_class 2", # these are the ground points
    paste("-step", step),
    paste("-kill", kill),
    paste("-use_", bbtype, "_bb", sep = ""),
    paste("-odir", out_path),
    "-otif"
  ))
  return(NULL)
}


make_dem <- function(input_file_pattern, step, out_path, bbtype, LAStools_path) {
  dir.create(out_path)
  system(paste(
    file.path(LAStools_path, "blast2dem"),
    "-i",
    input_file_pattern,
    #"-keep_class 2", # these are the ground points
    # -hillshade,
    paste("-step", step),
    paste("-use_", bbtype, "_bb", sep = ""),
    paste("-odir", out_path),
    "-otif"
  ))
  return(NULL)
}



# Thin the highest points in a laz file
thin_laz_highest=function(input_file_pattern, out_path, step, subcircle,LAStools_path){
  system(paste(
    file.path(LAStools_path, "lasthin"),
    "-i",
    input_file_pattern,
    paste("-subcircle", subcircle),
    paste("-step", step),
    "-highest",
    "-odir", out_path,
    "-olaz"
  ))
}

###################### Mosaicking
# Mosaic the rasters
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



mosaic_rasters=function(raster_path,
                        raster_resolution,
                        raster_output_name,
                        num_chunks,
                        epsg_code){
  
  A<-list.files(raster_path, pattern = ".tif$", full.names = TRUE)
  
  chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  A<-chunker(A, num_chunks)
  count<-1
  mymosaics <- lapply(A, function(x) {
    make_mosaic(
      files = x,
      fun = mean,
      na.rm = TRUE,
      crs = CRS(paste0("+init=EPSG:",epsg_code)),
      filename = paste(raster_path,raster_output_name,"_", count, ".tif", sep = "")
    )
    count<<-count+1
  })
  
  make_mosaic(
    files = list.files(raster_path, pattern = paste0(raster_resolution,"m_[[:digit:]].tif$"), full.names = TRUE),
    fun = mean,
    na.rm = TRUE,
    crs = CRS(paste0("+init=EPSG:",epsg_code)),
    filename = paste0(raster_path,raster_output_name,".tif")
  )
  
  
}



# FUNCTIONS THAT COMBINE THE ABOVE TO MAKE THE MAIN SCRIPT SHORTER

make_dsm_full=function(base_path,raster_output_name,raster_resolution,subcircle,epsg_code,LAStools_path){
  bbtype="tile"
  thin_path=file.path(base_path, "DSM_small_tiles/thin_highest"); dir.create(thin_path)
  DSM_folder=paste0(base_path,"DSM/"); dir.create(DSM_folder)
  DSM_raster_path=paste0(base_path,"DSM/sub",subcircle,"_",raster_resolution,"m/"); dir.create(DSM_raster_path)
  
  thin_laz_highest(input_file_pattern=paste0(base_path,"DSM_small_tiles/*.laz"),
                   out_path=thin_path, 
                   step=raster_resolution/4, subcircle=0.2,LAStools_path=LAStools_path)
  make_dem(
    input_file_pattern = file.path(thin_path, "*.laz"),
    step = raster_resolution,
    out_path = DSM_raster_path,
    bbtype = bbtype, # bbtype not applicable because using tiles not made with lastools... i think
    LAStools_path = LAStools_path)

}


make_dtm_full=function(base_path,raster_output_name,raster_resolution,tin_resolution,epsg_code,LAStools_path){
  bbtype="tile"#
  n_cores=4
  ground_path=file.path(base_path, "DSM_small_tiles/ground"); dir.create(ground_path)
  DTM_folder=paste0(base_path,"DTM/"); dir.create(DTM_folder)
  DTM_raster_path=paste0(base_path,"DTM/g",tin_resolution,"_",raster_resolution,"m/"); dir.create(DTM_raster_path)
  
  
  classify_ground_points(input_file_pattern=paste0(base_path,"DSM_small_tiles/*.laz"),  
                         out_path=ground_path, 
                         step=tin_resolution,
                         epsg_code,n_cores, LAStools_path)
  
  make_dtm(
    input_file_pattern = file.path(ground_path, "*.laz"),
    step = raster_resolution,
    kill = 50,
    out_path = DTM_raster_path,
    bbtype = bbtype, # bbtype not applicable because using tiles not made with lastools... i think
    LAStools_path = LAStools_path
  )
  
}



make_chm_full=function(base_path,raster_output_name,raster_resolution,tin_resolution,subcircle,h_seq,epsg_code,LAStools_path){
  bbtype="tile"#
  n_cores=4
  ground_path=file.path(base_path, "DSM_small_tiles/ground"); #dir.create(ground_path)
  chm_path = file.path(base_path, "DSM_small_tiles/chm");   dir.create(chm_path)
  CHM_folder=paste0(base_path,"CHM/"); dir.create(CHM_folder)
  CHM_raster_path=paste0(base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",raster_resolution,"m/"); dir.create(CHM_raster_path)
  tmp_laz_path=file.path(CHM_raster_path, "tmp_laz"); dir.create(tmp_laz_path)
  tmp_chm_path=file.path(CHM_raster_path, "tmp_chm"); dir.create(tmp_chm_path)
  
  system(paste(file.path(LAStools_path, "lasheight"),
               "-i",  file.path(ground_path, "*.laz"),
               paste("-drop_above", max(h_seq)),
               "-replace_z","-remain_buffered",
               paste("-odir", chm_path),"-olaz",
               paste("-cores", n_cores)
  ))
  
  chm_tiles_files=list.files(chm_path,full.names = TRUE, pattern = ".laz$")
  
  create_CHM_pitfree(chm_tiles_files,
                     raster_resolution,
                     h_seq,
                     subcircle,
                     kill=2, # ground
                     CHM_raster_path,
                     tmp_laz_path,
                     tmp_chm_path,
                     bbtype, # bbtype not applicable because using tiles not made with lastools... i think
                     LAStools_path)
}

do_everything=function(base_path,raster_output_name,raster_resolution,tin_resolution,subcircle,h_seq,epsg_code,LAStools_path){
  make_dsm_full(base_path=base_path,
                raster_output_name=raster_output_name,
                raster_resolution=raster_resolution,
                subcircle=subcircle,
                epsg_code=epsg_code,
                LAStools_path=LAStools_path)
  
  make_dtm_full(base_path=base_path,
                raster_output_name=raster_output_name,
                raster_resolution=raster_resolution,
                tin_resolution=tin_resolution,
                epsg_code=epsg_code,
                LAStools_path=LAStools_path)
  
  make_chm_full(base_path=base_path,
                raster_output_name=raster_output_name,
                raster_resolution=raster_resolution,
                tin_resolution=tin_resolution,
                subcircle=subcircle,
                h_seq=h_seq,
                epsg_code=epsg_code,
                LAStools_path=LAStools_path)
}

mosaic_all_rasters=function(base_path,raster_output_name,raster_resolution,tin_resolution,subcircle,epsg_code){
  DSM_raster_path=paste0(base_path,"DSM/sub",subcircle,"_",raster_resolution,"m/"); 
  DTM_raster_path=paste0(base_path,"DTM/g",tin_resolution,"_",raster_resolution,"m/"); 
  CHM_raster_path=paste0(base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",raster_resolution,"m/");
  mosaic_rasters(raster_path=DSM_raster_path,
                 raster_resolution=raster_resolution,
                 raster_output_name=paste0(raster_output_name,"_DSM_g",tin_resolution,"_sub",subcircle,"_",raster_resolution,"m"),
                 num_chunks=5,
                 epsg_code=epsg_code)
  
  mosaic_rasters(raster_path=DTM_raster_path,
                 raster_resolution=raster_resolution,
                 raster_output_name=paste0(raster_output_name,"_DTM_g",tin_resolution,"_sub",subcircle,"_",raster_resolution,"m"),
                 num_chunks=5,
                 epsg_code=epsg_code)
  mosaic_rasters(raster_path=CHM_raster_path,
                 raster_resolution=raster_resolution,
                 raster_output_name=paste0(raster_output_name,"_CHM_g",tin_resolution,"_sub",subcircle,"_",raster_resolution,"m"),
                 num_chunks=5,
                 epsg_code=epsg_code)
}

#######################################################
# Functions I'm not using
#######################################################
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

# Remove buffers                    ----
# I'M NOT USING THIS
#_______________________________________

#dtm_nobuff_path <- file.path(small_path, "DTM_NoBuffer")
#dir.create(dtm_nobuff_path)

#system(
#  paste(
#    file.path(LAStools_path, "lastile"),
#    "-i",
#    file.path(ground_path, "*.laz"),
#    "-remove_buffer",
#    paste("-epsg", epsg_code),
#    paste("-odir", dtm_nobuff_path),
#    paste("-odix","nobuff"),
#    "-olaz",
#    paste("-cores", n_cores)
#  )
#)