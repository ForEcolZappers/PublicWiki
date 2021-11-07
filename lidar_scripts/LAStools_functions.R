# These functions create DTMs and CHMs from LiDAR data using Lastools with system calls
# They come from Tom Swinfield, updated by Laura Bentley
# Order of functions in this script

# RESAMPLE LAS DATA
#1   create_small_tiles 

# MAKING DSM AND DTM
#2.1 make_dsm_full
#2.2 make_dtm_full
#2.3 mosaic_DSM_DTM_CCCHM

# MAKING CHM
#3.1 make_chm_las_files
#3.2 make_pitfree_chm_tiled_rasters
#3.3 mosaic_CHM_rasters

# TOOLS FOR MOSAICS
#4.1 make_mosaic
#4.2 mosaic_rasters

# TOOLS FOR LAS PROCESSING
#5.1 classify_ground_points
#5.2 make_dtm
#5.3 make_dem
#5.4 thin_laz_highest

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#1 Create tiles ###############
create_small_tiles=function(df,large_tiles_folder,tile_size,buffer_size){
  # This function resamples the original point cloud to smaller tiles with buffers
  # This helps with storage and processing and consistency across sites.
  # The small tiles are saved in a folder called DSM_small_tiles, and other functions rely on this naming system
  small_tiles_path = file.path(df$base_path, "DSM_small_tiles")
  dir.create(small_tiles_path)
  
  # Index the large tiles - creates the .lax files
  system(paste(
    file.path(df$LAStools_path, "lasindex"),
    "-i",
    file.path(paste0(df$base_path,large_tiles_folder), df$file_type),
    paste("-cores", df$n_cores)
  ))
  
  #Create small tiles 
  system(
    paste(
      file.path(df$LAStools_path, "lastile"),
      "-i",
      file.path(paste0(df$base_path,large_tiles_folder), df$file_type),
      paste("-tile_size",tile_size),
      paste("-buffer",buffer_size),
      paste("-epsg", df$epsg_code),
      paste("-odir", small_tiles_path),
      "-olaz",
      paste("-cores", df$n_cores)
    )
  )
  
  # Index the small tiles
  system(paste(
    file.path(df$LAStools_path, "lasindex"),
    "-i",
    file.path(small_tiles_path, "*.laz"),
    paste("-cores", df$n_cores)
  ))
  
}

#2.1
make_dsm_full=function(df,subcircle){
 
  thin_path=file.path(df$base_path, "DSM_small_tiles/thin_highest"); dir.create(thin_path)
  DSM_folder=paste0(df$base_path,"DSM/"); dir.create(DSM_folder)
  DSM_raster_path=paste0(df$base_path,"DSM/sub",subcircle,"_",df$raster_resolution,"m/"); dir.create(DSM_raster_path)
  
  thin_laz_highest(input_file_pattern=paste0(df$base_path,"DSM_small_tiles/",df$file_type),
                   out_path=thin_path, 
                   step=df$raster_resolution/4, subcircle=subcircle,LAStools_path=df$LAStools_path)
  make_dem(
    input_file_pattern = file.path(thin_path, "*.laz"),
    step = df$raster_resolution,
    out_path = DSM_raster_path,
    bbtype = df$bbtype, # bbtype not applicable because using tiles not made with lastools... i think
    LAStools_path = df$LAStools_path)
  
}

make_random_dsm_full=function(df,subcircle,num_chunks){
  
  thin_path=file.path(df$base_path, "DSM_small_tiles/thin_random"); dir.create(thin_path)
  DSM_folder=paste0(df$base_path,"DSM/"); dir.create(DSM_folder)
  DSM_raster_path=paste0(df$base_path,"DSM/random_sub_",subcircle,"_",df$raster_resolution,"m/"); dir.create(DSM_raster_path)
  
  
  # Thin the highest points in a laz file
  system(paste(
    file.path(df$LAStools_path, "lasthin"),
    "-i",
    paste0(df$base_path,"DSM_small_tiles/",df$file_type),
    paste("-step", df$raster_resolution),
    paste("-subcircle", subcircle),
    "-first_only -random",
    "-odir", thin_path,
    "-olaz"
  ))
  
  make_dem(
    input_file_pattern = file.path(thin_path, "*.laz"),
    step = df$raster_resolution,
    out_path = DSM_raster_path,
    bbtype = df$bbtype, 
    LAStools_path = df$LAStools_path)
  
  mosaic_rasters2(raster_input_path=DSM_raster_path,
                  raster_output_path=df$base_path,
                  raster_resolution=df$raster_resolution,
                  raster_output_name=paste0(df$raster_output_name,"_DSM_random_sub_",subcircle,"_",df$raster_resolution,"m"),
                  num_chunks=num_chunks,
                  epsg_code=df$epsg_code)
  
  DSM=raster(paste0(df$base_path,df$raster_output_name,"_DSM_random_sub_",subcircle,"_",df$raster_resolution,"m.tif"))
  return(DSM)
  
}

#2.2
make_dtm_full=function(df,tin_resolution){
  ground_path=file.path(df$base_path, "DSM_small_tiles/ground"); dir.create(ground_path)
  DTM_folder=paste0(df$base_path,"DTM/"); dir.create(DTM_folder)
  DTM_raster_path=paste0(df$base_path,"DTM/g",tin_resolution,"_",df$raster_resolution,"m/"); dir.create(DTM_raster_path)
  
  
  classify_ground_points(input_file_pattern=paste0(df$base_path,"DSM_small_tiles/",df$file_type),  
                         out_path=ground_path, 
                         step=tin_resolution,
                         df$epsg_code,df$n_cores, df$LAStools_path)
  
  make_dtm(
    input_file_pattern = file.path(ground_path, "*.laz"),
    step = df$raster_resolution,
    kill = 50,
    out_path = DTM_raster_path,
    bbtype = df$bbtype, # bbtype not applicable because using tiles not made with lastools... i think
    LAStools_path = df$LAStools_path
  )
  
}

#2.3
mosaic_DSM_DTM_CCCHM=function(df,tin_resolution,subcircle,num_chunks){
  DSM_raster_path=paste0(df$base_path,"DSM/sub",subcircle,"_",df$raster_resolution,"m/"); 
  DTM_raster_path=paste0(df$base_path,"DTM/g",tin_resolution,"_",df$raster_resolution,"m/"); 
  CHM_raster_path=paste0(df$base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/"); 
  dir.create(paste0(df$base_path,"CHM")); dir.create(CHM_raster_path)
  
  mosaic_rasters2(raster_input_path=DSM_raster_path,
                 raster_output_path=df$base_path,
                 raster_resolution=df$raster_resolution,
                 raster_output_name=paste0(df$raster_output_name,"_DSM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m"),
                 num_chunks=num_chunks,
                 epsg_code=df$epsg_code)
  
  mosaic_rasters2(raster_input_path=DTM_raster_path,
                 raster_output_path=df$base_path,
                 raster_resolution=df$raster_resolution,
                 raster_output_name=paste0(df$raster_output_name,"_DTM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m"),
                 num_chunks=num_chunks,
                 epsg_code=df$epsg_code)
  
  DSM=raster(paste0(df$base_path,df$raster_output_name,"_DSM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m.tif"))
  DTM=raster(paste0(df$base_path,df$raster_output_name,"_DTM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m.tif"))
  CCCHM=DSM-DTM
  writeRaster(CCCHM, filename=paste0(df$base_path,df$raster_output_name,"_CCCHM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m.tif"), format="GTiff")
  
  return(CCCHM)
}

#2.4
make_pulse_density_raster_full=function(df,num_chunks){
  
  pulse_density_raster_path=paste0(df$base_path,"DSM_small_tiles/pd_",df$raster_resolution,"m/"); 
  dir.create(pulse_density_raster_path)
  
  system(paste(
    file.path(df$LAStools_path, "lasgrid"),
    "-i",
    file.path(paste0(df$base_path,"DSM_small_tiles/", "*.laz")),
    "-keep_first", 
    paste("-step", df$raster_resolution),
    "-point_density",
    paste("-use_", df$bbtype, "_bb", sep = ""),
    paste("-odir", pulse_density_raster_path),
    paste("-epsg", df$epsg_code),
    paste("-cores", df$n_cores),
    "-otif"
  ))
  
  mosaic_rasters2(raster_input_path=pulse_density_raster_path,
                  raster_output_path=df$base_path,
                  raster_resolution=df$raster_resolution,
                  raster_output_name=paste0(df$raster_output_name,"_pd_",df$raster_resolution,"m"),
                  num_chunks=num_chunks,
                  epsg_code=df$epsg_code)
  
}

#2.4
make_point_density_raster_full=function(df,num_chunks){
  
  point_density_raster_path=paste0(df$base_path,"DSM_small_tiles/ptd_",df$raster_resolution,"m/"); 
  dir.create(point_density_raster_path)
  
  system(paste(
    file.path(df$LAStools_path, "lasgrid"),
    "-i",
    file.path(paste0(df$base_path,"DSM_small_tiles/", "*.laz")),
    #"-keep_first", 
    paste("-drop_classification 2"),
    paste("-step", df$raster_resolution),
    "-point_density",
    paste("-use_", df$bbtype, "_bb", sep = ""),
    paste("-odir", point_density_raster_path),
    paste("-epsg", df$epsg_code),
    paste("-cores", df$n_cores),
    "-otif"
  ))
  
  mosaic_rasters2(raster_input_path=point_density_raster_path,
                  raster_output_path=df$base_path,
                  raster_resolution=df$raster_resolution,
                  raster_output_name=paste0(df$raster_output_name,"_ptd_",df$raster_resolution,"m"),
                  num_chunks=num_chunks,
                  epsg_code=df$epsg_code)
  
}
# check vertical allignmetd DTM-DTM
################


###################### CHM functions
#3.1
make_chm_las_files=function(df,tin_resolution,subcircle,h_seq){
  ground_path=file.path(df$base_path, "DSM_small_tiles/ground"); #dir.create(ground_path)
  chm_path = file.path(df$base_path, "DSM_small_tiles/chm");   dir.create(chm_path)
  CHM_folder=paste0(df$base_path,"CHM/"); dir.create(CHM_folder)
  CHM_raster_path=paste0(df$base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/"); dir.create(CHM_raster_path)
  tmp_laz_path=paste0(CHM_raster_path, "tmp_laz"); dir.create(tmp_laz_path)
  tmp_chm_path=paste0(CHM_raster_path, "tmp_chm"); dir.create(tmp_chm_path)
  
  system(paste(file.path(df$LAStools_path, "lasheight"),
               "-i",  file.path(ground_path, "*.laz"),
               paste("-drop_above", max(h_seq)),
               "-replace_z","-remain_buffered",
               paste("-odir", chm_path),"-olaz",
               paste("-cores", df$n_cores)
  ))
  
  # create the tmp chm ground files
  system(paste(
    file.path(df$LAStools_path, "blast2dem"),
    "-i",file.path(chm_path, "*.laz"),
    paste("-keep_class", 2),
    paste("-step", df$raster_resolution),
    #paste("-kill", 40),
    paste("-use_", df$bbtype, "_bb", sep=""),
    paste("-odir", tmp_chm_path),
    paste("-odix", '_chm_ground.bil'),
    #"-o",
    #file.path(tmp_chm_path, paste(
    #  gsub(".laz", "", basename(i)), "_chm_ground.bil", sep = ""
    #)),
    paste("-cores", df$n_cores)
  ))
  
  
  # Thin the laz file
  system(paste(
    file.path(df$LAStools_path, "lasthin"),
    "-i",file.path(chm_path, "*.laz"),
    paste("-subcircle", subcircle),
    paste("-step",df$raster_resolution / 4),
    "-highest",
    paste("-odir", tmp_laz_path),
    paste("-odix", '_chm_thin'),'-olaz',
    #"-o",
    #file.path(tmp_laz_path, paste(
    #  gsub(".laz", "", basename(i)), "_temp.laz", sep = ""
    #)),
    paste("-cores", df$n_cores)
  ))
  
}

#3.2
make_pitfree_chm_tiled_rasters=function(df,tin_resolution,subcircle,h_seq){
  ground_path=file.path(df$base_path, "DSM_small_tiles/ground"); #dir.create(ground_path)
  chm_path = file.path(df$base_path, "DSM_small_tiles/chm");   #dir.create(chm_path)
  CHM_folder=paste0(df$base_path,"CHM/"); #dir.create(CHM_folder)
  CHM_raster_path=paste0(df$base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/"); #dir.create(CHM_raster_path)
  tmp_laz_path=paste0(CHM_raster_path, "tmp_laz"); #dir.create(tmp_laz_path)
  tmp_chm_path=paste0(CHM_raster_path, "tmp_chm"); #dir.create(tmp_chm_path)
  
  chm_tiles_files=list.files(chm_path,full.names = TRUE, pattern = ".laz$")
  cl <- makeCluster(4)
  registerDoParallel(cl)
  foreach(i = chm_tiles_files,.packages = c("raster"),.inorder = TRUE) %dopar% {
  #for (i in chm_tiles_files) {
    print(i)
    #i=chm_tiles_files[1]
    # get the maximum height in the file
    #CHM=readLAS("C:/Users/User/Documents/Data/ALS_Data/Tanzania_ALS/2014//DSM_small_tiles/chm/1032500_8883750.laz")
    
    # Sequentially build the CHM for each layer
    ## add code to drop green pixels 
    for (j in h_seq) {
      print(j)
      system(
        paste(
          file.path(df$LAStools_path, "blast2dem"),
          "-i",
          file.path(tmp_laz_path, 
                    paste(gsub(".laz", "", basename(i)), 
                          "_chm_thin.laz", sep = "")
          ),
          paste("-drop_z_below", j),
          paste("-step",df$raster_resolution),
          paste("-kill", 2),
          paste("-use_", df$bbtype, "_bb", sep=""),
          #paste("-cores", df$n_cores),
          "-o",
          file.path(tmp_chm_path, paste(
            gsub(".laz", "", basename(i)), "_chm_", j, ".bil", sep = ""
          ))
        )
      )
    }
    
    #print(rep('Done with the loop over h_seq',10000))
    # Merge the layers
    print(i)
    system(
      paste(
        file.path(df$LAStools_path, "lasgrid"),
        "-i",
        file.path(tmp_chm_path, 
                  paste(gsub(".laz", "", basename(i)), 
                        "_chm_*.bil", sep = "")
        ),
        "-merged",
        paste("-step", df$raster_resolution),
        "-highest",
        #paste("-set_min_max", -2, max(h_seq)),
        paste("-drop_z_below", 0), # drop any points below 0
        paste("-drop_z_above", max(h_seq)), # drop any points above the max height
        # "-use_bb",
        #paste("-cores", df$n_cores),
        "-o",
        file.path(CHM_raster_path, paste(
          gsub(".laz", "", basename(i)), "_r.tif", sep = ""
        ))
      )
    )
    
  }
  #stopCluster(cl)
}

#3.3
mosaic_CHM_rasters=function(df,tin_resolution,subcircle,num_chunks){
  CHM_raster_path=paste0(df$base_path,"CHM/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/");
  
  mosaic_rasters2(raster_input_path=CHM_raster_path,
                 raster_output_path=df$base_path,
                 raster_resolution=df$raster_resolution,
                 raster_output_name=paste0(df$raster_output_name,"_CHM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m"),
                 num_chunks=num_chunks,
                 epsg_code=df$epsg_code)
  CHM=raster(paste0(df$base_path,df$raster_output_name,"_CHM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m.tif"))
  return(CHM)
}

make_thin_chm_las_files=function(df,tin_resolution,subcircle,h_seq,pd){
  chm_path = file.path(df$base_path, "DSM_small_tiles/chm");  # dir.create(chm_path)
  CHM_folder=paste0(df$base_path,"CHM_thin/"); dir.create(CHM_folder)
  CHM_raster_path=paste0(df$base_path,"CHM_thin/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/"); dir.create(CHM_raster_path)
  tmp_laz_thin1_path=paste0(CHM_raster_path, "tmp_laz_thin1"); dir.create(tmp_laz_thin1_path)
  tmp_laz_thin2_path=paste0(CHM_raster_path, "tmp_laz_thin2"); dir.create(tmp_laz_thin2_path)
  
  tmp_chm_path=paste0(CHM_raster_path, "tmp_chm_thin"); dir.create(tmp_chm_path)

  
  # create the tmp chm ground files
  system(paste(
    file.path(df$LAStools_path, "blast2dem"),
    "-i",file.path(chm_path, "*.laz"),
    paste("-keep_class", 2),
    paste("-step", df$raster_resolution),
    #paste("-kill", 40),
    paste("-use_", df$bbtype, "_bb", sep=""),
    paste("-odir", tmp_chm_path),
    paste("-odix", '_chm_ground.bil'),
    #"-o",
    #file.path(tmp_chm_path, paste(
    #  gsub(".laz", "", basename(i)), "_chm_ground.bil", sep = ""
    #)),
    paste("-cores", df$n_cores)
  ))
  
  # Thin the laz file to required point density - RANDOM
  system(paste(
    file.path(df$LAStools_path, "lasthin"),
    "-i",file.path(chm_path, "*.laz"),
    paste("-step",1/pd),
    "-random",
    paste("-odir", tmp_laz_thin1_path),
    paste("-odix", '_chm_thin'),
    '-olaz',
    paste("-cores", df$n_cores)
  ))
  
  # Thin the laz file to highest per quarter cell
  system(paste(
    file.path(df$LAStools_path, "lasthin"),
    "-i",file.path(tmp_laz_thin1_path, "*.laz"),
    paste("-subcircle", subcircle),
    paste("-step",df$raster_resolution / 4),
    "-highest",
    paste("-odir", tmp_laz_thin2_path),
    paste("-odix", '_chm_thin'),'-olaz',
    #"-o",
    #file.path(tmp_laz_path, paste(
    #  gsub(".laz", "", basename(i)), "_temp.laz", sep = ""
    #)),
    paste("-cores", df$n_cores)
  ))
  
}

#3.2
make_pitfree_thin_chm_tiled_rasters=function(df,tin_resolution,subcircle,h_seq){
  ground_path=file.path(df$base_path, "DSM_small_tiles/ground"); #dir.create(ground_path)
  chm_path = file.path(df$base_path, "DSM_small_tiles/chm");   #dir.create(chm_path)
  CHM_folder=paste0(df$base_path,"CHM_thin/"); #dir.create(CHM_folder)
  CHM_raster_path=paste0(df$base_path,"CHM_thin/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/"); #dir.create(CHM_raster_path)
  tmp_laz_path=paste0(CHM_raster_path, "tmp_laz_thin2"); #dir.create(tmp_laz_path)
  tmp_chm_path=paste0(CHM_raster_path, "tmp_chm_thin"); #dir.create(tmp_chm_path)
  
  chm_tiles_files=list.files(chm_path,full.names = TRUE, pattern = ".laz$")
  cl <- makeCluster(4)
  registerDoParallel(cl)
  foreach(i = chm_tiles_files,.packages = c("raster"),.inorder = TRUE) %dopar% {
    #for (i in chm_tiles_files) {
    print(i)
    #i=chm_tiles_files[1]

    # Sequentially build the CHM for each layer
    ## add code to drop green pixels 
    for (j in h_seq) {
      print(j)
      system(
        paste(
          file.path(df$LAStools_path, "blast2dem"),
          "-i",
          file.path(tmp_laz_path, 
                    paste(gsub(".laz", "", basename(i)), 
                          "_chm_thin_chm_thin.laz", sep = "")
          ),
          paste("-drop_z_below", j),
          paste("-step",df$raster_resolution),
          paste("-kill", 2),
          paste("-use_", df$bbtype, "_bb", sep=""),
          #paste("-cores", df$n_cores),
          "-o",
          file.path(tmp_chm_path, paste(
            gsub(".laz", "", basename(i)), "_chm_", j, ".bil", sep = ""
          ))
        )
      )
    }
    
    #print(rep('Done with the loop over h_seq',10000))
    # Merge the layers
    print(i)
    system(
      paste(
        file.path(df$LAStools_path, "lasgrid"),
        "-i",
        file.path(tmp_chm_path, 
                  paste(gsub(".laz", "", basename(i)), 
                        "_chm_*.bil", sep = "")
        ),
        "-merged",
        paste("-step", df$raster_resolution),
        "-highest",
        #paste("-set_min_max", -2, max(h_seq)),
        paste("-drop_z_below", 0), # drop any points below 0
        paste("-drop_z_above", max(h_seq)), # drop any points above the max height
        # "-use_bb",
        #paste("-cores", df$n_cores),
        "-o",
        file.path(CHM_raster_path, paste(
          gsub(".laz", "", basename(i)), "_r.tif", sep = ""
        ))
      )
    )
    
  }
  #stopCluster(cl)
}

#3.3
mosaic_thin_CHM_rasters=function(df,tin_resolution,subcircle,num_chunks){
  CHM_raster_path=paste0(df$base_path,"CHM_thin/g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m/");
  
  mosaic_rasters2(raster_input_path=CHM_raster_path,
                  raster_output_path=df$base_path,
                  raster_resolution=df$raster_resolution,
                  raster_output_name=paste0(df$raster_output_name,"_CHM_thin_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m"),
                  num_chunks=num_chunks,
                  epsg_code=df$epsg_code)
  CHM=raster(paste0(df$base_path,df$raster_output_name,"_CHM_g",tin_resolution,"_sub",subcircle,"_",df$raster_resolution,"m.tif"))
  return(CHM)
}

###################### Mosaicing functions
#4.1
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

#4.2
mosaic_rasters=function(raster_input_path,raster_output_path, raster_resolution, raster_output_name, num_chunks, epsg_code){
  # This one has raster_output_path but is unused
  A<-list.files(raster_input_path, pattern = ".tif$", full.names = TRUE)
  
  chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  B<-chunker(A, num_chunks)
  #C=B[[1]]
  count<-1
  mymosaics <- lapply(B, function(x) {
    make_mosaic(
      files = x,
      fun = mean,
      na.rm = TRUE,
      crs = CRS(paste0("+init=EPSG:",epsg_code)),
      filename = paste(raster_input_path,raster_output_name,"_", count, ".tif", sep = "")
    )
    count<<-count+1
  })
  
  make_mosaic(
    files = list.files(raster_input_path, pattern = paste0(raster_resolution,"m_[[:digit:]].tif$"), full.names = TRUE),
    fun = mean,
    na.rm = TRUE,
    crs = CRS(paste0("+init=EPSG:",epsg_code)),
    filename = paste0(raster_input_path,raster_output_name,".tif")
  )
  
}



#4.3
mosaic_rasters2=function(raster_input_path,raster_output_path, raster_resolution, raster_output_name, num_chunks, epsg_code){
  
  raster_temp_path=paste0(raster_input_path,'temp/')
  dir.create(raster_temp_path)
  A<-list.files(raster_input_path, pattern = ".tif$", full.names = TRUE)
  
  chunker <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  B<-chunker(A, num_chunks)
  #C=B[[1]]
  count<-1
  mymosaics <- lapply(B, function(x) {
    make_mosaic(
      files = x,
      fun = mean,
      na.rm = TRUE,
      crs = CRS(paste0("+init=EPSG:",epsg_code)),
      filename = paste(raster_temp_path,raster_output_name,"_", count, ".tif", sep = "")
    )
    count<<-count+1
  })
  
  make_mosaic(
    files = list.files(raster_temp_path, pattern = "*.tif$", full.names = TRUE),
    fun = mean,
    na.rm = TRUE,
    crs = CRS(paste0("+init=EPSG:",epsg_code)),
    filename = paste0(raster_output_path,raster_output_name,".tif")
  )
  
}
######################################## Tools
#5.1
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

#5.2
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

#5.3
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

#5.4
thin_laz_highest=function(input_file_pattern, out_path, step, subcircle,LAStools_path){
  # Thin the highest points in a laz file
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