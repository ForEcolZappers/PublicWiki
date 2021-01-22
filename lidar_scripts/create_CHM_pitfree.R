

create_CHM_pitfree=function(input,
                             step,
                             h_seq,
                             subcircle,
                             kill,
                             out_path,
                             tmp_laz_path,
                             tmp_chm_path,
                             bbtype, # bbtype not applicable because using tiles not made with lastools... i think
                             LAStools_path){
  foreach(i = input,
          .packages = c("raster"),
          .inorder = TRUE) %dopar% {
   #for (i in input) {
            
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
            
            # Sequentially build the CHM for each layer
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
   }
 
}
