# Load
f = "prim_forests_loss_area.csv"
d =  read.csv(f, stringsAsFactor=F)
d$annual.rate = ((d$arealoss/d$areaforest)/18)*100
d = d[order(d$annual.rate),]

# Plot
png('defor.png',units="in", width=5, height=6, res=150)
barplot(height = d$annual.rate,
  names.arg = d$fips,
  beside = true, las = 2,
  main = "Tropical primary forest loss (2000-2018)",
  ylab = "Annual deforestation (%)",
  border = "black", axes = TRUE)
dev.off()

# Write html file
fileConn<-file('/home/app/index.html')
writeLines('<h1>My deforestation analysis app</h1>/n<img src="src/defor.png" alt="">', fileConn)
close(fileConn)
