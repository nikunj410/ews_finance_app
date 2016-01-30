# Sub_Afria_veg = raster("/Users/nikunj410/Box Sync/savannas_and_forests/codes/world_biome_simulation/Sub-Saharan Africa/SubSaharanAfricaVegetation.tif")
# africa_shape = readShapePoly("/Users/nikunj410/Box Sync/savannas_and_forests/data/shape_files/Africa/AfricanCountires.shp")
# e = extent(-19.5,52.75,-37.5,19)
# s<-raster(e, nrows=147, ncols=187, crs=Sub_Afria_veg@crs)
# Sub_Afria_veg<-resample(Sub_Afria_veg, s, method="bilinear")
# s<-raster(e, nrows=220, ncols=280, crs=Sub_Afria_veg@crs)
# Sub_Afria_veg<-resample(Sub_Afria_veg, s, method="bilinear")
# Sub_Afria_veg = mask(Sub_Afria_veg,africa_shape)
# plot(Sub_Afria_veg)
# B_vegetation = 76
# contour(Sub_Afria_veg, levels= c(B_vegetation), add = T, drawlabels = F)
# 
# vegetation = as.matrix(Sub_Afria_veg)
# columns = ncol(vegetation)
# rows = nrow(vegetation)
# B_rain_contours = contourLines(1:rows, 1:columns, vegetation, levels = c(B_vegetation))
# rainfall = as.matrix(Sub_Afria_rain)
# x = numeric(0)
# y = numeric(0)
# 
# 
# for (i in 1:length(B_rain_contours))
# {
#   x = c(x,floor(as.numeric(B_rain_contours[[i]]$x)))
#   y = c(y,floor(as.numeric(B_rain_contours[[i]]$y)))
# }
# 
# n = 10

res <- lsfit.circle(x=x, y=y)
res$coefficients[1]
