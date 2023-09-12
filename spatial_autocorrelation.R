
# variogram and spatial autocorrelation

library (geosphere)
library(ape)
library(readxl)
library(geostats)
library(nlme)
data_final <- read_excel("fieldcourse/data_final.xlsx")
View(data_final)

spatial <- distm(fun = distGeo, x = data_final [,c ("lat", "long")] ) 
Moran.I(x = data_final$exp_shannon, spatial  )

# could also see spa when points were colored according to sites.

semivariogram(x = data_final$lat, y = data_final$long, z = data_final$exp_shannon)

# lag: how much the signal is lagging according to distance (?)
# x not m bc coordinates are in decimal degrees

# change x to meters?

# gls model - input a correlation structure
# account for spa
# spatial a structure  distance matrix (spatial)

mod_gls <- gls( exp_shannon ~ LAI, data = data_final, correlation = corSpatial(form = ~ lat + long, 
                            nugget = T ) )
summary(mod_gls)
 ? gls
