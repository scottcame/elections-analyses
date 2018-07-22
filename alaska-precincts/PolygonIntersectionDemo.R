library(tidyverse)
library(sp)
library(rgeos)

m <- matrix(c(1,1.25,1,2.25,3,2.25,3,1.25), nrow=4, byrow = TRUE)
sps <- SpatialPolygons(list(Polygons(list(Polygon(m)), 1)))
spsDf <- fortify(sps)

m2 <- matrix(c(2,1,2,2,4,2,4,1), nrow=4, byrow = TRUE)
sps2 <- SpatialPolygons(list(Polygons(list(Polygon(m2)), 1)))
sps2Df <- fortify(sps2)

i <- gIntersection(sps, sps2)
iDf <- fortify(i)

ggplot() +
  geom_polygon(data=spsDf, aes(x=long, y=lat), fill='blue', alpha=.3) +
  geom_polygon(data=sps2Df, aes(x=long, y=lat), fill='red', alpha=.3) +
  geom_polygon(data=iDf, aes(x=long, y=lat), fill='green', alpha=.3) +
  labs(x=NULL, y=NULL)


