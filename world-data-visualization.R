######################################################
# ggplot regional data summary
# Daniel Hong
######################################################

library(ggplot2)
# Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld
# Equatorial Belts
mp <- mp + geom_point(aes(x=equatorial_P_I$lon, y=equatorial_P_I$lat), colour="yellow", size=1)
mp <- mp + geom_point(aes(x=equatorial_P_A$lon, y=equatorial_P_A$lat), colour="gold2", size=1)
# North East Pacific
mp <- mp + geom_point(aes(x=north_east_pacific1$lon, y=north_east_pacific1$lat), colour="dodgerblue", size=1)
mp <- mp + geom_point(aes(x=north_east_pacific2$lon, y=north_east_pacific2$lat), colour="deepskyblue", size=1)
mp <- mp + geom_point(aes(x=north_east_pacific3$lon, y=north_east_pacific3$lat), colour="darkslategray1", size=1)
# North West Pacific
mp <- mp + geom_point(aes(x=north_west_pacific1$lon, y=north_west_pacific1$lat), colour="forestgreen", size=1)
mp <- mp + geom_point(aes(x=north_west_pacific2$lon, y=north_west_pacific2$lat), colour="yellowgreen", size=1)
mp <- mp + geom_point(aes(x=north_west_pacific3$lon, y=north_west_pacific3$lat), colour="springgreen", size=1)
# Southern Belts
mp <- mp + geom_point(aes(x=southern1$lon, y=southern1$lat), colour="darkorange2", size=1)
mp <- mp + geom_point(aes(x=southern2$lon, y=southern2$lat), colour="firebrick1", size=1)
# Antarctic Belt
mp <- mp + geom_point(aes(x=antarctic$lon, y=antarctic$lat), colour="plum", size=1)

# North Atlantic
mp <- mp + geom_point(aes(x=north_atlantic$lon, y=north_atlantic$lat), colour="magenta", size=1)

mp
