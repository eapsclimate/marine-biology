######################################################
# Script for subset analysis to find unique regions 
# For algorithm development
# Daniel Hong
######################################################

# Remove all objects
rm(list=ls())

# Following packages are required: DBI and RPostgreSQL
# You can check if installed with
packageDescription('RPostgreSQL')
packageDescription('DBI')

# Loading necessary packages.
# (1) In order to connect R with PostgreSQL database you need to load the RPostgreSQL package.
library(RPostgreSQL)

# (2) Package for mapping in R
library(rworldmap)

# Load driver for PostgreSQL
drv <- dbDriver("PostgreSQL")
# dbConnect needs to know what kind of database driver it should use.

# Connect to PostgreSQL server
# For TCP/IP connection
con <- dbConnect(drv, host="seascape.ldeo.columbia.edu", user= "dhong", password="sh3266", dbname="oceandata")

# For SSH tunnel (PortForwarding)
#con <- dbConnect(drv, host="127.0.0.1", user= "username", password="******", dbname="database")

# Close database connection and unload driver. Only use after querying the database!
dbDisconnect(con)
dbUnloadDriver(drv)
# These commands are not necessary, but generally good practice when finished working with the database.



###############  Subsets  ###############
# Note: these queries should all work after the dbConnect command was successful.

load(file.choose())
# Clean Data
data <- subset(data, data$depth<20)

# Format date entries of clean data
data$date <- as.Date(data$date)
format(data$date[1], "%m")
as.numeric(format(data$date[1], "%m"))

# Map Plot
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(-80, 90), asp = 1)

# Plot on Top of Map
points(x=data$lon, y=data$lat, col = "red", cex = .3)


##### Clean Data #####
# Clean Data Plot
plot(data$SST, data$N, main="N vs SST", xlab="Temperature", ylab="Nitrate")
plot(data$chl, data$N, main="N vs Chl", xlab="Chlorophyll", ylab="Nitrate")

#****************************************

# Data Subset - North East Pacific Combined
north_east_pacific <- subset(data, data$lat > 20 & data$lon < -110)
plot(north_east_pacific$SST, north_east_pacific$N, main="N vs SST - NorthEastPacific", xlab="Temperature", ylab="Nitrate")
plot(north_east_pacific$chl, north_east_pacific$N, main="N vs Chl - NorthEastPacific", xlab="Chl", ylab="Nitrate")

# Data Subset - North East Pacific 1 (Most North)
north_east_pacific1 <- subset(data, data$lat > 65 & (data$lon > -180 & data$lon < -110))
plot(north_east_pacific1$SST, north_east_pacific1$N, main="N vs SST - NorthEastPacific 1", xlab="Temperature", ylab="Nitrate")
plot(north_east_pacific1$chl, north_east_pacific1$N, main="N vs Chl - NorthEastPacific 1", xlab="Chl", ylab="Nitrate")
# Color
plot(north_east_pacific1$SST, north_east_pacific1$N, main="N vs SST - NorthEastPacific 1", xlab="Temperature", ylab="Nitrate", col = ifelse(north_east_pacific1$lat > 70, "blue", "red"))

# Data Subset - North East Pacific 2
north_east_pacific2 <- subset(data, (data$lat < 65 & data$lat > 50) & (data$lon > -180 & data$lon < -110))
plot(north_east_pacific2$SST, north_east_pacific2$N, main="N vs SST - NorthEastPacific 2", xlab="Temperature", ylab="Nitrate")
plot(north_east_pacific2$chl, north_east_pacific2$N, main="N vs Chl - NorthEastPacific 2", xlab="Chl", ylab="Nitrate")
# Color
plot(north_east_pacific2$SST, north_east_pacific2$N, main="N vs SST - NorthEastPacific 2", xlab="Temperature", ylab="Nitrate", col = ifelse(north_east_pacific2$lat > 65, "blue", "red"))

# Data Subset - North East Pacific 3
north_east_pacific3 <- subset(data, (data$lat < 50 & data$lat > 20) & (data$lon > -170 & data$lon < -110))
plot(north_east_pacific3$SST, north_east_pacific3$N, main="N vs SST - NorthEastPacific 3", xlab="Temperature", ylab="Nitrate")
plot(north_east_pacific3$chl, north_east_pacific3$N, main="N vs Chl - NorthEastPacific 3", xlab="Chl", ylab="Nitrate")
# Color
plot(north_east_pacific3$SST, north_east_pacific3$N, main="N vs SST - NorthEastPacific 3", xlab="Temperature", ylab="Nitrate", col = ifelse(north_east_pacific3$lat > 35, "blue", "red"))

# Data Subset - North East Pacific 4
north_east_pacific4 <- subset(data, (data$lat < 20 & data$lat > -20) & data$lon < -110)
plot(north_east_pacific4$SST, north_east_pacific4$N, main="N vs SST - NorthEastPacific 3", xlab="Temperature", ylab="Nitrate")
plot(north_east_pacific4$chl, north_east_pacific4$N, main="N vs Chl - NorthEastPacific 3", xlab="Chl", ylab="Nitrate")
# Color
plot(north_east_pacific3$SST, north_east_pacific3$N, main="N vs SST - NorthEastPacific 3", xlab="Temperature", ylab="Nitrate", col = ifelse(north_east_pacific3$lat > 35, "blue", "red"))


# ****************************************

# Data Subset - North West Pacific Combined
north_west_pacific <- subset(data, data$lat > -30 & data$lon > 90)
plot(north_west_pacific$SST, north_west_pacific$N, main="N vs SST - NorthWestPacific", xlab="Temperature", ylab="Nitrate")
plot(north_west_pacific$chl, north_west_pacific$N, main="N vs SST - NorthWestPacific", xlab="Temperature", ylab="Nitrate")

# Data Subset - North West Pacific 1
north_west_pacific1 <- subset(data, (data$lat < 90 & data$lat > 45) & data$lon > 90)
plot(north_west_pacific1$SST, north_west_pacific1$N, main="N vs SST - NorthWestPacific 1", xlab="Temperature", ylab="Nitrate")
plot(north_west_pacific1$chl, north_west_pacific1$N, main="N vs Chl - NorthWestPacific 1", xlab="Chl", ylab="Nitrate")

# Data Subset - North West Pacific 2
north_west_pacific2 <- subset(data, (data$lat < 45 & data$lat > 38) & data$lon > 90)
plot(north_west_pacific2$SST, north_west_pacific2$N, main="N vs SST - NorthWestPacific 2", xlab="Temperature", ylab="Nitrate")
plot(north_west_pacific2$chl, north_west_pacific2$N, main="N vs Chl - NorthWestPacific 2", xlab="Chl", ylab="Nitrate")
# Color
plot(north_west_pacific2$SST, north_west_pacific2$N, main="N vs SST - NorthWestPacific 2", xlab="Temperature", ylab="Nitrate", col = ifelse(north_west_pacific2$lat > 42, "blue", "red"))

# Data Subset - North West Pacific 3
north_west_pacific3 <- subset(data, (data$lat < 38 & data$lat > 20) & data$lon > 90)
plot(north_west_pacific3$SST, north_west_pacific3$N, main="N vs SST - NorthWestPacific 3", xlab="Temperature", ylab="Nitrate")
plot(north_west_pacific3$chl, north_west_pacific3$N, main="N vs Chl - NorthWestPacific 3", xlab="Chl", ylab="Nitrate")
# Color
plot(north_west_pacific3$SST, north_west_pacific3$N, main="N vs SST - NorthWestPacific 3", xlab="Temperature", ylab="Nitrate", col = ifelse(north_west_pacific3$lat > 33 & north_west_pacific3$lat < 38, "blue", "red"))

# Data Subset - North West Pacific 4
north_west_pacific4 <- subset(data, (data$lat < 20 & data$lat > -20) & data$lon > 130)
plot(north_west_pacific4$SST, north_west_pacific4$N, main="N vs SST - NorthWestPacific 4", xlab="Temperature", ylab="Nitrate")
plot(north_west_pacific4$chl, north_west_pacific4$N, main="N vs Chl - NorthWestPacific 4", xlab="Chl", ylab="Nitrate")
# Color
plot(north_west_pacific4$SST, north_west_pacific4$N, main="N vs SST - NorthWestPacific 4", xlab="Temperature", ylab="Nitrate", col = ifelse(north_west_pacific4$lon > 130, "blue", "red"))
plot(north_west_pacific4$SST, north_west_pacific4$N, main="N vs SST - NorthWestPacific 4", xlab="Temperature", ylab="Nitrate", col = ifelse(north_west_pacific4$lat < -10, "blue", "red"))

#****************************************

# Data Subset - South East Pacific Combined
south_east_pacific <- subset(data, data$lat < -60 & data$lat > -90)
plot(south_east_pacific$SST, south_east_pacific$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate")
plot(south_east_pacific$chl, south_east_pacific$N, main="N vs Chl - SouthEastPacific", xlab="Chl", ylab="Nitrate")
# Color
plot(south_east_pacific$SST, south_east_pacific$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate", col = ifelse(south_east_pacific$lat > -45, "blue", "red"))

# Data Subset - South East Pacific 1
south_east_pacific1 <- subset(data, (data$lat < -20 & data$lat > -45) & (data$lon > -110 & data$lon < -70))
plot(south_east_pacific1$SST, south_east_pacific1$N, main="N vs SST - SouthEastPacific 1", xlab="Temperature", ylab="Nitrate")
plot(south_east_pacific1$chl, south_east_pacific1$N, main="N vs Chl - SouthEastPacific 1", xlab="Chl", ylab="Nitrate")
# Color
plot(south_east_pacific1$SST, south_east_pacific1$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate", col = ifelse(south_east_pacific1$lat > -45, "blue", "red"))

# Data Subset - South East Pacific 2
south_east_pacific2 <- subset(data, data$lat < -20 & data$lat > -45 & data$SST > 5)
plot(south_east_pacific2$SST, south_east_pacific2$N, main="N vs SST - SouthEastPacific 2", xlab="Temperature", ylab="Nitrate")
plot(south_east_pacific2$chl, south_east_pacific2$N, main="N vs Chl - SouthEastPacific", xlab="Chl", ylab="Nitrate")
# Color
plot(south_east_pacific1$SST, south_east_pacific1$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate", col = ifelse(south_east_pacific1$lat > -45, "blue", "red"))
#****************************************

# Data Subset - North Atlantic Combined
north_atlantic <- subset(data, data$lat > 20 & (data$lon > -110 & data$lon < 90))
plot(north_atlantic$SST, north_atlantic$N, main="N vs SST - North Atlantic", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic$chl, north_atlantic$N, main="N vs Chl - North Atlantic", xlab="Chl", ylab="Nitrate")
# Color
plot(north_atlantic$SST, north_atlantic$N, main="N vs SST - North Atlantic 1", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic$lat > 65, "blue", "red"))

# Data Subset - North Atlantic 1
north_atlantic1 <- subset(data, data$lat > 60 & (data$lon > -20 & data$lon < 90))
plot(north_atlantic1$SST, north_atlantic1$N, main="N vs SST - North Atlantic 1", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic1$chl, north_atlantic1$N, main="N vs Chl - North Atlantic 1", xlab="Chl", ylab="Nitrate")
# Color
plot(north_atlantic1$SST, north_atlantic1$N, main="N vs SST - North Atlantic 1", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic1$lat > 65, "blue", "red"))

# Data Subset - North Atlantic 2
north_atlantic2 <- subset(data, (data$lat < 60 & data$lat > 45) & (data$lon > -20 & data$lon < 90))
plot(north_atlantic2$SST, north_atlantic2$N, main="N vs SST - North Atlantic 2", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic2$chl, north_atlantic2$N, main="N vs Chl - North Atlantic 2", xlab="Chl", ylab="Nitrate")
# Color
plot(north_atlantic2$SST, north_atlantic2$N, main="N vs SST - North Atlantic 2", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic2$lat > 55, "blue", "red"))

# Data Subset - North Atlantic 3
north_atlantic3 <- subset(data, (data$lat < 45 & data$lat > 20) & (data$lon > -110 & data$lon < -20))
plot(north_atlantic3$SST, north_atlantic3$N, main="N vs SST - North Atlantic 3", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic3$chl, north_atlantic3$N, main="N vs Chl - North Atlantic 3", xlab="Chl", ylab="Nitrate")
# Color
plot(north_atlantic3$SST, north_atlantic3$N, main="N vs SST - North Atlantic 3", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic3$lon > -90, "blue", "red"))

# Data Subset - North Atlantic 3, Disection1
north_atlantic_d1 <- subset(north_atlantic3, north_atlantic3$N > 2 & (north_atlantic3$lat < 45 & north_atlantic3$lat > 20) & (north_atlantic3$lon > -93.429 & north_atlantic3$lon < -20.002) & (north_atlantic3$SST > 15 & north_atlantic3$SST < 27.0552) & (north_atlantic3$salt > 5.329 & north_atlantic3$salt < 37.498) & (north_atlantic3$chl > 0 & north_atlantic3$chl < 48.56))
plot(north_atlantic_d1$SST, north_atlantic_d1$N, main="N vs SST - North Atlantic D1", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic_d1$chl, north_atlantic_d1$N, main="N vs Chl - North Atlantic D1", xlab="Chl", ylab="Nitrate")
plot(north_atlantic_d1$salt, north_atlantic_d1$N, main="N vs Chl - North Atlantic D1", xlab="Chl", ylab="Nitrate")
# Color
plot(north_atlantic_d1$SST, north_atlantic_d1$N, main="N vs SST - North Atlantic D1", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic_d1$lat > 45, "blue", "red"))

# Data Subset - North Atlantic 3, Disection2
north_atlantic_d2 <- subset(north_atlantic3, (north_atlantic3$lat < 65.742 & north_atlantic3$lat > 22.42) & (north_atlantic3$lon > -93.417 & north_atlantic3$lon < -20.002) & (north_atlantic3$SST > 6.8 & north_atlantic3$SST < 22.4389) & (north_atlantic3$salt > 5.079 & north_atlantic3$salt < 37.089) & (north_atlantic3$chl > 0 & north_atlantic3$chl < 45.6))
plot(north_atlantic_d2$SST, north_atlantic_d2$N, main="N vs SST - North Atlantic D2", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic_d2$chl, north_atlantic_d2$N, main="N vs Chl - North Atlantic D2", xlab="Chl", ylab="Nitrate")
plot(north_atlantic_d2$salt, north_atlantic_d2$N, main="N vs Salinity - North Atlantic D2", xlab="Salinity", ylab="Nitrate")
# Color
plot(north_atlantic_d2$SST, north_atlantic_d2$N, main="N vs SST - North Atlantic D1", xlab="Temperature", ylab="Nitrate", col = ifelse(north_atlantic_d1$lat > 45, "blue", "red"))

# Data Subset - North Atlantic 4
north_atlantic4 <- subset(data, (data$lat < 20 & data$lat > -20) & (data$lon > -70 & data$lon < 30))
plot(north_atlantic4$SST, north_atlantic4$N, main="N vs SST - North Atlantic 4", xlab="Temperature", ylab="Nitrate")
plot(north_atlantic4$chl, north_atlantic4$N, main="N vs Chl - North Atlantic 4", xlab="Chl", ylab="Nitrate")

#****************************************

# Data Subset - South Atlantic Combined
south_atlantic <- subset(data, data$lat < -15 & (data$lon > -70 & data$lon < 30))
plot(south_atlantic$SST, south_atlantic$N, main="N vs SST - South Atlantic", xlab="Temperature", ylab="Nitrate")
plot(south_atlantic$chl, south_atlantic$N, main="N vs Chl - South Atlantic", xlab="Chl", ylab="Nitrate")
# Color
plot(south_atlantic$SST, south_atlantic$N, main="N vs SST - South Atlantic", xlab="Temperature", ylab="Nitrate", col = ifelse(south_atlantic$lat < -45, "blue", "red"))

# Data Subset - South Atlantic 1
south_atlantic1 <- subset(data, (data$lat < -20 & data$lat > -45) & (data$lon > -70 & data$lon < 30) & data$SST > 5)
plot(south_atlantic1$SST, south_atlantic1$N, main="N vs SST - South Atlantic 1", xlab="Temperature", ylab="Nitrate")
plot(south_atlantic1$chl, south_atlantic1$N, main="N vs Chl - South Atlantic 1", xlab="Chl", ylab="Nitrate")
# Color
plot(south_atlantic$SST, south_atlantic$N, main="N vs SST - South Atlantic", xlab="Temperature", ylab="Nitrate", col = ifelse(south_atlantic$lat < -45, "blue", "red"))

# Data Subset - South Atlantic 2
south_atlantic2 <- subset(data, data$lat < -45 & (data$lon > -70 & data$lon < 30))
plot(south_atlantic2$SST, south_atlantic2$N, main="N vs SST - South Atlantic 2", xlab="Temperature", ylab="Nitrate")
plot(south_atlantic2$chl, south_atlantic2$N, main="N vs Chl - South Atlantic 2", xlab="Chl", ylab="Nitrate")
# Color
plot(south_atlantic2$SST, south_atlantic2$N, main="N vs SST - South Atlantic", xlab="Temperature", ylab="Nitrate", col = ifelse(south_atlantic2$lat < -60, "blue", "red"))

#****************************************

# Data Subst - Indian Ocean
indian <- subset(data, (data$lat < 38 & data$lat > 20) & (data$lon > 30 & data$lon < 90))
plot(indian$SST, indian$N, main="N vs SST - Indian", xlab="Temperature", ylab="Nitrate")
plot(indian$chl, indian$N, main="N vs Chl - Indian", xlab="Chl", ylab="Nitrate")
# Color
plot(indian$SST, indian$N, main="N vs SST - Indian", xlab="Temperature", ylab="Nitrate")

#****************************************

# Data Subset - Equitorial Belt
equitorial <- subset(data, data$lat < 20 & data$lat > -20)
plot(equitorial$SST, equitorial$N, main="N vs SST - Equitorial Belt", xlab="Temperature", ylab="Nitrate")
plot(equitorial$chl, equitorial$N, main="N vs Chl - Equitorial Belt", xlab="Chl", ylab="Nitrate")
# Color
plot(equitorial$SST, equitorial$N, main="N vs SST - Equitorial Belt", xlab="Temperature", ylab="Nitrate", col=ifelse(equitorial$lon < -110, "blue", ifelse(equitorial$lon > -110 & equitorial$lon < -70, "green", ifelse(equitorial$lon > -70 & equitorial$lon < 30, "red", "yellow"))))

#****************************************

# Data Subset - Equatorial Belt - Pacific + Indian
equatorial_P_I <- subset(data, (data$lat < 20 & data$lat > -20) & (data$lon < -110 | data$lon > 30))
plot(equatorial_P_I$SST, equatorial_P_I$N, main="N vs SST - Equatorial Belt, Pacific", xlab="Temperature", ylab="Nitrate")
# Spring
equitorial_P_I_spring <- subset(spring_data, (spring_data$lat < 20 & spring_data$lat > -20) & (spring_data$lon < -110 | spring_data$lon > 30))
plot(equitorial_P_I_spring$SST, equitorial_P_I_spring$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate")
plot(equitorial_P_I_spring$SST, equitorial_P_I_spring$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate", col = ifelse(equitorial_P_I_spring$lon > 90 & equitorial_P_I_spring$lon < 130, "yellow", ifelse(equitorial_P_I_spring$lon < -110, "green", "blue")))
# Summer
equitorial_P_I_summer <- subset(summer_data, (summer_data$lat < 20 & summer_data$lat > -20) & (summer_data$lon < -110 | summer_data$lon > 30))
plot(equitorial_P_I_summer$SST, equitorial_P_I_summer$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate")
plot(equitorial_P_I_summer$SST, equitorial_P_I_summer$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate", col = ifelse(equitorial_P_I_summer$lon > 90 & equitorial_P_I_summer$lon < 130, "yellow", ifelse(equitorial_P_I_summer$lon < -110, "green", "blue")))
# Fall
equitorial_P_I_fall <- subset(fall_data, (fall_data$lat < 20 & fall_data$lat > -20) & (fall_data$lon < -110 | fall_data$lon > 30))
plot(equitorial_P_I_fall$SST, equitorial_P_I_fall$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate")
plot(equitorial_P_I_fall$SST, equitorial_P_I_fall$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate", col = ifelse(equitorial_P_I_fall$lon > 90 & equitorial_P_I_fall$lon < 130, "yellow", ifelse(equitorial_P_I_fall$lon < -110, "green", "blue")))
# Winter
equitorial_P_I_winter <- subset(winter_data, (winter_data$lat < 20 & winter_data$lat > -20) & (winter_data$lon < -110 | winter_data$lon > 30))
plot(equitorial_P_I_winter$SST, equitorial_P_I_winter$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate")
plot(equitorial_P_I_winter$SST, equitorial_P_I_winter$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate", col = ifelse(equitorial_P_I_winter$lon > 90 & equitorial_P_I_winter$lon < 130, "yellow", ifelse(equitorial_P_I_winter$lon < -110, "green", "blue")))
# Color
plot(equatorial_P_I$SST, equatorial_P_I$N, main="N vs SST - Equitorial Belt, Pacific", xlab="Temperature", ylab="Nitrate", col = ifelse(equatorial_P_I$lon > 90 & equatorial_P_I$lon < 130, "yellow", ifelse(equatorial_P_I$lon < -110, "green", "blue")))

#****************************************

# Data Subset - Equitorial Belt - Pacific (East Cluster) + Atlantic
equatorial_P_A <- subset(data, (data$lat < 20 & data$lat > -20) & (data$lon > -110 & data$lon < 30))
plot(equatorial_P_A$SST, equatorial_P_A$N, main="N vs SST - Equitorial Belt, Atlantic", xlab="Temperature", ylab="Nitrate")
plot(equatorial_P_A$SST, equatorial_P_A$N, main="N vs SST - Equitorial Belt, Atlantic", xlab="Temperature", ylab="Nitrate", col = ifelse(equatorial_P_A$lon < -70, "blue", "red"))

#****************************************

# Equitorial Belt - Peru
peru <- subset(data, (data$lat < 20 & data$lat > -20) & (data$lon > -110 & data$lon < -70))
plot(peru$salt, peru$N)
plot(peru$chl, peru$N)
# Spring
peru_spring <- subset(spring_data, (spring_data$lat < 20 & spring_data$lat > -20) & (spring_data$lon > -110 & spring_data$lon < -70))
plot(peru_spring$SST, peru_spring$N, main="N vs SST - Equitorial Belt, Peru", xlab="Temperature", ylab="Nitrate")
# Summer
peru_summer <- subset(summer_data, (summer_data$lat < 20 & summer_data$lat > -20) & (summer_data$lon > -110 & summer_data$lon < -70))
plot(peru_summer$SST, peru_summer$N, main="N vs SST - Equitorial Belt, Peru", xlab="Temperature", ylab="Nitrate")
# Fall
peru_fall <- subset(fall_data, (fall_data$lat < 20 & fall_data$lat > -20) & (fall_data$lon > -110 & fall_data$lon < -70))
plot(peru_fall$SST, peru_fall$N, main="N vs SST - Equitorial Belt, Peru", xlab="Temperature", ylab="Nitrate")
# Winter
peru_winter <- subset(winter_data, (winter_data$lat < 20 & winter_data$lat > -20) & (winter_data$lon > -110 & winter_data$lon < -70))
plot(peru_winter$SST, peru_winter$N, main="N vs SST - Equitorial Belt, Peru", xlab="Temperature", ylab="Nitrate")

#****************************************

# Data Subset - Southern Belt 1
southern1 <- subset(data, data$lat < -20 & data$lat > -45 & data$SST > 5)
plot(southern1$SST, southern1$N, main="N vs SST - Southern Belt 1", xlab="Temperature", ylab="Nitrate")
plot(southern1$chl, southern1$N, main="N vs Chl - Southern Belt 1", xlab="Chl", ylab="Nitrate")
# Color
plot(southern1$SST, southern1$N, main="N vs SST - Southern Belt 1", xlab="Temperature", ylab="Nitrate", col = ifelse(southern1$lat > -45, "blue", "red"))

#****************************************

# Data Subset - Southern Belt 2
southern2 <- subset(data, data$lat < -45 & data$lat > -60)
plot(southern2$SST, southern2$N, main="N vs SST - Southern Belt 2", xlab="Temperature", ylab="Nitrate")
plot(southern2$chl, southern2$N, main="N vs Chl - Southern Belt 2", xlab="Chl", ylab="Nitrate")

#****************************************

# Data Subset - Antarctic Belt
antarctic <- subset(data, data$lat < -60 & data$lat > -90)
plot(antarctic$SST, antarctic$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate")
plot(antarctic$chl, antarctic$N, main="N vs Chl - SouthEastPacific", xlab="Chl", ylab="Nitrate")
# Color
plot(antarctic$SST, antarctic$N, main="N vs SST - SouthEastPacific", xlab="Temperature", ylab="Nitrate", col = ifelse(antarctic$lat > -45, "blue", "red"))

#****************************************

# Open Satelite Salinity h5 File
library(rhdf5)
h5ls("SSS.h5")
sss <- h5read("SSS.h5","/bands/SSS_sum")
rotate <- function(x) t(apply(x, 2, rev))
sss <- rotate(rotate(sss))
sss <- apply(sss, 2, rev)
image(sss)
# Clean matrix
df_salinity <- matrix(nrow = 64800, ncol =  3)
n = 1
for(i in 90:-90){
  for(j in -180:180){
    if(i!=0 & j != 0) {
      df_salinity[n,] = c(i,j,sss[n])
      n <- n+1
    }
  }
}
# Convert to data frame
df_salinity <- as.data.frame(df_salinity)
df_salinity <- df_salinity[!is.na(as.numeric(df_salinity$V3)),]
colnames(df_salinity) <- c("lat","lon","sss")



# Open KD Satelite File - h5
h5ls("kd.h5")
kd <- h5read("kd.h5","/bands/mask_data_water_fraction_smoothed")



# Open KD Satelite File - ncdf4
library(ncdf4)
kd_nc <- nc_open("kd.nc")
str(kd_nc)

kd_matrix <- ncvar_get(kd_nc, "Kd_490")
kd_matrix <- rotate(rotate(kd_matrix))
kd_matrix <- apply(kd_matrix, 2, rev)
image(kd_matrix)

kd_lat <- ncvar_get(kd_nc, "lat")
kd_lat <- as.matrix(kd_lat)

kd_lon <- ncvar_get(kd_nc, "lon")
kd_lon <- as.matrix(kd_lon)

# kd match
df_kd <- matrix(nrow = 9331200, ncol =  3)
n=1
for(i in 1:2160) {
  for(j in 1:4320) {
    df_kd[n,] = c(kd_lat[i],kd_lon[j],kd_matrix[n])
    n <- n+1
  }
}
# Convert to data frame
df_kd <- as.data.frame(df_kd)
df_kd <- df_kd[!is.na(as.numeric(df_kd$V3)),]
colnames(df_kd) <- c("lat","lon","kd")



# match SSS & KD data
df_salinity_clean <- subset(df_salinity, df_salinity$sss > 25 & df_salinity$sss < 40)
df_kd_round <- subset(df_kd, df_kd$kd < 5)
df_kd_round[,1:2] <- round(df_kd_round[,1:2],0)

df_kd_sss_clean <- merge(df_kd_round, df_salinity_clean, by=c("lat","lon"), all=T)
df_kd_sss_clean <- df_kd_sss_clean[!is.na(as.numeric(df_kd_sss_clean$kd)),]
df_kd_sss_clean <- df_kd_sss_clean[!is.na(as.numeric(df_kd_sss_clean$sss)),]

df_kd_sss_nodupl <- df_kd_sss_clean[!duplicated(df_kd_sss_clean[c("lat","lon")]),]

# plot kd vs sss with duplicates
plot(df_kd_sss_clean$sss, df_kd_sss_clean$kd)
# plot kd vs sss without duplicates
plot(df_kd_sss_nodupl$sss, df_kd_sss_nodupl$kd)


# Atlantic subset
df_salinity_clean_atlantic <- subset(df_salinity, (df_salinity$lat > 20 & df_salinity$lat < 90) & (df_salinity$lon > -110 & df_salinity$lon < -20) & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_atlantic <- subset(df_kd, (df_kd$lat > 20 & df_kd$lat < 90) & (df_kd$lon > -110 & df_kd$lon < -20) & df_kd$kd < 5)
df_kd_round_atlantic[,1:2] <- round(df_kd_round_atlantic[,1:2],0)

df_kd_sss_clean_atlantic <- merge(df_kd_round_atlantic, df_salinity_clean_atlantic, by=c("lat","lon"), all=T)
df_kd_sss_clean_atlantic <- df_kd_sss_clean_atlantic[!is.na(as.numeric(df_kd_sss_clean_atlantic$kd)),]
df_kd_sss_clean_atlantic <- df_kd_sss_clean_atlantic[!is.na(as.numeric(df_kd_sss_clean_atlantic$sss)),]

df_kd_sss_atlantic_nodupl <- df_kd_sss_clean_atlantic[!duplicated(df_kd_sss_clean_atlantic[c("lat","lon")]),]

plot(df_kd_sss_atlantic_nodupl$sss, df_kd_sss_atlantic_nodupl$kd)



# Arctic subset
df_salinity_clean_arctic <- subset(df_salinity, df_salinity$lat > 60 & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_arctic <- subset(df_kd, df_kd$lat > 60 & df_kd$kd < 5)
df_kd_round_arctic[,1:2] <- round(df_kd_round_arctic[,1:2],0)

df_kd_sss_clean_arctic <- merge(df_kd_round_arctic, df_salinity_clean_arctic, by=c("lat","lon"), all=T)
df_kd_sss_clean_arctic <- df_kd_sss_clean_arctic[!is.na(as.numeric(df_kd_sss_clean_arctic$kd)),]
df_kd_sss_clean_arctic <- df_kd_sss_clean_arctic[!is.na(as.numeric(df_kd_sss_clean_arctic$sss)),]

df_kd_sss_arctic_nodupl <- df_kd_sss_clean_arctic[!duplicated(df_kd_sss_clean_arctic[c("lat","lon")]),]

plot(df_kd_sss_arctic_nodupl$sss, df_kd_sss_arctic_nodupl$kd)



# Pacific subset
df_salinity_clean_pacific <- subset(df_salinity, (df_salinity$lat > 50 & df_salinity$lat < 60) & (df_salinity$lon < -110 | df_salinity$lon > -20) & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_pacific <- subset(df_kd, (df_kd$lat > 20 & df_kd$lat < 60) & (df_kd$lon < -110 | df_kd$lon > -20) & df_kd$kd < 5)
df_kd_round_pacific[,1:2] <- round(df_kd_round_pacific[,1:2],0)

df_kd_sss_clean_pacific <- merge(df_kd_round_pacific, df_salinity_clean_pacific, by=c("lat","lon"), all=T)
df_kd_sss_clean_pacific <- df_kd_sss_clean_pacific[!is.na(as.numeric(df_kd_sss_clean_pacific$kd)),]
df_kd_sss_clean_pacific <- df_kd_sss_clean_pacific[!is.na(as.numeric(df_kd_sss_clean_pacific$sss)),]

df_kd_sss_pacific_nodupl <- df_kd_sss_clean_pacific[!duplicated(df_kd_sss_clean_pacific[c("lat","lon")]),]

plot(df_kd_sss_pacific_nodupl$sss, df_kd_sss_pacific_nodupl$kd)


# Equatorial PI subset
df_salinity_clean_equatorial_PI <- subset(df_salinity, (df_salinity$lat > -20 & df_salinity$lat < 20) & (df_salinity$lon < -110 | df_salinity$lon > 30) & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_equatorial_PI <- subset(df_kd, (df_kd$lat > -20 & df_kd$lat < 20) & (df_kd$lon < -110 | df_kd$lon > 30) & df_kd$kd < 1)
df_kd_round_equatorial_PI[,1:2] <- round(df_kd_round_equatorial_PI[,1:2],0)

df_kd_sss_clean_equatorial_PI <- merge(df_kd_round_equatorial_PI, df_salinity_clean_equatorial_PI, by=c("lat","lon"), all=T)
df_kd_sss_clean_equatorial_PI <- df_kd_sss_clean_equatorial_PI[!is.na(as.numeric(df_kd_sss_clean_equatorial_PI$kd)),]
df_kd_sss_clean_equatorial_PI <- df_kd_sss_clean_equatorial_PI[!is.na(as.numeric(df_kd_sss_clean_equatorial_PI$sss)),]

df_kd_sss_equatorial_PI_nodupl <- df_kd_sss_clean_equatorial_PI[!duplicated(df_kd_sss_clean_equatorial_PI[c("lat","lon")]),]

plot(df_kd_sss_equatorial_PI_nodupl$sss, df_kd_sss_equatorial_PI_nodupl$kd)


# Equatorial PA subset
df_salinity_clean_equatorial_PA <- subset(df_salinity, (df_salinity$lat > -20 & df_salinity$lat < 20) & (df_salinity$lon > -110 & df_salinity$lon < 30) & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_equatorial_PA <- subset(df_kd, (df_kd$lat > -20 & df_kd$lat < 20) & (df_kd$lon > -110 & df_kd$lon < 30) & df_kd$kd < 1)
df_kd_round_equatorial_PA[,1:2] <- round(df_kd_round_equatorial_PA[,1:2],0)

df_kd_sss_clean_equatorial_PA <- merge(df_kd_round_equatorial_PA, df_salinity_clean_equatorial_PA, by=c("lat","lon"), all=T)
df_kd_sss_clean_equatorial_PA <- df_kd_sss_clean_equatorial_PA[!is.na(as.numeric(df_kd_sss_clean_equatorial_PA$kd)),]
df_kd_sss_clean_equatorial_PA <- df_kd_sss_clean_equatorial_PA[!is.na(as.numeric(df_kd_sss_clean_equatorial_PA$sss)),]

df_kd_sss_equatorial_PA_nodupl <- df_kd_sss_clean_equatorial_PA[!duplicated(df_kd_sss_clean_equatorial_PA[c("lat","lon")]),]

plot(df_kd_sss_equatorial_PA_nodupl$sss, df_kd_sss_equatorial_PA_nodupl$kd)

# Southern belt subset 1
df_salinity_clean_southern <- subset(df_salinity, (df_salinity$lat < -60 & df_salinity$lat > -65) & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_southern <- subset(df_kd, (df_kd$lat < -60 & df_kd$lat > -65) & df_kd$kd < 5)
df_kd_round_southern[,1:2] <- round(df_kd_round_southern[,1:2],0)

df_kd_sss_clean_southern <- merge(df_kd_round_southern, df_salinity_clean_southern, by=c("lat","lon"), all=T)
df_kd_sss_clean_southern <- df_kd_sss_clean_southern[!is.na(as.numeric(df_kd_sss_clean_southern$kd)),]
df_kd_sss_clean_southern <- df_kd_sss_clean_southern[!is.na(as.numeric(df_kd_sss_clean_southern$sss)),]

df_kd_sss_southern_nodupl <- df_kd_sss_clean_southern[!duplicated(df_kd_sss_clean_southern[c("lat","lon")]),]

plot(df_kd_sss_southern_nodupl$sss, df_kd_sss_southern_nodupl$kd)


# Southern belt subset 2
df_salinity_clean_southern2 <- subset(df_salinity, df_salinity$lat < -70 & (df_salinity$sss > 25 & df_salinity$sss < 40))
df_kd_round_southern2 <- subset(df_kd, df_kd$lat < -70 & df_kd$kd < 5)
df_kd_round_southern2[,1:2] <- round(df_kd_round_southern2[,1:2],0)

df_kd_sss_clean_southern2 <- merge(df_kd_round_southern2, df_salinity_clean_southern2, by=c("lat","lon"), all=T)
df_kd_sss_clean_southern2 <- df_kd_sss_clean_southern2[!is.na(as.numeric(df_kd_sss_clean_southern2$kd)),]
df_kd_sss_clean_southern2 <- df_kd_sss_clean_southern2[!is.na(as.numeric(df_kd_sss_clean_southern2$sss)),]

df_kd_sss_southern2_nodupl <- df_kd_sss_clean_southern2[!duplicated(df_kd_sss_clean_southern2[c("lat","lon")]),]

plot(df_kd_sss_southern2_nodupl$sss, df_kd_sss_southern2_nodupl$kd)




# GeoTiff File
library(rgdal)
library(raster)
kd_tiff <- raster("kd.tif")
plot(kd_tiff)
kd_mat <- as.matrix(kd_tiff)
image(kd_mat)
kd_mat[kd_mat < -5000] <- NA
image(t(kd_mat))
hist(kd_mat)
kd_mat <- log(kd_mat)
hist(kd_mat)

kd_mat <- t(kd_mat)
image(kd_mat)

kd_mat <- rotate(kd_mat)





##### Multiple Regression N ~ (SST, Chl) #####
fit <- lm(north_east_pacific2$N ~ north_east_pacific2$SST + north_east_pacific2$chl, north_east_pacific2)
summary(fit)
plot(fit)



##### Cluster Solutions #####
# Data Preparation
mydata <- subset(north_east_pacific[,c(4,5,2)])
mydata <- na.omit(mydata) # listwise deletion of missing data
mydata <- scale(mydata) # standardize variables
rownames(mydata) <- mydata[,3]
mydata_Z <- mydata[,c(1,2)]

d <- dist(mydata_Z, method="euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit)
groups()

hc <- hclust(dist(mydata_Z), "ward.D2")
par(mfrow=c(1,1))
plot(cut(as.dendrogram(hc),h=50)$lower)






# K-Means Cluster
fit <- kmeans(mydata, 3)
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# K-Means Simple
library(cluster)
library(fpc)
clus <- kmeans(mydata, centers=3)
plotcluster(mydata, clus$cluster)

# Multiple Variables
library(reshape2)
result <- dcast(mydata, N ~ SST + chl)
result$clust <- kmeans(result[,2:ncol(result)],centers=3)$clust
result[sample(1:100,6),]
plot(c2~c1,result,col=result$clust,pch=20)
head(mydata)

# Ward Hierarchial Clustering
d <- dist(mydata_Z, method="euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
fitD <- as.dendrogram(fit)

# display dendrogram, upper height 50
plot(fit)
par(1)
plot(cut(fitD, h=50)$lower)
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendrogram with red borders around 3 clusters
rect.hclust(fit, k=3, border="red")

# Model Based
library(mclust)
fit <- Mclust(mydata)
plot(fit)
summary(fit)

# 











##### Data Subsets by Season #####
# Spring Data Subset Plot
spring_data <- subset(data, data$month >= 3 & data$month <= 5)
plot(spring_data$SST, spring_data$N, main="N vs SST - Spring", xlab="Temperature", ylab="Nitrate")

# Summer Data Subset Plot
summer_data <- subset(data, data$month >= 6 & data$month <= 8)
plot(summer_data$SST, summer_data$N, main="N vs SST - Summer", xlab="Temperature", ylab="Nitrate")

# Fall Data Subset Plot
fall_data <- subset(data, data$month >= 9 & data$month <= 11)
plot(fall_data$SST, fall_data$N, main="N vs SST - Fall", xlab="Temperature", ylab="Nitrate")

# Winter Data Subset Plot
winter_data <- subset(data, data$month == 12 | data$month == 1 | data$month == 2)
plot(winter_data$SST, winter_data$N, main="N vs SST - Winter", xlab="Temperature", ylab="Nitrate")




##### Plot Region by Season #####
# Plot for North-East Pacific - Spring, temp
north_east_pacific_spring <- subset(spring_data, (spring_data$lat < 60 & spring_data$lat > 20) & (spring_data$lon > -170 & spring_data$lon < -110))
plot(north_east_pacific_spring$SST, north_east_pacific_spring$N)
plot(north_east_pacific_spring$chl, north_east_pacific_spring$N)



# Map Plot
plot(newmap, xlim = c(-180, 180), ylim = c(-80, 90), asp = 1)

# Plot on Top of Map - Spring
points(x=spring_data$lon, y=spring_data$lat, col = "red", cex = .3)

# Plot on Top of Map - North-East Pacific - Spring
points(x=north_east_pacific_spring$lon, y=north_east_pacific_spring$lat, col = "blue", cex = .3)

# Color Palette - Blue~Red
rbPal <- colorRampPalette(c("red", "blue"))
north_east_pacific_spring$col <- rbPal(10)[as.numeric(cut(north_east_pacific_spring$lat,breaks = 10))]
plot(temperature <- north_east_pacific_spring[,4], nitrate <- north_east_pacific_spring[,5], col = north_east_pacific_spring$col)

# Color Blue/Red
plot(temperature <- north_east_pacific_spring[,4], nitrate <- north_east_pacific_spring[,5], col = ifelse(north_east_pacific_spring$lat>45,"blue","red"))

# Northern Pacific Subset - Spring
northern_pacific_spring <- subset(north_east_pacific_spring, north_east_pacific_spring$lat < 60 & north_east_pacific_spring$lat > 45)
plot(temperature <- northern_pacific_spring[,6], nitrate <- northern_pacific_spring[,5])

# Multiple Regression - Temp + Chl
fit <- lm(northern_pacific_spring[,5] ~ northern_pacific_spring[,4] + northern_pacific_spring[,6], northern_pacific_spring)
summary(fit)


