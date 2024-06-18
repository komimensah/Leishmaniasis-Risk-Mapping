library(terra)
library(raster)

# Set the path to the raster files and load them as a SpatRaster
raster_path <- "/Users/komim.agboka/Desktop/Ecological modelling-SI/Variables/"
rasters <- rast(list.files(raster_path, pattern = "\\.tif$", full.names = TRUE))

# If you need to see the names and check the rasters, you can print this:
names(rasters)

# Assume the names correspond correctly to your variables (Bio1, Bio12, Bio13, SoilMoisture,etc)
bio1 <- rasters[["Bio1"]]
bio12 <- rasters[["Bio12"]]
bio13 <- rasters[["Bio13"]]
soilMoisture <- rasters[["SoilMoisture"]]
Elevation<-rasters[["Elevation"]]
populationDensity<-rasters[["populationDensity"]]
NDVI <- rasters[["NDVI"]]
GRDI <- rasters[["GRDI"]]
# Membership Functions for GRDI
IGRDI <- ifel(GRDI < 20, 0.1,
              ifel(GRDI < 40, 0.3,
                   ifel(GRDI < 60, 0.5,
                        ifel(GRDI < 80, 0.7, 1))))

# NDVI Membership Functions
INDVI <- ifel(NDVI < 0.3, 0,
              ifel(NDVI < 0.4, 0.5,
                   ifel(NDVI < 0.6, 0.7, 1)))
# Bio1 Membership Functions
Ibio1 <- ifel(bio1 < 0, 0,
              ifel(bio1 < 8, 0,
                   ifel(bio1 < 23, 0.55,
                        ifel(bio1 <= 30, 1, 1))))

# Bio12 Membership Functions
Ibio12 <- ifel(bio12 < 474, 0.75,
               ifel(bio12 < 862, 0.50,
                    ifel(bio12 < 1207, 0.30,
                         ifel(bio12 > 2026, 0.1, 0.1))))

# Bio13 Membership Functions
Ibio13 <- ifel(bio13 < 60, 0.20,
               ifel(bio13 < 129, 0.40,
                    ifel(bio13 < 198, 0.55,
                         ifel(bio13 <= 525, 0.80, 1))))

# Soil Moisture Membership Functions
ISoilm <- ifel(soilMoisture < 0, 0.40,
               ifel(soilMoisture < 113, 0.50,
                    ifel(soilMoisture < 299, 0.60, 0.70)))

# Elevation Moisture Membership Functions
IElev <- ifel(Elevation > 486, 0.1,
              ifel(Elevation < 428, 1,1))

# Population Density Membership Functions
IPopDensity <- ifel(populationDensity < 50, 0.2,
                    ifel(populationDensity < 200, 0.5,
                         ifel(populationDensity < 500, 0.7, 1)))
# Calculate the combined fuzzy index

# Example weights, adjust based on the importance of each variable
weight_bio1 <- 0.22
weight_bio12 <- 0.13
weight_bio13 <- 0.13
weight_soilMoisture <- 0.18
weight_Elevation <- 0.12
weight_populationDensity <- 0.09
weight_NDVI <- 0.08
weight_GRDI <- 0.05
IFuzzy <- (IGRDI * weight_GRDI) + (INDVI * weight_NDVI)+ (Ibio1 * weight_bio1) + (Ibio12 * weight_bio12) + (Ibio13 * weight_bio13) + (ISoilm * weight_soilMoisture)+ (IElev * weight_Elevation)+ (IPopDensity * weight_populationDensity)


print(summary(Ibio1))
print(summary(Ibio12))
print(summary(Ibio13))
print(summary(ISoilm))
print(summary(IElev))
print(summary(IPopDensity))
print(summary(INDVI))
plot(IFuzzy)

# Optionally save the result as a new raster
writeRaster(IFuzzy, "/Users/komim.agboka/Desktop/Ecological modelling-SI/Variables/IFuzzy_1.tif")
