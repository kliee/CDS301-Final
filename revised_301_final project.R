library(plotly)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(gganimate)
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(readr)
library(leaflet)
library(readxl)
library(htmlwidgets)
library(cartogram)
library(tidyverse)
library(RColorBrewer)

rm(list=ls())

price <- read.csv("real estate market price in Seoul.csv", header=TRUE, fileEncoding = 'euc-kr', encoding = 'utf-8')

#gu, dong, price, year
price <- price[, c(1,2,3,4,5,12)]


colnames(price) <- c("year","cd1", "gu","cd2", "dong", "price")
price$cd <- paste(price$cd1, price$cd2, sep = "")

avg_2010 <- price %>%
  filter(year == 2010) %>%
  group_by(cd, gu, dong) %>%
  summarize(avg = mean(price, na.rm = T)) %>%
  ungroup()

avg_2020 <- price %>%
  filter(year == 2020) %>%
  group_by(cd, gu, dong) %>%
  summarize(avg = mean(price, na.rm = T)) %>%
  ungroup()

# Merge datasets based on gu and dong
merged <- inner_join(avg_2010, avg_2020, by = c("cd"), suffix = c("_2010", "_2020"))

# Calculate the difference in average_price
merged$price_diff <- merged$avg_2020 - merged$avg_2010

merged <- merged[, -c(2,3)]
merged <- rename(merged, gu=gu_2020)
merged <- rename(merged, dong=dong_2020)
merged$loca <- paste(merged$gu, merged$dong, sep = " ")

install.packages("translateR")
library(translateR)


library(sf)
library(leaflet)
library(raster)

options(scipen=999)
format(99999999,scientific = FALSE)

dong <- st_read("emd.shp", quiet=T)

dong<-rename(dong, cd=EMD_CD)
dong$cd <- paste0(dong$cd, "00")
dong <- subset(dong, select = -EMD_KOR_NM)

dong <- dong %>% select_if(~!all(is.null(.)))

dong_sf <- merge(merged, dong, by =("cd"))

dong_sf <- na.omit(dong_sf)

dong_sf <- st_as_sf(dong_sf)

centroids <- st_centroid(dong_sf)

##translate===============================================================================
# Install and load the stringi package
install.packages("stringi")
library(stringi)

# Define a translation function
translate_to_english <- function(text) {
  result <- stri_trans_list(text, from = "ko", to = "en")
  return(result)
}

# Assuming your text is in the 'loca' column
dong_sf$loca <- sapply(dong_sf$loca, translate_to_english)

# Print the translated data frame
print(dong_sf)











#Cartogram=======================================================================
library(sp)
library(raster)
library(sf)

num_ranks <- 30

# Create a color palette with num_ranks colors
color_palette <- colorRampPalette(c("blue", "red"))(num_ranks)

# Mutate the rank variable
dong_sf <- dong_sf %>%
  mutate(price_diff_rank = ntile(price_diff, num_ranks))

dong_sp <- as_Spatial(dong_sf)      # Error: could not find function "as_Spatial"

geometry_sp <- sf:::as_Spatial(dong_sf$geometry) # This works

par(new = TRUE)  
text(x = mean(bbox(dong_sp)[, 1]), y = min(bbox(dong_sp)[, 2]) - 0.02 * (max(bbox(dong_sp)[, 2]) - min(bbox(dong_sp)[, 2])),
     labels = "The difference in apartment prices between 2010 and 2020 was used as a variable.\nThe range of changes in apartment prices in each region of Seoul is distinguished by color.\nThe darker the color, the greater the increase in apartment prices.",
     adj = 0.5, cex = 0.8, font = 2, col = "black")

# Plot the SpatialPolygonsDataFrame with color based on the rank variable
spplot(dong_sp, "price_diff_rank", col.regions = color_palette, 
       main = "Seoul Apartment Price Diffrernce Ranking 2010-2020", key.space = "right", 
       scales = list(draw = TRUE), colorkey = list(space = "right", width = 1.5))


library(sp)
library(raster)
library(sf)
library(tmap)

# Assuming you have already loaded the 'dong_sf' data frame

num_ranks <- 30

# Create a color palette with num_ranks colors
color_palette <- colorRampPalette(c("blue", "red"))(num_ranks)

# Mutate the rank variable
dong_sf <- dong_sf %>%
  mutate(price_diff_rank = ntile(price_diff, num_ranks))

# Convert to SpatialPolygonsDataFrame
dong_sp <- as_Spatial(dong_sf)

# Create a thematic map with tmap
tm <- tm_shape(dong_sp) +
  tm_fill("price_diff_rank", palette = color_palette, title = "Price Difference Rank") +
  tm_borders() +
  tm_layout(main.title = "Seoul Apartment Price Difference Ranking 2010-2020")

# Add a legend
tm_legend(position = c("right", "top"), title = "Legend")

# Print the map
print(tm)





##choropleth map======================================================================
library(sp)
library(raster)
library(sf)
library(tmap)

num_ranks <- 30
dong_sf <- dong_sf %>%
  mutate(price_diff_rank = ntile(price_diff, num_ranks))

dong_sp <- as_Spatial(dong_sf)


tm <- tm_shape(dong_sp) +
  tm_fill("price_diff_rank", palette = "YlGnBu", title = "Price Difference Rank") +  # Using a darker palette "YlGnBu"
  tm_borders() +
  tm_layout(main.title = "Seoul Apartment Price Difference Ranking 2010-2020") +
  tm_credits("The difference in apartment prices between 2010 and 2020 was used as a variable. The range of changes in apartment prices in each region of Seoul is distinguished by color. The darker the color, the greater the increase in apartment prices.")


tm_legend(position = c("right", "top"), title = "Legend")


print(tm)




