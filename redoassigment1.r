#install.packages("tidyverse")
#library(tidyverse)

#read in all three data set, note : always have stringsAsFactors FALSE 
gw <- read.csv("gw_data.csv", na.strings = c("NA" , "N/A" ), stringsAsFactors = FALSE)
nat <- read.csv('nat_geo_data.csv', na.strings = c("NA" , "N/A" ), stringsAsFactors = FALSE)
aud <- read.csv('audubon_data.csv', na.strings = c("NA" , "N/A" ), stringsAsFactors = FALSE)

# mutate all rows*columns by applying: NA if is empty space
gw <- mutate_all(gw, list(~na_if(.,"")))
# check to see if there are any NA, should be sum > 0 if have NAs
sum(is.na(gw))

# same as above
nat <- mutate_all(nat, list(~na_if(.,"")))
sum(is.na(nat))

# same as above
aud <- mutate_all(aud, list(~na_if(.,"")))
sum(is.na(aud))

# remove the rows with NA
gw <- na.omit(gw)
# sum should now be 0 since all NA rows have been removed
sum(is.na(gw))

# same as above
nat <- na.omit(nat)
sum(is.na(nat))

# same as above
aud <- na.omit(aud)
sum(is.na(aud))

# creating a function to apply to lat and long columns where it will take
# the value of the cell and "x" "c" argument as either 'W' or 'N' to apply
# to the cells value.[] means indexing. 
#Indexing means selecting a subset of the elements in order to use them 
#in further analysis or possibly change them
convertToDecimalDegree <- function(x, c)
{
  # split the data "77_1.11 W" to "77" and "1.11 W"
  r <- strsplit(x,"_")[[1]]
  # clean up the decimal portion "1.11 W" to "1.11"
  r[2] <- trimws(gsub(c,'',r[2]))
  # convert the to numbers and do the calculation to get the final decimal degree
  #can create one single code for ifelse
  ##make the minutes part of the West coordinates negative## 
  dir <- ifelse(as.numeric(r[1]) > 0,-1,1)
  x <- (as.numeric(r[1]) * ifelse(c == "W",-1,1) * dir) + (as.numeric(r[2]) / 60)
  # ensure it is only 5 decimal points
  x <- format(round(x, 5), nsmall = 5)
  return(x)
}
# convert all survey type to lowercase and remove underscores
nat$Survey_Type <- tolower(gsub('_','',nat$Survey_Type))

# modify the data for gw table where the date needs formatting
# and the lat and long need cleaning and conversion.
gw$Date <- as.Date(gw$Date,format="%d-%b-%y")
gw$Longitude <- lapply(gw$Longitude,convertToDecimalDegree, "W")
gw$Latitude <- lapply(gw$Latitude,convertToDecimalDegree, "N")
gw$Survey_Type <- tolower(gsub('_','',gw$Survey_Type))
gw

# modify the data for aud where date needs re-formatting and a
# simple string manipulation to cleanup the lat long by removing
# the 'N' and 'W' for '' and '-1'
aud$Date <- as.Date(aud$Date, format = "%m/%d/%y")
aud$Longitude <- gsub('W', "-", aud$Longitude)
aud$Latitude <- gsub('N', "", aud$Latitude)
aud$Survey_Type <- tolower(gsub('_','',aud$Survey_Type))
aud

# final data combined
final = rbind(gw,aud,nat)

# using tidyvers filter to filter the data
final <- filter(final, Date > "2010-01-01" & Survey_Type == "transect")
final$Longitude <- as.numeric(final$Longitude)
final$Latitude <- as.numeric(final$Latitude)
write.csv(final, file="final_data.csv", row.names = FALSE)

#####################################################################
#make sure to sue "" when installing a package
#install.packages("sp") 
#library(sp)
#install.packages("rgdal")
#library(rgdal)

#REMOVE local path
clean_data <- read.csv("final_data.csv")

plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])

#Map of DC neighborhoods from maps2.dcgis.dc.gov
#create a folder Neighborhood_Clusters-shp, in that folder have all cluster dl
dc <- readOGR("Neighborhood_Clusters-shp", "Neighborhood_Clusters")

#Plot the map of DC
par(mar = c(1, 1, 1, 1))

plot(
  dc,
  col = "darkgrey",
  border = "white",
  main = "District of Columbia Bird Sightings"
)
plot(dc[46, ],
     add = TRUE,
     col = "#718BAE80",
     border = "white")


#Add your data
plot(plotting_data,
     add = TRUE,
     pch = 16,
     cex = 0.25)