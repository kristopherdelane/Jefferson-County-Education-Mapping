####
# Install packages as needed to run the following:
####

library(tigris)
library(acs)
library(stringr)
library(leaflet)
library(rgdal)
library(ggplot2)
library(dplyr)

#Install API key
api.key.install(key = "YOUR KEY HERE")

# The FIPS county code for Jefferson County
counties<-111

# Set this for Mapping, State and County
tracts<- tracts(state = 'KY', county = 111, cb = TRUE)

# Set this for pulling data
geo<- geo.make(state = c("KY"),
               county = 111, tract="*")

# Pull the data from ACS API
incomedata<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                  table.number = "B19001", col.names = "pretty")

# Explore the data pulled from the ASC package
names(attributes(incomedata))
attr(incomedata, "acs.colnames")

# Turn it into a dataframe and preserve the GEOID 
inc_df<-data.frame(paste0(str_pad(incomedata@geography$state, 2, "left", pad = "0"),
                         str_pad(incomedata@geography$county, 3, "left", pad = "0"),
                         str_pad(incomedata@geography$tract, 6, "left", pad = "0")),
                  incomedata@estimate[,c(1:10)],
                  stringsAsFactors = FALSE)

# Remove row names
rownames(inc_df)<-1:nrow(inc_df)

# Create a new dataframe that is aggregated data based on education level, also create a Percentage.
inc<- as.data.frame(inc_df[,1])
inc$tot<- inc_df[,2]
inc$Less50k<-rowSums(inc_df[,3:11])
names(inc)<- c("GEOID", "Total","Less50k")
inc$Percentage<-round(100*(inc$Less50k)/inc$Total,0)

# Merge the data with Mapping data and remove areas without area.
inc_merge<- geo_join(tracts, inc, "GEOID","GEOID")
inc_merge<- inc_merge[inc_merge$ALAND>0,]

# Create a popup box for interactivity.
popup<- paste0("GEOID: ", inc_merge$GEOID,"<br> Percentage of Household Income Less than $50,000:", inc_merge$Percentage)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = inc_merge$Percentage)

# Create a leaflet map that can be exportinc to HTML
map3<-leaflet()%>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=inc_merge,
              fillColor = ~pal(Percentage),
              color = "#b2aeae",
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal=pal,
            values = inc_merge$Percentage,
            position = "topleft",
            title = "Percentage of Households <br> in Jefferson County, KY <br> less than $50,000",
            labFormat = labelFormat(suffix = "%"))

# View the Map
map3


