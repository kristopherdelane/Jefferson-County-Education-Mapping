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
eddata<-acs.fetch(endyear = 2015, span = 5, geography = geo,
                  table.number = "B15001", col.names = "pretty")

# Explore the data pulled from the ASC package
names(attributes(eddata))
attr(eddata, "acs.colnames")

# Turn it into a dataframe and preserve the GEOID 
ed_df<-data.frame(paste0(str_pad(eddata@geography$state, 2, "left", pad = "0"),
                         str_pad(eddata@geography$county, 3, "left", pad = "0"),
                         str_pad(eddata@geography$tract, 6, "left", pad = "0")),
                  eddata@estimate[,c(1,3,16,17,18,24,25,26,32,33,34,35,44,57,58,59,65,66,67,73,74,75,76)],
                  stringsAsFactors = FALSE)

# Remove row names
rownames(ed_df)<-1:nrow(ed_df)

# Create a new dataframe that is aggregated data based on education level, also create a Percentage.
ed<- as.data.frame(ed_df[,1])
ed$tot<- ed_df[,2]-ed_df[,3]-ed_df[,12]-ed_df[,13]-ed_df[,24]
ed$ass<-ed_df[,4]+ed_df[,7]+ed_df[,10]+ed_df[,15]+ed_df[,18]+ed_df[,21]
ed$bac<-ed_df[,5]+ed_df[,8]+ed_df[,11]+ed_df[,16]+ed_df[,19]+ed_df[,22]
ed$grad<-ed_df[,6]+ed_df[,9]+ed_df[,12]+ed_df[,17]+ed_df[,20]+ed_df[,23]
names(ed)<- c("GEOID", "Total","Associate","Bachelors","Graduate")
ed$Percentage<-round(100*(ed$Associate+ed$Bachelors+ed$Graduate)/ed$Total,0)

# Merge the data with Mapping data and remove areas without area.
ed_merge<- geo_join(tracts, ed, "GEOID","GEOID")
ed_merge<- ed_merge[ed_merge$ALAND>0,]

# Create a popup box for interactivity.
popup<- paste0("GEOID: ", ed_merge$GEOID,"<br> Percentage of Population with Associate or higher:", ed_merge$Percentage)
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = ed_merge$Percentage)

# Create a leaflet map that can be exported to HTML
map3<-leaflet()%>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ed_merge,
              fillColor = ~pal(Percentage),
              color = "#b2aeae",
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal=pal,
            values = ed_merge$Percentage,
            position = "topleft",
            title = "Percentage of Population <br> in Jefferson County, KY <br> with Associate or higher",
            labFormat = labelFormat(suffix = "%"))

# View the Map
map3


