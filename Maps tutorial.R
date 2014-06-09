library(maps)

data(state)
data(county.fips)
#map('county', fill=FALSE, boundry=TRUE, col="red")

mapnames <- map("state", plot=FALSE)$names
# mapnames <- map("county", plot=FALSE)$names

region_list <- strsplit(mapnames, ':')

mapnames2 <- sapply(region_list, "[", 1)
m <- match(mapnames2, tolower(state.name))

map.area <- state.area[m]

clr <- rev(heat.colors(8))

area.buckets <- cut(map.area, breaks=8)

map('state', fill=TRUE, col=clr[area.buckets])


choro <- c( "red",  "red",  "red" ,"blue" ,"blue", "blue",  "red")
require(maps)
data(state)
names(choro) <-names(state)[1:7]
map("state",  lty = 1, lwd =1,
    boundary=TRUE, fill=TRUE,
    col=choro)
data(county.fips)
names(choro) <- names(county.fips)[1:7]
map("county", lty=1, lwd = 1, boundry = TRUE, fill=TRUE, col = choro)


# Extract mapnames for States
mapnames <- data.frame(
  state = map("state",plot=FALSE)[4]$names
  , col = sample(c("pink", "purple", "lavender", "blue"), 63, replace = TRUE)
)

#Plot the colors
map("state", regions = mapnames$state, col = mapnames$col, fill = TRUE, lty = 1, lwd= 1)
#Plot the outlines
map("state", regions = mapnames$state, col = "black", fill = FALSE, add = TRUE, lty = 1, lwd = 1)

library(ggplot2)
unemp <- read.csv("unemployment09.csv", header=F, sep=",",stringsAsFactors=F)
names(unemp) <- c('id', 'state-fips', "county-fips", 'name', 'year', '?', '?', '?', 'rate')

unemp$county <- tolower(gsub(" County, [A-Z]{2}", "", unemp$name))
unemp$state <- gsub("^.*([A-Z]{2}).*$", "\\1", unemp$name)

county_df <- map_data('county')
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")
county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]
county_df$state_name <- NULL

state_df <- map_data("state")

# combine together

choropleth <- merge(county_df, unemp, bu = c("state", "county"))
choropleth <- choropleth[order(choropleth$order),]


require(mapproj)
  map(’state’, proj = ’bonne’, param = 45) # Bonne equal-area projection of states

# because the projection is rectangular, these are not true areas on the globe.
m = map("state", fill = TRUE, plot = FALSE)
map('county',  fill=T, col = colors)
map('county',  c('washington', 'oregon') , fill=T, col = colors)colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")

data(unemp)
data(county.fips)
# define color buckets
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    county.fips$polyname)]
colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")
title("unemployment by county, 2009")
legend("topright", leg.txt, horiz = TRUE, fill = colors)
# Choropleth Challenge example, based on J’s solution, see:
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# To see the faint county boundaries, use RGui menu: File/SaveAs/PDF
}