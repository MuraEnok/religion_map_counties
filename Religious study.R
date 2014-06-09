library(foreign)
library(plyr)
library(maps)
#http://www.thearda.com/download/download.aspx?file=U.S.%20Religion%20Cens
url = "http://www.thearda.com/download/download.aspx?file=U.S.%20Religion%20Census%20Religious%20Congregations%20and%20Membership%20Study,%202010%20(County%20File).DTA"
relig <- read.dta(url)
# this gets a trimmed master description file to use as a look tool
desc <- read.csv("/Users/ak/Data/religion/orthodox_codes.csv", sep=",", header=TRUE)

# reli <- read.dta("/Users/ak/Data/Religion.DTA")
# this data grabs the total count for orthodox churchs definitions found in religion.TXT
orthodox <- c(16, 17, 18, 562, 563, 564, 565, 566, 567, 568)
# this show the total views of all orthodox churches cannonical or not in the survey from religion.TXT
orthodoxAll <- c(16, 17, 18, 29, 30, 31, 49, 50, 51, 57, 58, 59, 113, 114, 115, 222, 223, 224, 235, 236, 237, 238, 291, 292, 293,
              296, 297, 298, 314, 315, 316, 347, 348, 349, 350, 351, 352, 353, 354, 355, 420, 421, 422, 429, 430, 431,
              480, 481, 482, 483, 484, 485, 492, 493, 494, 508, 509, 510, 518, 519, 520, 547, 548, 549, 564, 565, 566, 567, 568)

state <-  c("OR", "WA", "ID", "CA", "MT", "UT", "LA", "WY")
# change the list here for total or summary files

reli <- relig[, orthodox]
reli <- subset(reli, !(reli$stabbr %in% c("AK", "HI")))

# this is for getting all counties with an orthodox church
reli <- reli[complete.cases(reli$orthcng),]
reli <- reli[ order(-reli$POP2010), ]

# this is for getting all counties without an orthodox church
reli <- reli[!complete.cases(relig$orthcng), ]
reli <- reli[ order(-reli$POP2010), ]
reli <- subset(reli, stabbr %in% state)

#test <- subset(reli, stabbr == "WA")





#county.fips <- cbind(county.fips, df)
#testWa <- subset(county.fips , )
# create a map of all counties with and without an orhtodox church
library(maps)
library(mapproj)
data(county.fips)
# create list of polyname as temp
temp <- strsplit(as.character(county.fips$polyname), ",")
# create matic of list
mat <- matrix(unlist(temp), ncol=2, byrow=TRUE)
temp.df <- as.data.frame(mat)
county.fips$state <- as.character(temp.df$V1)
h <- county.fips
h <- sqldf('select * from h where state ="washington" ')


colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
# reli <- reli[complete.cases(reli$fips),]
reli$o <- orthcng
reli$o[is.na(reli$orthcng)] <- 1
reli$o[!is.na(reli$orthcng)] <- 6
reli$colorBuckets <- reli$o

# test the data to remove virgina city fips codes and the duplication of fips in dates after that 

reli2 <- duplicated(reli$fips)

fips.test2 <- join(reli, county.fips, by = "fips", type="left"  )
fips.test2 <- fips.test2[complete.cases(fips.test2$polyname), 1:12]
reliX <- fips.test2

colorsmatched <- reli$colorBuckets[ match( county.fips$fips, reli$fips)]


fips.test <- join(county.fips, reli, by = "fips", type="left" , match = "all")


colorsmatched <- reli$colorBuckets[ match( county.fips$fips, reliX$fips)]
write.csv(fips.test, "fips.test.csv")
write.csv(fips.test2, "fips.test2.csv")

write.csv(county.fips, "county.fips.csv")
write.csv(reli, "reli.csv")
wa <- subset(reliX, stabbr == "WA")

map( 
  "county" , "washington",
  col = colors[ colorsmatched ] ,
  fill = TRUE , 
  resolution = 0 , 
  lty = 0 , 
  projection = "polyconic"
)
map(
  "state" , "washington",
  col = "black" , 
  fill = FALSE , 
  add = TRUE , 
  lty = 1 , 
  lwd = 1 , 
  projection = "polyconic"
)


# create the legend's text
# print the number of doctors per ten thousand people to the screen
#cut.points * 10000
leg.txt <- c( "< 150" , "151-200" , "201-250" , "251-320" , "321-420" , " > 421" )

#draw county chloropleth map
map( 
  "county" , 
  col = colors[ colorsmatched ] ,
  fill = TRUE , 
  resolution = 0 , 
  lty = 0 , 
  projection = "polyconic"
)
reli_la <- subset(reli, stabbr == "WA")

write.csv(reli, "/Users/ak/Data/orthodox deserts.csv")