# script for querying the religious census and creating a map 
library(foreign)
library(devtools) # load the latest version of Ari's package
install_github("choroplethr", "trulia")
library(choroplethr)
library(ggplot2) # this seems to help choroplethr run better
library(dplyr) # this is becoming my goto data processing tool though I miss proc sql;

#http://www.thearda.com/download/download.aspx?file=U.S.%20Religion%20Cens
url = "http://www.thearda.com/download/download.aspx?file=U.S.%20Religion%20Census%20Religious%20Congregations%20and%20Membership%20Study,%202010%20(County%20File).DTA"
relig <- read.dta(url)

# this data grabs the total count for orthodox churchs definitions found in religion.TXT
orthodox <- c(16, 17, 18, 562, 563, 564, 565, 566, 567, 568)

# this trims the dataset, removes the na, orders by most population counties and changes the names for choroplethr mapping
reli <- select(relig, orthodox) %.%
       # filter(!( stabbr %in% c("AK", "HI"))) %.% # newer version has AK & HI inset nice
        filter(complete.cases(orthcng)) %.%
        arrange(-POP2010) %.%
        mutate(region = fips, value = orthcng )

# map using choroplethr 
 choroplethr(reli, "county", num_buckets=8, title="Eastern Orthodox Congregations by County 2010")
