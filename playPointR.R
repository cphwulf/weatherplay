library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

apikey="VIv3QlwKGcXuQoMPZ5vpm5EVF7TOSbPL"
urlcc = paste("http://dataservice.accuweather.com/locations/v1/adminareas/DK?&details=true&apikey=",apikey)
rescc = httr::GET(urlcc)
rawcc = httr::content(rescc, as = "text")
dfcc = jsonlite::fromJSON(rawcc)

# find copenhagen
urlcph = "http://dataservice.accuweather.com/locations/v1/cities/search?apikey=VIv3QlwKGcXuQoMPZ5vpm5EVF7TOSbPL&q=Copenhagen"
rescph = httr::GET(urlcph)
dfcph <- httr::GET(urlcph) %>% 
  httr::content(as="text") %>% 
  jsonlite::fromJSON()

# current weather in copenhagen
urlcphcc = "http://dataservice.accuweather.com/currentconditions/v1/123094?apikey=VIv3QlwKGcXuQoMPZ5vpm5EVF7TOSbPL"

# current weather around point reyes
urlcphcc = "http://dataservice.accuweather.com/currentconditions/v1/2167028?apikey=VIv3QlwKGcXuQoMPZ5vpm5EVF7TOSbPL&details=true"
dfcph <- httr::GET(urlcphcc) %>% 
  httr::content(as="text") %>% 
  jsonlite::fromJSON()

# weekend weather around point reyes
urlpr = paste("http://dataservice.accuweather.com/forecasts/v1/daily/5day/2167028?details=true&metric=true&apikey=",apikey)
retval <- httr::GET(urlpr) %>% 
  httr::content(as="text") %>% 
  jsonlite::fromJSON() 
dfpr = retval$DailyForecasts

#### plot the forecast
# prepare data
dfn <- names(dfpr)
dfprsel <- dfpr[,c("Date","Temperature","AirAndPollen")]
uv = unlist(lapply(dfprsel$AirAndPollen, function(x) x[[2]][[6]]))
ragweed = unlist(lapply(dfprsel$AirAndPollen, function(x) x[[2]][[4]]))
dfprsel$max = dfprsel$Temperature$Maximum$Value
dfprsel$min = dfprsel$Temperature$Minimum$Value
dfprsel$uv = uv
dfprsel$ragragweed=ragweed
dfprsel2 = dfprsel[,-c(2)]
dfprsel2 = dfprsel2[,-c(2)]
dfprsel2$Date=gsub("T07:00:00-07:00","",dfprsel2$Date)

# to celcius
dfprsel2$max = unlist(lapply(dfprsel2$max, function(x) round((5/9)*(x-32),digits = 0)))
dfprsel2$min = unlist(lapply(dfprsel2$min, function(x) round((5/9)*(x-32),digits = 0)))
# data + aesthetics + geom

dfprsel3$idx=1:length(dfprsel2$max)
dfprsel2$idx=1:length(dfprsel2$max)


ggplot(dfprsel2,aes(idx))+
  geom_line(aes(y=min,color="min")) +
  geom_line(aes(y=max,color="max")) +
  geom_line(aes(y=ragweed,color="ragweed"))+
  geom_line(aes(y=uv,color="uv"))  +
  labs(title="Point Reyes Forecast",x="Days",y="Temp/mold/uv")

dfprsel3 = dfprsel2[, -c(1)]
