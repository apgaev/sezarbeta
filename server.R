library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library(randomForest)
options(shiny.sanitize.errors = TRUE)
load("rfModel.Rdata")
load("bag.boston.Rdata")
load("analtodel.Rdata")
load("theforest.Rdata")
cities <- read.csv2("cities.csv")
citiesnumbers <- read.csv2("cities1.csv")
#withgroup <- read.csv2("withgroup.csv")
withgroupy <- read.csv2("withgroupy.csv")
#analysetrain <- read.csv2("analysenumberstrain.csv")
analt <- read.csv2("analt.csv")
#maindata <- read.csv2("maindata.csv")
datist <- read.csv2("datist.csv")
cities <- select(cities, -c(X))
citiesnumbers <- select(citiesnumbers, -c(X))
#withgroup <- select(withgroup, -c(X))
withgroupy <- select(withgroupy, -c(X))
#analysetrain <- select(analysetrain, -c(X))
analt <- select(analt, -c(X))
#maindata <- select(maindata, -c(X))
datist <- select(datist, -c(X))
#analysetrainodel <-randomForest(factor(clusters) ~ ., data=analysetrain)
#bag.boston=randomForest(price~.,data=maindata, mtry=13, importance=TRUE)
server <- function(input, output, session) {
  data <- eventReactive(input$count, {
    fordistance = filter(cities, load_city ==  input$load_city | load_city == input$unload_city)
    fordistance = filter(fordistance, unload_city ==  input$load_city | unload_city == input$unload_city)
    forstartcoord = filter(citiesnumbers, unload_city == input$load_city)
    fordestcoord = filter(citiesnumbers, unload_city == input$unload_city)
    distance <- fordistance$distance
    start_lat <- as.numeric(head(forstartcoord$lat.y, n=1))
    start_lon <- as.numeric(head(forstartcoord$long.y, n=1))
    dest_lat <- as.numeric(head(fordestcoord$lat.y, n=1))
    dest_lon <- as.numeric(head(fordestcoord$long.y, n=1))
    from_msc <- as.logical(input$load_city == "Moscow")
    to_msc <- as.logical(input$unload_city == "Moscow")
    from_saintp <- as.logical(input$load_city == "St. Petersburg")
    to_saintp <- as.logical(input$unload_city == "St. Petersburg")
    to_east <- as.numeric(dest_lon - start_lon)
    to_south <- as.numeric(dest_lat - start_lat)
    if (to_msc == FALSE) {
     forgroup <- filter(withgroupy, unload_city == input$unload_city)
     thegroup <- forgroup$thegroup
    } else {
     forgroup <- filter(withgroupy, unload_city == input$load_city)
     thegroup <- forgroup$thegroup
    }
    volume <- as.numeric(input$volume)
    payment <- "безнал без НДС"
    loading_date <- as.integer(input$loading_date + 25569)
    transportation_duration <- input$duration
    weight <- as.numeric(input$weight)
    cargo_type <- input$cargo_type
    two_loadings <- as.logical(input$two_loadings)
    two_unloadings <- as.logical(input$two_unloadings)
    vehicle_type <- input$vehicle_type
    week <- as.integer(lubridate::week(input$loading_date))
    value <- input$value
    clusters <- as.factor(10)
    price <- as.integer(1)
    topredict <- data.frame(price, distance, value, volume, payment, loading_date, transportation_duration, weight, cargo_type, start_lat, start_lon, dest_lat, dest_lon, two_loadings, two_unloadings, vehicle_type, from_msc, to_msc, from_saintp, to_saintp, to_east, to_south, thegroup, week, clusters)
    topredict = select(topredict, -c(price))
    withpredict = rbind(analt, topredict)
    data.test = tail(withpredict, n=1)
    rfPredict<-predict(analtodel, data.test, probability=FALSE)
    topredict$clusters = as.factor(rfPredict)
    topredict$price <- as.integer(1)
    datacomplete = rbind(datist, topredict)
    data.test = tail(datacomplete, n=1)
    yhat.bag = predict(theforest,newdata=data.test)
    yhat.bag
  })
  
  output$price <- renderTable({
    data()
  })
}