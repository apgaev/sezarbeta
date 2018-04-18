library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(lubridate)
library(randomForest)
options(shiny.sanitize.errors = TRUE)

#load the random forest model, that predicts clusters
load("analtodel.Rdata")
#load the random forest model, that predicts the price
load("theforest.Rdata")
#load the dataset with the distances between major russian cities and its' coordinates
citiesnumbers <- read.csv2("cities1.csv")
citiesnumbers <- select(citiesnumbers, -c(X))
#load the dataset with cities and special groups, defined by logistics experts
withgroupy <- read.csv2("withgroupy.csv")
withgroupy <- select(withgroupy, -c(X))
#load the short version of the full dataset. Unfortunately I can not provide the whole dataset due to the privacy policy of my company.
analt <- read.csv2("analthead.csv")
datist <- read.csv2("dathead.csv")
analt <- select(analt, -c(X))
datist <- select(datist, -c(X))

server <- function(input, output, session) {
  data <- eventReactive(input$count, {
    #convert input data according to the model
    fordistance = filter(citiesnumbers, load_city ==  input$load_city | load_city == input$unload_city)
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
    
    #joining the input data with the original dataset made for the learning model in order to eleminate the new factor level error common for random forest
    withpredict = rbind(analt, topredict)
    data.test = tail(withpredict, n=1)
    
    #defining clusters that were previously gained from unsupervised learning model as a stage of data handling process
    rfPredict<-predict(analtodel, data.test, probability=FALSE)
    topredict$clusters = as.factor(rfPredict)
    
    topredict$price <- as.integer(1)
    
    #repeat the process of solving the random forest factor level error
    datacomplete = rbind(datist, topredict)
    data.test = tail(datacomplete, n=1)
    
    #completing the final prediction and receiving the result
    yhat.bag = predict(theforest,newdata=data.test)
    yhat.bag
  })
  
  output$price <- renderTable({
    data()
  })
}