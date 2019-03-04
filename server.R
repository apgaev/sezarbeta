library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
#library(lubridate)
library(randomForest)
library(shinyjs)
library(shinyalert)
library(reticulate)
library(tidyr)
library(jsonlite)
library(stringr)
use_python("/Users/antongaev/anaconda/bin/python", required = T)
options(shiny.sanitize.errors = TRUE)

#upload distance parser
source("~/Downloads/distances.R")

#upload the info and datasets chosen by admin to make calculations
the_model_to_use <- read.csv2("~/Downloads/cargo_type_module/the_model_to_use.csv")
models <- read.csv2("~/Downloads/cargo_type_module/models.csv")
models <- filter(models, user_model_name == as.character(the_model_to_use$the_model_to_use))

#if model contains clusters - upload the random forest model, that predicts clusters
if (is.na(models$cluster_model_name)==FALSE) {
    model <- paste0("~/Downloads/cargo_type_module/", models$cluster_model_name)
    load(model)
} 

#load the random forest model, that predicts the price
model <- paste0("~/Downloads/cargo_type_module/", models$server_model_name)
load(model)

server <- function(input, output, session) {
    #state reactive values
    region <- NULL
    makeReactiveBinding("region")
    city <- NULL
    makeReactiveBinding("city")
    street <- NULL
    makeReactiveBinding("street")
    latitude <- NULL
    makeReactiveBinding("latitude")
    
    #check for necessary columns
    columns_table <- read.csv2(file = paste0("~/Downloads/cargo_type_module/", as.character(models$columns_table)))
    for_colnames <- read.csv2(file = "~/Downloads/basic_column_names.csv")
    
    #hide inputs that not needed
    for (i in 1:nrow(for_colnames)) {
        addition <- grepl(for_colnames$basic_column_names[i], columns_table$parent_column_names)
        addition <- data.frame(addition)
        if (nrow(filter(columns_table, columns_table$column_name == as.character(for_colnames$basic_column_names[i])))==0) {
            if (nrow(filter(addition, addition == "TRUE"))==0) {
                shinyjs::hide(as.character(for_colnames$basic_column_names[i]))
            }
        }
    }
    
    #if it is a big city - show its streets
    observeEvent(input$load_city, {
        if (input$load_city == "Санкт-Петербург") {
            shinyjs::hide("moscow_streets")
            shinyjs::show("stp_streets")
        } else if (input$load_city == "Москва") {
            shinyjs::hide("stp_streets")
            shinyjs::show("moscow_streets")
        } else {
            shinyjs::hide("stp_streets")
            shinyjs::hide("moscow_streets")
            citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
            citiesnew <- select(citiesnew, region, city, latitude, longitude)
            citiesnew <- filter(citiesnew, city == input$load_city)
            output$load_city_dt = DT::renderDataTable(citiesnew)
        }
    })
    
    #the same for the destination
    observeEvent(input$unload_city, {
        if (input$unload_city == "Санкт-Петербург") {
            shinyjs::hide("moscow_streets_unload")
            shinyjs::show("stp_streets_unload")
        } else if (input$unload_city == "Москва") {
            shinyjs::hide("stp_streets_unload")
            shinyjs::show("moscow_streets_unload")
        } else {
            shinyjs::hide("stp_streets_unload")
            shinyjs::hide("moscow_streets_unload")
            citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
            citiesnew <- select(citiesnew, region, city, latitude, longitude)
            citiesnew <- filter(citiesnew, city == input$unload_city)
            output$load_city_dt_unload = DT::renderDataTable(citiesnew)
        }
    })
    
    observeEvent(input$add_new_location, {
        
        citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
        stpetersburgstreets <- read.csv2("~/Downloads/stpetersburgstreets.csv")
        moscowstreets <- read.csv2("~/Downloads/moscowstreets.csv")
        citiesnew <- select(citiesnew, -c(X))
        stpetersburgstreets <- select(stpetersburgstreets, -c(X))
        moscowstreets <- select(moscowstreets, -c(X))
        
        shinyalert(
            title = "Создать новую локацию",
            text = "Введите название региона (Москва, Санкт-Петербург, Московская, Татарстан и т.п.)",
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "input",
            inputType = "text",
            inputValue = "",
            inputPlaceholder = "",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            confirmButtonCol = "#00FFD9",
            cancelButtonText = "Cancel",
            timer = 0,
            imageUrl = "",
            animation = TRUE,
            callbackR = function(x) { if(x != FALSE) {
                region <- input$shinyalert
                
                shinyalert(
                    title = "Создать новую локацию",
                    text = "Введите название населенного пункта",
                    closeOnEsc = TRUE,
                    closeOnClickOutside = TRUE,
                    html = FALSE,
                    type = "input",
                    inputType = "text",
                    inputValue = "",
                    inputPlaceholder = "",
                    showConfirmButton = TRUE,
                    showCancelButton = TRUE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "#00FFD9",
                    cancelButtonText = "Cancel",
                    timer = 0,
                    imageUrl = "",
                    animation = TRUE,
                    callbackR = function(x) { if(x != FALSE) {
                        city <- input$shinyalert
                        
                        shinyalert(
                            title = "Создать новую локацию",
                            text = "Введите название улицы",
                            closeOnEsc = TRUE,
                            closeOnClickOutside = TRUE,
                            html = FALSE,
                            type = "input",
                            inputType = "text",
                            inputValue = "",
                            inputPlaceholder = "",
                            showConfirmButton = TRUE,
                            showCancelButton = TRUE,
                            confirmButtonText = "OK",
                            confirmButtonCol = "#00FFD9",
                            cancelButtonText = "Cancel",
                            timer = 0,
                            imageUrl = "",
                            animation = TRUE,
                            callbackR = function(x) { if(x != FALSE) {
                                street <- input$shinyalert
                                
                                shinyalert(
                                    title = "Создать новую локацию",
                                    text = "Введите координаты северной широты локации (разделитель - точка)",
                                    closeOnEsc = TRUE,
                                    closeOnClickOutside = TRUE,
                                    html = FALSE,
                                    type = "input",
                                    inputType = "text",
                                    inputValue = "",
                                    inputPlaceholder = "",
                                    showConfirmButton = TRUE,
                                    showCancelButton = TRUE,
                                    confirmButtonText = "OK",
                                    confirmButtonCol = "#00FFD9",
                                    cancelButtonText = "Cancel",
                                    timer = 0,
                                    imageUrl = "",
                                    animation = TRUE,
                                    callbackR = function(x) { if(x != FALSE) {
                                        latitude <- input$shinyalert
                                        
                                        shinyalert(
                                            title = "Создать новую локацию",
                                            text = "Введите координаты восточной долготы пункта (разделитель - точка)",
                                            closeOnEsc = TRUE,
                                            closeOnClickOutside = TRUE,
                                            html = FALSE,
                                            type = "input",
                                            inputType = "text",
                                            inputValue = "",
                                            inputPlaceholder = "",
                                            showConfirmButton = TRUE,
                                            showCancelButton = TRUE,
                                            confirmButtonText = "OK",
                                            confirmButtonCol = "#00FFD9",
                                            cancelButtonText = "Cancel",
                                            timer = 0,
                                            imageUrl = "",
                                            animation = TRUE,
                                            callbackR = function(x) {
                                                longitude <- input$shinyalert
                                                addition <- data.frame(region, city, street, latitude, longitude)
                                                if (city == "Санкт-Петербург") {
                                                    stpetersburgstreets <- rbind(stpetersburgstreets, addition)
                                                    write.csv2(stpetersburgstreets, "~/Downloads/stpetersburgstreets.csv")
                                                    updateSelectInput(session, "stp_streets", choices = stpetersburgstreets$street)
                                                } else if (city == "Москва") {
                                                    moscowstreets <- rbind(moscowstreets, addition)
                                                    write.csv2(moscowstreets, "~/Downloads/moscowstreets.csv")
                                                    updateSelectInput(session, "moscow_streets", choices = moscowstreets$street)
                                                } else {
                                                    citiesnew <- rbind(citiesnew, addition)
                                                    write.csv2(citiesnew, "~/Downloads/citiesnew.csv")
                                                    updateSelectInput(session, "load_city", choices = list("Санкт-Петербург", "Москва", остальные=citiesnew$city))
                                                }
                                            }
                                        )
                                        
                                    }}
                                )
                                
                            }}
                        )
                        
                    }}
                )
                
            }}
        )
        
    })
    
    data <- eventReactive(input$count, {
        #convert input data according to the model
        
        #state reactive values
        money_transfer_form <- NA
        #makeReactiveBinding("money_transfer_form")
        payment <- NA
        #makeReactiveBinding("payment")
        volume <- NA
        #makeReactiveBinding("volume")
        cargo_price <- NA
        #makeReactiveBinding("cargo_price")
        car_type <- NA
        #makeReactiveBinding("car_type")
        duration <- NA
        makeReactiveBinding("duration")
        calendar <- NA
        makeReactiveBinding("calendar")
        two_loadings.x <- NA
        makeReactiveBinding("two_loadings.x")
        two_loadings.y <- NA
        makeReactiveBinding("two_loadings.y")
        weight <- NA
        makeReactiveBinding("weight")
        diesel_price <- NA
        makeReactiveBinding("diesel_price")
        dollar <- NA
        makeReactiveBinding("dollar")
        cargo_type <- NA
        makeReactiveBinding("cargo_type")
        distance <- NA
        makeReactiveBinding("distance")
        
        #initialize empty dataset with empty input to fill it
        selected_dt <- data.frame(money_transfer_form, payment, volume, cargo_price, car_type, duration, calendar, two_loadings.x,
                                  two_loadings.y, weight, diesel_price, dollar, cargo_type, distance)
        
        #fill the dt
        selected_dt$money_transfer_form <- input$money_transfer_form
        selected_dt$payment <- as.factor(input$payment)
        selected_dt$volume <- input$volume
        selected_dt$cargo_price <- input$cargo_price
        selected_dt$car_type <- input$car_type
        selected_dt$duration <- input$duration
        selected_dt$calendar <- as.integer(input$calendar)
        selected_dt$two_loadings.x <- as.logical(input$two_loadings.x)
        selected_dt$two_loadings.y <- as.logical(input$two_loadings.y)
        selected_dt$weight <- input$weight
        
        #prepare for dollar/diesel parser
        addition <- grepl("diesel_price", columns_table$parent_column_names)
        dollar <- grepl("dollar", columns_table$parent_column_names)
        addition <- data.frame(addition, dollar)
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Загрузка курса доллара и цены на топливо", value = 0)
        
        #if there is either dollar or diesel_price - perform the calculaton
        #or pass
        if (nrow(filter(columns_table, columns_table$column_name == "diesel_price"))>0 | nrow(filter(columns_table, columns_table$column_name == "dollar"))>0 | nrow(filter(addition, addition == "TRUE"))!=0 | nrow(filter(addition, dollar == "TRUE"))!=0) {
                        #delete strings that cannot be defined through Yandex parser
                        progress$set(message = "Загрузка курса доллара и цены на топливо", value = 0)
                        
                        #deploy the parser
                        py_run_file("~/Downloads/usd_diesel.py")
                        jnastr240 = readLines("~/Downloads/usd_dollar.json") %>% 
                            str_c(collapse = ",") %>%  
                            (function(str) str_c("[", str, "]")) %>% 
                            fromJSON(simplifyDataFrame = T)
                        jnastr240<-jnastr240[[1]]
                        write.csv(jnastr240, "~/Downloads/usd_diesel.csv")
                        
                        selected_dt$diesel_price <- as.character(jnastr240$diesel_price[1])
                        selected_dt$diesel_price <- gsub("\\,", "\\.", selected_dt$diesel_price)
                        selected_dt$diesel_price <- as.numeric(selected_dt$diesel_price)
                        
                        selected_dt$dollar <- as.character(jnastr240$dollar[1])
                        selected_dt$dollar <- gsub("\\,", "\\.", selected_dt$dollar)
                        selected_dt$dollar <- as.numeric(selected_dt$dollar)
                        
        }
        
        #update remaining inputs
        selected_dt$cargo_type <- as.factor(input$cargo_type)
        city.x <- input$load_city
        
        #prepare for coordinates search
        citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
        stpetersburgstreets <- read.csv2("~/Downloads/stpetersburgstreets.csv")
        moscowstreets <- read.csv2("~/Downloads/moscowstreets.csv")
        citiesnew <- select(citiesnew, -c(X))
        stpetersburgstreets <- select(stpetersburgstreets, -c(X))
        moscowstreets <- select(moscowstreets, -c(X))
        
        if (input$load_city == "Санкт-Петербург") {
            region.x <- "Санкт-Петербург"
            street.x <- input$stp_streets
            stpetersburgstreets_filter <- filter(stpetersburgstreets, street == input$stp_streets)
            lat_from <- stpetersburgstreets_filter$latitude
            long_from <- stpetersburgstreets_filter$longitude
        } else if (input$load_city == "Москва") {
            region.x <- "Москва"
            street.x <- input$moscow_streets
            moscowstreets_filter <- filter(moscowstreets, street == input$moscow_streets)
            lat_from <- moscowstreets_filter$latitude
            long_from <- moscowstreets_filter$longitude
        } else {
            citiesnew_filter <- filter(citiesnew, city == input$load_city)
            region.x <- as.character(citiesnew_filter$region)
            street.x <- "Ленина"
            lat_from <- citiesnew_filter$latitude
            long_from <- citiesnew_filter$longitude
        }
        
        #add departure address basic info
        selected_dt$region.x <- region.x
        selected_dt$lat_from <- lat_from
        selected_dt$long_from <- long_from
        
        #repeat actions with destinations
        city.y <- input$unload_city
        if (input$unload_city == "Санкт-Петербург") {
            region.y <- "Санкт-Петербург"
            street.y <- input$stp_streets_unload
            stpetersburgstreets_filter <- filter(stpetersburgstreets, street == input$stp_streets_unload)
            lat_to <- stpetersburgstreets_filter$latitude
            long_to <- stpetersburgstreets_filter$longitude
        } else if (input$unload_city == "Москва") {
            region.y <- "Москва"
            street.y <- input$moscow_streets_unload
            moscowstreets_filter <- filter(moscowstreets, street == input$moscow_streets_unload)
            lat_to <- moscowstreets_filter$latitude
            long_to <- moscowstreets_filter$longitude
        } else {
            citiesnew_filter <- filter(citiesnew, city == input$unload_city)
            region.y <- as.character(citiesnew_filter$region)
            street.y <- "Ленина"
            lat_to <- citiesnew_filter$latitude
            long_to <- citiesnew_filter$longitude
        }
        
        #upload destination values to total dataset
        selected_dt$region.y <- region.y
        selected_dt$lat_to <- lat_to
        selected_dt$long_to <- long_to
        
        #initialize coordinates convertation from char to double
        final_table <- selected_dt
        final_table$latitude.x <- final_table$lat_from
        final_table$latitude.y <- final_table$lat_to
        final_table$longitude.x <- final_table$long_from
        final_table$longitude.y <- final_table$long_to
        final_table <- select(final_table, -c(lat_from, lat_to, long_from, long_to))
        
        final_table <- separate(final_table, latitude.x, into = c("lat_from", "titude"), sep = "\\.", remove = TRUE)
        final_table$lat_from <- as.integer(final_table$lat_from)
        final_table$titude <- substr(final_table$titude, 1, 2)
        final_table$titude <- as.integer(final_table$titude)
        final_table$titude <- final_table$titude*0.01
        final_table$lat_from <- final_table$lat_from+final_table$titude
        final_table <- select(final_table, -c(titude))
        
        final_table <- separate(final_table, latitude.y, into = c("lat_to", "titude"), sep = "\\.", remove = TRUE)
        final_table$lat_to <- as.integer(final_table$lat_to)
        final_table$titude <- substr(final_table$titude, 1, 2)
        final_table$titude <- as.integer(final_table$titude)
        final_table$titude <- final_table$titude*0.01
        final_table$lat_to <- final_table$lat_to+final_table$titude
        final_table <- select(final_table, -c(titude))
        
        final_table <- separate(final_table, longitude.x, into = c("long_from", "titude"), sep = "\\.", remove = TRUE)
        final_table$long_from <- as.integer(final_table$long_from)
        final_table$titude <- substr(final_table$titude, 1, 2)
        final_table$titude <- as.integer(final_table$titude)
        final_table$titude <- final_table$titude*0.01
        final_table$long_from <- final_table$long_from+final_table$titude
        final_table <- select(final_table, -c(titude))
        
        final_table <- separate(final_table, longitude.y, into = c("long_to", "titude"), sep = "\\.", remove = TRUE)
        final_table$long_to <- as.integer(final_table$long_to)
        final_table$titude <- substr(final_table$titude, 1, 2)
        final_table$titude <- as.integer(final_table$titude)
        final_table$titude <- final_table$titude*0.01
        final_table$long_to <- final_table$long_to+final_table$titude
        final_table <- select(final_table, -c(titude))
        
        #upload coords with right types to dt
        selected_dt <- final_table
        
        #initialize distance parser
        addition <- grepl("distance", columns_table$parent_column_names)
        addition <- data.frame(addition)
        if (nrow(filter(columns_table, columns_table$column_name == "distance"))==0) {
            if (nrow(filter(addition, addition == "TRUE"))!=0) {
                destination_final <- data.frame(region.y, city.y, street.y, lat_to, long_to)
                departure_final <- data.frame(region.x, city.x, street.x, lat_from, long_from)
                
                destination_final$X <- 1
                destination_final$region <- destination_final$region.y
                destination_final$city <- destination_final$city.y
                destination_final$latitude <- destination_final$lat_to
                destination_final$longitude <- destination_final$long_to
                
                departure_final$X <- 1
                departure_final$region <- departure_final$region.x
                departure_final$city <- departure_final$city.x
                departure_final$latitude <- departure_final$lat_from
                departure_final$longitude <- departure_final$long_from
                
                departure_final  <- select(departure_final, X, region, city, latitude, longitude)
                destination_final  <- select(destination_final, X, region, city, latitude, longitude)
                
                distance <- callModule(distances, "distances", reactive(destination_final), reactive(departure_final))
                if (nrow(distance)==0) {
                    shinyalert(
                        title = "Не удалось получить расстояние",
                        text = "Введите расстояние",
                        closeOnEsc = TRUE,
                        closeOnClickOutside = TRUE,
                        html = FALSE,
                        type = "input",
                        inputType = "text",
                        inputValue = "",
                        inputPlaceholder = "",
                        showConfirmButton = TRUE,
                        showCancelButton = TRUE,
                        confirmButtonText = "OK",
                        confirmButtonCol = "#00FFD9",
                        cancelButtonText = "Cancel",
                        timer = 0,
                        imageUrl = "",
                        animation = TRUE,
                        callbackR = function(x) {
                            distance <<- input$shinyalert
                        }
                    )
                } else {
                    distance <- distance$distance
                    print(distance)
                }
                
                selected_dt$distance <- as.integer(distance)
                print(selected_dt)
            }
        } else {
            destination_final <- data.frame(region.y, city.y, street.y, lat_to, long_to)
            departure_final <- data.frame(region.x, city.x, street.x, lat_from, long_from)
            
            destination_final$X <- 1
            destination_final$region <- destination_final$region.y
            destination_final$city <- destination_final$city.y
            destination_final$latitude <- destination_final$lat_to
            destination_final$longitude <- destination_final$long_to
            
            departure_final$X <- 1
            departure_final$region <- departure_final$region.x
            departure_final$city <- departure_final$city.x
            departure_final$latitude <- departure_final$lat_from
            departure_final$longitude <- departure_final$long_from
            
            departure_final  <- select(departure_final, X, region, city, latitude, longitude)
            destination_final  <- select(destination_final, X, region, city, latitude, longitude)
            
            distance <- callModule(distances, "distances", reactive(destination_final), reactive(departure_final))
            print(distance)
            if (nrow(distance)!=0) {
                selected_dt$distance <- as.integer(distance$distance)
                print(selected_dt)
            }
        }
        
        #upload unknown numeric columns
        columns_table <- na.omit(columns_table)
        columns_table$X <- grepl(".R", columns_table$profile_module_used)
        columns_table <- filter(columns_table, X==TRUE)
        if (nrow(columns_table)!=0) {
            for (i in 1:nrow(columns_table)) {
                to_source <- paste0("~/Downloads/cargo_type_module/", columns_table$profile_module_used[i])
                parent_column_names <- as.character(columns_table$parent_column_names[i])
                input_name <- as.character(columns_table$column_name[i])
                source(to_source)
                selected_dt <- callModule(a, "minus", reactive(selected_dt), 
                                          reactive(parent_column_names), reactive(input_name))
                selected_dt[ncol(selected_dt)] <- as.numeric(selected_dt[ncol(selected_dt)])
            }
        }
        
        #upload unknown factor columns
        columns_table <- read.csv2(file = paste0("~/Downloads/cargo_type_module/", as.character(models$columns_table)))
        columns_table <- na.omit(columns_table)
        columns_table$X <- grepl(".csv", columns_table$profile_module_used)
        columns_table <- filter(columns_table, X==TRUE)

        if (nrow(columns_table)!=0) {
            
            original_dt <- as.character(models$original_dt)
            the_dataset <- paste0("~/Downloads/", original_dt)
            addition <- read.csv2(the_dataset)
            addition <- select(addition, -c(X))
            
            for (i in 1:nrow(columns_table)) {
                #the model is in csv, so import it
                to_source <- paste0("~/Downloads/cargo_type_module/", columns_table$profile_module_used[i])
                groups_profile <- read.csv2(to_source)
                groups_profile <- select(groups_profile, -c(X))
                
                #the column it came from
                parent_column_names <- as.character(columns_table$parent_column_names[i])

                #need to change "column_name" to the name from dataset
                colnames(groups_profile)[1] <- 
                    parent_column_names
                
                #new_column_name
                input_name <- as.character(columns_table$column_name[i])
                
                #apply it
                #in groups_profile there are original column_vector and the_group - how it transforms
                #need to change selected_dt according to groups_profile
                selected_dt <- left_join(selected_dt, groups_profile, by = parent_column_names)
                print(selected_dt)
                addition_vector <- c(addition[which(colnames(addition) == as.character(columns_table$column_name[i]))])
                addition_tail_vector <- c(selected_dt[ncol(selected_dt)])
                selected_dt[ncol(selected_dt)] <- factor(addition_tail_vector[[1]], 
                                                         levels = levels(addition_vector[[1]]))
                colnames(selected_dt)[ncol(selected_dt)] <- 
                    input_name
                print(selected_dt)
            }
        }
        #print(selected_dt)
        #select only relevant columns for data.frame
        #columns_table <- read.csv2(file = paste0("~/Downloads/cargo_type_module/", as.character(models$columns_table)))
        #column_names <- as.character(columns_table$column_name)
        #column_names <- column_names[ column_names != "clusters" ]
        #selected_dt <- select(selected_dt, column_names)
        #print(selected_dt)
        #topredict <- data.frame(price, distance, value, volume, payment, loading_date, transportation_duration, weight, cargo_type, start_lat, start_lon, dest_lat, dest_lon, two_loadings, two_unloadings, vehicle_type, from_msc, to_msc, from_saintp, to_saintp, to_east, to_south, thegroup, week, clusters)
        #topredict <- data.frame(price, distance, value, volume, payment, loading_date, transportation_duration, weight, cargo_type, start_lat, start_lon, dest_lat, dest_lon, two_loadings, two_unloadings, vehicle_type, from_msc, to_msc, from_saintp, to_saintp, to_east, to_south, thegroup, week, clusters)
        #topredict = select(topredict, -c(price))
        #print(topredict)
        #joining the input data with the original dataset made for the learning model in order to eleminate the new factor level error common for random forest
        #withpredict = rbind(analt, topredict)
        #data.test = tail(withpredict, n=1)
        
        #defining clusters that were previously gained from unsupervised learning model as a stage of data handling process
        models <- read.csv2("~/Downloads/cargo_type_module/models.csv")
        models <- filter(models, user_model_name == as.character(the_model_to_use$the_model_to_use))
        original_dt <- as.character(models$original_dt)
        the_dataset <- paste0("~/Downloads/", original_dt)
        addition <- read.csv2(the_dataset)
        addition <- select(addition, -c(X))

        selected_dt$money_transfer_form <- factor(selected_dt$money_transfer_form, levels = levels(addition$money_transfer_form))
        selected_dt$payment <- factor(selected_dt$payment, levels = levels(addition$payment))
        selected_dt$car_type <- factor(selected_dt$car_type, levels = levels(addition$car_type))
        selected_dt$cargo_type <- factor(selected_dt$cargo_type, levels = levels(addition$cargo_type))
        #selected_dt$regions_applied <- factor(selected_dt$regions_applied, levels = levels(addition$regions_applied))
        #selected_dt$departure_applied <- factor(selected_dt$departure_applied, levels = levels(addition$departure_applied))
        rfPredict<-predict(rfModel_cluster, newdata=selected_dt, probability=FALSE)
        print(rfPredict)
        #topredict$clusters = as.factor(rfPredict)
        print(sapply(addition, class))
        print(sapply(selected_dt, class))
        #selected_dt$clusters = factor(rfPredict, levels = addition$cluster)
        selected_dt$clusters <- rfPredict
        print(selected_dt)
        write.csv2(selected_dt, file = "~/Downloads/selected_dt.csv")
        rfPredict<-predict(rfModel, newdata=selected_dt, probability=FALSE)
        as.matrix(rfPredict)
        #selected_dt
        #topredict$price <- as.integer(1)
        
        #repeat the process of solving the random forest factor level error
        #datacomplete = rbind(datist, topredict)
        #data.test = tail(datacomplete, n=1)
        
        #completing the final prediction and receiving the result
        #yhat.bag = predict(theforest,newdata=data.test)
        #yhat.bag
    })
    
    output$price <- DT::renderDataTable({
        data()
    },
    options = list(scrollX = TRUE))
}