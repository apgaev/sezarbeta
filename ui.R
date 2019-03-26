library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(dplyr)
citiesnew <- read.csv2("~/Downloads/citiesnew.csv")
stpetersburgstreets <- read.csv2("~/Downloads/stpetersburgstreets.csv")
moscowstreets <- read.csv2("~/Downloads/moscowstreets.csv")
car_type <- read.csv2("~/Downloads/car_type.csv")
models <- read.csv2("~/Downloads/models.csv")
the_model_to_use <- read.csv2("~/Downloads/cargo_type_module/the_model_to_use.csv")
models <- filter(models, user_model_name == as.character(the_model_to_use$the_model_to_use))
cargo_types <- read.csv2(file = paste0("~/Downloads/", as.character(models$cargo_types_ds)))
cargo_types <- filter(cargo_types, cargo_type != "необработанные")
cargo_types <- cargo_types[!duplicated(cargo_types$cargo_type), ]
car_types <- read.csv2(file = paste0("~/Downloads/", as.character(models$car_types_ds)))
car_types <- car_types[!duplicated(car_types$car_type), ]
car_types <- filter(car_types, car_type != "необработанные")
dashboardPage(skin = "blue",
              dashboardHeader(title = "Прогнозчик"
              ),
               dashboardSidebar(
                   sidebarMenu(
                       menuItem("Прогнозчик", tabName = "dashboard", icon = icon("dashboard"))
                   ))
               ,
               dashboardBody( 
                   tabItems(
                       # First tab content
                       tabItem(
                           tabName = "dashboard",
                           fluidRow(
                               column(5, 
                                      selectInput(inputId = "load_city", h3("Загрузка"), 
                                                  choices = list("Санкт-Петербург", "Москва", остальные=citiesnew$city))),
                               column(4,
                                      selectInput(inputId = "unload_city", h3("Разгрузка"), 
                                                  choices = list("Москва", "Санкт-Петербург", остальные=citiesnew$city))),
                               column(3, shinyalert::useShinyalert(),
                                      br(), br(), br(), actionButton("add_new_location", "Добавить новый город +"))
                           ),
                           fluidRow(
                             column(5,
                             shinyjs::useShinyjs(),
                             selectInput(inputId = "stp_streets", label = "Улица",
                                         choices = stpetersburgstreets$street, selected = "Цветочная"),
                             selectInput(inputId = "moscow_streets", label = "Улица",
                                         choices = moscowstreets$street, selected = "Факультетский"),
                             DT::dataTableOutput('load_city_dt')
                             ),
                             column(7,
                                    selectInput(inputId = "stp_streets_unload", label = "Улица",
                                                choices = stpetersburgstreets$street, selected = "Цветочная"),
                                    selectInput(inputId = "moscow_streets_unload", label = "Улица",
                                                choices = moscowstreets$street, selected = "Факультетский"),
                                    DT::dataTableOutput('load_city_dt_unload'))
                           ),
                           fluidRow(
                             column(6, 
                                    sliderInput("volume", "Объём",
                                                min = 1, max = 120, value = 82)),
                             column(6, 
                                    sliderInput("weight", "Вес",
                                                min = 0.1, max = 24, value = 20, step = 0.1))
                           ),
                           fluidRow(
                               
                               column(3, 
                                      dateInput(inputId = "calendar", 
                                                h3("Дата загрузки"), 
                                                value = Sys.Date())),
                               column(3, 
                                      numericInput(inputId = "duration", 
                                                   h3("Дней в пути"), 
                                                   value = 0)),
                               column(3,
                                      radioButtons(inputId = "two_loadings.x", h3("Более одной точек погрузки"),
                                                   choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE)),
                               column(3,
                                      radioButtons(inputId = "two_loadings.y", h3("Более одной точек выгрузки"),
                                                   choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE))),
                           fluidRow(
                               column(3, 
                                      numericInput(inputId = "cargo_price", 
                                                   h3("Стоимость груза"), 
                                                   value = 0)),
                               column(3,
                                      selectInput(inputId = "car_type", h3("Тип авто"), 
                                                  choices = car_types$car_type)),
                               column(3,
                                      selectInput(inputId = "money_transfer_form", h3("Форма оплаты"), 
                                                  choices = list("по ОТТН и документам на оплату", "по сканам ТТН и квитку", 
                                                                 "в обмен на ТТН", "перед погрузкой", "по скану счета",
                                                                 "по счету", "по факту выгрузки", "по факту загрузки"))),
                               column(3, selectInput(inputId = "payment", h3("Тип оплаты"), 
                                                     choices = list("безнал с НДС", "безнал без НДС", "нал на выгрузке",
                                                                    "нал на загрузке", "наличные", "перевод на карту")))
                           ),
                           fluidRow(
                             column(3,
                                    selectInput(inputId = "cargo_type", h3("Тип груза"), 
                                                choices = cargo_types$cargo_type)),
                             column(3,
                                    actionButton(inputId = "count", label = "Рассчитать"))
                           ),
                           fluidRow(
                             DT::dataTableOutput(outputId = "price")
                           )
                       )
)))