library(shiny)
library(shinydashboard)

dashboardPage( skin = "blue",
               dashboardHeader(title = span(tagList(img(src = 'https://sezarshop.ru/local/templates/.default/bundle/prod/src/images/logo.svg',
                                                        title = "Company Home", height = "60px"), "")), tags$li(class = "dropdown",
                                                                                                                tags$style(".main-header {max-height: 60px}"),
                                                                                                                tags$style(".main-header .logo {height: 60px;}"),
                                                                                                                tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
                                                                                                                tags$style(".navbar {min-height:60px !important}"))
               ),
               dashboardSidebar(
                 sidebarMenu(
                   menuItem("узнать цену на перевозку", tabName = "dashboard", icon = icon("dashboard")),
                   menuItem("посмотреть на карте", tabName = "widgets", icon = icon("th"))
                 ))
               ,
               dashboardBody( 
                 tabItems(
                   # First tab content
                   tabItem(
                     tabName = "dashboard",
                     fluidRow(
                       column(3, 
                              textInput(inputId = "load_city", 
                                        h3("Город погрузки"), 
                                        value = 0)),
                       column(3, 
                              textInput(inputId = "unload_city", 
                                        h3("Город разгрузки"), 
                                        value = 0)),
                       column(3, 
                              numericInput(inputId = "volume", 
                                           h3("Объем"), 
                                           value = 0)),
                       column(3, 
                              dateInput(inputId = "loading_date", 
                                        h3("Дата загрузки"), 
                                        value = "2018-01-01")),
                       column(3, 
                              numericInput(inputId = "duration", 
                                           h3("Дней в пути"), 
                                           value = 0)),
                       column(3, 
                              numericInput(inputId = "weight", 
                                           h3("Вес"), 
                                           value = 0)),
                       column(3,
                              selectInput(inputId = "cargo_type", h3("Тип груза"), 
                                          choices = list("Алкоголь" = "Alcohol", "Опасная Химия" = "Dangerous Chemistry",
                                                         "Оборудование" = "equipment", "Неопасная Химия" = "Non-hazardous Chemistry", "Масло" = "oil", "ТНП" = "Products", "Прочее" = "Stuff"), selected = "oil")),
                       column(3,
                              radioButtons(inputId = "two_loadings", h3("Более одной точек погрузки"),
                                           choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE)),
                       column(3,
                              radioButtons(inputId = "two_unloadings", h3("Более одной точек выгрузки"),
                                           choices = list("Нет" = FALSE, "Да" = TRUE),selected = FALSE)),
                       column(3, 
                              numericInput(inputId = "value", 
                                           h3("Стоимость груза"), 
                                           value = 0)),
                       column(3,
                              selectInput(inputId = "vehicle_type", h3("Тип авто"), 
                                          choices = list("Любой" = "any", "Контейнер" = "container",
                                                         "Изотерм" = "isoterm", "Рефрижиратор" = "ref", "С боковой/верхней погрузкой/разгрузкой" = "upside"), selected = "any")),
                       column(3,
                              actionButton(inputId = "count", label = "Рассчитать")),
                       column(3, 
                              tableOutput(outputId = "price"))
                     )
                   )
                 )))









