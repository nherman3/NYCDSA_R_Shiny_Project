##### Libraries: #####
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(data.table)
library(shinyjs)
library(shinyWidgets)

##### DataFrame for app #####
df <- read.csv(file = "./crashes_cleaned.csv")

##### Elimination of overlapping nearby data points: #####
df$latitude = round(df$latitude,4)
df$longitude = round(df$longitude,4)

##### Misc. Variables used in code: #####
month_list = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
month_list_short=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
day_list = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
day_list_short = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
vehicle_types = c("Bicycle/Scooter","E-Bicycle/Scooter","Motor Bike","Other/Unknown","Passenger Car","Truck","")
boro_list = c("bronx", "brooklyn", "manhattan", "queens", "staten island")

