app.R = Shiny App code for https://cocotheaussiecat.shinyapps.io/covid_19/

Dependencies:
01_who_data_import.R = imports COVID-19 data from Johns Hopkins github repo, cleans and manipulates data

02_growth_rate.R = creates plots used in app.R that show case growth rate by country & region

03_aus_testing = imports data on Australian COVID-19 testing rates, creates plots used in app.R

tests.csv = Australian COVID-19 daily testing and case statistics. Updated manually by repo owner with data sourced from Australian state government health websites
