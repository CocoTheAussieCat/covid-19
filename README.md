app.R = Shiny App code for https://cocotheaussiecat.shinyapps.io/covid_19/

Dependencies:
01_who_data_import.R = imports COVID-19 data from Johns Hopkins github repo, cleans and manipulates data

02_growth_rate.R = creates plots used in app.R that show case growth rate by country & region

03_aus_testing = imports data on Australian COVID-19 testing rates, creates plots used in app.R

tests.csv = DEPRECATED. Replaced by JSON data feed from Guardian Australia. 

population.csv = World population data from gapminder
