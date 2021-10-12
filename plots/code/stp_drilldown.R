## Plot Single touch payroll Jobs change since 100th case (and on last month)

# Load Libraries -----
library(highcharter)
library(readr)
library(dplyr)
library(tidyr)
library(here)
library(purrr)

# Load in the data -----
industry <- readabs::read_payrolls("industry_jobs") %>%
  filter(
    state == "Australia",
    sex == "Persons",
    age == "All ages"
  ) %>%
  select(-series)

subindustry <- readabs::read_payrolls("subindustry_jobs") %>%
  select(-series)

# Calulate changes -----
## Use dplyr lag for change on last month

## Get current ref period for filter
max_date <- max(industry$date)

## Calculate indsutry change values
industry_change <- industry %>%
  group_by(industry) %>%
  mutate(
    four_week_date = dplyr::lag(date, 4),
    four_week_change = round((value - dplyr::lag(value, 4)) / dplyr::lag(value, 4) * 100, 2),
    covid_change = round((value - 100) / 100 * 100, 2)
  ) %>%
  filter(date == max_date) %>% 
  pivot_longer(
    cols = c(four_week_change, covid_change),
    names_to = "change_from",
    values_to = "pct_change"
  )

## Calculate Indsutry subdivision changes
subindustry_change <- subindustry %>%
  group_by(industry_subdivision) %>%
  mutate(
    four_week_date = dplyr::lag(date, 4),
    four_week_change = round((value - dplyr::lag(value, 4)) / dplyr::lag(value, 4) * 100, 2),
    covid_change = round((value - 100) / 100 * 100, 2)
  ) %>%
  filter(date == max_date) %>% 
  pivot_longer(
    cols = c(four_week_change, covid_change),
    names_to = "change_from",
    values_to = "pct_change"
  ) %>% 
  # remove gropuping strcuture
  ungroup()

## Create a list of subdivision for passing to drilldwon
subindstry_list <- subindustry_change %>%
  ##filter to just teh covid data 
  ## create nested parent child rows
  group_nest(industry_division, change_from) %>% 
  ## Update the strcuture so that the toplevel plot and drilldwon speak to eachother
  mutate(
    id = industry_division,
    type = "bar",
    data = map(data, 
               mutate, 
               name = industry_subdivision, 
               y = pct_change,
    ),
    data = map(data,
               list_parse)
  )

## Build Plot for Change on 100th Covid Case -----
change_100 <- highcharter::hchart(
  ## I am going to leave all industries out
  filter(industry_change, 
         industry != "All industries",
         change_from == "covid_change"),
  "bar", #the type of graph is bar (gives categories along Y axis)
  hcaes( #set the aesthetics
    x = industry, #industry along the X axis
    y = pct_change, #value along the y axis
    name = "industry", #not actually sure what this does but it is the doco
    drilldown = industry #we will drilldown according the the industry children 
  ),
  name = "industry"
) %>% 
  ## Add in the drilldwon data
  hc_drilldown(
    allowPointDrilldown = T,
    series = list_parse(filter(subindstry_list, change_from == "covid_change"))
  ) %>% 
  highcharter::hc_yAxis(title = list(text = "% Change"),
                        labels = list(formatter = JS(
                          "function(){
                                         return(this.value + `%`)
                        }"))) %>% 
  highcharter::hc_xAxis(title = list(text = "Industry")) %>% 
  ##Add a title to the plot
  highcharter::hc_title(text = "% Change in STP Jobs from 100th COVID-19 Case" ) %>% 
  ##And a subtitle giving the date
  highcharter::hc_subtitle(
    text = paste("Week ending", format.Date(max(industry$date),"%d %B %Y"))
  )

## View the plot
change_100

## Save the plot
htmltools::save_html(change_100, file = here::here("plots", "output", "change_from_100_case.html"))

## Build Plot for change on 4 weeks ago 

change_4 <- highcharter::hchart(
  ## I am going to leave all industries out
  filter(industry_change, 
         industry != "All industries",
         change_from == "four_week_change"),
  "bar", #the type of graph is bar (gives categories along Y axis)
  hcaes( #set the aesthetics
    x = industry, #industry along the X axis
    y = pct_change, #value along the y axis
    name = "industry", #not actually sure what this does but it is the doco
    drilldown = industry #we will drilldown according the the industry children 
  ),
  name = "industry"
) %>% 
  ## Add in the drilldwon data
  hc_drilldown(
    allowPointDrilldown = T,
    series = list_parse(filter(subindstry_list, change_from == "four_week_change"))
  ) %>% 
  highcharter::hc_yAxis(title = list(text = "% Change"),
                        labels = list(formatter = JS(
                          "function(){
                                         return(this.value + `%`)
                        }"))) %>% 
  highcharter::hc_xAxis(title = list(text = "Industry")) %>% 
  ##Add a title to the plot
  highcharter::hc_title(text = "% Change in STP Jobs" ) %>% 
  ##And a subtitle giving the date
  highcharter::hc_subtitle(
    text = paste("Change from Week ending", format.Date(max(industry_change$four_week_date),"%d %B %Y"), "to week ending", format.Date(max(industry_change$date),"%d %B %Y"))
  )

# View plot
change_4

##Save plot
htmltools::save_html(change_4, file = here::here("plots", "output", "change_from_4_weeks.html"))



