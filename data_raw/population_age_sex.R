## Population by age data 


popdata <- readabs::read_abs("3101.0", tables = 59) 

glimpse(popdata)

pop_by_age_by_sex <- popdata %>% 
  readabs::separate_series(column_names = c("series", "sex", "age")) %>% 
  filter(sex != "Persons") %>%  # remove persons and just get by sex
  dplyr::mutate(
    working_age = case_when(
      age >= 15 & age <= 64 ~ "Working age",
      age <= 14 ~ "Below Working age",
      age >= 65 ~ "Above working age"
    )
  ) %>% 
  group_by(
    date,
    sex,
    working_age
  ) %>% 
  summarise(
    `Estimated Population` = sum(value)
  ) %>% 
  ungroup() %>% 
  group_by(date, sex) %>% 
  mutate(
    `Proportion of Sex` = `Estimated Population` / sum(`Estimated Population`) 
  ) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(
    `Proportion of Population` = `Estimated Population` /  sum(`Estimated Population`)
  )

pop_by_age <- popdata %>% 
  readabs::separate_series(column_names = c("series", "sex", "age")) %>% 
  filter(sex == "Persons") %>%  # remove persons and just get by sex
  dplyr::mutate(
    working_age = case_when(
      age >= 15 & age <= 64 ~ "Working age",
      age <= 14 ~ "Below Working age",
      age >= 65 ~ "Above working age"
    )
  ) %>% 
  group_by(
    date,
    sex,
    working_age
  ) %>% 
  summarise(
    `Estimated Population` = sum(value)
  ) %>% 
  ungroup() %>% 
  group_by(date, sex) %>% 
  mutate(
    `Proportion of Population` = `Estimated Population` / sum(`Estimated Population`) 
  ) 

  
pop_total <- pop_by_age %>% 
  group_by(date, sex) %>% 
  summarise(
    `Estimate Population` = sum(`Estimated Population`)
  )

ls(pattern = "pop_") %>% 
  map(~write_csv(get(.x), file = paste0("data/population/", .x, ".csv")))



