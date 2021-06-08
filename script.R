
library(tidyverse)
library(tidycensus)

# generic data
acs_year <- 2019
cmap_counties <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")


# get variables and descriptive variable names
acs_varnames <- load_variables(acs_year, "acs5/profile", cache = TRUE) %>%
  # get only non-percent records from DP05
  filter(str_sub(name, 1, 4) == "DP05",
         !str_detect(name, "P$")) %>%
  # remove unnecessary label text and calculate short identifier
  mutate(label = str_replace(label, '^Estimate!!', ''),
         id = as.numeric(str_sub(name, 7,9)),
         name_short = str_extract(label, "(?<=!!)[^!]*$")) %>%
  # select data of interest
  filter(id %in% c(1, 5:17, 37, 38, 39, 44,52, 57, 58, 71)) %>%
  # drop unnecessary columns and rename
  select(variable = name, name_full = label, name_short)



# Import 1970-2010 census data (NHGIS source)
nhgis_county <- read_csv("nhgis_csv/nhgis0002_ts_nominal_county.csv",
                         skip = 1,
                         col_types = "ccccccccciiiiiiiiiiiiiiiiiiiiiiiiiiiiiii") %>%
  filter(`FIPS State Code` == "17",
         `FIPS County Code` %in% str_sub(cmap_counties, 3, 5),
         `Row Source Year` %in% c("1970", "1980", "1990", "2000", "2010")) %>%
  rename(NAME = 6)

nhgis_place <- read_csv("nhgis_csv/nhgis0002_ts_nominal_place.csv",
                         skip = 1,
                         col_types = "ccccccccciiiiiiiiiiiiiiiiiiiiiiiiiiiii") %>%
  filter(`NHGIS Integrated Geographic Unit Code` == "G17014000") %>%
  rename(NAME = 7)


nhgis <- bind_rows(nhgis_county, nhgis_place)

  select(NAME = 6,
         year = "Row Source Year",
         10:___)



# Import 2015-2019 ACS data (data.census.gov via tidycensus)
acs_county <- get_acs(geography = "county",
                      state = "17",
                      variables = acs_varnames$variable,
                      cache_table = TRUE,
                      year = acs_year,
                      survey = "acs5",
                      output = "wide") %>%
  # Keep only CMAP counties
  filter(GEOID %in% cmap_counties)

acs_place <- get_acs(geography = "place",
                     state = "17",
                     variables = acs_varnames$variable,
                     cache_table = TRUE,
                     year = acs_year,
                     survey = "acs5",
                     output = "wide") %>%
  # Keep only Chicago
  filter(GEOID == 1714000)

# combine and calculate
acs <- bind_rows(acs_county, acs_place) %>%
  # remove MOEs
  select(!matches("M$")) %>%
  # remove indications of estimates
  set_names(str_replace(names(.),"(?<=_[:digit:]{4})E$",""))
  # # MAKE SOME BAD ASSUMPTIONS
  # mutate(
  #   # remove "hispanic" from "white"
  #   DP05_0037 = DP05_0037 - DP05_0071,
  #   # add "two or more" to "other"
  #   DP05_0057 = DP05_0057 + DP05_0058
  # ) %>%
  # select(-DP05_0058) %>%
  # mutate(year = acs_year, .after = 2)


new_names <- names(acs) %>%
  as_tibble() %>%
  left_join(acs_varnames, by = c("value" = "variable")) %>%
  mutate(name_short = case_when(is.na(name_short) ~ value,
                                TRUE ~ name_short)) %>%
  .[["name_short"]]


final <- set_names(acs, new_names)

write.csv(final, "2019out.csv")


# stitch together
final <- pivot_longer(acs, !1:3, names_to = "variable") %>%
  left_join(select(acs_varnames, variable, name_short), by = "variable")

