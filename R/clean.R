# Program Name: clean.R
# Author:       Jacob Englert
# Date:         18 September 2019
# Purpose:      Remove all suspicious observations from data provided by the
#               Togolese and create helpful new variables

# Load Required Packages --------------------------------------------------
library(tidyverse)

# Import Raw Data ---------------------------------------------------------

get_raw_data <- function(type, sheet, range, location, ...){
  names <- c("Year", month.name)
  file <- here::here("Data","Raw", paste(type,"Temps.xls"))
  df <- readxl::read_xls(file, sheet = sheet, range = range, col_names = names, ...) |>
    mutate(Location = location,
           Type = type)
  return(df)
}

# Maximum Temperatures
lome_max <- get_raw_data('Maximum', 1, "B6:N60", "Lome")
tabl_max <- get_raw_data('Maximum', 2, "B6:N60", "Tabligbo")
koum_max <- get_raw_data('Maximum', 3, "B6:N60", "Kouma-Konda")
atak_max <- get_raw_data('Maximum', 4, "B8:N62", "Atakpame")
soto_max <- get_raw_data('Maximum', 5, "B8:N41", "Sotouboua", na = '***', col_types = "numeric")
soko_max <- get_raw_data('Maximum', 6, "B6:N60", "Sokode")
kara_max <- get_raw_data('Maximum', 7, "B7:N45", "Kara")
niam_max <- get_raw_data('Maximum', 8, "B7:N61", "Niamtougou")
mang_max <- get_raw_data('Maximum', 9, "B6:N60", "Mango")
dapa_max <- get_raw_data('Maximum', 10, "B6:N60", "Dapaong")

# Minimum Temperatures
lome_min <- get_raw_data('Minimum', 1, "B6:N60", "Lome")
tabl_min <- get_raw_data('Minimum', 2, "B7:N61", "Tabligbo")
koum_min <- get_raw_data('Minimum', 3, "B7:N61", "Kouma-Konda")
atak_min <- get_raw_data('Minimum', 4, "B7:N61", "Atakpame")
soto_min <- get_raw_data('Minimum', 5, "B7:N40", "Sotouboua", na = '***', col_types = "numeric")
soko_min <- get_raw_data('Minimum', 6, "B6:N60", "Sokode")
kara_min <- get_raw_data('Minimum', 7, "B7:N45", "Kara")
niam_min <- get_raw_data('Minimum', 8, "B7:N61", "Niamtougou")
mang_min <- get_raw_data('Minimum', 9, "B6:N60", "Mango")
dapa_min <- get_raw_data('Minimum', 10, "B6:N60", "Dapaong")


# Compile Data ------------------------------------------------------------
all <- bind_rows(lome_max, tabl_max, koum_max, atak_max, soto_max,
                 soko_max, kara_max, niam_max, mang_max, dapa_max,
                 lome_min, tabl_min, koum_min, atak_min, soto_min,
                 soko_min, kara_min, niam_min, mang_min, dapa_min) |>
  pivot_longer(cols = January:December, names_to = 'Month', values_to = 'Temperature') |>
  mutate(Month = factor(Month, levels = month.name))


# Clean Data --------------------------------------------------------------
# Load supplementary data
geography <- read_csv(here::here("Data","Supplementary","Geography.csv"))
enso_sst <- read_csv(here::here("Data","Supplementary","EnsoSST.csv"))

# Add helpful variables
all2 <- all %>%
  mutate(MonthNum = match(Month, month.name),
         DecYear = round((Year + ((MonthNum - 0.5) / 12)), 3)) |>
  left_join(geography, join_by('Location')) |>
  left_join(enso_sst, join_by('DecYear')) |>
  mutate(ExcelAvg = ifelse(
    (Type == 'Maximum' &
      (
      (Location == 'Lome' & Year == 2007 & MonthNum == 1) |
      (Location == 'Kouma-Konda' & (Year == 1965 | 
                                       (Year == 1966 & MonthNum == 2) |
                                       (Year == 1969 & MonthNum %in% c(8,9,10)) |
                                       (Year == 1970 & MonthNum %in% c(4,12)) |
                                       (Year == 1971 & MonthNum >= 5) |
                                       (Year == 1972 & MonthNum %in% c(1,2,3,4,5,6,9,10)) |
                                       (DecYear >= 1973.7 & DecYear <= 1975.63)
                                     )
       ) |
      (Location == 'Sokode' & Year == 2008 & MonthNum == 9) |
      (Location == 'Kara' & ((Year == 1989 & MonthNum >= 11) |
                                  (DecYear >= 1993.25 & DecYear <= 1994.13))) |
      (Location == 'Niamtougou' & ((Year == 1976 & MonthNum >= 6) |
                                        (Year == 1977 & MonthNum == 10) |
                                        (Year == 1979 & MonthNum >= 7))) |
      (Location == 'Dapaong' & ((DecYear >= 1978.2 & Year <= 1979) |
                                     (Year == 1991 & MonthNum %in% c(10,11)) |
                                     (Year == 2008 & MonthNum == 12))) |
      (Location == 'Mango' & DecYear >= 2001.625 & DecYear <= 2002.6)
    ) |
    (Type == 'Minimum' &
      (
      (Location == 'Lome' & Year == 2007 & MonthNum == 1) |
      (Location == 'Kouma-Konda' & (Year == 1965 |
                                      (Year == 1966 & MonthNum == 2) |
                                      (Year == 1969 & MonthNum %in% c(8,9,10)) |
                                      (Year == 1970 & MonthNum %in% c(4,12)) |
                                      (Year == 1971 & MonthNum >= 5) |
                                      (Year == 1972 & MonthNum %in% c(1,2,3,4,5,6,9,10)) |
                                      (DecYear >= 1973.7 & DecYear <= 1975.63)
                                    )
      ) |
      (Location == 'Sokode' & ((Year == 2005 & MonthNum == 11) |
                                 (Year == 2006 & MonthNum %in% c(7,8,11,12)) |
                                 (Year == 2007 & !(MonthNum %in% c(2,9,10))) |
                                 (Year == 2008 & MonthNum %in% c(1,12))
                               )
      ) |
      (Location == 'Kara' & ((Year == 1993 & MonthNum == 4) |
                               (Year == 1994 & !(MonthNum %in% c(1,3))) |
                               (Year == 1995 & MonthNum %in% c(2,5))
                             )
      ) |
      (Location == 'Niamtougou' & ((Year == 1976 & MonthNum >= 6) |
                                     (Year == 1977 & MonthNum == 10) |
                                     (Year == 1979 & MonthNum >= 7)
                                   )
      ) |
      (Location == 'Dapaong' & ((Year == 1968 & MonthNum == 12) |
                                  (DecYear >= 1978.2 & Year <= 1979) |
                                  (Year == 1991 & MonthNum %in% c(10,11)) |
                                  (Year == 2008 & MonthNum == 12)
                                )
      ) |
      (Location == 'Mango' & DecYear >= 2001.625 & DecYear <= 2002.6)
      )
    )   
    ), 'Yes', 'No')
  )

# Print suspicious observations
all2 %>%
  filter(ExcelAvg == 'Yes') |>
  select(Location, DecYear, Year, Month, MonthNum)

# Flag  outliers using IQR method
clean <- all2 |>
  mutate(IQR = IQR(Temperature, na.rm = TRUE),
         Q1 = quantile(Temperature, 0.25, na.rm = TRUE),
         Q3 = quantile(Temperature, 0.75, na.rm = TRUE),
         .by = c(Type, Location, Month)) |>
  mutate(Outlier = ifelse(Temperature > Q3 + 1.5*IQR | 
                     Temperature < Q1 - 1.5 * IQR, "Yes", "No")) |>
  select(Year, DecYear, Month, MonthNum, ENSO, SST, Location, Longitude, 
         Latitude, Elevation, Type, Temperature, ExcelAvg, Outlier) |>
  arrange(Location, DecYear, Type)

write_csv(clean, here::here("Data","Clean","clean.csv"))
