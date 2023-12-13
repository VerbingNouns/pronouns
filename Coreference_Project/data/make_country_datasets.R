#profvis({
#### I GUESS I NEED TO ASSEMBLE THE DATA HERE UGHUGHUGH #### 
##### all code to create names_combined #####

###### USA ######
read_delim(file = list.files(path = "data/USA",
                             pattern = "yob",
                             full.names = TRUE),
           col_names = FALSE,
           id = "file",
           show_col_types = FALSE) |> 
  mutate(year = substr(file,13,16) |> as.integer()) |> 
  rename(name = X1, asab = X2, number = X3) |> 
  select(-file) |> 
  # join with SSN applications by year
  left_join(read_csv("data/USA/USA_SSA_applications_1880-2022.csv",
                     show_col_types = FALSE) |>
              rename(year = `year of birth`),
            by = join_by("year")) |> 
  group_by(year,asab) |> 
  # calculate proportion based on ASAB SSN applications
  mutate(proportion = case_when(asab == "M" ~ number/Male,
                                asab == "F" ~ number/Female),
         rank = rank(-number, ties.method = "first") |> as.integer(),
         name = str_to_title(name)) |> 
  select(-c("Male","Female","Total")) |> 
  mutate(region = "USA") -> names_USA
###### Canada ######
read_csv(file = "data/Canada/17100147-eng/17100147.csv",
         show_col_types = FALSE) |> 
  select("REF_DATE","GEO","Sex at birth","First name at birth","Indicator","VALUE") |> 
  pivot_wider(names_from = Indicator, values_from = VALUE) |> 
  rename(year = REF_DATE, region = GEO,
         asab = `Sex at birth`,
         name = `First name at birth`,
         number = Frequency,
         rank = Rank,
         proportion = `Proportion (%)`) |> # proportion by ASAB
  mutate(name = str_to_title(name),
         proportion = proportion/100,
         asab = case_when(asab == "Male" ~ "M",
                          asab == "Female" ~ "F",
                          TRUE ~ "X")) -> names_Canada
####### England and Wales ######
read_excel("data/EnglandWales/babynames1996to2021_EnglandWales.xlsx", 
           sheet = "1", # boy baby names
           na = "[x]", 
           skip = 7) |> 
  pivot_longer(cols = 2:53,
               names_to = "year_type",
               values_to = "value") |> 
  separate_wider_delim(cols = year_type, delim = " ", names = c("year", "type")) |> 
  pivot_wider(names_from = type, values_from = value) |> 
  mutate(asab = "M", region = "England and Wales") |> 
  # join with girl baby names
  rbind(read_excel("data/EnglandWales/babynames1996to2021_EnglandWales.xlsx", 
                   sheet = "2", # girl baby names
                   na = "[x]", 
                   skip = 7) |> 
          pivot_longer(cols = 2:53,
                       names_to = "year_type",
                       values_to = "value") |> 
          separate_wider_delim(cols = year_type, 
                               delim = " ", 
                               names = c("year", "type")) |> 
          pivot_wider(names_from = type, values_from = value) |> 
          mutate(asab = "F", region = "England and Wales")) |> 
  # tidy and remove empty rows
  rename(name = Name, rank = Rank, number = Count) |> 
  filter(!is.na(number)) |> 
  group_by(year,asab) |> 
  mutate(proportion = number/sum(number, na.rm = TRUE),
         name = str_to_title(name)) |> 
  ungroup() -> names_EW
###### Northern Ireland ######
read_excel("data/NorthernIreland/Data for Dashboard 2022.xlsx", 
           sheet = "Table 1 - Ranks and Geography", 
           skip = 3) |> 
  rename(year = "Year of Registration",
         asab = "Sex",
         name = "First name",
         number = "Number Given this Name") |> 
  mutate(region = "Northern Ireland",
         asab = case_when(asab == 1 ~ "M",
                          asab == 2 ~ "F",
                          TRUE ~ "X"),
         name = str_to_title(name)) |> 
  group_by(year, name, asab) |> 
  summarise(number = sum(number), .groups = "keep") |> 
  ungroup(name) |> 
  # join with birth data
  left_join(read_excel("data/NorthernIreland/Section 3 - Births_Tables_2022_Revised_Final.xlsx", 
                       sheet = "Table 3.1", skip = 4) |> 
              rename(year = "Year") |> 
              filter(!is.na(Females)) |> 
              mutate(year = as.numeric(year)),
            by = join_by("year")) |> 
  # tidy and organize
  mutate(rank = rank(-number, ties.method = "first"),
         proportion = case_when(asab == "M" ~ number/Males,
                                asab == "F" ~ number/Females),
         region = "Northern Ireland") |> 
  select(-c(`All Births`,Males,Females)) -> names_NI
###### Scotland ######
read_csv("data/Scotland/babies-first-names-all-names-all-years.csv",
         show_col_types = FALSE) |> 
  select(-position) |> # rank is not calculated the same way as for USA, position is a char string
  rename(year = yr, asab = sex, name = FirstForename) |> 
  mutate(asab = case_when(asab == "B" ~ "M",
                          asab == "G" ~ "F",
                          TRUE ~ "X"),
         region = "Scotland",
         name = str_to_title(name)) |> 
  group_by(year,asab) |> 
  mutate(proportion = number/sum(number, na.rm = TRUE)) |> 
  ungroup() -> names_Scotland
###### Ireland ######
read_csv("data/Ireland/VSA50.20231102T161146-boys.csv",
         show_col_types = FALSE) |> 
  mutate(asab = "M",
         `Statistic Label` = case_when(substr(`Statistic Label`, 50,50)=="R" ~ "rank",
                                       TRUE ~ "number")) |>
  pivot_wider(names_from = `Statistic Label`,
              values_from = VALUE) |> 
  rbind(read_csv("data/Ireland/VSA60.20231102T161100-girls.csv",
                 show_col_types = FALSE) |> 
          mutate(asab = "F",
                 `Statistic Label` = case_when(substr(`Statistic Label`, 51,51)=="R" ~ "rank",
                                               TRUE ~ "number")) |>
          pivot_wider(names_from = `Statistic Label`,
                      values_from = VALUE)) |> 
  select(-UNIT) |> 
  rename(name = Names, year = Year) |> 
  filter(!is.na(number)) |> 
  mutate(region = "Ireland",
         name = str_to_title(name)) |> 
  group_by(year,asab) |> 
  mutate(proportion = number/sum(number, na.rm = TRUE)) |> 
  ungroup() -> names_Ireland
###### New Zealand ######
read_excel("data/NewZealand/Top-100-girls-and-boys-names-since-1954-EDITED.xlsx", 
           sheet = "Boys' Names", 
           skip = 6) |> 
  rename(rank = Rank) |> 
  pivot_longer(cols = 2:139, 
               names_to = "X", 
               values_to = "Y",
               names_prefix = "x",
               values_transform = as.character) |> 
  filter(!is.na(rank)) |> 
  separate_wider_delim(cols = X, 
                       delim = "...", 
                       names = c("year", "type")) |> 
  mutate(type = case_when(as.numeric(type) %% 2 == 0 ~ "name",
                          as.numeric(type) %% 2 == 1 ~ "number")) |> 
  pivot_wider(names_from = type,
              values_from = "Y") |> 
  mutate(asab = "M") |> 
  # join with girl names
  rbind(read_excel("data/NewZealand/Top-100-girls-and-boys-names-since-1954-EDITED.xlsx", 
                   sheet = "Girls' Names", 
                   skip = 6) |> 
          rename(rank = Rank) |> 
          pivot_longer(cols = 2:139, 
                       names_to = "X", 
                       values_to = "Y",
                       names_prefix = "x",
                       values_transform = as.character) |> 
          filter(!is.na(rank)) |> 
          separate_wider_delim(cols = X, 
                               delim = "...", 
                               names = c("year", "type")) |> 
          mutate(type = case_when(as.numeric(type) %% 2 == 0 ~ "name",
                                  as.numeric(type) %% 2 == 1 ~ "number")) |> 
          pivot_wider(names_from = type,
                      values_from = "Y") |> 
          mutate(asab = "F")) |> 
  mutate(year = year |> as.integer(),
         number = number |> as.numeric(),
         name = str_to_title(name)) |> 
  # join with birth data
  left_join(read_delim("data/NewZealand/live_births_1954-2022-EDITED.txt", 
                       delim = "\t", escape_double = FALSE, 
                       na = "..", trim_ws = TRUE,
                       show_col_types = FALSE), 
            by = join_by(year)) |> 
  group_by(year,asab) |> 
  mutate(proportion = case_when(is.na(male) | is.na(female) ~ number/sum(number, na.rm = TRUE),#number/total, #(total/2)
                                asab == "M" ~ number/male,
                                asab == "F" ~ number/female),
         region = "New Zealand") |> 
  ungroup() |> 
  # SEEMS TO HAVE A DUPLICATE MICHAEL, I DONT KNOW HOW BEST TO HANDLE THIS
  filter(year != 1988, rank != 100, name != "Michael") |> # Remove duplicate Michael - no evidence this is diacritics
  select(-c(male, female, total)) -> names_NZ
###### Australia ######
# ACT
# no data, too small (only releases brief statement)
# NSW
names_nsw <- read_csv("data/Australia/NSW/popular_baby_names_1952_to_2022-v2.csv",
                      show_col_types = FALSE) |> 
  rename(rank = Rank,
         name = Name,
         number = Number,
         asab = Gender,
         year = Year) |> 
  mutate(region = "New South Wales",
         name = str_to_title(name),
         asab = case_when(asab == "Male" ~ "M",
                          asab == "Female" ~ "F",
                          TRUE ~ "X"))
# NT
names_nt <- read_csv("data/Australia/NT/NT_babynames_1980_2002-2022.csv",
                     show_col_types = FALSE) |> 
  group_by(year, asab) |>  
  mutate(rank = rank(-number, ties.method = "first") |> as.integer(),
         region = "Northern Territory",
         name = str_to_title(name)) |> 
  ungroup()
# Queensland
read_csv(file = list.files(path = "data/Australia/Queensland",
                           pattern = ".csv",
                           full.names = TRUE),
         col_names = TRUE,
         id = "file",
         show_col_types = FALSE) |> 
  mutate(file = str_remove(file, "data/Australia/Queensland/"),
         file = str_remove(file, "bdm_top-100-baby-names-"),
         file = str_remove(file, ".csv"),
         row = row_number()) |> 
  rename(year = file,
         name_F = "Girl Names",
         number_F = "Count of Girl Names",
         name_M = "Boy Names",
         number_M = "Count of Boy Names") |> 
  pivot_longer(cols = c(2:5),
               names_to = "label",
               values_to = "values",
               values_transform = list(values = as.character)) |> 
  separate(label, into = c("label", "asab"), sep = "_") |> 
  pivot_wider(names_from = label, values_from = values) |> 
  select(-row) |> 
  group_by(year, asab) |> 
  mutate(year = str_sub(year, start = -4, end = -1),
         number = as.numeric(number),
         year = as.numeric(year),
         rank = rank(-number, ties.method = "first") |> as.integer(),
         region = "Queensland") |> 
  ungroup() |> 
  filter(!is.na(name)) |> 
  # join with data from 1960-2005
  rbind(queensland_1960_2005 <- read_csv(file = list.files(path = "data/Australia/Queensland/long_format",
                                                           pattern = ".csv",
                                                           full.names = TRUE),
                                         col_names = TRUE,
                                         id = "file",
                                         show_col_types = FALSE) |> 
          rename(name = "Name",
                 asab = "Sex",
                 year = "Year",
                 number = "Count") |> 
          group_by(year, asab) |> 
          mutate(name = str_to_title(name),
                 asab = case_when(asab == "Male" ~ "M",
                                  asab == "Female" ~ "F",
                                  TRUE ~ "X"),
                 rank = rank(-number) |> as.integer(),
                 region = "Queensland") |> 
          ungroup() |> select(-file)) |> 
  mutate(name = str_to_title(name)) -> names_queensland
# South Australia
read_csv(file = list.files(path = "data/Australia/SouthAustralia/Baby Names 1944-2013",
                           pattern = ".csv",
                           full.names = TRUE),
         col_names = TRUE,
         id = "file",
         show_col_types = FALSE) |> 
  mutate(file = str_remove(file, "data/Australia/SouthAustralia/Baby Names 1944-2013/"),
         file = str_remove(file, "_top.csv")) |> 
  rbind(# had to manually rename "Number" to "Amount" for 2016 files
    read_csv(file = list.files(path = "data/Australia/SouthAustralia",
                               pattern = ".csv",
                               full.names = TRUE),
             col_names = TRUE,
             id = "file",
             show_col_types = FALSE) |> 
      mutate(file = str_remove(file, "data/Australia/SouthAustralia/"),
             file = str_remove(file, ".csv"),
             file = str_remove(file, "top"))) |> 
  rename(name = "Given Name",
         number = "Amount") |> 
  # several files were only top 100 names, not all names, so this throws a warning about file name
  separate(file, into = c("asab","year"), sep = "_") |> 
  group_by(year, asab) |> 
  mutate(name = str_to_title(name),
         rank = rank(-number, ties.method = "first") |> as.integer(),
         asab = case_when(asab == "female" ~ "F",
                          asab == "male" ~ "M",
                          TRUE ~ "X"),
         region = "South Australia",
         year = str_sub(year, -4, -1) |> as.numeric()) |> 
  select(-Position) |> 
  filter(name != "TOTAL", # 2016 included "TOTAL" as if it were a name
         name != "(Not") |> # REMOVE HIGHER FREQUENCY BAD DATA
  ungroup() -> names_soz
# Tasmania
read_excel("data/Australia/Tasmania/top-baby-names-for-2016.xlsx", # needs BIG cleaning
           col_names = FALSE, range = "A3:B136",
           .name_repair = "unique_quiet") |> 
  mutate(asab = "F",
         rank = rank(-`...2`, ties.method = "first")) |> 
  rbind(read_excel("data/Australia/Tasmania/top-baby-names-for-2016.xlsx", # needs BIG cleaning
                   col_names = FALSE, range = "A141:B284",
                   .name_repair = "unique_quiet") |> 
          mutate(asab = "M",
                 rank = rank(-`...2`, ties.method = "first"))) |> 
  rename(name = `...1`,
         number = `...2`) |> 
  mutate(name = str_to_title(name),
         year = 2016,
         region = "Tasmania") |> 
  # join taz_dataset_topbabynamesfor2015 girls names
  rbind(read_excel("data/Australia/Tasmania/dataset-topbabynamesfor2015.xlsx", 
                   sheet = "Top Baby Names 2015 - Female", 
                   col_names = FALSE, skip = 1,
                   .name_repair = "unique_quiet") |> 
          rename(name = `...1`,
                 number = `...2`) |> 
          mutate(name = str_to_title(name),
                 year = 2015,
                 rank = rank(-number) |> as.integer(),
                 asab = "F",
                 region = "Tasmania")) |> 
  # join taz_dataset_topbabynamesfor2015 boys names
  rbind(read_excel("data/Australia/Tasmania/dataset-topbabynamesfor2015.xlsx", 
                   sheet = "Top Baby Names 2015 - Male", 
                   col_names = FALSE, skip = 1,
                   .name_repair = "unique_quiet") |> 
          rename(name = `...1`,
                 number = `...2`) |> 
          mutate(name = str_to_title(name),
                 year = 2015,
                 rank = rank(-number) |> as.integer(),
                 asab = "M",
                 region = "Tasmania")) |> 
  mutate(name = str_to_title(name)) -> names_tasmania
# Victoria
read_csv("data/Australia/Victoria/popular_baby_names.csv",
         show_col_types = FALSE) |> 
  rename(rank = position,
         number = count,
         asab = sex) |> 
  mutate(region = "Victoria",
         asab = case_when(asab == "MALE" ~ "M",
                          asab == "FEMALE" ~ "F",
                          TRUE ~ "X"),
         name = str_to_title(name)) -> names_victoria
# Western Australia
read_csv("data/Australia/WesternAustralia/WA_babynames_1930-2022.csv",
         show_col_types = FALSE) |> 
  mutate(region = "Western Australia",
         name = str_to_title(name)) -> names_woz
# combine australian data
names_nsw |> 
  rbind(names_nt) |> 
  rbind(names_queensland) |> 
  rbind(names_soz) |> 
  rbind(names_tasmania) |> 
  rbind(names_victoria) |> 
  rbind(names_woz) |> #-> names_Australia
  group_by(name, asab, year) |> 
  summarise(number = sum(number),
            .groups = "drop") |> 
  # join with detailed births information
  left_join(read_csv("data/Australia/ABS_BIRTHS_SUMMARY_1.0.0_4+5+1..A.csv",
                     show_col_types = FALSE) |> # messy data, needs cleaning
              rename(measure = `MEASURE: Measure`,
                     region2 = `REGION: Region`,
                     year = `TIME_PERIOD: Time Period`,
                     population = OBS_VALUE) |> 
              select(measure, region2, year, population) |> 
              filter(region2 == "AUS: Australia",
                     measure != "1: Births") |> 
              pivot_wider(names_from = measure,
                          values_from = population),
            by = join_by("year")) |> 
  mutate(proportion = case_when(asab == "M" ~ number/`4: Male births`,
                                asab == "F" ~ number/`5: Female births`),
         region = "Australia") |> 
  group_by(year, asab) |> 
  mutate(rank = rank(-number, ties.method = "first")) |> 
  select(year, name, number, rank, asab, region, proportion) |> 
  ungroup() |> 
  # join with older historical data (missing ASAB; solution *estimate* by multiplying annual births by .5)
  left_join(read_csv("data/Australia/Births registered – 1934 to 2022(a).csv", skip = 1,
                     show_col_types = FALSE) |> 
              rename(year = Year, 
                     population = `Births registered`),
            by = join_by("year")) |> 
  mutate(proportion = case_when(is.na(proportion) ~ number/(population*.5), # no births by ASAB data, going by .5 of annual births
                                TRUE ~ proportion)) |> 
  select(-population) -> names_Australia

##### COMBINE #####
names_Australia |> 
  rbind(names_Canada) |> 
  rbind(names_EW) |> 
  rbind(names_Ireland) |> 
  rbind(names_NI) |> 
  rbind(names_NZ) |> 
  rbind(names_Scotland) |> 
  rbind(names_USA) -> names_combined
#})
