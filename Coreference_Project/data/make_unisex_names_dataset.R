unisex_names <- names_combined |> 
  mutate(asab = case_when(asab == "F" ~ "AFAB", asab == "M" ~ "AMAB", TRUE ~ "X"),
         asab = factor(asab, levels = c("AFAB", "AMAB", "X")),
         name = str_to_sentence(name)) |> 
  #filter(number > 1) |> 
  group_by(name, asab, region) |> 
  mutate(year_count = n_distinct(year)) |> ungroup() |> 
  pivot_wider(names_from = asab,
              values_from = c(number, rank, proportion, year_count),
              values_fill = 0) |> 
  #filter(!is.na(number_AFAB),!is.na(number_AMAB)) |> 
  mutate(year_count_reg = case_when(year_count_AFAB < year_count_AMAB ~ year_count_AFAB, TRUE ~ year_count_AMAB)) |> 
  select(-c(year_count_AFAB, year_count_AMAB)) |> 
  group_by(name) |> mutate(year_count = max(year_count_reg)) |> ungroup() |> 
  filter(year_count > 0)

write_csv(unisex_names, "data/unisex_names.csv")
