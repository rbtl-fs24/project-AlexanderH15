library(tidyverse)
library(gapminder)

litter_raw <- read_csv("data/raw/litter_waste.csv")

#glimpse(litter_raw)
#Normalizing:

a <- names(litter_raw)
a <- a[6:22]

#for (i in a) {
#litter_per_km <- litter_raw |> 
#  mutate("i" = "i" / distance_m)
#}
litter_per_km <- litter_raw

for (i in a) {
  litter_per_km[,i] <- litter_raw[,i] / litter_raw[,4] *1000
  
}
#glimpse(litter_per_km)


#Pivot long

#litter_long <- litter_per_km |> 
#  pivot_longer(cols = paper_m_g:total_m_g_measured,
#               names_to = "waste_category",
#               values_to = "amount")
#glimpse(litter_long)

# transform m_g variables
data_mass <- litter_per_km |> 
  select(id:notes, ends_with("m_g")) |> 
  pivot_longer(-c(id:notes), 
               names_to = "waste_category",
               values_to = "mass") |> 
  mutate(waste_category = gsub(pattern = "_m_g", replacement = "", x = waste_category))

# transform _n variables
data_amount <- litter_per_km |> 
  select(id:notes, ends_with("_n")) |> 
  pivot_longer(-c(id:notes), 
               names_to = "waste_category",
               values_to = "amount") |> 
  mutate(waste_category = gsub(pattern = "_n", replacement = "", x = waste_category))

# merge the two
litter <- data_mass |> 
  left_join(data_amount)

write_csv(litter, "data/processed/litter_waste_processed.csv")



#Location Overview----------------------------------------------------------

loc <- subset(litter, waste_category=="total")

loc |> 
  mutate(date=NULL,waste_category=NULL) |> 
  rename('distance (m)' = 'distance_m')
  #rename('Total Mass' = 'mass') |> 
  #rename('Total Amount' = 'amount')

loc2 <- loc |> 
  mutate(street = case_when(id < 4 ~ 'Zürichstrasse',
                            id < 6 ~ 'Witikonstrasse(Binz)',
                            id > 5 ~ 'Witikonerstrasse(Pfaffhausen)')) |>
  mutate(length = case_when(id < 4 ~ 254,
                            id < 6 ~ 351,
                            id > 5 ~ 423)) |>
  mutate(id=NULL,date=NULL,waste_category=NULL,location=NULL,distance_m=NULL) |> 
  relocate(length) |> 
  relocate(street) 

write_csv(loc2, "data/final/location_overview.csv")


#Section comparison------------------------------------------------------
sec_z1 <- subset(litter, id < 4)
sec_z <- subset(sec_z1, waste_category != "total")

sec_z |> 
  mutate(id=NULL,date=NULL,notes=NULL, distance_m=NULL)

write_csv(sec_z, "data/final/section_comparison.csv")



#Total Analysis-----------------------------------------------------------


litter_total <- subset(litter, waste_category != "total")

litter_total2 <- litter_total |> 
  mutate(street = case_when(id < 4 ~ 'Zürichstrasse',
                            id < 6 ~ 'Witikonstrasse(Binz)',
                            id > 5 ~ 'Witikonerstrasse(Pfaffhausen)')) |> 
  mutate(id=NULL,date=NULL,notes=NULL, distance_m=NULL, location=NULL) |> 
  relocate(street)

litter_mean <- litter_total2 |> 
  group_by(street, waste_category) |> 
  summarise(mean_mass = mean(mass),
            mean_amount = mean(amount),
            .groups = 'drop')

write_csv(litter_mean, "data/final/total_analysis.csv")



