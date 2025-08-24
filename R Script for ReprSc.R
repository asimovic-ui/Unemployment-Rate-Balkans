library(tidyverse)
library(readxl)
library(visdat)
library(knitr)
library(flextable)

getwd()

view(Unempl_Tot)

Unempl_Tot <- Unempl_Tot %>% select(-c(4:65))
view(Unempl_Tot)

unempl_male <- unempl_male %>% select(-c(4:65))
view(unempl_male)

Unempl_Tot <- Unempl_Tot %>%
  mutate(across(4:6, round))
view(Unempl_Tot)

unempl_male <- unempl_male %>%
  mutate(across(4:6, round))
view(unempl_male)

vis_miss(Unempl_Tot)

view(Unempl_Tot)
view(unempl_male)



merged_data <- merge(Unempl_Tot, unempl_male, by = "Country Name")
view(merged_data)
merged_data_left <- left_join(Unempl_Tot, unempl_male, by = "Country Name")
view(merged_data_left)


merged_data <- merge(Unempl_Tot, unempl_male, by = "Country Name")
view(merged_data)



library(dplyr)
library(ggplot2)


wb_merged_data_tidy <- wb_merged_data_tidy %>%
  pivot_longer(cols = 3:last_col(), names_to = "Year", values_to = "2022")


view(merged_data_left)
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
                  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", 
                  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
                  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")


view(merged_data_left)

bal_countries <- c("Serbia", "Bosnia and Herzegowina", "Montenegro","Greece", "Albania", "North Macedonia")
view bal_countries

merged_data <- merged_data %>%
  filter(Country Code.x %in% bal_countries)

ggplot(data = merged_data, aes(x = Country Code.x, "Unemployment, total", "Unemployment, male")) + 
  geom_col() + theme_minimal()

ggplot(data = merged_data, aes(x = `Country Name`, y = Unemployment, total, fill = `Unemployment,male`)) + 
  geom_col() + 
  theme_minimal

ggplot(data = merged_data, aes(x = Country Code.x, 'Indicator Name')) + 
  geom_col() + theme_minimal()

ggplot(data = merged_data, aes(x = `Country Name`, y = `Indicator Name`)) + 
  geom_col() + 
  theme_minimal()

ggplot(data = merged_data, aes(x = `Country Name`, y = `Indicator Name.x`)) + 
  geom_col() + 
  theme_minimal()

colnames(merged_data)

ggplot(data = merged_data, aes(x = `Country Code.x`, y = `Indicator Name.x`)) + 
  geom_col() + 
  theme_minimal()

ggplot(data = merged_data, aes(x = reorder(Country Code.x, Indicator Name.x), Indicator Name)) + 
  geom_col() + theme_minimal()

ggplot(data = merged_data, aes(x = reorder(`Country Code.x`, `Indicator Name.x`), y = `Indicator Name.x`)) + 
  geom_col() + 
  theme_minimal()

warnings()

ggplot(data = merged_data, aes(x = reorder(Country Name, "2022"), y = Indicator_Name)) + 
  geom_col() + theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment total in %", 
       title = " across the Balkan, N = 6")


ggplot(data = merged_data, aes(x = reorder(`Country Name`, `Indicator Name`), y = `Indicator_Name`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Total (%)", 
       title = "Unemployment Across the Balkans, N = 6")



ggplot(data = merged_data, aes(x = reorder(`Country Name`, `Indicator Name.x`, `Indicator_Name.y`), y = `2022`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Total in %", 
       title = "Unemployment Across the Balkans, N = 6")


ggplot(data = merged_data, aes(x = reorder(`Country Name`), y = `2022`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Total in %", 
       title = "Unemployment Across the Balkans, N = 6")

ggplot(data = merged_data, aes(x = reorder(`Country Name`, `2022.y`), y = `2022.x`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Total in %", 
       title = "Unemployment Across the Balkans, N = 6")


merge(unempl_male, Unempl_Tot, all=TRUE)
merged_data_1 <- merge(Unempl_Tot, unempl_male,"by Country Name"
)


merged_data_1 <- merge(Unempl_Tot, unempl_male, by.x = "Country Name", by.y = "Country Name")

head(merged_data_1)
view(merged_data_1)

rename(merged_data_1, Unemply Total = '2022.x')

merged_data_1 <- rename(merged_data_1, `Unemply Total` = `2022.x`)
merged_data_1 <- rename(merged_data_1, `Unempl Male` = `2022.y`)
view(merged_data_1)


merged_data_1 <- merged_data_1 %>%
  select(-c(2, 3, 4, 6, 7, 8, 9, 11))
view(merged_data_1)
merged_data_1 <- na.omit(merged_data_1)
view(merged_data_1)


bal_countries <- c("Serbia", "Bosnia and Herzegowina", "Montenegro","Greece", "Albania", "North Macedonia", "Kosovo")
view (bal_countries)



merged_data_1 <- merged_data_1 %>%
  filter(Country Name %in% bal_countries)

merged_data_1 <- merged_data_1 %>%
  filter(`Country Name` %in% bal_countries)
view(merged_data_1)


ggplot(data = merged_data_1, aes(x = Country Name, `Unemply Total`)) + 
  geomcol() + theme_minimal(_)


ggplot(data = merged_data_1, aes(x = `Country Name`, y = `Unemply Total`)) + 
  geom_col() + 
  theme_minimal()


ggplot(data = merged_data, aes(x = reorder(Country Name, -Unemply Total), Unemply Total)) + 
  geom_col() + theme_minimal()


ggplot(data = merged_data, aes(x = reorder(`Country Name`, -`Unemply Total`), y = `Unemply Total`)) + 
  geom_col() + 
  theme_minimal() +
  labs

ggplot(data = merged_data, aes(x = reorder(`Country Name`, -`Unemply Total`))) + 
  geom_col(aes(y = `Unemply Total`), fill = "blue") + 
  geom_col(aes(y = `UNempl Male`), fill = "red", alpha = 0.5) + 
  theme_minimal() +
  labs(x = "Country Name", y = "Values") +
  facet_wrap(~ variable, scales = "free_y")

ggplot(data = merged_data, aes(x = reorder("Country Name", -"Unemply Total"), y = "Unemply Total")) + 
  geom_col() + 
  theme_minimal() +
  labs(x = "Country Name", y = "Unemply Total")
ggplot(data = merged_data, aes(x = reorder(`Country Name`, -`Unemply Total`), y = `Unemply Total`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = "Country Name", y = "Unemply Total")


ggplot(data = merged_data, aes(x = reorder(`Country Name`, -`Unemply Total`))) + 
  geom_col(aes(y = `Unemply Total`), fill = "blue") + 
  geom_col(aes(y = `Unempl Male`), fill = "red", alpha = 0.5) + 
  theme_minimal() +
  labs(x = "Country Name", y = "Values") +
  facet_wrap(~ variable, scales = "free_y")

ggplot(data = merged_data_1, aes(x = reorder(`Country Name`, -Unempl_Tot), y = value, fill = variable)) + 
  geom_col(position = "dodge") + 
  theme_minimal() +
  labs(x = "Country Name", y = "Values", fill = "Legend") +
  scale_fill_manual(values = c("Unemply Total" = "blue", "Unempl Male" = "red"))


long_data <- merged_data_1 %>%
  pivot_longer(cols = c(Unempl_Tot, Unempl_Male), 
               names_to = "variable", 
               values_to = "value")

long_data <- merged_data_1 %>%
  pivot_longer(cols = c(Unemply Total, Unempl Male), 
               names_to = "variable", 
               values_to = "value")

long_data <- merged_data_1 %>%
  pivot_longer(cols = c(`Unemply Total`, `Unempl Male`), 
               names_to = "variable", 
               values_to = "value")

view(long_data)


ggplot(data = long_data, aes(x = reorder(`Country Name`, -value), y = value, fill = variable)) + 
  geom_col(position = "dodge") + 
  theme_minimal() +
  labs(x = "Country Name", y = "Values", fill = "Legend") +
  scale_fill_manual(values  = c("Unemply_Total" = "blue", "Unempl_Male" = "red"))


ggplot(data = long_data, aes(x = reorder(`Country Name`, -value), y = value, fill = variable)) + 
  geom_col(position = "dodge") + 
  theme_minimal() +
  labs(x = "Country Name", y = "Unemployment rate in %", fill = "Legend") +
  scale_fill_manual(values = c(`Unemply Total` = "orange", `Unempl Male` = "yellow"))


ggplot(merged_data, aes(x = Unemply_Total, y = Unempl Male, label = Country name)) +
  geom_point(alpha = 0.75) +
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(title = "Unemployment total vs male, N = 6", 
       x = "Unemply_Total", 
       y = "Unempl Male") +
  theme_minimal()


ggplot(long_data, aes(x = `Unemply_Total`, y = `Unempl Male`, label = `Country Name`)) +
  geom_point(alpha = 0.75) +
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(title = "Unemployment Total vs Male", 
       x = "Unemployment Total", 
       y = "Unemployment Male") +
  theme_minimal


plot_data <- long_data %>%
  filter(variable %in% c("Unemply_Total", "Unempl Male"))

ggplot(plot_data, aes(x = `value`, y = `value`, label = `Country Name`, color = variable)) +
  geom_point(alpha = 0.75) +
  geom_text(hjust = 0, vjust = 1.5, size = 3) +
  labs(title = "Unemployment Total vs Male", 
       x = "Unemployment Male", 
       y = "Unemployment Total") +
  theme_minimal()

ggplot(data = long_data, aes(x = reorder(Country Name, -Unemply Total), y = Unemployment Male, ymin = 0, ymax = Unemployment Total)) + 
  geom_pointrange(color = ifelse(long_data$Country Name == "Serbia", "darkred", "black"), linewidth = 0.85) + theme_minimal() +
  labs(x = NULL, y = "Unemployment Total", title = "Unemployment Total, N = 6") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data = long_data, aes(x = reorder(`Country Name`, -`Unemply Total`), 
                             y = `Unemployment Male`, 
                             ymin = 0, 
                             ymax = `Unemploy Total`)) + 
  geom_pointrange(aes(color = ifelse(`Country Name` == "Serbia", "darkred", "black")), 
                  linewidth = 0.85) + 
  theme_minimal() +
  labs(x = NULL, y = "Unemply Total", title = "Unemply Total, N = 6") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_identity() 


ggplot(data = long_data, aes(x = reorder(`Country Name`, -`Unemply Total`), 
                             y = `Unempl Male`, 
                             ymin = 0, 
                             ymax = `Unemply Total`)) + 
  geom_pointrange(aes(color = ifelse(`Country Name` == "Serbia", "darkred", "black")), 
                  linewidth = 0.85) + 
  theme_minimal() +
  labs(x = NULL, y = "Unemployment Total", title = "Unemployment Total, N = 6") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_identity()


ggplot(data = long_data, aes(x = reorder(Country Name, -Unemployment Total), y = Unemployment Total)) + 
  geom_col() + theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Rate Total vs Unemployment Rate male in %", 
       title = "Unemployment Rate Total vs Male in Balkan, N = 6")


ggplot(data = long_data, aes(x = reorder(`Country Name`, -`Unemply Total`), 
                             y = `Unemply Total`)) + 
  geom_col() + 
  theme_minimal() +
  labs(x = NULL, 
       y = "Unemployment Rate Total vs Unemployment Rate Male in %", 
       title = "Unemployment Rate Total vs Male in Balkan, N = 6")



table_contents <- long_data %>% 
  select(Country Name, Unemply Total, Unempl Male) %>%
  arrange(Country)

table_contents <- long_data %>% 
  select(`Country Name`, `Unemply Total`, `Unempl Male`) %>%
  arrange(`Country Name`)



long_data <-long_data_1 %>%
  pivot_longer(cols = c(variable, value)
               names_to = "variable", 
               values_to = "value")


long_data <- data %>%
  pivot_longer(cols = `Value`, 
               names_to = "Variable", 
               
               data <- data  1 %>%
                 Country = c("Greece", "Greece", "Kosovo", "Kosovo", "Montenegro", "Montenegro", "North Macedonia", "North Macedonia", "Serbia", "Serbia"),
               Variable = c("Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male"),
               Value = c(12, 9, 12, 11, 15, 16, 14, 16, 8, 8)
  )

view(data.frame())


long_data <- data_table  %>%
  Country = c("Greece", "Greece", "Kosovo", "Kosovo", "Montenegro", "Montenegro", "North Macedonia", "North Macedonia", "Serbia", "Serbia"),
Variable = c("Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male"),
Value = c(12, 9, 12, 11, 15, 16, 14, 16, 8, 8)


long_data <- data.frame(
  Country = c("Greece", "Greece", "Kosovo", "Kosovo", "Montenegro", "Montenegro", "North Macedonia", "North Macedonia", "Serbia", "Serbia"),
  Variable = c("Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male", "Unemply Total", "Unempl Male"),
  Value = c(12, 9, 12, 11, 15, 16, 14, 16, 8, 8)
)

view(long_data)


table_contents <- merged_data_1 %>% 
  select(`Country Name`, `Unemply Total`, `Unempl Male`) %>%
  rename(Country = Country Name, `Total Unemplyoment` = Unemply Total, 'Unemployment Male' = Unempl Male) %>%
  arrange(Country) 

table_contents <- merged_data_1 %>% 
  select(`Country Name`, `Unemply Total`, `Unempl Male`) %>%
  rename(
    Country = `Country Name`, 
    `Total Unemployment` = `Unemply Total`, 
    `Unemployment Male` = `Unempl Male`
  ) %>%
  arrange(Country)

view(merged_data_1)

flextable(table_contents) %>%
  save_as_docx(path = "table_unempl total vs male.docx")  