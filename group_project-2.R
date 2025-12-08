# GROUP PROJECT 2
library(rJava)
library(xlsx)
library(tidyverse)


pistols <- read.csv("pistols_manufacturers.csv")
pistols <- pistols %>% select(where(~any(!is.na(.))))
pistols$Street <- coalesce(pistols$Street, pistols$X.4)
pistols$X.4 <- NULL

precip <- read.csv("precipitation-1.csv", skip=3)
drought <- read.csv("drought_severity_index-1.csv", skip=2)
temp <- read.csv("average_temp-1.csv", skip=3)
agriculture <- read.csv("US_Counties_Agriculture_Census_(USDA)-1.csv")
population <- read.xlsx("co-est2024-pop-1.xlsx", sheetIndex=1)
population <- population[-1,]
population <- population[1:3144,]
population$Geographic.Area <- substr(population$Geographic.Area, 2, 100)
population <- population %>% separate(
  Geographic.Area, 
  into=c("County", "State"),
  sep=","
  )
population$County <- tolower(population$County)
population$County <- gsub("[^a-z0-9]", "", population$County)
temp$Name <- tolower(temp$Name)
temp$Name <- gsub("[^a-z0-9]", "", temp$Name)
drought$Name <- tolower(drought$Name)
drought$Name <- gsub("[^a-z0-9]", "", drought$Name)
precip$Name <- tolower(precip$Name)
precip$Name <- gsub("[^a-z0-9]", "", precip$Name)
agriculture$atlas_name <- tolower(agriculture$atlas_name)
agriculture$atlas_name <- gsub("[^a-z0-9]", "", agriculture$atlas_name)
agriculture$atlas_name <- paste0(agriculture$atlas_name, "county")

combined_data <- merge(temp[,1:5], precip[,c(1,4,5)], by="ID")
combined_data <- merge(combined_data, drought[,c(1,4,5)], by="ID")
combined_data <- combined_data %>% rename(
  temp = Value.x,
  precip = Value.y,
  drought = Value
)
combined_data$Name <- paste0(combined_data$Name, tolower(combined_data$State))
agriculture$atlas_name <- paste0(agriculture$atlas_name, 
                                 tolower(agriculture$State))
combined_data <- merge(combined_data, agriculture, by.x="Name", 
                       by.y="atlas_name", all.x=TRUE)
population$State <- substr(population$State, 2, 1000)
population$County <- paste0(population$County, tolower(population$State))
combined_data <- merge(combined_data, population, by.x="Name", by.y="County", 
                       all.x=TRUE)

cities <- read.csv("uscities.csv")
pistols$City <- tolower(pistols$City)
pistols$City <- gsub("[^a-z0-9]", "", pistols$City)
pistols$City <- paste0(pistols$City, tolower(pistols$State))
cities$city_ascii <- tolower(cities$city_ascii)
cities$city_ascii <- gsub("[^a-z0-9]", "", cities$city_ascii)
cities$city_ascii <- paste0(cities$city_ascii, tolower(cities$state_id))
cities <- cities %>% distinct(city_ascii, .keep_all=TRUE)
pistols <- pistols %>% distinct(RDS.Key, .keep_all=TRUE)
pistols <- merge(pistols, cities[,2:6], by.x="City", by.y="city_ascii", 
                 all.x=TRUE)
pistols <- pistols %>% filter(!is.na(county_name))
pistols$county_name <- tolower(pistols$county_name)
pistols$county_name <- gsub("[^a-z0-9]", "", pistols$county_name)
pistols$county_name <- paste0(pistols$county_name, "county") 
pistols$county_name <- paste0(pistols$county_name,  
                              tolower(pistols$state_name))
pistols <- pistols %>% select(TOTAL, state_name, county_name) %>%
  group_by(county_name, state_name) %>%
  summarize(
    total_pistols = sum(TOTAL, na.rm=TRUE)
  )
combined_data <- merge(combined_data, pistols, by.x="Name", by.y="county_name", 
                       all.x=TRUE)
combined_data$total_pistols[is.na(combined_data$total_pistols)] <- 0
combined_data$gun_manufacturer <- combined_data$total_pistols != 0

write.csv(combined_data, "combined_data-2", row.names=FALSE)

combined_data <- combined_data %>% mutate(
  pop_density = X2024/atlas_area,
  percent_farmland = LandinFarmsAcr/atlas_acre
)

ggplot(combined_data, aes(x=percent_farmland)) +
  geom_histogram()

ggplot(combined_data, aes(x=percent_farmland)) +
  geom_boxplot()

ggplot(combined_data, aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 10000), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 2500), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 2500, percent_farmland > 0.75), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 2500, percent_farmland > 0.75), 
       aes(x=pop_density, y=percent_farmland, color=gun_manufacturer)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 10000, percent_farmland > 0.75), 
       aes(x=pop_density, y=percent_farmland, color=gun_manufacturer)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 10000, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 10000, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point() +
  geom_smooth(se=FALSE)

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland, color=drought)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland, color=temp)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland, color=precip)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=percent_farmland, color=total_pistols)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer), 
       aes(x=pop_density, y=total_pistols, color=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer, total_pistols < 1000),
       aes(x=pop_density, y=total_pistols, color=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer, total_pistols < 250), 
       aes(x=pop_density, y=total_pistols, color=percent_farmland)) + 
  geom_point()

ggplot(combined_data %>% filter(pop_density < 200, percent_farmland > 0.75, 
                                gun_manufacturer, total_pistols < 100), 
       aes(x=pop_density, y=total_pistols, color=percent_farmland)) + 
  geom_point()

ggplot(combined_data, aes(x=gun_manufacturer, y=percent_farmland)) +
  geom_boxplot()

ggplot(combined_data, aes(x=gun_manufacturer, y=pop_density)) +
  geom_boxplot()

install.packages("ggridges")
library(ggridges)

ggplot(combined_data %>% filter(pop_density < 200),
       aes(x=pop_density, y=gun_manufacturer, color=gun_manufacturer, 
           fill=gun_manufacturer)) +
  geom_density_ridges(alpha=0.8)

ggplot(combined_data, 
       aes(x=state_abbr, fill=(pop_density < 200 & percent_farmland > 0.75 & 
                                gun_manufacturer))) +
  geom_bar()

combined_data_filtered <- combined_data %>% 
  filter(pop_density < 50, percent_farmland > 0.75, gun_manufacturer)

ggplot(combined_data_filtered, aes(x=pop_density, y=percent_farmland)) +
  geom_point()

ggplot(combined_data_filtered, aes(x=drought)) +
  geom_histogram()

combined_data_filtered <- combined_data_filtered %>% arrange(desc(drought))

ggplot(combined_data_filtered, 
       aes(x=drought, y=state_abbr, color = state_abbr, fill=state_abbr)) +
         geom_density_ridges(alpha=0.8)

min_temp <- read.csv("jan_min_temp.csv", skip=3)
max_temp <- read.csv("july_max_temp.csv", skip=3)

min_temp <- min_temp %>% select(ID, Value) %>% rename(min_temp = Value)
max_temp <- max_temp %>% select(ID, Value) %>% rename(max_temp = Value)

combined_data <- merge(combined_data, min_temp, by="ID", all.x=TRUE)
combined_data_filtered <- merge(combined_data_filtered, min_temp, by="ID", all.x=TRUE)

combined_data <- merge(combined_data, max_temp, by="ID", all.x=TRUE)
combined_data_filtered <- merge(combined_data_filtered, max_temp, by="ID", all.x=TRUE)

ggplot(combined_data_filtered, 
       aes(x=min_temp, y=state_abbr, color=state_abbr, fill=state_abbr)) +
  geom_density_ridges(alpha=0.8)

ggplot(combined_data_filtered, 
       aes(x=max_temp, y=state_abbr, color=state_abbr, fill=state_abbr)) +
  geom_density_ridges(alpha=0.8)

ggplot(combined_data_filtered, aes(x=min_temp, y=max_temp, color=drought)) +
  geom_point() +
  scale_color_gradient(high="blue", low="yellow")

combined_data <- combined_data %>% mutate(
  percent_farm_failed = FailedCropAcr/CroplandAcr
)

combined_data_filtered <- combined_data_filtered %>% mutate(
  percent_farm_failed = FailedCropAcr/CroplandAcr
)

ggplot(combined_data_filtered, aes(x=percent_farm_failed)) +
  geom_histogram()

ggplot(combined_data_filtered %>% filter(percent_farm_failed < 0.1), 
       aes(x=min_temp, y=max_temp, color=drought)) +
  geom_point() +
  scale_color_gradient(high="blue", low="yellow")

avg_temp <- read.csv("avg_temp-2.csv", skip=3)
combined_data <- merge(combined_data, avg_temp %>% select(ID, Value) %>% rename(avg_temp = Value), by="ID")
combined_data_filtered <- merge(combined_data_filtered, avg_temp %>% select(ID, Value) %>% rename(avg_temp = Value), by="ID")

ggplot(avg_temp, aes(x=Value)) +
  geom_histogram()

ggplot(combined_data_filtered, aes(x=avg_temp)) +
  geom_histogram()

combined_data <- combined_data %>% mutate(temp_range = max_temp-min_temp)
combined_data_filtered <- combined_data_filtered %>% mutate(temp_range = max_temp-min_temp)

ggplot(combined_data, aes(x=temp_range)) + 
  geom_histogram()

ggplot(combined_data_filtered, aes(x=temp_range)) + 
  geom_histogram()

ggplot(combined_data, aes(x=min_temp, y=max_temp, color=temp_range)) + 
  geom_point()

ggplot(combined_data_filtered, aes(x=min_temp, y=max_temp, color=temp_range)) + 
  geom_point()

combined_data %>% filter(
  gun_manufacturer,
  percent_farmland > 0.65,
  pop_density < 50,
  drought < 3,
  drought > 0, 
  avg_temp > 55,
  avg_temp < 71, 
  temp_range < 75,
  percent_farm_failed < 0.1
)

indicies <- combined_data %>% select(
    ID, 
    Name, 
    pop_density, 
    percent_farmland, 
    gun_manufacturer, 
    percent_farm_failed, 
    avg_temp, 
    temp_range, 
    drought) %>% 
  mutate(
    p_1 = (pop_density^3)/1000000, 
    pf_1 = (1.5/percent_farmland) - 1.5,
    g_1 = -1000 * (as.numeric(gun_manufacturer) - 1),
    ff_1 = (percent_farm_failed^3)/0.01,
    at_1 = ((avg_temp-60)^2)/25,
    tr_1 = (temp_range^2)/5625,
    d_1 = ((drought-1.5)^4)/5.0625,
    index_1 = p_1 + pf_1 + g_1 + ff_1 + at_1 + tr_1 + d_1) %>%
  arrange((index_1))

ggplot(indicies, aes(x=index_1)) + 
  geom_boxplot()

ggplot(indicies %>% filter(index_1 < 10000), aes(x=index_1)) +
  geom_boxplot()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=index_1)) + 
  geom_histogram()

ggplot(indicies %>% filter(index_1 < 2500, gun_manufacturer), aes(x=index_1)) +
  geom_histogram()

ggplot(indicies %>% filter(index_1 < 500), aes(x=index_1)) +
  geom_histogram()

ggplot(indicies %>% filter(index_1 < 100), aes(x=index_1)) +
  geom_histogram()

ggplot(indicies %>% filter(index_1 < 25), aes(x=index_1)) +
  geom_histogram()

indicies <- indicies %>% mutate(index_no_g = index_1 - g_1)

ggplot(indicies %>% filter(index_no_g < 100), aes(x=index_no_g)) +
  geom_histogram()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=p_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=pf_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=g_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=ff_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=at_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=tr_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=d_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=p_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=pf_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=g_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=ff_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=at_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=tr_1, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=d_1, y=index_1)) +
  geom_point()


ggplot(indicies %>% filter(index_1 < 2500), aes(x=pop_density, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=percent_farmland, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=percent_farm_failed, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=avg_temp, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=temp_range, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 2500), aes(x=drought, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=pop_density, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=percent_farmland, y=index_1)) +
  geom_point()


ggplot(indicies %>% filter(index_1 < 50), aes(x=percent_farm_failed, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=avg_temp, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=temp_range, y=index_1)) +
  geom_point()

ggplot(indicies %>% filter(index_1 < 50), aes(x=drought, y=index_1)) +
  geom_point()

install.packages("common")
library(common)

ggplot(combined_data, aes(x=pop_density)) + 
  geom_histogram(binwidth = 1000) +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Number of Counties",
    title="Contiguous US Counties Population Density Distribution"
  ) +
  theme_minimal()

ggplot(combined_data, aes(x=percent_farmland)) + 
  geom_histogram() +
  labs(
    x="Percent Farmland",
    y="Number of Counties",
    title="Percentage of County Area That is Farmland"
  ) +
  theme_minimal()

ggplot(combined_data, aes(x=pop_density, y=percent_farmland)) + 
  geom_point(alpha = 0.6) +
  labs(
    x="Population Density",
    y="Percent Farmland",
    title="Percent Farmland vs. Population Density"
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<10000), aes(x=pop_density, y=percent_farmland)) + 
  geom_point(alpha = 0.6) +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 10,000 people/mi", supsc("2"))
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<2500), aes(x=pop_density, y=percent_farmland)) + 
  geom_point(alpha = 0.6) +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 2,500 people/mi", supsc("2"))
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<100), aes(x=pop_density, y=percent_farmland)) + 
  geom_point(alpha = 0.6) +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"))
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<100, percent_farmland > 0.6), 
       aes(x=pop_density, y=percent_farmland)) + 
  geom_point(alpha = 0.6) +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%")
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<100, percent_farmland > 0.6), 
       aes(x=pop_density, y=percent_farmland, color = total_pistols)) +
  geom_point() +
  scale_color_gradient(high="red", low="blue") +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%"),
    color = "Total\nPistols"
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(pop_density<100, percent_farmland > 0.6), 
       aes(x=pop_density, y=percent_farmland, color = gun_manufacturer)) +
  geom_point() +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%"),
    color = "County Has\nGun Manufacturer"
  ) +
  theme_minimal()

combined_data <- combined_data %>% 
  mutate(candidate = pop_density < 100 & percent_farmland > 0.6 & gun_manufacturer)

ggplot(combined_data %>% filter(candidate), 
       aes(x=pop_density, y=percent_farmland, color = drought)) +
  geom_point() +
  scale_color_gradient(high="red", low="blue") +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    color = "Drought\nSeverity\nIndex"
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(candidate), 
       aes(x=pop_density, y=percent_farmland, color = avg_temp)) +
  geom_point() +
  scale_color_gradient(high="red", low="blue") +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    color = "Average\nTemperature"
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(candidate), 
       aes(x=pop_density, y=percent_farmland, color = temp_range)) +
  geom_point() +
  scale_color_gradient(high="red", low="blue") +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    color = "Temperature\nRange"
  ) +
  theme_minimal()

ggplot(combined_data %>% filter(candidate), 
       aes(x=pop_density, y=percent_farmland, color = percent_farm_failed)) +
  geom_point() +
  scale_color_gradient(high="red", low="blue") +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"), ")"),
    y="Percent Farmland",
    title = "Percent Farmland vs. Population Density",
    subtitle = str_c("Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    color = "% Farmland\nFailed"
  ) +
  theme_minimal()

ggplot(combined_data%>%filter(!is.na(candidate)), 
       aes(x=drought, y=candidate, color=candidate, fill=candidate)) +
  geom_density_ridges(alpha=0.7, show.legend=FALSE) +
  labs(
    title = "Drought Severity Index Distribution for Candidate and Non Candidate Counties",
    subtitle = str_c("Candidate County: Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    y = "Candidate County",
    x = "Drought Severity Index"
  ) +
  theme_minimal()

ggplot(combined_data%>%filter(!is.na(candidate)), 
       aes(x=avg_temp, y=candidate, color=candidate, fill=candidate)) +
  geom_density_ridges(alpha=0.7, show.legend=FALSE) +
  labs(
    title = "Average Temperature Distribution for Candidate and Non Candidate Counties",
    subtitle = str_c("Candidate County: Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    y = "Candidate County",
    x = "Average Temperature"
  ) +
  theme_minimal()

ggplot(combined_data%>%filter(!is.na(candidate)), 
       aes(x=temp_range, y=candidate, color=candidate, fill=candidate)) +
  geom_density_ridges(alpha=0.7, show.legend=FALSE) +
  labs(
    title = "Temperature Range Distribution for Candidate and Non Candidate Counties",
    subtitle = str_c("Candidate County: Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    y = "Candidate County",
    x = "Temperature Range"
  ) +
  theme_minimal()

ggplot(combined_data%>%filter(!is.na(candidate)), 
       aes(x=percent_farm_failed, y=candidate, color=candidate, fill=candidate)) +
  geom_density_ridges(alpha=0.7, show.legend=FALSE) +
  labs(
    title = "Percent Farmland Failed Distribution for Candidate and Non Candidate Counties",
    subtitle = str_c("Candidate County: Population Density < 100 people/mi", supsc("2"),
                     ", Percent Farmland > 60%, Has Gun Manufacturer"),
    y = "Candidate County",
    x = "Percent Farmland Failed"
  ) +
  theme_minimal()

ggplot(indicies %>% filter(index_1 < 50), aes(x=pop_density, y=index_1)) +
  geom_point() +
  labs(
    x=str_c("Population Density (people/mi", supsc("2"),")"),
    y="Zombie Index",
    title = "Zombie Index vs. Population Density",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()

ggplot(indicies %>% filter(index_1 < 50), aes(x=percent_farmland, y=index_1)) +
  geom_point() +
  labs(
    x="Percent Farmland",
    y="Zombie Index",
    title = "Zombie Index vs. Percent Farmland",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()


ggplot(indicies %>% filter(index_1 < 50), aes(x=percent_farm_failed, y=index_1)) +
  geom_point() +
  labs(
    x="Percent Farmland Failed",
    y="Zombie Index",
    title = "Zombie Index vs. Percent Farmland Failed",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()

ggplot(indicies %>% filter(index_1 < 50), aes(x=avg_temp, y=index_1)) +
  geom_point() +
  labs(
    x="Average Temperature",
    y="Zombie Index",
    title = "Zombie Index vs. Average Temperature",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()

ggplot(indicies %>% filter(index_1 < 50), aes(x=temp_range, y=index_1)) +
  geom_point() +
  labs(
    x="Temperature Range",
    y="Zombie Index",
    title = "Zombie Index vs. Temperature Range",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()

ggplot(indicies %>% filter(index_1 < 50), aes(x=drought, y=index_1)) +
  geom_point() +
  labs(
    x="Drought Severity Index",
    y="Zombie Index",
    title = "Zombie Index vs. Drought Severity Index",
    subtitle = "Zombie Index < 50"
  ) +
  theme_minimal()

  