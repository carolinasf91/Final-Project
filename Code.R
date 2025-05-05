#<----------------------Packages-----------------------------
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ROCR")
install.packages("dplyr")
install.packages("stringr")
install.packages("broom")
library(dplyr)
library(ggplot2)
library(tidyr)
library(ROCR)
library(stringr)
library(broom)

#<----------------------Repeated functions-----------------------------
#Standardise NCMP year from 2XXX/XX to 2XXX
clean_year <- function(df, year_col = "NCMP.Year") {
  df %>%
    rename(Year = all_of(year_col)) %>%
    mutate(
      Year = as.numeric(str_extract(Year, "^[0-9]{4}"))
    )
}

#Replace NA with 0 
replace_na_with_zero <- function(df, columns) {
  df %>%
    mutate(
      across(all_of(columns), ~ ifelse(is.na(.), 0, .))
    )
}

#Calculate obesity progression risk
get_progression_risk <- function(df, prevalence_col = "Obesity.Prevalence") {
  df %>%
    pivot_wider(names_from = School.Year, values_from = !!sym(prevalence_col)) %>%
    filter(!is.na(Reception) & !is.na(`Year 6`)) %>%
    mutate(
      Progression_Risk = `Year 6` - Reception
    )
}

#My plot theme
my_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(face = "bold"))
}

#Calculate prevalence
calculate_prevalence <- function(x, total) {
  return(100 * x / total)
}

#<----------------------Load files & clean-----------------------------
#<----------------------Ethnicity-----------------------------
ethnicity_data <- read.csv("C:/Users/CAROL/My Drive/Uni/Birkbeck/Year 4/Project/My Project/Data/Processed/ncmp-bmi-by-ethnicity-of-child.csv")
summary(ethnicity_data)

#Replace NAs with 0
ethnicity_data <- ethnicity_data %>%
  replace_na_with_zero(., c('Severe.obesity'))

#Clean the year format
ethnicity_data <- clean_year(ethnicity_data)    

#Clean ethnicity categories 
unique(ethnicity_data$Ethnic.category)    
ethnicity_data <- ethnicity_data %>%
  mutate(
    Ethnic.category = case_when(
      Ethnic.category %in% c("White", "WHITE") ~ "White",
      Ethnic.category %in% c("Mixed", "MIXED") ~ "Mixed",
      Ethnic.category %in% c("Asian or Asian British", "Asian", "ASIAN") ~ "Asian or Asian British",
      Ethnic.category %in% c("Black or Black British", "Black", "BLACK") ~ "Black or Black British",
      Ethnic.category %in% c("Chinese", "CHINESE", "Chinese                                           ") ~ "Chinese",
      Ethnic.category %in% c("Any Other Ethnic Group", "ANY OTHER ETHNC GROUP", 
                             "ANY OTHER ETHNIC GROUP", "Any other ethnic group", 
                             "Any Other Ethnic Group                            ") ~ "Any Other Ethnic Group",
      Ethnic.category %in% c("Not stated", "NOT STATED", "Unknown") ~ "Not Stated",
      TRUE ~ Ethnic.category  
    )
  )

#<----------------------Local authority-----------------------------
local_authority_data <- read.csv("C:/Users/CAROL/My Drive/Uni/Birkbeck/Year 4/Project/My Project/Data/Processed/ncmp-bmi-by-local-authority.csv")
summary(local_authority_data)

#Convert columns to numeric
local_authority_data <- local_authority_data %>%    
  mutate(
    across(c(Underweight, Healthy.weight, Overweight, Obese, 
             Severely.obese, Overweight.and.obese.combined, 
             Number.of.children.measured, Participation.Rate), as.numeric)
  )

#Replace NA with 0
local_authority_data <- replace_na_with_zero(local_authority_data, c(
  "Underweight", "Healthy.weight", "Overweight", "Obese",
  "Severely.obese", "Overweight.and.obese.combined",
  "Number.of.children.measured", "Participation.Rate"
))

# Clean the NCMP year format
local_authority_data <- clean_year(local_authority_data)

#<----------------------Yearly BMI-----------------------------
yearly_bmi_data <- read.csv("C:/Users/CAROL/My Drive/Uni/Birkbeck/Year 4/Project/My Project/Data/Processed/Number-of-children-by-BMI-category-and-NCMP-collection year.csv")
summary(yearly_bmi_data)

#Clean the NCMP year format
yearly_bmi_data <- yearly_bmi_data %>%
  mutate(
    Year = as.numeric(substr(Year, 1, 4))
  )

#<----------------------Gender-----------------------------
gender_bmi_data <- read.csv("C:/Users/CAROL/My Drive/Uni/Birkbeck/Year 4/Project/My Project/Data/Processed/ncmp-bmi-by-gender.csv")
summary(gender_bmi_data)

#Replace NA with 0 
gender_bmi_data <- replace_na_with_zero (gender_bmi_data, c("Underweight", "Healthy.Weight", "Severe.Obesity"))

# Clean the NCMP year format
gender_bmi_data <- gender_bmi_data %>%
  mutate(
    Year = as.numeric(str_extract(Year, "^[0-9]{4}"))
  )
head(gender_bmi_data)


#------------------------------------------------------------------------------------------------------#
#<----------------------Yearly trend-----------------------------
#Visulisation of the yearly trend of each BMI category
yearly_bmi_summary <- yearly_bmi_data %>%
  group_by(Year) %>%
  summarise(
    Underweight = sum(Underweight, na.rm = TRUE),
    Healthy_Weight = sum(Healthy.weight, na.rm = TRUE),
    Overweight = sum(Overweight, na.rm = TRUE),
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Total = sum(Number.of.children.measured, na.rm = TRUE)
  ) %>%
  #Calculate prevalence for each category 
  mutate(
    Underweight_Pct = calculate_prevalence(Underweight, Total),
    Healthy_Weight_Pct = calculate_prevalence(Healthy_Weight, Total),
    Overweight_Pct = calculate_prevalence(Overweight, Total),
    Obese_Pct = calculate_prevalence(Obese, Total)
  ) %>%
  select(Year, ends_with("Pct")) %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Percentage")

#Plot of yearly trend
ggplot(yearly_bmi_summary, aes(x = Year, y = Percentage, color = Category)) +
  geom_line(size = 1.2) +
  labs(title = "Trends in BMI Categories Over Time",
       x = "Year", y = "Percentage of Children",
       color = "BMI Category") +
  my_theme()


#------------------------------------------------------------------------------------------------------#
#<----------------------Gender analysis----------------------------- 
#Calculating the obesity prevalence between boys and girls
gender_bmi_data <- gender_bmi_data %>% 
  mutate(
    Obesity.Prevalence = (Obese..including.severe.obesity. / Number.Measured)*100
  )
#bar graph plot option 1 - TO DECIDE
ggplot(gender_bmi_data, aes(x = as.factor(Year), y = Obesity.Prevalence, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Obesity Prevalence Over Time",
       x = "Year", y = "Obesity Prevalence (%)") +
  scale_fill_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84", "Both" = "#A3B18A")) +
  my_theme()
#bar graph plot option 2 - TO DECIDE
ggplot(gender_bmi_data, aes(x = as.factor(Year), y = Obesity.Prevalence, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Obesity Prevalence Over Time",
       x = "Year", y = "Obesity Prevalence (%)") +
  scale_fill_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84", "Both" = "#A3B18A")) +
  facet_wrap(~Gender) + 
  my_theme() 


#T-test: Is there a significant difference in obesity prevalence rates between the genders? 
gender_bmi_rates <- gender_bmi_data %>%
  filter(Gender %in% c("Boys", "Girls")) %>%
  select(Year, School.Year, Gender, Obesity.Prevalence) %>%
  pivot_wider(names_from = Gender, values_from = Obesity.Prevalence)  
#Need to filter for Reception
reception_data <- gender_bmi_rates %>% filter(School.Year == "Reception")
t_test_reception <- t.test(reception_data$Boys, reception_data$Girls, paired = TRUE)
#Need to filter for Year 6
year6_data <- gender_bmi_rates %>% filter(School.Year == "Year 6")
t_test_year6 <- t.test(year6_data$Boys, year6_data$Girls, paired = TRUE)
#Results
t_test_reception
t_test_year6


#Probability of a child being classed as obese in Reception and Year 6? 
#Need to filter data for Reception and Year 6
gender_reception_data <- gender_bmi_data %>% filter(School.Year == "Reception")
gender_year6_data <- gender_bmi_data %>% filter(School.Year == "Year 6")
#Merge Reception and Year 6 data by Year and Gender
obesity_transition <- gender_reception_data %>%
  select(Year, Gender, Obesity.Prevalence) %>%
  rename(Reception_Obesity = Obesity.Prevalence) %>%
  left_join(gender_year6_data %>% 
              select(Year, Gender, Obesity.Prevalence) %>%
              rename(Year6_Obesity = Obesity.Prevalence), 
            by = c("Year", "Gender"))
#Calculating the probability
obesity_transition <- obesity_transition %>%
  mutate(
    Probability_Obese_Y6_given_R = Year6_Obesity / Reception_Obesity
  )
#Print result
print(obesity_transition)
#Plot showing probability
ggplot(obesity_transition, aes(x = Year, y = Probability_Obese_Y6_given_R, color = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  my_theme() +
  labs(title = "Likelihood of Staying Obese from Reception to Year 6",
       x = "Year", y = "Probability (Obese in Year 6 | Obese in Reception)") +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84", "Both" = "#A3B18A"))
#Average probability split by gender
obesity_transition %>%
  group_by(Gender) %>%
  summarise(Average_Probability = mean(Probability_Obese_Y6_given_R, na.rm = TRUE))
#Plot with 2020 smoothed due to COVID-19 impact
ggplot(obesity_transition %>% filter(Year != 2020), 
       aes(x = Year, y = Probability_Obese_Y6_given_R, color = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", linetype = "dashed", size = 1) +
  labs(
    title = "Likelihood of Staying Obese from Reception to Year 6",
    x = "Year", y = "Probability (Obese in Year 6 | Obese in Reception)") +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84", "Both" = "#A3B18A")) +
  my_theme()


#Probability that a child is classed Overweight in Reception and Obese in Year 6
#Calculate children in Reception that are Overweight
gender_reception_data <- gender_reception_data %>%
  mutate(
    Reception_Overweight = (Overweight/Number.Measured)*100
  )
#Join to the table
obesity_transition <- obesity_transition %>%
  left_join(gender_reception_data %>% select(Year, Gender, Reception_Overweight),
            by = c("Year", "Gender"))
#Calculate children in Year 6 that are Overweight
gender_year6_data <- gender_year6_data %>%
  mutate(
    Year6_Overweight = (Overweight/Number.Measured)*100
  )
#Join to the table
obesity_transition <- obesity_transition %>%
  left_join(gender_year6_data %>% select(Year, Gender, Year6_Overweight),
            by = c("Year", "Gender"))
#Calculating the probability
obesity_transition <- obesity_transition %>%
  mutate(
    Prob_Overweight_Y6_given_R = Year6_Overweight/ Reception_Overweight,
    Prob_Overweight_or_Obese_Y6_given_R = 
      (Year6_Overweight + Year6_Obesity) / (Reception_Overweight + Reception_Obesity)
  )
#Plot showing probability with 2020 smoothed due to COVID-19 impact
ggplot(obesity_transition %>% filter(Year != 2020), 
       aes(x = Year, y = Prob_Overweight_or_Obese_Y6_given_R, color = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Probability of Remaining or Becoming Overweight/Obese from Reception to Year 6",
    x = "Year", y = "Probability"
  ) +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84","Both" = "#A3B18A")) +
  my_theme()
#Plot showing probability without 2020 smoothed
ggplot(obesity_transition %>% filter(Year != 2020), 
       aes(x = Year, y = Prob_Overweight_or_Obese_Y6_given_R, color = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", linetype = "dashed", size = 1) +
  labs(
    title = "Probability of Remaining or Becoming Overweight/Obese from Reception to Year 6",
    x = "Year", y = "Probability"
  ) +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84","Both" = "#A3B18A")) +
  my_theme()


#Probability of a Healthy Weight child in Reception being classed as Overweight or Obese in Year 6
#Calculate number of children in Reception that are Healthy Weight 
gender_reception_data <- gender_reception_data %>%
  mutate(
    Reception_HealthyWeight = (Healthy.Weight/Number.Measured)*100
  )
#Join to table
obesity_transition <- obesity_transition %>%
  left_join(gender_reception_data %>% select(Year, Gender, Reception_HealthyWeight),
            by = c("Year", "Gender"))
#Calculate number of children in Year 6 that are Healthy Weight 
gender_year6_data <- gender_year6_data %>%
  mutate(
    Year6_HealthyWeight = (Healthy.Weight/Number.Measured)*100
  )
#Join to table
obesity_transition <- obesity_transition %>%
  left_join(gender_year6_data %>% select(Year, Gender, Year6_HealthyWeight),
            by = c("Year", "Gender"))
#Calculating the probability 
obesity_transition <- obesity_transition %>%
  mutate(
    Prob_Y6_OWorObese_given_R_Healthy = 
      (Year6_Overweight + Year6_Obesity) / Reception_HealthyWeight
  )
#Plot of probability 
ggplot(obesity_transition %>% filter(Year != 2020),
       aes(x = Year, y = Prob_Y6_OWorObese_given_R_Healthy, color = Gender)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Probability of Becoming Overweight or Obese by Year 6 (Given Healthy Weight in Reception)",
    x = "Year", y = "Probability"
  ) +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84","Both" = "#A3B18A")) +
  my_theme()


#------------------------------------------------------------------------------------------------------#
#<----------------------Ethnicity----------------------------- 
#Calculate the prevalence by ethnicity, excluding "Total" category.
ethnicity_obesity <- ethnicity_data %>%
  filter(Ethnic.category != "Total") %>%  
  mutate(
    Obesity.Prevalence = (Obesity..including.severe.obesity. / Number.of.children.measured) * 100
  ) %>%
  select(Year, School.Year, Ethnic.category, Obesity.Prevalence)
#Calculate the average by ethnic category 
ethnicity_summary <- ethnicity_obesity %>%
  group_by(Ethnic.category) %>%
  summarise(Average_Obesity = mean(Obesity.Prevalence, na.rm = TRUE)) %>%
  arrange(desc(Average_Obesity))
#Print result
print(ethnicity_summary)


#Calculate the yearly trend, ignoring NAs
ethnicity_trend <- ethnicity_data %>%
  filter(Ethnic.category != "Total") %>%
  group_by(Year, Ethnic.category) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE), 
    Total = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Total) * 100
  )
#Plot of the yearly trend 
ggplot(ethnicity_trend, aes(x = Year, y = Obesity.Prevalence, color = Ethnic.category, group = Ethnic.category)) +
  geom_line(size = 1.2) +
  labs(
    title = "Obesity Prevalence Trends by Ethnic Group Over Time",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "Ethnic Group"
  ) +
  my_theme()+
  theme(
    legend.position = "right"
  )
#Yearly trend plot with 2020 smoothed due to COVID-19 impact 
ggplot(ethnicity_trend, aes(x = Year, y = Obesity.Prevalence, color = Ethnic.category, group = Ethnic.category)) +
  geom_line(size = 1.2, alpha = 0.6) +  
  geom_smooth(se = FALSE, method = "loess", span = 0.5) +  
  labs(
    title = "Smoothed Obesity Prevalence Trends by Ethnic Group",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "Ethnic Group"
  ) +
  my_theme()


#Comparison of obesity prevalence between Reception and Year 6
#Group prevalence by ethnicity and school year
ethnicity_obesity_prevalence <- ethnicity_data %>%
  filter(Ethnic.category != "Total") %>%
  group_by(School.Year, Ethnic.category) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Measured) * 100
  )
#Plot of obesity prevalence, with Reception and Year 6 side by side
ggplot(ethnicity_obesity_prevalence, aes(x = Ethnic.category, y = Obesity.Prevalence, fill = School.Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Obesity Prevalence by Ethnic Group: Reception vs Year 6",
    x = "Ethnic Group",
    y = "Obesity Prevalence (%)",
    fill = "School Year"
  ) +
  my_theme()


#Do children from certain ethnic backgrounds have a higher risk of obesity progression?
#Obesity prevalence by ethnic group and school year 
ethnicity_obesity_prevalence_progression <- ethnicity_data %>%
  filter(Ethnic.category != "Total") %>%
  group_by(School.Year, Ethnic.category) %>%
  summarise(
    Obesity.Prevalence = sum(Obesity..including.severe.obesity., na.rm = TRUE) / 
      sum(Number.of.children.measured, na.rm = TRUE) * 100,
    .groups = "drop"
  )
#Pivot needed to compare Reception to Year 6 and calculate progression risk. Sorted by highest to lowest risk
ethnicity_progression <- ethnicity_obesity_prevalence_progression %>%
  pivot_wider(
    names_from = School.Year,
    values_from = Obesity.Prevalence
  ) %>%
  mutate(
    Progression_Risk = `Year 6` - Reception) %>%
  arrange(desc(Progression_Risk))
#Print result
print(ethnicity_progression)
#Obesity prevalence drilled down to ethnic sub-category 
ethnicity_subcategory_prevalence <- ethnicity_data %>%
  filter(Ethnic.subcategory != "" & !is.na(Ethnic.subcategory)) %>%  
  group_by(School.Year, Ethnic.subcategory) %>%
  summarise(
    Obesity.Prevalence = sum(Obesity..including.severe.obesity., na.rm = TRUE) /
      sum(Number.of.children.measured, na.rm = TRUE) * 100,
    .groups = "drop"
  )
#Pivot needed to compare Reception to Year 6 and calculate progression risk using the function get_progression_risk(). Sorted by highest to lowest risk
subcategory_progression <- ethnicity_subcategory_prevalence %>%
  get_progression_risk() %>%
  arrange(desc(Progression_Risk))
#Print result
print(subcategory_progression)


#Is there any variation in the obesity progression risk within ethnic groups? 
#Sub-categories are linked to their parent ethnic group
subcategory_with_parents <- ethnicity_data %>%
  select(Ethnic.category, Ethnic.subcategory) %>%
  distinct() %>%
  filter(Ethnic.subcategory != "" & !is.na(Ethnic.subcategory))
#New table joining the parent ethnic names to the subcategory_progression 
subcategory_progression_labeled <- subcategory_progression %>%
  left_join(subcategory_with_parents, by = "Ethnic.subcategory") %>%
  select(Ethnic.category, Ethnic.subcategory, Reception, `Year 6`, Progression_Risk)
#Calculate the summary statistics (mean, maximum, minimum and range) within each parent ethnic group. Sorted by descending range. 
group_disparities <- subcategory_progression_labeled %>%
  group_by(Ethnic.category) %>%
  summarise(
    Mean_Progression = mean(Progression_Risk),
    Max_Progression = max(Progression_Risk),
    Min_Progression = min(Progression_Risk),
    Range = Max_Progression - Min_Progression,
    .groups = "drop"
  ) %>%
  arrange(desc(Range))
#Print result
print(group_disparities)
#Box plot showing the summary statistics 
ggplot(subcategory_progression_labeled, aes(x = Ethnic.category, y = Progression_Risk)) +
  geom_boxplot(fill = "#A3B18A", color = "black") +
  labs(
    title = "Variation in Obesity Progression Risk Within Ethnic Groups",
    x = "Ethnic Group",
    y = "Progression Risk (%)"
  ) +
  my_theme()

#Plot for obesity progression by ethnic subcategory. Facet by subcategory - option 1
dot_data <- subcategory_progression %>%
  pivot_longer(cols = c(Reception, `Year 6`),
               names_to = "School_Year",
               values_to = "Obesity_Prevalence") %>%
  mutate(
    School_Year = factor(School_Year, levels = c("Reception", "Year 6"))
  )
ggplot(dot_data, aes(x = School_Year, y = Obesity_Prevalence, group = Ethnic.subcategory)) +
  geom_line(aes(color = Progression_Risk), size = 1) +
  geom_point(aes(color = Progression_Risk), size = 3) +
  scale_color_gradient(low = "#A3B18A", high = "#C97B84") + 
  facet_wrap(~ Ethnic.subcategory, ncol = 3, scales = "free_y") +
  labs(
    title = "Obesity Progression by Ethnic Subcategory",
    x = "School Year",
    y = "Obesity Prevalence (%)",
    color = "Progression Risk (%)"
  ) +
  my_theme()

#Plot for obesity progression by ethnic subcategory - option 2
heatmap_data <- subcategory_progression %>%
  pivot_longer(cols = c(Reception, `Year 6`, Progression_Risk),
               names_to = "Metric",
               values_to = "Value")
subcategory_order <- subcategory_progression %>%
  arrange(desc(Progression_Risk)) %>%
  pull(Ethnic.subcategory)
heatmap_data$Ethnic.subcategory <- factor(heatmap_data$Ethnic.subcategory, levels = subcategory_order)
ggplot(heatmap_data, aes(x = Metric, y = Ethnic.subcategory, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#5B8FA8", high = "#C97B84") +  
  labs(
    title = "Obesity Prevalence and Progression Risk by Ethnic Subcategory",
    x = "Metric",
    y = "Ethnic Subcategory",
    fill = "Value (%)"
  ) +
  my_theme()


#Trend analysis of obesity prevalence over time, by ethnic subcategory and school year
#Yearly prevalence calculation by subcategory, grouped by year
subcategory_trends <- ethnicity_data %>%
  filter(!is.na(Ethnic.subcategory) & Ethnic.subcategory != "") %>%
  group_by(Year, School.Year, Ethnic.subcategory) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Measured) * 100
  )
#Plot of trend lines. Facet by subcategory   
ggplot(subcategory_trends, aes(x = Year, y = Obesity.Prevalence, color = School.Year, group = School.Year)) +
  geom_line(size = 1.1) +
  facet_wrap(~ Ethnic.subcategory, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Obesity Trends Over Time by Ethnic Subcategory",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) + 
  my_theme()
#Plot with 2020 smoothed due to COVID-19 impact 
ggplot(subcategory_trends, aes(x = Year, y = Obesity.Prevalence, color = School.Year, group = School.Year)) +
  geom_line(size = 1.1, alpha = 0.6) +  
  geom_smooth(se = FALSE, method = "loess", span = 0.5) +  
  facet_wrap(~ Ethnic.subcategory, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Smoothed Obesity Trends Over Time by Ethnic Subcategory",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()


#------------------------------------------------------------------------------------------------------#
#<----------------------Ethnicity & Gender----------------------------- 
#Obesity progression risk by ethnic group, split by gender
#Calculates average obesity by ethnic group and school year
ethnicity_avg <- ethnicity_data %>%
  filter(Ethnic.category != "Total") %>%
  group_by(School.Year, Ethnic.category) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Ethnicity.Obesity = (Obese / Measured) * 100
  )
#Calculates average obesity by gender 
gender_avg <- gender_bmi_data %>%
  filter(Gender %in% c("Boys", "Girls")) %>%
  group_by(School.Year, Gender) %>%
  summarise(
    Obese = sum(Obese..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.Measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Gender.Obesity = (Obese / Measured) * 100
  )
#Join the average calculations for ethnicity and gender, so that an estimated obesity can be calculated
ethnic_gender_estimate <- expand.grid(
  School.Year = unique(ethnicity_avg$School.Year),
  Ethnic.category = unique(ethnicity_avg$Ethnic.category),
  Gender = c("Boys", "Girls")
) %>%
  left_join(ethnicity_avg, by = c("School.Year", "Ethnic.category")) %>%
  left_join(gender_avg, by = c("School.Year", "Gender")) %>%
  mutate(
    Estimated.Obesity = (Ethnicity.Obesity * Gender.Obesity) / mean(Gender.Obesity)
  )
#Pivot the table so that each row contains both Reception and Year 6 data 
ethnic_gender_progression <- ethnic_gender_estimate %>%
  select(Ethnic.category, Gender, School.Year, Estimated.Obesity) %>%
  pivot_wider(names_from = School.Year, values_from = Estimated.Obesity) %>%
  filter(!is.na(Reception) & !is.na(`Year 6`)) %>%
  mutate(
    Progression_Risk = `Year 6` - Reception
  ) %>%
  arrange(desc(Progression_Risk))
#Print result
print(ethnic_gender_progression)

#Plot of obesity progression risk by ethnicity and gender 
#Grouped bar chat - option 1
ggplot(ethnic_gender_progression, aes(x = Ethnic.category, y = Progression_Risk, fill = Gender)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84")) +
  labs(
    title = "Obesity Progression Risk by Ethnicity and Gender",
    x = "Ethnic Group",
    y = "Progression Risk (%)",
    fill = "Gender"
  ) +
  my_theme()

#Slope plot - option 2 
#Pivot table for slope plot. Facet by category  
slope_data <- ethnic_gender_progression %>%
  pivot_longer(cols = c(Reception, `Year 6`), names_to = "School_Year", values_to = "Obesity") %>%
  mutate(
    School_Year = factor(School_Year, levels = c("Reception", "Year 6"))
  )
ggplot(slope_data, aes(x = School_Year, y = Obesity, group = interaction(Ethnic.category, Gender), color = Gender)) +
  geom_line(size = 1.2, alpha = 0.6) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Boys" = "#5B8FA8", "Girls" = "#C97B84")) +
  facet_wrap(~ Ethnic.category, scales = "free_y") +
  labs(
    title = "Obesity Progression from Reception to Year 6",
    x = "School Year",
    y = "Estimated Obesity Prevalence (%)",
    color = "Gender"
  ) +
  my_theme()

#Heatmap plot - option 3
#Pivot table for heatmap
heatmap_data <- ethnic_gender_progression %>%
  pivot_longer(cols = c(Reception, `Year 6`, Progression_Risk),
               names_to = "Metric", values_to = "Value")
#Reordering of categories for a better layout
heatmap_data$Ethnic.category <- factor(heatmap_data$Ethnic.category,
                                       levels = unique(ethnic_gender_progression$Ethnic.category[order(-ethnic_gender_progression$Progression_Risk)]))
ggplot(heatmap_data, aes(x = Metric, y = interaction(Ethnic.category, Gender), fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#5B8FA8", high = "#C97B84") +
  labs(
    title = "Obesity Prevalence and Progression by Ethnicity and Gender",
    x = "Metric",
    y = "Ethnic Group x Gender",
    fill = "Value (%)"
  ) +
  my_theme()


#------------------------------------------------------------------------------------------------------#
#<----------------------Geographic location - Regional----------------------------- 
#Trend analysis of obesity rates by region 
#Annual obesity prevalence by region, using the defined function to calculate obesity prevalence 
geo_summary_region_only <- local_authority_data %>%
  filter(LA.Name == "-") %>%
  group_by(Year, Region = Region...Local.Authority, School.Year) %>%
  summarise(
    Obese = sum(Obese, na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = calculate_prevalence(Obese, Measured)
  )

#Average obesity prevalence by region 
region_averages <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  summarise(
    Avg_Obesity = mean(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Plot showing the average obesity prevalence by region
ggplot(region_averages, aes(x = reorder(Region, Avg_Obesity), y = Avg_Obesity, fill = School.Year)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Average Obesity Prevalence by Region",
    x = "Region",
    y = "Obesity Prevalence (%)",
    fill = "School Year"
  ) +
  coord_flip() +
  my_theme()



#Linear model showing the estimated trend for each region by school year. Slope represents the average annual change in obesity prevalence
regional_trends <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  do(tidy(lm(Obesity.Prevalence ~ as.numeric(Year), data = .))) %>%
  filter(term == "as.numeric(Year)") %>%
  rename(Slope = estimate) %>%
  ungroup()
print(regional_trends)
#Plot showing the change in obesity prevalence, ordered by slope of change 
ggplot(regional_trends, aes(x = reorder(Region, Slope), y = Slope, fill = School.Year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  coord_flip() +
  labs(
    title = "Rate of Change in Obesity Prevalence by Region",
    x = "Region",
    y = "Year-on-Year Change in Obesity Prevalence (%)",
    fill = "School Year"
  ) +
  my_theme()


#In which regions does obesity increase the most between Reception and Year 6? 
#Calculating the average obesity prevalence by region and school year using the function get_progression_risk()
regional_avg <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  summarise(
    Obesity.Prevalence = mean(Obese / Measured * 100, na.rm = TRUE),
    .groups = "drop"
  )
regional_progression <- regional_avg %>%
  get_progression_risk() %>%
  arrange(desc(Progression_Risk))
#Display first 5 rows of regional progression 
head(regional_progression, 5)
#Plot of progression risk by region 
ggplot(regional_progression, aes(x = reorder(Region, Progression_Risk), y = Progression_Risk)) +
  geom_col(fill = "#A3B18A") +
  coord_flip() +
  labs(
    title = "Average Obesity Progression Risk by Region",
    subtitle = "Reception to Year 6 (All Years)",
    x = "Region",
    y = "Progression Risk (%)"
  ) +
  my_theme()


#How has obesity changed over time by region?
#Plot time series of obesity prevalence. Facet by region  
ggplot(geo_summary_region_only, aes(x = Year, y = Obesity.Prevalence, color = School.Year)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Obesity Prevalence Over Time by Region",
    subtitle = "Reception and Year 6 Children",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()
#Calculation of the national average obesity prevalence, for comparision with regional average obesity prevalence 
national_avg <- geo_summary_region_only %>%
  group_by(Year, School.Year) %>%
  summarise(
    National_Avg = mean(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Plot of regional trend overlayed with national averages. Facet by region  
ggplot() +
  #Regional lines are solid
  geom_line(data = geo_summary_region_only, 
            aes(x = Year, y = Obesity.Prevalence, color = School.Year), size = 1.2) +
  #National lines are dashed 
  geom_line(data = national_avg, 
            aes(x = Year, y = National_Avg, color = School.Year), 
            linetype = "dashed", size = 1) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Regional Obesity Trends with National Average Overlay",
    subtitle = "Reception and Year 6 Children",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()
#Plot with smoothed curve 
ggplot(geo_summary_region_only, aes(x = Year, y = Obesity.Prevalence, color = School.Year)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, size = 1.2) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Smoothed Obesity Trends Over Time by Region",
    subtitle = "Reception and Year 6 (LOESS Smoothing)",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()


#Peak year/years for obesity prevalence for each region
peak_years <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  filter(Obesity.Prevalence == max(Obesity.Prevalence, na.rm = TRUE)) %>%
  ungroup()
#Plot showing the peak years per region, with 2020 smoothed due to COVID-19 impact
ggplot(geo_summary_region_only, aes(x = Year, y = Obesity.Prevalence, color = School.Year)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, size = 1.2) +
  geom_point(data = peak_years, aes(x = Year, y = Obesity.Prevalence), size = 2.5, shape = 21, fill = "black") +
  geom_text(
    data = peak_years,
    aes(x = Year, y = Obesity.Prevalence, label = Year),
    color = "black",
    size = 3,
    vjust = -1
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Smoothed Obesity Trends with Peak Year",
    subtitle = "Reception and Year 6",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()

#Lowest year/years for obesity prevalence for each region
low_years <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  filter(Obesity.Prevalence == min(Obesity.Prevalence, na.rm = TRUE)) %>%
  mutate(
    Type = "Drop"
  ) %>%
  ungroup()
#There are regions with more than one peak year, of the same value. Therefore for peak years, only want to show the first peak year 
peak_years <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  filter(Obesity.Prevalence == max(Obesity.Prevalence, na.rm = TRUE)) %>%
  slice_head(n = 1) %>%
  mutate(
    Type = "Peak"
  ) %>%
  ungroup()
#There are regions with more than one low year, of the same value. Therefore for low years, only want to show the first low year
low_years <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  filter(Obesity.Prevalence == min(Obesity.Prevalence, na.rm = TRUE)) %>%
  slice_head(n = 1) %>%
  mutate(
    Type = "Drop"
  ) %>%
  ungroup()
#Combining the peak and low years
extremes <- bind_rows(peak_years, low_years)
#Plot showing the smoothed peak and low years due to COVID-19 impace. One facet per region
ggplot(geo_summary_region_only, aes(x = Year, y = Obesity.Prevalence, color = School.Year)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6, size = 1.2) +
  geom_point(data = extremes, aes(x = Year, y = Obesity.Prevalence), shape = 21, size = 2.5, fill = "black", color = "black") +
  geom_text(
    data = extremes,
    aes(x = Year, y = Obesity.Prevalence, label = paste0(Type, "\n", Year)),
    color = "black", size = 2.8, vjust = -0.6, fontface = "bold"
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Smoothed Obesity Trends by Region with Peak and Low Years",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()

#Plot showing the peak and low years (not smoothed). One facet per region
ggplot(geo_summary_region_only, aes(x = Year, y = Obesity.Prevalence, color = School.Year, group = School.Year)) +
  geom_line(size = 1.2) +  # Raw line
  geom_point(size = 1.5) + # Points for every year
  geom_point(
    data = extremes,
    aes(x = Year, y = Obesity.Prevalence),
    inherit.aes = FALSE,
    shape = 21, size = 2.5, fill = "black", color = "black"
  ) +
  geom_text(
    data = extremes,
    aes(x = Year, y = Obesity.Prevalence, label = paste0(Type, "\n", Year)),
    inherit.aes = FALSE,
    color = "black", size = 2.8, vjust = -0.6, fontface = "bold"
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Obesity Trends by Region with Peak and Low Years",
    x = "Year",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()


#Which regions are experiencing the fastest increase in obesity rates?
#Calculate slope coefficient, standard error, t-statistic and p-value
regional_trends <- geo_summary_region_only %>%
  group_by(Region, School.Year) %>%
  summarise(
    Slope = coef(lm(Obesity.Prevalence ~ (Year)))[2],
    Std.Error = summary(lm(Obesity.Prevalence ~ (Year)))$coefficients[2,2],
    T = summary(lm(Obesity.Prevalence ~ (Year)))$coefficients[2,3],
    P = summary(lm(Obesity.Prevalence ~ (Year)))$coefficients[2,4],
    .groups = "drop"
  )
#Sort regional trends table into descending Slope order
regional_trends %>%
  arrange(desc(Slope))
#Print result
print(regional_trends)


#Is the obesity gap between Reception and Year 6 widening in any regions?
#Calculation of the yearly progressoion gap for each region
regional_gaps <- geo_summary_region_only %>%
  select(Region, Year, School.Year, Obesity.Prevalence) %>%
  pivot_wider(names_from = School.Year, values_from = Obesity.Prevalence) %>%
  rename(Reception = Reception, Year6 = `Year 6`) %>%
  mutate(
    Progression_Gap = Year6 - Reception
  ) %>%
  filter(!is.na(Reception) & !is.na(Year6))
#Plot showing the gap over time 
ggplot(regional_gaps, aes(x = Year, y = Progression_Gap)) +
  geom_line(aes(color = Region), size = 1.2) +
  labs(
    title = "Change in Obesity Progression Gap Over Time",
    subtitle = "Gap = Year 6 % - Reception %",
    x = "Year",
    y = "Obesity Progression Gap (%)",
    color = "Region"
  ) +
  my_theme() +
  theme(legend.position = "bottom")
#Calculation for the slope of progression_gap for each region 
gap_trends <- regional_gaps %>%
  group_by(Region) %>%
  summarise(
    Slope = coef(lm(Progression_Gap ~ (Year)))[2],
    .groups = "drop"
  ) %>%
  arrange(desc(Slope))
#View top three regions 
top_regions <- gap_trends %>% slice_max(Slope, n = 3) %>% pull(Region)
#Plot showing the trend
ggplot(regional_gaps %>% filter(Region %in% top_regions),
       aes(x = Year, y = Progression_Gap, color = Region)) +
  geom_line(size = 1.2) +
  geom_smooth(se = FALSE, method = "lm", linetype = "dashed") +
  labs(
    title = "Regions with Fastest Growing Obesity Progression Gap",
    subtitle = "Gap = Year 6 % - Reception %",
    x = "Year",
    y = "Progression Gap (%)",
    color = "Region"
  ) +
  my_theme()


#Analysis of the regional variation in obesity prevalence over time 
#Calculation of standard deviation and range of regional obesity prevalence 
regional_variation <- geo_summary_region_only %>%
  group_by(Year, School.Year) %>%
  summarise(
    SD = sd(Obesity.Prevalence, na.rm = TRUE),
    Range = max(Obesity.Prevalence, na.rm = TRUE) - min(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Plot showing the standard deviation of regional obesity prevalence over time
ggplot(regional_variation, aes(x = Year, y = SD, color = School.Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Standard Deviation of Regional Obesity Prevalence Over Time",
    x = "Year",
    y = "Standard Deviation (%)",
    color = "School Year"
  ) +
  my_theme()
#Plow showing the range of obesity prevalence across regions over time 
ggplot(regional_variation, aes(x = Year, y = Range, color = School.Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Range of Obesity Prevalence Across Regions Over Time",
    x = "Year",
    y = "Prevalence Range (%)",
    color = "School Year"
  ) +
  my_theme()
#Summary statistics for reginal obesity prevalence 
regional_summary_stats <- geo_summary_region_only %>%
  group_by(Year, School.Year) %>%
  summarise(
    Max = max(Obesity.Prevalence, na.rm = TRUE),
    Min = min(Obesity.Prevalence, na.rm = TRUE),
    Range = Max - Min,
    SD = sd(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Print result
print(regional_summary_stats)


#Regional variation in obesity prevalence pre and post-pandemic comparison
#Label each year in regional_summary_stats as pre or post-COVID
regional_summary_stats <- regional_summary_stats %>%
  mutate(
    Period = ifelse(Year <= "2018/19", "Pre-COVID", "Post-COVID")
  )
#Calculate the average standard deviation and range per year and School Year
variation_by_period <- regional_summary_stats %>%
  group_by(School.Year, Period) %>%
  summarise(
    Mean_SD = mean(SD, na.rm = TRUE),
    Mean_Range = mean(Range, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(School.Year, desc(Period))
#Print result
print(variation_by_period)
#Extract standard deviation for Reception pre and post-COVID
reception_sd_pre <- regional_summary_stats %>%
  filter(School.Year == "Reception", Period == "Pre-COVID") %>%
  pull(SD)
reception_sd_post <- regional_summary_stats %>%
  filter(School.Year == "Reception", Period == "Post-COVID") %>%
  pull(SD)
#Extract standard deviation for Year 6 pre and post-COVID
year6_sd_pre <- regional_summary_stats %>%
  filter(School.Year == "Year 6", Period == "Pre-COVID") %>%
  pull(SD)
year6_sd_post <- regional_summary_stats %>%
  filter(School.Year == "Year 6", Period == "Post-COVID") %>%
  pull(SD)
#T-Test comparing pre and post-COVID standard deviation 
t_test_reception <- t.test(reception_sd_pre, reception_sd_post, var.equal = FALSE)
t_test_year6 <- t.test(year6_sd_pre, year6_sd_post, var.equal = FALSE)
#Print results
t_test_reception
t_test_year6


#<----------------------Geographic location - Local Authority----------------------------- 
#LA obesity progression risk
la_summary <- local_authority_data %>%
  group_by(LA.Name, School.Year) %>%
  summarise(
    Obese = sum(Obese, na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Measured) * 100
  )
#Calculation for average prevalence over time for each local authority
la_summary_avg <- la_summary %>%
  group_by(LA.Name, School.Year) %>%
  summarise(Average_Obesity = mean(Obesity.Prevalence, na.rm = TRUE),
            .groups = "drop")
#Calculation for the obesity progression risk for each local authority 
la_progression <- la_summary_avg %>%
  pivot_wider(names_from = School.Year, values_from = Average_Obesity) %>%
  filter(!is.na(Reception) & !is.na(`Year 6`)) %>%
  mutate(
    Progression_Risk = `Year 6` - Reception
  ) %>%
  arrange(desc(Progression_Risk))
#Plot showing the 10 local authorities with the highest obesity progression risk
ggplot(la_progression %>% slice_max(Progression_Risk, n = 10), 
       aes(x = reorder(LA.Name, Progression_Risk), y = Progression_Risk)) +
  geom_col(fill = "#A3B18A") +
  coord_flip() +
  labs(
    title = "Top 10 Local Authorities by Obesity Progression Risk",
    x = "Local Authority",
    y = "Progression Risk (Year 6 - Reception)"
  ) +
  my_theme()
#Plot showing the 10 local authorities with the lowest obesity progression risk
ggplot(la_progression %>% slice_min(Progression_Risk, n = 10), 
       aes(x = reorder(LA.Name, -Progression_Risk), y = Progression_Risk)) +
  geom_col(fill = "#A3B18A") +
  coord_flip() +
  labs(
    title = "Lowest 10 Local Authorities by Obesity Progression Risk",
    x = "Local Authority",
    y = "Progression Risk (Year 6 - Reception)"
  ) +
  my_theme()


#Rate of change in obesity prevalence for each local authority 
#Calculation of obesity prevalence per year 
la_trends_data <- local_authority_data %>%
  filter(LA.Name != "-" & LA.Name != "Total") %>%
  group_by(Year, LA.Name, School.Year) %>%
  summarise(
    Obese = sum(Obese, na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Measured) * 100
  )
#Calculation of slope 
la_trends <- la_trends_data %>%
  group_by(LA.Name, School.Year) %>%
  summarise(
    Slope = coef(lm(Obesity.Prevalence ~ (Year)))[2],
    .groups = "drop"
  )
#Apply filter for Year 6
la_trends %>%
  filter(School.Year == "Year 6") %>%
  slice_max(Slope, n = 10)
#Plot the 10 local authorities with the fastest increase in Year 6 obesity 
ggplot(la_trends %>% filter(School.Year == "Year 6") %>% slice_max(Slope, n = 10),
       aes(x = reorder(LA.Name, Slope), y = Slope)) +
  geom_col(fill = "#C97B84") +
  coord_flip() +
  labs(
    title = "Annual Increase in Obesity Prevalence in Year 6 by Local Authority",
    x = "Local Authority",
    y = "Trend Slope (%/year)"
  ) +
  my_theme()


#Are there any local authorities with a flat or declining trend? 
#Categorise the local authority trends into declining, flat or increasing
la_trend_flags <- la_trends %>%
  mutate(
    Trend_Category = case_when(
      Slope < -0.05 ~ "Declining",
      Slope >= -0.05 & Slope <= 0.05 ~ "Flat",
      Slope > 0.05 ~ "Increasing"
    )
  )
#Apply filter to only show the local authorities that are flat or declining
flat_declining_las <- la_trend_flags %>%
  filter(Trend_Category %in% c("Flat", "Declining"))
#Print result
print(flat_declining_las)
#Apply filter to show only Year 6 
flat_declining_las_year6 <- la_trend_flags %>%
  filter(School.Year == "Year 6" & Trend_Category %in% c("Flat", "Declining") ) %>%
  arrange(Slope)
#Print result
print(flat_declining_las_year6)
#Comparing each local authority trend to the regional average that they belong to
la_region_lookup <- local_authority_data %>%
  select(LA.Name, Region = Region...Local.Authority) %>%
  distinct()
#Join regional information and calculate the slope
region_trends <- la_trends_data %>%
  left_join(la_region_lookup, by = "LA.Name") %>%
  group_by(Region, School.Year) %>%
  summarise(
    Slope = coef(lm(Obesity.Prevalence ~ (Year)))[2],
    .groups = "drop"
  )
#Join region slope in a new table with the local authority slope and calculate the difference 
la_vs_region <- la_trends %>%
  left_join(la_region_lookup, by = "LA.Name") %>%
  left_join(region_trends, by = c("Region", "School.Year"), suffix = c("_LA", "_Region")) %>%
  mutate(
    Slope_Diff = Slope_LA - Slope_Region
  ) %>%
  arrange(desc(Slope_Diff))
#Print result
print(la_vs_region)
#Extract top and bottom 10 for visualisation, filtered to Year 6 only 
top_bottom_las <- la_vs_region %>%
  filter(School.Year == "Year 6") %>%
  slice_max(Slope_Diff, n = 10) %>%
  bind_rows(
    la_vs_region %>%
      filter(School.Year == "Year 6") %>%
      slice_min(Slope_Diff, n = 10)
  )
#Plot showing the top and bottom 10 local authorities 
ggplot(top_bottom_las, aes(x = reorder(LA.Name, Slope_Diff), y = Slope_Diff, fill = Region)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top and Bottom 10 LAs by Difference from Regional Obesity Trend",
    x = "Local Authority",
    y = "Difference from Regional Slope (%/year)",
    fill = "Region"
  ) +
  my_theme()


#Comparison of local authorities obesity prevalence against regional and national average
#Re-use la_summary for local authority obesity prevalence and regional information 
la_avg <- la_summary %>%
  filter(LA.Name != "-" & LA.Name != "") %>%
  left_join(la_region_lookup, by = "LA.Name") %>%
  rename(Obesity.LA = Obesity.Prevalence)
#Regional average calculation 
region_avg <- la_avg %>%
  group_by(Region, School.Year) %>%
  summarise(
    Obesity.Region = mean(Obesity.LA, na.rm = TRUE), 
    .groups = "drop"
  )
#England average calculation 
england_avg <- la_avg %>%
  group_by(School.Year) %>%
  summarise(
    Obesity.England = mean(Obesity.LA, na.rm = TRUE), 
    .groups = "drop"
  )
#Join local, regional and England values into one table 
la_vs_averages <- la_avg %>%
  left_join(region_avg, by = c("Region", "School.Year")) %>%
  left_join(england_avg, by = "School.Year") %>%
  mutate(
    Diff_vs_Region = Obesity.LA - Obesity.Region,
    Diff_vs_England = Obesity.LA - Obesity.England
  )
#Sort the table in descending order showing the difference from regional average, filtered to Year 6
la_vs_averages %>%
  filter(School.Year == "Year 6") %>%
  arrange(desc(Diff_vs_Region))
#Sort the table in ascending order showing the difference from from regional average, filtered to Year 6
la_vs_averages %>%
  filter(School.Year == "Year 6") %>%
  arrange(Diff_vs_Region)
#Sort the table in descending order showing the difference from national average, filtered to Year 6
la_vs_averages %>%
  filter(School.Year == "Year 6") %>%
  arrange(desc(Diff_vs_England))
#Sort the table in ascending order showing the difference from from national average, filtered to Year 6
la_vs_averages %>%
  filter(School.Year == "Year 6") %>%
  arrange(Diff_vs_England)


#Is the spread in obesity prevalence across local authorities changing?
#Caculation of the standard deviate and range of obesity prevalence across local authorities per year and school year 
la_spread <- la_trends_data %>%
  group_by(Year, School.Year) %>%
  summarise(
    SD = sd(Obesity.Prevalence, na.rm = TRUE),
    Range = max(Obesity.Prevalence, na.rm = TRUE) - min(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Print result
print(la_spread)
#Plot the standard deviation
ggplot(la_spread, aes(x = Year, y = SD, color = School.Year, group = School.Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Standard Deviation of Obesity Prevalence Across Local Authorities",
    x = "Year",
    y = "Standard Deviation (%)",
    color = "School Year"
  ) +
  my_theme()
#Plot the range 
ggplot(la_spread, aes(x = Year, y = Range, color = School.Year, group = School.Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Range of Obesity Prevalence Across Local Authorities",
    x = "Year",
    y = "Range (Max − Min) (%)",
    color = "School Year"
  ) +
  my_theme()


#------------------------------------------------------------------------------------------------------#
#<----------------------Ethnicity & Location----------------------------- 
#Analyis of ethnicity and location on obesity rates, using ONS 2021 census data for support  
#Load the ONS 2021 Census ethnicity data
ethnicity_population <- read.csv("C:/Users/CAROL/My Drive/Uni/Birkbeck/Year 4/Project/My Project/Data/Processed/population-by-ethnicity-and-local-authority-2021.csv")
#Calculate the average Year 6 obesity prevalence by local authority 
la_obesity_avg <- la_summary %>%
  filter(School.Year == "Year 6") %>%
  rename(Obesity_Prevalence = Obesity.Prevalence)
#Pivot the census data so there is one row per local authority and ethnicity categories are columns 
ethnicity_by_la <- ethnicity_population %>%
  rename(LA.Name = Geography) %>%
  group_by(LA.Name, Ethnicity) %>%
  summarise(Population = sum(Value1, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Ethnicity, values_from = Population) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  )
#Map the ONS census ethnicity categories to the NCMP ethnicity categories
colnames(ethnicity_by_la)
ncmp_ethnic_map <- list(
  "White" = c("White", "White British", "White Irish", "Any Other White Background"),
  "Black or Black British" = c("Black", "Black African", "Black Caribbean", "Any Other Black Background"),
  "Asian or Asian British" = c("Asian", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any Other Asian Background"),
  "Mixed" = c("Mixed", 
              "Mixed White And Asian", 
              "Mixed White And Black African", 
              "Mixed White And Black Caribbean", 
              "Any Other Mixed/Multiple Ethnic Background"),
  "Chinese" = c("Chinese"),  
  "Any Other Ethnic Group" = c("Arab", "Other", "Any Other Ethnic Background"),
  "Not Stated" = NULL  
)
#Calculation for the ethnicity group percentage for each local authority 
la_ncmp_ethnic_percentage <- ethnicity_by_la %>%
  rowwise() %>%
  mutate(
    pct_White = sum(c_across(all_of(ncmp_ethnic_map[["White"]])), na.rm = TRUE) / Total * 100,
    pct_Black = sum(c_across(all_of(ncmp_ethnic_map[["Black or Black British"]])), na.rm = TRUE) / Total * 100,
    pct_Asian = sum(c_across(all_of(ncmp_ethnic_map[["Asian or Asian British"]])), na.rm = TRUE) / Total * 100,
    pct_Mixed = sum(c_across(all_of(ncmp_ethnic_map[["Mixed"]])), na.rm = TRUE) / Total * 100,
    pct_Chinese = sum(c_across(all_of(ncmp_ethnic_map[["Chinese"]])), na.rm = TRUE) / Total * 100,
    pct_Other = sum(c_across(all_of(ncmp_ethnic_map[["Any Other Ethnic Group"]])), na.rm = TRUE) / Total * 100
  ) %>%
  ungroup()
#Join the ethnicity group percentage to the Year 6 obesity prevalence 
la_ethnic_ncmp <- la_obesity_avg %>%
  left_join(la_ncmp_ethnic_percentage %>% select(LA.Name, starts_with("pct_")), by = "LA.Name")
#Pivot table into a longer view to allow for facet plots 
la_ethnic_long_ncmp <- la_ethnic_ncmp %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Ethnic_Group",
    names_prefix = "pct_",
    values_to = "Percentage"
  )
#Facet plots showing the correlatin between ethnicity percentage and obesity prevalence 
ggplot(la_ethnic_long_ncmp, aes(x = Percentage, y = Obesity_Prevalence)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#1C5F8A") +
  facet_wrap(~ Ethnic_Group, scales = "free_x") +
  labs(
    title = "Correlation Between Ethnic Composition and Obesity Prevalence (Year 6)",
    x = "% of Local Authority Population",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()
#Calculation of Pearson correlation coefficients for each ethnic group 
cor_table <- la_ethnic_long_ncmp %>%
  group_by(Ethnic_Group) %>%
  summarise(
    Correlation = cor(Percentage, Obesity_Prevalence, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    Label = paste0("r = ", round(Correlation, 2))
  )
#Facet plot for Year 6 with Pearson correlation coefficient displayed 
la_ethnic_long_ncmp_labeled <- la_ethnic_long_ncmp %>%
  left_join(cor_table, by = "Ethnic_Group")
ggplot(la_ethnic_long_ncmp_labeled, aes(x = Percentage, y = Obesity_Prevalence)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#1C5F8A") +
  facet_wrap(~ Ethnic_Group, scales = "free_x") +
  geom_text(data = cor_table, 
            aes(x = Inf, y = -Inf, label = Label),
            hjust = 1.1, vjust = -1.1, inherit.aes = FALSE,
            fontface = "bold", size = 4) +
  labs(
    title = "Correlation Between Ethnic Composition and Obesity Prevalence (Year 6)",
    subtitle = "With Pearson correlation coefficients",
    x = "% of Local Authority Population",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()


#Calculate the average Reception obesity prevalence by local authority 
la_obesity_reception <- la_summary %>%
  filter(School.Year == "Reception") %>%
  rename(Obesity_Prevalence = Obesity.Prevalence)
#Join the ethnicity group percentage to the Reception obesity prevalence 
la_ethnic_ncmp_reception <- la_obesity_reception %>%
  left_join(la_ncmp_ethnic_percentage %>% select(LA.Name, starts_with("pct_")), by = "LA.Name")
#Pivot table into a longer view to allow for facet plots 
la_ethnic_long_ncmp_reception <- la_ethnic_ncmp_reception %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Ethnic_Group",
    names_prefix = "pct_",
    values_to = "Percentage"
  )
#Calculation of Pearson correlation coefficients for each ethnic group 
cor_table_reception <- la_ethnic_long_ncmp_reception %>%
  group_by(Ethnic_Group) %>%
  summarise(
    Correlation = cor(Percentage, Obesity_Prevalence, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    Label = paste0("r = ", round(Correlation, 2))
  )
#Facet plot for Reception with Pearson correlation coefficient displayed 
ggplot(la_ethnic_long_ncmp_reception, aes(x = Percentage, y = Obesity_Prevalence)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#5B8FA8") +
  facet_wrap(~ Ethnic_Group, scales = "free_x") +
  geom_text(
    data = cor_table_reception,
    aes(x = Inf, y = -Inf, label = Label),
    inherit.aes = FALSE,
    hjust = 1.1, vjust = -1.1,
    size = 4, fontface = "bold"
  ) +
  labs(
    title = "Correlation Between Ethnic Composition and Obesity Prevalence (Reception)",
    subtitle = "With Pearson correlation coefficients",
    x = "% of Local Authority Population",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()


#Side by side comparison of Reception and Year 6
#Add a school year column to the la_ethnic _long tables 
reception_long <- la_ethnic_long_ncmp_reception %>%
  mutate(
    School_Year = "Reception"
  )
year6_long <- la_ethnic_long_ncmp %>%
  mutate(
    School_Year = "Year 6"
  )
#Combine the Reception and Year 6 tables 
ethnic_combined <- bind_rows(reception_long, year6_long)
#Plot showing the comparison of the correlation 
ggplot(ethnic_combined, aes(x = Percentage, y = Obesity_Prevalence, color = School_Year)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~ Ethnic_Group, scales = "free_x") +
  scale_color_manual(values = c("Reception" = "#5B8FA8", "Year 6" = "#C97B84")) +
  labs(
    title = "Comparison of Ethnicity-Obesity Correlation: Reception vs Year 6",
    x = "% of Local Authority Population",
    y = "Obesity Prevalence (%)",
    color = "School Year"
  ) +
  my_theme()


#Table to display the correlation coefficients for Reception and Year 6
cor_reception <- la_ethnic_long_ncmp_reception %>%
  group_by(Ethnic_Group) %>%
  summarise(
    r_Reception = cor(Percentage, Obesity_Prevalence, use = "complete.obs"),
    .groups = "drop"
  )
cor_year6 <- la_ethnic_long_ncmp %>%
  group_by(Ethnic_Group) %>%
  summarise(
    r_Year6 = cor(Percentage, Obesity_Prevalence, use = "complete.obs"),
    .groups = "drop"
  )
#Join Reception and Year 6
cor_compare <- cor_reception %>%
  left_join(cor_year6, by = "Ethnic_Group") %>%
  mutate(
    Difference = r_Year6 - r_Reception
  ) %>%
  arrange(desc(abs(Difference)))
#Print result
print(cor_compare)


#How does obesity rates vary by region and ethnic group
#Merge region into ethnicity_data using year and school year
regional_lookup <- local_authority_data %>%
  filter(LA.Name == "-") %>%
  select(Year, School.Year, Region = `Region...Local.Authority`) %>%
  distinct()
ethnicity_with_region <- ethnicity_data %>%
  left_join(regional_lookup, 
            by = c("Year", "School.Year"))
#Filter to exclude total category, calculate obesity prevalence by ethnic group and region 
ethnicity_by_region <- ethnicity_with_region %>%
  filter(Ethnic.category != "Total", !is.na(Region)) %>%
  group_by(Region, Ethnic.category) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = (Obese / Measured) * 100
  )
#Plot showing obesity prevalence by ethnic group in each region 
ggplot(ethnicity_by_region, aes(x = Ethnic.category, y = Obesity.Prevalence, fill = Ethnic.category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Region, scales = "free_y") +
  labs(
    title = "Obesity Prevalence by Ethnic Group in Each Region",
    x = "Ethnic Group", y = "Obesity Prevalence (%)"
  ) +
  my_theme()


#Variation in obesity prevalence across regions by ethnicity, with boxplot
ethnicity_box_data <- ethnicity_with_region %>%
  filter(Ethnic.category != "Total", !is.na(Region)) %>%
  mutate(
    Obesity.Prevalence = (Obesity..including.severe.obesity. / Number.of.children.measured) * 100
  )
#Box plot of results 
ggplot(ethnicity_box_data, aes(x = Ethnic.category, y = Obesity.Prevalence)) +
  geom_boxplot(fill = "#A3B18A", color = "black", outlier.alpha = 0.3) +
  labs(
    title = "Variation in Obesity Prevalence by Ethnic Group Across Regions",
    x = "Ethnic Group",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()


#Calculation of standard deviation and range of obesity prevalence per ethnic group
ethnicity_variation_summary <- ethnicity_with_region %>%
  filter(Ethnic.category != "Total", !is.na(Region)) %>%
  mutate(
    Obesity.Prevalence = (Obesity..including.severe.obesity. / Number.of.children.measured) * 100
  ) %>%
  group_by(Ethnic.category) %>%
  summarise(
    Mean = round(mean(Obesity.Prevalence, na.rm = TRUE), 2),
    SD = round(sd(Obesity.Prevalence, na.rm = TRUE), 2),
    Min = round(min(Obesity.Prevalence, na.rm = TRUE), 2),
    Max = round(max(Obesity.Prevalence, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    Range = round(Max - Min, 2)
  ) %>%
  arrange(desc(SD))  
#Print result
print(ethnicity_variation_summary)


#Have regional inequalities widened post-COVID?
#Calculation of year on year regional trend, excluding England total
regional_variation <- ethnicity_with_region %>%
  filter(!is.na(Region), Region != "England", Ethnic.category != "Total") %>%
  mutate(
    Year = as.numeric(Year),
    Obesity.Prevalence = (Obesity..including.severe.obesity. / Number.of.children.measured) * 100,
    Period = ifelse(Year <= 2019, "Pre-COVID", "Post-COVID")
  ) %>%
  group_by(Period, Year, Region) %>%
  summarise(
    SD = sd(Obesity.Prevalence, na.rm = TRUE),
    Range = max(Obesity.Prevalence, na.rm = TRUE) - min(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Comparison of average standard deviation and range pre and post-COVID
regional_sd_summary <- regional_variation %>%
  group_by(Period) %>%
  summarise(
    Mean_SD = mean(SD, na.rm = TRUE),
    Mean_Range = mean(Range, na.rm = TRUE),
    .groups = "drop"
  )
#Print result
print(regional_sd_summary)
#Statistical test to compare the periods 
pre_sd <- regional_variation %>% filter(Period == "Pre-COVID") %>% pull(SD)
post_sd <- regional_variation %>% filter(Period == "Post-COVID") %>% pull(SD)
#T-Test to compare pre and post-COVID standard deviations 
t_test_result <- t.test(pre_sd, post_sd, var.equal = FALSE)
#Print result
print(t_test_result)


#Have ethnic inequalities widened post-COVID?
#Calculation of yearly obesity prevalence by ethnic group 
ethnic_variation <- ethnicity_with_region %>%
  filter(!is.na(Region), Region != "England", Ethnic.category != "Total") %>%
  group_by(Year, School.Year, Ethnic.category) %>%
  summarise(
    Obese = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Obesity.Prevalence = ifelse(Measured > 0, (Obese / Measured) * 100, NA),
    Period = ifelse(Year <= 2019, "Pre-COVID", "Post-COVID")
  )
#Standard deviation and range calculations to measure inequality, per year and school year
ethnic_dispersion <- ethnic_variation %>%
  group_by(Year, School.Year, Period) %>%
  summarise(
    SD = sd(Obesity.Prevalence, na.rm = TRUE),
    Range = max(Obesity.Prevalence, na.rm = TRUE) - min(Obesity.Prevalence, na.rm = TRUE),
    .groups = "drop"
  )
#Comparison of average standard deviation and range for pre and post-COVID
ethnic_summary <- ethnic_dispersion %>%
  group_by(Period) %>%
  summarise(
    Mean_SD = mean(SD, na.rm = TRUE),
    Mean_Range = mean(Range, na.rm = TRUE),
    .groups = "drop"
  )
#Print result
print(ethnic_summary)
#T-test for standard deviation 
pre_sd_ethnic <- ethnic_dispersion %>% filter(Period == "Pre-COVID") %>% pull(SD)
post_sd_ethnic <- ethnic_dispersion %>% filter(Period == "Post-COVID") %>% pull(SD)
#2 sample t-test on standard deviation 
ethnic_sd_test <- t.test(pre_sd_ethnic, post_sd_ethnic, var.equal = FALSE)
#Print result
print(ethnic_sd_test)


#Are there any local authorities that have both high ethnic minority representation and high obesity progression risk?
#Standardise local authority names
la_progression_clean <- la_progression %>%
  filter(!is.na(LA.Name) & LA.Name != "") %>%
  mutate(
    clean_la = tolower(gsub("[^a-z]", "", trimws(LA.Name)))
  )
la_ethnic_percentage_clean <- la_ncmp_ethnic_percentage %>%
  mutate(
    Minority_Percent = 100 - pct_White, 
    clean_la = tolower(gsub("[^a-z]", "", trimws(LA.Name)))
  ) %>%
  select(LA.Name,clean_la, Minority_Percent)
#Join using the cleaned local authority names 
la_combined <- la_progression_clean %>%
  left_join(la_ethnic_percentage_clean, by = "clean_la") %>%
  filter(!is.na(Minority_Percent), !is.na(Progression_Risk))
#In a new table, add the top 25% for both metrics
cutoffs <- la_combined %>%
  summarise(
    risk_75 = quantile(Progression_Risk, 0.75, na.rm = TRUE),
    minority_75 = quantile(Minority_Percent, 0.75, na.rm = TRUE)
  )
#Flag any high risk and high minority local authorities
la_combined <- la_combined %>%
  mutate(
    High_Risk = Progression_Risk >= cutoffs$risk_75,
    High_Minority = Minority_Percent >= cutoffs$minority_75,
    High_Both = High_Risk & High_Minority
  )
#View the local authorities that meet the cut offs 
priority_las <- la_combined %>%
  filter(High_Both) %>%
  select(LA.Name.x, Progression_Risk, Minority_Percent) %>%
  arrange(desc(Progression_Risk)) %>%
  rename(LA.Name = LA.Name.x)
#Print result
print(priority_las)


#------------------------------------------------------------------------------------------------------#
#<----------------------Multivariable logistic regression model-----------------------------
#Extraction and preparation of Reception data
reception <- ethnicity_with_region %>%
  filter(School.Year == "Reception", Ethnic.category != "Total", !is.na(Region)) %>%
  group_by(Year, Region, Ethnic.category) %>%
  summarise(
    Obese_Reception = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured_Reception = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  )

#Extraction and preparation of Year 6 data
year6 <- ethnicity_with_region %>%
  filter(School.Year == "Year 6", Ethnic.category != "Total", !is.na(Region)) %>%
  group_by(Year, Region, Ethnic.category) %>%
  summarise(
    Obese_Year6 = sum(Obesity..including.severe.obesity., na.rm = TRUE),
    Measured_Year6 = sum(Number.of.children.measured, na.rm = TRUE),
    .groups = "drop"
  )

#Join for Reception and Year 6, on year, region and ethnicity 
model_data <- left_join(year6, reception, by = c("Year", "Region", "Ethnic.category")) %>%
  filter(Measured_Reception > 0, Measured_Year6 > 0) %>%
  mutate(
    Prop_Obese_Reception = Obese_Reception / Measured_Reception
  )

#Split data into training and testing data sets 
train_data_region <- model_data %>% filter(Year <= 2018)
train_data_region <- train_data_region %>%
  mutate(
    Obese_Year6 = round(Obese_Year6),
    Obese_Reception = round(Obese_Reception)
  )
test_data_region <- model_data %>% filter(Year > 2018)
test_data_region <- test_data_region %>%
  mutate(
    Obese_Year6 = round(Obese_Year6),
    Obese_Reception = round(Obese_Reception)
  )

#Train logical regression model using the training data 
model_region <- glm(
  cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
    Prop_Obese_Reception + Ethnic.category + Region,
  data = train_data_region,
  family = binomial
)
#View summary of the model 
summary(model_region)

#Using the testing data, predicting the obesity probability 
test_data_region$predicted_prob <- predict(model_region, newdata = test_data_region, type = "response")

#Evaluation of the model, using ROC curve and AOC value. Creating a binary outcome. 1 if obesity prevalence is Year 6 is > 20%
test_data_region$obese_binary <- ifelse(test_data_region$Obese_Year6 / test_data_region$Measured_Year6 > 0.2, 1, 0)
pred <- prediction(test_data_region$predicted_prob, test_data_region$obese_binary)
perf_roc <- performance(pred, "tpr", "fpr")
perf_auc <- performance(pred, "auc")
auc_region <- perf_auc@y.values[[1]]
#ROC curve plot 
plot(perf_roc, col = "#5B8FA8", lwd = 2, main = paste("Region + Ethnicity ROC Curve (AUC =", round(auc_region, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "grey")
#Print AUC 
print(paste("AUC:", round(auc_region, 3)))

#Check of model calibration by calculating the range and distribution of probabilities 
summary(test_data_region$predicted_prob)
#Check to compare predicted vs actual obesity rates 
test_data_region %>%
  group_by(Ethnic.category) %>%
  summarise(
    Mean_Predicted = mean(predicted_prob),
    Mean_Actual = mean(Obese_Year6 / Measured_Year6)
  ) %>%
  arrange(desc(Mean_Predicted))

#5-fold cross validation
#Set up 5-fold cross-validation
set.seed(42)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_data_region)))
auc_scores_region <- numeric(k)
#Run the cross-validation
for (i in 1:k) {
  train_fold <- train_data_region[folds != i, ]
  test_fold <- train_data_region[folds == i, ]
  model_cv <- glm(
    cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
      Prop_Obese_Reception + Ethnic.category + Region,
    data = train_fold,
    family = binomial
  )
  test_fold$predicted <- predict(model_cv, newdata = test_fold, type = "response")
  test_fold$actual <- ifelse(test_fold$Obese_Year6 / test_fold$Measured_Year6 > 0.2, 1, 0)
  pred_obj <- prediction(test_fold$predicted, test_fold$actual)
  auc <- performance(pred_obj, "auc")@y.values[[1]]
  auc_scores_region[i] <- auc
}
#Calculate and print the mean AUC score
mean_auc_region <- mean(auc_scores_region)
print(paste("Mean cross-validated AUC (Region + Ethnicity):", round(mean_auc_region, 3)))

#Predict using the testing data set 
test_data_region$predicted_prob <- predict(model_region, newdata = test_data_region, type = "response")

#Summary mean predicted and actual obesity prevalence by region + ethnicity 
risk_summary <- test_data_region %>%
  group_by(Region, Ethnic.category, drop = TRUE) %>%
  summarise(
    Mean_Predicted = mean(predicted_prob, na.rm = TRUE),
    Actual_Obesity = sum(Obese_Year6, na.rm = TRUE) / sum(Measured_Year6, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Predicted)) %>%
  mutate(
    Group = paste(Region, Ethnic.category, sep = " - "),
    Group = factor(Group, levels = Group)
  )
#View result
print(risk_summary)



#<----------------------Model optimisation part 1 - Region x Ethnicity-----------------------------
#Creation of a region x ethnicity combined variable
train_data_combined <- train_data_region %>%
  mutate(
    Obese_Year6 = round(Obese_Year6),
    Obese_Reception = round(Obese_Reception),
    Region_Ethnicity = interaction(Region, Ethnic.category, drop = TRUE)
  )
test_data_combined <- test_data_region %>%
  mutate(
    Obese_Year6 = round(Obese_Year6),
    Obese_Reception = round(Obese_Reception),
    Region_Ethnicity = interaction(Region, Ethnic.category, drop = TRUE)
  )

#Model is trained using the combined region x ethnicity variable using the training data set, for Year 6 children only
model_combined <- glm(
  cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
    Prop_Obese_Reception + Region_Ethnicity,
  data = train_data_combined,
  family = binomial
)
#View summary of the model 
summary(model_combined)

#Predicting the obesity probability using the testing data set 
test_data_combined$predicted_combined <- predict(model_combined, newdata = test_data_combined, type = "response")

#Evaluation of the model using ROC curve and AUC score
test_data_combined$actual_binary <- ifelse(test_data_combined$Obese_Year6 / test_data_combined$Measured_Year6 > 0.2, 1, 0)
pred_combined <- prediction(test_data_combined$predicted_combined, test_data_combined$actual_binary)
perf_combined <- performance(pred_combined, "auc")
perf_roc_combined <- performance(pred_combined, "tpr", "fpr")
auc_combined <- perf_combined@y.values[[1]]
#ROC curve plot 
plot(perf_roc_combined, col = "#5B8FA8", lwd = 2, main = paste("Region x Ethnicity ROC Curve (AUC =", round(auc_combined, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "grey")
#Print AUC
print(paste("AUC (with Region x Ethnicity):", round(auc_combined, 3)))

#Check of model calibration by calculating the range and distribution of probabilities 
summary(test_data_combined$predicted_prob)
#Check to compare predicted vs actual obesity rates 
test_data_combined %>%
  group_by(Ethnic.category) %>%
  summarise(
    Mean_Predicted = mean(predicted_prob),
    Mean_Actual = mean(Obese_Year6 / Measured_Year6)
  ) %>%
  arrange(desc(Mean_Predicted))

#5-fold cross validation
#A combined region x ethnicity column is added to the training data 
train_data_combined <- train_data_combined %>%
  mutate(
    Region_Ethnicity = interaction(Region, Ethnic.category, drop = TRUE)
  )

#Set up 5-fold cross-validation
set.seed(42)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_data_combined)))
auc_scores_combined <- numeric(k)
#Run the cross-validation
for (i in 1:k) {
  train_fold <- train_data_combined[folds != i, ]
  test_fold <- train_data_combined[folds == i, ]
  model_cv <- glm(
    cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
      Prop_Obese_Reception + Region_Ethnicity,
    data = train_fold,
    family = binomial
  )
  test_fold$predicted <- predict(model_cv, newdata = test_fold, type = "response")
  test_fold$actual <- ifelse(test_fold$Obese_Year6 / test_fold$Measured_Year6 > 0.2, 1, 0)
  pred_obj <- prediction(test_fold$predicted, test_fold$actual)
  auc <- performance(pred_obj, "auc")@y.values[[1]]
  auc_scores_combined[i] <- auc
}
#Calculate and print the mean AUC score
mean_auc_combined <- mean(auc_scores_combined)
print(paste("Mean cross-validated AUC (Region x Ethnicity):", round(mean_auc_combined, 3)))

#Check to make sure the Region_Ethnicity column is in the training data 
test_data_combined <- test_data_combined %>%
  mutate(
    Region_Ethnicity = interaction(Region, Ethnic.category)
  ) %>%
  mutate(
    Region_Ethnicity = factor(Region_Ethnicity, levels = levels(train_data_combined$Region_Ethnicity))
  )

#Predict using the testing data set 
test_data_combined$predicted_prob <- predict(model_combined, newdata = test_data_combined, type = "response")

#Summary mean predicted and actual obesity prevalence by region x ethnicity 
risk_summary_combined <- test_data_combined %>%
  group_by(Region_Ethnicity) %>%
  summarise(
    Mean_Predicted = mean(predicted_prob, na.rm = TRUE),
    Actual_Obesity = sum(Obese_Year6, na.rm = TRUE) / sum(Measured_Year6, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Predicted)) %>%
  mutate(
    Group = factor(Region_Ethnicity, levels = Region_Ethnicity)
  )
print(risk_summary_combined)

#Extraction of top 15 highest risk combinations 
top_risk_combined <- risk_summary_combined %>% slice_max(Mean_Predicted, n = 15)

#Plot showing the predicted vs actual obesity prevalence by the top 15 risk combinations  
ggplot(top_risk_combined , aes(x = Group)) +
  geom_col(aes(y = Mean_Predicted * 100), fill = "#C97B84", width = 0.6) +
  geom_point(aes(y = Actual_Obesity), color = "#5B8FA8", size = 3) +
  labs(
    title = "Top 15 Region x Ethnicity Groups by Predicted Obesity Risk (Year 6)",
    subtitle = "Bars = Predicted | Dots = Actual",
    x = "Region x Ethnicity Group",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()

#Plot showing the predicted vs actual obesity prevalence by region x ethnicity 
ggplot(risk_summary_combined, aes(x = Actual_Obesity, y = Mean_Predicted * 100)) +
  geom_point(color = "#A3B18A", size = 2.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Predicted vs Actual Obesity Prevalence by Region x Ethnicity",
    x = "Actual Obesity Prevalence (%)",
    y = "Predicted Obesity Prevalence (%)"
  ) +
  my_theme()

#Confusion matrix 
#Threshold classification 
threshold <- 0.2
test_data_combined$predicted_class <- ifelse(test_data_combined$predicted_prob > threshold, 1, 0)
#The actual classification already exists within test_data_combined$obese_binary
#Creation of confusion matrix 
conf_matrix_combined <- table(Predicted = test_data_combined$predicted_class, Actual = test_data_combined$obese_binary)
print("Confusion Matrix:")
print(conf_matrix_combined)

#Calculation of performance metrics
TP <- conf_matrix_combined[2, 2] #True positives
TN <- conf_matrix_combined[1, 1] #True negatives
FP <- conf_matrix_combined[2, 1] #False positives
FN <- conf_matrix_combined[1, 2] #False negatives
#Calculation of accuracy, sensitivity and specificity 
accuracy <- (TP + TN) / sum(conf_matrix_combined)
sensitivity <- TP / (TP + FN)  
specificity <- TN / (TN + FP)  
cat("\nAccuracy:", round(accuracy, 3), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")


#<----------------------Model optimisation part 2 - Region x Ethnicity x Gender-----------------------------
#Extraction of Reception data 
gender_reception <- gender_bmi_data %>%
  filter(School.Year == "Reception") %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  select(Year, Gender, Obese_Reception = Obese..including.severe.obesity., 
         Measured_Reception = Number.Measured) %>%
  mutate(
    Obese_Reception = round(Obese_Reception),
    Measured_Reception = round(Measured_Reception)
  )

#Extraction of Year 6 data
gender_year6 <- gender_bmi_data %>%
  filter(School.Year == "Year 6") %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  select(Year, Gender, Obese_Year6 = Obese..including.severe.obesity., 
         Measured_Year6 = Number.Measured) %>%
  mutate(
    Obese_Year6 = round(Obese_Year6),
    Measured_Year6 = round(Measured_Year6)
  )

#Joining of Reception and Year 6 data by year and gender 
gender_combined <- left_join(gender_year6, gender_reception, by = c("Year", "Gender")) %>%
  filter(!is.na(Obese_Year6), !is.na(Obese_Reception)) %>%
  mutate(
    Prop_Obese_Reception = Obese_Reception / Measured_Reception
  )
#Join with ethnicity and region data 
region_ethnicity <- ethnicity_with_region %>%
  filter(Ethnic.category != "Total", !is.na(Region)) %>%
  mutate(
    Ethnic.category = factor(trimws(Ethnic.category)),
    Region = factor(Region),
    Year = as.numeric(Year)
  ) %>%
  select(Year, Region, Ethnic.category) %>%
  distinct()

#Creation of all combinations of Year x Gender x Region x Ethnicity 
combined_keys <- expand.grid(
  Year = unique(gender_combined$Year),
  Gender = unique(gender_combined$Gender),
  Region = unique(region_ethnicity$Region),
  Ethnic.category = unique(region_ethnicity$Ethnic.category)
)

#Join with gender obesity data 
model_data_gender <- left_join(combined_keys, gender_combined, by = c("Year", "Gender")) %>%
  filter(!is.na(Prop_Obese_Reception), !is.na(Obese_Year6), !is.na(Measured_Year6))

#Creation of the combined Region x Ethnicity x Gender variable 
model_data_gender <- model_data_gender %>%
  mutate(
    Region_Ethnicity_Gender = interaction(Region, Ethnic.category, Gender, drop = TRUE)
  )

#Split data into training and testing data sets 
train_data_gender <- model_data_gender %>% filter(Year <= 2018)
test_data_gender  <- model_data_gender %>% filter(Year > 2018)

#Model is trained using the combined region x ethnicity x gender variable using the training data set, for Year 6 children only
model_gender <- glm(
  cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
    Prop_Obese_Reception + Region_Ethnicity_Gender,
  data = train_data_gender,
  family = binomial
)

#View summary of the model 
summary(model_gender)

#Predicting the obesity probability using the testing data set  
test_data_gender$predicted_gender <- predict(model_gender, newdata = test_data_gender, type = "response")

##Evaluation of the model using ROC curve and AUC score
test_data_gender$actual_binary <- ifelse(test_data_gender$Obese_Year6 / test_data_gender$Measured_Year6 > 0.2, 1, 0)
pred_gender <- prediction(test_data_gender$predicted_gender, test_data_gender$actual_binary)
perf_auc_gender <- performance(pred_gender, "auc")
perf_roc_gender <- performance(pred_gender, "tpr", "fpr")
auc_gender <- perf_auc_gender@y.values[[1]]

#ROC curve plot 
plot(perf_roc_gender, col = "#5B8FA8", lwd = 2, main = paste("Region x Ethnicity x Gender ROC Curve (AUC =", round(auc_gender, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "grey")
#Print AUC
print(paste("AUC with Region x Ethnicity x Gender:", round(auc_gender, 3)))


#Check of model calibration by calculating the range and distribution of probabilities 
summary(test_data_gender$predicted_gender)
#Check to compare predicted vs actual obesity rates 
test_data_gender %>%
  group_by(Ethnic.category) %>%
  summarise(
    Mean_Predicted = mean(predicted_gender),
    Mean_Actual = mean(Obese_Year6 / Measured_Year6)
  ) %>%
  arrange(desc(Mean_Predicted))

#5-fold cross validation 
#Set up 5-fold cross-validation
set.seed(42)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(train_data_gender)))
auc_scores_gender <- numeric(k)

#Run the cross-validation
for (i in 1:k) {
  train_fold <- train_data_gender[folds != i, ]
  test_fold <- train_data_gender[folds == i, ]
  model_cv <- glm(
    cbind(Obese_Year6, Measured_Year6 - Obese_Year6) ~ 
      Prop_Obese_Reception + Region_Ethnicity_Gender,
    data = train_fold,
    family = binomial
  )
  test_fold$predicted <- predict(model_cv, newdata = test_fold, type = "response")
  test_fold$actual <- ifelse(test_fold$Obese_Year6 / test_fold$Measured_Year6 > 0.2, 1, 0)
  pred_obj <- prediction(test_fold$predicted, test_fold$actual)
  auc <- performance(pred_obj, "auc")@y.values[[1]]
  auc_scores_gender[i] <- auc
}

#Calculate and print the mean AUC score
mean_auc_gender <- mean(auc_scores_gender)
print(paste("Mean cross-validated AUC (Region x Ethnicity x Gender):", round(mean_auc_gender, 3)))

#top 15 risk groups
#Summarise mean predicted and actual obesity rate per group
risk_gender_summary <- train_data_gender %>%
  mutate(
    predicted = predict(model_gender, newdata = train_data_gender, type = "response")
  ) %>%
  group_by(Region_Ethnicity_Gender) %>%
  summarise(
    Mean_Predicted = mean(predicted, na.rm = TRUE),
    Actual_Obesity = sum(Obese_Year6, na.rm = TRUE) / sum(Measured_Year6, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Predicted)) %>%
  mutate(
    Group = factor(Region_Ethnicity_Gender, levels = Region_Ethnicity_Gender)
  )

# Select top 15 groups
top_gender_risk <- risk_gender_summary %>% slice_max(Mean_Predicted, n = 15)

#Plot showing the predicted vs actual obesity prevalence by the top 15 risk combinations 
ggplot(top_gender_risk, aes(x = Group)) +
  geom_col(aes(y = Mean_Predicted * 100), fill = "#C97B84", width = 0.6) +
  geom_point(aes(y = Actual_Obesity), color = "#5B8FA8", size = 3) +
  labs(
    title = "Top 15 Region x Ethnicity x Gender Groups by Predicted Obesity Risk (Year 6)",
    subtitle = "Bars = Predicted | Dots = Actual",
    x = "Group",
    y = "Obesity Prevalence (%)"
  ) +
  my_theme()

#Plot showing the predicted vs actual obesity prevalence by region x ethnicity x gender
ggplot(risk_gender_summary, aes(x = Actual_Obesity, y = Mean_Predicted * 100)) +
  geom_point(color = "#A3B18A", size = 2.5, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Predicted vs Actual Obesity Prevalence by Region x Ethnicity x Gender",
    x = "Actual Obesity Prevalence (%)",
    y = "Predicted Obesity Prevalence (%)"
  ) +
  my_theme()

#Confusion matrix
#Threshold classification 
threshold <- 0.2
test_data_gender$predicted_class <- ifelse(test_data_gender$predicted_gender > threshold, 1, 0)
#Creation of confusion matrix 
conf_matrix_gender <- table(Predicted = test_data_gender$predicted_class, Actual = test_data_gender$actual_binary)
print("Confusion Matrix:")
print(conf_matrix_gender)

#Calculation of performance metrics
TP <- conf_matrix_gender[2, 2] #True positives
TN <- conf_matrix_gender[1, 1] #True negatives
FP <- conf_matrix_gender[2, 1] #False positives
FN <- conf_matrix_gender[1, 2] #False negatives
#Calculation of accuracy, sensitivity and specificity 
accuracy <- (TP + TN) / sum(conf_matrix_gender)
sensitivity <- TP / (TP + FN)  
specificity <- TN / (TN + FP)  
cat("\nAccuracy:", round(accuracy, 3), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")


#Summary table showing the AUC scores for the 3 models
auc_summary <- tibble::tibble(
  Model = c(
    "First: Region + Ethnicity",
    "Second: Region x Ethnicity",
    "Third: Region x Ethnicity x Gender"
  ),
  AUC_Test = c(
    round(auc_region, 3),
    round(auc_combined, 3),
    round(auc_gender, 3)
  ),
  AUC_CV_Mean = c(
    round(mean_auc_region, 3),
    round(mean_auc_combined, 3), 
    round(mean_auc_gender, 3)
  )
)
#Print tibble
print(auc_summary)