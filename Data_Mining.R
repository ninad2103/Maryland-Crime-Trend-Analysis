# Load the packages
library(tidyverse)
library(lubridate)

install.packages("Rserve")

library(Rserve)
Rserve(args="--no-save")


# Read in the crime data
print(Crime_Data1)
colnames(Crime_Data1)[colnames(Crime_Data1)=="VIOLENT CRIME RATE PER 100,000 PEOPLE"] <- "crime_rate"

# Select and mutate columns the needed columns
crime_use <- Crime_Data1 %>% 
  select(JURISDICTION, YEAR, POPULATION, crime_rate) 

# Peek at the data
head(crime_use)

#Rescaling the data
# Mutate data to create another year column, YEAR_R
crime_use <- crime_use %>%
  mutate(YEAR_R = YEAR - min(YEAR))
head(crime_use)

#Building LMER- linear meixed effecr regression model
# load the lmerTest package
library(lmerTest)

# Build a lmer and save it as lmer_crime
lmer <- lmer(crime_rate ~ YEAR_R + (YEAR_R|JURISDICTION), data=crime_use)

# Print the model output
lmer


# Examine the model outputs using summary
summary(lmer)

coef(summary(lmer))

ggplot(fortify(lmer), aes(YEAR_R, crime_rate, color=JURISDICTION)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun=mean, geom="line")



# This is for readability 
noquote("**** Fixed-effects ****")

# Use fixef() to view fixed-effects
fixef(lmer)

# This is for readability 
noquote("**** Random-effects ****")

# Use ranef() to view random-effects
ranef(lmer)


# Add the fixed-effect to the random-effect and save as county_slopes
print(fixef(lmer)["YEAR_R"] )
print(ranef(lmer)$JURISDICTION["YEAR_R"])

county_slopes <- fixef(lmer)["YEAR_R"] + ranef(lmer)$JURISDICTION["YEAR_R"]


# Add a new column with county names
county_slopes <-
  county_slopes %>% 
  rownames_to_column("county")

head(county_slopes)

# Load usmap package
library(usmap)

# load and filter map data
county_map <- us_map(regions = "counties", include = "MD")

# See which counties are not in both datasets
county_slopes %>% anti_join(county_map, by = "county")
county_map %>% anti_join(county_slopes, by = "county")

# Rename crime_names county
county_slopes  <- county_slopes  %>% 
  mutate(county = ifelse(county == "Baltimore City", "Baltimore city", county))

# Merge the map and slope data frames
both_data <- county_slopes %>% full_join(county_map, by = "county")

# Peek at the data
head(both_data)

# Set the notebook's plot settings
options(repr.plot.width=10, repr.plot.height=5)




# Plot the results 
crime_map <- 
  ggplot(data=both_data, aes(x=x, y=y, group=county, fill=YEAR_R)) +
  geom_text(aes(label = county), data = both_data,  size = 1, hjust = 1)+
  geom_polygon() + 
  scale_fill_continuous(name = expression(atop("Change in crime rate","(Number year"^-1*")")),
                        low = "skyblue", high ="red")


# Look at the map
crime_map

install.packages("xlsx")
library(xlsx)
write.xlsx(both_data, "/Users/nishigandha/Documents/Semester 2/IS 733/Project/Both_Data.xls")












