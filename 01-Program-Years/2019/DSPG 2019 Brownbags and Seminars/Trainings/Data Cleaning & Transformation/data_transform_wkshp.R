# Install tidyverse R package
install.packages("tidyverse")
install.packages("scales")

# Loads the tidyverse R package
library(tidyverse)
library(scales)

# Read in comma delimiated (.csv) "franchise" dataframe
data1 <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE)
# Read in comma delimiated (.csv) "franchise2" dataframe
data2 <- read.csv(file.choose(), header = T, stringsAsFactors = TRUE)

# Coerces dataframe into a special type, 'tibble'
data1 <- as_tibble(data1)
data2 <- as_tibble(data2)
data1
data2

## SUMMARIZE CASES

# Summary of average rent across franchises, could use this to aggregate data if saved
options(pillar.sigfig = 9)
data1 %>%
  summarize(mean = mean(Rent), n = n())

# GROUP CASES

# Summary of average rent across franchises grouped by embedded community
data1 %>%
  group_by(Community) %>%
  summarize(mean = mean(Rent), n = n())

# Summary of number of employees working at franchises grouped by embedded community
data1 %>%
  group_by(Community) %>%
  tally(NEmp)

## MANIPULATE CASES

# Filter franchises out by those whose establishment began before "2009" 
data1 %>%
  filter(Year < 2009)

# Filter franchises out by those whose establishment began before "2009" and provide mean
data1 %>%
  filter(Year < 2009) %>%
  summarise(mean = mean(Sales))

# Filter franchises out by those whose establishment began after "2009" and provide mean
data1 %>%
  filter(Year > 2009) %>%
  summarise(mean = mean(Sales))

# Distinct franchise cases
data1 %>%
  distinct()

# Distinct franchise building colors
data1 %>%
  distinct(BdgColor)

# Slice a portion of the data frame 5 through total
data1 %>%
  slice(5:n())

data1 %>%
  filter(between(row_number(), 5, n()))

# Slice a portion of the data frame, dropping everything after case "4"
data1 %>%
  slice(-5:-n())

data1 %>%
  filter(between(row_number(), 1, 4))

# Sample a fraction of the data frame (a holdover data frame)
data1 %>%
  sample_frac(0.5, replace = FALSE)

# Arrange franchises by annual sales in decending order
data1 %>%
  arrange(desc(Sales))

# Add a new case to the data set
data1 <- data1 %>%
  add_row(Franchise = 11, Year = 2019, BdgColor = "Orange", 
          NEmp = 10, Rent = 15023, EmpPay = 300000, 
          MktBudg = 45000, CustomerSat = 7.5, Sales = 20000, Community = "Suburban", 
          ParkCap = 16)
data1

## MANIPULATE VARIABLES

# Select only Franchise, CustomerSat, Sales, and Community variables
data1 %>%
  select(Franchise, CustomerSat, Sales, Community)

# Select only variables that start with letter "C"
data1 %>%
  select(starts_with("C"))

# Mutate a new variable column called "Overhead" which is Rent + EmpPay + MktBudg
data1 <- data1 %>%
  mutate(Overhead = Rent + EmpPay + MktBudg)
data1

# Mutate new variable column "NetProfit" which is Sales - Overhead
data1 <- data1 %>% 
  mutate(NetProfit = Sales - Overhead)
data1

# Transmute new variable "NetProfit" and drop all other variables
data1 %>% 
  transmute(NetProfit = Sales - Overhead)

# Add new column "FranchNum"
data1 <- data1 %>%
  add_column(FranchNum = c(1, 10, 5, 8, 4, 6, 3, 2, 9, 7, 11), .before = "Year")
data1

# Rename Franchise to "CaseNum"
data1 <- data1 %>%
  rename("CaseNum" = "Franchise")
data1

## COMBINE TABLES

# Bind data1 table next to data2 table
data1 %>%
  bind_cols(data2)

# Bind data1 to data2 matching on rows that match
data1 <- data1 %>%
  left_join(data2, by = "CaseNum")
data1

# Bind new rows of data
newdata <- tibble(
  CaseNum = c(12, 13, 14),
  FranchNum = c(12, 13, 14),
  Year = c(2019, 2019, 2019),
  BdgColor = c("Orange", "Orange", "Orange"),
  NEmp = c(12, 11, 9),
  Rent = c(12003, 14560, 12456),
  EmpPay = c(320000, 310403, 290000),
  MktBudg = c(45000, 45000, 45000),
  CustomerSat = c(7.6, 4.5, 8.9),
  Sales = c(23000, 14000, 45000),
  Community = c("Rural", "Rural", "Urban"),
  ParkCap = c(23, 45, 2),
  Overhead = c(377003, 396963, 347456),
  NetProfit = c(-354003, -382963, -302456),
  HealthCode = factor(c("A", "A", "A"), levels = c("A", "B", "C", "D"))
)
newdata

data1 <- data1 %>%
  bind_rows(newdata)
data1

# Smoothing the correlation between CustomerSat and Sales 
cor.test(data1$CustomerSat, data1$Sales, method= "pearson")

# Add the regression line
data1 %>%
  ggplot(aes(x = CustomerSat, y = Sales)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_classic() +
  scale_y_continuous(breaks = round(seq(min(-4000000), max(data1$Sales), by = 500000), 1), labels = comma) +
  scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1), 1))

# Remove the confidence interval
data1 %>%
  ggplot(aes(x = CustomerSat, y = Sales)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE) +
  theme_classic()+
  scale_y_continuous(breaks = round(seq(min(-4000000), max(data1$Sales), by = 500000), 1), labels = comma) +
  scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1), 1))

# Loess method
data1 %>%
  ggplot(aes(x = CustomerSat, y = Sales)) + 
  geom_point()+
  geom_smooth() +
  theme_classic()+
  scale_y_continuous(breaks = round(seq(min(-8000000), max(8000000), by = 1000000), 1), labels = comma) +
  scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1), 1))

# Save predicted values of Sales on CustomerSat (predicted values are represented for the CustomerSat), 
# returns fitted values for observed data [for time series data, use n.ahead argument to predict into future]
predict_SalesSat <- predict(lm(data1$Sales ~ data1$CustomerSat, data = data1))
predict_SalesSat

# Uses loess smoothing method instead of linear
predict_SalesSat2 <- predict(loess(data1$Sales ~ data1$CustomerSat, data = data1))
predict_SalesSat2

## Interpolation of Points

# Interpolate Sales for missing years of data
data1 %>%
  ggplot(aes(x = Year, y = Sales)) + 
  geom_point()+
  theme_classic()+
  scale_y_continuous(breaks = round(seq(min(-4000000), max(data1$Sales), by = 500000), 1), labels = comma) +
  scale_x_continuous(breaks = pretty(data1$Year, n = 10))

# Approximate linear values between points
approxData <- data.frame(
  with(data1, 
       approx(data1$Year, data1$Sales, method = "linear", n = 200)
  ),
  method = "approx()"
)
approxData

# Graph of orginal points with interpolated points
approxData %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point()+
  theme_classic()+
  scale_y_continuous(breaks = round(seq(min(-4000000), max(data1$Sales), by = 500000), 1), labels = comma) +
  scale_x_continuous(breaks = pretty(data1$Year, n = 10))

