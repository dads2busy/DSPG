# Install tidyverse R package
install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")
install.packages("psych")

# Loads the tidyverse R package
library(tidyverse)
library(stringr)
library(lubridate)
library(psych)

# Read in comma delimiated (.csv) "pig-pen1" dataframe
data1 <- read.csv(file.choose(), header = T, stringsAsFactors = FALSE)

# Coerces dataframe into a special type, 'tibble'
data1 <- as_tibble(data1)

# Reports back number of observations, variables, and a listing of variable types
glimpse(data1)
options(pillar.sigfig = 7)

# Removing duplicate rows
data1 <- data1 %>% distinct()
data1

# Looking at MonthlyCharges
data1$MonthlyCharges
is.na(data1$MonthlyCharges)

# Counting missing values
data1 %>%
  summarize(count = sum(is.na(MonthlyCharges)))

# Mutate missing values of Monthly Charges using median
data1 %>%
  mutate(MonthlyCharges
         = replace(MonthlyCharges,
                   is.na(MonthlyCharges),
                   median(MonthlyCharges, na.rm = TRUE)))

# Mutate missing values of Monthly Charges using median
data1 <- data1 %>%
  mutate(MonthlyCharges
         = replace(MonthlyCharges,
                   is.na(MonthlyCharges),
                   median(MonthlyCharges, na.rm = TRUE)))
data1

# Looking at missing values for Total Charges
data1$TotalCharges
is.na(data1$TotalCharges)

# Replacing with standard missing value type, NA for Total Charges
data1 <- data1 %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "na", NA)) %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges == "N/A", NA))
data1
is.na(data1$TotalCharges)

# See what data type Total Charges is
class(data1$TotalCharges)

# Changing to Total Charges to numeric type
data1$TotalCharges <- as.numeric(data1$TotalCharges)
class(data1$TotalCharges)

# Replace missing Total Charges values with median
data1 <- data1 %>%
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))
data1


# Looking at missing values for PaymentMethod
data1$PaymentMethod
is.na(data1$PaymentMethod)

# Replacing "--" and "" for Payment Method with NA
data1 <- data1 %>%
  mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod ==  "--", NA)) %>%
  mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod ==  "", NA))
data1

# Replace NA for Payment Method with "Unavailable"
data1 <- data1 %>%
  mutate(PaymentMethod = replace(PaymentMethod, is.na(PaymentMethod), "Unavailable"))
data1

# Remove extra spaces from Gender Variable
data1 <- data1 %>%
  mutate(Gender = str_trim(Gender))
data1

# Recodes Gender character values and coerces it to be a factor
data1 <- data1 %>%
  mutate(Gender = replace(Gender, Gender ==  "M", "Male")) %>%
  mutate(Gender = replace(Gender, Gender ==  "F", "Female"))
data1$Gender <- as.factor(data1$Gender)
data1

# Recodes dates into standardized format using two methods and 'merges' them together into original column
mdy1 <- mdy(data1$CustomerSince)
mdy1
ymd1 <- ymd(data1$CustomerSince)
ymd1
data1 <- data1 %>%
  mutate(CustomerSince = coalesce(mdy1, ymd1))
data1

# Check for outliers with summarize function
summary(data1)
hist(data1$NetworkPerf)
boxplot(data1$NetworkPerf)

# Use describe function in psych package to get descriptive statistics
describe(data1)
describe.by(data1, "Gender")


  
  
