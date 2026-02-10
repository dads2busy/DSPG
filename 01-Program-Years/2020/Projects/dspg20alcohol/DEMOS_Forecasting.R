#Load required Packages
##Copied from Andrew Maloney's Code
packages <- c("prophet","data.table", "dplyr")

for (pkgs in packages){
  if(!require(pkgs, character.only = TRUE)){ # Condition 
    install.packages(pkgs) # Install if not
    library(pkgs) # Load if installed
  }
}
df = fread('Iowa_Liquor_Sales.csv',select =c("Date"))
df1 = df %>% count(Date)
df1 = rename(df1, ds = Date, y = n)
df1$ds <- as.Date(df1$ds , format = "%m/%d/%Y")
df1 = transform(df1, day = wday(ds))
#df1<-df1[!(df1$day==1||df1$day==6||df1$day==7),]
df1<-df1[!(df1$day==6),]
df1<-df1[!(df1$day==7),]
df1<-df1[!(df1$day==1),]
m <- prophet(df1)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)

df2_1 = fread('2016-2018Iowa_Liquor_Sales(CollegeTowns) (2).csv',select =c("Date"))
df2_2 = fread('2019Iowa_Liquor_Sales(CollegeTowns).csv',select =c("Date"))
df2_bind = rbind(df2_1,df2_2)
df2 = df2_bind %>% count(Date)
df2 = rename(df2, ds = Date, y = n)
df2$ds <- as.Date(df2$ds , format = "%m/%d/%Y")
df2 = transform(df2, day = wday(ds))
df2$cap = 8000
m2 <- prophet(df2,growth = 'logistic')
future2 <- make_future_dataframe(m2, periods = 365)
future2$cap = 8000
tail(future2)
forecast2 <- predict(m2, future2)
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m2, forecast2)
prophet_plot_components(m2, forecast2)

dyplot.prophet(m2, forecast2)