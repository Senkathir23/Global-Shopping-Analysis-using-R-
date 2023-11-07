#install.packages("readxl")
#install.packages("ggplot") 
#install.packages("treemap")
#install.packages("ggplot2")
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("patchwork")
#install.packages("gridExtra")  
#install.packages("factoextra")
#install.packages("reticulate")
#install.packages("hrbrthemes")
#install.packages("FactoMineR")
#install.packages("highcharter")
#install.packages("countrycode")

library(plyr)
library(dplyr)
library(plotly)
library(readxl)
library(ggplot)
library(stringr)
library(readxl)
library(treemap)
library(ggplot2)
library("lattice")
library(lubridate)
library(tidyverse)
library(factoextra)
library(ggthemes)
library(hrbrthemes)
library(highcharter) 
library(hrbrthemes)
library(FactoMineR)
library(countrycode)

#Importing the Dataset
retail <- read_excel("Online Retail.xlsx")
View(retail)

#No of rows and Column
sprintf("There are %.0f rows and %.0f columns in our dataset",nrow(retail),ncol(retail))

#Data Type
str(retail)

#Summary of the dataset
summary(retail)
boxplot(retail$Quantity, retail$UnitPrice, col = "#ACAC89")

#Top 5 row
head(retail,5)

#Bottom 5 row
tail(retail,5)

#Total customers
n_distinct(retail$CustomerID)

#Total Products
n_distinct(retail$Description)

#Finding Na Values
colSums(is.na(retail))

#Omiting the NA row of Custumer ID
retail = retail %>% na.omit(retail$CustomerID)

#Replacing the NA values with empty
retail$Description <- replace_na(retail$Description, "empty")

colSums(is.na(retail))

#quantities that are negative adn sorting
quantity_check = retail %>% filter(Quantity < 0) %>% arrange(Quantity)

head(quantity_check)
nrow(quantity_check)

#highest negative value
retail %>% filter(CustomerID == 16446)

# deleting the outliers by Invoice Number, InvoiceNo of top most negavtive values
retail = retail[!(retail$InvoiceNo == 581483 | retail$InvoiceNo == 541431),]

# filtering our data to have only positive Quantities
retail = retail %>%  filter(Quantity > 0) %>%  filter(UnitPrice >0)

#Checking the outliers
summary(retail)
boxplot(retail$Quantity, retail$UnitPrice, col = "#ACAC89")

#COuntry
unique(retail$Country)
#vars(retail$Spent,retail$Quantity)

#New column total amount
retail = mutate(retail, Spent = Quantity * UnitPrice)
View(retail)

# new date and time columns extracted from InvoiceDate column
retail$InvoiceDate <- as.character(retail$InvoiceDate)
retail$date <- sapply(retail$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
retail$time <- sapply(retail$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})

# new month, year and hour columns
retail$year <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][1]})
retail$month <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][2]})
retail$hour <- sapply(retail$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})

# turning date feature to date type
retail$date <- as.Date(retail$date, "%Y-%m-%d")

# creating day of the week feature
retail$day_week <- wday(retail$date, label = TRUE)

# setting up a frame with unique descriptions for further exploration of products offered
products_list <- unique(retail$Description)
##################################################

head(retail,2)
View(retail)

#Based on COuntry 
country_invoice = retail %>% group_by(Country)  %>% dplyr::summarise(n = n()) 
ggplot(data=country_invoice, aes(x=Country, y=n)) +
  geom_bar(stat="identity", color="blue", fill="white") + coord_flip()

#Based on COuntry without uk
country_invoice = retail %>% group_by(Country) %>% filter(Country != "United Kingdom") %>% dplyr::summarise(n = n()) 
ggplot(data=country_invoice, aes(x=Country, y=n)) +
  geom_bar(stat="identity", color="blue", fill="white") + coord_flip()

#Heat Map - based on revenue
retail_country_top5 = retail %>%
  group_by(Country)  %>% 
  filter(Country != "United Kingdom") %>% 
  dplyr::summarise(revenue = sum(Spent), transactions = n_distinct(InvoiceNo)) %>% arrange(desc(revenue))

retail_country_top5 
retail_country_top5 = head(retail_country_top5,5)
treemap(retail_country_top5,
        index="Country",
        vSize="revenue",
        vColor="revenue",type="value",palette="RdBu",title="Top 5 Country based on Revenue")

#Revenue Density

top_countries_filter <- retail %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' 
         | Country == 'Australia')

top_5 <- top_countries_filter %>%
  group_by(Country, date) %>%
  dplyr::summarise(revenue = sum(Spent), transactions = n_distinct(InvoiceNo), 
                   customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

top_5 %>% 
  group_by(Country) %>%
  dplyr::summarise(revenue = sum(revenue))

ggplot(top_5, aes(x = date, y = revenue, colour = Country)) + geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = ' Country', y = 'Revenue', title = 'Revenue by Country over Time') + 
  theme(panel.grid.major = element_line(colour = NA),
        legend.text = element_text(colour = "skyblue4"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "seashell1",
                                       colour = NA), legend.key = element_rect(fill = "gray71"),
        legend.background = element_rect(fill = NA))



#day week revenue
retail %>%
  group_by(day_week) %>%
  dplyr::summarise(revenue = sum(Spent)) %>%
  hchart(type = 'column', hcaes(x = day_week, y = revenue)) %>% 
  hc_yAxis(title = list(text = "Revenue")) %>%  
  hc_xAxis(title = list(text = "Day of the Week")) %>% 
  hc_title(text = "Revenue by Day of Week")

#worls
map_info <- retail %>% 
  group_by(Country) %>% 
  dplyr::summarise(revenue = sum(Spent))
retai1 <- summarise_at(group_by(retail,CustomerID,Country), vars(Spent,Quantity), funs(sum(.,na.rm = TRUE)))

highchart(type = "map") %>%
  hc_add_series_map(worldgeojson,
                    map_info %>% 
                      bind_cols(as_tibble(map_info$revenue)) %>% 
                      group_by(map_info$Country) %>% 
                      dplyr::summarise(revenue = log1p(sum(value))) %>% 
                      ungroup() %>% 
                      mutate(iso2 = countrycode(sourcevar = map_info$Country, 
                                                origin="country.name", destination="iso2c")),
                    value = "revenue", joinBy = "iso2") %>%
  hc_title(text = "Revenue by country (log)") %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "{point.map_info$Country}") %>% 
  hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)))

#####
#last purchased date
recency <- retail %>% 
  dplyr::select(CustomerID, InvoiceDate) %>% 
  mutate(recency = as.Date("2011-12-09") - as.Date(InvoiceDate))

recency <- recency %>% 
  dplyr::select(CustomerID, recency) %>% 
  group_by(CustomerID) %>% 
  slice(which.min(recency))

head(recency,10)


#frequency of the customer
amount_products <- retail %>%
  dplyr::select(CustomerID, InvoiceDate) %>% 
  group_by(CustomerID, InvoiceDate) %>% 
  summarize(n_prod = n())
amount_products

df_frequency <- amount_products %>% 
  dplyr::select(CustomerID) %>%
  group_by(CustomerID) %>% 
  summarize(frequency = n())

head(df_frequency,3)

#spent
monetary <- select(retai1, c("CustomerID", "Spent"))
head(monetary,3)

# inner join the three RFM data frames by CustomerID
rfm <- recency %>% 
  dplyr::inner_join(., df_frequency, by = "CustomerID") %>% 
  dplyr::inner_join(., monetary, by = "CustomerID")
head(rfm)

# drop the days from recency column and transform it into numeric data type
rfm <- rfm %>% 
  mutate(recency = str_replace(recency, " days", "")) %>% 
  mutate(recency = as.numeric(recency)) %>% 
  ungroup()

head(rfm, 3)

rfm_clean <- select(rfm, -CustomerID)

head(rfm_clean)
#kmeans
# scaling
rfm_norm <- scale(rfm_clean)
summary(rfm_norm)

#elbow
#fviz_nbclust(rfm_norm, kmeans, method="wss")+geom_vline(xintercept=3,linetype=2) + labs(subtitle = "Elbow method")

wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(rfm_norm, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fancy kmeans
kmeans_fancy <- kmeans(rfm_norm, 3)

# plot the clusters
f = fviz_cluster(kmeans_fancy, data = rfm_norm, geom = c("point"),ellipse.type = "convex",palette = "Set1", ggtheme = theme_minimal())
ggplotly(f)
f

kmeans = kmeans(x = rfm_norm, centers = 3)
y_kmeans = kmeans$cluster
library(cluster)
clusplot(rfm_norm,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('K MEANS'))


