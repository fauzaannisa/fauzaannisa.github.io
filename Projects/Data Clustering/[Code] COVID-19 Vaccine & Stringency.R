library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(factoextra)
library(corrplot)

##Numbering on specific comment indicates the sub-section on the coursework##

#2.1 COVID-19 Data Loading
covid <- read.csv("owid-covid-data.csv", header = TRUE)
View(sea)

#2.2 Extract date to separate day, month, year for visualisation purposes
covid = covid %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))
covid$MonthName <- month.abb[covid$month]

#Select ASEAN countries as main data sets
sea <- covid[covid$location %in% c("Indonesia","Malaysia","Singapore","Philippines",
                                   "Laos","Cambodia","Thailand","Vietnam", "Myanmar",
                                   "Brunei", "Timor"),]

#Select chosen variables to be analysed
sea1 <- sea %>% select(c('location','date','day','month','MonthName','year',
                         'new_cases_per_million','new_deaths_per_million',
                         'reproduction_rate','hospital_beds_per_thousand',
                         'people_fully_vaccinated_per_hundred','stringency_index',
                         'gdp_per_capita','population_density','human_development_index'))

#Discover the total of missing values
sum(is.na(sea1))

# Removes rows, calculates missing values with Backward Imputation method,
# and merge to the main dataset (sea1).
# Specific rows chosen from first vaccination date.

sea_imp <- sea1 %>% slice(c(413:846, 1242:1735, 2069:2589, 2977:3421, 3820:4314,
                            4684:5142, 5569:6028, 6378:6921, 7368:7834 ,8264:8667,
                            9132:9561)) %>%
                    fill(c(people_fully_vaccinated_per_hundred, new_cases_per_million,
                           new_deaths_per_million, reproduction_rate), .direction = "down") 

sea1 <- sea1[-c(413:846, 1242:1735, 2069:2589, 2977:3421, 3820:4314,
                4684:5142, 5569:6028, 6378:6921, 7368:7834,8264:8667,
                9132:9561),]

sea1 <- rbind(sea1, sea_imp) %>% arrange(location)

#Adjust the median of Stringency Index based on location
sea1 <- sea1 %>% group_by(location) %>% 
  mutate_at(vars(stringency_index), 
            ~replace_na(., 
                        median(., na.rm = TRUE)))

#Ensuring the median of Stringency Index in each country
aggregate(x= sea1$stringency_index, by = list(sea1$location), FUN = median)

# Fill in the rest of NA value with 0
sea1[is.na(sea1)] <- 0

#Outlier Removal with IQR Method
outliers_rmv <- quantile(sea1$stringency_index,
                        probs = c(.25, .75), na.rm = FALSE)
iqr <- IQR(sea1$stringency_index)
iqrup <- outliers_rmv[2]+1.5*iqr #upper interquartile
iqrlow <- outliers_rmv[2]-1.5*iqr #lower interquartile
sea_clean <- subset(sea1,
                    sea1$stringency_index>iqrlow &
                      sea1$stringency_index<iqrup)
View(sea_clean)

#2.3.1 Pearson Correlation between people fully vaccinated per hundred with stringency index
cor(sea_clean$people_fully_vaccinated_per_hundred, sea_clean$stringency_index,  method = "pearson")

#Correlation scatter plot between Vaccination and Stringency Index
ggscatter(sea_clean, x = "people_fully_vaccinated_per_hundred", y = "stringency_index", cor.method = "spearman",
          add = "reg.line", conf.int = TRUE, color = "location") + stat_cor(aes(color = location), label.x = 3)


#Data scaling for clustering purposes
sea_scale = scale(sea_clean[,7:15], center = TRUE, scale = TRUE)
sea_scale = as_tibble(sea_scale)


#2.3.2 PCA Analysis
sea_clean_pca = prcomp(sea_scale[,1:9])
summary(sea_clean_pca)
pca_transform = as.data.frame(-sea_clean_pca$x[,1:2])
View(sea_scale)

pca_var =  tibble(proportional_variance =  sea_clean_pca$sdev^2/sum(sea_clean_pca$sdev^2) , PC =paste0("PC", 1:9))
print(pca_var)

#Visualise the most significant variance
ggplot(pca_var, aes(x = 1:9, y = cumsum(proportional_variance))) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = 1:9, labels = pca_var$PC, name = "Principal Component SEA") +
  scale_y_continuous(name = "Cummulative Variance Explained",
                     breaks = seq.default(from = 0.6, to = 1, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  labs(caption = "Fig. 1 Explaining dataset variance using PCA")

#adding the PC1 and PC2 to the main data frame (sea_clean)
sea_scale[,c("PC1", "PC2")] = sea_clean_pca$x[,1:2]
pca_transform$location = sea_clean$location #adding location for plotting

ggplot(sea_scale, aes(x = PC1, y = PC2, color = location)) +
  geom_point(size = 2)  + 
  labs(title = "Visualisation of Principal Component Analysis",
       subtitle = "Plotted by location",
       colour = "Country") +
  theme(text=element_text(size=20)) 

#2.3.3 Silhouette Analysis
pca_transform <- subset(pca_transform, select = -c(location))
set.seed(123)

#Running the optimal clusters of k
fviz_nbclust(pca_transform, kmeans, method = "silhouette")

#2.3.4 K-MEANS Clustering
sea_scale <- subset(sea_scale, select = -c(location)) #removing the character attributes
elbow1 <- kmeans(pca_transform, centers = 2, nstart = 20)
fviz_cluster(elbow1, geom = "point", data = pca_transform) + 
              ggtitle("Result of K-MEANS Clustering") +
              theme(text=element_text(size=20)) 

#Adding and transforming cluster variable to the main data frame
sea_scale$location = sea_clean$location #adding back the location variable
sea_clean[,c("PC1", "PC2")] = sea_clean_pca$x[,1:2]
sea_clean$Cluster = elbow1$cluster
sea_clean = mutate(sea_clean,
                   Cluster_name = case_when(Cluster == 1 ~ "One",Cluster == 2 ~ "Two"))

#2.3.5 Pearson Correlation Matrix
seacorr <- sea_clean[,7:17]
seacorr1 <- cor(seacorr)
corrplot(seacorr1, method="number", number.cex = 0.7)

#3.4 Supported visualisation for discussion
#Filtering the cluster
seaclu1 <- filter(sea_clean, Cluster_name=="One")
seaclu2 <- filter(sea_clean, Cluster_name=="Two")

#Summarise each cluster
sea_clu_mean1 <- seaclu1 %>%
  group_by(MonthName, year) %>% summarise(new_cases_per_million = max(new_cases_per_million),
                                          new_deaths_per_million = max(new_deaths_per_million),
                                         people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred),
                                         stringency_index = max(stringency_index), .groups = 'drop') %>%
                                subset(year<2022)

View(sea_clu_mean2)
sea_clu_mean2 <- seaclu2 %>%
  group_by(MonthName,year) %>% summarise(new_cases_per_million = max(new_cases_per_million),
                                         new_deaths_per_million = max(new_deaths_per_million),
                                         people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred),
                                         stringency_index = max(stringency_index), .groups = 'drop') %>%
                                 subset(year<2022)

#add unavailable data to fill in the visualisation
sea_clu_mean1 <- sea_clu_mean1 %>% add_row(MonthName= 'Jan',
                                             year=2020,
                                             new_cases_per_million=0,
                                             new_deaths_per_million=0,
                                             people_fully_vaccinated_per_hundred=0,
                                             stringency_index=0) 

sea_clu_mean2 <- sea_clu_mean2 %>% add_row(MonthName= c('Jan','Feb'),
                                           year=c(2020,2020),
                                           new_cases_per_million=c(0,0),
                                           new_deaths_per_million=c(0,0),
                                           people_fully_vaccinated_per_hundred=c(0,0),
                                           stringency_index=c(0,0)) 


#Cluster 1 visualisation: Fully vaccinated people and stringency index
ggplot(sea_clu_mean1, aes(x=fct_inorder(MonthName), group = 1)) + 
    geom_line(aes(y = people_fully_vaccinated_per_hundred), color = '#8c1a0f', linewidth=1, show.legend =  TRUE) + 
    geom_line(aes(y = stringency_index), color='#0c9957', linetype="longdash", linewidth=1, show.legend = TRUE) +
    theme(legend.position="bottom")  +
    facet_grid(.~year) +
    scale_x_discrete(limits = month.abb) +
    scale_y_continuous("People Fully Vaccinated (per hundred)", 
        sec.axis = sec_axis(~ . * 1, name = "Stringency Index")) +
    labs(title = "Influence of Vaccination Deployment to Stringency Index",
        subtitle = "in Cluster 1 countries",
        x = "Month",
        color = "Legend") +
    theme(text=element_text(size=20)) 

#Cluster 2 visualisation: Fully vaccinated people and stringency index
ggplot(sea_clu_mean2, aes(x=fct_inorder(MonthName), group = 1)) + 
    geom_line(aes(y = people_fully_vaccinated_per_hundred), color = '#8c1a0f', linewidth=1, show.legend =  TRUE) + 
    geom_line(aes(y = stringency_index), color='#0c9957', linetype="longdash", linewidth=1, show.legend = TRUE) +
    theme(legend.position="bottom")  +
    facet_grid(.~year) +
    scale_x_discrete(limits = month.abb) +
    scale_y_continuous("People Fully Vaccinated (per hundred)", 
         sec.axis = sec_axis(~ . * 1, name = "Stringency Index")) +
    labs(title = "Influence of Vaccination Deployment to Stringency Index",
         subtitle = "in Cluster 2 countries",
         x = "Month",
         color = "Legend") +
    theme(text=element_text(size=20)) 
