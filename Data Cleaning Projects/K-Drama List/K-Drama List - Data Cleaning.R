#Data Loading
kdramalist1 <- read.csv("top100_kdrama_aug_2023.csv", header=TRUE, sep=',')
kdramalist2 <- read.csv("top100_kdrama.csv", header=TRUE, sep=',')
View(kdramalist2)

#1.Change Column Name
colnames(kdramalist1) <- c("Drama_Name","Release_Year","Aired_Date","Aired_Days",
                           "Episode","Aired_Network","Duration","Content_Rating",
                           "Synopsis","Cast","Genre","Tags","Drama_Rank",
                           "Drama_Rating")
colnames(kdramalist2) <- c("Drama_Name","Release_Year","Aired_Date","Aired_Days",
                           "Episode","Aired_Network","Duration","Content_Rating",
                           "Synopsis","Cast","Genre","Tags","Drama_Rank",
                           "Drama_Rating")

#2. Merge Data
kdramalist <- natural_join(kdramalist1, kdramalist2, by="Drama_Name", jointype="FULL")
View(kdramalist)
kdramalist_raw <- natural_join(kdramalist1, kdramalist2, by="Drama_Name", jointype="FULL") #created for comparison
View(kdramalist_raw)

#3.Data Cleaning and Transformation

#3.1 Replace Values in Columns and Cells
kdramalist[128,"Aired_Days"] <- "Friday" 
kdramalist$Content_Rating[kdramalist$Content_Rating == '18+ Restricted (violence & profanity) '] <- '18+ Restricted'
kdramalist$Drama_Rank <- as.numeric(gsub("#", "", kdramalist$Drama_Rank))
kdramalist <- kdramalist[, !names(kdramalist) %in% c("Synopsis")] #remove Synopsis column

#3.2 Remove Duplicates in Drama Name
duplicates <- kdramalist$Drama_Name[duplicated(kdramalist$Drama_Name)]
print(duplicates)
#-- found out few dramas with incomplete title --
kdramalist$Drama_Name <- replace(kdramalist$Drama_Name, c(64,65),
                                 c("Missing: The Other Side 2", "Missing: The Other Side"))
kdramalist <- kdramalist[!duplicated(kdramalist$Drama_Name), ] #remove the rest of duplicate dramas

#3.3 Adjust row list to ease value adjustment
row.names(kdramalist) <- NULL
row.names(kdramalist) <- 1:nrow(kdramalist)

#3.4 Find None Values and Replace
rows_with_none <- which(kdramalist$Aired_Network == "None", arr.ind = TRUE)
print(rows_with_none)
kdramalist[127,"Aired_Network"] <- "Netflix"

#3.5 Shorten the tags (max. 3)
tags_list <- strsplit(kdramalist$Tags, ", ")
shorten_tags <- sapply(tags_list, function(tags) paste(tags[1:3], collapse = ", "))
kdramalist$Tags <- shorten_tags


#3.6 Change duration value from character to numeric
kdramalist$Duration <- sapply(strsplit(kdramalist$Duration, "\\D+"), 
                                   function(x) {
                                     x <- as.numeric(x);
                                     if(length(x) == 2) return(60 * x[1] + x[2]) else return(x)
                                   })
colnames(kdramalist)[9] <- "Duration_Minute"

#3.7 Add column for drama types for categorization
kdramalist = mutate(kdramalist,
                    Drama_Category = case_when(
                      Episode >= 50 & Duration_Minute >= 30 ~ "Opera Drama",
                      Episode >= 11 & Episode <= 40 & Duration_Minute >= 30 ~ "Regular Drama",
                      Episode >= 6 & Episode <= 10 ~ "Netflix Original Drama",
                      TRUE ~ "Other"))


#3.8 Separate the Aired Date
kdramalist_date <- separate(kdramalist, Aired_Date, into=c("Aired_Start_Date", "Aired_End_Date"), sep=" - ")
kdramalist_date$Aired_Start_Date <- as.Date(kdramalist_date$Aired_Start_Date, format = "%b %d, %Y")
kdramalist_date$Aired_End_Date <- as.Date(kdramalist_date$Aired_End_Date, format = "%b %d, %Y")
View(kdramalist_date)

#3.9 Fill in Missing Data in Aired_End_Date

#3.9.1 Find out the Aired_Days of NA values in Aired_End_Date to missing data calculation purposes
na_indices_AED <- is.na(kdramalist_date$Aired_End_Date)
aired_days_na <- kdramalist_date$Aired_Days[na_indices_AED]
print(aired_days_na)

#3.9.2 Calculation of Aired_End_Date based on Aired_Start_Date and total of Aired_Days per week
weeks <- ceiling(kdramalist_date$Episode / 1) #all NA of Aired_End_Date only has 1 episode each week
missing_indices <- is.na(kdramalist_date$Aired_End_Date)
kdramalist_date$Aired_End_Date[missing_indices] <- kdramalist_date$Aired_Start_Date[missing_indices] + 
                                              (weeks[missing_indices] -1) * weeks() 

#3.10 Mutate the Aired_Start_Date and Aired_End_Date to the main tables
kdramalist <- mutate(kdramalist,
                      Aired_Start_Date = kdramalist_date$Aired_Start_Date,
                      Aired_End_Date = kdramalist_date$Aired_End_Date)
kdramalist <- kdramalist[, !names(kdramalist) %in% c("Aired_Date")]

#3.11 Finalize column order
kdramalist <- select(kdramalist, Drama_Name, Release_Year, Episode, Drama_Rating, Drama_Rank,
                     Duration_Minute, Cast, Genre, Aired_Start_Date, Aired_End_Date, Aired_Days,
                     Aired_Network, Drama_Category, Content_Rating, Tags)
View(kdramalist)
                      
#4. Download tables as Excel
write_xlsx(kdramalist, "K-Drama List.xlsx")
write_xlsx(kdramalist_raw, "K-Drama List (Raw).xlsx")




