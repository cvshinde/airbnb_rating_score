#load libraries
library(tidyverse)
library(caret)
library(dplyr)

train_x<-read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")
test_y <- read_csv("airbnb_test_y_2023.csv")
#join the full_dataing y to the full_dataing x file
#also turn the target variables into factors
train_xy <- cbind(train_x, train_y)
test_xy <- cbind(test_x, test_y)
full_data<- rbind(train_xy, test_xy)

#####BASELINE CLASSIFIER

# Create a table of counts
counts <- table(train_y$perfect_rating_score)

# Print the counts
print(counts)

# Check which value occurs more frequently
if (counts["YES"] > counts["NO"]) {
  print("YES occurs more frequently.")
} else if (counts["YES"] < counts["NO"]) {
  print("NO occurs more frequently.")
} else {
  print("YES and NO have the same frequency.")
}


baseline_preds = rep("NO", nrow(train_y)) 

# Evaluate the baseline "model"
baseline_correct = ifelse(baseline_preds == train_y$perfect_rating_score, 1, 0)
baseline_accuracy =  sum(baseline_correct)/length(baseline_correct)


#####EXTERNAL DATASOURCE

external <- read_csv("census.csv")
external <- unite(external, race_gender_min_max_age, race, sex, minAge, maxAge, sep = "_")


external_wide <- pivot_wider(external, names_from = c(race_gender_min_max_age), values_from = population)
external_wide 

external_wide_cleaned <- external_wide %>%
  mutate(across(.cols = everything(), ~ifelse(is.na(.), 0, .)))


external_wide_cleaned  <- external_wide_cleaned %>%
group_by(zipcode) %>%
  summarise(across(everything(), sum))


external_wide_cleaned$zipcode <- as.character(external_wide_cleaned$zipcode)
full_data <- right_join(external_wide_cleaned,full_data, by='zipcode', multiple='all')

column_names <- colnames(full_data)
print(column_names)

target_cols <- c("WHITE ALONE_Female_15_17", "SOME OTHER RACE ALONE_Female_15_17", "SOME OTHER RACE ALONE_Female_65_66", "ASIAN ALONE_Female_60_61", "ASIAN ALONE_Male_10_14", "SOME OTHER RACE ALONE_Female_25_29", "NA_Female_67_69", "TWO OR MORE RACES_Female_75_79", "NA_Male_21_21", "NA_Female_80_84", "NA_Female_5_9", "AMERICAN INDIAN AND ALASKA NATIVE ALONE_Male_45_49", "TWO OR MORE RACES_Female_NA_5", "BLACK OR AFRICAN AMERICAN ALONE_Male_35_39", "BLACK OR AFRICAN AMERICAN ALONE_Male_45_49", "ASIAN ALONE_Female_15_17", "WHITE ALONE_Male_15_17", "TWO OR MORE RACES_Female_15_17", "WHITE ALONE_Male_22_24", "TWO OR MORE RACES_Male_30_34", "BLACK OR AFRICAN AMERICAN ALONE_Female_75_79", "AMERICAN INDIAN AND ALASKA NATIVE ALONE_Male_25_29", "SOME OTHER RACE ALONE_Female_85_NA", "WHITE ALONE_Male_75_79", "BLACK OR AFRICAN AMERICAN ALONE_Male_NA_5", "BLACK OR AFRICAN AMERICAN ALONE_NA_NA_NA", "WHITE ALONE_Male_18_19", "ASIAN ALONE_Female_20_20", "AMERICAN INDIAN AND ALASKA NATIVE ALONE_Male_35_39", "AMERICAN INDIAN AND ALASKA NATIVE ALONE_Male_67_69", "NA_Male_85_NA", "ASIAN ALONE_Female_67_69", "WHITE ALONE_Female_55_59", "TWO OR MORE RACES_NA_NA_NA", "TWO OR MORE RACES_Female_60_61", "NA_Female_85_NA", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE_Female_67_69", "WHITE ALONE_Male_21_21", "AMERICAN INDIAN AND ALASKA NATIVE ALONE_Female_NA_NA", "TWO OR MORE RACES_Male_22_24")

full_data[target_cols][is.na(full_data[target_cols])] <- 0

female_cols <- target_cols[grepl("Female", target_cols)]
full_data$female_population <- rowSums(full_data[, female_cols], na.rm = TRUE)

male_cols <- target_cols[grepl("Male", target_cols)]
full_data$male_population <- rowSums(full_data[, male_cols], na.rm = TRUE)

# Min-Max Normalization
full_data$normalized_male <- (full_data$male_population - min(full_data$male_population)) / (max(full_data$male_population) - min(full_data$male_population))
full_data$normalized_female <- (full_data$female_population - min(full_data$female_population)) / (max(full_data$female_population) - min(full_data$female_population))

#CLEANING full_data 

full_data$cleaning_fee = ifelse(is.na(full_data$cleaning_fee), 0, full_data$cleaning_fee)
full_data$beds= ifelse(is.na(full_data$beds), mean(full_data$beds, na.rm=TRUE), full_data$beds)
full_data$bedrooms = ifelse(is.na(full_data$bedrooms), median(full_data$bedrooms, na.rm=TRUE), full_data$bedrooms)
full_data$bathrooms = ifelse(is.na(full_data$bathrooms), median(full_data$bathrooms, na.rm=TRUE), full_data$bathrooms)

full_data$cancellation_policy = ifelse(full_data$cancellation_policy %in% c("strict", "super_strict_30","super_strict_60"), "strict", full_data$cancellation_policy)
full_data$cancellation_policy = as.factor(full_data$cancellation_policy)

full_data$bed_category <- ifelse(full_data$bed_type == 'Real Bed' , 'bed', 'other')
full_data$bed_category= as.factor(full_data$bed_category)

library(readr)
full_data$cleaning_fee <- parse_number(full_data$cleaning_fee)

full_data$price <- parse_number(full_data$price)

full_data["cleaning_fee"][is.na(full_data["cleaning_fee"])] <- 0

full_data$has_cleaning_fee <- ifelse(full_data$cleaning_fee > 0 , 'YES', 'NO')
full_data$has_cleaning_fee <- factor(full_data$has_cleaning_fee)

full_data$extra_people = as.numeric(gsub("\\$", "", full_data$extra_people))
full_data$charges_for_extra <- ifelse(is.na(full_data$extra_people) | full_data$extra_people == 0, "NO", "YES")
full_data$charges_for_extra <- as.factor(full_data$charges_for_extra)

# Amenities cleaning
full_data$amenities = str_replace_all(full_data$amenities, '[\\"{}]', '')
full_data$amenities = strsplit(full_data$amenities, ",")

#install.packages("zoo")
library(tidyverse)
library(dplyr)
library(tidyr)
library(zoo)
#AQ) latitude
full_data$latitude <- na.approx(full_data$latitude, na.rm = FALSE)
#AS) latitude
full_data$latitude <- na.approx(full_data$latitude, na.rm = FALSE)

#AS) market 
table_market <- table(full_data$market)
full_data$market <- ifelse(full_data$market %in% names(table_market[table_market < 300]), "OTHER", full_data$market)
full_data$market<- ifelse(is.na(full_data$market), "OTHER", full_data$market)
full_data$market <- factor(full_data$market)

#AW Monthly price and BB Price
full_data$price <- gsub("\\$", "", full_data$price)

full_data$price <- as.numeric(full_data$price)
#full_data$monthly_price[is.na(full_data$monthly_price)] <- full_data$price[is.na(full_data$monthly_price)]* 30
full_data$monthly_price <- ifelse(is.na(full_data$monthly_price), full_data$price * 30, full_data$monthly_price)
full_data$monthly_price <- gsub("\\$", "", full_data$monthly_price)
full_data$monthly_price <- as.numeric(gsub("[,]", "", full_data$monthly_price))

# BC Property type
unique(full_data$property_type)
full_data$property_type <- as.factor(ifelse(full_data$property_type %in% c("Apartment", "Serviced apartment" , "Loft"), 'apartment',
                                          ifelse(full_data$property_type %in% c("Bed & Breakfast", "Boutique hotel" ,"Hostel"), 'hotel',
                                                 ifelse(full_data$property_type %in% c( 'Bungalow' ,'House'), 'house',
                                                        ifelse(full_data$property_type%in% c( 'Townhouse', 'Condominium'), 'condo','other')))))

# AR License
class(full_data$license)
full_data$license <- ifelse(grepl("pending", tolower(full_data$license)), "pending",
                          ifelse(full_data$license != "" & full_data$license != "NA", "Has license", "Unknown"))
#full_data$license <- as.factor(ifelse(is.na(full_data$license), "unknown", full_data$license))

full_data$license <- replace(full_data$license , is.na(full_data$license ), "unknown")
full_data$license <- as.factor(full_data$license )
summary(full_data$license)
sum(is.na(full_data$city_name))
sum(is.na(full_data$availability_30))
sum(is.na(full_data$availability_60))
sum(is.na(full_data$availability_90))
sum(is.na(full_data$availability_365))
sum(is.na(full_data$host_since))
sum(is.na(full_data$host_total_listings_count))
sum(is.na(full_data$instant_bookable))
sum(is.na(full_data$is_business_travel_ready))
sum(is.na(full_data$is_location_exact))

#full_data$host_since <- as.Date(full_data$host_since)
#date_mean <- mean(full_data$host_since, na.rm = TRUE)
#full_data$host_since[is.na(full_data$host_since)] <- date_mean


host_total_listings_count_mean <- mean(full_data$host_total_listings_count, na.rm = TRUE)
full_data$host_total_listings_count[is.na(full_data$host_total_listings_count)] <- host_total_listings_count_mean

full_data$instant_bookable <- ifelse(full_data$instant_bookable == "TRUE", 1, 0)
full_data$is_location_exact <- ifelse(full_data$is_location_exact == "TRUE", 1, 0)
full_data$is_business_travel_ready <- ifelse(full_data$is_business_travel_ready == "TRUE", 1, 0)

full_data$is_business_travel_ready <- ifelse(is.na(full_data$is_business_travel_ready), "MISSING", full_data$is_business_travel_ready)


# Replace NA values with "missing"
full_data$host_response <- ifelse(is.na(full_data$host_response_rate), "MISSING",ifelse(full_data$host_response_rate == "100%", "ALL", "SOME"))
full_data$host_response <- as.factor(full_data$host_response)

full_data$host_acceptance <- ifelse(is.na(full_data$host_acceptance_rate), "MISSING",ifelse(full_data$host_acceptance_rate == "100%", "ALL", "SOME"))
full_data$host_acceptance <- as.factor(full_data$host_acceptance)

# Replace NAs in host_is_superhost with FALSE
full_data$host_is_superhost <- ifelse(is.na(full_data$host_is_superhost), FALSE, full_data$host_is_superhost)
full_data$host_is_superhost  <- ifelse(full_data$host_is_superhost  == "TRUE", 1, 0)

#Replace F,T to 0,1
full_data$host_identity_verified <- ifelse(is.na(full_data$host_identity_verified), FALSE, full_data$host_identity_verified)
full_data$host_identity_verified <- ifelse(full_data$host_identity_verified == "TRUE", 1, 0)

# Calculate the mean of host_total_listings_count
mean_host_total_listings_count <- mean(full_data$host_total_listings_count, na.rm = TRUE)
# Replace NA values with the mean
full_data$host_total_listings_count[is.na(full_data$host_total_listings_count)] <- mean_host_total_listings_count

# Convert zipcode column to character
full_data$zipcode <- as.character(full_data$zipcode)

# Remove decimal part from 5-digit zip codes
full_data$zipcode <- gsub("\\.0$", "", full_data$zipcode)

# Add a trailing zero to 4-digit zip codes
full_data$zipcode <- ifelse(nchar(full_data$zipcode) == 4, paste0(full_data$zipcode, "0"), full_data$zipcode)

# Cut the last 3 digits from 8-digit zip codes
full_data$zipcode <- ifelse(nchar(full_data$zipcode) == 8, substr(full_data$zipcode, 1, 5), full_data$zipcode)

# Print the modified zip code data
print(full_data$zipcode)

# Convert security_deposit and price into numbers
full_data$security_deposit <- as.numeric(gsub("\\$|,","",full_data$security_deposit))
full_data$price <- as.numeric(gsub("\\$|,","",full_data$price))

# Calculate the mean of non-NA values in the security_deposit column
deposit_mean <- mean(full_data$security_deposit, na.rm = TRUE)
# Replace NA values with the mean
full_data$security_deposit[is.na(full_data$security_deposit)] <- deposit_mean

# Convert cleaning_fee and price into numbers
full_data$cleaning_fee <- as.numeric(gsub("\\$|,","",full_data$cleaning_fee))
# Replace NAs in cleaning_fee and price with 0

library(dplyr)
full_data<- mutate(full_data, cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee))

#chechking if there is any missing values for require_guest_phone_verification
sum(is.na(full_data$require_guest_phone_verification))
sum(is.na(full_data$price))
sum(is.na(full_data$require_guest_profile_picture))
sum(is.na(full_data$requires_license))
sum(is.na(full_data$room_type))
sum(is.na(full_data$security_deposit))
sum(is.na(full_data$property_type))
sum(is.na(full_data$weekly_price))


# Check if price column has any dollar signs
grep("\\$", full_data$price)

# replacing true false with 1 and 0 in require_guest_phone_verification
full_data$require_guest_phone_verification <- ifelse(full_data$require_guest_phone_verification == "TRUE", 1, 0)


#making another variable price per person
full_data<-full_data%>%
  mutate (full_data,
          price_per_person = price/accommodates)


#making another variable ppp_ind
full_data$ppp_ind <- ifelse(full_data$price_per_person > median(full_data$price_per_person, na.rm = TRUE), 1, 0)
full_data$ppp_ind <- as.factor(full_data$ppp_ind)

full_data <- full_data %>%
  mutate(bedrooms_accommodates = bedrooms * accommodates,
         bedrooms_bathrooms_ratio= ifelse(bathrooms == 0, 0, bedrooms/bathrooms),
         beds_per_bedrooms= ifelse(bedrooms == 0, 0, beds/bedrooms),
         beds_per_accomodates= beds/ accommodates,
         bathrooms_per_accomodates= bathrooms/accommodates)

#sum(is.na(full_data$amenities_count))
sum(is.na(full_data$ppp_ind ))
sum(is.na(full_data$price_per_person))

#making another variable has_min_nights
full_data$has_min_nights <- as.factor(ifelse(full_data$minimum_nights > 1, "YES", "NO"))

# Calculate the average availability for different time frames
full_data$avg_availability_30 <- rowMeans(full_data[, c("availability_30", "availability_60", "availability_90")])
full_data$avg_availability_365 <- rowMeans(full_data[, c("availability_365", "availability_60", "availability_90")])

# Create a new column for the region
# Create a new column for the region
full_data$region <- ifelse(full_data$state %in% c("CT", "ME", "MA", "NH", "RI", "VT"), "New England",
                           ifelse(full_data$state %in% c("NJ", "NY", "PA"), "Mid-Atlantic",
                                  ifelse(full_data$state %in% c("IL", "IN", "MI", "OH", "WI"), "Midwest",
                                         ifelse(full_data$state %in% c("IA", "KS", "MN", "MO", "NE", "ND", "SD"), "Great Plains",
                                                ifelse(full_data$state %in% c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV"), "Southeast",
                                                       ifelse(full_data$state %in% c("AL", "KY", "MS", "TN"), "South Central",
                                                              ifelse(full_data$state %in% c("AR", "LA", "OK", "TX"), "Southwest",
                                                                     ifelse(full_data$state %in% c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"), "Mountain",
                                                                            ifelse(full_data$state %in% c("AK", "CA", "HI", "OR", "WA"), "West",
                                                                                   "Other"
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
)    
                        
# Create new variables indicating presence of specific keywords
full_data$has_bus <- ifelse(grepl("bus", full_data$transit, ignore.case = TRUE),"bus","")
full_data$has_train <- ifelse(grepl("train|subway|metro", full_data$transit, ignore.case = TRUE),"train","")
full_data$has_uber_lyft <- ifelse(grepl("uber|lyft", full_data$transit, ignore.case = TRUE),"uber","")
full_data$has_bike_share <- ifelse(grepl("bike share", full_data$transit, ignore.case = TRUE),"bike","")
full_data$has_streetcar <- ifelse(grepl("streetcar", full_data$transit, ignore.case = TRUE),"streetcare","")

full_data$combined_transit <- paste(
  full_data$has_bus,
  full_data$has_train,
  full_data$has_uber_lyft,
  full_data$has_bike_share,
  full_data$has_streetcar,
  sep = ", "
)


full_data$house_rules_pet <- grepl("pets", full_data$house_rules, ignore.case = TRUE)
full_data$house_rules_smoking <- grepl("smoking", full_data$house_rules, ignore.case = TRUE)
full_data$house_rules_parties <- grepl("parties", full_data$house_rules, ignore.case = TRUE)

full_data$host_name_frequency <- ave(seq(nrow(full_data)), full_data$host_name, FUN = length)

sum(is.na(full_data$has_bus))
sum(is.na(full_data$has_train))
sum(is.na(full_data$host_has_profile_pic))
sum(is.na(full_data$region))
sum(is.na(full_data$house_rules_parties))
sum(is.na(full_data$combined_transit))
sum(is.na(full_data$zipcode))

full_data$host_has_profile_pic[is.na(full_data$host_has_profile_pic)] <- "Unknown"

###library(quanteda)
library(quanteda.textmodels)
library(readr)
library(dplyr)
library(tokenizers)
library(tm)
library(quanteda)
library(tidyverse)
library(tm)
library(text2vec)
library(SnowballC)
library(glmnet)
library(vip)

# Define a tokenizer function
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords()) %>% #remove stopwords
    #stemDocument %>%
    word_tokenizer 
}

#install.packages("quanteda")
#library(quanteda)

# Tokenize the descriptions in full_data
it_full = itoken(full_data$description, 
                 preprocessor = tolower, 
                 tokenizer = cleaning_tokenizer, 
                 ids = full_data$ID, 
                 progressbar = FALSE)

# Create the vocabulary from the tokenized itoken object
vocab = create_vocabulary(it_full)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)

# Create a vectorizer object using the vocabulary we learned
vectorizer = vocab_vectorizer(vocab_small)

# Convert the documents into a DTM and make it a binary BOW matrix
dtm_full = create_dtm(it_full, vectorizer)
dtm_full_bin <- dtm_full>0+0

library(tidytext)

# Load the AFINN sentiment lexicon
data("afinn")

# Load the AFINN lexicon
afinn <- get_sentiments("afinn")
# Get the sentiment dictionary
afinn_positive <- afinn %>%
  filter(value > 0) %>%
  select(word)

afinn_negative <- afinn %>%
  filter(value < 0) %>%
  select(word)

install.packages("quanteda")
library(quanteda)

afinn_dict <- dictionary(list(negative = afinn_negative$word, positive = afinn_positive$word))

# Compare the words to the sentiment lists and count them
sentiments <- dfm_lookup(as.dfm(dtm_full_bin), afinn_dict)


sentiments <- convert(sentiments, to = "data.frame") %>%
  mutate(sent_score = as.factor(ifelse(positive > negative, 'P', 'N')))

# Count the number of Negative scores in the sentiments data frame
neg_count <- table(sentiments$sent_score)['N']

##

full_data <- cbind(sentiments, full_data)

##DATA VISUALIZATION

install.packages("ggplot2")
library(ggplot2)
#chart 1
chart1 <- ggplot(full_data, aes(x = factor(room_type), fill = factor(room_type))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "Black") + labs(title="Number of listing each Room Type", y="Number of Listing", x="Room Type")
print(chart1)

library(dplyr)

#chart 2
chart2 <- map_data("usa")
ggplot() +
  geom_polygon(data = chart2, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = airbnb_df_3, aes(x = longitude, y = latitude), color = price, size =10) +
  labs(x = "Longitude", y = "Latitude", title = "Airbnb Listings in the USA") +
  scale_color_gradientn(colors = terrain.colors(10))+
  theme_bw()

#chart 3
airbnb_df_1 <- full_data %>% group_by(room_type) %>% summarise( avg_price = mean(price))

chart3 <- ggplot(aes(x = room_type, y = avg_price, fill = room_type), data = airbnb_df_1) + geom_bar(stat = "identity") + labs(title="Average price for each Room Type", y="Average Price ($)", x="Room Type") + 
  geom_text(aes(label = sprintf("%0.2f", round(avg_price, digits = 2))), vjust = 0) + scale_fill_manual(values=c("#c63287","#d44c9c","#e366b1"))

print(chart3)

#Chart 4

airbnb_df_2 <- full_data 

chart4 <- ggplot(airbnb_df_2, aes(as.factor(host_is_superhost), availability_365, fill = as.factor(host_is_superhost))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Availability of rooms vs. Host Status", y = "Availability out of 365 days", x = "Is host a Superhost?") +
  scale_fill_manual(values = c("#94C973", "#FF0000"), labels = c("Not Superhost", "Superhost"))

print(chart4)

#Chart5
airbnb_df_3 <- full_data %>%
  filter(price < 500)

# Creating a violin plot to showcase density and distribution of prices for each neighborhood group
chart5 <- ggplot(airbnb_df_3, aes(x = city_name, y = price, fill = city_name)) +
  geom_violin() +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Density and Distribution of Prices for Each Neighborhood Group",
       x = "Neighborhood Group",
       y = "Price") +
  theme_bw()

print(chart5)

#chart 6 
ggplot(data = airbnb_df_3, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(colour = city_name), binwidth = 500, size = 1.25)

#chart 7
airbnb_df_v7 <- full_data %>% filter(bookings < 1000)


chart7 <- ggplot(data = airbnb_df_v7) + geom_point(mapping = aes(x = new_host_response_rate, y = bookings)) + labs(title="Host Response Rate vs. Bookings", y="Bookings", x="Host Response Rate (%)")

print(chart7)

#full_data_new <- full_data[1:nrow(full_data_x), ]  # Rows 1 to nrow(df1) are from df1
#test_new <- full_data[(nrow(full_data_x)+1):nrow(full_data), ]

#full_data_final <- cbind(full_data_new, full_data_y)

library(caret)
library(tidyr)

# select variables
selected_vars <- c("perfect_rating_score","beds", "security_deposit",
                   "has_cleaning_fee", "host_total_listings_count", "price_per_person", 
                   "ppp_ind", "property_type", "bed_category", "bathrooms", 
                   "charges_for_extra", "host_acceptance", "host_response", 
                   "market", "host_is_superhost","availability_30", "price", "availability_365",
                   "host_identity_verified", "latitude", "longitude","instant_bookable","is_location_exact", 
                   "bedrooms_accommodates","region","host_has_profile_pic","host_name_frequency",
                   "house_rules_pet","house_rules_smoking","house_rules_parties",
                   "bedrooms_bathrooms_ratio","beds_per_bedrooms","beds_per_accomodates",
                   "bathrooms_per_accomodates","cancellation_policy","monthly_price","city",
                   "has_min_nights","host_total_listings_count","sent_score")

data_selected <- full_data[, selected_vars]


# create dummy variables
dummies <- dummyVars(~., data = data_selected, fullRank = TRUE)
data_selected$perfect_rating_score <- as.factor(data_selected$perfect_rating_score)
data_dummies <- data.frame(predict(dummies, newdata = data_selected)) 

library(tidyverse)
library(tm)
library(text2vec)
library(SnowballC)
library(glmnet)
library(vip)
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    #removeWords(stopwords(kind="en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}
it_train = itoken(full_data$amenities,
                  preprocessor = tolower, #preprocessing by converting to lowercase
                  tokenizer = cleaning_tokenizer, 
                  #ids = full_data$id, 
                  progressbar = FALSE)
it_train_1 = itoken(full_data$host_verifications,
                  preprocessor = tolower, #preprocessing by converting to lowercase
                  tokenizer = cleaning_tokenizer, 
                  #ids = full_data$id, 
                  progressbar = FALSE)

it_train_2 = itoken(full_data$combined_transit,
                    preprocessor = tolower, #preprocessing by converting to lowercase
                    tokenizer = cleaning_tokenizer, 
                    #ids = full_data$id, 
                    progressbar = FALSE)
vocab = create_vocabulary(it_train)
vocab_1 = create_vocabulary(it_train_1)
vocab_2 = create_vocabulary(it_train_2)
vocab_final = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.5)
vocab_final_1 = prune_vocabulary(vocab_1, term_count_min = 10, doc_proportion_max = 0.5)
vocab_final_2 = prune_vocabulary(vocab_2, term_count_min = 10, doc_proportion_max = 0.5)
vectorizer = vocab_vectorizer(vocab_final)
vectorizer_1 = vocab_vectorizer(vocab_final_1)
vectorizer_2 = vocab_vectorizer(vocab_final_2)
# Convert the training documents into a DTM
dtm_train = create_dtm(it_train, vectorizer)
dtm_train_1 = create_dtm(it_train_1, vectorizer_1)
dtm_train_2 = create_dtm(it_train_2, vectorizer_2)
dim(dtm_train)
library(Matrix)

# Convert sparse matrix to data frame
dtm_train_df <- as.data.frame(as.matrix(dtm_train))
dtm_train_df_1 <- as.data.frame(as.matrix(dtm_train_1))
dtm_train_df_2 <- as.data.frame(as.matrix(dtm_train_2))
data_dummies_combined <- cbind(dtm_train_df, data_dummies)
data_dummies_combined_2 <- cbind(dtm_train_df_1, data_dummies_combined)
data_dummies_combined_final <- cbind(dtm_train_df_2, data_dummies_combined_2)

#CHECKS
trainxy_new <- data_dummies_combined_final[1:nrow(train_xy), ]  # Rows 1 to nrow(df1) are from df1
testxy_new <- data_dummies_combined_final[(nrow(train_xy)+1):nrow(data_dummies_combined_final), ]

dim(trainxy_new)
dim(testxy_new)

set.seed(1)
train_xy_index <- createDataPartition(trainxy_new$perfect_rating_score, p = 0.7, list = FALSE)
train_xy_train <- trainxy_new[train_xy_index, ]
train_xy_val <- trainxy_new[-train_xy_index, ]

#LOGISTIC REGRESSION - MODEL 1

logreg <- glm(train_xy_train$perfect_rating_scoreYES ~ ., data = train_xy_train, family = binomial)
summary(logreg)
valid_preds <- predict(logreg, newdata = train_xy_val, type="response")

logit_preds <- ifelse(predict(logreg, newdata = train_xy_val, type = "response") >=0.47, 1, 0)

actual_pred_lm= as.factor(train_xy_val$perfect_rating_score)

class_valid_lg = factor(logit_preds, level=unique(actual_pred_lm))
logit_cm <- confusionMatrix(data = class_valid_lg, reference =actual_pred_lm)
logit_cm$table

accuracy <- logit_cm$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 4)))


class_valid_lg <- factor(logit_preds, level = unique(actual_pred_lm))

# Calculate confusion matrix
logit_cm <- confusionMatrix(data = class_valid_lg, reference = actual_pred_lm)

# Extract true positive, false positive, true negative, and false negative values from the confusion matrix
tp <- logit_cm$table[2, 2]
fp <- logit_cm$table[2, 1]
tn <- logit_cm$table[1, 1]
fn <- logit_cm$table[1, 2]

# Calculate true positive rate (TPR) and false positive rate (FPR)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)

# Print the results
cat("True Positive Rate (TPR):", tpr, "\n")
cat("False Positive Rate (FPR):", fpr, "\n")


#Final_prediction for logistic regression

testx_d <- testxy_new %>%
  select(-perfect_rating_scoreYES)

probs_perfect <- predict(logreg, newdata = testx_d, type="response")

#make binary classifications (make sure to check for NAs!)
library(caret)
classifications_perfect <- ifelse(probs_perfect > .47, "YES", "NO")
classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
summary(classifications_perfect)
accuracy_perfect <- sum(classifications_perfect== data_selected$perfect_rating_score) / nrow(data_selected)


#XGboost - MODEL -2 
#install.packages('xgboost')
  library(xgboost)
  
  # Extract your predictor variables (X) and response variable (y) from your data frame:
  x_train_1 <- as.matrix(train_xy_train[, -which(names(train_xy_train) == "perfect_rating_scoreYES")])
  y_train_1 <- train_xy_train$perfect_rating_scoreYES
  
  
  library(caret)
  # Convert your data to a DMatrix object:
  dtrain <- xgb.DMatrix(data = x_train_1, label = y_train_1)
  
  # Set the parameters for the XGBoost model:
  params <- list(
    objective = "reg:squarederror",  # Use "reg:linear" for linear regression
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Perform k-fold cross-validation:
  cv_result <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 8,  # Set the number of folds for cross-validation
    metrics = "rmse",  # Use "error" for classification problems
    early_stopping_rounds = 10  # Set the number of rounds to wait for no improvement
  )
  
  # Get the cross-validation results:
  cv_error <- tail(cv_result$evaluation_log[, 1], 1)
  cv_stddev <- tail(cv_result$evaluation_log[, 2], 1)
  
  cat(paste0("CV RMSE: ", cv_error, " +/- ", cv_stddev))
  
  # Train the final model using the full dataset:
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = cv_result$best_iteration
  )
  
  # To make predictions on new data:
  x_val <- as.matrix(train_xy_val[, -which(names(train_xy_val) == "perfect_rating_scoreYES")])
  y_val <- train_xy_val$perfect_rating_scoreYES
  dtest <- xgb.DMatrix(data = x_val)
  y_pred <- predict(model, newdata = dtest)
  
  y_pred_binary <- ifelse(y_pred >= 0.479, 1, 0)
  
  actual_pred_lm= as.factor(train_xy_val$perfect_rating_score)
  
  class_valid_lg = factor(y_pred_binary, level=unique(actual_pred_lm))
  logit_cm <- confusionMatrix(data = class_valid_lg, reference =actual_pred_lm)
  logit_cm$table
  
  
  # Extract true positive, false positive, true negative, and false negative values from the confusion matrix
  tp <- logit_cm$table[2, 2]
  fp <- logit_cm$table[2, 1]
  tn <- logit_cm$table[1, 1]
  fn <- logit_cm$table[1, 2]
  
  # Calculate true positive rate (TPR) and false positive rate (FPR)
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  
  # Print the results
  cat("True Positive Rate (TPR):", tpr, "\n")
  cat("False Positive Rate (FPR):", fpr, "\n")
  accuracy <- (tn + tp) / sum(tp+fp+tn+fn)
  cat("Accuracy:", accuracy, "\n")

#FINAL PREDICTION for XGBOOST

library(caret)
testx_d <- testxy_new[, -which(names(testxy_new) == "perfect_rating_scoreYES")]
x_test_final <- as.matrix(testx_d)
dtest_final <- xgb.DMatrix(data = x_test_final)
y_pred_final <- predict(model, newdata = dtest_final, type = "response")

y_pred_binary_final <- ifelse(y_pred_final >= 0.479, "YES","NO")

# Print the predicted binary class labels
print(y_pred_binary_final)

y_pred_binary_final <- ifelse(is.na(y_pred_binary_final),"NO", y_pred_binary_final)
summary(y_pred_binary_final)
accuracy_perfect_xg <- sum(y_pred_binary_final== data_selected$perfect_rating_score) / nrow(data_selected)

#this code creates sample outputs in the correct format
write.table(y_pred_binary_final, "perfect_rating_score_group19_16th_may_final_submission.csv", row.names = FALSE)


#RANDOM FOREST - MODEL 3

library(ranger)

x_rf <- train_xy_train %>%
  select(-perfect_rating_scoreYES)
x_val_rf <- train_xy_val %>%
  select(-perfect_rating_scoreYES)


y_rf <-  train_xy_train$perfect_rating_scoreYES
y_val_rf <- train_xy_val$perfect_rating_scoreYES


rf.mod <- ranger(x = x_rf, y = y_rf,
                 mtry=22, num.trees=150,
                 importance="impurity",
                 probability = TRUE)

rf_preds <- predict(rf.mod, data=x_val_rf)$predictions[,2]
#rf_preds <- predict(rf.mod, data=train_xy_val)
rf_classifications <- ifelse(rf_preds>0.5, 0, 1)

rf_acc <- mean(ifelse(rf_classifications == y_val_rf, 1, 0))
rf_acc
vip(rf.mod)
library(caret)

rf_classifications <- factor(ifelse(rf_preds > 0.54, 0, 1))
y_val_rf <- factor(y_val_rf)

# Create a confusion matrix
cm <- confusionMatrix(rf_classifications, y_val_rf)

# Extract true positive, false positive, true negative, and false negative values from the confusion matrix
tp <- cm$table[2, 2]
fp <- cm$table[2, 1]
tn <- cm$table[1, 1]
fn <- cm$table[1, 2]

# Calculate true positive rate (TPR) and false positive rate (FPR)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)

# Print the results
cat("True Positive Rate (TPR):", tpr, "\n")
cat("False Positive Rate (FPR):", fpr, "\n")
accuracy <- (tn + tp) / sum(tp+fp+tn+fn)
cat("Accuracy:", accuracy, "\n")

#Prediction for Random Forest

# Make predictions with Random Forest
testx_d <- testxy_new[, -which(names(testxy_new) == "perfect_rating_scoreYES")]
rf_preds <- predict(rf.mod, data = testx_d, type = "response")
y_pred_binary_final <- ifelse(rf_preds >= 0.54, "NO", "YES")

# Replace NA values with "NO"
y_pred_binary_final <- ifelse(is.na(y_pred_binary_final), "NO", y_pred_binary_final)

# Print the predicted binary class labels
print(y_pred_binary_final)

# Calculate accuracy
accuracy_perfect_rf <- sum(y_pred_binary_final == data_selected$perfect_rating_score) / nrow(data_selected)


#GLMNET - MODEL 4

install.packages("glmnet")
library(glmnet)
library(caret)

x_train <- as.matrix(train_xy_train[, -which(names(train_xy_train) == "perfect_rating_scoreYES")])
y_train <- train_xy_train$perfect_rating_scoreYES

x_val <- as.matrix(train_xy_val[, -which(names(train_xy_val) == "perfect_rating_scoreYES")])
y_val <- train_xy_val$perfect_rating_scoreYES

cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", nfolds = 10)

best_lambda <- cv_fit$lambda.min
final_fit <- glmnet(x_train, y_train, family = "binomial", lambda = best_lambda)
probabilities <- predict(final_fit, newx = x_val, type = "response")
y_pred_binary <- ifelse(probabilities >= 0.46, 1, 0)

actual_pred_lm= as.factor(train_xy_val$perfect_rating_scoreYES)
class_valid_lg = factor(y_pred_binary, level=unique(actual_pred_lm))
cm <- confusionMatrix(data = class_valid_lg, reference =actual_pred_lm)
cm$table

accuracy <- cm$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 4)))

# Extract true positive, false positive, true negative, and false negative values from the confusion matrix
tp <- cm$table[2, 2]
fp <- cm$table[2, 1]
tn <- cm$table[1, 1]
fn <- cm$table[1, 2]

# Calculate true positive rate (TPR) and false positive rate (FPR)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)

# Print the results
cat("True Positive Rate (TPR):", tpr, "\n")
cat("False Positive Rate (FPR):", fpr, "\n")
accuracy <- (tn + tp) / sum(tp+fp+tn+fn)
cat("Accuracy:", accuracy, "\n")

#FINAL PREDICTION - GLM NET

# Remove prs variable from dummy data frame
testx_d_g <- testxy_new[, -which(names(data_selected) == "perfect_rating_scoreYES")]
probs_perfect_g <- predict(final_fit, newx = testx_d_g, type="response")


#make binary classifications (make sure to check for NAs!)
library(caret)
classifications_perfect <- ifelse(probs_perfect > .46, "YES", "NO")
classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
summary(classifications_perfect)
accuracy_perfect <- sum(classifications_perfect== data_selected$perfect_rating_score) / nrow(data_selected)

###GBM - MODEL 5

#GBM
data_gbm_dummies <- data_dummies
#CHECKS
trainxy_new_gbm <- data_gbm_dummies[1:nrow(train_xy), ]  # Rows 1 to nrow(df1) are from df1
testxy_new_gbm <- data_gbm_dummies[(nrow(train_xy)+1):nrow(data_gbm_dummies), ]

train_xy_index_gbm <- createDataPartition(trainxy_new_gbm$perfect_rating_score, p = 0.7, list = FALSE)
train_xy_train_gbm <- trainxy_new_gbm[train_xy_index_gbm, ]
train_xy_val_gbm <- trainxy_new_gbm[-train_xy_index_gbm, ]

library(gbm)
colnames(train_xy_train)
# create formula for GBM model
library(gbm)
colnames(train_xy_train)
formula_gbm <-perfect_rating_scoreYES ~ beds + security_deposit + has_cleaning_fee.YES + price_per_person + ppp_ind.1 +
  property_type.condo + property_type.hotel + property_type.house + property_type.other + bed_category.other +
  bathrooms + charges_for_extra.YES + host_acceptance.MISSING + host_acceptance.SOME + host_response.MISSING +
  host_response.SOME + market.Boston + market.Chicago + market.D.C. + market.Denver + market.East.Bay..CA +
  market.Los.Angeles + market.Monterey.Region + market.Nashville + market.New.Orleans +
  market.New.York + market.North.Carolina.Mountains + market.OTHER + market.Other..Domestic. +
  market.Portland + market.San.Diego + market.San.Francisco + market.Seattle + host_is_superhost +
  availability_30 + price + availability_365 + host_identity_verified + latitude + longitude + instant_bookable +
  is_location_exact + bedrooms_accommodates + regionMidwest + regionMountain + regionNew.England + regionOther +
  regionSouth.Central + regionSoutheast + regionSouthwest + regionWest + host_name_frequency +
  house_rules_petTRUE + house_rules_smokingTRUE + house_rules_partiesTRUE
# train GBM model
library(gbm)
gbm_mod <- gbm(formula_gbm, 
               data = train_xy_train, 
               n.trees = 1000, 
               interaction.depth = 6, 
               shrinkage = 0.01, 
               bag.fraction = 0.5, 
               train.fraction = 0.7, 
               cv.folds = 5,
               distribution = "gaussian")

# make predictions on validation set
gbm_preds <- predict(gbm_mod, newdata = train_xy_val, type = "response")

# convert probabilities to binary classification
gbm_classifications <- ifelse(gbm_preds > 0.44, 1, 0)

# calculate accuracy
gbm_acc <- mean(ifelse(gbm_classifications == train_xy_val$perfect_rating_scoreYES, 1, 0))

# create confusion matrix
gbm_cm <- confusionMatrix(factor(gbm_classifications, levels = c(0, 1)), 
                          factor(train_xy_val$perfect_rating_scoreYES, levels = c(0, 1)))

# print results
print(paste("GBM Accuracy:", round(gbm_acc, 4)))
print(gbm_cm$table)

# calculate true positive rate and false positive rate
tp <- gbm_cm$table[2, 2]
fp <- gbm_cm$table[2, 1]
tn <- gbm_cm$table[1, 1]
fn <- gbm_cm$table[1, 2]
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)
print(paste("GBM True Positive Rate (TPR):", round(tpr, 4)))
print(paste("GBM False Positive Rate (FPR):", round(fpr, 4)))

#FINAL PREDICTIONS - GBM

library(gbm)
library(caret)

# Prepare the test data
testx_d <- testxy_new[, -which(names(testxy_new) == "perfect_rating_scoreYES")]
x_test_final <- as.matrix(testx_d)

# Make predictions with GBM
y_pred_final <- predict(model, newdata = x_test_final, type = "response")

# Convert probabilities to binary class labels using a threshold of 0.479
y_pred_binary_final <- ifelse(y_pred_final >= 0.44, "YES", "NO")

# Print the predicted binary class labels
print(y_pred_binary_final)

# Replace NA values with "NO"
y_pred_binary_final <- ifelse(is.na(y_pred_binary_final), "NO", y_pred_binary_final)

# Calculate accuracy
accuracy_perfect_gbm <- sum(y_pred_binary_final == data_selected$perfect_rating_score) / nrow(data_selected)


#NAIVE BAYES - MODEL 6

# Create a new data frame without the 187nd column
train_xy_train_subset <- subset(train_xy_train, select = -c(187))
train_xy_val_subset <- subset(train_xy_val, select = -c(187))

# Install and load the e1071 package
install.packages("e1071")
library(e1071)

column_name <- "perfect_rating_scoreYES"  # Replace with the column name you want to find
column_number <- which(colnames(train_xy_train) == column_name)

# Train the Naive Bayes model
nb_model <- naiveBayes(train_xy_train_subset, train_xy_train$perfect_rating_scoreYES)

# Make predictions on validation set
nb_probs <- predict(nb_model, newdata = train_xy_val[, -1], type = "raw")[, 2]
# Convert nb_preds to factor with levels "0" and "1"
nb_preds_factor <- factor(ifelse(nb_probs > 0.55, "1", "0"), levels = c("0", "1"))

# Convert train_xy_val$perfect_rating_scoreYES to factor with levels "0" and "1"
train_xy_val$perfect_rating_scoreYES <- factor(train_xy_val$perfect_rating_scoreYES, levels = c("0", "1"))

# Calculate confusion matrix
nb_cm <- confusionMatrix(data = nb_preds_factor, reference = train_xy_val$perfect_rating_scoreYES)

# Print the confusion matrix
nb_cm$table

# Extract true positive, false positive, true negative, and false negative values from the confusion matrix
tp <- nb_cm$table[2, 2]
fp <- nb_cm$table[2, 1]
tn <- nb_cm$table[1, 1]
fn <- nb_cm$table[1, 2]

# Calculate true positive rate (TPR) and false positive rate (FPR)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)

# Print the results
cat("True Positive Rate (TPR):", tpr, "\n")
cat("False Positive Rate (FPR):", fpr, "\n")

#FINAL PREDICITON 
# Make predictions on the test data
nb_test_probs <- predict(nb_model, newdata = testxy_new[, -1], type = "raw")[, 2]
# Convert nb_test_probs to factor with levels "0" and "1"
nb_test_preds_factor <- factor(ifelse(nb_test_probs > 0.55, "1", "0"), levels = c("0", "1"))


#ENSEMBLE METHOD

#valid_preds <- predict(logreg, newdata = train_xy_val, type="response")
#probabilities <- predict(final_fit, newx = x_val, type = "response")
#y_pred <- predict(model, newdata = dtest)

# Combine predictions using voting (simple majority voting)
ensemble_preds <- ifelse(valid_preds + y_pred >=0.38, 1, 0)

# Evaluate the ensemble
actual_pred <- train_xy_val$perfect_rating_scoreYES
ensemble_cm <- table(ensemble_preds, actual_pred)
accuracy <- sum(diag(ensemble_cm)) / sum(ensemble_cm)

# Extract true positive, false positive, true negative, and false negative values from the confusion matrix
tp <- ensemble_cm[2, 2]
fp <- ensemble_cm[1, 2]
tn <- ensemble_cm[1, 1]
fn <- ensemble_cm[2, 1]

# Calculate true positive rate (TPR) and false positive rate (FPR)
tpr <- tp / (tp + fn)
fpr <- fp / (fp + tn)

# Print the results
cat("True Positive Rate (TPR):", tpr, "\n")
cat("False Positive Rate (FPR):", fpr, "\n")

# Print the evaluation metrics
print(paste("Accuracy:", round(accuracy, 4)))

#FINAL PREDICTION

library(caret)
classifications_perfect_e <- ifelse(probs_perfect + y_pred_final> .38, "YES", "NO")
classifications_perfect_e <- ifelse(is.na(classifications_perfect_e), "NO", classifications_perfect_e)
summary(classifications_perfect_e)
accuracy_perfect_e <- sum(classifications_perfect_e== data_selected$perfect_rating_score) / nrow(data_selected)


###FITTING CURVE - XGBOOST

library(xgboost)
library(caret)
library(ggplot2)

# Define the range of training set sizes
train_sizes <- seq(5000, 66000, by = 5000)

# Create empty vectors to store accuracy values
accuracies <- vector("numeric", length(train_sizes))

# Perform cross-validation for each training size
for (i in 1:length(train_sizes)) {
  size <- train_sizes[i]
  
  # Subset the training data based on the current training size
  x_train <- as.matrix(train_xy_train[1:size, -which(names(train_xy_train) == "perfect_rating_scoreYES")])
  y_train <- train_xy_train$perfect_rating_scoreYES[1:size]
  
  # Convert the data to a DMatrix object
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  
  # Set the parameters for the XGBoost model
  params <- list(
    objective = "reg:squarederror",
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  # Perform cross-validation
  cv_result <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 1000,
    nfold = 5,  # Set the number of folds for cross-validation
    metrics = "rmse",
    early_stopping_rounds = 10
  )
  
  # Get the best iteration from cross-validation
  best_iteration <- cv_result$best_iteration
  
  # Train the final model using the full training data up to the current size
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_iteration
  )
  
  # Subset the validation data based on the current training size
  x_val <- as.matrix(train_xy_val[, -which(names(train_xy_val) == "perfect_rating_scoreYES")])
  y_val <- train_xy_val$perfect_rating_scoreYES
  
  # Convert the validation data to a DMatrix object
  dtest <- xgb.DMatrix(data = x_val)
  
  # Make predictions using the trained model
  y_pred <- predict(model, newdata = dtest)
  
  # Convert predictions to binary classes
  y_pred_binary <- ifelse(y_pred >= 0.468, 1, 0)
  
  # Calculate accuracy for the current training size
  accuracy <- sum(y_pred_binary == y_val) / length(y_val)
  
  # Store the accuracy value
  accuracies[i] <- accuracy
}

# Create a data frame with training sizes and corresponding accuracy values
learning_curve_data <- data.frame(TrainingSize = train_sizes, Accuracy = accuracies)

# Plot the learning curve
ggplot(learning_curve_data, aes(x = TrainingSize, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "XGBoost Learning Curve", x = "Training Size", y = "Accuracy") +
  theme_minimal()

###FITTING CURVE - GBM

#GBM
library(gbm)
library(caret)

# Define the range of training sizes to evaluate
training_sizes <- seq(1000, 50000, by = 1000)

# Initialize vectors to store accuracy and error rate for each training size
accuracies <- numeric(length(training_sizes))
error_rates <- numeric(length(training_sizes))

# Iterate over each training size
for (i in 1:length(training_sizes)) {
  current_size <- training_sizes[i]
  
  # Subset the training data based on the current training size
  current_train <- train_xy_train[1:current_size, ]
  
  # Train the GBM model on the current training size
  gbm_mod <- gbm(formula_gbm, 
                 data = current_train, 
                 n.trees = 1000, 
                 interaction.depth = 6, 
                 shrinkage = 0.01, 
                 bag.fraction = 0.5, 
                 train.fraction = 0.7, 
                 cv.folds = 5,
                 distribution = "gaussian")
  
  # Make predictions on the validation set
  gbm_preds <- predict(gbm_mod, newdata = train_xy_val, type = "response")
  
  # Convert probabilities to binary classification
  gbm_classifications <- ifelse(gbm_preds > 0.44, 1, 0)
  
  # Calculate accuracy and error rate
  current_cm <- confusionMatrix(factor(gbm_classifications, levels = c(0, 1)), 
                                factor(train_xy_val$perfect_rating_scoreYES, levels = c(0, 1)))
  accuracies[i] <- current_cm$overall['Accuracy']
  error_rates[i] <- 1 - accuracies[i]
}

# Plot the learning curve
plot(training_sizes, accuracies, type = "l", xlab = "Training Size", ylab = "Accuracy",
     main = "GBM Learning Curve")

#### FITTING CURVE - Naive Bayes
library(e1071)
library(caret)

# Define the range of training sizes to evaluate
training_sizes <- seq(1000, 10000, by = 1000)

# Initialize vectors to store accuracy, true positive rate (TPR), and false positive rate (FPR) for each training size
accuracies <- numeric(length(training_sizes))
tprs <- numeric(length(training_sizes))
fprs <- numeric(length(training_sizes))

# Iterate over each training size
for (i in 1:length(training_sizes)) {
  current_size <- training_sizes[i]
  
  # Subset the training data based on the current training size
  current_train <- train_xy_train_subset[1:current_size, ]
  
  # Train the Naive Bayes model on the current training size
  nb_model <- naiveBayes(current_train, train_xy_train$perfect_rating_scoreYES)
  
  # Make predictions on the validation set
  nb_probs <- predict(nb_model, newdata = train_xy_val_subset, type = "raw")[, 2]
  
  # Convert nb_probs to factor with levels "0" and "1" using a threshold of 0.7
  nb_preds_factor <- factor(ifelse(nb_probs > 0.7, "1", "0"), levels = c("0", "1"))
  
  # Convert train_xy_val$perfect_rating_scoreYES to factor with levels "0" and "1"
  train_xy_val_subset$perfect_rating_scoreYES <- factor(train_xy_val_subset$perfect_rating_scoreYES, levels = c("0", "1"))
  
  # Calculate confusion matrix
  nb_cm <- confusionMatrix(data = nb_preds_factor, reference = train_xy_val_subset$perfect_rating_scoreYES)
  
  # Extract true positive, false positive, true negative, and false negative values from the confusion matrix
  tp <- nb_cm$table[2, 2]
  fp <- nb_cm$table[2, 1]
  tn <- nb_cm$table[1, 1]
  fn <- nb_cm$table[1, 2]
  
  # Calculate true positive rate (TPR) and false positive rate (FPR)
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  
  # Store accuracy, TPR, and FPR for the current training size
  accuracies[i] <- nb_cm$overall['Accuracy']
  tprs[i] <- tpr
  fprs[i] <- fpr
}

# Plot the learning curves
plot(training_sizes, accuracies, type = "l", xlab = "Training Size", ylab = "Accuracy",
     main = "Naive Bayes Learning Curve")

###FITTING CURVE - GLMNET

library(glmnet)
library(caret)

# Define the range of training sizes to evaluate
training_sizes <- seq(1000, 22000, by = 1000)

# Initialize vectors to store accuracy, true positive rate (TPR), and false positive rate (FPR) for each training size
accuracies <- numeric(length(training_sizes))
tprs <- numeric(length(training_sizes))
fprs <- numeric(length(training_sizes))

# Iterate over each training size
for (i in 1:length(training_sizes)) {
  current_size <- training_sizes[i]
  
  # Subset the training data based on the current training size
  current_train <- train_xy_train[1:current_size, ]
  
  # Convert data to matrix format
  x_train <- as.matrix(current_train[, -which(names(current_train) == "perfect_rating_scoreYES")])
  y_train <- current_train$perfect_rating_scoreYES
  
  # Fit the glmnet model on the current training size
  cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", nfolds = 10)
  best_lambda <- cv_fit$lambda.min
  final_fit <- glmnet(x_train, y_train, family = "binomial", lambda = best_lambda)
  
  # Make predictions on the validation set
  x_val <- as.matrix(train_xy_val[, -which(names(train_xy_val) == "perfect_rating_scoreYES")])
  probabilities <- predict(final_fit, newx = x_val, type = "response")
  y_pred_binary <- ifelse(probabilities >= 0.46, 1, 0)
  
  # Calculate accuracy
  cm <- confusionMatrix(data = factor(y_pred_binary, levels = c(0, 1)), 
                        reference = factor(train_xy_val$perfect_rating_scoreYES, levels = c(0, 1)))
  accuracy <- cm$overall['Accuracy']
  
  # Create confusion matrix
  tp <- cm$table[2, 2]
  fp <- cm$table[2, 1]
  tn <- cm$table[1, 1]
  fn <- cm$table[1, 2]
  
  # Calculate true positive rate (TPR) and false positive rate (FPR)
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  
  # Store accuracy, TPR, and FPR for the current training size
  accuracies[i] <- accuracy
  tprs[i] <- tpr
  fprs[i] <- fpr
}

# Plot the learning curves
plot(training_sizes, accuracies, type = "l", xlab = "Training Size", ylab = "Accuracy",
     main = "GLM Regression Learning Curve")

###FITTING CURVE - LOGISTIC REGRESSION

logreg <- glm(train_xy_train$perfect_rating_scoreYES ~ ., data = train_xy_train, family = binomial)

# Create a sequence of training sizes
training_sizes <- seq(1000, 80000, by = 1000)

# Initialize vectors to store accuracy and error rates
accuracies <- vector(length = length(training_sizes))
error_rates <- vector(length = length(training_sizes))

# Calculate accuracy and error rate for different training sizes
for (i in 1:length(training_sizes)) {
  current_size <- training_sizes[i]
  current_data <- train_xy_train[1:current_size, ]
  
  # Fit logistic regression model on the current training size
  current_logreg <- glm(perfect_rating_scoreYES ~ ., data = current_data, family = binomial)
  
  valid_preds <- predict(current_logreg, newdata = train_xy_val, type = "response")
  
  # Make predictions on the validation set
  logit_preds <- ifelse(predict(current_logreg, newdata = train_xy_val, type = "response") >= 0.46, 1, 0)
  
  actual_pred_lm <- as.factor(train_xy_val$perfect_rating_score)
  
  class_valid_lg <- factor(logit_preds, level = unique(actual_pred_lm))
  
  # Calculate confusion matrix
  logit_cm <- confusionMatrix(data = class_valid_lg, reference = actual_pred_lm)
  accuracies[i] <- logit_cm$overall['Accuracy']
  error_rates[i] <- 1 - accuracies[i]
}

# Plot the fitting curve
plot(training_sizes, accuracies, type = "l", xlab = "Training Size", ylab = "Accuracy", , main = "Learning Curve: Logistic Regression")

###FITTING CURVE - RANDOM FOREST 

library(caret)
library(ranger)

# Define the range of training sizes to evaluate
training_sizes <- seq(1000, 60000, by = 1000)

# Initialize vectors to store accuracy, true positive rate (TPR), and false positive rate (FPR) for each training size
accuracies <- numeric(length(training_sizes))
tprs <- numeric(length(training_sizes))
fprs <- numeric(length(training_sizes))

# Iterate over each training size
for (i in 1:length(training_sizes)) {
  current_size <- training_sizes[i]
  
  # Subset the training data based on the current training size
  current_train <- train_xy_train[1:current_size, ]
  
  # Train the Random Forest model on the current training size
  rf_mod <- ranger(x = current_train[, -which(names(current_train) == "perfect_rating_scoreYES")],
                   y = current_train$perfect_rating_scoreYES,
                   num.trees = 200)
  
  # Make predictions on the validation set
  rf_preds <- predict(rf_mod, data = train_xy_val)$predictions
  
  # Convert probabilities to binary classification
  rf_classifications <- ifelse(rf_preds > 0.55, 1, 0)
  
  # Convert rf_classifications and train_xy_val$perfect_rating_scoreYES to factors with common levels
  rf_classifications <- factor(rf_classifications, levels = c("0", "1"))
  train_xy_val_subset <- factor(train_xy_val$perfect_rating_scoreYES, levels = c("0", "1"))
  
  # Calculate accuracy
  rf_acc <- mean(ifelse(rf_classifications == train_xy_val_subset, 1, 0))
  
  # Create confusion matrix
  cm <- confusionMatrix(data = rf_classifications, reference = train_xy_val_subset)
  
  # Extract true positive, false positive, true negative, and false negative values from the confusion matrix
  tp <- cm$table[2, 2]
  fp <- cm$table[2, 1]
  tn <- cm$table[1, 1]
  fn <- cm$table[1, 2]
  
  # Calculate true positive rate (TPR) and false positive rate (FPR)
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  
  # Store accuracy, TPR, and FPR for the current training size
  accuracies[i] <- rf_acc
  tprs[i] <- tpr
  fprs[i] <- fpr
}

# Plot the learning curves
plot(training_sizes, accuracies, type = "l", xlab = "Training Size", ylab = "Accuracy",
     main = "Random Forest Learning Curve")
