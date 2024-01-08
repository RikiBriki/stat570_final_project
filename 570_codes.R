setwd(here::here()) 

library(dplyr)
library(purrr)
library(jsonlite)
library(tidyr)
# Read JSONL data from a file
jsonl_data <- stream_in(file("data.json"))
head(jsonl_data)
# View the structure of the data
str(jsonl_data)

# View the first few rows of the data
head(jsonl_data)

class(jsonl_data$event_params[1][1])

jsonl_data$event_params[[4983]]
jsonl_data$event_params[[1]][1] ## Access keys

jsonl_data$event_params[[1]][2] ## Access all values 

jsonl_data$event_params[[1]][[2]][1] ## Access int value 

jsonl_data$event_params[[1]][[2]][2] ## Access string value

#Y# kisinin ortalama oturum suresi, Ortalama oturum sayisi
#Y# en cok olan eventler
#Y# ilk dokunusla son dokunus
#Y# Breakdown by = ios, Android, Egypt, Android Version, Continent, 
#Y# 100, 50 tanesi 1. seviye bitirdi, 30 tanesi 2. seviyeyi bitirdi
#Y# user engagement ne kadar sure oyun telefonda on ekranda
#Y# uygulama kac??nc?? gunde s??l??n??yor
#Y# Levellar??n success rate ?? en zor levellar
#Y# Average tr??als t??ll success
#Y# oyuncular??n yaslar??n??n d??st??rbut??onu
#Y# ULKE BAZLI ??nsan say??s??
#Y# Telefon markasi bazli 
unique(jsonl_data$event_name)

#filtered_data=jsonl_data_flat[jsonl_data_flat$event_name=="level_end"&jsonl_data_flat$key=="level",]
list_of_dfs=list()

for(i in 1:nrow(jsonl_data)){
  temp_df= as.data.frame(jsonl_data$event_params[[i]])
  list_of_dfs[[i]] <- temp_df
  print(i/nrow(jsonl_data))
}

class(list_of_dfs)

### 
list_of_dfs[[4983]]
list_of_dfs[[1]]$value$int_value

### Will give error
#for (i in seq_along(list_of_dfs)) {
#  print(i)
#  list_of_dfs[[i]]$value <- ifelse(
#    !is.na(list_of_dfs[[i]]$value$int_value),
#    as.character(list_of_dfs[[i]]$value$int_value),
#    as.character(list_of_dfs[[i]]$value$string_value)
#  )
#  list_of_dfs[[i]] <- list_of_dfs[[i]][, !(names(list_of_dfs[[i]]) %in% c("int_value", "string_value"))]
#}
#
## Print the resulting modified list of data frames
#print(list_of_dfs[[4983]])


### Re Attempt with try catch and handle the error 

list_of_dfs=list()

for(i in 1:nrow(jsonl_data)){
  temp_df= as.data.frame(jsonl_data$event_params[[i]])
  list_of_dfs[[i]] <- temp_df
  print(i/nrow(jsonl_data))
}


for (i in seq_along(list_of_dfs)) {
  #print(i)
  tryCatch({
    if ("value" %in% names(list_of_dfs[[i]])) {
      if (is.list(list_of_dfs[[i]]$value)) {
        list_of_dfs[[i]]$value <- ifelse(
          !is.na(list_of_dfs[[i]]$value$int_value),
          as.character(list_of_dfs[[i]]$value$int_value),
          as.character(list_of_dfs[[i]]$value$string_value)
        )
        list_of_dfs[[i]]$value <- as.character(list_of_dfs[[i]]$value)
        list_of_dfs[[i]] <- list_of_dfs[[i]][, !(names(list_of_dfs[[i]]) %in% c("int_value", "string_value"))]
      }
    }
  }, error = function(e) {
    # If an error occurs, rename the second column to "value"
    if (length(names(list_of_dfs[[i]])) >= 2) {
      #list_of_dfs[[4983]]="cancer"
      #print(i)
      #print(list_of_dfs[[i]])
      #print(i)
      new_temp_df= cbind.data.frame(list_of_dfs[[i]][[1]],c(list_of_dfs[[i]][[2]]))
      names(new_temp_df) <- c("key","value")
      list_of_dfs[[i]]<<- new_temp_df
      #print(list_of_dfs[[i]])
    }
  })
}
#### the <<- operator above is very important 

## now we can do normal things 
combined_df <- bind_rows(list_of_dfs, .id = "df_id")

flattened_df <- combined_df %>%
  pivot_wider(names_from = key, values_from = value)

flattened_df <- flattened_df[, -1]

final_df= cbind.data.frame(jsonl_data,flattened_df)
final_df2= final_df |> select(-event_params,-user_properties,-items)
#write.csv(final_df2,"flattened.csv")
colnames(final_df2)
## we got to unnest device, geo, app_info, traffic_source , privacy_info


# Print the reshaped dataframe
class(final_df2$privacy_info)
class(final_df2$event_date)
nrow(final_df2$privacy_info)
temp_df=final_df2$privacy_info
temp_df
str(final_df2)
final_df2$items[2]

is_dataframe <- function(column) {
  is.data.frame(column)
}


dataframe_cols <- c()

# Loop through each column in final_df2
for (col in colnames(final_df2)) {
  if (is_dataframe(final_df2[[col]])) {
    dataframe_cols <- c(dataframe_cols, col)
  }
}

combined_nested_dfs=rep(0,nrow(final_df2))

for (element in dataframe_cols){
  temp_index=as.character(element)
  temp_df= final_df2[[temp_index]]
  combined_nested_dfs=cbind.data.frame(combined_nested_dfs,temp_df)
}
final_df2=final_df2 |> select(-dataframe_cols)
final_df3=cbind.data.frame(final_df2,combined_nested_dfs)

final_df3$event_timestamp
final_df3$user_first_touch_timestamp
options(scipen=999)
write.csv(final_df3,"plsfkinwork3.csv")
