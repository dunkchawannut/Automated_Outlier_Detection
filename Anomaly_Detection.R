#--- OUTLIER DETECTION
#--- Created by Chawannut (Dunk) Prommin

library("dplyr")
setwd("D:\\BlackWood")
data <- read.csv("data2.csv")

#check data type of each column
col_type <- sapply(data , class)
numeric_col <- col_type[col_type == "numeric"]
numeric_col <- names(numeric_col)
categorical_col <- col_type[col_type == "factor"]
categorical_col <- names(categorical_col)

# ---- FUNCTION FOR
# ---- UNIVARIATE OUTLIER DETECTION  (NUMERIC)
univariate_numeric_outlier <- function(data , numeric_col , k = 2){
 # browser()
  
  outlier_table <- data.frame()
  
  for(i in numeric_col){
    variable <- data[[i]]
    
    Q1 <- quantile(variable , 0.25)
    Q3 <- quantile(variable , 0.75)
    IQR <- Q3 - Q1  
    threshold <- c(Q1 - k*IQR , Q3 + k*IQR)
    
    outlier <- variable[(variable > threshold[2]
                         | variable < threshold[1]) == TRUE] 
    
    if(length(outlier) > 0 ){
    index <- which(variable %in% outlier)
    index <- data.frame("RowIndex" = index , 
                        "Column" = i , 
                        "Value" = outlier )
    outlier_table <- rbind(outlier_table,index)
    }
    
  }

  return(outlier_table)
}

# ---- FUNCTION FOR\
#---- UNIVARIATE OUTLIER DETECTION  (CATEGORICAL)
univariate_categorical_outlier <- function(data , categorical_col , cutoff = 0.05){
 # browser()
  outlier_table <- data.frame()
  
  for(j in categorical_col){
    
    contingency_table <- prop.table(table(data[[j]] )) 
    categorical_outier <- names(contingency_table[contingency_table <  cutoff])
    
    index <- row.names(data[data[[j]] %in% categorical_outier,])
    
    if(length(index) > 0 ){
    index <- data.frame("RowIndex" = index , 
                        "Column" = j , 
                        "Value" = categorical_outier )
    outlier_table <- rbind(outlier_table,index)
    }
    
  }
  return(outlier_table)
  }


#---- BIVARIATE OUTLIER DETECTION (NUMERICAL + CATEGORICAL)
bivariate_numeric_outlier <- function(x , categorical_col,numeric_col){
  #browser()
  outlier_table <- data.frame()
  
   for(j in categorical_col){
     
  split_data <- split(data , data[[j]] )
  
  split_data <- lapply(split_data , function(x){
    univariate_numeric_outlier(x , numeric_col)
  })
  
  split_data <- do.call("rbind" , split_data)
  split_data$split_by <- j
  

  if(length(nrow(split_data)) > 0 ){
    outlier_table <- rbind(outlier_table,split_data)
  }
  
  }
  
  return(outlier_table)
}
  
bivariate_numeric_outlier(data , categorical_col , numeric_col )

#---- BIVARIATE OUTLIER DETECTION (CATEGORICAL + CATEORICAL)

bivariate_categorical_outlier <- function(x , categorical_col , p = 0.05){
  #browser()
  all_comb <- combn(categorical_col , 2,simplify =  TRUE)
  outlier <- data.frame()
  #--- LOOP THROUGH EACH COMBINATION
  for(k in 1:ncol(all_comb)){
    
  table_result <- prop.table(table( data[[all_comb[1,k]]] , data[[all_comb[2,k]]] ))
  
  #get row and col name
  temp <- which(table_result <= p & table_result > 0 ,arr.ind = T)
  
  
  if(nrow(temp) > 0){
  value <- table_result[table_result <= p & table_result > 0]
  
  row_name <- row.names(table_result)[temp[,1]]
  col_name <- colnames(table_result)[temp[,2]]
  
  store <- data.frame("row.name" = row_name, 
                      "col.name" = col_name, 
                      "Percentage" = value,
                      "Column1" = all_comb[1,k],
                      "Column2" = all_comb[2,k])
  
  outlier <- rbind(outlier , store)
  }
  
  }
  return(outlier)
}

bivariate_categorical_outlier(data , categorical_col )


### TEST
univariate_numeric_outlier(data , numeric_col )
univariate_categorical_outlier(data , categorical_col)

bivariate_numeric_outlier(data,categorical_col ,numeric_col )
bivariate_categorical_outlier(data,categorical_col)


