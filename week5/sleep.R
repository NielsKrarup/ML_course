#sleep
library(stringr)
stringr::sub
str_extract(string = csv_files[1], pattern = "/.*\\.csv")

csv_files <- list.files(path = "~/Desktop/ML/ML_repo/ML/data/Sleep-EDF-15_U-Time")

for(i in csv_files) {
  name <- gsub(pattern = "\\.csv", replacement = "", x = i)
  
  obj <- read.csv(file = file.path("~/Desktop/ML/ML_repo/ML/data/Sleep-EDF-15_U-Time", i), header = F)
  head(obj)
  print(name)
  print(dim(obj))
  assign(x = name, value = obj)
}


head(X_train)
head(y_train)

head(X_test)
head(y_test)


assign()
read.csv(file = )