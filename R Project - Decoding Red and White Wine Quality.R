# Import libraries



# Check working directory
getwd()
file.choose()

# Importing the wine dataset and printing only some rows
wine_dataset <- read.csv('C:\\Users\\franc\\Documents\\GitHub\\rproject\\Dataset\\winequality.csv')
head(wine_dataset)

# Checking elements inside the dataset
str(wine_dataset) # First attribute is char, the next 11 attributes are numeric, and the last one is integer.

# Check if there is any NULL values
nulls_summary <- data.frame(NullValues = colSums(is.na(wine_dataset)))

nulls_summary
