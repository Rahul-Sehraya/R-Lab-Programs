#using plot and hist
years = factor(c(2004,2006,2005,2004,2004,2006,2005,2003,2005,2006,2003,2005,2006,2004))

plot(years,
     xlab="Year occurances",
     ylab ="Years")
years_table = table(years)
years_table

year_data = c(2003,2003,2003,2004,2004,2004,2004,2004,2005,2005,2005,2006,2006,2007,2007,2007,2007)

hist(year_data,
     xlab = "Year Occurrences",
     ylab = "Frequency",
     main = "Histogram of Years",
     breaks = c(2002.5, 2003.5, 2004.5, 2005.5, 2006.5, 2007.5))

#Sepal.Length iris dataset 
classification = c()

for (i in 1:nrow(iris)) {
  if (iris$Sepal.Length[i] > 5) {
    classification[i] = "greater than 5"
  } else {
    classification[i] = "lesser than 5"
  }
}
data.frame(Sepal.Length = iris$Sepal.Length, Classification = classification)
