# 1. create a matrix of 2 row and 3 column
mtx = matrix(c(1,2,3,1,2,3),nrow= 2, ncol=3)
mtx

# 2. create an array of dimension 3,3,2 

arr = array(c(1), dim = c(3,3,2))
arr

# 3. create a factor 
fct = c(2,4,5,7,9,2,4,7,9,7)
numbers = factor(fct)
numbers
nlevels(numbers)
# 4. to generate 15 random numbers and find numbers greater than the average

temp = floor(runif(15,min=1,max = 100))
sort(temp)
avg = floor(sum(temp)/15)
print(avg)
for (i in  temp){
  if(i>=avg){
    print(i)
  }
  else{
    next
  }
}

# 5. reverse order of a given factor

colors = factor(c('red','blue','black','gray','yellow','gray','yellow','black'), levels = c('red','blue','black','gray','yellow'))
colors 
#reverse the elements of the factor
reversed <- rev(colors)
# reassign the reversed values to original values, replacing to reassign new elements to old levels
levels(reversed) <- rev(levels(reversed))
reversed

# 6. to create dataframes
Name = c("Rahul","Gautam","Sufiyan","")
roll = c(81,80,83)
df = data.frame(Name,roll)
df

