#Experiment 2 : Working with vectors, replace elements of vectors, create matrix using vectors
#Creating matrix using Matrix function
#Sequence of numbers
#working with R arrays
#working with R factors
#working with R data frames

# Create vectors
v1 <- c(2,5,2)
v2<- c(1,2,5)
#Matrix using 2 Vectors
mtrx <- cbind(v1,v2)
print(mtrx)

#Vector element replace
v2[3] <- 8
mtrx

mtrx <- cbind(v1,v2)
print(mtrx)

#Sequence of numbers
1:5

#Matrix Using Matrix function
Mtx <- matrix(c(1,2,6,3,7,8),nrow=3,ncol=2)
Mtx
#Matrix element replace
Mtx[1,2]<- 10
Mtx

#R arrays
#dim=c(rowsize,colsize,no. of matrix)
arr = array(c('red','blue','green'), dim=c(3,2,4))
arr

#R factors
clrs = c('red','blue','black','gray','yellow','gray','yellow','black')
colors = factor(clrs)
colors 
nlevels(colors)
is.factor(colors)
colors_new <- factor(colors,levels = c('red','black','blue')) 
is.factor(colors_new)
colors_new
nlevels(colors_new)

#Data Frames
Name = c("Rahul","Gautam","Sufiyan")
roll = c(81,80,83)
df = data.frame(Name,roll)
df
