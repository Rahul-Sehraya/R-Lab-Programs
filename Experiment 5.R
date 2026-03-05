# Matrix Multiplication 
mtx1 = matrix(c(7,9,12,2,4,13), nrow =2, ncol=3)
mtx2 = matrix(c(1,7,12,19,2,8,13,20,3,9,14,21), nrow =3, ncol=4)

mtx3 = mtx1%*%mtx2
mtx3


v1 <- c(12,7,3,4.2,18,2,54,-21,8,-5)
#mean of vector
len = length(v1)
mean(v1)
#trimmed mean
mean(v1, trim = 0.3)

#mean with NA value
v2 <- c(v1,NA)
mean(v2)
v2


#median 
median(v1)

v3<- c(2,13,8,7,15,3,9)
median(v3)

#mode 
v4 <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
modal<- function(m){
  uniqv <- unique(m)
  uniqv[which.max(tabulate(match(m,uniqv)))] 
}

mode_ans <- modal(v4)
mode_ans
