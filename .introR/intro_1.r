# Introduction to R --------------------------
# assign
a1 <- 1
a1 = 1; b1 = 1

a2 <- c(1,2,3)
a2 <- a2 + 10

a3 <- c("a","b","c")

# data type and indexing ------------------------------------------
# type for single value
# numeric (double) / integer / character / logical
a1 <- runif(10, -5, 5)
typeof(a1)
is.double(a1)
is.numeric(a1)

a1 <- as.integer(a1)
typeof(a1)

a1 <- as.character(a1)

a1 <- as.logical(a1)

a1 <- c(0,0,1,1,1) # a1 <- c(FALSE, F,TRUE, T, T)
a1 <- as.logical(a1)
typeof(a1)
is.logical(a1)


# type of data strcutre
# vector / matrix (arracy) / data.frame / list

a1 <- rnorm(10, 0, 1)
class(a1)

names(a1)
names(a1) <- paste0("a", 10)
names(a1) <- paste0("x", 1:length(a1))
a1

a1 <- matrix(a1, ncol = 2)
class(a1)

rownames(a1) <- paste0("x", 1:dim(a1)[1])
colnames(a1) <- paste0("a", 1:dim(a1)[2])


a1 <- data.frame(a1)
class(a1)

a1 <- list(a1)
class(a1)

str(a1) # structure


# indexing
# $; [];
vector_a <- rnorm(10, 0, 1)
vector_a[]
vector_a[1]
vector_a[c(2,3,4)]
vector_a[2:3]
names(vector_a) <- paste0("x", 1:length(vector_a))

tmp <- c(a1 = 1, b1 = 2, c1 = 1)
names(tmp)
names(tmp)[c(1,2)] <- c("aa1", "bb1")
names(tmp)

vector_a["x1"]

matrix_a <- matrix(vector_a, ncol = 2)
matrix_a[1,]
matrix_a[,1]
matrix_a[1:2,]
matrix_a[,1:2]

matrix_a[c(2,3),c(1,2)]


rownames(matrix_a) <- paste0("x", 1:dim(matrix_a)[1])
colnames(matrix_a) <- paste0("a", 1:dim(matrix_a)[2])

matrix_a[c("x1","x2"), "a1"]


dataframe_a <- data.frame(matrix_a)
class(dataframe_a)

names(dataframe_a)
names(dataframe_a)[2] <- "B1"

colnames(dataframe_a)
rownames(dataframe_a)
rownames(dataframe_a) <- NULL

dataframe_a[,1]
dataframe_a[[1]]

dataframe_a[c(1,2),]

dataframe_a$a1
dataframe_a$a1[c(1,2,3)]


list_a1 <- list(dataframe_a)
list_a2 <-
  list(
    myvector = vector_a,
    matrix_a,
    dataframe_a,
    list_a1)

names(list_a2)[c(2,3,4)] <- c("mymatrix", "mydataframe", "mylist")

list_a2[1]
list_a2[[1]]
list_a2$myvector
list_a2["myvector"]
list_a2[["myvector"]]
list_a2[1:2]
list_a2[[1:2]]

list_a2[[3]][c(1,2),]

list_a2[[4]][c(1,2),]
list_a2[[4]][[1]][c(1,2),]


# factor

grades <- c("grade9", "grade10", "grade11","grade12","grade13")
sort(grades)

grades <- factor(grades, levels = c("grade9", "grade10", "grade11","grade12","grade13"))
sort(grades)

