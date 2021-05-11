set.seed(10111)

dataframe_b <-
  data.frame(
    gender = c(rep("female", 10), rep("male", 10)),
    scores = c(rnorm(10, 80, 10), rnorm(10, 75, 5)),
    hours  = rpois(20, 2)
)

# more advanced indexing
dataframe_b

dataframe_b$hours <- dataframe_b$hours + 1

dataframe_b[1:10, ]
dataframe_b[dataframe_b$gender == "female", ]
dataframe_b[dataframe_b$gender == "female" & dataframe_b$scores > 80, ]

dataframe_b$pass <- dataframe_b$scores > 80

# if

if(dataframe_b$scores[1] > 80) {

  pass <- "pass"

} else {

  pass <- "fail"

}

# for
for(i in 1:dim(dataframe_b)[1]) {

  if(dataframe_b$scores[i] > 80) {

    pass <- "pass"

  } else {

    pass <- "fail"

  }

  dataframe_b$pass[i] <-  pass

}


# function
female_dat <- dataframe_b[dataframe_b$gender == "female", ]
for(i in 1:dim(female_dat)[1]) {

  if(female_dat$scores[i] > 80) {

    pass <- "pass"

  } else {

    pass <- "fail"

  }

  female_dat$pass[i] <-  pass

}
female_dat

filter_pass <- function(mydata, gender, cut_score) {
  filtered_dat <- mydata[dataframe_b$gender == gender, ]
  for(i in 1:dim(filtered_dat)[1]) {

    if(filtered_dat$scores[i] > cut_score) {

      pass <- "pass"

    } else {

      pass <- "fail"

    }

    filtered_dat$pass[i] <-  pass

  }
  return(filtered_dat )

}
filter_pass(dataframe_b, gender = "male", cut_score = 30)
filter_pass(dataframe_b, gender = "male", cut_score = 70)






