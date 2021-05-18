# S3 ---------------------------------------
# generate Class
## 클래스를 부여해주는 함수
mkclass <- function(class, ...){
  obj <- list(...)
  class(obj) <- ifelse(missing(class), "default", class)
  return(obj)
}

## 메쏘드를 사용하는 함수
area <- function(obj) {
  UseMethod("area", obj)
}
## 해당 메쏘드 안에 있는 실제 실행함수
area.default <- function(obj){
  res <- sum(unlist(obj))
  return(res)
}
area.rectangle <- function(obj){
  return(obj[["x"]] * obj[["y"]])
}
area.circle <- function(obj){
  radi <- obj[["radius"]]
  res <- pi*radi^2
  return(res)
}



# S4 --------------------------
setClass("test",
         slots = c(
           name   = "character",
           gender = "character",
           age    = "numeric",
           test   = "list"
         ),
         prototype = list(
           name   = "",
           gender = "",
           age    = 0,
           test   = list()
         ),
         validity = function(object) {
           if (!is.character(object@name)) {
             stop("@name must be a character")
           }
           return(TRUE)
         }
)

set_class <- function(name, gender, age) {
  tmp <- new("test")

  tmp@name   <- name
  tmp@gender <- gender
  tmp@age    <- age

  return(tmp)
}

person1 <- set_class("sooyong", "male", 10)

person1@test <-
  data.frame(
    korean = 80,
    math   = 90
  )

setGeneric("showscore",
           function(object, ...) standardGeneric("showscore"))
setMethod(f = "showscore",         # function name
          signature = "test", # class
          function(object, subject) {

            paste0(object@name, "is ",
                   object@age, "years old and ",
                   subject, " score is ", object@test[subject] )
          }
)

showscore(person1, subject = "math")





















