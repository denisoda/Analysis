# My Functions

# If package is installed
is.installed <- function(mypkg) {
  is.element(mypkg, installed.packages()[, 1]) 
}

is.installed("plyr")


# Observe an object
inspect <- function(a) {
  list(.length = length(a), 
       .mode = mode(a), 
       .type = typeof(a), 
       .class = class(a), 
       .str = str(a))
}