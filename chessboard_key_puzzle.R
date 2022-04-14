library("numbers")

convert_from_binary <- function(vector, base = 10) {
  conversion <- c()
  i <- 1
  for (i in 1:length(vector)) {
    conversion[i] <- vector[i] * 2^(i-1)
  }
  return(sum(conversion))
}

vector_sum <- function(vector) {
  sum(vector)
}


flip_bit <- function(input) {
  if (input %in% c(1,0)) {
    mod((input + 1), 2)
  } else {
    warning("Input must be a 0 or 1")
  }
}


flip_position <- function(vector, position) {
  if (position <= length(vector)) {
  vector[position] <- flip_bit(vector[position])
  return(vector)
  }
}


flip_coins <- function(dim_x, dim_y) {
  dim <- dim_x * dim_y
  round(runif(dim, min = 0, max = 1))
}


hide_key <- function(dim_x, dim_y, key_index = NULL) {
  dim <- dim_x * dim_y
  if (is.null(key_index)) {
    key_index <- sample(dim, 1)
  } else if (key_index <= dim){
    key_index <- key_index
  }
  key_vector <- integer(dim)
  key_vector[key_index] <- key_vector[key_index] + 1
  return(key_vector)
}


create_board <- function(vector, dim_x, dim_y) {
  t(matrix(vector, nrow = dim_x, ncol = dim_y))
}


flip <- function(dim_x, dim_y, flip_position, mod_value = (dim_x * dim_y), modifyer = 1, key_index = NULL, set_seed = FALSE) {
  if (set_seed) {
    set.seed(set_seed) 
    cat("Seed set to", set_seed, "\n")
  }
  starting_vector <- flip_coins(dim_x = dim_x, dim_y = dim_y)
  starting_vector_value <- convert_from_binary(starting_vector)
  starting_vector_sum_value <- vector_sum(starting_vector)
  
  key_vector <- hide_key(dim_x, dim_y, key_index)
  key_vector_value <- convert_from_binary(key_vector)
  key_vector_sum_value <- vector_sum(key_vector)
  
  flipped_vector <- flip_position(starting_vector, flip_position)
  flipped_vector_value <- convert_from_binary(flipped_vector)
  flipped_vector_sum_value <- vector_sum(flipped_vector)
  
  flipped_vector_mod_value <- mod(flipped_vector_value, mod_value) + modifyer
  
  vectors_list <- data.frame(type = c("starting_vector", "key_vector", "flipped_vector"), 
                             vector = c(paste0(unlist(starting_vector), collapse = ""),
                                        paste0(unlist(key_vector), collapse = ""),
                                        paste0(unlist(flipped_vector), collapse = "")),
                             base_10_value = c(starting_vector_value, key_vector_value, flipped_vector_value), 
                             vector_sum_value = c(starting_vector_sum_value, key_vector_sum_value, flipped_vector_sum_value), 
                             vector_mod_value = c(NA, NA, flipped_vector_mod_value)) 
  return(vectors_list)
}


get_flip_options_old <- function(flip_df) {
  starting_vector <- lapply(strsplit(flip_df$vector[[1]], split = ""), as.integer)[[1]]
  key_vector <- lapply(strsplit(flip_df$vector[[2]], split = ""), as.integer)[[1]]
  dim <- length(starting_vector)
  
  options <- list()
  
  i <- 1
  for (i in 1:dim) {
    options[[i]] <- flip_position(starting_vector, i)
  }
  
  return(options)
}


get_flip_options <- function(vector) {
  dim <- length(vector)
  options <- list()
  
  i <- 1
  for (i in 1:dim) {
    options[[i]] <- flip_position(vector, i)
  }
  
  return(options)
}


fibonacci <- function(fibonacci_seed = 1, fibonacci_length) {
  fibonacci <- c()
  i <- 1
  for (i in 1:fibonacci_length) {
    if (i > 2) {
      fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
    } else if (i <= 2) {
      fibonacci[i] <- fibonacci_seed
    }
  }
  return(fibonacci)
}


map_to_fibonacci <- function(vector, fibonacci_seed = 1) {
  conversion <- c()
  fibonacci_vector <- fibonacci(fibonacci_seed, length(vector))
  i <- 1
  for (i in 1:length(vector)) {
    conversion[i] <- vector[i] * fibonacci_vector[i]
  }
  return(sum(conversion))
}


flip_df <- flip(dim_x = 4, 
                dim_y = 4, 
                #mod_value = 4, 
                #modifyer = 1, 
                flip_position = 5, 
                key_index = 5, 
                set_seed = 34563456)

flip_df

starting_vector <- lapply(strsplit(flip_df$vector[[1]], split = ""), as.integer)[[1]]
from_starting_vector_options_list <- get_flip_options(starting_vector)
from_starting_vector_options_list

convert_from_binary(from_starting_vector_options_list[[1]])

map_to_fibonacci(from_starting_vector_options_list[[1]])

options_list_base_10 <- lapply(from_starting_vector_options_list, convert_from_binary)
options_list_base_10_clean <- sort(unlist(options_list_base_10))
options_list_base_10_clean

vector_value <- sort(unlist(options_list_base_10))[11]

options_list_fibonacci <- lapply(from_starting_vector_options_list, map_to_fibonacci)
sort(unlist(options_list_fibonacci))

x <- 1:length(options_list_base_10)
y <- sort(unlist(options_list_base_10))

plot(x, y)

x <- 1:length(options_list_fibonacci)
y <- sort(unlist(options_list_fibonacci))

plot(x, y)

modulus <- 17
base <- exp(16)

sort(mapply(mod, n = options_list_base_10, m = modulus))
sort(mapply(mod, n = options_list_fibonacci, m = modulus))

sort(mapply(log, x = options_list_base_10, base = base))
sort(mapply(log, x = options_list_fibonacci, base = base))

sort(mapply(log, base = options_list_base_10, x = modulus))

sort(mapply(sqrt, x = options_list_base_10))

log(vector_value, exp(.927113))

vector_value

create_board(test, dim_x = 2, dim_y = 2)

mod(2, 4)

