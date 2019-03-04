#############################################################################################################
#chapter. Data Visualization with ggplot2
library ( tidyverse)
mpg  
ggplot ( data  =  mpg )  + 
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))

ggplot ( data  =  mpg )  + 
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  color  =  class ))

ggplot ( data  =  mpg )  + 
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  size  =  class ))

ggplot ( data  =  mpg )  +
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  alpha  =  class ))

ggplot ( data  =  mpg )  +
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  shape  =  class ))

ggplot ( data  =  mpg )  + 
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ),  color  =  "blue" )

ggplot ( data  =  mpg )  +
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  facet_wrap ( ~  class ,  nrow  =  2 )

ggplot ( data  =  mpg )  + 
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  facet_grid ( drv  ~  cyl )

ggplot ( data  =  mpg )  +
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))

ggplot ( data  =  mpg )  +
  geom_smooth ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))

ggplot ( data  =  mpg )  +
  geom_smooth ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  linetype  =  drv ))

ggplot ( data  =  mpg )  + 
  geom_smooth ( mapping  =  aes ( x  =  displ ,  y  =  hwy )) 

ggplot ( data  =  mpg )  + 
  geom_smooth ( mapping  =  aes ( x  =  displ ,  y  =  hwy ,  group  =  drv ))

ggplot ( data  =  mpg )  + 
  geom_smooth ( 
    mapping  =  aes ( x  =  displ ,  y  =  hwy ,  color  =  drv ),
    show.legend  =  FALSE )

ggplot ( data  =  mpg )  +
  geom_point ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  geom_smooth ( mapping  =  aes ( x  =  displ ,  y  =  hwy ))

ggplot ( data  =  mpg ,  mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  geom_point ()  + 
  geom_smooth ()

ggplot ( data  =  mpg ,  mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  geom_point ( mapping  =  aes ( color  =  class ))  +
  geom_smooth ()

ggplot ( data  =  mpg ,  mapping  =  aes ( x  =  displ ,  y  =  hwy ))  + 
  geom_point ( mapping  =  aes ( color  =  class ))  + 
  geom_smooth ( data  =  filter ( mpg ,  class  ==  "subcompact" ), se  =  FALSE )

ggplot ( data  =  diamonds )  + 
  geom_bar ( mapping  =  aes ( x  =  cut ))

ggplot ( data  =  diamonds )  + 
  stat_count ( mapping  =  aes ( x  =  cut ))

demo  <-  tribble (
  ~ a ,       ~ b , 
  "bar_1" ,  20 , 
  "bar_2" ,  30 , 
  "bar_3" ,  40 
  )

ggplot ( data  =  demo )  +
  geom_bar ( mapping  =  aes ( x  =  a ,  y  =  b ),  stat  =  "identity" )

ggplot ( data  =  diamonds )  + 
  geom_bar ( mapping  =  aes ( x  =  cut ,  y  =  ..prop.. ,  group  =  1 ) )

ggplot ( data  =  diamonds )  + 
  stat_summary (
    mapping  =  aes ( x  =  cut ,  y  =  depth ), 
    fun.ymin  =  min , 
    fun.ymax  =  max , 
    fun.y  =  median
    )

ggplot ( data  =  diamonds )  +
  geom_bar ( mapping  =  aes ( x  =  cut ,  color  =  cut )) 

ggplot ( data  =  diamonds )  + 
  geom_bar ( mapping  =  aes ( x  =  cut ,  fill  =  cut ))

ggplot ( data  =  diamonds )  + 
  geom_bar ( mapping  =  aes ( x  =  cut ,  fill  =  clarity ))

ggplot ( 
  data  =  diamonds ,
  mapping  =  aes ( x  =  cut ,  fill  =  clarity )
  )  + 
  geom_bar ( alpha  =  1 / 5 ,  position  =  "identity" ) 

ggplot ( 
  data  =  diamonds , mapping  =  aes ( x  =  cut ,  color  =  clarity ) 
  )  + 
  geom_bar ( fill  =  NA ,  position  =  "identity" )

ggplot ( data  =  diamonds )  + 
  geom_bar ( 
    mapping  =  aes ( x  =  cut ,  fill  =  clarity ), 
    position  =  "fill"
    )

ggplot ( data  =  diamonds )  + 
  geom_bar ( 
    mapping  =  aes ( x  =  cut ,  fill  =  clarity ), 
    position  =  "dodge" 
    )

ggplot ( data  =  mpg )  + 
  geom_point (
    mapping  =  aes ( x  =  displ ,  y  =  hwy ), 
    position  =  "jitter" 
    )

ggplot ( data  =  mpg ,  mapping  =  aes ( x  =  class ,  y  =  hwy ))  + 
  geom_boxplot () 

ggplot ( data  =  mpg ,  mapping  =  aes ( x  =  class ,  y  =  hwy ))  +
  geom_boxplot ()  +
  coord_flip ()

nz  <-  map_data ( "nz" )

ggplot ( nz ,  aes ( long ,  lat ,  group  =  group ))  +
  geom_polygon ( fill  =  "white" ,  color  =  "black" )

ggplot ( nz ,  aes ( long ,  lat ,  group  =  group ))  + 
  geom_polygon ( fill  =  "white" ,  color  =  "black" )  + 
  coord_quickmap ()

bar  <-  ggplot ( data  =  diamonds )  + 
  geom_bar ( 
    mapping  =  aes ( x  =  cut ,  fill  =  cut ), 
    show.legend  =  FALSE ,
    width  =  1 
    )  + 
  theme ( aspect.ratio  =  1 )  + 
  labs ( x  =  NULL ,  y  =  NULL )

bar  +  coord_flip ()

bar  +  coord_polar ()

#####################################################################################################################
# Chapter. Data Transformation with dplyr

library ( nycflights13 )

library ( tidyverse )
flights

filter ( flights ,  month  ==  1 ,  day  ==  1 )

jan1  <-  filter ( flights ,  month  ==  1 ,  day  ==  1 )

( dec25  <-  filter ( flights ,  month  ==  12 ,  day  ==  25 ))

filter ( flights ,  month  ==  11  |  month  ==  12 )

nov_dec  <-  filter ( flights ,  month  %in%  c ( 11 ,  12 ))

filter ( flights ,  ! ( arr_delay  >  120  |  dep_delay  >  120 ))

filter ( flights ,  arr_delay  <=  120 ,  dep_delay  <=  120 )

NA  >  5

10  ==  NA

NA  +  10

NA  /  2

NA  ==  NA

x  <-  NA

y  <-  NA

x  ==  y

is.na ( x )

df  <-  tibble ( x  =  c ( 1 ,  NA ,  3 )) 

filter ( df ,  x  >  1 )

arrange ( flights ,  year ,  month ,  day )

arrange ( flights ,  desc ( arr_delay ))

df  <-  tibble ( x  =  c ( 5 ,  2 ,  NA ))

arrange ( df ,  x )

arrange ( df ,  desc ( x ))

select ( flights ,  year ,  month ,  day )

select ( flights ,  year : day )

select ( flights ,  - ( year : day ))

rename ( flights ,  tail_num  =  tailnum )

select ( flights ,  time_hour ,  air_time ,  everything ())

flights_sml  <-  select ( flights , 
                         year : day ,
                         ends_with ( "delay" ), 
                         distance ,
                         air_time 
                         )

mutate ( flights_sml , 
         gain  =  arr_delay  -  dep_delay ,
         speed  =  distance  /  air_time  *  60
         )

mutate ( flights_sml , 
         gain  =  arr_delay  -  dep_delay , 
         hours  =  air_time  /  60 , 
         gain_per_hour  =  gain  /  hours 
         )

transmute ( flights , 
            gain  =  arr_delay  -  dep_delay ,
            hours  =  air_time  /  60 , 
            gain_per_hour  =  gain  /  hours
            )

transmute ( flights , 
            dep_time , 
            hour  =  dep_time  %/%  100 ,
            minute  =  dep_time  %%  100
            )

( x  <-  1 : 10 )

lag ( x )

lead ( x )

x

cumsum ( x )

cummean ( x )

y  <-  c ( 1 ,  2 ,  2 ,  NA ,  3 ,  4 )

min_rank ( y )

min_rank ( desc ( y ))

row_number ( y )

dense_rank ( y )

percent_rank ( y )

cume_dist ( y )

summarize ( flights ,  delay  =  mean ( dep_delay ,  na.rm  =  TRUE ))

by_day  <-  group_by ( flights ,  year ,  month ,  day )

summarize ( by_day ,  delay  =  mean ( dep_delay ,  na.rm  =  TRUE ))

by_dest  <-  group_by ( flights ,  dest )

delay  <-  summarize ( by_dest ,
                       count  =  n (),
                       dist  =  mean ( distance ,  na.rm  =  TRUE ), 
                       delay  =  mean ( arr_delay ,  na.rm  =  TRUE )
                       )

delay  <-  filter ( delay ,  count  >  20 ,  dest  !=  "HNL" )

ggplot ( data  =  delay ,  mapping  =  aes ( x  =  dist ,  y  =  delay ))  + 
  geom_point ( aes ( size  =  count ),  alpha  =  1 / 3 )  +
  geom_smooth ( se  =  FALSE )

delays  <-  flights  %>% 
  group_by ( dest )  %>% 
  summarize ( 
    count  =  n (),
    dist  =  mean ( distance ,  na.rm  =  TRUE ), 
    delay  =  mean ( arr_delay ,  na.rm  =  TRUE ) 
    )  %>% 
  filter ( count  >  20 ,  dest  !=  "HNL" )

flights  %>% 
  group_by ( year ,  month ,  day )  %>% 
  summarize ( mean  =  mean ( dep_delay ))

flights  %>% 
  group_by ( year ,  month ,  day )  %>% 
  summarize ( mean  =  mean ( dep_delay ,  na.rm  =  TRUE ))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)

(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
  ungroup() %>%
  summarise(flights = n())

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

######################################################################################################################
# Chapter. Data Import with readr

library(tidyverse)

heights <- read_csv("data/heights.csv")

read_csv("a,b,c
1,2,3
         4,5,6")

read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
         1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("a,b,c\n1,2,.", na = ".")

str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = ".")

x <- parse_integer(c("123", "345", "abc", "123.45"))

x

problems(x)

parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

parse_number("$123,456,789")

parse_number("123.456.789", locale = locale(grouping_mark = "."))

parse_number("123'456'789", locale = locale(grouping_mark = "'"))

charToRaw("Hadley")

x1 <- "El Ni\xf1o was particularly bad this year"

x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

x1

x2

parse_character(x1, locale = locale(encoding = "Latin1"))

parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))

guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")

parse_factor(c("apple", "banana", "bananana"), levels = fruit)

parse_datetime("2010-10-01T2010")

parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)

parse_time("01:10 am")

parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")

parse_date("01/02/15", "%d/%m/%y")

parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

guess_parser("2010-10-01")

guess_parser("15:01")

guess_parser(c("TRUE", "FALSE"))

guess_parser(c("1", "5", "9"))

guess_parser(c("12,352,561"))

str(parse_guess("2010-10-10"))

challenge <- read_csv(readr_example("challenge.csv"))

problems(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_integer(),
    y = col_character()
  )
)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)

tail(challenge)

challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)

challenge2

challenge2 <- read_csv(readr_example("challenge.csv"), 
                       col_types = cols(.default = col_character())
)

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)

df

type_convert(df)

write_csv(challenge, "challenge.csv")

challenge

write_csv(challenge, "challenge-2.csv")

read_csv("challenge-2.csv")

write_rds(challenge, "challenge.rds")

read_rds("challenge.rds")

library(feather)

write_feather(challenge, "challenge.feather")

read_feather("challenge.feather")

######################################################################################################################
# Chapter. Functions

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

x <- df$a

(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)

(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01(c(0, 5, 10))

rescale01(c(-10, 0, 10))

rescale01(c(1, 2, 3, NA, 5))

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

identical(0L, 0)

x <- sqrt(2) ^ 2

x

x == 2

x - 2

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

y <- 10

if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)

mean_ci(x)

mean_ci(x, conf = 0.99)

mean(1:10, na.rm = TRUE)

average <- mean(feet / 12 + inches, na.rm = TRUE)

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 1:3)

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 6:1, na.rm = "foo")

sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

stringr::str_c("a", "b", "c", "d", "e", "f")

commas <- function(...) stringr::str_c(..., collapse = ", ")

commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("Important output")

x <- c(1, 2)

sum(x, na.mr = TRUE)

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}

f <- function() {
  if (x) {
    # Do 
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}

f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_missings(mtcars)

x <- show_missings(mtcars) 

class(x)

dim(x)

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 

f <- function(x) {
  x + y
} 

y <- 100

f(10)

y <- 1000

f(10)

`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}

table(replicate(1000, 1 + 2))

rm(`+`)

###################################################################################################################