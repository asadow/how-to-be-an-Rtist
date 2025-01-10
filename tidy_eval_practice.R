z <- c("a", "b")
df <- data.frame(Z = 1:3, a = 1:3, b = 1:3, z = "test", x = "blah")

select_this <- function(df, x){
  x_upper <- enexpr(x) %>%
    as_name %>%
    str_to_upper

  df %>%
    select(
      all_of(x_upper), all_of(x)
    )
}

wrapper_select_this <- function(df, x){
  df %>% select_this(x)

}

wrapper_select_this <- function(df, x){
  df %>% select_this({{ x }})
}

df %>% select_this(z)

df %>%
  wrapper_select_this(z)

df %>%
  wrapper_select_this(z) %>%
  select(z)

df %>% select(z)


select_this <- function(df, x){
  df %>%
    select(
      {{ x }}
    )
}

wrapper_select_this <- function(df, x){
  df %>% select_this( {{ x }})

}

df %>% wrapper_select_this(z)
