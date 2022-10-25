
## TIDYEVAL ####
## Paraphrased from
## https://rpubs.com/lionel-/programming-draft
## and
## https://rpubs.com/lionel-/tidyeval-introduction
### QUASIQUOTATION: THE BASIS OF TIDYEVAL
## The fundamental quoting function is quote(),
## it returns the expression or i.e. quote of its argument:
x <- quote(letters[1:5])
## A quoted expression can be evaluated using the function eval()
eval(x)
paste0(eval(x), 1)


#> [1] "a" "b" "c" "d" "e"
context <- list(letters = rev(LETTERS))
eval(x, context)
# #> [1] "Z" "Y" "X" "W" "V"

df <- tibble(mass = c(70, 80, 90), height = 1.6, 1.7, 1.8)
bmi_expr <- quote(mass / height^2)
eval(bmi_expr, df)
#> [1] 27.3 31.2 35.2

## with() is a quoting function
## quoting functions then typically do
## the eval (evaluation) step for you
with(df, mass / height^2)

## Because dataframes can be contexts
## R has its identity as a data-oriented language.
## Here are more quoting functions
# with(df, expr)
# lm(formula, df)
# mutate(df, new = expr)
# ggplot(df, aes(expr))

## Note "" is a quoting function
## Sometimes you want to make a quoting function use object x
## which represents "your string"
## So you will need to unquote "your string" first
## Unquote can also mean "evaluate right away"
## You cannot use eval, which does not evaluate right away
## The below does not work

x <- "dplyr"
library(x)
x <- quote("dplyr")
library(eval(x))
## To see why, run
debugonce(library)
## then run an empty line??
##And F10 to scroll through

## Error stems from
substitute(eval(x)) %>% as.character

##  We want x to evaluate right away into dplyr or "dplyr"
## as library(dplyr) or library("dplyr") will work
## This can be done with

library(x, character.only = TRUE)
## But notice that this depends on the character.only argument
## Unfortunately there is no general unquoting operator in base R
## functions

## The solution to this problem is 2-fold
## 1) a tidyeval operator called bang-bang (!!)
## It signals the quoting function that part of the argument
## is to be unquoted, i.e., evaluated right away.
## 2) functions that support !!

## Recall the fundamental quoting function is quote(),
## In the tidyverse, it is expr().
## expr() quotes its argument with support for !!
## so that its expression
## can unquote, that is, evaluate right away.
## Since the quotation of its argument
## is not a pure quotation, we call it quasiquotation.

expr(!!x)

## Not only expr(), but all quoting functions
## in the tidyverse support !!. They all support quasiquotation

## quote() does not have quasiquotation support.
## It is pure quotation
## See how quote(eval(x)) quotes eval(x)
## i.e. We cannot unquote/evaluate its argument right away
x <- "height"
quote(eval(x))

## This is the basis of tidyeval


## Why this matters ####

## On one hand we could abandon trying to have a function use
## object x where x evaluates to "your string"
## Instead we could quote "your string" without the ""
x <- quote(height)

## eval() does the job
with(starwars,  mass / (eval(x))^2)
starbmi <- function(x) with(starwars,  mass / (eval(x))^2)
starbmi(x)

## So one way to store names can be quote(height)
height <- quote(height)
starbmi(height)

## But we usually store names in strings
## so that many can easily be combined.
c("height", "weight")
## We can't easily combine names when they are written without ""
## that is, as objects. The following won't work
quote(height, weight)

## Not only can we not combine names,
## but it is also less friendly to read names without "".
## Text in R without quotes represent objects and functions
## They are not just names.
## It would be confusing to change this convention.
## So we keep names in strings and assign
## them to any object, like e.g. x:
x <- "height"

## But this is using base R
## which does not have unquoting support within quoting functions
## Let's use the tidyvesre then.
## Recall that quoting functions in the tidyverse support
## the !! unquotation mechanism.

## transmute() is the tidyverse equivalent to with
## So let's try to use !! as transmute() supports unquoting
transmute(starwars, mass / (!!x)^2)

## We still get an error
## qq_show() shows us why

rlang::qq_show(transmute(starwars, mass / (!!x)^2))

## transmute() is using a string.
## We want the string to refer to an object.
## We want the string to be a symbol.
## So we try
x <- sym("height")

transmute(starwars, mass / (!!x)^2)

## Our function starbmi has no problems
starbmi <- function(x) transmute(starwars, mass / (!!x)^2)
starbmi(x)
## We can even write

starbmi <- function(x) transmute(starwars, mass / (!!sym(x))^2)
starbmi("height")

## What if we want to write
## starbmi(height)?
## Just like transmute has no "" around height?
## In other words, what if we want to skip the sym() step?
## sym("height") is a symbol for height after all
transmute(starwars, mass / height^2)

## Put yet another way, what if we want
## starbmi to be a quoting function?
## We need a function to quote the argument x
## We can't use expr(), as expr() gives us back x
rm(x)
expr(x)

## We need an enclosed expr(),
## that is, an expr() that can be enclosed in a function
## so that it quotes the argument supplied by you, rather than x as is.
## This function is called enexpr()
starbmi <- function(x) transmute(starwars, mass / (!!enexpr(x))^2)
starbmi(height)

## The shortcut for !!enexpr() is curly curly: {{}}
starbmi <- function(x) transmute(starwars, mass / {{x}}^2)
starbmi(height)


### Q: WHY USE ENQUO and not EXEXPR????
### END Q
## enquos() returns a list
## Typically used in e.g.
grouped_mean2 <- function(data, summary_var, ...) {
  summary_var <- enquo(summary_var)
  group_vars <- enquos(...)

  data %>%
    group_by(!!group_vars) %>%
    summarise(mean = mean(!!summary_var))
}

grouped_mean2(mtcars, disp, cyl, am)
## Instead of forwarding the arguments to group_by
## we passed the list of length 2 to it
## We passed this
quos(cyl, am)
## Q Whats the difference between quos and vars?
vars(cyl, am)
## END Q
## Note quos is just function(x) enquos(x)

## Another way to see this is using qq_show
vars <- list(
  quote(cyl),
  quote(am)
)
rlang::qq_show(group_by(!!vars))

## We need to splice the list

## Triple-bang (!!!), is the unquote-splice operator
## It takes each element of a list and unquotes them
## as independent arguments to the surrounding "function call"

rlang::qq_show(group_by(!!!vars))

## Q: What is function call?

## We use !!! instead
grouped_mean3 <- function(data, summary_var, ...) {
  summary_var <- enquo(summary_var)
  group_vars <- enquos(...)

  data %>%
    group_by(!!!group_vars) %>%
    summarise(mean = mean(!!summary_var))
}
## This works and we can even
## modify the name of the quoted argument
## For example, here we modify the name of am to be BLAH
grouped_mean3(mtcars, disp, cyl, BLAH = am)

### NAMES ####

## You’ll have to unquote the LHS of :=.
## This vestigial operator is interpreted
## by tidy eval functions in exactly the same way
## as = but with !! support


args_names <- function(...) {
  vars <- enquos(..., .named = TRUE)
  names(vars)
}

my_name <- "Adam"
args_names(!!my_name := 1)

## TRIPPY: KEEP OR DELETE? ##
## Now you can realize that
## expr() is actually function(x) enexpr(x)
## so that you can provide the argument !!x to expr() for example:
x <- "height"
expr <- function(x) enexpr(x)
expr(!!x)
## TRIPPY

## Aside:

## Because arguments in ... can have arbitrary names,
## like smelly = underwear,
## we don’t want to “use up” valid names.
# # In tidyverse packages we use the convention of
## prefixing named arguments with a dot so that
## conflicts are less likely:

x1 <- "y <- x + 10"

x2 <- rlang::parse_expr(x1)
x2 <- sym("y <- x + 10")
#> y <- x + 10
is.call(x2)
#> [1] TRUE


`%W>%` <- function(lhs,rhs){
  # `options()` changes options but returns value BEFORE change
  opts <- options(warn = -1)
  on.exit(options(warn=opts$warn))
  eval.parent(substitute(lhs %>% rhs))
}

"%W>%" <- function(lhs, rhs) substitute(lhs %>% rhs)
data.frame(a= c(1,-1)) %W>% mutate(a=sqrt(a))
"%W>%" <- function(lhs, rhs) quote(lhs %>% rhs)
data.frame(a= c(1,-1)) %W>% mutate(a=sqrt(a))

lobstr::ast(data.frame(a = c(1, -1)) %W>% mutate(a = sqrt(a)))
lobstr::ast(data.frame(a = c(1, -1)) %>% mutate(a = sqrt(a)))
data.frame(a= c(1,-1)) %>% mutate(a=sqrt(a))

f <- function(x) {
  list(quote(x),
       substitute(x),
       x)
}

x_supplied <- 100
f(x = x_supplied)
