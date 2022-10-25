library(bookdown)

serve_book(dir = ".", output_dir = "_book", preview = TRUE, quiet = FALSE)

unlink("_main.Rmd")

bookdown::publish_book()

Y
preview_chapter("03-opening-data.rmd")







render_book(input = ".", output_format = NULL, clean = TRUE,
            envir = parent.frame(),
            output_dir = NULL, new_session = NA, preview = FALSE,
            config_file = "_bookdown.yml")
#
