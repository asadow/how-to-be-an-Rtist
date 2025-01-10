read_csv_in_df <- function(n_characteristics, names_characteristics){

  file_path <- file.choose()
  directory_path <- path_dir(file_path)
  files <- list.files(directory_path, pattern = "csv")
  file_paths <- dir_ls(directory_path)

  m <- str_split_fixed(files, "[._]", n_characteristics)
  colnames(m) <- names_characteristics

  df <- m %>%
    as_tibble %>%
    mutate(data = map(file_paths, read_csv))

  df

}

cols <- c("source", "country", "date" ,"file_type")
file_path <- file.choose()
directory_path <- path_dir(file_path)
df <- tibble(directory = directory_path, file = list.files(directory_path))
m <- str_split_fixed(df$file, "[._]", 4)
colnames(m) <- cols
df_file_info <- as_tibble(m)
df %>%
  bind_cols(df_file_info) %>%
  mutate(file_paths = glue("{directory_path}/{file}"))


df <- read_csv_in_df(4, cols)
df$data %>% names
