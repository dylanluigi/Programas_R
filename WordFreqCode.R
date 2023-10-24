library(tidyverse)


read_file <- function(file_path) {
  print(paste("Reading:", file_path))
  result <- tryCatch({    
    data <- readr::read_file(file_path, locale = readr::locale(encoding = "iso-8859-1"))
    return(tibble(content = data, file = basename(file_path)))
  }, warning = function(warning_msg) {
    message("Warning reading ", file_path, ": ", warning_msg)
    return(NULL)
  }, error = function(error_msg) {
    # Handle errors
    message("Failed to read ", file_path, ": ", error_msg)
    return(NULL)
  })
  return(result)
}

directory <- "path/To/Files"

file_paths <- list.files(directory, pattern = "*.txt", full.names = TRUE)


all_books <- map_dfr(file_paths, read_file)


head(all_books)

print(length(file_paths))


cleaned_books <- all_books %>%
  mutate(cleaned_content = str_remove_all(content, "[^A-Za-z]")) %>%  # Remove everything except letters
  mutate(cleaned_content = str_to_upper(cleaned_content))  # Convert to uppercase


letter_counts <- cleaned_books %>%
  unnest(tokens = tokenizers::tokenize_characters(cleaned_content, strip_non_alphanum = TRUE, simplify = TRUE)) %>%
  count(tokens) %>%
  filter(str_length(tokens) == 1)


total_letters <- sum(letter_counts$n)
letter_counts <- letter_counts %>%
  mutate(percentage = n / total_letters * 100)

print(letter_counts)
