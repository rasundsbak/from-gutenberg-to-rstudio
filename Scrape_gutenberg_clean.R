setwd("/Users/path/to/your/files/")
getwd()

# install.packages("gutenbergr")
#install.packages("devtools")
#install.packages("usethis")
# etc.

library(conflicted)
library(tidyverse)
library(gutenbergr)
library(devtools)
library(usethis)

conflict_scout()

conflicts_prefer(dplyr::filter())
conflicts_prefer(dplyr::lag())

# Want to scrape selected books from gutenberg.org. search"political science". import to Obsidian vault.

political_science_subjects <- gutenberg_subjects |>
  filter(subject == "Political science")

View(political_science_subjects)
class(political_science_subjects)

to_be_downloaded <- political_science_subjects$gutenberg_id

Political_science_texts <- gutenberg_download(
  to_be_downloaded)

length(to_be_downloaded)

# Identifies text markers such as chapters

P_sci_text_w_patterns <- Political_science_texts |>
  gutenberg_add_sections(
    pattern = "^(Letter|Chapter) [0-9]+",   # Multiple formats fra "details"
    format_fn = stringr::str_to_title,
    group_by = "auto",
    section_col = "section"
  )

# Get overview over what you have done

dim(P_sci_text_w_patterns)
head(P_sci_text_w_patterns)


# How many books/ gutenberg_ids do we have?
Political_science_texts |> 
  summarise(
    antall_rader = n(),
    antall_boker = n_distinct(gutenberg_id)
  )

# Lines pr book
Political_science_texts |>
  count(gutenberg_id, name = "antall_linjer")

# We need metadata, in order to do a good job

meta_p_sci <- gutenberg_metadata |>
  filter(gutenberg_id %in% to_be_downloaded)

# looking at the result, to confirm
glimpse(meta_p_sci)
head(meta_p_sci)

# A big chunk of code is supposed to solve this process. I had some help from ChatGPT
# I might split up and try to understand this later...
# But not sure...

write_gutenberg_md <- function(book_id, out_dir = "books_md") {
  # text
  txt <- gutenberg_download(book_id)
  
  # metadata for this book
  meta <- gutenberg_metadata |>
    filter(gutenberg_id == book_id)
  
  subs <- gutenberg_subjects |>
    filter(gutenberg_id == book_id)
  
  # select elements from metadata
  title    <- meta$title[1]
  author   <- meta$author[1]
  language <- meta$language[1] %||% "unknown"
  rights   <- meta$rights[1]    %||% ""
  
  subjects_str <- subs |>
    pull(subject) |>
    paste(collapse = "; ")
  
  # merge the text lines
  full_text <- txt |>
    pull(text) |>
    paste(collapse = "\n")
  
  # metadata-header
  header_lines <- c(
    paste0("# Title: ", title),
    paste0("# Author: ", author),
    paste0("# Gutenberg ID: ", book_id),
    paste0("# Language: ", language),
    paste0("# Rights: ", rights),
    paste0("# Subjects: ", subjects_str),
    "",
    "--- START OF TEXT ---",
    ""
  )
  
  md_lines <- c(header_lines, full_text)
  
  # filenames
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  safe_title <- stringr::str_replace_all(title, "[^[:alnum:]]+", "_")
  file_name  <- file.path(out_dir, paste0(book_id, "_", safe_title, ".md"))
  
  writeLines(md_lines, con = file_name, useBytes = TRUE)
  message("Wrote ", file_name)
}

# When we download the corpus, the vector to_be_downloaded is essential
View(to_be_downloaded)
class(to_be_downloaded)

# Adding the metadata with a dplyr function
Political_science_with_meta <- Political_science_texts |>
  left_join(meta_p_sci, by = "gutenberg_id")

glimpse(Political_science_with_meta)

# This is the final step, and if everything works, the corpus will be downloaded to a folder called books_md
purrr::walk(to_be_downloaded, write_gutenberg_md)

# This was all, and you may make your own corpus.


