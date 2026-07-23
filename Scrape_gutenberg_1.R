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

# Checking out the vector to_be_downloaded
View(to_be_downloaded)
class(to_be_downloaded)
length(to_be_downloaded)

# The download
Political_science_texts <- gutenberg_download(
  to_be_downloaded)

# If the job takes too long, and you get doubts, you may use 
# gutenberg_cache_clear_all()

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

# We want all subjects for these books.
subjects_p_sci_all <- gutenberg_subjects |>
  filter(gutenberg_id %in% to_be_downloaded)

glimpse(subjects_p_sci_all)
head(subjects_p_sci_all)

# Putting it together

write_gutenberg_md <- function(book_id,
                               texts_df,
                               meta_df,
                               subjects_df,
                               out_dir = "books_md") {
  # Text lines for the book
  txt <- texts_df |>
    filter(gutenberg_id == book_id)
  
  if (nrow(txt) == 0) {
    warning("Ingen tekstlinjer for book_id = ", book_id)
    return(invisible(NULL))
  }
  
  # Metadata for the book
  meta <- meta_df |>
    filter(gutenberg_id == book_id)
  
  # All of the subjects for the book
  subs <- subjects_df |>
    filter(gutenberg_id == book_id)
  
  # Choose elements from metadata
  title    <- meta$title[1]
  author   <- meta$author[1]
  language <- meta$language[1] %||% "unknown"
  rights   <- meta$rights[1]    %||% ""
  
  subjects_str <- subs |>
    pull(subject) |>
    paste(collapse = "; ")
  
  # Merging the text lines
  full_text <- txt |>
    pull(text) |>
    paste(collapse = "\n")
  
  # Metadata-header
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
  
  # Filename and folder
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  safe_title <- stringr::str_replace_all(title, "[^[:alnum:]]+", "_")
  file_name  <- file.path(out_dir, paste0(book_id, "_", safe_title, ".md"))
  
  writeLines(md_lines, con = file_name, useBytes = TRUE)
  message("Wrote ", file_name)
}

# This is the final step, and if everything works, the corpus will be downloaded to a folder called books_md
purrr::walk(
  to_be_downloaded,
  ~ write_gutenberg_md(
    book_id     = .x,
    texts_df    = Political_science_texts,
    meta_df     = meta_p_sci,
    subjects_df = subjects_p_sci_all,  # alle subjects for disse bøkene
    out_dir     = "books_md"
  )
)

# This was all, and you may make your own corpus.


