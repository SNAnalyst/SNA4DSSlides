
#' Create code for index.Rmd
#' 
#' Creates the code to go into index.Rmd
#' 
#' This function searches through the slide Rmd file and extracts the 
#' tags ("names") and accompanying descriptions. 
#' Then, if creates the full code that only needs to be copied and pasted as is 
#' into index.rmd to have fully up-to-date links. 
#' Rendering index.Rmd yields a fully functional index.html file.
#' 
#' Of course, it depends on providing this function with the correct info.
#' 
#' NOTE: using the 'name' option in a xaringan Rmd does not work well when done 
#' on a slide that also has \code{layout: true}. 
#' In this case, leave the slide with \code{layout: true} empty in terms of 
#' content and use \code{name} on the very next slide where you do have content.
#' Otherwise, many (all?) browser can not find the shortcut and will open the 
#' front page of the slidedeck, rather than the slide \code{name} refers to.
#' 
#' As a useful convention for the title of the deck in index.html (this is 
#' given in \code{lecture_description}) is:
#' 
#' Week 02 -- Lab: Intro to R in SNA
#' 
#' @param rmd_file the full path to the Rmd file that needs to be parsed (typically a path on your own computer)
#' @param lecture_description the title for the slide deck 
#'
#' @return code to be copied as is into index.rmd
#' @keywords internal
make_index_code <- function(rmd_file, lecture_description = "Week 02 -- Lab: Intro to R in SNA") {
  
  slidedeckFile <- strsplit(rmd_file, "/") |>
    {\(x) x[[1]]}() |>
    {\(x) x[length(x)]}() |>
    stringr::str_remove(".Rmd") |>
    stringr::str_remove(".rmd")
  
  txt <- readLines(rmd_file)
  name_locs <- grepl('^name:', txt) |> which()
  description_locs <- grepl('^description:', txt) |> which()
  
  output <- matrix(ncol = 2, nrow = length(name_locs))
  output[, 1] <- txt[name_locs] |>
    stringr::str_remove('name:') |>
    stringr::str_trim()
  
  for (nn in 1:nrow(output)) {
    descr <- description_locs %in% c(name_locs[nn] - 1, name_locs[nn] + 1) |> which()
    if (length(descr) != 0) {  # there is a matching description for the name
      if (length(descr) == 1) {
        output[nn, 2] <- txt[description_locs[descr]] |>
          stringr::str_remove('description:') |>
          stringr::str_remove_all('"') |>
          stringr::str_trim()
      } else if (length(descr) > 1) {
        stop("name '", output[nn, 1], " 'has multiple descriptions next to it, make sure to only have 1")
      }
    }
  }
  
  code_matrix <- data.frame(matrix("", ncol = 1, nrow = 12 + nrow(output)))
  code_matrix[1, ] <- paste0("<!-- ", lecture_description, " -->")
  code_matrix[2, ] <- "```{r, include = FALSE}"
  code_matrix[3, ] <- "overview <- matrix(c("
  final_name <- 3 + nrow(output)
  code_matrix[4:final_name, ] <- glue::glue('"{name}", "{description}",', name = output[, 1], description = output[, 2])
  # remove the last comma for the last one
  code_matrix[final_name, 1] <- stringr::str_remove(code_matrix[(3 + nrow(output)), 1], ",$")
  code_matrix[final_name + 1, ] <- ("), byrow = TRUE, ncol = 2) |> ")
  code_matrix[final_name + 2, ] <- "as.data.frame() |> "
  code_matrix[final_name + 3, ] <- 'setNames(c("name", "description"))'
  code_matrix[final_name + 5, ] <- glue::glue('slidedeckFile <- "{slidedeckFile}"')
  code_matrix[final_name + 6, ] <- glue::glue('lectureName <- "{lecture_description}"')
  code_matrix[final_name + 7, ] <- "```"
  code_matrix[final_name + 9, ] <- "`r make_overview(overview, path, slidedeckFile, lectureName)`"
  
  colnames(code_matrix) <- NULL
  print(code_matrix, right = FALSE, quote = FALSE, row.names = FALSE)
}




