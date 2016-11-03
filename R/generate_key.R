#' Generate strain collection key
#'
#' Generates strain collection keyfile assuming you are using
#' one 96 well plate, that is in position 1 and rotated 180 degrees
#' in position 4. Positions 2 and 3 are controls.
#'
#' @param path_experiment String. Path to the experimental 96 well plate
#' CSV. The CSV should be in wide format with the first column labeled "row"
#' with corresponding row NUMBERS, and remaining columns numbered.
#' @param path_control String. Path to the control plate in positions 2 and 3.
#' @param collection_id String. Name of strain collection. Start with letter
#' and no spaces please!
#'
#' @details Woah, so many cool details about the implementation of this function.
#' Any special cases or things we should know?
#'
#' @examples
#' generate_key('Group_2.csv', 'WT.csv', 'MATa-CTF4-Group1')
#'
#' @import tidyverse
#' @export

generate_key <- function(path_experiment, path_control, collection_id) {
  plate1 <-
    read_csv(path_experiment, col_types = cols()) %>%
    gather(column, strain_id, -row, convert = T) %>%
    mutate(row = 2L * row, column = 2L * column)

  plate2 <-
    read_csv(path_control, col_types = cols()) %>%
    gather(column, strain_id, -row, convert = T) %>%
    mutate(row = 2L * row, column = 2L * column - 1L)

  plate3 <-
    read_csv(path_control, col_types = cols()) %>%
    gather(column, strain_id, -row, convert = T) %>%
    mutate(row = 2L * row - 1L, column = 2L * column)

  plate4 <-
    read_csv(path_experiment, col_types = cols()) %>%
    gather(column, strain_id, -row, convert = T) %>%
    mutate(
      row = 2L * (9L - row) - 1L,
      column = 2L * (13L - column) - 1L
    )

  plate384 <-
    bind_rows(plate1, plate2, plate3, plate4) %>%
    mutate(
      strain_collection_id = collection_id,
      plate = 1L,
      plate_control = ((strain_id=='rec2458 pWJ1781') &
                         (!((row == 2) & (column == 2))) &
                         (!((row == 15) & (column == 23)))),
      strain_collection_notes = ''
    ) %>%
    select(strain_collection_id, strain_id, plate, row, column, everything()) %>%
    arrange(row, column)

  write_csv(plate384, paste0(collection_id, '.csv'))
}
