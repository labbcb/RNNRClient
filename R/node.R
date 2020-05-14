#' Convert node as list to data frame
#'
#' @param node as list
#'
#' @return data frame
#' @export
#' @importFrom dplyr tibble
node_df <- function(node) {
  tibble(
    worker_node = node$host,
    port = node$port,
    active = node$active,
    cpu_cores = node$cpu_cores,
    memory_gb = node$ram_gb
  )
}
