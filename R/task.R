ua <- httr::user_agent("http://github.com/labbcb/RNNRCLient")

#' List tasks
#'
#' @param host URL to server
#'
#' @return a list
#' @export
#' @importFrom httr modify_url GET http_type http_error content
#' @importFrom jsonlite fromJSON
list_tasks <- function(host = "http://localhost:8080") {
  path <- "/tasks"
  url <- modify_url(host, path = path)

  resp <- GET(url, ua)

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (http_error(resp)) {
    stop(
      sprintf(
        "TES API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Convert task as list to data frame
#'
#' Some fields are renamed. Log fields are dicarded. Also add elapsed time fields.
#' Task name is splitted into two columns: workflow and task.
#'
#' @param task as list
#'
#' @return a data frame
#' @export
#' @importFrom dplyr tibble %>%
#' @importFrom lubridate as_datetime
#' @importFrom tidyr separate
task_df <- function(task) {
  tibble(
    name = task$name,
    created = as_datetime(task$creation_time),
    state = task$state,
    request_cpu_cores = task$resources$cpu_cores,
    request_memory_gb = ifelse(is.null(task$resources$ram_gb), 0, task$resources$ram_gb),
    worker_node = task$host,
    max_cpu_cores = ceiling(task$metrics$cpu_percentage / 100),
    max_memory_gb = task$metrics$memory * 1e-9,
    started = as_datetime(task$logs[[1]]$start_time),
    completed = as_datetime(task$logs[[1]]$end_time),
    executor_started = as_datetime(task$logs[[1]]$logs[[1]]$start_time),
    executor_completed = as_datetime(task$logs[[1]]$logs[[1]]$end_time),
    from_execution = executor_completed - executor_started,
    from_start = completed - started,
    from_creation = completed - created,
    exit_code = task$logs[[1]]$logs[[1]]$exit_code
  ) %>% separate(col = "name", into = c("workflow", "task"), sep = "\\.")
}

#' Read tasks from JSON file
#'
#' @importFrom jsonlite read_json
#' @export
read_tasks <- function(file) {
  read_json(file) %>%
    map_df(task_df)
}
