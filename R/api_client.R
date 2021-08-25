#' Set credentials to pythonanywhere as environment variables
#'
#' Initialize env variables. Note that they disappear after session is closed
#'
#' @param username Your pythonanywhere username
#' @param token Your pythonanywhere personal token
#'
#' @return sets envs
#'
#' @examples
#' pyany_auth(username, token)
#'
#' @export
pyany_auth <- function(username, token){
  Sys.setenv(PYANYWHERE_PAT = token)
  Sys.setenv(PYANYWHERE_USERNAME = username)
}
#' List content in given folder
#'
#' List all files in directory
#'
#' @param folder_name Directory name as displayed in files cart
#'
#' @return object of class pythonanywhere_api
#'
#' @examples
#' pyany_list_files(folder_name)
#'
#' @export
pyany_list_files <- function(folder_name){
  creds <- get_creds()
  api_base <- glue::glue("https://www.pythonanywhere.com/api/v0/user/{creds$username}/")

  path <- glue::glue("files/path/home/{creds$username}/{folder_name}")
  resp <- httr::GET(glue::glue(api_base, path),
                   httr::add_headers("Authorization" = glue::glue("Token {creds$token}")))

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  structure(
    list(
      content = names(parsed),
      path = path,
      response = resp
    ),
    class = "pythonanywhere_api"
  )
}

print.github_api <- function(x, ...) {
  cat("<pythonanywhere_api directories list ", sep = "")
  str(x$content)
  invisible(x)
}
#' Download a file
#'
#' Downloads a file
#'
#' @param file_path Path to file, for example folder_name/file_name.extension
#' @param file_extension File extension. For now only .rds and .csv are working
#'
#' @return content of csv (tibble) or rds (tibble or other objects)
#'
#' @examples
#' an_object <- pyany_download_file(file_path, file_extension = "csv")
#'
#' @export
pyany_download_file <- function(file_path, file_extension = NULL){
  creds <- get_creds()
  api_base <- glue::glue("https://www.pythonanywhere.com/api/v0/user/{creds$username}/")

  resp <- httr::GET(glue::glue(api_base, "files/path/home/{creds$username}/{file_path}"),
                   httr::add_headers("Authorization" = glue::glue("Token {creds$token}")))

  if (httr::http_type(resp) != "application/octet-stream") {
    stop("API did not return binary file", call. = FALSE)
  }

  resp <- pyany_read_file(resp, type = file_extension)
}

# Helper functions
pyany_pat <- function(){
  pat <- Sys.getenv('PYANYWHERE_PAT')
  username <- Sys.getenv('PYANYWHERE_USERNAME')
  if(identical(pat,"")){
    stop("Please set env var PYANYWHERE_PAT to your pythonanywhere personal access token, see instructions in package readme page.",
         call. = FALSE)
  }
  pat
}

pyany_username <- function(){
  username <- Sys.getenv('PYANYWHERE_USERNAME')
  if(identical(username, "")){
    stop("Please set env var PYANYWHERE_USERNAME to your pythonanywhere username",
         call. = FALSE)
  }
  username
}
get_creds <- function(){
  creds <- list(
    token  = pyany_pat(),
    username = pyany_username()
  )
  creds
}
pyany_read_file <- function(resp, type = "rds"){
  connection <- rawConnection(resp$content)
  if(is.null(type)){
    stop("Please specify file extension (rds or csv)", call. = FALSE)
  }
  if(type == "rds"){
    file <- readRDS(gzcon(connection))
  }else if(type == "csv"){
    file <- readr::read_csv(gzcon(connection))
  }
  gc()
  close.connection(connection)
  file
}
