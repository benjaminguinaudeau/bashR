#' sudo$
#' @description This function executes the given command as sudo
#' @param cmd A bash command to execute as sudo
#' @export

sudo <- function(cmd,
                 intern = F,
                 ignore.stdout = F,
                 ignore.stderr = F){

  if(class(try(keyring::key_get("SUDO_PASSWORD")))[1] == "try-error"){
    keyring::key_set("SUDO_PASSWORD")
  }

  system(glue::glue("{ keyring::key_set('SUDO_PASSWORD') } | { cmd }"),
         intern = intern,
         ignore.stdout = ignore.stdout,
         ignore.stderr = ignore.stderr)
}



