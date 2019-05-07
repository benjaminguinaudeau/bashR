#' sudo$
#' @description This function executes the given command as sudo
#' @param command A bash command to execute as sudo
#' @param env_var Should the password be saved for further use
#' @export

sudo <- function(command,
                 intern = F,
                 ignore.stdout = F,
                 ignore.stderr = F,
                 env_var = T){

  if(class(try(keyring::key_get("SUDO_PASS"), silent = T))[1] == "try-error"){
    keyring::key_set("SUDO_PASS")
  }

  out <- exec(glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { command }"),
         intern = intern,
         ignore.stdout = ignore.stdout,
         ignore.stderr = ignore.stderr)

  if(!env_var) keyring::key_delete("SUDO_PASS")

  return(out)
}

#' exec
#' @export

exec <- function(string, cmd = F, ...) if(cmd == T) return(string) else return(system(string, ...))

#' current_user
#' @export

current_user <- function() system("echo $USER", intern = T)

#' source_rscript
#' @export

source_rscript <- function(path, cmd = F) exec(glue::glue("Rscript { path }"), cmd = cmd)

#' append
#' @export

append <- function(path, string, cmd = F) exec(glue::glue("echo { string } >> { path }"), cmd = cmd)

#' chmod
#' @export

chmod <- function(path, right, recursive = NULL, cmd, ...){
  recursive <- ifelse(is.null(recursive), "", recursive)

  exec(glue::glue("chmod { recursive } { right } { path }"), cmd = cmd, ...)
}

