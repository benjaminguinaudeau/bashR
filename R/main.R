#' sudo$
#' @description This function executes the given command as sudo
#' @param command A bash command to execute as sudo
#' @param env_var Should the password be saved for further use
#' @export

sudo <- function(command,
                 intern = F,
                 ignore.stdout = F,
                 ignore.stderr = T,
                 env_var = T,
                 cmd = F){

  if(.Platform$OS.type == "windows"){
    if(cmd){
      out <- command
    } else {
      out <- exec(glue::glue("{ command }"),
                  intern = intern,
                  ignore.stdout = ignore.stdout,
                  ignore.stderr = ignore.stderr)
    }
  } else {
    if(class(try(keyring::key_get("SUDO_PASS"), silent = T))[1] == "try-error"){
      keyring::key_set("SUDO_PASS")
    }

    if(cmd){
      out <- glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { command }")
    } else {

      out <- exec(glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { command }"),
                  intern = intern,
                  ignore.stdout = ignore.stdout,
                  ignore.stderr = ignore.stderr)
    }

    if(!env_var) keyring::key_delete("SUDO_PASS")
  }



  return(out)
}

#' ufw
#' @export

ufw <- function(command, port, cmd = F, ...){
  sudo(glue::glue("ufw { command } { port }"), cmd = cmd, ...)
}

#' exec
#' @export

exec <- function(string, cmd = F, ...){
  if(cmd) return(string) else return(system(string, ...))
}

#' current_user
#' @export

current_user <- function(intern = T, cmd = F, ...){
  if(cmd) return("echo $USER") else return(system("echo $USER", ...))
}

#' source_rscript
#' @export

source_rscript <- function(path, cmd = F){
  exec(glue::glue("Rscript { path }"), cmd = cmd)
}

#' append
#' @export

append <- function(path, string, cmd = F){
  exec(glue::glue("echo { string } >> { path }"), cmd = cmd)
}

#' chmod
#' @export

chmod <- function(path, right, recursive = NULL, cmd, ...){
  recursive <- ifelse(is.null(recursive), "", recursive)

  exec(glue::glue("chmod { recursive } { right } { path }"), cmd = cmd, ...)
}

#' move
#' @export

move <- function(origin, desg, recursive = F, cmd = F){


  exec(glue::glue("cp { origin } { dest }"), cmd = T)


}

