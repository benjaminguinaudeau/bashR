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

  if(os() == "Windows"){
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

#' os
#' @export

os <- function() Sys.info()['sysname']

#' exec
#' @export
exec <- function(string, cmd = F, ...){

  if(cmd){
    return(string)}
  else{
    if(os() == "Windows"){
      return(shell(string, ...))
    }else {
      return(system(string, ...))
    }
  }
}

#' run_as_job
#' @export

run_as_job <- function(.command, import_global = F, import_package = T, env_to_import = NULL, output = ".tmp.Rdata"){
  current_env <- rlang::current_env()

  if(import_global){.GlobalEnv %>% as.list %>% imap(~{current_env[[.y]] <- .x})}
  if(!is.null(env_to_import)){env_to_import %>% as.list %>% imap(~{current_env[[.y]] <- .x})}
  if(!exists("env")){env <- rlang::new_environment()}
  if(import_package){packages <- (.packages())} else {packages <- "base"}

  if(fs::file_exists(output)){fs::file_delete(output)}
  if(fs::file_exists("script_for_job")){fs::file_delete("script_for_job")}

  .command %>%
    as.character %>%
    .[2] %>%
    stringr::str_remove_all("\\{|\\}") %>%
    paste(paste(glue::glue("pacman::p_load({ packages})"), collapse = "\n"), .,
          glue::glue('if(exists("out")){save(out, file = "[output]")}',.open = "[", .close = "]")) %>%
    stringr::str_trim(.) %>%
    stringr::str_split("\n") %>%
    .[[1]] %>%
    writeLines("script_for_job")

  rstudioapi::jobRunScript("script_for_job", workingDir = getwd(), importEnv = env)

  if(fs::file_exists(output)){load(output)}
  if(fs::file_exists(output)){fs::file_delete(outpu)}
  # if(fs::file_exists("script_for_job")){fs::file_delete("script_for_job")}

  if(exists("out")){return(out)}
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

