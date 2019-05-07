#' sudo$
#' @description This function executes the given command as sudo
#' @param cmd A bash command to execute as sudo
#' @param env_var Should the password be saved for further use
#' @export

sudo <- function(cmd,
                 intern = F,
                 ignore.stdout = F,
                 ignore.stderr = F,
                 env_var = T){

  if(class(try(keyring::key_get("SUDO_PASS"), silent = T))[1] == "try-error"){
    keyring::key_set("SUDO_PASS")
  }

  out <- system(glue::glue("echo { keyring::key_get('SUDO_PASS') } | sudo -S { cmd }"),
         intern = intern,
         ignore.stdout = ignore.stdout,
         ignore.stderr = ignore.stderr)

  if(!env_var) keyring::key_delete("SUDO_PASS")

  return(out)
}

