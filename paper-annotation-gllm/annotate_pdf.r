#' Annotate a PDF
#'
#' @param pdf name of pdf file
#' @param query query
#' @param env name of environment (leaving the default usually makes sense)
#' @param verbose should intermediate steps produce screen messages.
#'
#' @returns Nothing, produces html version of chat as a file
#' @export
annotate_pdf <- function(pdf,
                         query,
                         env = "r-opinion",
                         verbose = TRUE) {
  
  # track how long the whole operation takes
  start <- Sys.time()
  if (verbose) {
    id <- cli::cli_progress_bar(
      type = "tasks", 
      total = 5L,
      format = "{cli::pb_status} (task {cli::pb_current}/{cli::pb_total}) [{cli::pb_elapsed}]",
      format_done = "{pdf} annotated [{cli::pb_elapsed}]"
    )
  }
  check_env(env = env)
  pag <- reticulate::import("pyautogui")
  pyperclip <- reticulate::import("pyperclip")


  # just for my system, make sure keyboard is set to US
  pag$hotkey("ctrl", ",")

  if (verbose) cli::cli_progress_update(status = "Opening Firefox")
  system("firefox https://chatgpt.com/?model=gpt-4\\&temporary-chat=true")

  # check if rate limit is reached
  Sys.sleep(2L)
  found <- try(pag$locateOnScreen("rate.png"), silent = TRUE)
  if (methods::is(found, "try-error")) {
    found <- try(pag$locateOnScreen("rate-alt.png"), silent = TRUE)
  }
  if (methods::is(found, "try-error")) {
    found <- try(pag$locateOnScreen("rate-alt2.png"), silent = TRUE)
  }
  if (methods::is(found, "try-error")) {
    found <- try(pag$locateOnScreen("rate-alt2.png"), silent = TRUE)
  }
  if (methods::is(found, "try-error")) {
    found <- try(pag$locateOnScreen("rate-alt4.png"), silent = TRUE)
  }
  if (!methods::is(found, "try-error")) {
    cli::cli_progress_cleanup()
    cli::cli_alert_info("rate limit reached. Waiting until {Sys.time() + 60 * 60 * 1}")
    Sys.sleep(60 * 60 * 1) # wait 3 hours
    annotate_pdf(
      pdf = pdf,
      query = query,
      env = env,
      verbose = verbose
    )
  }
  wait_for_gpt("attach.png", verbose = verbose)
  
  
  if (verbose) cli::cli_progress_update(status = "click on attach file and then \"upload from computer\"")
  press_png("attach.png", verbose = verbose)
  Sys.sleep(1)
  pag$press("down", presses = 3L, interval = 0.05)
  pag$press("enter")


  # type file name into selection
  if (verbose) cli::cli_progress_update(status = "type file name into selection")
  Sys.sleep(1)
  pag$write(pdf, interval = 0)
  Sys.sleep(1)
  pag$press("enter")


  # after upload, ask questions
  if (verbose) cli::cli_progress_update(status = "Asking Questions")
  pyperclip$copy(query)
  # Sys.sleep(1)
  wait_for_gpt(img = "uploaded.png", verbose = verbose)
  press_png("chat.png", verbose = verbose)
  pag$hotkey("ctrl", "v")
  pag$press("enter")

  found <- try(pag$locateOnScreen("rate-alt3.png"), silent = TRUE)
  if (!methods::is(found, "try-error")) {
    cli::cli_progress_cleanup()
    cli::cli_alert_info("rate limit reached. Waiting until {Sys.time() + 60 * 60 * 1}")
    Sys.sleep(60 * 60 * 1) # wait 3 hours
    annotate_pdf(
      pdf = pdf,
      query = query,
      env = env,
      verbose = verbose
    )
  }
  
  # wait for text generation, then save page
  if (verbose) cli::cli_progress_update(status = "Saving file")
  Sys.sleep(1)
  wait_for_gpt(img = "done.png", verbose = verbose)
  pag$hotkey("ctrl", "s")
  Sys.sleep(1)
  pag$write(pdf, interval = 0)
  pag$press("enter")

  # close window
  Sys.sleep(2)
  press_png("chat2.png", verbose = verbose)
  Sys.sleep(1)
  pag$hotkey("ctrl", "w")
  if (verbose) cli::cli_progress_done()
}


create_env <- function(env = "r-opinion") {
  if (!reticulate::virtualenv_exists(env)) {
    reticulate::virtualenv_create(env)
    reticulate::virtualenv_install(env, c(
      "pyautogui",
      "pyperclip",
      "Pillow",
      "opencv-python"
    ))
  }
}


check_env <- function(env = "r-opinion") {
  env_exists <- reticulate::virtualenv_exists(env)
  if (!env_exists) {
    if (utils::askYesNo(paste(
      "Virtual environemnt",
      env,
      "does not exists. Do you want to create it?"
    ))) {
      create_env(env = env)
    } else {
      stop(env, " does not exist")
    }
  }
  reticulate::use_virtualenv("r-opinion")
  invisible(env_exists)
}


press_png <- function(png, verbose) {
  pag <- reticulate::import("pyautogui")
  wait_for_gpt(png, verbose = verbose)
  element_location <- pag$center(pag$locateOnScreen(png))
  Sys.sleep(1)
  pag$click(x=element_location$x, y=element_location$y)
}


wait_for_gpt <- function(img = "done.png", verbose) {
  start <- Sys.time()
  if (verbose) cli::cli_progress_bar(format = "{cli::pb_spin} waiting to see {.file {img}} [{cli::pb_elapsed}]")
  pag <- reticulate::import("pyautogui")
  Sys.sleep(3)
  repeat({
    Sys.sleep(1)
    found <- try(pag$locateOnScreen(img), silent = TRUE)
    if (verbose) cli::cli_progress_update()
    if (!methods::is(found, "try-error")) {
      break
    }
    if (as.numeric(Sys.time() - start, units = "secs") > 180) {
      stop("timed out after 3 minutes")
    }
  })
}


read_results <- function(pdf) {
  if (file.exists(paste0(pdf, ".html"))) {
    json <- rvest::read_html(paste0(pdf, ".html"))|> 
      rvest::html_elements(".agent-turn .text-message") |> 
      rvest::html_text2() |> 
      str_extract("\\{.*\\}")
    if (isTRUE(nchar(json) > 1L)) {
      df <- try(jsonlite::fromJSON(json), silent = TRUE)
      if (!methods::is(df, "try-error")) {
        out <-  df |> 
          tibble::as_tibble() |> 
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "variable", 
                              values_to = "result") |> 
          mutate(file = basename(pdf))
        return(out)
      }
    }
    cli::cli_alert_warning("{paste0(pdf, \".html\")} had issues and could not be parsed")
    return(NULL)
  }  
}
