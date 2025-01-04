annotate_pdf <- function(pdf, verbose = TRUE) {
  pag <- import("pyautogui")
  pyperclip <- import("pyperclip") 
  # just for my system, make sure keyboard is set to US
  pag$hotkey("ctrl", ",")

  locations <- list(
    initial_textbox = list(x=860, y=625),
    textbox = list(x=860, y=1040)
  )

  if (verbose) cli::cli_progress_step("Opening Firefox")
  system("firefox chatgpt.com")
  
  if (verbose) cli::cli_progress_step("click on attach file and then \"upload from computer\"")
  wait_for_gpt(img = "paper-annotation-gllm/attach.png")
  files_dropdown <- pag$center(pag$locateOnScreen("paper-annotation-gllm/attach.png"))
  Sys.sleep(3)
  pag$click(x=files_dropdown$x, y=files_dropdown$y)
  Sys.sleep(1)
  pag$press("down", presses = 3L, interval = 0.25)
  pag$press("enter")

  
  # type file name into selection
  if (verbose) cli::cli_progress_step("type file name into selection")
  Sys.sleep(1)
  pag$write(pdf, interval=0)
  Sys.sleep(1)
  pag$press("enter")
    
  # after upload, ask questions
  if (verbose) cli::cli_progress_step("Asking Questions 1.X")
  pyperclip$copy(
    'You annotate academic papers for the presence and use of computational tools to measure human opinion. You answer only with one of the possible answers and in the provided structure.
    
Question 1.0: Is an Opinion Measurement Tool applied or developed in this paper?
Possible answers: Yes /  No (unclear)
Coding instructions: we are only looking for the tool that is used in the research. Sometimes it is not entirely clear whether what a paper used constitutes a tool or just, e.g., a model or an alogorithm. If there is a GitHub repo or link to code present, this is usually a good indicator that the author(s) packaged the resources they used as a tool. Statements such as “you can use it on your own data” should also tilt you towards yes. In most cases, it should be pretty clear though, especially when the authors use the name of specific software.
    
Question 1.1: What is the name of the Opinion Measurement Tool (OMT) applied or developed in this paper?
Possible answers: Write in only name (separate multiple tools with ; )
    
Question 1.2: What link is reported for the tool?
Possible answers: Write in only link (- if no link is present, separate links for multiple tools with ; )
    
Question 1.3: What reference is reported for the tool?
Possible answers: Write in only reference (- if no link is present, separate references for multiple with ; )
Coding instructions: If a reference to the tool is provided, copy it from the bibliography.
    
Answer in this format:
    
{
  "Question 1.0": "No (unclear)",
  "Question 1.1": "CoreNLP",
  "Question 1.2": "https://stanfordnlp.github.io/CoreNLP/",
  "Question 1.3": "Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and David McClosky. 2014. The Stanford CoreNLP Natural Language Processing Toolkit In Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.",
}')
  Sys.sleep(1)
  wait_for_gpt(img = "paper-annotation-gllm/uploaded.png")
  pag$click(x=locations$initial_textbox$x, y=locations$initial_textbox$y)
  pag$hotkey("ctrl", "v")
  pag$press("enter")
  
  if (verbose) cli::cli_progress_step("Asking Questions 2.X")
  pyperclip$copy(
    'Question 2.0: Are there (additional) tools reviewed (e.g., in the related work section)?
Possible answers: Yes /  No (unclear)
Coding instructions: There are cases where it is not immediately clear whether what the current tool/approach is compared to is a tool according to our definition. If in doubt, include here anyway. Even if no tool is used in the paper, the author(s) might have still considered using tools and have included their names and references in the paper.

    
Question 2.1: What are the names of the reviewed opinion measurement tools?
Possible answers: Write in only name (separate multiple tools with ; )
    
Question 2.2: What link is reported for the reviewed tools?
Possible answers: Write in only link (- if no link is present, separate links for multiple tools with ; )
    
Question 2.3: What references are reported for the reviewed tool(s)?
Possible answers: Write in only reference (- if no link is present, separate references for multiple with ; )
Coding instructions: If a reference to the tool is provided, copy it from the bibliography.
    
Answer in this format:
    
{
  "Question 2.0": "No (unclear)",
  "Question 2.1": "CoreNLP",
  "Question 2.2": "https://stanfordnlp.github.io/CoreNLP/",
  "Question 2.3": "Manning, Christopher D., Mihai Surdeanu, John Bauer, Jenny Finkel, Steven J. Bethard, and David McClosky. 2014. The Stanford CoreNLP Natural Language Processing Toolkit In Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics: System Demonstrations, pp. 55-60.",
}')
  wait_for_gpt()
  pag$click(x=locations$textbox$x, y=locations$textbox$y)
  pag$hotkey("ctrl", "v")
  pag$press("enter")

  if (verbose) cli::cli_progress_step("Asking Questions 3.X")
  pyperclip$copy(
    'Question 3.1: What approach for measuring opinions (or related concepts) is used in the paper? Select all that are applicable?
Possible answers: Write in only name (separate multiple tools with ; )

    
Question 3.2: Does the computational approach identify and/or consider the specific targets of evaluations when analyzing opinions?
Possible answers: Yes /  No (unclear)
Coding instructions: We define opinions as "(1) human-generated (2) textual expressions that reflect a person\'s (3) subjective evaluation, belief, or feeling (4) about a particular entity, topic, event, or aspect thereof". This question asks whether the target of an evaluation, belief or feeling is measured. I.e., in "I like pizza" is only the positive sentiment measured, or is the target (pizza) extracted as well.
    
Question 3.3: Do the authors report validation of their opinion measurement?
Possible answers: Yes /  No (unclear)
    
Answer in this format:
    
{
  "Question 3.1": "Classic Supervised Machine Learning",
  "Question 3.2": "No (unclear)",
  "Question 3.3": "Yes",
}
  ')
  wait_for_gpt()
  pag$click(x=locations$textbox$x, y=locations$textbox$y)
  pag$hotkey("ctrl", "v")
  pag$press("enter")
  

  if (verbose) cli::cli_progress_step("Asking Questions 4.X")
  pyperclip$copy(
    'Question 4.1: What subjective evaluation, belief, or feeling is measured?
Possible answers: Write in only name (separate multiple tools with ; )
    
Question 4.2: Does the computational approach identify and/or consider the specific targets of evaluations when analyzing opinions?
Possible answers: Yes /  No (unclear)    
Question 4.3: Which particular entity, topic, event, or aspect thereof related to opinion is measured?
Possible answers: Write in only name (separate multiple tools with ; )
    
Answer in this format:
    
{
  "Question 4.1": "Assessment(s) [of/towards]",
  "Question 4.2": "No (unclear)",
  "Question 4.3": "movies",
}')
  wait_for_gpt()
  pag$click(x=locations$textbox$x, y=locations$textbox$y)
  pag$hotkey("ctrl", "v")
  pag$press("enter")


  if (verbose) cli::cli_progress_step("Asking Questions 5.X")
  pyperclip$copy(
    'Question 5.0: Does the paper mention or hint which data(set) was analysed?
Possible answers: Yes /  No (unclear)   
    
Question 5.1: Where did the dataset come from?
Possible answers: benchmark dataset (e.g., SemEval2013); Social Media; Media; Governmental texts; Online Reviews; Personal Communication (e.g., emails, messenger texts); Other (write in)
    
Question 5.2: Which natural language are the measured opinions expressed in?
Possible answers: Write in only name (separate multiple tools with ; )

Question 5.3: Which specific country does the dataset cover?
Possible answers: Write in only name (- if no country; separate multiple countries with ; )

Question 5.4: What is/are the name(s) of the data(sets) analysed in this paper?
Possible answers: Write in (- if no name is mentioned, separate multiple datasets with ; )

Question 5.5: What is/are the name(s) of other data(sets) mentioned in this paper?analysed in this paper?
Possible answers: Write in (- if no name is mentioned, separate multiple datasets with ; )

Question 5.6: What link is reported for the dataset (if any)?
Possible answers: Write in (- if no name is mentioned, separate multiple datasets with ; )

Question 5.7: What reference is reported for the dataset (if any)?
Possible answers: Write in (- if no reference is mentioned, separate multiple datasets with ; )

Answer in this format:
    
{
  "Question 5.0": "Yes",
  "Question 5.1": "benchmark dataset",
  "Question 5.2": "German",
  "Question 5.3": "Germany, Austria",
  "Question 5.4": "-",
  "Question 5.5": "SemEval2013",
  "Question 5.6": "-",
  "Question 5.7": "-",
}')
  wait_for_gpt()
  pag$click(x=locations$textbox$x, y=locations$textbox$y)
  pag$hotkey("ctrl", "v")
  pag$press("enter")

  # wait for text generation, then save page
  if (verbose) cli::cli_progress_step("Saving file")
  Sys.sleep(1)
  wait_for_gpt()
  pag$hotkey("ctrl", "s")
  Sys.sleep(1)
  pag$write(pdf, interval=0)
  pag$press("enter")
  # close window
  Sys.sleep(2)
  pag$click(x=locations$textbox$x, y=locations$textbox$y)
  pag$hotkey("ctrl", "w")  
  cli::cli_alert_success("{pdf} annotated [{prettyunits::pretty_dt(Sys.time() - start)}]")
}

wait_for_gpt <- function(img = "paper-annotation-gllm/done.png",
                         img_fallback = "paper-annotation-gllm/done2.png") {
  cli::cli_progress_bar(format = "{cli::pb_spin} waiting for GPT [{cli::pb_elapsed}]")
  pag <- import("pyautogui")
  start_time <- Sys.time()
  Sys.sleep(3)
  repeat({
    Sys.sleep(1)
    found <- try(pag$locateOnScreen(img), silent = TRUE)
    cli::cli_progress_update()
    if (!methods::is(found, "try-error")) {
      break
    }
    # after more than a minute, ChatGPT probably uses image 2
    if (Sys.time() - start > 1) {
      img <- img_fallback
    }
  })
}



