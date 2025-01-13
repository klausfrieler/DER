get_feedback <- function(page_no, answer, correct){
  if(page_no == 1){
    return("")
  }
  key <- "INCORRECT"
  style <- "color:orange"
  if(answer == correct){
    key <- "CORRECT"
    style <- "color:green"
  }
  shiny::h3(psychTestR::i18n(key), style = style)

}


# ask_repeat <- function(prompt) {
#   psychTestR::NAFC_page(
#     label = "ask_repeat",
#     prompt = prompt,
#     choices = c("go_back", "continue"),
#     labels = lapply(c("GO_BACK", "CONTINUE"), psychTestR::i18n),
#     save_answer = FALSE,
#     arrange_vertically = FALSE,
#     on_complete = function(state, answer, ...) {
#       psychTestR::set_local("do_intro", identical(answer, "go_back"), state)
#     }
#   )
# }


get_transition_page <- function(answer, correct){
  psychTestR::one_button_page(body = shiny::div(
    get_feedback(4, answer, correct),
    shiny::p(psychTestR::i18n("MAIN_INTRO"))),
    button_text = psychTestR::i18n("CONTINUE"))
}

make_practice_page <- function(page_no, video_dir) {
  psychTestR::reactive_page(function(answer, ...) {
    get_practice_page(page_no, answer, video_dir)
  })
}

get_practice_page <- function(page_no, answer, video_dir){
  training_answers  <- DER_item_bank[DER_item_bank$type == "practice",]$correct
  example_videos <- DER_item_bank[DER_item_bank$type == "practice",]$video_file
  if(page_no == 4){
    page <- get_transition_page(answer, training_answers[3])
  }
  else{
    prompt <- shiny::div(get_feedback(page_no, answer, training_answers[page_no - 1]),
                         get_prompt(page_no, 3, practice_page = TRUE))
    page <- DER_item(label = sprintf("training%s", page_no),
                     correct_answer = training_answers[page_no],
                     prompt = prompt,
                     video_dir = video_dir,
                     video_file = example_videos[page_no],
                     practice_page = TRUE)
  }
  page
}

practice <- function(video_dir) {
  lapply(1:4, make_practice_page, video_dir) %>% unlist()
}
