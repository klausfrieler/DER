# scoring <- function(){
#   psychTestR::code_block(function(state,...){
#     results <- psychTestR::get_results(state = state,
#                                        complete = FALSE,
#                                        add_session_info = FALSE) %>% as.list()
#     sum_score <- sum(purrr::map_lgl(results$DER, function(x) x$correct))
#     total_score <- sum(purrr::map_dbl(results$DER, function(x) x$total_score))
#     num_same <- sum(purrr::map_dbl(results$DER, function(x) as.numeric(x$correct_answer == 2)))
#     num_questions <- length(results$DER)
#     max_score <- 2 * num_questions - num_same
#     perc_correct <- sum_score/num_questions
#     psychTestR::save_result(place = state,
#                  label = "score",
#                  value = perc_correct)
#     psychTestR::save_result(place = state,
#                              label = "num_questions",
#                              value = num_questions)
#     psychTestR::save_result(place = state,
#                             label = "total_score",
#                             value = total_score)
#     psychTestR::save_result(place = state,
#                             label = "max_score",
#                             value = max_score)
#
#   })
# }

get_prompt <- function(item_number, num_items, practice_page = FALSE) {
  if(practice_page){
    shiny::div(
      shiny::h4(psychTestR::i18n("SAMPLE_PROGRESS_TEXT"), style  = "text_align:center"),
      shiny::p(psychTestR::i18n("PRACTICE_INSTRUCTION"), style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
  }
  else{
    shiny::div(
      shiny::h4(psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(
          num_question = item_number,
          test_length = if (is.null(num_items))
            "?"
          else
            num_items
        )
      ), style  = "text_align:center"),
      shiny::p(psychTestR::i18n("ITEM_INSTRUCTION"), style = "margin-left:20%;margin-right:20%;text-align:center")
    )
  }
}



main_test <- function(num_items,
                      video_dir,
                      label,
                      dict = DER::DER_dict,
                      ...) {

  elts <- c()
  item_bank <- DER::DER_item_bank %>% filter(type == "test")
  item_sequence <- sample(1:nrow(item_bank), max(0, min(num_items, nrow(item_bank))))
  for(i in 1:length(item_sequence)){
    item <- item_bank[item_sequence[i],]
    messagef("Added item %d, stimulus = %s, correct = %s", item_sequence[i], item[1,]$video_file, item[1,]$correct)
    item_page <- DER_item(label = sprintf("q%d", i),
                          correct_answer = item[1,]$correct,
                          prompt = get_prompt(i, num_items, practice_page = FALSE),
                          video_file = item$video_file[1],
                          video_dir = video_dir,
                          practice_page = FALSE)
    elts <- psychTestR::join(elts, item_page)
  }
  elts  <- psychTestR::join(elts, scoring(sequence = item_sequence, label = label))
  elts
}

scoring <- function(sequence, label){
  psychTestR::code_block(function(state, ...){
    browser()
    res <- psychTestR::get_results(state, complete = T) %>% as.list()
    res <- res[[label]] %>% unlist()
    ground_truth <- DER::DER_item_bank[sequence, ]$correct
    perc_correct <- mean(res == ground_truth)
    score <- sum((res == ground_truth)*DER::DER_item_bank[sequence, ]$difficulty)/sum(DER::DER_item_bank[sequence, ]$difficulty)
    psychTestR::save_result(place = state,
                            label = "num_correct",
                            value = sum(res == ground_truth))
    psychTestR::save_result(place = state,
                            label = "perc_correct",
                            value = perc_correct)
    psychTestR::save_result(place = state,
                            label = "score",
                            value = score)
    psychTestR::save_result(place = state,
                            label = "num_questions",
                            value = length(sequence))

    res
  })
}

DER_welcome_page <- function(dict = DER::DER_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      # shiny::div(psychTestR::i18n("INTRO_TEXT"),
      #          style = "margin-left:0%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

DER_finished_page <- function(dict = DER::DER_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

DER_final_page <- function(dict = DER::DER_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

# show_item <- function(video_dir) {
#   function(item, ...) {
#     #stopifnot(is(item, "item"), nrow(item) == 1L)
#     item_number <- psychTestRCAT::get_item_number(item)
#     num_items <- psychTestRCAT::get_num_items_in_test(item)
#     emotion <- psychTestR::i18n(item[1,]$emotion_i18)
#     messagef("Showing item %s", item_number)
#     DER_item(
#       label = paste0("q", item_number),
#       emotion = emotion,
#       video_file = item$video_file,
#       correct_answer = item$answer,
#       prompt = get_prompt(item_number, num_items, emotion),
#       video_dir = video_dir,
#       save_answer = TRUE,
#       get_answer = NULL,
#       on_complete = NULL,
#       instruction_page = FALSE
#     )
#   }
# }
