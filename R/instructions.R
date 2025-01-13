info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  psychTestR::one_button_page(body = shiny::div(psychTestR::i18n(id, html = TRUE),
                                              style = style),
                              button_text = psychTestR::i18n("CONTINUE"))
}

instructions <- function(video_dir) {
  c(
    # psychTestR::code_block(function(state, ...) {
    #   psychTestR::set_local("do_intro", TRUE, state)
    # }),
    info_page("INTRO_TEXT"),
    #show_sample_page(video_dir),
    #show_second_sample_page(video_dir),
    practice(video_dir)
    # psychTestR::while_loop(
    #   test = function(state, ...) psychTestR::get_local("do_intro", state),
    #   logic = practice(video_dir)
    #   ),
    # psychTestR::one_button_page(psychTestR::i18n("MAIN_INTRO"),
    #                             button_text = psychTestR::i18n("CONTINUE"))
  )
}

# show_sample_page <- function(video_dir){
#   demo_sample <- "Demo-1"
#   video_url <- file.path(video_dir, sprintf("%s.mp3", demo_sample))
#   video <- get_video_element(url = video_url, autoplay = F)
#   body <- shiny::div(
#     shiny::div(psychTestR::i18n("SAMPLE1a"),
#                style = "text-align: justify; margin-left:20%;
#                margin-right:20%; margin-bottom:20px"),
#     shiny::p(video)
#   )
#   psychTestR::one_button_page(
#     body = body,
#     button_text = psychTestR::i18n("CONTINUE")
#   )
#
# }
#
# show_second_sample_page <- function(video_dir){
#   demo_sample <- "Demo-1"
#   video_url <- file.path(video_dir, sprintf("%s.mp3", demo_sample))
#   video <- get_video_element(url = video_url, autoplay = F)
#   body <- shiny::div(
#     shiny::div(psychTestR::i18n("SAMPLE1b"),
#                style = "text-align: justify; margin-left:20%;
#                margin-right:20%; margin-bottom:20px"),
#     shiny::p(video)
#   )
#   psychTestR::one_button_page(
#     body = body,
#     button_text = psychTestR::i18n("CONTINUE")
#   )
#
# }
