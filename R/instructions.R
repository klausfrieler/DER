info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%;margin-bottom:1em") {
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
    show_sample_page(video_dir, page_no = 1),
    show_sample_page(video_dir, page_no = 2),
    #show_second_sample_page(video_dir),
    practice(video_dir, num_samples = 1)
    # psychTestR::while_loop(
    #   test = function(state, ...) psychTestR::get_local("do_intro", state),
    #   logic = practice(video_dir)
    #   ),
    # psychTestR::one_button_page(psychTestR::i18n("MAIN_INTRO"),
    #                             button_text = psychTestR::i18n("CONTINUE"))
  )
}
show_sample_page <- function(video_dir, page_no = c(1, 2)){
  demo_sample <- DER_item_bank[DER_item_bank$type == "practice",]$video_file[page_no]
  video_url <- file.path(video_dir, demo_sample)
  video <- get_video_element(url = video_url, autoplay = T)
  body <- shiny::div(
    shiny::div(psychTestR::i18n(sprintf("SAMPLE1%s", c("a", "b")[page_no])),
               style = sprintf("text-align: %s; margin-left:20%%;margin-right:20%%; margin-bottom:20px",
                               c("justify", "center")[page_no])),
    shiny::p(video)
  )
  psychTestR::one_button_page(
    body = body,
    button_text = psychTestR::i18n("CONTINUE")
  )

}
