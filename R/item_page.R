media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

#media_mobile_play_button <- shiny::tags$button(
#  shiny::tags$strong(psychTestR::i18n("CLICK_HERE_TO_PLAY")),
#  id = "btn_play_media",
#  style = "visibility: visible;height: 50px",
#  onclick = media_js$play_media
#)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)

get_video_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("video/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  video  <- shiny::tags$video(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(video, media_mobile_play_button)
}

get_video_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
            )
  src    <- shiny::tags$source(src = url, type = paste0("video/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  video  <- shiny::tags$video(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  video
}

video_NAFC_page_flex <- function(label,
                                 prompt,
                                 choices,
                                 video_url,
                                 correct_answer,
                                 save_answer = TRUE,
                                 get_answer = NULL,
                                 on_complete = NULL,
                                 admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  video_ui <- get_video_ui(video_url, wait = T, loop = F, width = 200)
  style <- NULL
  ui <- shiny::div(
    tagify(prompt),
    video_ui,
    psychTestR::make_ui_NAFC(as.character(1:length(choices)),
                             labels = choices,
                             hide = TRUE,
                             arrange_vertically = FALSE,
                             id = "response_ui")
    )

  validate <- function(answer, ...) !is.null(answer)
  psychTestR::page(ui = ui, label = label,
                   get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui)
}



DER_item <- function(label = "",
                     video_file,
                     correct_answer,
                     prompt = "",
                     video_dir = "",
                     # on_complete = NULL,
                     # get_answer = NULL,
                     practice_page = FALSE
){
  prompt <- shiny::div(prompt, shiny::p(video_file, style = "text-size:10px;color:blue"))
  labels <- c(psychTestR::i18n("ANGER"), psychTestR::i18n("JOY"), psychTestR::i18n("SADNESS"), psychTestR::i18n("FEAR"))
  choices <- c("anger", "joy", "sadness", "fear")
  video_url <- file.path(video_dir, video_file)
  force(correct_answer)
  # get_answer <- function(input, state, ...) {
  #   answer <- as.numeric(input$last_btn_pressed)
  #   correct <- correct_answer == answer
  #   correct_diff <- 0
  #   if(correct_answer != 2){
  #     correct_diff <- as.numeric(answer %in% c(1,3))
  #     total_score <- correct + correct_diff
  #   }
  #   else{
  #     correct_diff <- NA
  #     total_score <- as.numeric(correct)
  #   }
  #   result <- tibble(answer = answer,
  #                    correct_answer = correct_answer,
  #                    correct = correct,
  #                    correct_diff = correct_diff,
  #                    total_score = total_score,
  #                    stimulus = video_file)
  #   #print(result)
  # }
  psychTestR::video_NAFC_page(
    label = label,
    prompt = prompt,
    url = video_url,
    choices = choices,
    labels = labels,
    arrange_choices_vertically = F,
    video_width = "50%",
    save_answer = !practice_page
  )
}

