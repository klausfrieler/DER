#' DER
#'
#' This function defines a DER (Dance Emotion Recognition)  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the DER in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the DER, consider using \code{\link{DER_demo}()}.
#' For a standalone implementation of the DER,
#' consider using \code{\link{DER_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param label (Character scalar) Label to give the DER results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

DER <- function(num_items = 12L,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                label = "DER",
                feedback = DER_feedback_with_score(),
                dict = DER::DER_dict
                ) {
  video_dir <- "https://media.gold-msi.org/test_materials/DER"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  video_dir <- gsub("/$", "", video_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) DER_welcome_page(),
     if (take_training) psychTestR::new_timeline(instructions(audio_dir),
                                                 dict = dict),
    #if (take_training) psychTestR::new_timeline(info_page("INSTRUCTIONS"),
    #                                            dict = dict),
    psychTestR::new_timeline(
      main_test(num_items = num_items,
                audio_dir = audio_dir,
                dict = dict
                ),
      dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) DER_finished_page(),
    psychTestR::end_module())
}
