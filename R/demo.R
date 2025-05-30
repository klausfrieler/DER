#' Demo DER
#'
#' This function launches a demo for the DER.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgoldstudy@gmail.com},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}), German (\code{"de"}), Russian (\code{"ru"}) and Nederlands (\code{"nl"}).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{DER}()}.
#' @export
#'
DER_demo <- function(num_items = 3L,
                     feedback = DER::DER_feedback_with_graph(),
                     admin_password = "demo",
                     researcher_email = "longgoldstudy@gmail.com",
                     dict = DER::DER_dict,
                     language = "en",
                     ...) {
  elts <- psychTestR::join(
    DER_welcome_page(dict = dict),
    DER::DER(num_items = num_items,
             with_welcome = FALSE,
             take_training = TRUE,
             feedback = feedback,
             dict = dict,
             ...),
      DER_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Dance Emotion Recognition Test Demo",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
