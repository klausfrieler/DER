options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone DER
#'
#' This function launches a standalone testing session for the DER
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed.  Defaults to  TRUE
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}) and German (\code{"de"})
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param ... Further arguments to be passed to \code{\link{DER}()}.
#' @export

DER_standalone  <- function(title = NULL,
                            num_items = 20L,
                            with_id = FALSE,
                            with_feedback = FALSE,
                            with_welcome = TRUE,
                            admin_password = "conifer",
                            researcher_email = "longgoldstudy@gmail.com",
                            languages = c("en", "de"),
                            dict = DER::DER_dict,
                            validate_id = "auto",
                            take_training = FALSE,
                            ...) {
  feedback <- NULL
  if(with_feedback) {
    feedback <- DER::DER_feedback_with_graph(label = "DER")
    #feedback <- DER::DER_feedback_with_score(label = "DER")
  }
  elts <- psychTestR::join(
    if(with_welcome) DER_welcome_page(dict = dict),
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = shiny::p(psychTestR::i18n("ENTER_ID"), style = "width:60%;text-align:justify"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
      DER::DER(num_items = num_items,
               with_welcome =  FALSE,
               with_finish = FALSE,
               feedback = feedback,
               dict = dict,
               take_training = take_training,
               ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    DER_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <-
      DER::DER_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
