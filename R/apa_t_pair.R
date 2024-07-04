

#' APA Text for Paired-Samples T-Test
#'
#' Create APA-formatted text for the results of a paired-samples t-test.
#'
#' @param x A numeric vector of values for level 1.
#' @param y A numeric vector of values for level 2.
#' @param dv A character string describing the dependent variable (DV) in the output statement. Default is "the DV".
#' @param level1 A character string describing level 1 in the output statement. Default is "level 1".
#' @param level2 A character string describing level 2 in the output statement. Default is "level 2".
#'
#' @return A character string with the APA-formatted results of the paired-samples t-test.
#' @export
#'
#' @examples
#' # Assuming self_res_att is a dataframe loaded in your environment
#' apa_t_pair(x = self_res_att$f_self, y = self_res_att$f_non)
#'
#' # Specify the text for DV and levels
#' apa_t_pair(x = self_res_att$f_self, y = self_res_att$f_non,
#'            dv = "preferences for female faces",
#'            level1 = "participants who resembled those faces",
#'            level2 = "non-self participants")
apa_t_pair <- function(x, y,
                       dv = "the DV",
                       level1 = "level 1",
                       level2 = "level 2") {
  t_results <- t.test(x, y, paired = TRUE)

  template <- "A paired-samples t-test was conducted to compare {dv} between {level1} (M = {mean1}, SD = {sd1}) and {level2} (M = {mean2}, SD = {sd2}). There was a {non}significant difference; t({df}) = {t_value}, p = {p_value}."

  glue::glue(
    template,
    dv      = dv,
    level1  = level1,
    level2  = level2,
    mean1   = round0(mean(x), 1),
    sd1     = round0(sd(x), 1),
    mean2   = round0(mean(y), 1),
    sd2     = round0(sd(y), 1),
    non     = ifelse(t_results$p.value < .05, "", "non-"),
    df      = round0(t_results$parameter, 0),
    t_value = round0(t_results$statistic, 2),
    p_value = round0(t_results$p.value, 3)
  )
}
