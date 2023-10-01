#' Function to determine the depth to bedrock based on litholog intervals that
#' are labelled as either 'Bedrock' or 'Surficial'
#'
#' This function finds the first occurrence of 'Bedrock' that is below any other
#' intervals labelled as 'Surficial' in each log. As such, this represents a
#' maximum estimate of bedrock depth; it avoids the problems with glaciotectonic
#' rafts of bedrock, but can potentially overestimate bedrock depth if some
#' intervals are misclassified as surficial with the bedrock strata.
#'
#' @param lithologs tibble of litholog data containing the column '.pred_class'
#'   which as two factor levels, 'Bedrock' and 'Surficial'.
#' @param response character, name of the response variable column, default
#' is '.pred_class'.
#' @param option character, one of c("last", "first"). Indicates whether to
#'   define the top of the bedrock based on the lowermost occurrence of
#'   predicted bedrock that occurs under all other predicted surficial units, or
#'   the uppermost occurrence of predicted bedrock. The default is "last". This
#'   makes the predicted bedrock top less susceptible to the presence of rafted
#'   bedrock rafts, although in some cases could make the predicted bedrock top
#'   too deep, if units in the bedrock have been misclassified as surficial.
#'
#' @return tibble containing the bedrock depths per well, with 'gicwellid' and
#'   '.bedrock_dep' columns.
#' @export
pick_bedrock <- function(lithologs, response = ".pred_class",
                         option = c("last", "first")) {

  option <- match.arg(option)

  if (option == "last") {
    # get the maximum depth of any units that are classified as surficial
    max_surf <- lithologs |>
      dplyr::group_by(.data$gicwellid) |>
      dplyr::filter(!!rlang::sym(response) == "Surficial") |>
      dplyr::summarise(minv = max(.data$int_top_dep))

    # take the top of the next interval beneath any surficial as the bedrock top
    y_pred <- lithologs |>
      dplyr::left_join(max_surf) |>
      dplyr::group_by(.data$gicwellid) |>
      dplyr::filter(
        .data$int_top_dep > .data$minv | is.na(.data$minv),
        !!rlang::sym(response) == "Bedrock"
      ) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::rename(.bedrock_dep = "int_top_dep") |>
      dplyr::select("gicwellid", ".bedrock_dep")

  } else if (option == "first") {
    y_pred <- lithologs |>
      dplyr::group_by(.data$gicwellid) |>
      dplyr::filter(
        !!rlang::sym(response) == "Bedrock",
        !duplicated(!!rlang::sym(response))
      ) |>
      dplyr::ungroup() |>
      dplyr::rename(.bedrock_dep = "int_top_dep") |>
      dplyr::select("gicwellid", ".bedrock_dep")
  }

  return(y_pred)
}
