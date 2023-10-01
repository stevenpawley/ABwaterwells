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
#' @param response character, name of the response variable column, default is
#'   '.pred_class'.
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
pick_bedrock = function(lithologs, response = ".pred_class",
                        option = c("last", "first")) {
  # some checks
  option = match.arg(option)
  if (!data.table::is.data.table(lithologs)) {
    lithologs = data.table::as.data.table(lithologs)
  }

  if (option == "last") {
    # get the maximum depth of any units that are classified as surficial
    max_surf = lithologs[get(response) == "Surficial"]
    max_surf = max_surf[, .(minv = max(get("int_top_dep"))), by = "gicwellid"]

    # take the top of the next interval beneath any surficial as the bedrock top
    ypred = max_surf[lithologs, on = "gicwellid"]
    ypred = ypred[, .SD[
      (get("int_top_dep") > get("minv") | is.na(get("minv"))) &
        (get(response) == "Bedrock"),
    ], by = "gicwellid"]
    ypred = ypred[, .SD[1], by = "gicwellid"]

  } else if (option == "first") {
    ypred = lithologs[, .SD[
      get(response == "Bedrock") & !duplicated(get(response))],
      by = "gicwellid"]
  }

  data.table::setnames(ypred, "int_top_dep", ".bedrock_dep")
  ypred = ypred[, .SD, .SDcols = c("gicwellid", ".bedrock_dep")]

  return(ypred)
}
