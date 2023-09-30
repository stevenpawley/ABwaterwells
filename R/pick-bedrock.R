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
pick_bedrock = function(lithologs, response = ".pred_class",
                        option = c("last", "first")) {
  # check for data.table
  option = match.arg(option)
  if (!data.table::is.data.table(lithologs)) {
    lithologs = data.table::as.data.table(lithologs)
  }

  if (option == "last") {
    # take the depth as the maximum depth of any units that are classified as surficial
    max_surf = lithologs[, .SD[get("response") == "surficial"]]
    max_surf = max_surf[, .(minv = max(get("int_top_dep"))), by = "gicwellid"]

    # take the top of the next interval beneath any surficial as the bedrock top
    y_pred = merge(lithologs, max_surf, by = "gicwellid", all.x = TRUE)
    y_pred[, .SD[get("int_top_dep") > get("minv") | is.na(get("minv"))], by = "gicwellid"]
    y_pred = y_pred[, .SD[1], by = "gicwellid"]

    # rename and select columns
    data.table::setnames(y_pred, "int_top_dep", "bedrock_dep")
    y_pred = y_pred[, .SD, .SDcols = c("gicwellid", "bedrock_dep")]
  }

  if (option == "first") {
    # take the depth as the first occurrence of predicted bedrock
    y_pred = lithologs[
      get("response") == "Bedrock" & !duplicated(get("response")),
      by = "gicwellid"
    ]
    data.table::setnames(y_pred, "int_top_dep", ".bedrock_dep")
    y_pred = y_pred[, .SD, .SDcols = c("gicwellid", ".bedrock_dep")]
  }

  return(y_pred)
}
