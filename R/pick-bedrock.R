pick_bedrock = function(lithologs, response = ".pred_class") {
  # get the maximum depth of any units that are classified as surficial
  max_surf = lithologs[, .SD[get("response") == "surficial"]]
  max_surf = max_surf[, .(minv = max(get("int_top_dep"))), by = "gicwellid"]

  # take the top of the next interval beneath any surficial as the bedrock top
  y_pred = merge(lithologs, max_surf, by = "gicwellid", all.x = TRUE)
  y_pred[, .SD[get("int_top_dep") > get("minv") | is.na(get("minv"))], by = "gicwellid"]
  y_pred = y_pred[, .SD[1], by = "gicwellid"]

  # rename and select columns
  data.table::setnames(y_pred, "int_top_dep", "bedrock_dep")
  y_pred = y_pred[, .SD, .SDcols = c("gicwellid", "bedrock_dep")]

  return(y_pred)
}
