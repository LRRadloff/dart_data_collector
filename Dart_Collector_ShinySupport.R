# haven't understood the definiton of onclick completely
# source: https://stefanengineering.com/2019/07/06/delete-rows-from-shiny-dt-datatable/
get_delete_button <- function(id_stem, counter) {
  as.character(
    actionButton(
      paste(id_stem, counter, sep = "_"),
      label = NULL,
      icon = icon("trash"),
      onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'
    )
  )  
}

parse_delete_event <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}