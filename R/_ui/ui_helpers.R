# UI - Helper Components
# Reusable UI component functions

# Helper function for consistent download button styling
download_button_style <- function() {
  "width:100%; background-color: #204d74; color: white"
}

# Helper function for creating info helpers with consistent styling
create_info_helper <- function(tag, title, content, icon = "circle-question") {
  helper(
    shiny_tag = tag,
    type = "inline",
    icon = icon,
    title = title,
    content = content,
    buttonLabel = "Close",
    fade = TRUE,
    size = "s"
  )
}
