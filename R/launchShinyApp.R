launchShinyApp <-
function() {
    appDir <- system.file("shinyapp", package = "xMWAS")
    if (appDir == "") {
        stop("Could not find shinyapp directory. Try re-installing `xMWAS`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")
}
