#' launch_adepro - Launches the AdEPro application
#'
#' @export
#'
#' @description
#' Starts the AdEPro application in the client's browser.
#'
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#'
#' @keywords adepro
#'
#' @details Further information on how to use this application can be found in the vignette of this package.
#'
#' @examples
#' \dontrun{
#' ## Launch application on localhost (127.0.0.1)
#' ## -------------------------------------------
#' ## By default launch_adepro starts the application on localhost
#' ## and a randomly selected port (e.g. 9876), in which case you can connect
#' ## to the running application by navigating your browser to
#' ## http://localhost:9876.
#' launch_adepro()
#'
#' ## Launch application on a different host
#' ## --------------------------------------
#' ## You can also run the application on a different host
#' ## by specifying a hostname and port. Just make sure to
#' ## use an open port on your machine. Here "open" means
#' ## that the port should not be used by another service
#' ## and the port is opened by your firewall.
#' launch_adepro(host="your-hostname", port=8888)
#'
#'
#' ## Make the application available to your coworkers
#' ## ------------------------------------------------
#' ## within your local area network even without a
#' ## dedicated Shiny server. The value set through the
#' ## host argument says to accept any connection (not just from localhost).
#' ## Then take note of your local IP (if you are under linux,
#' ## you can see it through ifconfig). Say your IP is 192.168.1.70.
#' ## Your colleagues can use your app by inserting in the address
#' ## bar of their browser 192.168.1.70:8888, i.e. your IP followed
#' ## by : and the port number you selected.
#' launch_adepro(host="0.0.0.0", port=8888)
#'
#' ## Launch application on a different browser
#' ## ----------------------------------------
#' ## To run the shiny app on a different browser than your standard browser
#' ## use the "browser" argument to set the path to the respective .exe file.
#' launch_adepro(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#'
#'
#' ## launching the application.
#' }
#'
#' @importFrom graphics barplot legend symbols matplot text polygon par rect grconvertX grconvertY lines
#' @import utils
#' @import shiny
#' @import V8
#' @importFrom shinyBS updateCollapse bsCollapse bsCollapsePanel
#' @importFrom shinyWidgets knobInput pickerInput circleButton radioGroupButtons materialSwitch
#' @importFrom shinyjs useShinyjs extendShinyjs inlineCSS click delay disable enable reset
#' @importFrom MASS eqscplot
#' @importFrom audio play audioSample
#' @importFrom shape plotcircle
#' @importFrom seriation seriate get_order
#' @import Cairo
#' @import dplyr
#' @import forcats
#' @import here
#' @importFrom readr read_csv
#' @import rlang
#' @import tidyr
#' @importFrom haven read_sas
#' @importFrom stats aggregate na.omit
#' @importFrom grDevices xy.coords
#' @return A shiny app
#'

launch_adepro <- function(host = "127.0.0.1", port = NULL, browser = NULL) {

  #Shiny options to use graphical Cairo library
  options(shiny.usecairo = TRUE)

  #Shiny options for maximal upload size
  options(shiny.maxRequestSize = 110*1024^2)

  #shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))

  #### Run Shiny App ####
  adepro_app <- shiny::shinyApp(
    ui = ui,
    server = server
  )
  # adepro_app <- system.file("app", package = "adepro")

  if (!is.null(browser)) options(browser = browser)
  shiny::runApp(adepro_app, host = host, port = port)
}
