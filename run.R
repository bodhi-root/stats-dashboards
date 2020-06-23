# File: run.R
# Separate script to launch app, ensuring that the port is always the same and keeping
# the default browser (which always has problems) from opening.  You can view the
# app by navigating to:
#
# http://127.0.0.1:50000/

library(shiny)
shiny::runApp(port=50000, launch.browser=F)

