#' @export
lumix <- function(..., width = 400, height = 300) {
  ip <- as.list(unlist(list(...)))

  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),

    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML("
          .fullWidth {
            width: 100%;
          }

          .module-title {
            line-height: 41px;
            color: #ffffff;
            font-size: 24px;
            text-align: center;
            vertical-align: middle;
            background-color: #22478a;
            border-radius: 4px;
          }

          .card-body {
            -webkit-box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
            box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
            -moz-box-shadow: 0 6px 12px rgba(0, 0, 0, 0.175);
          }

          .main-panel {
            margin-top: 20px;
            text-align: center;
            width: 100%;
          }

          .slicer-panel {
            width: 100%;
          }

          .my-shiny-time-input {
            text-align: center;
          }

          .input-group {
            margin: 0 auto;
          }
      ")
      )
    ),

    miniUI::gadgetTitleBar("App name"),
    miniUI::miniContentPanel(

      shiny::div(
        shiny::actionButton("connect_x", "Connect camera(s)", width = "75%"),
        style = "text-align: center;"
      ),

      shiny::tags$hr(),

      shiny::div(
        shinyTime::timeInput("time_x", "Set recording duration (hh:mm:ss)",
                             "01:00:00")
      ),

      shiny::tags$hr(),

      shiny::div(
        shiny::actionButton("start_x", "Start recording", width = "37.5%"),
        shiny::actionButton("stop", "Stop recording", width = "37.5%"),
        style = "text-align: center;"
      )
    )
  )

  server <- function(input, output, session) {
    toggleAll <- function(state = "OFF") {
      input_list <- shiny::reactiveValuesToList(input)
      to_toggle <- grepl("_x", names(input_list))
      input_list <- input_list[to_toggle]

      for (name in names(input_list)) {
        if (state == "OFF") {
          shinyjs::disable(name)
        } else {
          shinyjs::enable(name)
        }
      }
    }

    shiny::observeEvent(input$connect_x, {
      lapply(ip, connect)
    })

    shiny::observeEvent(input$start_x, {
      lapply(ip, start_recording)
      toggleAll("OFF")
    })

    shiny::observeEvent(input$stop, {
      lapply(ip, stop_recording)
      toggleAll("ON")
    })

    shiny::observeEvent(input$done, {
      lapply(ip, stop_recording)
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      lapply(ip, stop_recording)
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Stack preparation", width = width, height = height)
  )
}
