#' @export
lumix <- function(..., width = 400, height = 300) {
  user_ip <- as.list(unlist(list(...)))

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

    miniUI::gadgetTitleBar("Lumix control"),
    miniUI::miniContentPanel(

      shiny::div(
        shiny::actionButton("discover_x", "Find camera(s)", width = "37.5%"),
        shiny::actionButton("connect_x", "Connect camera(s)", width = "37.5%"),
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
        shiny::actionButton("stop_x", "Stop recording", width = "37.5%"),
        style = "text-align: center;"
      )
    )
  )

  server <- function(input, output, session) {
    toggleAll <- function(state = "OFF", exclude = "") {
      input_list <- shiny::reactiveValuesToList(input)
      to_toggle <- grepl("_x", names(input_list)) &
        !(names(input_list) %in% exclude)
      input_list <- input_list[to_toggle]

      for (name in names(input_list)) {
        if (state == "OFF") {
          shinyjs::disable(name)
        } else {
          shinyjs::enable(name)
        }
      }
    }

    ip <- reactiveVal(user_ip)
    connected <- reactiveVal()

    shiny::observeEvent(ip(), {
      if (length(ip()) > 0) {
        toggleAll("ON", exclude = c("time_x", "start_x", "stop_x"))
      } else {
        toggleAll("OFF", exclude = "discover_x")
      }
    })

    shiny::observeEvent(connected(), {
      if (length(connected()) > 0) {
        toggleAll("ON")
      } else {
        if (length(ip()) > 0) {
          toggleAll("OFF", exclude = c("discover_x", "connect_x"))
        } else {
          toggleAll("OFF", exclude = "discover_x")
        }
      }
    })

    shiny::observeEvent(input$discover_x, {
      tmp <- unique(c(ip(), discover()))
      ip(tmp)
    })

    shiny::observeEvent(input$connect_x, {
      success <- sapply(ip(), connect)
      connected(ip()[success])
    })

    shiny::observeEvent(input$start_x, {
      lapply(connected(), start_recording)
      toggleAll("OFF", exclude = "stop_x")

      duration <- lubridate::hour(input$time_x) * 3600 +
        lubridate::minute(input$time_x) * 60 +
        lubridate::second(input$time_x)

      if (duration > 0) {
        shinyjs::delay(duration * 1000, shinyjs::click("stop_x"))
      }
    })

    shiny::observeEvent(input$stop_x, {
      lapply(connected(), stop_recording)
      toggleAll("ON")
    })

    shiny::observeEvent(input$done, {
      lapply(connected(), stop_recording)
      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      lapply(connected(), stop_recording)
      shiny::stopApp()
    })
  }

  shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Stack preparation", width = width, height = height)
  )
}
