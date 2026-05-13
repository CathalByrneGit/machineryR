#' Process dashboard Shiny module UI
#'
#' @param id Shiny module namespace id
#' @export
process_dashboard_ui <- function(id) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop_mac("shiny is required for the dashboard module.")
  }
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        shiny::h3("Process Dashboard"),
        shiny::uiOutput(ns("process_name"))
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::h4("State Distribution"),
        shiny::plotOutput(ns("state_chart"), height = "300px")
      ),
      shiny::column(6,
        shiny::h4("Process Flow"),
        if (requireNamespace("visNetwork", quietly = TRUE)) {
          visNetwork::visNetworkOutput(ns("flow_diagram"), height = "300px")
        } else {
          shiny::p("Install visNetwork for the flow diagram.")
        }
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Overdue Instances"),
        if (requireNamespace("DT", quietly = TRUE)) {
          DT::DTOutput(ns("overdue_table"))
        } else {
          shiny::tableOutput(ns("overdue_table_base"))
        }
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Instance History"),
        shiny::verbatimTextOutput(ns("instance_detail"))
      )
    )
  )
}

#' Process dashboard Shiny module server
#'
#' @param id Shiny module namespace id
#' @param mac_ctx_r Reactive returning a mac_context.
#' @param process_id_r Reactive returning a process_id string.
#' @export
process_dashboard_server <- function(id, mac_ctx_r, process_id_r) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop_mac("shiny is required for the dashboard module.")
  }
  shiny::moduleServer(id, function(input, output, session) {

    summary_r <- shiny::reactive({
      mac_summary(mac_ctx_r(), process_id_r())
    })

    overdue_r <- shiny::reactive({
      mac_overdue(mac_ctx_r(), process_id_r())
    })

    proc_r <- shiny::reactive({
      mac_get_process(mac_ctx_r(), process_id_r())
    })

    output$process_name <- shiny::renderUI({
      shiny::p(shiny::strong(proc_r()$name))
    })

    output$state_chart <- shiny::renderPlot({
      df <- summary_r()
      if (nrow(df) == 0) return(NULL)
      barplot(
        df$count,
        names.arg = df$state,
        main = "Instances per State",
        xlab = "State",
        ylab = "Count",
        col = "steelblue",
        las = 2
      )
    })

    output$overdue_table <- if (requireNamespace("DT", quietly = TRUE)) {
      DT::renderDT({
        df <- overdue_r()
        DT::datatable(df, selection = "single", options = list(pageLength = 10))
      })
    } else {
      shiny::renderTable({ overdue_r() })
    }

    output$overdue_table_base <- shiny::renderTable({ overdue_r() })

    selected_instance <- shiny::reactive({
      if (requireNamespace("DT", quietly = TRUE)) {
        sel <- input$overdue_table_rows_selected
        if (!is.null(sel) && length(sel) > 0) {
          df <- overdue_r()
          df$instance_id[[sel[[1]]]]
        }
      }
    })

    output$instance_detail <- shiny::renderPrint({
      iid <- selected_instance()
      if (is.null(iid)) {
        cat("Select a row to view transition history.\n")
      } else {
        hist <- mac_history(mac_ctx_r(), iid)
        print(hist)
      }
    })

    if (requireNamespace("visNetwork", quietly = TRUE)) {
      output$flow_diagram <- visNetwork::renderVisNetwork({
        proc <- proc_r()
        nodes <- data.frame(
          id = seq_along(names(proc$states)),
          label = vapply(proc$states, function(s) s$display_name, character(1)),
          group = vapply(proc$states, function(s) if (isTRUE(s$terminal)) "terminal" else "active", character(1)),
          stringsAsFactors = FALSE
        )
        state_idx <- stats::setNames(seq_along(names(proc$states)), names(proc$states))

        edges_list <- lapply(proc$transitions, function(tr) {
          data.frame(from = state_idx[[tr$from]], to = state_idx[[tr$to]],
                     label = tr$trigger$type, stringsAsFactors = FALSE)
        })
        edges <- if (length(edges_list) > 0) dplyr::bind_rows(edges_list) else data.frame(from = integer(), to = integer(), label = character())

        visNetwork::visNetwork(nodes, edges) |>
          visNetwork::visGroups(groupname = "terminal", color = "salmon") |>
          visNetwork::visGroups(groupname = "active", color = "lightblue") |>
          visNetwork::visEdges(arrows = "to") |>
          visNetwork::visLayout(randomSeed = 42)
      })
    }
  })
}
