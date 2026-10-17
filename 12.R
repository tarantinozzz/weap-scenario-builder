# Public demonstration using synthetic configuration only.
# Start from the folder containing 12.R and README.md.
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(openxlsx)
library(Rcpp)
library(rio)
library(rJava)
library(dplyr)
library(tidyr)
library(stringr)

csv_path <- file.path("data", "inputs.csv")
if (!file.exists(csv_path)) stop("Place authorized inputs.csv in the local data folder.")

csv_path <- normalizePath(csv_path, winslash = "/", mustWork = TRUE)

exclusive_label <- "Example parameter 010"

input_data <- read.csv(csv_path, fileEncoding = "latin1", fill = TRUE, header = TRUE)

lookup_path <- file.path(dirname(csv_path), "naming_lookup.csv")
lookup_data <- read.csv(lookup_path, fileEncoding = "UTF-8", header = TRUE)

lookup_abbr <- function(val) {
  m <- which(as.character(lookup_data$Trigger) == as.character(val))
  if (!length(m)) return("")
  abbr <- as.character(lookup_data$Abbr[m[1]])
  if (endsWith(abbr, "_")) paste0(abbr, as.character(lookup_data$CalSim[m[1]]))
  else abbr
}

compute_name <- function(vals) {
  pieces <- character(0)
  for (i in seq_len(nrow(input_data))) {
    rule <- input_data$Name_Rule[i]
    if (is.na(rule) || rule == "") next
    if (!(rule %in% c("changed", "demand")) && as.character(vals[i]) == "0") next
    piece <- ""
    sep   <- ifelse(is.na(input_data$Name_Sep[i]), "", input_data$Name_Sep[i])
    
    if (rule == "raw") piece <- vals[i]
    if (rule == "lookup") piece <- lookup_abbr(vals[i])
    if (rule == "math") {
      parts <- strsplit(input_data$Name_Param[i], "\\|")[[1]]
      op  <- substr(parts[1], 1, 1)
      num <- as.numeric(substr(parts[1], 2, nchar(parts[1])))
      suf <- parts[2]
      x <- as.numeric(vals[i])
      res <- if (op == "*") x * num else x / num
      piece <- paste0(res, suf)
    }
    if (rule == "binary") { if (vals[i] == "1") piece <- input_data$Name_Param[i] }
    if (rule == "suffix") piece <- paste0(vals[i], input_data$Name_Param[i])
    if (rule == "changed") {
      if (as.character(vals[i]) != as.character(input_data$default[i]))
        piece <- input_data$Name_Param[i]
    }
    if (rule == "gcm") {
      n <- as.integer(vals[i])
      piece <- paste0(input_data$Name_Param[i], sprintf("%02d", n))
    }
    if (rule == "demand") {
      if (vals[i] == "1") piece <- input_data$Name_Param[i]
      else {
        grp <- input_data$Group[i]
        g <- which(input_data$Group == grp)
        cons_rows <- setdiff(g, i)
        on <- cons_rows[vals[cons_rows] == "1"]
        if (length(on)) {
          shorts <- input_data$Name_Param[on]
          piece <- paste0("DM", paste(shorts, collapse = "-"))
        }
      }
      if (piece != "") piece <- paste0("_", piece)
    }
    if (piece != "") pieces <- c(pieces, paste0(piece, sep))
  }
  name <- paste0(pieces, collapse = "")
  name <- gsub("\\+_", "_", name)
  sub("[+_]+$", "", name)
}

branch_notes <- setNames(as.list(input_data$Note), input_data$Branch)
branch_variables <- tapply(input_data$Variable,
                           sapply(strsplit(input_data$Branch, "\\\\"), `[`, 1),
                           unique)

headers <- unique(input_data$Header)
ui_rows <- which(as.logical(input_data$UI))
trigger_rules <- input_data[!is.na(input_data$Trigger_Branch) & input_data$Trigger_Branch != "", ]

cb_groups <- list()
if ("Group" %in% colnames(input_data)) {
  for (g in unique(input_data$Group[!is.na(input_data$Group) & input_data$Group != ""]))
    cb_groups[[g]] <- which(input_data$Group == g)
}
cb_group_skip    <- unlist(lapply(cb_groups, function(idx) idx[-1]))
cb_group_members <- unlist(cb_groups)

group_of_branch <- function(branch) {
  bi <- which(input_data$Branch == branch)
  if (length(bi) && !is.na(input_data$Type[bi]) && input_data$Type[bi] == "checkbox_group")
    input_data$Group[bi] else NA
}
group_default_checked <- function(gi) !is.na(input_data$default[gi]) && input_data$default[gi] == 1

branch_short <- function(branch) {
  bi <- which(input_data$Branch == branch)[1]
  if (is.na(bi)) branch else input_data$Branch_2[bi]
}

non_group_choices <- input_data$Branch_2[setdiff(ui_rows, cb_group_members)]
search_choices <- c("", non_group_choices, names(cb_groups))

ui <- fluidPage(
  theme = shinytheme("slate"),
  tags$head(tags$script(HTML("
    $(document).on('change', 'input[type=radio][data-shiny-group]', function() {
      Shiny.setInputValue('radio_group_' + $(this).data('shiny-group'), $(this).val(), {priority:'event'});
    });
  "))),
  tags$style(HTML("
    .radio-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:10px 16px; }
    .radio-grid label { display:flex; align-items:center; gap:6px; font-weight:normal; margin:0; }
    .radio-grid-wrapper { padding-bottom:14px; }
    .radio-col label { font-weight:normal; }
    .flag-row { background:#5c2b2b; border-left:4px solid red; padding:4px; }
    .scn-branch { font-size:12px; }
    .scn-var .form-group { margin-bottom:0; }
    .scn-var select { font-size:11px; padding:2px 4px; height:auto; }
    .del-btn { color:red; border:none; background:none; font-weight:bold;
               padding:2px 6px; font-size:13px; }
  ")),
  titlePanel("Request Form"),
  tabsetPanel(
    tabPanel("Base Scenario", br(),
             uiOutput("base_table"), br(),
             actionButton("save_base", "Save Base Scenario")),
    tabPanel("Add Scenarios", br(),
             selectizeInput("branch_search", "Search Branch", choices = search_choices,
                            selected = "",
                            options = list(placeholder = "Type to search branch...", create = FALSE)),
             actionButton("add_row", "Add Row"), br(), br(),
             tags$div(id = "rows_container"), br(),
             actionButton("add_scenario", "Add Scenario"),
             actionButton("done", "Done")),
    tabPanel("Run WEAP", br(),
             textInput("weap_area", "WEAP Area", value = "", placeholder = "Enter your local WEAP area name"),
             textInput("end_year", "End year", placeholder = "e.g. 2021"),
             numericInput("num_vbs", "Number of VBS files", value = 1, min = 1),
             textInput("run_label", "Run label", placeholder = "e.g. myLabel"),
             actionButton("run_weap", "Run WEAP")
    )
  )
)

server <- function(input, output, session) {
  
  base_scenario <- reactiveVal(NULL)
  all_scenarios <- reactiveVal(list())
  scenario_count <- reactiveVal(1)
  flagged_rows  <- reactiveVal(c())
  last_export   <- reactiveVal(NULL)

  output$base_table <- renderUI({
    flagged <- flagged_rows()
    panels <- lapply(headers, function(h) {
      rids <- setdiff(intersect(which(input_data$Header == h), ui_rows), cb_group_skip)
      if (!length(rids)) return(NULL)
      body <- lapply(rids, function(i) {
        default_i <- ifelse(is.na(input_data$default[i]), "", as.character(input_data$default[i]))
        w <- switch(input_data$Type[i],
                    "binary" = materialSwitch(paste0("base_expr_", i), label = NULL,
                                              value = default_i == "1", status = "success"),
                    "dropdown" = selectInput(paste0("base_expr_", i), label = NULL,
                                             choices = c("", trimws(strsplit(as.character(input_data$Choices[i]), ",")[[1]])),
                                             selected = default_i),
                    "radio_grid" = {
                      vals <- trimws(strsplit(as.character(input_data$Choices[i]), ",")[[1]])
                      labs <- trimws(strsplit(as.character(input_data$Labels[i]),  ",")[[1]])
                      div(class = "radio-grid-wrapper",
                          div(class = "radio-grid",
                              mapply(function(val, lab) tags$label(
                                tags$input(type = "radio", name = paste0("base_expr_", i),
                                           `data-shiny-group` = paste0("grid_", i), value = val,
                                           checked = if (!is.na(input_data$default[i]) && default_i == val) "checked"),
                                lab), vals, labs, SIMPLIFY = FALSE)))
                    },
                    "checkbox_group" = {
                      grp <- input_data$Group[i]; g <- cb_groups[[grp]]
                      div(class = "radio-col",
                          lapply(g, function(gi)
                            checkboxInput(paste0("cb_", gi), label = input_data$Branch_2[gi],
                                          value = group_default_checked(gi))))
                    },
                    textInput(paste0("base_expr_", i), label = NULL, value = default_i,
                              placeholder = "Enter expression...")
        )
        lab <- if (!is.na(input_data$Type[i]) && input_data$Type[i] == "checkbox_group")
          input_data$Group[i] else input_data$Branch_2[i]
        div(class = if (i %in% flagged) "flag-row" else "",
            fluidRow(
              column(5, p(lab, style = "font-size:12px; margin-top:6px;")),
              column(5, w),
              column(2, actionButton(paste0("info_", i), "\u2139\ufe0f",
                                     style = "background:none;border:none;color:#5b9bd5;font-size:16px;"))))
      })
      tagList(h4(h, style = "background:#3a3f44; padding:8px; margin-top:15px;"),
              do.call(tagList, body))
    })
    do.call(tagList, panels)
  })

  observe({
    for (grp in names(cb_groups)) {
      g <- cb_groups[[grp]]
      excl <- g[which(input_data$Branch_2[g] == exclusive_label)]
      others <- setdiff(g, excl)
      if (length(excl)) local({
        e <- excl; o <- others
        observeEvent(input[[paste0("cb_", e)]], {
          if (isTRUE(input[[paste0("cb_", e)]]))
            for (oo in o) updateCheckboxInput(session, paste0("cb_", oo), value = FALSE)
        }, ignoreInit = TRUE)
      })
      for (oo in others) local({
        this <- oo; e <- excl
        observeEvent(input[[paste0("cb_", this)]], {
          if (isTRUE(input[[paste0("cb_", this)]]) && length(e))
            updateCheckboxInput(session, paste0("cb_", e), value = FALSE)
        }, ignoreInit = TRUE)
      })
    }
  })

  observe({
    lapply(seq_len(nrow(trigger_rules)), function(r) {
      i <- which(input_data$Branch == trigger_rules$Branch[r])
      ti <- which(input_data$Branch == trigger_rules$Trigger_Branch[r])
      if (!length(i) || !length(ti)) return()
      observeEvent(input[[paste0("base_expr_", ti)]], {
        tval <- as.character(input[[paste0("base_expr_", ti)]])
        if (is.null(tval) || tval == "") return()
        when <- as.character(trigger_rules$Trigger_When[r])
        met  <- if (startsWith(when, "NOT")) tval != sub("NOT", "", when) else tval == when
        
        dep <- input[[paste0("base_expr_", i)]]; fl <- flagged_rows()
        if (met) {
          if (is.null(dep) || dep == "" || dep == "0") {
            showNotification(paste("Please set", input_data$Branch_2[i],
                                   "to a non-zero value first."), type = "warning")
            updateSelectInput(session, paste0("base_expr_", ti), selected = "0")
          } else flagged_rows(setdiff(fl, i))
        } else flagged_rows(setdiff(fl, i))
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })

  observe({
    lapply(seq_len(nrow(input_data)), function(i)
      observeEvent(input[[paste0("info_", i)]], {
        showModal(modalDialog(title = "Note",
                              HTML(gsub("\n", "<br/>", input_data$Note[i])),
                              easyClose = TRUE, footer = modalButton("Close")))
      }))
  })

  read_base_value <- function(i) {
    if (!as.logical(input_data$UI[i]))
      return(ifelse(is.na(input_data$default[i]), "", as.character(input_data$default[i])))
    ty <- input_data$Type[i]
    if (!is.na(ty) && ty == "checkbox_group") {
      v <- input[[paste0("cb_", i)]]
      if (is.null(v)) return(ifelse(group_default_checked(i), "1", "0"))
      return(if (isTRUE(v)) "1" else "0")
    }
    if (!is.na(ty) && ty == "radio_grid") {
      v <- input[[paste0("radio_group_grid_", i)]]
      return(if (is.null(v)) ifelse(is.na(input_data$default[i]), "", as.character(input_data$default[i])) else as.character(v))
    }
    if (!is.na(ty) && ty == "binary") {
      v <- input[[paste0("base_expr_", i)]]
      return(if (is.null(v)) ifelse(is.na(input_data$default[i]), "0", as.character(input_data$default[i]))
             else ifelse(isTRUE(v), "1", "0"))
    }
    v <- input[[paste0("base_expr_", i)]]
    if (is.null(v) || is.na(v)) ifelse(is.na(input_data$default[i]), "", as.character(input_data$default[i]))
    else as.character(v)
  }
  
  observeEvent(input$save_base, {
    base <- input_data[, c("Branch", "Variable")]
    base$Scenario_1 <- vapply(seq_len(nrow(input_data)), read_base_value, character(1))
    base_scenario(base)
    showNotification("Base scenario saved!", type = "message")
  })

  rid_counter <- reactiveVal(0)
  rows <- reactiveVal(data.frame(rid = integer(), Branch = character(),
                                 Variable = character(), stringsAsFactors = FALSE))
  
  scn_widget <- function(i, rid) {
    wid <- paste0("scn_expr_", rid)
    default_i <- ifelse(is.na(input_data$default[i]), "", as.character(input_data$default[i]))
    switch(input_data$Type[i],
           "binary" = materialSwitch(wid, label = NULL,
                                     value = default_i == "1", status = "success"),
           "dropdown" = selectInput(wid, label = NULL,
                                    choices = c("", trimws(strsplit(as.character(input_data$Choices[i]), ",")[[1]])),
                                    selected = default_i),
           textInput(wid, label = NULL, value = default_i, placeholder = "Enter expression...")
    )
  }
  
  add_branch <- function(branch) {
    if (branch %in% rows()$Branch) return()
    rid <- rid_counter() + 1; rid_counter(rid)
    top <- strsplit(branch, "\\\\")[[1]][1]
    bi  <- which(input_data$Branch == branch)[1]
    var0 <- input_data$Variable[bi]
    rows(rbind(rows(), data.frame(rid = rid, Branch = branch,
                                  Variable = var0, stringsAsFactors = FALSE)))
    grp <- group_of_branch(branch)
    if (!is.na(grp)) {
      g <- cb_groups[[grp]]
      row_ui <- div(id = paste0("row_", rid),
                    fluidRow(
                      column(4, p(grp, class = "scn-branch")),
                      column(5, div(class = "radio-col",
                                    lapply(g, function(gi)
                                      checkboxInput(paste0("scn_cb_", rid, "_", gi),
                                                    label = input_data$Branch_2[gi],
                                                    value = group_default_checked(gi))))),
                      column(2, actionButton(paste0("info_r_", rid), "\u2139\ufe0f",
                                             style = "background:none;border:none;color:#5b9bd5;font-size:16px;")),
                      column(1, actionButton(paste0("del_", rid), "X", class = "del-btn"))))
      excl <- g[which(input_data$Branch_2[g] == exclusive_label)]
      others <- setdiff(g, excl)
      if (length(excl)) local({
        rr <- rid; e <- excl; o <- others
        observeEvent(input[[paste0("scn_cb_", rr, "_", e)]], {
          if (isTRUE(input[[paste0("scn_cb_", rr, "_", e)]]))
            for (oo in o) updateCheckboxInput(session, paste0("scn_cb_", rr, "_", oo), value = FALSE)
        }, ignoreInit = TRUE)
      })
      for (oo in others) local({
        rr <- rid; this <- oo; e <- excl
        observeEvent(input[[paste0("scn_cb_", rr, "_", this)]], {
          if (isTRUE(input[[paste0("scn_cb_", rr, "_", this)]]) && length(e))
            updateCheckboxInput(session, paste0("scn_cb_", rr, "_", e), value = FALSE)
        }, ignoreInit = TRUE)
      })
    } else {
      row_ui <- div(id = paste0("row_", rid),
                    fluidRow(
                      column(4, p(branch_short(branch), class = "scn-branch")),
                      column(2, div(class = "scn-var",
                                    selectInput(paste0("var_", rid), label = NULL,
                                                choices = branch_variables[[top]], selected = var0))),
                      column(3, scn_widget(bi, rid)),
                      column(2, actionButton(paste0("info_r_", rid), "\u2139\ufe0f",
                                             style = "background:none;border:none;color:#5b9bd5;font-size:16px;")),
                      column(1, actionButton(paste0("del_", rid), "X", class = "del-btn"))))
    }
    insertUI("#rows_container", where = "beforeEnd", ui = row_ui)
    
    observeEvent(input[[paste0("del_", rid)]], {
      removeUI(paste0("#row_", rid))
      rows(rows()[rows()$rid != rid, , drop = FALSE])
    }, ignoreInit = TRUE, once = TRUE)
    
    observeEvent(input[[paste0("info_r_", rid)]], {
      bi2 <- which(input_data$Branch == branch)[1]
      showModal(modalDialog(title = "Note",
                            HTML(gsub("\n", "<br/>", input_data$Note[bi2])),
                            easyClose = TRUE, footer = modalButton("Close")))
    })
  }
  
  observeEvent(input$add_row, {
    req(input$branch_search)
    s <- input$branch_search
    branch <- if (s %in% names(cb_groups)) input_data$Branch[cb_groups[[s]][1]]
    else input_data$Branch[which(input_data$Branch_2 == s)[1]]
    add_branch(branch)
    dep <- trigger_rules[trigger_rules$Trigger_Branch == branch, ]
    for (d in seq_len(nrow(dep))) add_branch(dep$Branch[d])
    
    ti <- which(input_data$Branch == branch & !is.na(input_data$Trigger_Branch) &
                  input_data$Trigger_Branch != "")
    for (t in ti) add_branch(input_data$Trigger_Branch[t])
  })

  observeEvent(input$add_scenario, {
    req(nrow(rows()) > 0, !is.null(base_scenario()))
    cur <- rows()
    scenario_count(scenario_count() + 1)
    sname <- paste0("Scenario_", scenario_count())
    base <- base_scenario(); fs <- base[, c("Branch", "Variable")]; fs[[sname]] <- base$Scenario_1
    
    done_groups <- c()
    for (k in seq_len(nrow(cur))) {
      rid <- cur$rid[k]; branch <- cur$Branch[k]; grp <- group_of_branch(branch)
      bi <- which(input_data$Branch == branch)[1]
      if (!is.na(grp)) {
        if (grp %in% done_groups) next
        done_groups <- c(done_groups, grp)
        for (gi in cb_groups[[grp]]) {
          v <- input[[paste0("scn_cb_", rid, "_", gi)]]
          val <- if (isTRUE(v)) "1" else "0"
          mr <- which(fs$Branch == input_data$Branch[gi] & fs$Variable == input_data$Variable[gi])
          if (length(mr)) fs[[sname]][mr] <- val
        }
      } else {
        wid <- paste0("scn_expr_", rid)
        v <- input[[wid]]
        expr <- if (is.null(v)) ""
        else if (!is.na(input_data$Type[bi]) && input_data$Type[bi] == "binary")
          (if (isTRUE(v)) "1" else "0")
        else as.character(v)
        vr <- input[[paste0("var_", rid)]]; if (is.null(vr)) vr <- cur$Variable[k]
        if (expr != "") {
          mr <- which(fs$Branch == branch & fs$Variable == vr)
          if (length(mr)) fs[[sname]][mr] <- expr
          else { nr <- data.frame(Branch = branch, Variable = vr, stringsAsFactors = FALSE)
          nr[[sname]] <- expr; fs <- rbind(fs, nr) }
        }
      }
    }
    sc <- all_scenarios(); sc[[sname]] <- fs; all_scenarios(sc)
    for (rid in rows()$rid) removeUI(paste0("#row_", rid))
    rows(data.frame(rid = integer(), Branch = character(),
                    Variable = character(), stringsAsFactors = FALSE))
    showNotification(paste(sname, "saved!"), type = "message")
  })

  observeEvent(input$done, {
    req(!is.null(base_scenario()))
    all_data <- base_scenario()
    for (s in all_scenarios())
      all_data <- merge(all_data, s, by = c("Branch", "Variable"), all = TRUE)
    if (ncol(all_data) > 3)
      for (col in 4:ncol(all_data)) {
        na <- is.na(all_data[[col]]); all_data[[col]][na] <- all_data[[3]][na]
      }
    all_data[] <- lapply(all_data, function(c)
      if (is.list(c)) vapply(c, function(x) if (is.null(x)) "" else as.character(x[1]), character(1)) else c)
    name_map <- c(Scenario_1 = "Base")
    for (nm in names(all_scenarios())) {
      s <- all_scenarios()[[nm]]
      vals <- s[[nm]][match(input_data$Branch, s$Branch)]
      name_map[nm] <- compute_name(vals)
    }
    for (nm in names(name_map))
      names(all_data)[names(all_data) == nm] <- name_map[nm]
    fn <- file.path(dirname(csv_path),
                    paste0("Input_Params_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    write.csv(all_data, fn, row.names = FALSE)
    last_export(fn)
    showNotification(paste("Saved as", fn), type = "message")
  })
  
 observeEvent(input$run_weap, {
    fn <- last_export()
    if (is.null(fn) || !file.exists(fn)) {
      showNotification("No export found. Click Done first.", type = "error")
      return()
    }
    
    label <- input$run_label          
    if (is.null(label) || trimws(label) == "") {
      showNotification("Enter a run label first.", type = "error")
      return()
  }
    vbs_base <- paste0("VBS_", trimws(label))
    area10 <- substr(trimws(input$weap_area), nchar(trimws(input$weap_area)) - 9, nchar(trimws(input$weap_area)))
    ver_folder <- paste0(area10, " WEAP Version")
    run_folder <- paste0(format(Sys.Date(), "%Y-%m-%d"), " ", trimws(label))
    out_dir <- paste("results",
                     format(Sys.Date(), "%Y"), ver_folder, run_folder, sep = "\\")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_dir <- normalizePath(out_dir, winslash = "\\", mustWork = TRUE)
    file.copy(fn, file.path(out_dir, basename(fn)), overwrite = TRUE)
    stopApp(list(
      output_dir    = paste0(out_dir, "\\"),
      Input_CSVfile = fn,
      WEAP_Area     = trimws(input$weap_area),
      End_year      = trimws(input$end_year),
      Num_VBS_Files = input$num_vbs,
      VBS_BaseName  = vbs_base
    ))
 })
}
vals <- runApp(shinyApp(ui, server))
output_dir    <- vals$output_dir
Input_CSVfile <- vals$Input_CSVfile
WEAP_Area     <- vals$WEAP_Area
End_year      <- vals$End_year
Num_VBS_Files <- vals$Num_VBS_Files
VBS_BaseName  <- vals$VBS_BaseName

WEAP_OuputNamePrefix = paste0('DRAFT ', WEAP_Area, "_")
WEAP_OuputNamePrefix <- gsub("Example Model", "WEAP", WEAP_OuputNamePrefix)

output_description = paste('Model:', WEAP_Area, '; Model Run:', Sys.Date(), '; Model Output:', Sys.Date(), '; WEAP Solver: LPSolve (standard); Status:  Preliminary')

setwd(output_dir)

Inputs <- read.csv(file = Input_CSVfile, check.names = FALSE, colClasses = "character")

if (Num_VBS_Files <= 1) {
  VBS_Filenames <- paste0(VBS_BaseName, ".vbs")
} else {
  VBS_Filenames <- paste0(VBS_BaseName, "_", seq_len(Num_VBS_Files), ".vbs")
}

for (vf in VBS_Filenames) {
  VBSfile <- file(vf)
  writeLines(c('Set WEAP = CreateObject("WEAP.WEAPApplication")',
               'WEAP.AutoCalc = TRUE',
               'WEAP.Verbose = 0',
               'WEAP.Logfile = WEAP.Directory + "WeapErrors.txt"',  
               paste('WEAP.ActiveArea = "', WEAP_Area,'"', sep = ""),
               paste('WEAP.EndYear =', End_year, sep = "")
  ), VBSfile)
  close(VBSfile)
}

pick_vbs <- function(run_col_index, vbs_files) {
  
  idx <- ((run_col_index - 3) %% length(vbs_files)) + 1
  vbs_files[idx]
}

for(i in 4:ncol(Inputs)) { 
  target_vbs <- pick_vbs(i-1, VBS_Filenames)
  
  write('\'==============================', file = target_vbs, append=TRUE)
  write( paste('\'RUN NUMBER ',i-3), file = target_vbs, append=TRUE, sep = "")
  write('\'==============================', file = target_vbs, append=TRUE)

  write('WEAP.ActiveScenario = "Current Accounts"', file = target_vbs, append=TRUE) 
  for(j in 1:nrow(Inputs)) { 
    write( paste('WEAP.Branch("',Inputs[j,1],'").Variables("',Inputs[j,2],'").Expression = "',Inputs[j,i], '"', sep = ""), 
           file = target_vbs, append=TRUE, sep = "") 
  }

  write('WEAP.ActiveScenario = "Example Future"', file = target_vbs, append=TRUE) 
  for(j in 1:nrow(Inputs)) { 
    if (Inputs[j,2]!='Initial Storage'){
      write( paste('WEAP.Branch("',Inputs[j,1],'").Variables("',Inputs[j,2],'").Expression = "',Inputs[j,i], '"', sep = ""), 
             file = target_vbs, append=TRUE, sep = "") 
    }
  }  

  write('WEAP.View =  "Scenario Explorer"', file = target_vbs, append=TRUE)
  write('WEAP.LoadOverview("Example Summary")', file = target_vbs, append=TRUE)
  write(paste('CALL WEAP.ExportResults("', output_dir,WEAP_OuputNamePrefix,  'Summary_',colnames(Inputs)[i], '.csv", FALSE, TRUE)', sep = ""), 
        file = target_vbs, append=TRUE)
  write('WEAP.LoadOverview("Example Details")', file = target_vbs, append=TRUE)
  write(paste('CALL WEAP.ExportResults("', output_dir,WEAP_OuputNamePrefix,  'AllData_',colnames(Inputs)[i], '.csv", FALSE, TRUE)', sep = ""), 
        file = target_vbs, append=TRUE)
}

