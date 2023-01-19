gradient_color <- function(x) {
  if (length(x)) return("#416ea4")
  
  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x),
                              maxColorValue = 255)
  normalized <- (x - min(x)) / (max(x) - min(x))
  
  blue_pal(normalized)
}

#' @importFrom htmltools div
bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4",
                      background = NULL) {
  bar <- htmltools::div(style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(style = list(flexGrow = 1, marginLeft = "8px",
                                       background = background), bar)
  
  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

table_d_kpi <- function(mart_kpi, base_date = "2021-03-31", platform_cd = NULL,
                        moving = c("d1", "w1", "w2", "w3", "w4", "m1")[5],
                        style = NULL) {
  metrics <- c("n_user", "n_session", "tot_timeonsite", "n_newsession") %>%
    paste(moving, sep = "_")
  
  if (is.null(platform_cd)) {
    platform_cd <- c("ONS", "HMP", "IWL", "LPS", "HLO", "HRT", "SMP")
  }
  
  tab <- mart_kpi %>%
    filter(date %in% base_date) %>%
    filter(platform %in% platform_cd) %>%
    mutate(platform = case_when(
      platform %in% "ONS" ~ "앱1",
      platform %in% "HMP" ~ "앱2",
      platform %in% "IWL" ~ "앱3",
      platform %in% "LPS" ~ "앱4",
      platform %in% "HLO" ~ "앱5",
      platform %in% "HRT" ~ "앱6",
      platform %in% "SMP" ~ "앱7")) %>%
    select(platform, starts_with(metrics)) %>%
    rename_at(metrics, function(x) substr(x, 1, nchar(x) - 3)) %>%
    mutate(avg_duration = round(tot_timeonsite / n_session),
           pct_newsession = round(n_newsession / n_session * 100),
           session_per_user = round(n_session / n_user, 1),
           newsession_color = gradient_color(pct_newsession)) %>%
    select(platform, n_user, n_session, session_per_user, avg_duration,
           pct_newsession, newsession_color) %>%
    arrange(platform)
  
  if (is.null(style)) {
    style = list(fontFamily = "NanumSquare")
  }
  
  tab %>%
    reactable(
      style = style,
      highlight = TRUE,
      outlined = TRUE,
      defaultColDef = colDef(
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        platform = reactable::colDef(name = "플랫폼", align = "left", width = 100),
        n_user = reactable::colDef(name = "활성 사용자", align = "left",
                                   cell = function(value) {
                                     width <- paste0(value / (max(tab$n_user) + 20000) * 100, "%")
                                     value <- value %>% formatC(format = "d", big.mark = ",")
                                     bar_chart(value, width = width, fill = "#008cec", background = "#e1e1e1")
                                   }),
        n_session = reactable::colDef(name = "세션수", align = "left",
                                      cell = function(value) {
                                        width <- paste0(value / (max(tab$n_session) + 20000) * 100, "%")
                                        value <- value %>% formatC(format = "d", big.mark = ",")
                                        bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
                                      }),
        session_per_user = reactable::colDef(name = "사용자당 세션수", align = "left",
                                             cell = function(value) {
                                               width <- paste0(value / (max(tab$session_per_user) + 0.5) * 100, "%")
                                               value <- value %>% formatC(format = "f", digits = 1)
                                               bar_chart(value, width = width, fill = "#32cd32", background = "#e1e1e1")
                                             }),
        avg_duration = reactable::colDef(name = "평균 세션 시간", align = "left",
                                         cell = function(value) {
                                           width <- paste0(value / max(tab$avg_duration) * 100, "%")
                                           value <- bitReport::sec2hms(value)
                                           bar_chart(value, width = width, fill = "#b070c8", background = "#e1e1e1")
                                         }),
        pct_newsession = reactable::colDef(
          name = "신규 세션",
          defaultSortOrder = "desc",
          cell = htmlwidgets::JS("function(cellInfo) {
        const sliceColor = cellInfo.row['newsession_color']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\">' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '%' + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }"),
          html = TRUE,
          align = "center",
          width = 140,
          class = "user-score"
        ),
        newsession_color = reactable::colDef(show = FALSE)
      ),
      theme = reactable::reactableTheme(
        highlightColor = "#f3fafb",
        borderColor = "hsl(0, 0%, 93%)",
        headerStyle = list(borderColor = "hsl(0, 0%, 90%)"),
        # Vertically center cells
        cellStyle = list(display = "flex", flexDirection = "column",
                         justifyContent = "center")
      ))
}

