##==============================================================================
## User defined function
##==============================================================================

##------------------------------------------------------------------------------
## H6 title in report
##------------------------------------------------------------------------------
html_h6 <- function (msg, id = paste0("ID", stats::runif(1))) {
  htmltools::div(
    class = "section level6", 
    id = id, 
    style = "font-family: 'NanumSquare',sans-serif; font-size: 13px;
    margin-top: 0; margin-bottom: 0.5em; padding: 0; line-height: 1.2; 
    text-align: left; color: #222222;", 
    msg) %>% 
    as.character() %>% 
    cat()
}

##------------------------------------------------------------------------------
## Custome ggplot map theme
##------------------------------------------------------------------------------
theme_custom_map <- function(base_size = 11,
                             base_family = "NanumSquare",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22,
                             title_size = 16,
                             subtitle_size = 12,
                             title_margin = 7,
                             subtitle_margin = 5, ...) {
  theme_void(base_size = base_size,
             base_family = base_family,
             base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        hjust = 0, size = title_size, colour = "black",
        margin = margin(b = title_margin)),
      plot.subtitle = element_text(
        hjust = 0,
        size = subtitle_size, margin = margin(b = subtitle_margin)),
      complete = TRUE,
      ...
    )
}

##------------------------------------------------------------------------------
## Page break in report
##------------------------------------------------------------------------------
break_page_after <- function () {
  htmltools::HTML(cat(paste0(htmltools::div(class = "page-break-after",
                                            ""))))
}


##------------------------------------------------------------------------------
## 1차원 맵 플롯
##------------------------------------------------------------------------------
map_uni_dim <- function(admi_df, data_df, cty_cd, metric, nbins = 4, title, expand_x = FALSE, size = 4) {
  binn <- function(x, nbins = 4) {
    breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = nbins + 1) %>% 
      round()
    
    breaks[1] <- 0
    breaks <- breaks %>% 
      purrr::map_dbl(
        function(x) pretty(x, shrink = 0.2)[2]
      ) %>% 
      unique()
    
    BINN <- cut(x, breaks = breaks, dig.lab = 10)
    levels(BINN) <- levels(BINN) %>% 
      stringr::str_replace(",", " ~ ")%>% 
      stringr::str_replace_all("\\B(?=(\\d{3})+(?!\\d))", ",")
    
    BINN
  }
  
  merged <- admi_df %>% 
    filter(CTY_CD %in% cty_cd) %>% 
    mutate(ADMI_NM = stringr::str_replace(ADMI_NM, "([[:alpha:]]+[[:space:]])([[:alpha:]]+)", "\\2")) %>% 
    left_join(
      data_df %>% 
        select(ADMI_CD, !!metric),
      by = c("ADMI_CD")
    ) %>% 
    mutate(BINN = binn(!!sym(metric)))
  
  palette_4 <- c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C")
  
  palette_4 <- c("#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C")
  
  if (any(is.na(merged$BINN))) {
    new_binn <- factor(ifelse(is.na(merged$BINN), "0", merged$BINN))
    levels(new_binn) <- c("0", levels(merged$BINN))
    merged$BINN <- new_binn
    
    palette_4 <- c("#FFFFFF", "#FFFFCC", "#FED976", "#FEB24C", "#FD8D3C")    
  }
  
  if (expand_x) {
    x_min <- st_bbox(merged)[1] - 0.02
    x_max <- st_bbox(merged)[3] + 0.02
  } else {
    x_min <- st_bbox(merged)[1]
    x_max <- st_bbox(merged)[3]
  }
  
  ggplot() +
    geom_sf(data = merged, aes(fill = BINN), lwd = 0.3) +
    scale_fill_manual(values = palette_4, na.value = "white") +
    coord_sf(xlim  = c(x_min, x_max)) +    
    ggrepel::geom_text_repel(
      data = merged, 
      aes(label = ADMI_NM, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      family = "NanumSquare",
      size = size
    ) +    
    labs(title = glue::glue(title),
         subtitle = "여백",
         xlab = "", ylab = "") +    
    theme_custom_map(
      legend.position = "bottom",
      # subtitle_size = 15,
      subtitle_margin = -20
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +    
    theme(
      text = element_text(color = "#3A3F4A"),
      legend.title = element_blank(),
      legend.key.width = unit(1.2, 'cm'),
      legend.key.height = unit(0.25, 'cm'),   
      plot.title = element_text(
        face = "bold", size = 14, margin = margin(t = 5), hjust = 0.030),
      plot.subtitle = element_text(
        color = "white", face = "bold", size = 10, margin = margin(t = 0), hjust = 1))  
}



