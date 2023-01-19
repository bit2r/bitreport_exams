################################################################################
## 일반현황 보고서 시각화
################################################################################
get_public <- function(tab, base_ym, mega_cd, mega_abb_nm, cty_cd, cty_nm, 
                       admi_df, dim = 2, pos_legend = "topright", expand_x = FALSE) {
  library(tidyverse)
  library(sf)
  library(cowplot)
  library(biscale)
  library(formattable)  
  library(hlireport)
  
  ##============================================================================
  ## 01. 연령대별 인구수 추이
  ##============================================================================
  tab_tot <- tab$tab_population %>% 
    filter(CTY_CD %in% cty_cd) %>%  
    group_by(BASE_YM) %>% 
    summarise(POPULATION = round(sum(POPULATION) / 10000)) %>% 
    mutate(BASE_YM = ifelse(substr(BASE_YM, 5, 6) == "12", substr(BASE_YM, 3, 4),
           paste(substr(BASE_YM, 3, 4), substr(BASE_YM, 5, 6), sep = "."))) %>% 
    mutate(BASE_YM = glue::glue("`{BASE_YM}")) %>% 
    mutate(BASE_YM = factor(BASE_YM))
  
  obj_01 <- tab$tab_population %>% 
    mutate(BASE_YM = ifelse(substr(BASE_YM, 5, 6) == "12", substr(BASE_YM, 3, 4),
                            paste(substr(BASE_YM, 3, 4), substr(BASE_YM, 5, 6), sep = "."))) %>% 
    mutate(BASE_YM = glue::glue("`{BASE_YM}")) %>%     
    filter(CTY_CD %in% cty_cd) %>%  
    mutate(BASE_YM = factor(BASE_YM),
           AGE_GROUP = factor(AGE_GROUP)) %>% 
    ggplot(aes(x = BASE_YM, y = POPULATION/10000, group = AGE_GROUP)) +
    geom_area(aes(fill = AGE_GROUP), alpha = 0.6) +
    geom_text(data = tab_tot, 
              aes(x = BASE_YM, y = POPULATION + POPULATION/18, label = POPULATION,  group = 1), 
              colour = "gray20", size = 3) + 
    labs(#title = glue::glue("연령대별 인구수 추이"), 
         x = "", y = "인구수 (만명)", fill = "") + 
    theme_minimal(base_family = "NanumSquare") +
    theme(
      axis.text.x = element_text(size = 11)
    )
  
  
  ##============================================================================
  ## 02. 1인가구 비율과 고령인구 비율 추이
  ##============================================================================
  ##----------------------------------------------------------------------------
  ## 02.01. 1인가구 비율
  ##----------------------------------------------------------------------------
  trend_household <- tab$tab_household %>% 
    filter(CTY_CD %in% cty_cd) %>%   
    mutate(BASE_YM = ifelse(substr(BASE_YM, 5, 6) == "12", substr(BASE_YM, 3, 4),
                            paste(substr(BASE_YM, 3, 4), substr(BASE_YM, 5, 6), sep = "."))) %>% 
    mutate(BASE_YM = glue::glue("`{BASE_YM}")) %>%     
    mutate(BASE_YM = factor(BASE_YM)) %>% 
    rename(!!mega_abb_nm := RT_HOUSEHOLD_TOT) %>% 
    rename(!!cty_nm := RT_HOUSEHOLD) %>% 
    pivot_longer(
      cols = c(4, 5),
      names_to = "ORG",
      values_to = "RT_HOUSEHOLD"
    ) %>% 
    mutate(ORG = factor(ORG, levels = c(mega_abb_nm, cty_nm)))
  
  if (cty_cd %in% "3611") {
    trend_household <- trend_household %>% 
      filter(ORG %in% "세종특별자치시")
  }
  
  left <- trend_household %>% 
    mutate(BASE_YM = factor(BASE_YM)) %>% 
    ggplot(aes(x = BASE_YM, y = RT_HOUSEHOLD, color = ORG)) +
    geom_line(aes(group = ORG), stat = "identity") +
    geom_point() +
    geom_text(
      data = filter(trend_household, as.integer(BASE_YM) %% 2 == 0) %>% 
        group_by(BASE_YM) %>% 
        mutate(VJUST = ifelse(RT_HOUSEHOLD < max(RT_HOUSEHOLD), 1.5, -1.5)),
      aes(x = BASE_YM, label = glue::glue("{round(RT_HOUSEHOLD, 1)}%"), vjust = VJUST), 
      show.legend = FALSE) +
    scale_color_manual(values = c("red", "blue")) +     
    # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    ylim(range(trend_household$RT_HOUSEHOLD) + (diff(range(trend_household$RT_HOUSEHOLD)) / 2.5) * c(-1, 1)) +
    labs(title = "1인가구 비율",
         x = "",
         y = "",
         color = "지역") +
    theme_minimal(base_family = "NanumSquare") +
    theme(
      # plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(size = 9),
      legend.title = element_blank(),
      legend.position = c(1,1),
      legend.direction = "horizontal",
      legend.justification = c(1, 0), 
      legend.key.width = unit(1, "lines"), 
      legend.key.height = unit(1, "lines")
    )
  
  
  ##----------------------------------------------------------------------------
  ## 02.02. 고령인구 비율
  ##----------------------------------------------------------------------------
  trend_pop_old <- tab$tab_pop_old %>% 
    filter(CTY_CD %in% cty_cd) %>%   
    mutate(BASE_YM = ifelse(substr(BASE_YM, 5, 6) == "12", substr(BASE_YM, 3, 4),
                            paste(substr(BASE_YM, 3, 4), substr(BASE_YM, 5, 6), sep = "."))) %>% 
    mutate(BASE_YM = glue::glue("`{BASE_YM}")) %>%  
    mutate(BASE_YM = factor(BASE_YM)) %>% 
    rename(!!mega_abb_nm := RT_POP_OLD_TOT) %>% 
    rename(!!cty_nm := RT_POP_OLD) %>% 
    pivot_longer(
      cols = c(4, 5),
      names_to = "ORG",
      values_to = "RT_POP_OLD"
    ) %>% 
    mutate(ORG = factor(ORG, levels = c(mega_abb_nm, cty_nm)))
  
  if (cty_cd %in% "3611") {
    trend_pop_old <- trend_pop_old %>% 
      filter(ORG %in% "세종특별자치시")
  }
  
  right <- trend_pop_old %>% 
    mutate(BASE_YM = factor(BASE_YM)) %>% 
    mutate(VJUST = ifelse(RT_POP_OLD < max(RT_POP_OLD), 1.5, -1.5)) %>%        
    ggplot(aes(x = BASE_YM, y = RT_POP_OLD, color = ORG)) +
    geom_line(aes(group = ORG), stat = "identity") +
    geom_point() +
    geom_text(
      data = filter(trend_pop_old, as.integer(BASE_YM) %% 2 == 0) %>% 
        group_by(BASE_YM) %>% 
        mutate(VJUST = ifelse(RT_POP_OLD < max(RT_POP_OLD), 1.5, -1.5)),
      aes(x = BASE_YM, label = glue::glue("{round(RT_POP_OLD, 1)}%"), vjust = VJUST), 
                show.legend = FALSE) +
    scale_color_manual(values = c("red", "blue")) +      
    # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +    
    ylim(range(trend_pop_old$RT_POP_OLD) + (diff(range(trend_pop_old$RT_POP_OLD)) / 2.5) * c(-1, 1)) +
    labs(title = "65세 이상 인구 비율",
      x = "",
      y = "",
      color = "지역") +
    theme_minimal(base_family = "NanumSquare") +
    theme(
      # plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(size = 9),
      legend.title = element_blank(),      
      legend.position = c(1,1),
      legend.direction = "horizontal",
      legend.justification = c(1, 0), 
      legend.key.width = unit(1, "lines"), 
      legend.key.height = unit(1, "lines")
   )
  
  
  ##----------------------------------------------------------------------------
  ## 02.03. 플롯 병합
  ##----------------------------------------------------------------------------
  obj_02 <- ggdraw() +
    draw_plot(left, 0, 0, 0.5, 1) +
    draw_plot(right, 0.5, 0, 0.5, 1)  
  
  
  ##============================================================================
  ## 03. 인구수 분포와 소득수준 분포
  ##============================================================================
  ##----------------------------------------------------------------------------
  ## 03.01. 인구수 분포
  ##----------------------------------------------------------------------------
  nbins <- 4  
  
  base_month <- glue::glue("{substr(base_ym, 1, 4)}년 {substr(base_ym, 5, 6)}월")
  
  p_map_pop <- map_uni_dim(admi_df, tab$tab_pop_admi, cty_cd, "POPULATION", 
              title = "인구수 분포", expand_x = expand_x)#, 
              # subtitle = glue::glue("기준: {base_month}, 단위: 명"))
  
  ##----------------------------------------------------------------------------
  ## 03.02. 소득수준 분포
  ##----------------------------------------------------------------------------
  base_month <- "2021년 12월"
  
  p_map_income <- map_uni_dim(admi_df, tab$tab_income, cty_cd, "AVE_INCOME_AMT", 
                           title = "소득수준 분포", expand_x = expand_x)#, 
                           # subtitle = glue::glue("기준: {base_month}, 단위: 만원/년"))

  
  ##----------------------------------------------------------------------------
  ## 02.03. 플롯 병합
  ##----------------------------------------------------------------------------  
  obj_03 <- ggdraw() +
    draw_plot(p_map_pop, 0, 0, 0.5, 1) +
    draw_plot(p_map_income, 0.5, 0, 0.5, 1)  
  
  
  list(obj_01 = obj_01, obj_02 = obj_02, obj_03 = obj_03)
}
