# Räumliche Statistik: Hauspreise in Manhattan --------------------

# Laden relevanter Packages -----------------------------------------------

library(tidyverse)   # Data Cleaning
library(sf)          # Arbeiten mit Geodaten
library(tidycensus)  # Extraktion relevanter Daten
library(tmap)        # Maps
library(openxlsx)    # Export Excel
library(spatialreg)  # Rechnen Spatial-Error- und Spatial-Lag-Modelle
library(spdep)       # Nachbarschaftsmatrix
library(car)         # Extraktion Varianzinflationsfaktoren
library(stargazer)   # Regressionstabelle

# Einlesen der Daten ------------------------------------------------------

ny <- tidycensus::get_acs(
  geography = "tract",
  variables = c(
    median_value   = "B25077_001",  # Median Hauswert
    median_income  = "B19013_001",  # Median Haushaltseinkommen
    total_pop      = "B01003_001",  # Einwohneranzahl
    below_pov      = "B05010_001",  # Leben unterhalb Armutsgrenze in letzten 12 Monaten
    black_pop      = "B02001_003",  # PoC
    hisp_pop       = "B03002_012"), # Hispanics
  state = "NY",                     # Bundesstaat New York
  county = "New York",              # Manhattan
  year = 2023,                      # neuestes Jahr
  survey = "acs5",                  # American Community Survey
  geometry = T,                     # Geodaten werden ebenfalls geladen
  output = "wide"                   # Variablen als einzelne Spalten
) 


# Auswahl und Erstellen relevanter Variablen
ny_clean = ny %>% 
  select(
    median_valueE,
    median_incomeE,
    total_popE,   
    below_povE,   
    black_popE,   
    hisp_popE,
    geometry) %>% 
  mutate(
    black_ratio = black_popE/total_popE * 100, # Anteil PoC
    hisp_ratio = hisp_popE/total_popE * 100,   # Anteil Hispanics
    poverty = below_povE/total_popE * 100,     # Anteil Armut
    density = as.numeric(total_popE/st_area(geometry)*1000000) # Einwohnerdichte/km²
    ) %>% 
  select(-c(black_popE,
            hisp_popE,
            total_popE,
            below_povE))


# Erstellung deskriptiver Karten ------------------------------------------


# Median Immobilienwert

tm_shape(ny_clean)+
  tm_polygons(fill = "median_valueE",
              fill.legend = tm_legend("Median-Wert der \nImmobilien (in $)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Immobilienwerte in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.85, 
    legend.text.size  = 0.7)


# Medianeinkommen

tm_shape(ny_clean)+
  tm_polygons(fill = "median_incomeE",
              fill.legend = tm_legend("Medianeinkommen (in $)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Medianeinkommen in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.85, 
    legend.text.size  = 0.7)


# Armutsquote

tm_shape(ny_clean)+
  tm_polygons(fill = "poverty",
              fill.legend = tm_legend("Armutsquote (in %)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Armutsquote in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.8, 
    legend.text.size  = 0.7)

# Anteil an Dunkelhäutigen
tm_shape(ny_clean)+
  tm_polygons(fill = "black_ratio",
              fill.legend = tm_legend("Anteil an dunkelhäu-\ntigen Personen (in %)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Anteil an Schwarzen in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.8, 
    legend.text.size  = 0.7)


# Anteil Hispanics
tm_shape(ny_clean)+
  tm_polygons(fill = "hisp_ratio",
              fill.legend = tm_legend("Anteil an Hispanics (in %)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Anteil an Hispanics in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.8, 
    legend.text.size  = 0.7)


# Einwohnerdichte
tm_shape(ny_clean)+
  tm_polygons(fill = "density",
              fill.legend = tm_legend("Einwohnerdichte \n(in EW/km²)"),
              fill.scale = tm_scale_intervals(
                style = "quantile",
                n = 5,
                label.na = "NA"))+
  tm_title("Einwohnerdichte in Manhattan",
           fontfamily = "sans",
           size = 1.5)+
  tm_layout(
    frame = F,
    legend.position = c("left", "top"),
    legend.title.size = 0.85, 
    legend.text.size  = 0.7)


# Deskriptive Statistiken -------------------------------------------------


create_ny_stats <- function(df, variables) {
  
  # Geometrievariable entfernen
  df_no_geo <- df %>% sf::st_drop_geometry()
  
  # Berechnen deskriptiver Statistiken
  stats_wide <- df_no_geo %>%
    summarise(
      across(
        all_of(variables),
        list(
          N          = ~ sum(!is.na(.)),
          "NA"       = ~ sum(is.na(.)), 
          Min        = ~ min(., na.rm = T),
          Max        = ~ max(., na.rm = T),
          Mittelwert = ~ mean(., na.rm = T),
          Median     = ~ median(., na.rm = T),
          SD         = ~ sd(., na.rm = T),
          Q1         = ~ quantile(., 0.25, na.rm = T),
          Q3         = ~ quantile(., 0.75, na.rm = T)
        ),
        .names = "{.col}...{.fn}" 
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", ".value"),
      names_sep = "\\.\\.\\." 
    )
  
  # Formatierung
  stats_final <- stats_wide %>%
    rename(
      `μ` = Mittelwert,
      `σ` = SD,
      Q2 = Median) %>%
    mutate(
      # Berechnung des Konfidenzintervalls
      se = ifelse(N > 1, `σ` / sqrt(N), NA),
      CI_low = `μ` - qt(0.975, df = pmax(N - 1, 1)) * se,
      CI_high = `μ` + qt(0.975, df = pmax(N - 1, 1)) * se) %>%
    mutate(
      # Runden
      across(c(`μ`, Min, Q1, Q2, Q3, Max, `σ`, CI_low, CI_high), ~ round(., 2)),
      `95% KI` = ifelse(is.na(CI_low), "n.a.", 
                        paste0("[", format(CI_low, decimal.mark = ",", nsmall = 2), 
                               "; ", format(CI_high, decimal.mark = ",", nsmall = 2), "]"))) %>%
    mutate(
      # Deutsches Zahlenformat
      across(
        c(`μ`, Min, Q1, Q2, Q3, Max, `σ`),
        ~ format(., decimal.mark = ",", big.mark = ".", nsmall = 2))) %>%
    # Spaltenreihenfolge festlegen
    select(Variable, `μ`, `95% KI`, Min, Q1, Q2, Q3, Max, `σ`, N, `NA`)
  
  return(stats_final)
}


# Definition der relevanten Variablen
ny_variables <- c("median_valueE", "median_incomeE", "poverty", 
                  "black_ratio", "hisp_ratio", "density")

# Anwendung der Funktion auf Variablen
desk_tab_ny <- create_ny_stats(ny_clean, ny_variables)

# Export in Excel-Datei
desk_tab_ny %>% 
  write.xlsx("Tabellen/Deskriptive Statistiken.xlsx")


# Rechnen der Regressionsmodelle ------------------------------------------

# Entfernen der NAs
ny_final <- ny_clean %>% 
  drop_na(median_valueE, median_incomeE, poverty, black_ratio, hisp_ratio, density)

# Gewichtete Nachbarschaftsmatrix erstellen
ny_nb <- poly2nb(ny_final, queen = TRUE)
ny_listw <- nb2listw(ny_nb, style = "W", zero.policy = TRUE)

# Spezifikation der Regressionsgleichung
reg_formula <- median_valueE ~ median_incomeE + poverty + black_ratio + hisp_ratio + density

# Berechnen der 3 Modellarten
model_ols     <- lm(reg_formula, data = ny_final)
model_sar_err <- errorsarlm(reg_formula, data = ny_final, listw = ny_listw, zero.policy = TRUE)
model_sar_lag <- lagsarlm(reg_formula, data = ny_final, listw = ny_listw, zero.policy = TRUE)

# Berechnung Morans I
get_moran_df <- function(model, name, listw) {
  res <- residuals(model)
  
  mtest <- moran.test(
    res, 
    listw = listw, 
    alternative = "greater", 
    zero.policy = TRUE
  )
  
  data.frame(
    Modell = name,
    `Morans I` = round(unname(mtest$estimate["Moran I statistic"]), 4),
    `p-Wert` = round(mtest$p.value, 5)
  )
}

# Ergebnisse Morans I-Statistik in einem Dataframe
moran_results <- bind_rows(
  get_moran_df(model_ols, "OLS", ny_listw),
  get_moran_df(model_sar_err, "Spatial Error", ny_listw),
  get_moran_df(model_sar_lag, "Spatial Lag", ny_listw)
)

# Export in Excel-Datei
moran_results %>% 
  write.xlsx("Tabellen/Morans I für Regressionsmodelle.xlsx")


# Berechnung der VIF-Werte
vif_values <- car::vif(model_ols)
vif_table <- tibble(
  Variable = names(vif_values),
  VIF = round(as.numeric(vif_values), 2)
)

# Export in Excel-Datei
write.xlsx(vif_table, "Tabellen/VIF_Werte.xlsx")

# Berechnung der Lagrange-Multiplier-Statistik
extract_rs <- function(model, listw) {
  rs <- lm.RStests(model, listw, test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag"), zero.policy = TRUE)
  tibble(
    Test = c("LM_RSerr", "LM_RSlag", "adjLM_RSerr", "adjLM_RSlag"),
    Wert = c(rs$RSerr$statistic, rs$RSlag$statistic, rs$adjRSerr$statistic, rs$adjRSlag$statistic),
    p_Wert = c(rs$RSerr$p.value, rs$RSlag$p.value, rs$adjRSerr$p.value, rs$adjRSlag$p.value)
  )
}

# Anwenden der Funktion auf Modellspezifikation
rs_table <- extract_rs(model_ols, ny_listw)

# Export in Excel-Datei
write.xlsx(rs_table, "Tabellen/Lagrange_Multiplier_Test.xlsx")


# Regressionstabelle ------------------------------------------------------


# Hilfsfunktion für Signifikanzsterne
add_stars <- function(p_val) {
  if (is.na(p_val)) return("")
  if (p_val < 0.01) return("***")
  if (p_val < 0.05) return("**")
  if (p_val < 0.1)  return("*")
  return("")
}

# Berechnung der räumlichen Parameter

# Spatial Error Modell
lambda_val <- model_sar_err$lambda
lambda_se  <- model_sar_err$lambda.se
lambda_p   <- 2 * pnorm(abs(lambda_val / lambda_se), lower.tail = FALSE)

# Spatial Lag Modell
rho_val <- model_sar_lag$rho
rho_se  <- model_sar_lag$rho.se
rho_p   <- 2 * pnorm(abs(rho_val / rho_se), lower.tail = FALSE)


# Zeile für Rho (Spatial Lag)
rho_line <- c("Rho", 
              "", 
              "", 
              paste0(format(round(rho_val, 3), nsmall = 3), add_stars(rho_p)))

rho_se_line <- c("", "", "", paste0("(", format(round(rho_se, 3), nsmall = 3), ")"))

# Zeile für Lambda (Spatial Error)
lambda_line <- c("Lambda", 
                 "", 
                 paste0(format(round(lambda_val, 3), nsmall = 3), add_stars(lambda_p)),
                 "") 

lambda_se_line <- c("", "", paste0("(", format(round(lambda_se, 3), nsmall = 3), ")"), "")


# Zeile für AIC 
aic_line <- c("AIC", 
              format(round(AIC(model_ols), 2), big.mark = ".", decimal.mark = ","),
              format(round(AIC(model_sar_err), 2), big.mark = ".", decimal.mark = ","),
              format(round(AIC(model_sar_lag), 2), big.mark = ".", decimal.mark = ","))


# Erstellung der Regressionstabelle
stargazer(model_ols, model_sar_err, model_sar_lag,
          type = "html",
          out = "Tabellen/Regressionstabelle_NY.html",
          title = "Regressionsergebnisse: Hauspreise in Manhattan",
          model.names = FALSE,
          column.labels = c("OLS", "Spatial Error", "Spatial Lag"),
          style = "apsr",
          covariate.labels = c("Medianeinkommen", 
                               "Armutsquote (in %)", 
                               "Anteil Schwarze (in %)", 
                               "Anteil Hispanics (in %)", 
                               "Einwohnerdichte"),
          dep.var.labels = "Median Hauswert (in $)",
          keep.stat = c("n"), 
          add.lines = list(rho_line, rho_se_line, 
                           lambda_line, lambda_se_line,
                           aic_line),
          decimal.mark = ",",
          digit.separator = ".")










