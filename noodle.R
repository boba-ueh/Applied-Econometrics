#Similar summary function
sjPlot::tab_model(model, show.est = TRUE, show.se = TRUE, show.stat = TRUE, show.p = TRUE,show.ci = FALSE)

# QUICK DATA DISCRIPTION
# na.omit
psych::describe((data <- na.omit(read.csv("https://kinhteluong.online/esdata/wage/wage1.csv"))[c("wage","age")]), fast = TRUE)

table(read.csv("https://kinhteluong.online/esdata/wage/wage1.csv")$social)

par(mar=c(7.1, 6.1, 4.1, 3.1))
hist(data$wage, main = "Histogram of Wages", xlab = "Wage (thousand/month)", breaks = 100, freq = TRUE, col = "yellow")

barplot(table(data$wage), main = "Bar Plot of Wages", xlab = "Wage (thousand/month)", ylab = "Frequency", col = "blue")

plot(data$wage, data$age, main = "Scatter Plot of Wage vs Age", xlab = "Wage (thousand/month)", ylab = "Age", col = "red", pch = 19) # pch=19 for solid circles

boxplot(data$wage, main = "Box Plot of Wages", xlab = "Wage (thousand/month)", col = "green")

plot(data$wage, type="l", main = "Line Plot of Wages", xlab = "Index", ylab = "Wage (thousand/month)", col = "purple")
pie(table(data$wage), main = "Pie Chart of Wages", col = rainbow(length(unique(data$wage))))

#Practice 1
    #Add tech category to data
    data$tech <- ifelse(data$spec == "technology",1,0)
  

#Practice 2
    car::linearHypothesis(model, c("ocities = 0", "rural = 0"))
    car::linearHypothesis(model, c("gender = 0", "family = 0"))
    car::linearHypothesis(model, c("gender = 0", "origin = 0"))
    car::linearHypothesis(model, c("age = 0", "tenure = 0"))
    car::linearHypothesis(model, c("age = 0", "log(tenure) = 0"))
    car::linearHypothesis(model, c("log(income) = 0", "log(distance) = 0"))
    car::linearHypothesis(model, c("science = 0", "social = 0"))
    car::linearHypothesis(model, c("income=0", "distance=0"))
    car::linearHypothesis(model_wage, c("science - social = 0"))
    
    #LOG-LOG #LOG-LIN
    #Simple model
    model <- lm(log(wage) ~ age + schooling + tenure + gender + origin + science + social, data = read.csv("https://kinhteluong.online/esdata/wage/wage8.csv")); 
    print(round((1.01^coef(model) - 1) * 100, 3));
    
    summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- c(NA, round((exp(coef(model)[-1]) - 1) * 100, 3)); signif_codes <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); coefficients$`Signif. codes` <- signif_codes; print(coefficients)
     
      model <- lm(log(whours) ~ log(income) + log(wage) + log(distance) + gender + siblings + ocities + rural, data = read.csv("https://kinhteluong.online/esdata/job/job4.csv")); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- c(NA, sapply(names(coef(model))[-1], function(var) ifelse(var %in% c("log(income)", "log(wage)", "log(distance)"), (1.01^coef(model)[var] - 1) * 100, (exp(coef(model)[var]) - 1) * 100))); coefficients <- round(coefficients, 3); coefficients$`Sig`  <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); print(coefficients); cat("Residual standard error:", round(summary_model$sigma, 3), "on", summary_model$df[2], "DF\nR-squared:", round(summary_model$r.squared, 3), " Adjusted R-squared:", round(summary_model$adj.r.squared, 3), "\nF-statistic:", round(summary_model$fstatistic[1], 3), "on", summary_model$fstatistic[2], "and", summary_model$fstatistic[3], "DF, p-value:", format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), digits = 3), "\n")
    
      log_loglin_analysis <- function(data_url, formula, log_vars) { data <- read.csv(data_url); model <- lm(formula, data = data); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- c(NA, sapply(names(coef(model))[-1], function(var) ifelse(var %in% log_vars, (1.01^coef(model)[var] - 1) * 100, (exp(coef(model)[var]) - 1) * 100))); coefficients <- round(coefficients, 3); coefficients$`Sig` <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); print(coefficients); cat("Residual standard error:", round(summary_model$sigma, 3), "on", summary_model$df[2], "DF\nR-squared:", round(summary_model$r.squared, 3), " Adjusted R-squared:", round(summary_model$adj.r.squared, 3), "\nF-statistic:", round(summary_model$fstatistic[1], 3), "on", summary_model$fstatistic[2], "and", summary_model$fstatistic[3], "DF, p-value:", format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), digits = 3), "\n") };log_loglin_analysis(
        data_url = "https://kinhteluong.online/esdata/job/job4.csv",
        formula = log(whours) ~ log(income) + log(wage) + log(distance) + gender + siblings + ocities + rural,
        log_vars = c("log(income)", "log(wage)", "log(distance)")
      )
      
      log_loglin_analysis <- function(data_url, formula) { data <- read.csv(data_url); model <- lm(formula, data = data); summary_model <- summary(model); print(summary_model); cat("\n\tExact Effects Log_LogLin\n"); print(round((1.01^coef(model)[grepl('log', names(coef(model)))] - 1) * 100, 3)); print(round((exp(coef(model)[!grepl('log', names(coef(model))) & names(coef(model)) != '(Intercept)']) - 1) * 100, 3)) };log_loglin_analysis(
        data_url = "https://kinhteluong.online/esdata/job/job4.csv",
        formula = log(whours) ~ log(income) + log(wage) + log(distance) + gender + siblings + ocities + rural
      )
      
  #LIN-LOG
    
    model <- lm(wage ~ log(age) + schooling + log(tenure)*gender + origin + science + social, data = read.csv("https://kinhteluong.online/esdata/wage/wage3.csv")); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- c(NA, sapply(names(coef(model))[-1], function(var) ifelse(var %in% c("log(age)", "log(schooling)", "log(tenure)"), coef(model)[var] * log(1.01), coef(model)[var]))); coefficients <- round(coefficients, 3); coefficients$`Sig`  <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); print(coefficients); fstat <- summary_model$fstatistic; cat("Residual standard error:", round(summary_model$sigma, 3), "on", summary_model$df[2], "DF\nR-squared:", round(summary_model$r.squared, 3), " Adjusted R-squared:", round(summary_model$adj.r.squared, 3), "\nF-statistic:", round(summary_model$fstatistic[1], 3), "on", summary_model$fstatistic[2], "and", summary_model$fstatistic[3], "DF, p-value:", format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), digits = 3), "\n")

    lin_log_analysis <- function(data_url, formula) { data <- read.csv(data_url); model <- lm(formula, data = data); summary_model <- summary(model); print(summary_model); cat("\n\tExact Effects LinLog\n"); print(round(coef(model)[grepl('log', names(coef(model)))] * log(1.01), 3)); print(round(coef(model)[!grepl('log', names(coef(model))) & names(coef(model)) != '(Intercept)'], 3)) };lin_log_analysis(
      data_url = "https://kinhteluong.online/esdata/wage/wage3.csv",
      formula = wage ~ log(age) + schooling + log(tenure)*gender + origin + science + social
    )
    
#QUADRATIC FUNCTIONAL FORM
    analyze_and_plot <- function(data_url, formula, extremum_var) { data <- read.csv(data_url); raw_var <- gsub("log\\((.+)\\)", "\\1", extremum_var); if (grepl("log\\(", extremum_var)) { transformed_var <- paste0("log(data$", raw_var, ")"); if (!extremum_var %in% names(data)) { data[[extremum_var]] <- eval(parse(text = transformed_var), envir = data) } } else { if (!raw_var %in% names(data)) { stop(paste("Error: Variable", raw_var, "not found in dataset.")) } }; if (any(is.na(data[[extremum_var]]))) { data <- na.omit(data); if (nrow(data) == 0) { stop("Error: All rows removed due to missing values in the variable.") } }; model <- lm(formula, data = data); print(summary(model)); extremum_value <- -coef(model)[extremum_var] / (2 * coef(model)[paste0("I(", extremum_var, "^2)")]); extremum_y <- coef(model)["(Intercept)"] + coef(model)[extremum_var] * extremum_value + coef(model)[paste0("I(", extremum_var, "^2)")] * extremum_value^2; for (var in names(data)) { if (var %in% names(coef(model)) && var != extremum_var) { extremum_y <- extremum_y + coef(model)[var] * mean(data[[var]], na.rm = TRUE) } }; plot_data <- data.frame(x_var = seq(min(data[[extremum_var]], na.rm = TRUE), max(data[[extremum_var]], na.rm = TRUE), by = 0.1)); plot_data$y_var <- coef(model)["(Intercept)"] + coef(model)[extremum_var] * plot_data$x_var + coef(model)[paste0("I(", extremum_var, "^2)")] * plot_data$x_var^2; for (var in names(data)) { if (var %in% names(coef(model)) && var != extremum_var) { plot_data$y_var <- plot_data$y_var + coef(model)[var] * mean(data[[var]], na.rm = TRUE) } }; x_label <- gsub("\\.", " ", extremum_var); y_label <- paste("Predicted", all.vars(formula[[2]])); title <- paste(y_label, "vs", x_label); ggplot(plot_data, aes(x = x_var, y = y_var)) + geom_line(color = "blue") + geom_point(aes(x = extremum_value, y = extremum_y), color = "red", size = 3) + geom_text(aes(x = extremum_value, label = paste0("Extremum\n(", round(extremum_value, 4), ", ", round(extremum_y, 4), ")"), y = extremum_y), vjust = 0.5, hjust = 0.5, color = "red") + labs(title = title, x = x_label, y = y_label) + theme_minimal() };analyze_and_plot(
      data_url = "https://kinhteluong.online/esdata/job/job5.csv",
      formula = log(whours) ~ log(income) + I(log(income)^2) + log(wage) + log(distance) + gender + siblings + ocities + rural,
      extremum_var = "log(income)"
    )
    
    cat("Extremum (Vertex) of the parabola:", round(-coef(model)["log(income)"] / (2 * coef(model)["I(log(income)^2)"]),3), "\n");car::linearHypothesis(model, c("log(income) = 0", "I(log(income)^2) = 0"))

    cat("Extremum (Vertex) of the parabola:", round(-coef(model)["income"] / (2 * coef(model)["I(income^2)"]),3), "\n");car::linearHypothesis(model, c("income = 0", "I(income^2) = 0"))

  #Bài tập 3
    model <- lm(log(noodle) ~ income + log(price) + distance + gender + family + ocities + rural, data = read.csv("https://kinhteluong.online/esdata/noodle/noodle41.csv")); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- sapply(names(coef(model)), function(var) { if (var %in% c("income","distance","gender", "family", "ocities", "rural")) { (exp(coef(model)[var]) - 1) * 100 } else { (1.01^coef(model)[var] - 1) * 100 } }); coefficients <- round(coefficients, 3); signif_codes <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); coefficients$`Sig` <- signif_codes; print(coefficients);fstat <- summary_model$fstatistic; cat("\nF-statistic:", round(fstat[1], 3), "on", round(fstat[2], 3), "and", round(fstat[3], 3), "DF, p-value:", format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE), digits = 3))
    
    
  #Bài tập 6
    model <- lm( log(noodle) ~ log(income) + log(price) + distance*gender + family + ocities + rural, data = read.csv("https://kinhteluong.online/esdata/noodle/noodle52.csv")); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- sapply(names(coef(model)), function(var) { if (var %in% c("distance","gender", "family", "ocities", "rural")) { (exp(coef(model)[var]) - 1) * 100 } else { (1.01^coef(model)[var] - 1) * 100 } }); coefficients <- round(coefficients, 3); signif_codes <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); coefficients$`Sig` <- signif_codes; print(coefficients);fstat <- summary_model$fstatistic; cat("\nF-statistic:", round(fstat[1], 3), "on", round(fstat[2], 3), "and", round(fstat[3], 3), "DF, p-value:", format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE), digits = 3))

       

    #CORRELATION MATRIX
    library(dplyr); exclude_vars <- c("income"); print(subset(as.data.frame(as.table(cor(read.csv("https://kinhteluong.online/esdata/noodle/noodle66.csv") %>% mutate(across(all_of(exclude_vars), log, .names = "log_{col}")) %>% select(where(is.numeric), -all_of(exclude_vars)), method = "pearson", use = "complete.obs"))), abs(Freq) >= 0.8 & Var1 != Var2))
    
    print(subset(as.data.frame(as.table(cor(read.csv("https://kinhteluong.online/esdata/noodle/noodle2.csv") %>% select_if(is.numeric), method = "pearson", use = "complete.obs"))), abs(Freq) >= 0.8 & Var1 != Var2))
    
    #AUX
    data <- read.csv("https://kinhteluong.online/esdata/job/job9.csv"); model <- lm(whours ~ log(income) + log(wage) + distance + gender + siblings + ocities + rural, data); aux_model <- lm(distance ~ log(income) + log(wage) + gender + siblings + ocities + rural, data); cat("Main Model - R2:", round(summary(model)$r.squared, 3), "Adjusted R2:", round(summary(model)$adj.r.squared, 3), "\nAux Model - R2:", round(summary(aux_model)$r.squared, 3), "Adjusted R2:", round(summary(aux_model)$adj.r.squared, 3))    
    
      #VIF
    car::vif(lm(whours ~ income + wage + distance + gender + siblings + ocities + rural, data=read.csv("https://kinhteluong.online/esdata/job/job9.csv")))


  #Bai tap 7
    library(dplyr); vars <- c("income"); log_vars <- paste0("log(", vars, ")"); data <- read.csv("https://kinhteluong.online/esdata/noodle/noodle66.csv"); model <- lm(log(noodle) ~ log(income) + price + distance + gender + family + ocities + rural, data); aux_model <- lm(distance ~ log(income) + price + gender + family + ocities + rural, data); summary_model <- summary(model); coefficients <- as.data.frame(summary_model$coefficients); coefficients$Exact_Effect <- c(NA, sapply(names(coef(model))[-1], function(var) ifelse(var %in% log_vars, (1.01^coef(model)[var] - 1) * 100, (exp(coef(model)[var]) - 1) * 100))); coefficients <- round(coefficients, 3); coefficients$`Sig` <- symnum(summary_model$coefficients[, 4], cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")); print(coefficients); fstat <- summary_model$fstatistic; cat("\nResidual standard error:", round(summary_model$sigma, 3), "\nR-squared:", round(summary_model$r.squared, 3), " Adjusted R-squared:", round(summary_model$adj.r.squared, 3), "\nF-statistic:", round(summary_model$fstatistic[1], 3), "on", summary_model$fstatistic[2], "and", summary_model$fstatistic[3], "DF, p-value:", format.pval(pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE), digits = 3), "\n"); cat("\n"); print(subset(as.data.frame(as.table(cor(data %>% mutate(across(all_of(vars), log, .names = "log_{col}")) %>% select(where(is.numeric), -all_of(vars)), method = "pearson", use = "complete.obs"))), abs(Freq) >= 0.8 & Var1 != Var2)); cat("\nMain Model R2:", round(summary_model$r.squared, 3), "Adjusted R2:", round(summary_model$adj.r.squared, 3), "\nAux Model R2:", round(summary(aux_model)$r.squared, 3), "Adjusted R2:", round(summary(aux_model)$adj.r.squared, 3)); cat("\n\n"); print(round(car::vif(model)[car::vif(model) >= 5], 2))


# HETEROSKEDASTICITY   

    data <- read.csv("https://kinhteluong.online/esdata/wage/wage9.csv");formula <- log(wage) ~ log(age) + log(schooling) + tenure + gender + origin + science + social; model <- lm(formula, data = data);specific_tests <- list(c("rural=0", "ocities=0"), c("gender=0", "family=0")); predictor_formula <- as.formula(paste("~", deparse(formula[[3]])));lmtest::bptest(model, predictor_formula, data = data);round(robust <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC0")),3);car::linearHypothesis(model, paste(names(coef(model))[-1], "= 0"), white.adjust = "hc1");for (test_set in specific_tests) {print(car::linearHypothesis(model, test_set, white.adjust = "hc1"))}
      
      cat("\n\tExact Effects Log_LogLin\n");round((1.01^coef(robust)[grepl("log", names(coef(robust)))] - 1) * 100, 3);round((exp(coef(robust)[!grepl("log", names(coef(robust))) & names(coef(robust)) != "(Intercept)"]) - 1) * 100, 3)
    
    cat("\n\tExact Effects LinLog\n"); print(round(coef(model)[grepl('log', names(coef(model)))] * log(1.01), 3)); print(round(coef(model)[!grepl('log', names(coef(model))) & names(coef(model)) != '(Intercept)'], 3))
    
    