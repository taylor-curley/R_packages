roc_selection <- function(data, varname, outcomevar, low_bin, high_bin, step_bin){

  bin_range <- seq(low_bin,high_bin,step_bin)

  ag_all <- c()
  az_all <- c()
  sd_HR <- c()
  sd_FAR <- c()
  sd_criterion <- c()

  for (x in bin_range){

    binned_data <-bin_calculations(data, varname, outcomevar,x)
    sd_HR <- c(sd_HR, sd(binned_data$HR, na.rm = TRUE))
    sd_FAR <- c(sd_FAR, sd(binned_data$FAR, na.rm = TRUE))
    az_all <- c(az_all, az_calc(data, varname, outcomevar,x))
    ag_all <- c(ag_all, ag_calc(data, varname, outcomevar,x))
    sd_criterion <- c(sd_criterion, var(as.vector(criterion(data, varname, outcomevar,x))))

  }

  all_means <- data.frame(Bin = bin_range, ag_all, az_all, sd_HR, sd_FAR, sd_criterion)


  plot(all_means$Bin,all_means$ag_all, main = "Bin # by A_g Statistic", xlab = "Bin #", ylab = "A_g Statistic")
  return(all_means)
}
