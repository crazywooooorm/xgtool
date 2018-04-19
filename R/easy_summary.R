#-- target is numerical, can it be categorical?
easy_summary <- function(data, target, wt, ...) {
  if(missing(wt)) {
    data <- data %>%
      mutate(.wt = 1)

    target <- enquo(target)

    data %>%
      summarise(mean = weighted.mean(!!target, w = .wt, na.rm = T),
                n = sum(!is.na(!!target)),
                sum_wts = sum(.wt, na.rm = T),
                sd = sqrt(sum(.wt * (!!target - mean)^2, na.rm = T) / (n - 1)),
                se = sd/sqrt(n))
  } else {
    target <- enquo(target)
    wt <- enquo(wt)

    data %>%
      summarise(mean = weighted.mean(!!target, w = !!wt, na.rm = T),
                n = sum(!is.na(!!target)),
                sum_wts = sum(!!wt, na.rm = T),
                sd = sqrt(sum(wt * (!!target - mean)^2, na.rm = T) / (n - 1)),
                se = sd/sqrt(n))
  }
}

