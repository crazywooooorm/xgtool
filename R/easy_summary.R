#-- target is numerical, can it be categorical?
easy_summary <- function(data, target, wt, ...) {
  if(missing(wt)) {
    target <- dplyr::enquo(target)

    summary_table <- data %>%
      dplyr::summarise(mean = mean(!!target, na.rm = T),
                       n = sum(!is.na(!!target)),
                       sum_wts = n,
                       sd = sqrt(sum((!!target - mean)^2, na.rm = T)/(n - 1)),
                       se = sd/sqrt(n))
  } else {
    target <- dplyr::enquo(target)
    wt <- dplyr::enquo(wt)

    summary_table <- data %>%
      dplyr::summarise(mean = weighted.mean(!!target, w = !!wt, na.rm = T),
                       n = sum(!is.na(!!target)),
                       sum_wts = sum(!!wt, na.rm = T),
                       sd = sqrt(sum(wt * (!!target - mean)^2, na.rm = T)/(n - 1)),
                       se = sd/sqrt(n))
  }

  summary_table %>%
    dplyr::mutate(ci_l = mean - stats::qnorm(0.975) * se,
                  ci_u = mean + stats::qnorm(0.975) * se) %>%
    dplyr::select(mean, se, n, sum_wts, ci_l, ci_u)
}

