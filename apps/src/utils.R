get_sample <- function(auc, n_samples, prevalence, scale_to_d=F){
  # https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc
  # http://dx.doi.org/10.5093/ejpalc2018a5
  t <- sqrt(log(1/(1-auc)**2))
  z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) /
            (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))
  d <- z*sqrt(2)
  
  n_pos <- sum(sample(c(0,1), n_samples, replace=TRUE, prob=c(1-prevalence, prevalence)))
  n_neg <- n_samples - n_pos
  
  x <- c(rnorm(n_neg, mean=0), rnorm(n_pos, mean=d))
  y <- c(rep(0, n_neg), rep(1, n_pos))
  
  if(scale_to_d){
    x <- x/d
  }
  return(data.frame(predicted=x, actual=y))
}


plot_binned_ridges <- function(data, ci=0.95, hdi=T, limit_y=FALSE, subtitle="",
                               factor_levels=NULL) {
  
  p_data <-
    data %>%
    pivot_longer(!n_sim)
  
  if(hdi) {
    cred.int <- data %>%
      pivot_longer(!n_sim) %>%
      group_by(name) %>%
      summarise(CI=list(hdi(value, ci=ci)),
                m=median(value, na.rm=TRUE)) %>%
      unnest_wider(CI)
    
    p_data <-
      left_join(p_data, cred.int, by="name") %>%
      mutate(in_interval=value > CI_low & value < CI_high)
  } else {
    probs <- c((1 - ci)/2, 1 - (1 - ci)/2)
    
    p_data <-
      p_data %>%
      group_by(name) %>%
      arrange(value) %>%
      mutate(percentile=row_number()/n()) %>%
      ungroup() %>%
      mutate(in_interval = percentile > probs[1] & percentile < probs[2])
  }
  
  
  if(is.null(factor_levels)){
    factor_levels <- data %>% select(-n_sim) %>% names()
  }
  
  p_data$name <- factor(p_data$name, levels=factor_levels)
  
  p <-
    p_data %>%
    ggplot(aes(x = value, y = name, height = stat(count), fill=in_interval)) +
    geom_density_ridges(stat = "binline", scale = 0.95, draw_baseline=FALSE) +
    coord_flip() +
    theme_bw() +
    labs(
      y="", x="",
      subtitle=subtitle
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "position.none") +
    scale_fill_manual(values=c("#ADD8E6","grey50"), breaks=c(TRUE, FALSE))
  
  if(!limit_y){
    p <- p + scale_x_continuous(labels=scales::dollar_format())
  }
  p
}

