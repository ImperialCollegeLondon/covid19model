# functions to plot using user provided linear predictor

plot_infections_ <- 
  function(object,
           eta,
           groups = NULL,
           dates=NULL, 
           date_breaks="2 weeks", 
           date_format="%Y-%m-%d",
           cumulative=FALSE, 
           by_100k = FALSE,
           levels = c(20, 50, 95), 
           log=FALSE,
           plotly = FALSE, 
           ...) {
    levels <- epidemia:::check_levels(levels)
    
    inf <- posterior_infections_(
      object = object,
      eta = eta,
      ...
    )
    
    # transform data
    inf <- epidemia:::gr_subset(inf, groups)
    
    if (cumulative) {
      inf <- epidemia:::cumul(inf)
    }
    
    pops <- object$pops
    if (by_100k) {
      inf <- epidemia:::norm_obs(pops, inf)
    }
    
    qtl <- epidemia:::get_quantiles(
      inf,
      levels,
      dates,
      date_format
    )
    
    p <- epidemia:::base_plot(qtl, log, date_breaks)
    
    nme <- "Infections"
    if (by_100k) {
      nme <- paste0(nme, " per 100k")
    }
    
    p <- p + ggplot2::scale_fill_manual(
      name = nme,
      values = ggplot2::alpha("deepskyblue4", levels/100)
    )
    
    p <- p + ggplot2::ylab("Infections")
    
    if (plotly) {
      p <- plotly::ggplotly(p)
    }
    return(p)
}






plot_obs_ <-
  function(object,
           type,
           eta,
           posterior_mean = FALSE,
           groups = NULL,
           dates = NULL,
           date_breaks = "2 weeks",
           date_format = "%Y-%m-%d",
           cumulative = FALSE,
           by_100k = FALSE,
           levels = c(30, 60, 90),
           log = FALSE,
           plotly = FALSE,
           ...) {
    
    levels <- epidemia:::check_levels(levels)
    
    if (is.null(type)) {
      stop("must specify an observation type")
    }
    
    alltypes <- sapply(object$obs, function(x) epidemia:::.get_obs(formula(x)))
    w <- which(type == alltypes)
    if (length(w) == 0) {
      stop(paste0("obs does not contain any observations
    for type '", type, "'"), call. = FALSE)
    }
    
    if (is.null(groups)) {
      groups <- object$groups
    }
    
    obs <- posterior_predict_(
      object = object,
      eta=eta,
      types = type,
      posterior_mean = posterior_mean,
      ...
    )
    
    # transform data
    obs <- epidemia:::gr_subset(obs, groups)
    
    data_orig <- object$data
    data_orig <- data_orig[data_orig$group %in% groups, ]
    
    newdata <- list(...)$newdata
    if (is.null(newdata)) {
      data <- data_orig
    } else {
      data <- check_data(
        formula = formula(object$rt),
        data = newdata,
        group_subset = groups
      )
    }
    
    # get observed outcomes
    obj <- epidemia:::epiobs_(object$obs[[w]], data)
    df <- data.frame(
      group = epidemia:::get_gr.epiobs_(obj),
      date = epidemia:::get_time.epiobs_(obj),
      obs = epidemia:::get_obs.epiobs_(obj)
    )
    
    # remove negative values
    df <- df[df$obs >= 0, ]
    
    # classify data as prediction or not a prediction
    data_orig <- data_orig[, c("group", "date", type)]
    df <- dplyr::left_join(df, data_orig, , by = c("group", "date"))
    names(df)[4] <- c("new")
    w <- is.na(df$new)
    
    all_in_sample <- ifelse(any(w), FALSE, TRUE)
    empty <- nrow(df) == 0
    if (!empty) {
      if (all_in_sample){
        df$new <- "Observed"
      } else {
        df$new[w] <- "Out-of-sample"
        df$new[!w] <- "In-sample"
      }
    }
    
    if (cumulative) {
      obs <- epidemia:::cumul(obs)
      df <- df %>%
        dplyr::group_by(.data$group) %>%
        dplyr::mutate(obs = cumsum(obs))
      df <- as.data.frame(df)
    }
    
    pops <- object$pops
    if (by_100k) {
      obs <- epidemia:::norm_obs(pops, obs)
      df <- epidemia:::norm_df(pops, df)
    }
    
    qtl <- epidemia:::get_quantiles(
      obs,
      levels,
      dates,
      date_format
    )
    
    names(df)[3] <- type
    p <- epidemia:::base_plot(qtl, log, date_breaks)
    
    p <- p + ggplot2::geom_bar(
      mapping = ggplot2::aes_string(x = "date", y = type, fill = "new"),
      data = df,
      stat = "identity",
      alpha = 0.7
    )
    
    df1 <- data.frame(
      date = obs$time, 
      median = apply(obs$draws, 2, function(x) quantile(x, 0.5)),
      group = obs$group
    )
    
    p <- p + ggplot2::geom_line(
      mapping = ggplot2::aes(x = date, y = median), 
      data = df1, 
      color = "deepskyblue4"
    )
    
    cols <- c(
      "deepskyblue4",
      ggplot2::alpha("deepskyblue4", rev(levels) * 0.7 / 100),
      "coral4",
      "darkslategray3"
    )
    
    if (all_in_sample) {
      names(cols) <- c("median", paste0(levels, "% CI"), "Observed", "dummy")
    } else {
      names(cols) <- c("median", paste0(levels, "% CI"), "In-sample", "Out-of-sample")
    }
    
    nme <- type
    if (by_100k) {
      nme <- paste0(nme, " per 100k")
    }
    cols <- ggplot2::scale_fill_manual(name = nme, values = cols)
    
    p <- p + cols
    
    if (plotly) {
      p <- plotly::ggplotly(p)
    }
    return(p)
}




plot_rt_ <-
  function(object,
           eta,
           groups = NULL,
           dates = NULL,
           date_breaks = "2 weeks",
           date_format = "%Y-%m-%d",
           levels = c(30, 60, 90),
           log = FALSE,
           smooth = 1,
           plotly = FALSE,
           ...) {
    levels <- epidemia:::check_levels(levels)
    
    rt <- posterior_rt_(
      object = object,
      eta = eta,
      ...
    )
    
    # transform data
    rt <- epidemia:::gr_subset(rt, groups)
    rt <- epidemia:::smooth_obs(rt, smooth)
    
    qtl <- epidemia:::get_quantiles(
      rt,
      levels,
      dates,
      date_format
    )
    p <- epidemia:::base_plot(qtl, log, date_breaks, TRUE)
    
    df <- data.frame(
      date = rt$time, 
      median = apply(rt$draws, 2, function(x) quantile(x, 0.5)),
      group = rt$group
    )
    
    p <- p + ggplot2::geom_line(
      mapping = ggplot2::aes(x = date, y = median), 
      data = df, 
      color = "seagreen"
    )
    
    p <- p + ggplot2::scale_fill_manual(
      name = "R_t", 
      values = ggplot2::alpha("seagreen", levels * 0.7/100)
    )
    
    p <- p + ggplot2::geom_hline(
      yintercept = 1,
      color = "black",
      size = 0.7
    )
    
    if (plotly) {
      p <- p + ggplot2::ylab(plotly::TeX("$R_t$"))
      p <- plotly::ggplotly(p) %>% plotly::config(mathjax = "cdn")
    } else {
      p <- p + ggplot2::ylab(expression(R[t]))
    }
    return(p)
  }


posterior_rt_ <-
  function(object,
           eta,
           draws = NULL,
           seed = NULL,
           adjusted = TRUE,
           ...) {
    return(posterior_latent_(
      object = object,
      eta = eta,
      draws = draws,
      seed = seed,
      series = if (adjusted) "Rt" else "Rt_unadj",
      ...
    ))
  }

posterior_infections_ <-
  function(object,
           eta,
           newdata = NULL,
           draws = NULL,
           seed = NULL,
           ...) {
    return(posterior_latent_(
      object = object,
      eta = eta,
      newdata = newdata,
      draws = draws,
      seed = seed,
      series = "infections",
      ...
    )
    )
  }

posterior_latent_ <-
  function(object,
           eta,
           series = c("Rt", "Rt_unadj", "infections"),
           draws = NULL,
           seed = NULL, ...) {
    out <- posterior_sims_(
      object = object,
      eta = eta,
      series = series,
      draws = draws,
      seed = seed,
      ...
    )
    return(out[[series]])
  }


posterior_predict_ <-
  function(object,
           eta,
           draws = NULL,
           types = NULL,
           seed = NULL,
           posterior_mean = FALSE, ...) {
    alltypes <- sapply(
      object$obs,
      function(x) epidemia:::.get_obs(formula(x))
    )
    if (is.null(types)) {
      types <- alltypes
    } else {
      w <- !(types %in% alltypes)
      if (any(w)) {
        stop(paste0(types[w], " not a modeled type of observation.",
                    call. = FALSE
        ))
      }
    }
    out <- posterior_sims_(
      object = object,
      eta = eta,
      draws = draws,
      seed = seed,
      ...
    )
    out <- if (posterior_mean) out$E_obs else out$obs
    return(out[[types]])
  }





posterior_sims_ <- function(object,
                           eta,
                           draws = NULL,
                           seed = NULL,
                           ...) {

  all <- c(list(R = object$rt), object$obs)
  
  data <- object$data
  rt <- epidemia:::epirt_(all$R, data)
  
  obs <- lapply(all[-1], epidemia:::epiobs_, data)
  
  stanmat <- epidemia:::subsamp(
    object,
    as.matrix(object$stanfit),
    NULL
  )
  
  standata <- epidemia:::pp_standata(
    object = object,
    rt = rt,
    obs = obs,
    data = data
  )

  if (length(eta) > 0) {
    colnames(eta) <- paste0("eta[", seq_len(ncol(eta)), "]")
    stanmat <- cbind(stanmat, as.matrix(eta))
  }
  
  oeta <- do.call(cbind, lapply(obs, epidemia:::pp_eta, stanmat))
  if (length(oeta) > 0) {
    oeta <- sweep(oeta, 2, standata$offset, "+")
    colnames(oeta) <- paste0("oeta[", seq_len(ncol(oeta)), "]")
    stanmat <- cbind(stanmat, as.matrix(oeta))
  }
  
  # stanmatrix may require relabeling
  stanmat <- epidemia:::pp_stanmat(
    stanmat = stanmat,
    orig_nms = object$orig_names,
    groups = levels(data$group)
  )
  
  sims <- rstan::gqs(epidemia:::stanmodels$epidemia_pp_base,
                     data = standata,
                     draws = stanmat
  )
  
  # get list of indices for slicing result of gqs
  ind <- Map(
    function(x, y) x:y,
    standata$starts,
    standata$starts + standata$NC - 1
  )
  
  # get latent series
  nms <- c("Rt_unadj", "Rt", "infections", "infectiousness")
  out <- lapply(
    nms,
    function(x) epidemia:::parse_latent(sims, x, ind, rt)
  )
  names(out) <- nms
  
  # add posterior predictive
  n <- standata$oN[seq_len(standata$R)]
  
  out <- c(out, list(
    obs = epidemia:::parse_obs(sims, "obs", n, obs),
    E_obs = epidemia:::parse_obs(sims, "E_obs", n, obs)
  ))
  return(out)
}






