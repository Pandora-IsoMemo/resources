plotTargets <- function(fruitsObj, modelResults, individual, estType = "Source contributions",
                        groupType = "Parameter", filterType = "",
                        groupVars = "", ylabel = "", xlabel = "", headerLabel = "",
                        xTextSize = 24, yTextSize = 24,
                        xAxisSize = 18, yAxisSize = 18,
                        plotType = "BoxPlot", returnType = "plot",
                        showLegend = FALSE, colorPalette = "default",
                        Teaser = TRUE, contributionLimit = "None",
                        pointDat = data.frame(), histBins = 50,
                        binSize = NULL,
                        fontFamily = NULL, whiskerMultiplier = 0.95, boxQuantile = 0.68) {
  if (length(groupVars) == 0) {
    return(NULL)
  }
  modelResults <- modelResults[modelResults[, 1] == estType, ]
  filterRows <- which(individual == modelResults[, filterType])
  if (filterType != "all" & length(filterRows) > 0 & filterType != groupType) {
    modelResults <- modelResults[filterRows, ]
  }
  modelResults <- modelResults[modelResults[, groupType] %in% groupVars, ]
  modelResults$group <- factor(modelResults[, groupType], levels = unique(modelResults[, groupType]))
  modelResults <- modelResults[, c("estimate", "group")]
  if (contributionLimit == "0-100%") {
    modelResults <- modelResults %>% mutate_(estimate = ~ estimate * 100)
    if (nrow(pointDat) > 0) {
      pointDat <- pointDat %>% mutate_(y = ~ y * 100)
    }
  }
  if (Teaser == TRUE & filterType != "Parameter" & estType == "Source contributions" & headerLabel == "") {
    headerLabel <- getTeaser(fruitsObj, individual, filterType)
  }

  if (returnType == "data") {
    if (!is.null(binSize)) {
      sequence <- seq(floor(100 * min(modelResults$estimate)) / 100,
        floor(100 * (max(modelResults$estimate) + binSize)) / 100,
        by = binSize
      )

      modelResults$bin <- cut(modelResults$estimate,
        breaks = sequence, include.lowest = TRUE
      )

      modelResults <- modelResults %>%
        group_by_(~group, ~bin) %>%
        summarise(nSamples = n()) %>%
        ungroup()
    }
    return(modelResults)
  }

  if (plotType == "BoxPlot") {
    # fix R CMD check warnings
    lower <- upper <- middle <- q1 <- q3 <- group <- estimate <- iqr <- minim <- maxim <- meanEst <- q32 <- q68 <- q95 <- q05 <- NULL

    # if (xlabel == ""){
    #   xlabel <- groupType
    # }

    if (ylabel == "") {
      if (contributionLimit == "0-100%") {
        ylabel <- "contribution (%)"
      } else {
        ylabel <- "contribution"
      }
    }
    dataSummary <- modelResults %>%
      group_by(group) %>%
      summarise(
        # sd = sd(estimate),
        median = median(estimate),
        meanEst = mean(estimate),
        q68 = quantile(estimate, boxQuantile),
        q95 = quantile(estimate, 1 - ((1 - whiskerMultiplier) / 2)),
        q32 = quantile(estimate, 1 - boxQuantile),
        q05 = quantile(estimate, (1 - whiskerMultiplier) / 2),
      ) %>%
      ungroup()
    if (colorPalette == "white") {
      p <- ggplot(dataSummary, aes_(x = ~group)) +
        ylab(ylabel) +
        xlab(xlabel)
    } else {
      p <- ggplot(dataSummary, aes_(x = ~group, fill = ~group)) +
        ylab(ylabel) +
        xlab(xlabel)
    }


    p <- p + geom_boxplot(
      mapping = aes(
        lower = q32,
        upper = q68,
        middle = median,
        ymin = q05,
        ymax = q95
      ),
      stat = "identity"
    ) + geom_errorbar(aes(ymin = meanEst, ymax = meanEst), linetype = "dashed", data = dataSummary)
    if (contributionLimit == "0-100%") {
      p <- p + ylim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + ylim(c(0, 1))
    }

    if (nrow(pointDat) > 0) {
      if (contributionLimit == "0-100%") {
        pointDat$y <- pointDat$y / 100
      }
      p <- p + geom_point(
        data = pointDat, mapping = aes_(x = ~group, y = ~y),
        color = pointDat$pointColor,
        size = pointDat$pointSize, alpha = pointDat$pointAlpha,
        show.legend = FALSE
      )
    }
    if (!is.null(fontFamily)) {
      p <- p + theme(text = element_text(family = fontFamily))
    }
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPaletteRamp <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPaletteRamp(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "KernelDensity") {
    if (xlabel == "") {
      if (contributionLimit == "0-100%") {
        xlabel <- "contribution (%)"
      } else {
        xlabel <- "contribution"
      }
    }

    if (ylabel == "") {
      ylabel <- "density"
    }

    p <- ggplot(modelResults, aes_(x = ~estimate, fill = ~group)) +
      geom_density(alpha = 0.3) +
      ylab(ylabel) +
      xlab(xlabel)
    if (contributionLimit == "0-100%") {
      p <- p + xlim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + xlim(c(0, 1))
    }
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "Histogram") {
    if (xlabel == "") {
      if (contributionLimit == "0-100%") {
        xlabel <- "contribution (%)"
      } else {
        xlabel <- "contribution"
      }
    }

    if (ylabel == "") {
      ylabel <- "number of observations"
    }

    p <- ggplot(modelResults, aes_(x = ~estimate, fill = ~group)) +
      geom_histogram(alpha = 0.5, binwidth = NULL, bins = histBins, position = "identity") +
      ylab(ylabel) +
      xlab(xlabel)
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_fill_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "Trace") {
    if (xlabel == "") {
      xlabel <- ""
    }

    if (ylabel == "") {
      ylabel <- "value"
    }

    modelResults$X <- rep(
      1:(nrow(modelResults) / length(unique(modelResults$group))),
      length(unique(modelResults$group))
    )
    p <- ggplot(modelResults, aes_(x = ~X, y = ~estimate, colour = ~group)) +
      geom_line(alpha = 0.65) +
      ylab(ylabel) +
      xlab(xlabel)

    if (contributionLimit == "0-100%") {
      p <- p + ylim(c(0, 100))
    }
    if (contributionLimit == "0-1") {
      p <- p + ylim(c(0, 1))
    }

    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_color_manual(values = colorPalette(p$data$group %>% unique() %>% length()))
    }
  }
  if (plotType == "AutoCor") {
    if (xlabel == "") {
      xlabel <- "lag"
    }

    if (ylabel == "") {
      ylabel <- "autocorrelation"
    }
    acfData <- split(modelResults, modelResults$group)
    acfDataNew <- gather(bind_rows(lapply(acfData, function(x) {
      acf(x$estimate, lag.max = 50, plot = FALSE)$acf
    })), key = "estimate", value = "Value")
    acfDataNew$X <- rep(
      1:(nrow(acfDataNew) / length(acfData)),
      length(acfData)
    )

    p <- ggplot(acfDataNew, aes_(x = ~X, y = ~Value, colour = ~estimate)) +
      geom_line(alpha = 0.65) +
      ylab(ylabel) +
      xlab(xlabel)
    if (colorPalette != "default") {
      colorPalette <- brewer.pal(n = 9, name = colorPalette)
      colorPalette <- colorRampPalette(colorPalette)
      p <- p + scale_color_manual(values = colorPalette(p$data$estimate %>% unique() %>% length()))
    }
  }
  p <- p + theme(
    axis.title.x = element_text(size = xTextSize),
    axis.title.y = element_text(size = yTextSize),
    axis.text.x = element_text(size = xAxisSize),
    axis.text.y = element_text(size = yAxisSize)
  )
  if (headerLabel != "") {
    p <- p + labs(title = headerLabel) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  if (!showLegend) {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

getTeaser <- function(data, individual, filterType) {
  if (filterType == "Target") {
    if (individual != "all") {
      means <- data$obsvn[rownames(data$obsvn) == individual, ]
      sds <- data$obsvnError[rownames(data$obsvn) == individual, ]
    } else {
      means <- colMeans(data$obsvn)
      sds <- signif(sqrt(colMeans(data$obsvnError^2)) +
        sqrt(colMeans(scale(data$obsvn, center = TRUE, scale = FALSE)^2)), 3)
    }
  } else {
    if (individual != "all") {
      individuals <- which(data$covariates[, filterType] == individual)
    } else {
      individuals <- 1:NROW(data$covariates)
    }
    means <- colMeans(data$obsvn[individuals, , drop = FALSE])
    sds <- signif(sqrt(colMeans(data$obsvnError[individuals, , drop = FALSE]^2)) +
      sqrt(colMeans(scale(data$obsvn[individuals, , drop = FALSE], center = TRUE, scale = FALSE)^2)), 3)
  }
  teaserText <- paste0(individual, ": ")
  for (i in 1:ncol(data$obsvn)) {
    teaserText <- paste0(teaserText, colnames(data$obsvn)[i], " = ", means[i], " (", sds[i], ") ; ")
  }
  teaserText
}
