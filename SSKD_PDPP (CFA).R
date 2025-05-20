model <- '
D1 =~ composite_D1.1 + composite_D1.2 + composite_D1.3
D2 =~ composite_D2.1 + composite_D2.2 + composite_D2.3 + composite_D2.4
D3 =~ composite_D3.1 + composite_D3.2 + composite_D3.3
D4 =~ composite_D4.1 + composite_D4.2 + composite_D4.3 + composite_D4.4
'
fit <- cfa(model, data = cleaned_data, estimator = "WLSMV")  # WLSMV for ordinal data

summary(fit, fit.measures = TRUE, standardized = TRUE)
semPaths(fit, "std", whatLabels = "std", edge.label.cex = 0.8, layout = "tree")
modindices(fit, sort. = TRUE, minimum.value = 10)
