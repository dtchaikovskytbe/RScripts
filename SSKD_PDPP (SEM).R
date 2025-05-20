model <- '
  # Latent variables
D1 =~ composite_D1.1 + composite_D1.2 + composite_D1.3
D2 =~ composite_D2.1 + composite_D2.2 + composite_D2.3 + composite_D2.4
D3 =~ composite_D3.1 + composite_D3.2 + composite_D3.3
D4 =~ composite_D4.1 + composite_D4.2 + composite_D4.3 + composite_D4.4

  # Regression paths from demographic variables
D1 ~ Kumpulan + Jantina
D2 ~ Kumpulan + Jantina
D3 ~ Kumpulan + Jantina
D4 ~ Kumpulan + Jantina

'
fit <- sem(model, data = cleaned_data)
summary(fit)
