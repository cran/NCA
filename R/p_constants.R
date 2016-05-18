p_GLOBAL_NAMES  <- c("Number of observations", "Scope",
                     "Xmin", "Xmax", "Ymin", "Ymax")
p_RESULT_NAMES  <- c("Ceiling zone","Effect size", "# above", "Accuracy", " ",
                     "Slope", "Intercept", "Abs. ineff.",
                     "Rel. ineff.", "Condition ineff.", "Outcome ineff."  )

ceilings        <- c("ols", "lh", "cols", "qr", "ce_vrs",
                     "cr_vrs", "ce_fdh", "cr_fdh", "sfa")

line.colors     <- list(ols="green",        lh="red3",            cols="darkgreen",
                        qr="blue",          ce_vrs="orchid4",     cr_vrs="violet",
                        ce_fdh="red",       cr_fdh="orange",
                        sfa="darkgoldenrod")
line.types      <- list(ols=1,              lh=2,                 cols=3,
                        qr=4,               ce_vrs=5,             cr_vrs=1,
                        ce_fdh=6,           cr_fdh=1,             sfa=7)

line.width      <- 1.5
point.type      <- 21
