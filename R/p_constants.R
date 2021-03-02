p_GLOBAL_NAMES  <- c("Number of observations", "Scope",
                     "Xmin", "Xmax", "Ymin", "Ymax")
p_RESULT_NAMES  <- c("Ceiling zone", "Effect size", "# above", "c-accuracy",
                     "Fit", "p-value", "p-accuracy", " ",
                     "Slope", "Intercept", "Abs. ineff.", "Rel. ineff.",
                     "Condition ineff.", "Outcome ineff."  )

p_ceilings_step <- c("ce_vrs", "ce_fdh")
                   # , "ce_lfdh", "ce_fdhi", "ce_cm", "ce_cm_conf")
p_ceilings_line <- c("ols", "cols", "qr", "lh", "sfa",
                     "cr_vrs", "cr_fdh", "c_lp")
                   # , "ct_fdh", "cr_fdhi",
                   #  "cr_cm", "cr_cm_conf", "c_lp")
ceilings        <- c(p_ceilings_step, p_ceilings_line)
p_no_bottleneck <- c("ols")
# p_no_bottleneck <- c("ols", "ce_cm")

# Keep in sync with line.colors.Rd and line.type.Rd
line.colors     <- list(ols="green",        lh="red3",            cols="darkgreen",
                        qr="lightpink",     ce_vrs="orchid4",     cr_vrs="violet",
                        ce_fdh="red",       cr_fdh="orange",      sfa="darkgoldenrod",
                        c_lp="blue")
                        #ce_fdh="red",       ce_lfdh="red2",       ce_fdhi="purple",
                        #ce_cm="darkgreen",  cr_fdh="orange",      ct_fdh="lightgreen",
                        #cr_fdhi="brown",    cr_cm="darkgrey",
                        #ce_cm_conf="black", cr_cm_conf="black",
                        #c_lp="lightpink",   sfa="darkgoldenrod")
line.types      <- list(ols=1,              lh=5,                 cols=3,
                        qr=4,               ce_vrs=5,             cr_vrs=1,
                        ce_fdh=6,           cr_fdh=1,             sfa=7,
                        c_lp=2)
                        #ce_fdh=6,           ce_lfdh=3,            ce_fdhi=7,
                        #ce_cm=5,            cr_fdh=1,             ct_fdh=2,
                        #cr_fdhi=4,          cr_cm=7,
                        #ce_cm_conf=6,       cr_cm_conf=1,
                        #c_lp=5,             sfa=7)

line.width      <- 1.5
point.type      <- 21
point.color     <- "blue"

# Used to compare floats
epsilon         <- 1e-10
