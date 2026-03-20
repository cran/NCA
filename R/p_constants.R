p_GLOBAL_NAMES  <- c("Number of observations", "Scope",
                     "Xmin", "Xmax", "Ymin", "Ymax")
p_PARAM_NAMES  <- c(
  "Ceiling zone", "Effect size", "# above",
  "Slope", "Intercept",
  "p-value", "p-accuracy",
  " ",
  "Complexity", "Fit", "Ceiling accuracy", "Accuracy nof", "Noise", "Noise nof",
  "Exceptions", "Exceptions nof", "Support", "Support nof", "Spread", "Sharpness",
  "  ",
  "Abs. ineff.", "Rel. ineff.",
  "Condition ineff.", "Outcome ineff.")

p_ceilings_step  <- c("ce_vrs", "ce_fdh")
p_ceilings_line  <- c("ols", "cols", "qr",
                      "cr_vrs", "cr_fdh", "c_lp")
ceilings         <- c(p_ceilings_step, p_ceilings_line)
p_ceiling_custom <- "custom"
p_no_bottleneck  <- c("ols")
p_no_peer_line   <- c("cols", "qr", "c_lp", "custom")

# Keep in sync with line.colors.Rd and line.type.Rd
line.colors     <- list(ols="green",        c_lp="blue",          cols="darkgreen",
                        qr="lightpink",     ce_vrs="orchid4",     cr_vrs="violet",
                        ce_fdh="red",       cr_fdh="orange",      custom="purple")
line.types      <- list(ols=1,              c_lp=2,               cols=3,
                        qr=4,               ce_vrs=5,             cr_vrs=1,
                        ce_fdh=6,           cr_fdh=1,             custom=7)

line.width      <- .75
point.type      <- 21
point.color     <- "blue"
point.size      <- 2.0

dash.count      <- 75

# Used to compare floats
epsilon         <- 1e-10
delta           <- 1E6

p_parallel_force <- "NCA_PARALLEL_FORCE"
p_parallel_start <- "NCA_PARALLEL_START"
p_skip_purity    <- "NCA_SKIP_PURITY"

p_ENLARGEMENT_CONSTANT <- 1e-10
