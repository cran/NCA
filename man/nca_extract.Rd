\name{nca_extract}
\alias{nca_extract}
\title{get model parameters}
\description{Get the model parameters.}
\usage{nca_extract(model, x=NULL, ceiling=NULL, param='Effect size')}
\arguments{
\item{model}{The model produced by nca_analysis}
\item{x}{Name of the condition, defaults to the first}
\item{ceiling}{Name of the ceiling, defaults to the first}
\item{param}{Name of the parameter, default 'Effect size'}
}
\examples{
data(nca.example)
model <- nca_analysis(nca.example, c('Individualism', 'Risk taking'), 3)

# The names of the parameters can be found in the summaries
nca_output(model, plots=FALSE, summaries=TRUE)

# Get the Ceiling zone
extract <- nca_extract(model, 'Individualism', 'ce_fdh', 'Ceiling zone')
print(extract)

# Get the Scope
extract <- nca_extract(model, 'Individualism', 'ce_fdh', 'Scope')
print(extract)

# For multiple values: vectorize one argument and pin the others
names <- c('Individualism', 'Risk taking')
extracts <- sapply(names, nca_extract,
                   model=model, ceiling='ce_fdh', param='Ceiling zone')
print(extracts)

params <- c('Effect size', 'Ceiling zone')
extracts <- sapply(params, nca_extract,
                   model=model, x='Individualism', ceiling='ce_fdh')
print(extracts)

# It is also possible to get the extract directly from the model
# Get the Ceiling zone
print(model$`Ceiling zone`$Individualism$ce_fdh)

# Get the Scope, global parameters are independent of the ceiling  
print(model$Scope$Individualism)
}
\keyword{functions}
