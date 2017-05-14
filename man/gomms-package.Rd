\name{gomms-package}
\alias{gomms-package}
\docType{package}
\title{
GLM-Based Ordination Method
}
\description{
preliminary analysis of similarity between samples in a low (2 or 3) dimensional display.
}
\author{
Michael B. Sohn

Maintainer: Michael B. Sohn <msohn@mail.med.upenn.edu>
}
\references{
Sohn, M.B. and Li, H. (2017). A GLM-Based Latent Variable Ordination Method for Microbiome Samples (Submitted).
}
\examples{
\dontrun{
# load test data

data(gomms_test_data);

# estimate factor scores
cdat <- as.matrix(gomms_test_data[,-ncol(gomms_test_data)]);
rslt <- gomms(cdat);

# plot estimated factor scores
y <- as.matrix(gomms_test_data$group);
gomms.plot(rslt, y);
}
}
