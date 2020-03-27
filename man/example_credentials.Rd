\name{example_credentials}
\alias{example_credentials}
\docType{data}
\title{
A brief example of the form of a credentials file.
}
\description{
An example of the format of a credential data frame.
}
\usage{data("example_credentials")}
\format{
  A data frame with 3 observations on the following 2 variables.
  \describe{
    \item{\code{id}}{the user ID}
    \item{\code{password}}{the password in plain text}
  }
}
\details{
For most purposes, you will want to either create a CSV file with the credentials information or create a Google sheet (with "sharing" set to "anyone can read".) `example_credentials` simply shows the format of the data to be stored in that local file or Google Sheet.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(example_credentials.R)
}
\keyword{datasets}
