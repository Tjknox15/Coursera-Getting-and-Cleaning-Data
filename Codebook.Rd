\name{final_data}
\alias{final_data}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("final_data")}
\format{
  A data frame with 10299 observations on the following 82 variables.
  \describe{
    \item{\code{activityID}}{a numeric vector}
    \item{\code{activityType}}{a factor with levels \code{LAYING} \code{SITTING} \code{STANDING} \code{WALKING} \code{WALKING_DOWNSTAIRS} \code{WALKING_UPSTAIRS}}
    \item{\code{subjectID}}{a numeric vector}
    \item{\samp{timeBodyAccMean-X}}{a numeric vector}
    \item{\samp{timeBodyAccMean-Y}}{a numeric vector}
    \item{\samp{timeBodyAccMean-Z}}{a numeric vector}
    \item{\samp{timeBodyAcc-std-X}}{a numeric vector}
    \item{\samp{timeBodyAcc-std-Y}}{a numeric vector}
    \item{\samp{timeBodyAcc-std-Z}}{a numeric vector}
    \item{\samp{timeGravityAccMean-X}}{a numeric vector}
    \item{\samp{timeGravityAccMean-Y}}{a numeric vector}
    \item{\samp{timeGravityAccMean-Z}}{a numeric vector}
    \item{\samp{timeGravityAcc-std-X}}{a numeric vector}
    \item{\samp{timeGravityAcc-std-Y}}{a numeric vector}
    \item{\samp{timeGravityAcc-std-Z}}{a numeric vector}
    \item{\samp{timeBodyAccJerkMean-X}}{a numeric vector}
    \item{\samp{timeBodyAccJerkMean-Y}}{a numeric vector}
    \item{\samp{timeBodyAccJerkMean-Z}}{a numeric vector}
    \item{\samp{timeBodyAccJerk-std-X}}{a numeric vector}
    \item{\samp{timeBodyAccJerk-std-Y}}{a numeric vector}
    \item{\samp{timeBodyAccJerk-std-Z}}{a numeric vector}
    \item{\samp{timeBodyGyroMean-X}}{a numeric vector}
    \item{\samp{timeBodyGyroMean-Y}}{a numeric vector}
    \item{\samp{timeBodyGyroMean-Z}}{a numeric vector}
    \item{\samp{timeBodyGyro-std-X}}{a numeric vector}
    \item{\samp{timeBodyGyro-std-Y}}{a numeric vector}
    \item{\samp{timeBodyGyro-std-Z}}{a numeric vector}
    \item{\samp{timeBodyGyroJerkMean-X}}{a numeric vector}
    \item{\samp{timeBodyGyroJerkMean-Y}}{a numeric vector}
    \item{\samp{timeBodyGyroJerkMean-Z}}{a numeric vector}
    \item{\samp{timeBodyGyroJerk-std-X}}{a numeric vector}
    \item{\samp{timeBodyGyroJerk-std-Y}}{a numeric vector}
    \item{\samp{timeBodyGyroJerk-std-Z}}{a numeric vector}
    \item{\code{timeBodyAccMagnitudeMean}}{a numeric vector}
    \item{\code{timeBodyAccMagnitudeStdDev}}{a numeric vector}
    \item{\code{timeGravityAccMagnitudeMean}}{a numeric vector}
    \item{\code{timeGravityAccMagnitudeStdDev}}{a numeric vector}
    \item{\code{timeBodyAccJerkMagnitudeMean}}{a numeric vector}
    \item{\code{timeBodyAccJerkMagnitudeStdDev}}{a numeric vector}
    \item{\code{timeBodyGyroMagnitudeMean}}{a numeric vector}
    \item{\code{timeBodyGyroMagnitudeStdDev}}{a numeric vector}
    \item{\code{timeBodyGyroJerkMagnitudeMean}}{a numeric vector}
    \item{\code{timeBodyGyroJerkMagnitudeStdDev}}{a numeric vector}
    \item{\samp{freqBodyAccMean-X}}{a numeric vector}
    \item{\samp{freqBodyAccMean-Y}}{a numeric vector}
    \item{\samp{freqBodyAccMean-Z}}{a numeric vector}
    \item{\samp{freqBodyAcc-std-X}}{a numeric vector}
    \item{\samp{freqBodyAcc-std-Y}}{a numeric vector}
    \item{\samp{freqBodyAcc-std-Z}}{a numeric vector}
    \item{\samp{freqBodyAccMeanFreq-X}}{a numeric vector}
    \item{\samp{freqBodyAccMeanFreq-Y}}{a numeric vector}
    \item{\samp{freqBodyAccMeanFreq-Z}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMean-X}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMean-Y}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMean-Z}}{a numeric vector}
    \item{\samp{freqBodyAccJerk-std-X}}{a numeric vector}
    \item{\samp{freqBodyAccJerk-std-Y}}{a numeric vector}
    \item{\samp{freqBodyAccJerk-std-Z}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMeanFreq-X}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMeanFreq-Y}}{a numeric vector}
    \item{\samp{freqBodyAccJerkMeanFreq-Z}}{a numeric vector}
    \item{\samp{freqBodyGyroMean-X}}{a numeric vector}
    \item{\samp{freqBodyGyroMean-Y}}{a numeric vector}
    \item{\samp{freqBodyGyroMean-Z}}{a numeric vector}
    \item{\samp{freqBodyGyro-std-X}}{a numeric vector}
    \item{\samp{freqBodyGyro-std-Y}}{a numeric vector}
    \item{\samp{freqBodyGyro-std-Z}}{a numeric vector}
    \item{\samp{freqBodyGyroMeanFreq-X}}{a numeric vector}
    \item{\samp{freqBodyGyroMeanFreq-Y}}{a numeric vector}
    \item{\samp{freqBodyGyroMeanFreq-Z}}{a numeric vector}
    \item{\code{freqBodyAccMagnitudeMean}}{a numeric vector}
    \item{\code{freqBodyAccMagnitudeStdDev}}{a numeric vector}
    \item{\code{freqBodyAccMagnitudeMeanFreq}}{a numeric vector}
    \item{\code{freqBodyAccJerkMagnitudeMean}}{a numeric vector}
    \item{\code{freqBodyAccJerkMagnitudeStdDev}}{a numeric vector}
    \item{\code{freqBodyAccJerkMagnitudeMeanFreq}}{a numeric vector}
    \item{\code{freqBodyGyroMagnitudeMean}}{a numeric vector}
    \item{\code{freqBodyGyroMagnitudeStdDev}}{a numeric vector}
    \item{\code{freqBodyGyroMagnitudeMeanFreq}}{a numeric vector}
    \item{\code{freqBodyGyroJerkMagnitudeMean}}{a numeric vector}
    \item{\code{freqBodyGyroJerkMagnitudeStdDev}}{a numeric vector}
    \item{\code{freqBodyGyroJerkMagnitudeMeanFreq}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(final_data)
## maybe str(final_data) ; plot(final_data) ...
}
\keyword{datasets}
