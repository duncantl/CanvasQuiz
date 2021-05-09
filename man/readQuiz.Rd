\name{readQuiz}
\alias{readQuiz}
\title{Read a Canvas Quiz from XML}
\description{
 This extracts the lists of questions from the XML document representing
 the quiz. Each question
 This is just a prototype to illustrate an approach.
 I haven't given the representation of the questions much thought
 or dealt with richer types of answers.
}
\usage{
readQuiz(file, doc = htmlParse(file))
}
\arguments{
  \item{file}{the name of the file or the XML content itsel}
  \item{doc}{the parsed XML document. We use
    \code{\link[XML]{htmlParse}}
    rather than     \code{\link[XML]{xmlParse}}
    as we encountered one sample document that had embedded HTML
    content that was not well-formed.
    Also, this removes the default namespaces and makes the XPath
    queries simpler as we don't have to specify a namespace.}
}
\value{
A list with an element for each question for which an answer is
expected, i.e., we omit text-only questions.
}
\references{
The schema for the XML documents is at \url{http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd}
}
\author{
Duncan Temple Lang
}
\examples{
f = system.file("exampleDocs", "quiz3.xml")
qz3 = readQuiz(f)
names(qz3)

sapply(qz3, names)

sapply(qz3, function(x) x$meta['question_type'])

lapply(qz3, `[[`, "answer")
}

\keyword{IO}
