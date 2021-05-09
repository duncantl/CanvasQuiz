This is a quick exploration of reading the XML that Canvas generates when exporting a quiz.
I am not very familiar with Canvas quizzes and the range of possible question types and content. 
So this is based on looking at 3 sample quizzes and currently limited and the schema.
This is hopefully useful as a starting point for others to generalize.

The exams package might have some data structures to represent questions (or it may just translate
directly.)  If not, and I were doing this more comprehensively, I would probably consider defining S4 classes to
represent the different types of questions. The XMLSchema package might help with this.

The schema is at [http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd](http://www.imsglobal.org/xsd/ims_qtiasiv1p2p1.xsd)
The R object representing the types and elements in the schema is availabe in
inst/schema/QTISchema.rds. It is useful to see what child nodes and attributes can be in each XML element.

The format may be documented [here](http://www.imsglobal.org/question/index.html) but this seems
slightly different.



# Example Usage

```r
f = system.file("exampleDocs", "quiz3.xml", package = "QTIQuiz")
qz3 = readQuiz(f)
names(qz3)
```
