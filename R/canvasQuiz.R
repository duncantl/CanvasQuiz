readQuiz =
    #
    # Use htmlParse() rather than xmlParse() as the HTML content within a question may not be well formed.
    # If you do use xmlParse(), then the document will have a default namespace and we need to specify that
    # in the XPath queries.
    #
function(file, doc = htmlParse(file))
{
    qs = getQuestions(doc)
    metad = getMetadata(qs = qs)
    ans = mapply(processQuestion, qs, metad, SIMPLIFY = FALSE)
#    if(meta)
#        list(answers = ans, meta = metad)
#    else
#        ans
}

getMetadata =
function(doc, qs = getQuestions(doc))    
{
    lapply(qs, getQMetadata)
}

getQuestions =
    #
    # only the questions that have a resprocessing node, so ignore the 
    # "text only" 'Questions'.
    #
function(doc)
{
    q = getNodeSet(doc, "//item[@title = 'Question' and .//resprocessing]")
    names(q) = sapply(q, getQuestionName)
    q
}

getQuestionName =
function(q)
{
    xmlGetAttr(q, "label", xmlGetAttr(q, "title", xmlGetAttr(q, "ident")))
}

if(FALSE) {
processQuestion =
    #
    # Old version, overwritten below.
    #
function(q, meta = getQMetadata(q), prompt = TRUE)
{
    ansId = xpathSApply(q, ".//respcondition//varequal[starts-with(@respident, 'response')]", xmlValue)
    if(length(ansId)) {
        xquery = sprintf(".//response_label[ %s ]", paste(sprintf("@ident = '%s'", ansId), collapse = " or "))
        ans = xpathSApply(q, xquery, xmlValue)
    } else
        ans = ""
    score = xpathSApply(q, ".//respcondition/setvar[@varname = 'SCORE' and @action = 'Set']", xmlValue)
    if(length(score) == 0)
        score = NA
    
    ans = data.frame(answer = ans, score = score, stringsAsFactors = FALSE)

    if(prompt) 
        ans$prompt = xmlValue(getNodeSet(q, ".//presentation//mattext")[[1]])

    ans
}
}
 

processQuestion =
function(q, meta = getQMetadata(q), prompt = TRUE)
{
    ans = list()
    ans$prompt = xpathSApply(q, ".//presentation/material/mattext", xmlValue)
    ans$responses = xpathApply(q, ".//presentation/response_lid", getResponses, meta)
    ans$answer = getAnswer(q, meta)
    ans$meta = meta
    ans$info = xmlAttrs(q)
    
    ans
}

getResponses =
function(r, meta)
{
   xpathSApply(r, ".//response_label", function(x) structure(xmlValue(x), names = xmlGetAttr(x, "ident")))
}

getAnswer =
    #
    # Operate on the resprocessing node.
    # These have 1 outcomes and 1 or more respcondition nodes.
    # With formula questions, there may be an itemproc_extension sibling node of resprocessing.
    # The outcomes appears to be a sequence of one possibly more <decvar> nodes and these nodes have only attributes
    #   mavalue, minvalue, varname, vartype
    # See s[[1]]$decvarType
    #
    # The respcondition nodes have a conditionvar and setvar
function(q, meta)
{
# browser()   
    ansId = xpathSApply(q, ".//respcondition//varequal[starts-with(@respident, 'response')]", xmlValue)
    if(length(ansId)) {
        xquery = sprintf(".//response_label[ %s ]", paste(sprintf("@ident = '%s'", ansId), collapse = " or "))
        ans = xpathSApply(q, xquery, xmlValue)
        if(length(ans) == 0)
            ans = ansId
    } else
        ans = ""
    score = xpathSApply(q, ".//respcondition/setvar[@varname = 'SCORE' and @action = 'Set']", xmlValue)
    if(length(score) == 0)
        score = NA

    list(answer = ans, score = score)
}


getQMetadata =
function(q)
{
   xpathSApply(q, ".//qtimetadatafield", getQMetadataValue)
}

getQMetadataValue =
function(x)    
{
   structure( xmlValue(x[["fieldentry"]]), names = xmlValue(x[["fieldlabel"]]))
}
