read =
    #
    # Use htmlParse() rather than xmlParse() as the HTML content within a question may not be well formed.
    # If you do use xmlParse(), then the document will have a default namespace and we need to specify that
    # in the XPath queries.
    #
function(file, meta = TRUE, doc = htmlParse(file))
{
    qs = getQuestions(doc)
    metad = getMetadata(qs = qs)
    ans = mapply(processQuestion, qs, metad, SIMPLIFY = FALSE)
    if(meta)
        list(answers = ans, meta = metad)
    else
        ans
}

getMetadata =
function(doc, qs = getQuestions(doc))    
{
    do.call(rbind, lapply(qs, getQMetadata))
}

getQuestions =
function(doc)
{
    q = getNodeSet(doc, "//item[@title = 'Question' and .//resprocessing]")
    names(q) = sapply(q, xmlGetAttr, "ident")
    q
}

processQuestion =
function(q, meta = getQMetadata(q), prompt = TRUE)
{
browser()    
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

getQMetadata =
function(q)
{
   xpathApply(q, ".//qtimetadatafield", getQMetadataValue)
}

getQMetadataValue =
function(x)    
{
   structure( xmlValue(x[["fieldentry"]]), names = xmlValue(x[["fieldlabel"]]))
}
