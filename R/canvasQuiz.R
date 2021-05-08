read =
function(file, meta = TRUE, doc = htmlParse(file))
{
    qs = getQuestions(doc)
    ans = lapply(qs, processQuestion)
    if(meta)
        list(answers = ans, meta = getMetadata(qs = qs))
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
function(q)
{
    ansId = xpathSApply(q, ".//respcondition//varequal[@respident = 'response1']", xmlValue)
    if(length(ansId)) {
        xquery = sprintf(".//response_label[ %s ]", paste(sprintf("@ident = '%s'", ansId), collapse = " or "))
        ans = xpathSApply(q, xquery, xmlValue)
    } else
        ans = ""
    score = xpathSApply(q, ".//respcondition/setvar[@varname = 'SCORE' and @action = 'Set']", xmlValue)
    if(length(score) == 0)
        score = NA
    
    data.frame(answer = ans, score = score, stringsAsFactors = FALSE)
}

getQMetadata =
function(q)
{
   xpathApply(q, ".//qtimetadatafield", getQMetadata)
}

getQMetadata =
function(x)    
{
   structure( xmlValue(x[["fieldentry"]]), names = xmlValue(x[["fieldlabel"]]))
}
