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
    q = getNodeSet(doc, "//item[.//resprocessing]")
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
    names(ans$responses) = xpathApply(q, ".//presentation/response_lid/material/mattext", xmlValue)
    
    ans$answer = getAnswer(q, meta)

    ans$meta = meta
    ans$info = xmlAttrs(q)

    ans$points = as.numeric(meta['points_possible'])

    ans$feedback = getFeedback(q, meta)
    
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

    #
    #  If the conditionvar has an immediate child <varequal>, then grab that value.
    #  If the <varequal> is not the immediate child, then it is a richer condition, e.g, <and>, <or>, <not>.
    #
    #
    # Some questions have two <respcondition> nodes corresponding to continue="No" or "Yes".
    # It appears this arises when there is feedback for the wrong answer.
    #
    #
    #
function(q, meta)
{
    ansId = xpathSApply(q, ".//respcondition/conditionvar/varequal[starts-with(@respident, 'response')]", xmlValue)
    if(length(ansId)) {
        ans = lookupLabel(ansId, q)
        if(length(ans) == 0)
            ans = ansId
    } else if(length(rp <- getNodeSet(q, ".//respcondition"))) {
        # Richer condition.
        if(length(rp) > 1) {
            warning("skipping additioinal respcondition(s)")
#            browser()
        }
        ans = mkCondition(rp[[1]][[1]][[1]], q, meta)
    } else
        ans = ""
 
    score = xpathSApply(q, ".//respcondition/setvar[@varname = 'SCORE' and @action = 'Set']", xmlValue)
    if(length(score) == 0)
        score = NA

    list(answer = ans, score = score)
}

lookupLabel =
function(id, q, addVarName = FALSE)
{
    xquery = sprintf(".//response_label[ %s ]", paste(sprintf("@ident = '%s'", id), collapse = " or "))
    ans = xpathSApply(q, xquery, xmlValue)
    if(length(ans) == 0)
       return(id)

    if(addVarName) {
        # Get the name of the variable of which this value one possible answer, e.g. color1.
        xquery = sprintf(".//response_label[ %s ]/ancestor::response_lid/material/mattext", paste(sprintf("@ident = '%s'", id), collapse = " or "))
        names(ans) = xpathSApply(q, xquery, xmlValue)
    }
    
    ans
}


mkCondition =
    #
    # or, and, not, other
    #
    #  vargte varlte
    #
    # not, and, or, unanswered, other, varequal, varlt, varlte, vargt, vargte, varsubset, varinside, varsubstring,
    #  durequal, durlt, durlte, durgt, durgte, var_extension
    #
    # can see this from the schema - qti$conditionvarType@elType@slotTypes
    #
function(cv, q, meta)
{
   #if(getQuestionName(q) == "Range") browser()
    
   op = xmlName(cv)
   els = lapply(xmlChildren(cv), mkConditionEl, q)
   structure(list(operator = op, els = els), class = "Condition")
}

mkConditionEl =
function(node, q)
{
    nm = xmlName(node)
    switch(nm,
           varequal =,
           vargte =,
           varlte = lookupLabel(xmlValue(node), q),
           other = NULL,
           and =,
           or =,
           not = mkCondition(node, q))  # left out q here and got an error of course. Perhaps nice example to illustrate debuggging
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



getFeedback =
function(q, meta)
{
   xpathSApply(q, ".//itemfeedback", function(x) structure(xmlValue(x), names = xmlGetAttr(x, 'ident')))
}
