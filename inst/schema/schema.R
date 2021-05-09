library(XMLSchema)
s = readSchema("../schema.xsd")
saveRDS(s, 'QTISchema.rds')

# schema for two namespaces
names(s)
# Types, etc. for the QTI schema
names(s[[1]])

# Many entries with name ...Type
# and also 141 Element types corresponding to the XML node.

qti = s[[1]]
qti$itemType
