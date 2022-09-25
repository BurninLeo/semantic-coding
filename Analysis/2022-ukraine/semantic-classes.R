require(igraph)

# Define the classes for semantic text coding (stc)

setRefClass("stcElement", fields=list(label="character"))

# Little class for tagging

setRefClass("stcTags", fields=c("tags"), methods=list(
    
    initialize = function(tags=NULL) {
        iTags = unique(tags)
        if (length(iTags) == 0) {
            tags <<- NULL
        } else {
            tags <<- iTags
        }
    },
    
    add = function(x) {
        tags <<- unique(c(tags, x))
    },
    
    contains = function(x) {
        return(any(x %in% tags))
    }
        
))
setMethod("+", signature(e1="stcTags", e2="stcTags"),
      function(e1, e2) {
          return(new("stcTags", tags=c(e1$tags, e2$tags)))
      }
)

# N-Quads

setRefClass("stcIRI", fields=list(id="character"), contains = c("stcElement"), methods=list(
    initialize = function(id="NONE", label=NA) {
        id <<- id
        if (is.na(label) || is.null(label)) {
            label <<- id
        } else {
            label <<- label
        }
    },
    canonize = function(text, level=0) {
        return(paste("<", id, ">", sep=""))
    },
    setLabel = function(label) {
        label <<- label
    }
))
setRefClass("stcLiteral", contains = c("stcElement"), methods=list(
    canonize = function(text, level=0) {
        return(label)
    }
))
setRefClass("stcBlankNode", contains = c("stcElement"), methods=list(
    initialize = function(label="") {
        label <<- label
        
        # Init cache for canonical name
        env = environment(fun = .self$canonize)
        env$canonicalID = NA
    }, 
    canonize = function(text, level=0) {
        # Use cache
        env = environment(fun = .self$canonize)
        if (is.na(env$canonicalID)) {
            relation = text$getNamedRelation(label)
            if (inherits(relation, "stcRelation")) {
                env$canonicalID = relation$canonize(text)
            } else {
                env$canonicalID = paste("[dead_node:", label, "]")
            }
        }
        return(env$canonicalID)
    },
    empty = function() {
        return ((length(label) == 0) || is.na(label) || (trimws(label) == ""))
    }
))

setMethod("show", "stcIRI", function(object) {
    cat("[IRI]", object$canonize(), sep=" ")
})

setMethod("show", "stcLiteral", function(object) {
    cat("[Literal] \"", object$label, "\"", sep="")
})

setMethod("show", "stcBlankNode", function(object) {
    cat("[BlankNode]", object$label)
})
setMethod("as.logical", "stcBlankNode", function(x) {
    return ((length(x$label) != 0) && !is.na(x$label) && (trimws(x$label) != ""))
})

# Relations

setRefClass("stcRelation", fields = list(entityA="stcElement", operator="stcIRI", entityB="stcElement", id="stcBlankNode", description="character", source="character", attributes="vector", tags="stcTags"),
    methods = list(
        
        initialize = function(entityA, operator, entityB, id=NULL, description="", source="", tags=NULL, attributes=NULL) {
            entityA <<- entityA
            entityB <<- entityB
            operator <<- operator
            # Character fields
            description <<- description
            source <<- source
            # Optional fields
            if (is.null(id)) {
                id <<- new("stcBlankNode")
            } else {
                id <<- id
            }
            if (length(tags) == 0) {
                tags <<- new("stcTags")
            } else {
                tags <<- tags
            }
            if (!is.null(attributes) && is.vector(attributes)) {
                attributes <<- attributes
            }
        },
        
        canonize = function(text, level=0) {
            if (level > 10) {
                print(.self)
                stop("Unable to canonize")
            }
            return(
                paste("(", entityA$canonize(text, level+1), " ", operator$canonize(text), " ", entityB$canonize(text, level+1), ")", sep="")
            )
        },
        
        containsAttribute = function(attrID) {
            return(attrID %in% attributes)
        },
        
        copy = function() {
            rc = new("stcRelation", entityA = entityA$copy(), operator = operator$copy(), entityB = entityB$copy(), id = id$copy(), description=description, source=source, tags = tags$copy())
            # Also copy the attributes
            if (!is.null(attributes)) {
                rc$attributes = attributes
            }
            # Return the resulting copy
            return(rc)
        },
        
        getDescription = function() {
            return(
                paste(entityA$label, " <-- ", operator$label, " --> ", entityB$label, sep="")
            )
        },
        
        getOutput = function() {
            desc = getDescription()
            relID = id
            if (as.logical(relID)) {
                desc = paste(desc, "[[", id$label, "]]", sep=" ")
            }
            # Any name?
            if ((length(description) != 0) && (description != "")) {
                desc = paste(desc, "\n>>", description, "<<", sep="")
            }
            # Any attributes
            if (length(attributes) > 0) {
                for (attr in attributes) {
                    desc = paste(desc, "\n*", attr, sep="")
                }
            }
            return(desc)
        },
        
        # Tell if this relation defines a node by itfeld.
        # That requires a BlankNode plus a description for that
        node = function() {
          if (id$empty()) {
            return(FALSE)
          }
          if (description == "") {
            return(FALSE)
          }
          return(TRUE)
        }
))

setMethod("show", "stcRelation", function(object) {
    cat(object$getOutput())
})


# Complete semantic of a text
# This is encapsulated into a class in order to allow relation-management (i.e., extract attributes)
setRefClass("semantic", fields = c("name", "relations"), methods = list(
    
    initialize = function(name, relations) {
        # Must take care of setting the variables
        name <<- as.character(name)
        
        # Initialize relations
        relations <<- c()
        env = environment(fun = .self$add)
        env$namedRelations = list()
        
        # Initialize nodes (list of node desc => number)
        env$nodes = list()
        env$entities = list()
        
        # Import relations, if given
        if (length(relations) > 0) {
            for (relation in relations) {
                .self$add(relation)
            }
        }
    },
    
    add = function(relation) {
        if (!inherits(relation, "stcRelation")) {
            stop(paste("Cannot use element of class [", class(relation), "] for semantic$add(), expecting a [stcRelation]", sep=""))
        }
        
        # Maybe, this is "only" an attribute or a description for a previous relation
        if (inherits(relation$entityA, "stcBlankNode") && (relation$operator$label == "stc://base-0-4/has_attribute")) {
            # Lookup the names relation
            relName = relation$entityA$label
            namedRelation = getNamedRelation(relName)
            if (inherits(namedRelation, "stcRelation")) {
                attrName = relation$entityB$label
                namedRelation$attributes = c(namedRelation$attributes, attrName)
                return()
            } else {
                warning(paste("There is relation [", relName, "] to assign attribute to", sep=""))
            }
        }
        if (inherits(relation$entityA, "stcBlankNode") && (relation$operator$label == "http://purl.org/dc/terms/title")) {
            # Lookup the names relation
            relName = relation$entityA$label
            namedRelation = getNamedRelation(relName)
            if (inherits(namedRelation, "stcRelation")) {
                namedRelation$description = relation$entityB$label
                return()
            } else {
                warning(paste("There is relation [", relName, "] to assign description to", sep=""))
            }
        }
        
        
        # Enlist
        relIndex = length(relations) + 1
        relations[[relIndex]] <<- relation
        
        # check if the name is already in use
        if (as.logical(relation$id)) {
            relName = relation$id$label
            # Check if this name is already in use
            env = environment(fun = .self$add)
            if (is.null(env$namedRelations[[relName]])) {
                env$namedRelations[[relName]] = relIndex
            } else {
                warning(paste("Relation name [[", relName, "]] is not unique."))
            }
        }
    },
    
    # Add tags to all relations in the semantic
    # @param stcTags tags The tag(s) to add
    # @param vector excludeAttributes Do not apply the tag to relations having this attribute
    addTags = function(tags, excludeAttributes=c("stc://base-0-4/relation:background")) {
        for (i in 1:length(relations)) {
            # Eventually exclude background knowledge
            if (!any(excludeAttributes %in% relations[[i]]$attributes)) {
                relations[[i]]$tags <<- relations[[i]]$tags + tags
            }
        }
    },
    
    copy = function() {
        # Need to copy all the relations
        relC = NULL
        for (r in relations) {
            relC = c(relC, r$copy())
        }
        return(new("semantic", name=name, relations=relC))
    },
    
    getBlankNodeLabels = function() {
        labels = NULL
        for (r in relations) {
            if (!r$id$empty()) {
                labels = c(labels, r$id$label)
            }
        }
        return(unique(labels))
    },
    
    getNamedRelation = function(name) {
        env = environment(fun = .self$add)
        if (is.null(env$namedRelations[[name]])) {
            return(NA)
        }
        index = env$namedRelations[[name]]
        return(relations[[index]])
    },
    
    getNodes = function() {
        refreshNodes()
        env = environment(fun = .self$add)
        return(env$nodes)
    },
    
    getEntity = function(id) {
        env = environment(fun = .self$add)
        return(env$entities[[id]])
    },
    
    # Find all relations that connect to the specified entity
    getRelationsToEntity = function(entity) {
        reList = NULL
        entCan = entity$canonize(.self)
        for (relation in relations) {
            if ((relation$entityA$canonize(.self) == entCan) || (relation$entityB$canonize(.self) == entCan)) {
                reList = c(reList, relation)
            }
        }
        return(reList)
    },
    
    # Replace IRIs that are connected via <stc://base/identical> or another given operator
    # @param {character} operator One or more operator(s) to use for identity, if not just <stc://base/identical>
    # @return {semanic} A copy of the orignal seminatic with identical entities joined
    #
    mergeIRIs = function(operator="stc://base/identical") {
        # First, we need to find the relations the define identical-relations
        # And we will also remove the <identical> relations
        newRelations = NULL
        reList = list()
        for (rel in relations) {
            if (rel$operator$id %in% operator) {
                reList[[rel$entityA$id]] = rel$entityB$id
            } else {
                newRelations = c(newRelations, rel)
            }
        }
        # No go through all relations and replace the aliases
        lookup = names(reList)
        for (rel in newRelations) {
            if (inherits(rel$entityA, "stcIRI") && (rel$entityA$id %in% lookup)) {
                rel$entityA$id = reList[[rel$entityA$id]]
            }
            if (inherits(rel$entityB, "stcIRI") && (rel$entityB$id %in% lookup)) {
                rel$entityB$id = reList[[rel$entityB$id]]
            }
        }
        # Set the new relations to be used in the semantic - and use a new one, as we need to do the naming stuff
        return(new("semantic", name=name, relations=newRelations))
    },
    
    refreshNodes = function() {
        # Give each node a number
        # (cannot do this before all relations have been read, as named relations may be missing before)
        for (i in 1:length(relations)) {
            relation = relations[[i]]
            registerNode(relation$entityA)
            registerNode(relation$entityB)
            # and maybe the relation itself
            if (!relation$id$empty()) {
                registerNode(relation)
            }
        }
    },
    
    registerNode = function(entity) {
        if (!(inherits(entity, "stcElement") || inherits(entity, "stcRelation"))) {
            stop(paste("The entity for semantic$registerNode() must be a stcElement or stcRelation, got", class(entity)))
        }
        entityID = entity$canonize(.self)
        env = environment(fun = .self$add)
        if (is.null(env$nodes[[entityID]])) {
            env$nodes[[entityID]] = length(env$nodes) + 1
            env$entities[[entityID]] = entity
        }
    },
    
    # Return a vector with the same length like the the relations
    # that is TRUE for each relations that contains the IRI and
    # FALSE for any other relation
    # @param {character} iri One or more IRI IDs
    # @return logical
    relationsContainIRI = function(iri) {
        reNums = c()
        for (reNum in 1:length(relations)) {
            rel = relations[[reNum]]
            if (
                (inherits(rel$entityA, "stcIRI") && (rel$entityA$id %in% iri)) ||
                (inherits(rel$entityB, "stcIRI") && (rel$entityB$id %in% iri)) ||
                (rel$operator$id %in% iri)
            ) {
                reNums = c(reNums, T)
            } else {
                reNums = c(reNums, F)
            }
        }
        return(reNums)
    },
    
    # Return a vector with the same length like the the relations
    # that is TRUE for each relations that contains one of the given tag(s)
    # FALSE for any other relation
    relationsContainTag = function(tags) {
        reNums = c()
        for (reNum in 1:length(relations)) {
            rel = relations[[reNum]]
            if (rel$tags$contains(tags)) {
                reNums = c(reNums, T)
            } else {
                reNums = c(reNums, F)
            }
        }
        return(reNums)
    },
    
    # Print the relations in the semantic with there original IRIs
    relationsPrint = function() {
        for (rel in relations) {
            if (inherits(rel$entityA, "stcIRI")) {
                labelA = rel$entityA$id
            } else {
                labelA = rel$entityA$label
            }
            if (inherits(rel$entityB, "stcIRI")) {
                labelB = rel$entityB$id
            } else {
                labelB = rel$entityB$label
            }
            
            cat(paste(
                "[", labelA, "] ",
                "<", rel$operator$id, "> ",
                "[", labelB, "]",
                "\n", sep=""
            ))
            for (attr in rel$attributes) {
                cat(paste(
                    " * ", attr,
                    "\n", sep=""
                ))
            }
        }
    },
    
    # Change the names of BlankNodes, using other names than listes in "avoid"
    renameBlankNodes = function(avoid=labels) {
        # Also cannot use the labels already used by own nodes
        own = .self$getBlankNodeLabels()
        # Which to we have to replace
        for (l_search in intersect(own, avoid)) {
            # Count trailing number up until its free
            root = gsub("\\d+$", "", l_search)
            count = 0
            repeat {
                count = count + 1
                l_replace = paste(root, count, sep="")
                if (!((l_replace %in% avoid) || (l_replace %in% own))) {
                    break
                }
            }
            
            # Replace all occurences of "l_search" for "l_replace"
            for (r in relations) {
                if (inherits(r$entityA, "stcBlankNode") && r$entityA$label == l_search) {
                    r$entityA$label = l_replace
                }
                if (inherits(r$entityB, "stcBlankNode") && r$entityB$label == l_search) {
                    r$entityB$label = l_replace
                }
                if (inherits(r$id, "stcBlankNode") && !r$id$empty() && r$id$label == l_search) {
                    r$id$label = l_replace
                }
            }
            own = c(own, l_replace)
        }
    },
    
    # Extract a part of the semantic that has certain attribute(s)
    # @param character[] attributes
    # @return semantic
    slice = function(attributes) {
        subRelations = NULL
        ix = 0
        for (relation in relations) {
            if (any(attributes %in% relation$attributes)) {
                ix = ix + 1
                subRelations[[ix]] = relation
            }
        }
        newSemantic = new("semantic", name=name, relations=subRelations)
        return(newSemantic)
    },
    
    # Sort the relations so that each BlankNode is defined before used
    sortByDefinition = function() {
        unsorted = relations
        defined = NULL
        sorted = NULL
        
        checkDefined = function(entity) {
            if (!inherits(entity, "stcBlankNode") || entity$empty()) {
                return(TRUE)
            }
            blankID = entity$label
            if (blankID %in% defined) {
                return(TRUE)
            }
            return(FALSE)
        }
        
        ri = 1
        # Go through the list until everything is sorted
        while ((ri <= length(unsorted)) && (length(unsorted) > 0)) {
            # Check if we can sort this relation
            r = unsorted[[ri]]
            if (checkDefined(r$entityA) && checkDefined(r$entityB)) {
                # Move to the sorted list
                sorted = c(sorted, r)
                unsorted = unsorted[-ri]
                # May define a BlankNode itself
                if (!r$id$empty() && !(r$id$label %in% defined)) {
                    defined = c(defined, r$id$label)
                    # Restart
                    ri = 1
                    next
                }
            } else {
                ri = ri + 1
            }
        }
        if (length(unsorted) > 0) {
            stop(paste("Undefined BlankNodes in semantic:\n", paste(sorted, collapse = "\n")))
        }
        
        relations <<- sorted
        
        # Refresh the name cache
        env = environment(fun = .self$add)
        env$namedRelations = NULL
        for (relIndex in 1:length(relations)) {
            relation = relations[[relIndex]]
            if (!relation$id$empty()) {
                relName = relation$id$label
                # Check if this name is already in use
                if (is.null(env$namedRelations[[relName]])) {
                    env$namedRelations[[relName]] = relIndex
                } else {
                    warning(paste("Relation name [[", relName, "]] is not unique."))
                }
            }
        }
    },
    
    #b = methods[[1]]$copy()
    #relations = rev(b$relations)
    
    bagOfEntities = function() {
        bag = NULL
        for (r in 1:length(relations)) {
            relation = relations[[r]]
            entityA = relation$entityA$canonize(.self)
            entityB = relation$entityB$canonize(.self)
            bag = rbind(bag,
                c("text" = name, "entity" = entityA, "pos" = "left"),
                c("text" = name, "entity" = entityB, "pos" = "right")
            )
        }
        return(bag)
    },
    
    # Create a edge list for igraph, first try
    edgelist = function(includeConcepts=F) {
        elist = NULL
        for (r in 1:length(relations)) {
            relation = relations[[r]]
            elist = rbind(elist,
                  c(relation$entityA$canonize(.self), relation$entityB$canonize(.self))
            )
            # Plus edges between singular nodes (mostly entities) and nodes created by relations (group nodes)
            if (includeConcepts && !relation$id$empty()) {
                relID = relation$canonize(.self)
                elist = rbind(elist, c(relation$entityA$canonize(.self), relID))
                elist = rbind(elist, c(relation$entityB$canonize(.self), relID))
            }
        }
        return(elist)
    },
    
    # Return a list of IDs and one colum per tag
    tagMatrix = function(includeConcepts=F) {
        tags = NULL
        matrix = data.frame()
        for (r in 1:length(relations)) {
            tagsA = relation$entityA$tags
            
            tagsB = relation$entityA$tags
            if (includeConcepts && !relation$id$empty()) {
                tagsC = relation$tags
            }
        }
    },
    
    verticeData = function(includeConcepts=F) {
        data = NULL
        
        encodeType = function(entity) {
            if (inherits(entity, "stcBlankNode")) {
                type = "blank"
            } else if (inherits(entity, "stcLiteral")) {
                type = "literal"
            } else if (inherits(entity, "stcIRI")) {
                type = "iri"
            } else {
                type = NA
            }
            return(type)
        }
        
        tags = c()
        
        for (r in 1:length(relations)) {
            relation = relations[[r]]
            
            # We may or may not encouter new tags
            rTags = relation$tags$tags
            nTags = rTags[!(rTags %in% tags)]
            if (length(nTags) > 0) {
                tags = c(tags, nTags)
            }
            
            # Add the information on these nodes
            data = rbind(data,
                     c("id" = relation$entityA$canonize(.self), "type" = encodeType(relation$entityA), "tags" = relation$tags),
                     c("id" = relation$entityB$canonize(.self), "type" = encodeType(relation$entityB), "tags" = relation$tags)
            )
            if (includeConcepts && !relation$id$empty()) {
                data = rbind(data,
                     c("id" = relation$canonize(.self), "type" = "relation", "tags" = relation$tags)
                )
            }
        }
        tags = sort(tags)
        
        # The dataframe will contain a "List" for each variable, probably due to the refclass in the last variable
        df = data.frame(data)
        # names(df) = c("id", "type", paste("tag.", tags, sep=""))
        # names(df) = c("id", "type", "tags")
        df$id = as.character(unlist(df$id))
        df$type = as.character(unlist(df$type))
        
        # Now, we need to de-duplicate by ID, but merge the tags
        idList = sort(unique(df$id))
        # Init the de-duplicated list with tags as logicals
        dfu = data.frame(row.names = idList)
        dfu$id = NA
        dfu$type = NA
        dfu$occur = NA
        for (tag in paste("tag.", tags, sep="")) {
            dfu[[tag]] = NA
        }
        # Do the actualy merging of the tags
        for (id in idList) {
            entries = df[df$id == id,]
            entTags = entries$tags
            eTags = entTags[[1]]
            if (length(entTags) > 1) {
                for (en in 2:length(entTags)) {
                    # Merge the tags
                    eTags = eTags + entTags[[en]]
                }
            }
            tagMatch = (tags %in% eTags$tags)
            dfu[id,] = c(id=id, type=entries[[1, "type"]], occur=length(entTags), tagMatch)
        }
        
        dfu$type = as.factor(dfu$type)
        dfu$occur = as.numeric(dfu$occur)
        for (tag in paste("tag.", tags, sep="")) {
            dfu[[tag]] = as.logical(dfu[[tag]])
        }
        
        return(dfu)
    }
))
#tmp = new("semantic", name="S1")
#tmp = importNQuads2("text380.nq")
#tmp2 = new("semantic", name="Test", relations=tmp)
# str(tmp2$relations)
#tmp2

setMethod("+", signature(e1="semantic", e2="semantic"),
      function(e1, e2) {
          # Need to rename BlankNodes
          e2c = e2$copy()
          e2c$renameBlankNodes(avoid=e1$getBlankNodeLabels())
          # Add name and relations
          return(new("semantic",
              name = paste(e1$name, e2c$name, sep="+"),
              relations = c(e1$relations, e2c$relations)
          ))
      }
)
setMethod("as.igraph", signature(x="semantic"),
    function(x) {
        return(graph_from_data_frame(
            x$edgelist(T), 
            vertices = x$verticeData(T),
            directed=FALSE
        ))
    }
)

# tmp = methods[["text380"]] + methods[["text423"]]
# methods[["text423"]]$renameBlankNodes(avoid=methods[["text380"]]$getBlankNodeLabels())
