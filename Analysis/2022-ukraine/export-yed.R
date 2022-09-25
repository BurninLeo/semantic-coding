export_yED = function(semantic, filename, edgeColor=NULL, groups=TRUE, colorRelationNode="#FFFFFF", colorEntityNode="#EEEEEE") {
    
    export_yED_entity = function(semantic, entity, nums, nodesDone, connection) {
        if (is.null(entity)) {
            stop("Received NULL ax entity")
            return(nodesDone)
        }
        entID = entity$canonize(semantic)
        if (entID %in% nodesDone) {
            return(nodesDone)
        }
        if (inherits(entity, "stcBlankNode")) {
            entRelation = semantic$getNamedRelation(entity$label)
            nodesDone = export_yED_relation(semantic, entRelation, nums, nodesDone, connection)
        } else if (inherits(entity, "stcLiteral")) {
            nodesDone = export_yED_literal(semantic, entity, nums, nodesDone, connection)
        } else if (inherits(entity, "stcIRI")) {
            # Get tags from relations for the color
            relList = semantic$getRelationsToEntity(entity)
            tags = new("stcTags")
            for (rel in relList) {
                tags = tags + rel$tags
            }
            if (inherits(edgeColor, "function")) {
                color = edgeColor(tags)
            } else {
                color = "#000000"
            }
            # Export the node
            nodesDone = export_yED_iri(semantic, entity, nums, nodesDone, color, connection)
        } else {
            stop(paste("Cannot export_yED_entity(", class(entity), ")"), sep="")
        }
        return(nodesDone)
    }
    
    export_yED_relations = function(semantic, connection) {
        f = connection
        nodesDone = c()
        # We need the semantic fully defined in its relation order
        semantic$sortByDefinition()
        # We also need numbers
        nums = semantic$getNodes()

        # start with the relations that define a new node
        # Go through in reversed order to properly catch groups
        for (i in length(semantic$relations):1) {
            relation = semantic$relations[[i]]
            
            if (relation$node()) {
                if (groups) {
                    nodesDone = export_yED_relation(semantic, relation, nums, nodesDone, connection)
                } else {
                    nodesDone = export_relation_node(semantic, relation, nums, nodesDone, connection)
                }
            }
        }
        
        # Then add the remaining nodes
        for (nodeID in names(nums)) {
            entity = semantic$getEntity(nodeID)
            if (!inherits(entity, "stcRelation")) {
                nodesDone = export_yED_entity(semantic, entity, nums, nodesDone, connection)
            }
        }
        
        # And finally the edges
        for (i in 1:length(semantic$relations)) {
            relation = semantic$relations[[i]]
            if (groups || !relation$node()) {
                export_edges_relation(semantic, relation, nums, connection)
            } else {
                export_edges_flat(semantic, relation, nums, connection)
            }
        }
    }
    
    # Export a relation as group with two noded within
    export_yED_relation = function(semantic, relation, nums, nodesDone, connection) {
        relNum = nums[[relation$canonize(semantic)]]
        writeLines(c(
            paste('<node id="n', relNum,'" yfiles.foldertype="group">', sep=""),
            '<data key="d6">',
            '<y:ProxyAutoBoundsNode>',
            '<y:Realizers active="0">',
            '<y:GroupNode>',
            paste('<y:Fill color="', colorRelationNode, '" transparent="false"/>', sep=""),
            paste(
                paste('<y:NodeLabel autoSizePolicy="node_width" configuration="CroppingLabel" backgroundColor="', "#b7b69e", '" modelName="internal" modelPosition="t" leftInset="16">', sep=""),
                # entitiesXML(relation$getDescription()),
                entitiesXML(relation$description),
                '</y:NodeLabel>', sep=""),
            '</y:GroupNode>',
            '</y:Realizers>',
            '</y:ProxyAutoBoundsNode>',
            '</data>',
            '<graph edgedefault="undirected" id="n0:">'
        ), connection)
        
        for (entity in c(relation$entityA, relation$entityB)) {
            nodesDone = export_yED_entity(semantic, entity, nums, nodesDone, connection)
        }
        
        # Close the group
        writeLines(c(
            '</graph>',
            '</node>'
        ), connection)
        
        return(nodesDone)
    }
    
    # Export a relation as separate node, do not export parts of the relation
    export_relation_node = function(semantic, relation, nums, nodesDone, connection) {
        relNum = nums[[relation$canonize(semantic)]]
        
        if (inherits(edgeColor, "function")) {
            color = edgeColor(relation$tags)
        } else {
            color = "#000000"
        }
        
        writeLines(c(
            paste('<node id="n', relNum,'">', sep=""),
            '<data key="d5">',
            '    <y:ShapeNode>',
            '    <y:Geometry height="43.44" width="200" />',
            paste('    <y:Fill color="', colorRelationNode, '" transparent="false"/>', sep=""),
            paste('    <y:BorderStyle color="', color, '" raised="false" type="line" width="1.0"/>', sep=""),
            '    <y:NodeLabel alignment="center" autoSizePolicy="node_width" configuration="CroppingLabel" fontSize="11" xml:space="preserve">',
                entitiesXML(relation$description),
            #'    <y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel>',
            # '    <y:ModelParameter><y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/></y:ModelParameter>',
            '    </y:NodeLabel>',
            '    <y:Shape type="roundrectangle"/>',
            '    </y:ShapeNode>',
            '</data>',
            '</node>'
        ), connection)
        
        return(nodesDone)
    }
    
    export_yED_literal = function(semantic, literal, nums, nodesDone, connection) {
        litID = literal$canonize(semantic)
        litNum = nums[[litID]]
        writeLines(c(
            paste('<node id="n', litNum, '">', sep=""),
            '<data key="d6">',
            '<y:ShapeNode>',
            '<y:Geometry height="30" width="150"/>',
            paste('<y:NodeLabel>', entitiesXML(literal$label), '</y:NodeLabel>', sep=""),
            '</y:ShapeNode>',
            '</data>',
            '</node>'
        ), connection)
        
        nodesDone = c(nodesDone, litID)
        return(nodesDone)
    }
    
    export_yED_iri = function(semantic, entity, nums, nodesDone, color, connection) {
        entID = entity$canonize(semantic)
        entNum = nums[[entID]]
        writeLines(c(
            paste('<node id="n', entNum, '">', sep=""),
            '<data key="d6">',
            '<y:ShapeNode>',
            '<y:Geometry height="30" width="150"/>',
            paste('<y:Fill color="', colorEntityNode, '" transparent="false"/>', sep=""),
            paste('<y:BorderStyle color="', color, '" type="line" width="3.0"/>', sep=""),
            paste('<y:NodeLabel autoSizePolicy="node_width" configuration="CroppingLabel">', entitiesXML(entity$label), '</y:NodeLabel>', sep=""),
            '</y:ShapeNode>',
            '</data>',
            '</node>'
        ), connection)
        
        nodesDone = c(nodesDone, entID)
        return(nodesDone)
    }
    
    export_yED_edge = function(na, nb, color, connection) {
        writeLines(c(
            paste('<edge id="n', na, '::n', nb, '" source="n', na, '" target="n', nb, '">', sep=""),
            '<data key="d9">',
            '<y:PolyLineEdge>',
            paste('<y:LineStyle color="', color, '" type="line" width="3.0"/>', sep=""),
            '<y:Arrows source="none" target="none"/>',
            '</y:PolyLineEdge>',
            '</data>',
            '</edge>'
        ), connection)
    }
    
    export_edges_relation = function(semantic, relation, nums, connection) {
        eaID = relation$entityA$canonize(semantic)
        ebID = relation$entityB$canonize(semantic)
        na = nums[[eaID]]
        nb = nums[[ebID]]
        
        if (inherits(edgeColor, "function")) {
            color = edgeColor(relation$tags)
        } else {
            color = "#000000"
        }
        
        export_yED_edge(na, nb, color, connection)
    }
    
    # Export the edges for a relation, either directly (no new entity created by the relation) or through the relation-node.
    export_edges_flat = function(semantic, relation, nums, connection) {
        # Need two edges
        eaID = relation$entityA$canonize(semantic)
        ebID = relation$entityB$canonize(semantic)
        reID = relation$canonize(semantic)
        na = nums[[eaID]]
        nb = nums[[ebID]]
        nr = nums[[reID]]
        
        if (inherits(edgeColor, "function")) {
            color = edgeColor(relation$tags)
        } else {
            color = "#000000"
        }
        
        export_yED_edge(na, nr, color, connection)
        export_yED_edge(nb, nr, color, connection)
    }
    
    entitiesXML = function(text) {
        return(gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", enc2utf8(text)))))
    }
    
    options("encoding" = "UTF-8")
    f = file(filename)
    open(f, open="w")
    # Start with the header
    writeLines(c(
        '<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
        '<graphml xmlns:y="http://www.yworks.com/xml/graphml">',
        '<key for="node" id="d5" yfiles.type="nodegraphics"/>',
        '<key for="node" id="d6" yfiles.type="nodegraphics"/>',
        '<key for="edge" id="d9" yfiles.type="edgegraphics"/>',
        '<graph edgedefault="undirected" id="G">'
    ), con=f, useBytes = T)
    
    # Go through the nodes, adding groups recursively
    export_yED_relations(semantic, f)
    
    # Add the edges
    
    # End with closing the header
    writeLines(c(
        '</graph>',
        '</graphml>'
    ), con=f, useBytes = T)
    close(f)
}