

test_that("python references work", {

 impm = c("ABC", "AbstractEdgeAppender", "AbstractEdgeStorage", "AbstractEntityStorage", 
"abstractmethod", "AbstractRelationTypeStorage", "Any", "append_to_file", 
"collect_entities_by_type", "collect_relation_types", "convert_input_data", 
"Counter", "datetime", "Dict", "Dictionary", "EDGE_STORAGES", 
"EdgeList", "EdgelistReader", "ENTITY_STORAGES", "EntityList", 
"EntitySchema", "ExitStack", "generate_edge_path_files", "generate_entity_path_files", 
"Iterable", "List", "log", "Optional", "ParquetEdgelistReader", 
"parse_config_partial", "Path", "random", "RELATION_TYPE_STORAGES", 
"RelationSchema", "torch", "TSVEdgelistReader", "Tuple", "UNPARTITIONED"
)

 z = check_bigg()
 expect_true( all(c("tor", "conf", "imps") %in% names(z)) )
 expect_true( all(impm %in% z$imps) )
})
