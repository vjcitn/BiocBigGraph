#' no bas
#' @examples
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' co = simple_conf(p3k, dynamic_relations=FALSE)
#' co
#' @export
simple_conf = function(sce, workdir=tempdir(), N_GENES=1000, N_BINS=5, 
   N_PARTITIONS=1L, FEATURIZED=FALSE, entity_path="ents",
    ...) {
  N_GENES = as.integer(N_GENES)
  N_BINS = as.integer(N_BINS)
  
  tsvtarget = paste0(tempfile(tmpdir=workdir), ".tsv")
#  sce_to_triples(sce, outtsv=tsvtarget, ngenes=N_GENES, n_bins=N_BINS)

#  basilisk::basiliskRun(proc, function(sce, workdir, N_PARTITIONS, FEATURIZED,
#       entity_path, ... ) {
#
# get python modules direct from included source
# as of 25.02.2024 there is no accommodation of GPU which requires
# C++ compilation of some code in fbsource
#
   fbloc = system.file("python", "fbsource", package="BiocBigGraph")
   tor = import_from_path("torchbiggraph", path=fbloc)
   conf = import_from_path("torchbiggraph.config", path=fbloc)
   imps = import_from_path("torchbiggraph.converters.importers", path=fbloc)
   
#
# get configuration basics, to be used later
#
     confin = setup_config_schema_optionals(...)
     if (confin$bucket_order_string != "INSIDE_OUT")
          stop("configure bucket_order_string unrecognized")
     confin$bucket_order = conf$BucketOrder$INSIDE_OUT
     badind = which(names(confin) == "bucket_order_string")
     confin = confin[-badind]

#
# create entity schemas for cells and genes
#
   entC = conf$EntitySchema(num_partitions = N_PARTITIONS, 
            featurized=FEATURIZED, dimension=confin$dimension)
   entG = conf$EntitySchema(num_partitions = N_PARTITIONS, 
            featurized=FEATURIZED, dimension=confin$dimension)
   ents = reticulate::dict(C = entC, G = entG)

#
# set up weights which define 'relations' or edge types in graph  
# this is dictated by number of bins into which expression values
# were mapped
#
   group_levels = seq_len(N_BINS)
   group_tags = paste0("r", group_levels - 1L)
   wts = 1.0*group_levels # make float
   rels = lapply( group_levels, function(x) 
    conf$RelationSchema(
      name=group_tags[x],
      lhs='C',
      rhs='G',
      weight=wts[x],
      operator='none',
      all_negs=FALSE
    ))

# 
# build configuration schema
#
# set up args
#
    entity_path = file.path(workdir, entity_path)
    edge_paths = file.path(workdir, c("train", "validate", "test"))
    checkpoint_path = file.path(workdir, c("chkpt"))

    confargs = c(entities = ents, relations = r_to_py(rels), entity_path = entity_path,
     edge_paths = r_to_py(edge_paths), checkpoint_path = checkpoint_path, confin)
#
# call the ConfigSchema method
#
    confsch = do.call(conf$ConfigSchema, confargs)
    palib = try(reticulate::import("pathlib"))
    if (inherits(palib, "try-error")) stop("be sure pathlib can be imported by reticulate")
    confsch
  trpaths = reticulate::r_to_py(lapply(unname(edge_paths),
      palib$Path))
  imps$convert_input_data(
    confsch$entities,
    confsch$relations,
    confsch$entity_path,
    confsch$edge_paths,
    trpaths,
    imps$TSVEdgelistReader(lhs_col=0L, rhs_col=2L, rel_col=1L),
    dynamic_relations = confsch$dynamic_relations)
#
# OK?
#

  }
