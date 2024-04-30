#' complete simple processing of SCE
#' @param sce SingleCellExperiment instance ready for conversion with 'sce_to_triples'
#' @param cell_id_var character(1) mandatory selection of
#' column of colData(sce) that is to be used as colnames
#' (which is often missing)
#' @param workdir location of temporary storage, defaults to tempdir()
#' @param N_GENES numeric(1) passed to 'sce_to_triples'
#' @param N_BINS numeric(1) passed to 'sce_to_triples' for discretization
#' @param cuts numeric() passed to 'sce_to_triples' for discretization
#' @param colname character(1) passed to 'sce_to_triples' identifying 'cell' entity token
#' @param N_PARTITIONS integer(1) for PytorchBigGraph configuration
#' @param FEATURIZED logical(1) for PytorchBigGraph configuration
#' @param entity_path character(1) subfolder of tempdir for entity recording
#' @param \dots passed directly to `set_config`
#' @note Any changes to configuration can be passed via ....  For example,
#' `num_epochs` defaults to 5L, pass directly in this function.
#' @return SingleCellExperiment with additional altExp and rowData corresponding to cell and gene embeddings
#' @examples
#' # NB as of April 2024 we do not control the random operations
#' # of PyTorch BigGraph ... this example is not reproducible
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' assay(p3k) = as.matrix(assay(p3k)) # dense for now
#' p3k = scuttle::logNormCounts(p3k)
#' set.seed(1234)
#' co = CG_embed_simple(p3k, cell_id_var="Barcode", num_epochs=6L,
#'    N_GENES=50, dynamic_relations=FALSE, colname="Barcode")
#' co
#' pp = prcomp(t(assay(altExp(co,"pbg_cell_emb"))))
#' data(p3k.fine)
#' coar = sapply(strsplit(p3k.fine, ":"), "[", 1)
#' pairs(pp$x[,1:4], pch=".", col=factor(coar))
#' library(ggplot2)
#' mdf = data.frame(pp$x[,c(2,4)], type=coar)
#' ggplot(mdf, aes(x=PC2, y=PC4, colour=coar, text=coar)) + 
#'     geom_point()
#' @export
CG_embed_simple = function(sce, cell_id_var, workdir=tempdir(), N_GENES=1000, N_BINS=5, 
   N_PARTITIONS=1L, FEATURIZED=FALSE, entity_path="ents", cuts=NULL, colname=NULL,
    ...) {
  if (missing(cell_id_var)) stop("cell_id_var must be supplied")
  if (!(cell_id_var %in% names(colData(sce)))) stop(
         "cell_id_var not in colData(sce)")
#  set_train() # this is a quick internal pip run because
  # the training module seems to lose track of where
  # torchbiggraph can be loaded from
  N_GENES = as.integer(N_GENES)
  N_BINS = as.integer(N_BINS)
  
  clkeep = match.call()
  tsvtarget = paste0(tempfile(tmpdir=workdir), ".tsv")
  sce_to_triples(sce, outtsv=tsvtarget, ngenes=N_GENES, n_bins=N_BINS,
     cuts=cuts, colname=colname)

#
# get python modules direct from included source
# as of 25.02.2024 there is no accommodation of GPU which requires
# C++ compilation of some code in fbsource
#
#   fbloc = system.file("python", "fbsource", package="BiocBigGraph")
   torc = import("torch")
   torc$manual_seed(0)
   torc$use_deterministic_algorithms(mode=FALSE, warn_only=TRUE)
   rand = import("random")
   rand$seed(0)
   #tor = import_from_path("torchbiggraph", path=fbloc)
   #conf = import_from_path("torchbiggraph.config", path=fbloc)
   #imps = import_from_path("torchbiggraph.converters.importers", path=fbloc)
   #ut = import_from_path("torchbiggraph.util", path=fbloc)
   #trainer = import_from_path("torchbiggraph.train", path=fbloc)
   tor = import("torchbiggraph")
   conf = import("torchbiggraph.config")
   imps = import("torchbiggraph.converters.importers")
   ut = import("torchbiggraph.util")
   trainer = import("torchbiggraph.train")
#
# assume pathlib is available
#
   palib = import("pathlib")
   
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
    edge_paths = rep(" ", 3)
    edge_paths[1] = tsvtarget
    file.create(file.path(workdir, "validate"))
    edge_paths[2] = file.path(workdir, "validate")
    file.create(file.path(workdir, "test"))
    edge_paths[3] = file.path(workdir, "test")
    edge_paths_out = file.path(workdir, c("train_out", "validate_out", "test_out"))
#    sapply(edge_paths_out, file.create)
    checkpoint_path = file.path(workdir, c("chkpt"))

    confargs = c(entities = ents, relations = r_to_py(rels), entity_path = entity_path,
     edge_paths = r_to_py(edge_paths_out), checkpoint_path = checkpoint_path, confin)
#
# call the ConfigSchema method
#
    confsch = do.call(conf$ConfigSchema, confargs)
#
# convert TSV for triples into hdf5
#
# python API has mix of string and Path for out and in
# convert_input_data(
#     entity_configs: Dict[str, 
#               torchbiggraph.config.EntitySchema], 
#     relation_configs: List[torchbiggraph.config.RelationSchema], 
#     entity_path: str, 
#     edge_paths_out: List[str], 
#     edge_paths_in: List[pathlib.Path], 
#     edgelist_reader: torchbiggraph.converters.importers.EdgelistReader, 
#     entity_min_count: int = 1, 
#     relation_type_min_count: int = 1, i
#     dynamic_relations: bool = False) -> None
# 
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
# train
#
  ut$setup_logging()
  si = ut$SubprocessInitializer()
  sl = ut$setup_logging
  si$register(sl, confsch$verbose)
#
# there is code here that doesn't get used because
# some subprocess management used in the working
# fb15k example won't work in reticulate context Mar 2024
#
  tmpf = reticulate::import("tempfile")
  ndir = tmpf$TemporaryDirectory(prefix="torchbiggraph_config_")
  uuid = reticulate::import("uuid")
  nname = uuid$uuid4()$hex
  pyatt = import("attr")
  trc = pyatt$evolve(confsch,
        edge_paths=list(confsch$edge_paths[[1]]))

  trainer$train(trc) #, subprocess_init=si)


#
# convert components of configuration for preservation
# in R
can_retain = c("background_io", "batch_size", 
"bias", "checkpoint_path", 
"checkpoint_preservation_interval", "comparator", "dimension", 
"disable_lhs_negs", "disable_rhs_negs", "distributed_init_method", 
"distributed_tree_init_order", "dynamic_relations", "edge_paths", 
"entity_path", "eval_fraction", 
"eval_num_batch_negs", "eval_num_uniform_negs", 
"global_emb", "half_precision", "hogwild_delay", "init_path", 
"init_scale", "loss_fn", "lr", "margin", "max_edges_per_chunk", 
"max_norm", "NAME", "num_batch_negs", "num_edge_chunks", "num_epochs", 
"num_gpus", "num_groups_for_partition_server", "num_groups_per_sharded_partition_server", 
"num_machines", "num_partition_servers", "num_uniform_negs", 
"partition_shard_size", "regularization_coef", "regularizer", 
"relation_lr", "verbose", 
"workers")
#
 response = lapply(can_retain, function(x) confsch[[x]])
 names(response) = can_retain
# ans = list(conf=response, call.args=clkeep)
# class(ans) = c("sce_pbg_emb", class(ans))
#
# ans
 ing = ingest_embeddings(list(conf=response))
 colnames(sce) = sce[[cell_id_var]]
 csce = SummarizedExperiment(ing$cemb)
 g_emb = ing$gemb
 g_ent_ord = ing$G_entities_ordered
 colnames(csce) = ing$C_entities_ordered
 csce = csce[, colnames(sce)] # reorder
 altExp(sce, "pbg_cell_emb") = csce
 S4Vectors::metadata(sce) = list(ini=S4Vectors::metadata(sce), call=clkeep, conf=response,
     g_emb=g_emb, g_ent_ord=g_ent_ord, stats=ing$stats)
 sce 
}

