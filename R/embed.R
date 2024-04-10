#' produce a list with defaults or substitutes for most configuration parameters
#' @param dimension integer(1) defaults to 400
#' @param init_scale numeric(1) defaults to 0.001
#' @param max_norm defaults to NULL
#' @param global_emb logical(1) defaults to FALSE
#' @param comparator character(1) defaults to dot
#' @param bias logical(1) defaults to FALSE
#' @param loss_fn character(1) defaults to softmax
#' @param margin numeric(1) defaults to 0.1
#' @param regularization_coef numeric(1) defaults to 0.001
#' @param regularizer character(1) defaults to N3
#' @param init_path defaults to NULL
#' @param checkpoint_preservation_interval NULL(1) defaults to NULL
#' @param num_epochs integer(1) defaults to 5
#' @param num_edge_chunks defaults to NULL
#' @param max_edges_per_chunk integer(1) defaults to 1000000000
#' @param bucket_order_string character(1) defaults to INSIDE_OUT
#' @param workers defaults to NULL
#' @param batch_size integer(1) defaults to 1000
#' @param num_batch_negs integer(1) defaults to 50
#' @param num_uniform_negs integer(1) defaults to 1000
#' @param disable_lhs_negs logical(1) defaults to FALSE
#' @param disable_rhs_negs logical(1) defaults to FALSE
#' @param lr numeric(1) defaults to 0.1
#' @param relation_lr defaults to NULL
#' @param eval_fraction numeric(1) defaults to 0
#' @param eval_num_batch_negs integer(1) defaults to 1000
#' @param eval_num_uniform_negs integer(1) defaults to 1000
#' @param background_io logical(1) defaults to FALSE
#' @param verbose integer(1) defaults to 0
#' @param hogwild_delay numeric(1) defaults to 2
#' @param dynamic_relations logical(1) defaults to TRUE
#' @param num_machines integer(1) defaults to 1
#' @param num_partition_servers integer(1) defaults to -1
#' @param distributed_init_method defaults to NULL
#' @param distributed_tree_init_order logical(1) defaults to TRUE
#' @param num_gpus integer(1) defaults to 0
#' @param num_groups_for_partition_server integer(1) defaults to 16
#' @param num_groups_per_sharded_partition_server integer(1) defaults to 1
#' @param partition_shard_size integer(1) defaults to 250
#' @param half_precision logical(1) defaults to FALSE
#' @return S3class pbgconf which inherits from "list"
#' @export
config_constants = function(
      dimension=400L, 
      init_scale=0.001, 
      max_norm=NULL, 
      global_emb=FALSE, 
      comparator='dot',
      bias=FALSE, 
      loss_fn='softmax', 
      margin=0.1, 
      regularization_coef=0.001, 
      regularizer='N3',
      init_path=NULL, 
      checkpoint_preservation_interval=NULL, 
      num_epochs=5L,
      num_edge_chunks=NULL, 
      max_edges_per_chunk=1000000000L, 
      bucket_order_string = "INSIDE_OUT",
      workers=NULL,
      batch_size=1000L,
      num_batch_negs=50L, 
      num_uniform_negs=1000L,
      disable_lhs_negs=FALSE, 
      disable_rhs_negs=FALSE, 
      lr=0.1, 
      relation_lr=NULL,
      eval_fraction=0.0, 
      eval_num_batch_negs=1000L, 
      eval_num_uniform_negs=1000L, 
      background_io=FALSE,
      verbose=0L, 
      hogwild_delay=2.0, 
      dynamic_relations=TRUE, 
      num_machines=1L,
      num_partition_servers=-1L, 
      distributed_init_method=NULL, 
      distributed_tree_init_order=TRUE, 
      num_gpus=0L,
      num_groups_for_partition_server=16L, 
      num_groups_per_sharded_partition_server=1L, 
      partition_shard_size=250L, 
      half_precision=FALSE) {
ans = list(
 dimension = dimension,
 init_scale = init_scale,
 max_norm = max_norm,
 global_emb = global_emb,
 comparator = comparator,
 bias = bias,
 loss_fn = loss_fn,
 margin = margin,
 regularization_coef = regularization_coef,
 regularizer = regularizer,
 init_path = init_path,
 checkpoint_preservation_interval = checkpoint_preservation_interval,
 num_epochs = num_epochs,
 num_edge_chunks = num_edge_chunks,
 max_edges_per_chunk = max_edges_per_chunk,
 bucket_order_string = bucket_order_string,
 workers = workers,
 batch_size = batch_size,
 num_batch_negs = num_batch_negs,
 num_uniform_negs = num_uniform_negs,
 disable_lhs_negs = disable_lhs_negs,
 disable_rhs_negs = disable_rhs_negs,
 lr = lr,
 relation_lr = relation_lr,
 eval_fraction = eval_fraction,
 eval_num_batch_negs = eval_num_batch_negs,
 eval_num_uniform_negs = eval_num_uniform_negs,
 background_io = background_io,
 verbose = verbose,
 hogwild_delay = hogwild_delay,
 dynamic_relations = dynamic_relations,
 num_machines = num_machines,
 num_partition_servers = num_partition_servers,
 distributed_init_method = distributed_init_method,
 distributed_tree_init_order = distributed_tree_init_order,
 num_gpus = num_gpus,
 num_groups_for_partition_server = num_groups_for_partition_server,
 num_groups_per_sharded_partition_server = num_groups_per_sharded_partition_server,
 partition_shard_size = partition_shard_size,
 half_precision = half_precision
)
class(ans) = c("pbgconf", "list")
ans
}

#' printer for long configuration object
#' @param x inherits from "pbgconf"
#' @param \dots not used
#' @export
print.pbgconf = function(x, ...) {
cat(sprintf("instance of pbgconf with %d elements.\n", length(x)))
}

# only for use within a basiliskEnv
# excludes entities, relations, entity_path,
# excludes edge_paths, checkpoint_path, which are computed
setup_config_schema_optionals = function( 
 dimension=400L, init_scale=0.001, max_norm=NULL, global_emb=FALSE, comparator='dot',
 bias=FALSE, loss_fn='softmax', margin=0.1, regularization_coef=0.001, regularizer='N3',
 init_path=NULL, checkpoint_preservation_interval=NULL, num_epochs=5L,
 num_edge_chunks=NULL, max_edges_per_chunk=1000000000L, 
 bucket_order=NULL,
 bucket_order_string="INSIDE_OUT", # must be dropped
 workers=NULL, batch_size=1000L, num_batch_negs=50L, num_uniform_negs=1000L,
 disable_lhs_negs=FALSE, disable_rhs_negs=FALSE, lr=0.1, relation_lr=NULL,
 eval_fraction=0.0, eval_num_batch_negs=1000L, eval_num_uniform_negs=1000L, background_io=FALSE,
 verbose=0L, hogwild_delay=2.0, dynamic_relations=TRUE, num_machines=1L,
 num_partition_servers=-1L, distributed_init_method=NULL, distributed_tree_init_order=TRUE, num_gpus=0L,
 num_groups_for_partition_server=16L, num_groups_per_sharded_partition_server=1L, 
 partition_shard_size=250L, half_precision=FALSE) {
 list(
  dimension=dimension,
  init_scale=init_scale,
  global_emb=global_emb,
  comparator=comparator,
  bias=bias,
  loss_fn=loss_fn,
  margin=margin,
  regularization_coef=regularization_coef,
  regularizer=regularizer,
  num_epochs=num_epochs,
  max_edges_per_chunk=max_edges_per_chunk,
  bucket_order = bucket_order,
  bucket_order_string = bucket_order_string,
  batch_size=batch_size,
  num_batch_negs=num_batch_negs,
  num_uniform_negs=num_uniform_negs,
  disable_lhs_negs=disable_lhs_negs,
  disable_rhs_negs=disable_rhs_negs,
  lr=lr,
  eval_fraction=eval_fraction,
  eval_num_batch_negs=eval_num_batch_negs,
  eval_num_uniform_negs=eval_num_uniform_negs,
  background_io=background_io,
  verbose=verbose,
  hogwild_delay=hogwild_delay,
  dynamic_relations=dynamic_relations,
  num_machines=num_machines,
  num_partition_servers=num_partition_servers,
  distributed_tree_init_order=distributed_tree_init_order,
  num_gpus=num_gpus,
  num_groups_for_partition_server=num_groups_for_partition_server,
  num_groups_per_sharded_partition_server=num_groups_per_sharded_partition_server
)
}

#
#class ConfigSchema(torchbiggraph.schema.Schema)
# |  ConfigSchema(*,
# entities: Dict[str,
# torchbiggraph.config.EntitySchema],
# relations: List[torchbiggraph.config.RelationSchema],
# dimension: int,
# init_scale: float = 0.001,
# max_norm: Optional[float] = None,
# global_emb: bool = True,
# comparator: str = 'cos',
# bias: bool = False,
# loss_fn: str = 'ranking',
# margin: float = 0.1,
# regularization_coef: float = 0,
# regularizer: str = 'N3',
# entity_path: str,
# edge_paths: List[str],
# checkpoint_path: str,
# init_path: Optional[str] = None,
# checkpoint_preservation_interval: Optional[int] = None,
# num_epochs: int = 1,
# num_edge_chunks: Optional[int] = None,
# max_edges_per_chunk: int = 1000000000,
# bucket_order: torchbiggraph.config.BucketOrder = <BucketOrder.INSIDE_OUT: 'inside_out'>,
# workers: Optional[int] = None,
# batch_size: int = 1000,
# num_batch_negs: int = 50,
# num_uniform_negs: int = 50,
# disable_lhs_negs: bool = False,
# disable_rhs_negs: bool = False,
# lr: float = 0.01,
# relation_lr: Optional[float] = None,
# eval_fraction: float = 0.05,
# eval_num_batch_negs: Optional[int] = 1000,
# eval_num_uniform_negs: Optional[int] = 1000,
# background_io: bool = False,
# verbose: int = 0,
# hogwild_delay: float = 2,
# dynamic_relations: bool = False,
# num_machines: int = 1,
# num_partition_servers: int = -1,
# distributed_init_method: Optional[str] = None,
# distributed_tree_init_order: bool = True,
# num_gpus: int = 0,
# num_groups_for_partition_server: int = 16,
# num_groups_per_sharded_partition_server: int = 1,
# partition_shard_size: int = 250,
# half_precision: bool = False) -> None
#


#' set configuration schema parameters
#' @param \dots named arguments to override elements of `config_constants`
#' @return instance of 'pbgconf'
#' @export
set_config = function(...) {
 defcon = config_constants()
 inargs = list(...)
 bad = setdiff(names(inargs), names(defcon))
 if (length(bad)>0) message("set_config had some arguments not known to 'config_constants' that will be ignored")
 tomod = intersect(names(inargs), names(defcon))
 ans = lapply(tomod, function(x) defcon[[x]] = inargs[[x]])
 class(ans) = class(defcon)
 ans
}


#' complete processing of SCE
#' @param sce SingleCellExperiment instance ready for conversion with 'sce_to_triples'
#' @param cell_id_var character(1) mandatory selection of
#' column of colData(sce) that is to be used as colnames
#' (which is often missing)
#' @param workdir location of temporary storage, defaults to tempdir()
#' @param N_GENES numeric(1) passed to 'sce_to_triples'
#' @param N_BINS numeric(1) passed to 'sce_to_triples' for discretization
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
#' co = CG_embed_sce(p3k, cell_id_var="Barcode", 
#'    N_GENES=50, dynamic_relations=FALSE)
#' co
#' pp = prcomp(t(assay(altExp(co,"pbg_cell_emb"))))
#' data(p3k.fine)
#' coar = sapply(strsplit(p3k.fine, ":"), "[", 1)
#' pairs(pp$x[,1:4], pch=".", col=factor(coar))
#' library(ggplot2)
#' mdf = data.frame(pp$x[,c(2,4)], type=coar)
#' ggplot(mdf, aes(x=PC2, y=PC4, colour=coar, text=coar)) + 
#'    geom_point() 
#' @export
CG_embed_sce = function(sce, cell_id_var, workdir=tempdir(), N_GENES=1000, N_BINS=5, 
   N_PARTITIONS=1L, FEATURIZED=FALSE, entity_path="ents",
    ...) {
  if (missing(cell_id_var)) stop("cell_id_var must be supplied")
  if (!(cell_id_var %in% names(colData(sce)))) stop(
         "cell_id_var not in colData(sce)")
  set_train() # this is a quick internal pip run because
  # the training module seems to lose track of where
  # torchbiggraph can be loaded from
  N_GENES = as.integer(N_GENES)
  N_BINS = as.integer(N_BINS)
  
  clkeep = match.call()
  tsvtarget = paste0(tempfile(tmpdir=workdir), ".tsv")
  sce_to_triples(sce, outtsv=tsvtarget, ngenes=N_GENES, n_bins=N_BINS)

  proc = basilisk::basiliskStart(bsklenv, fork=FALSE)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function(sce, workdir, N_PARTITIONS, FEATURIZED,
       entity_path, ... ) {
#
# get python modules direct from included source
# as of 25.02.2024 there is no accommodation of GPU which requires
# C++ compilation of some code in fbsource
#
   fbloc = system.file("python", "fbsource", package="BiocBigGraph")
   torc = import("torch")
   torc$manual_seed(0)
   torc$use_deterministic_algorithms(mode=FALSE, warn_only=TRUE)
   rand = import("random")
   rand$seed(0)
   tor = import_from_path("torchbiggraph", path=fbloc)
   conf = import_from_path("torchbiggraph.config", path=fbloc)
   imps = import_from_path("torchbiggraph.converters.importers", path=fbloc)
   ut = import_from_path("torchbiggraph.util", path=fbloc)
   trainer = import_from_path("torchbiggraph.train", path=fbloc)
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
 colnames(csce) = ing$C_entities_ordered
 csce = csce[, colnames(sce)] # reorder
 altExp(sce, "pbg_cell_emb") = csce
 metadata(sce) = c(metadata(sce), call=clkeep)
 sce 

  }, sce=sce, workdir=workdir, N_PARTITIONS=N_PARTITIONS, FEATURIZED=FEATURIZED, 
         entity_path = entity_path, ..., fork=FALSE)
}

