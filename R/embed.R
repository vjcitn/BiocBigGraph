#' produce a list with defaults or substitutes for most configuration parameters
#' @importFrom reticulate import import_from_path r_to_py
#' @import SummarizedExperiment
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
