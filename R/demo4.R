demo4base = list(conf = list(background_io = FALSE, batch_size = 1000L, bias = FALSE, 
    checkpoint_path = "demo4/chkpt", checkpoint_preservation_interval = NULL, 
    comparator = "dot", dimension = 400L, disable_lhs_negs = FALSE, 
    disable_rhs_negs = FALSE, distributed_init_method = NULL, 
    distributed_tree_init_order = TRUE, dynamic_relations = FALSE, 
    edge_paths = c("demo4/train_out", "demo4/validate_out", "demo4/test_out"
    ), entity_path = "demo4/ents", eval_fraction = 0, eval_num_batch_negs = 1000L, 
    eval_num_uniform_negs = 1000L, global_emb = FALSE, half_precision = FALSE, 
    hogwild_delay = 2, init_path = NULL, init_scale = 0.001, 
    loss_fn = "softmax", lr = 0.1, margin = 0.1, max_edges_per_chunk = 1000000000L, 
    max_norm = NULL, NAME = "config", num_batch_negs = 50L, num_edge_chunks = NULL, 
    num_epochs = 30L, num_gpus = 0L, num_groups_for_partition_server = 16L, 
    num_groups_per_sharded_partition_server = 1L, num_machines = 1L, 
    num_partition_servers = -1L, num_uniform_negs = 1000L, partition_shard_size = 250L, 
    regularization_coef = 0.001, regularizer = "N3", relation_lr = NULL, 
    verbose = 0L, workers = NULL), call.args = "CG_embed_sce(sce = p3k, 
    workdir = 'demo4', N_GENES = 1500L, num_epochs = 30L, dynamic_relations = FALSE)")

#' illustrate working with trained embedding
#' @param path location where compressed embedding output is found
#' @examples
#' gg = grab_bgdemo()
#' td = tempdir()
#' unzip(gg, exdir=td)
#' mm = make_demo_emb(td)
#' mm
#' rhdf5::h5ls(dir(mm$conf$edge_paths[1], full=TRUE))
#' @export
make_demo_emb = function(path) {
    obj = demo4base
    obj$workdir = path
    obj$conf$edge_paths = file.path(path, obj$conf$edge_paths)
    obj$conf$checkpoint_path = file.path( path, obj$conf$checkpoint_path)
    obj$conf$entity_path = file.path( path, obj$conf$entity_path)
    class(obj) = c("CG_emb", "list")
    obj
}

#' printer for configuration
#' @param x CG_emb instance
#' @param \dots not used
#' @export
print.CG_emb = function(x, ...) {
  cat(sprintf("BiocBigGraph embedding config for workdir %s\n", 
       x$workdir))
}

