test_that("test_config_parms are known", {
  p = c("dimension", "init_scale", "max_norm", "global_emb", "comparator", 
   "bias", "loss_fn", "margin", "regularization_coef", "regularizer", 
   "init_path", "checkpoint_preservation_interval", "num_epochs", 
   "num_edge_chunks", "max_edges_per_chunk", "bucket_order_string", 
   "workers", "batch_size", "num_batch_negs", "num_uniform_negs", 
   "disable_lhs_negs", "disable_rhs_negs", "lr", "relation_lr", 
   "eval_fraction", "eval_num_batch_negs", "eval_num_uniform_negs", 
   "background_io", "verbose", "hogwild_delay", "dynamic_relations", 
   "num_machines", "num_partition_servers", "distributed_init_method", 
   "distributed_tree_init_order", "num_gpus", "num_groups_for_partition_server", 
   "num_groups_per_sharded_partition_server", "partition_shard_size", 
   "half_precision")
  nn = config_constants()
  expect_true(all(p %in% names(nn)))
})
   
