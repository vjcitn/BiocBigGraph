
test_that("sce_to_triples works", {
 tf = tempfile()
 p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
 assay(p3k) = as.matrix(assay(p3k)) # dense for now
 p3k = scuttle::logNormCounts(p3k)
 sce_to_triples(p3k, tf, ngenes=100)
 x = readLines(tf)
#> length(x)
#[1] 270000
#> x[1]
#[1] "AAACATACAACCAC-1\tr0\tENSG00000090382"
 expect_true(length(x)==270000)
 l1 = x[1]
 expect_true(length(grep("090382", l1))==1)
})
