test_that("Diverse subset works", {
  data(Wheat599_Amat)
  set.seed(500)
  subset <- div.sub(K = wheat599, n.sub = 10, n.gens = 200, n.iters = 5, verbose = T)
  expect_equal(subset$Best_subset, c("15292", "775", "220384", "31089", "107131", "103122", "220399",
                                     "126309", "78690", "126308"))
})
