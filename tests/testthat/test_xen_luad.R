
test_that("cache_xen_luad works", {
    pa = cache_xen_luad()
    luad = restoreZipXenSPEP(pa)
  expect_true(methods::is(luad, "XenSPEP"))
  expect_true(all(dim(luad)==c(377L, 162254L)))
  expect_true(all(dim(getTranscripts(luad))==c(12165021L, 11L)))
})
