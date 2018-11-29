context("openCyto Plugins")

## -------------------------------------------------------------------------
# gate_draw -

test_that("gate_draw", {
  
  expect_equal(unname(gate_draw(fs[[1]], channels = c("FSC-A","SSC-A"), alias = "Cells", type = "r")), filters(list(rg)))
  
})

## -------------------------------------------------------------------------
# ppmanualGate -

test_that("ppmanualGate", {
  
  expect_message(ppmanualGate(fs = fs, gs = gs, groupBy = 1), "Numeric groupBy is not supported, use pData variables instead. All samples will be grouped together.")
  expect_equal(ppmanualGate(fs = fs, gs = gs, groupBy = NA), 1)
  
})