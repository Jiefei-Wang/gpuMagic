test_that("type conversion",{
  for(i in 1:5){
    expect_equal(getTypeNum(getTypeStr(i)),i)
  }
})
