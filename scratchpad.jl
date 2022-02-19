using LinearAlgebra

a = [1.0000000           sqrt(2.0)           2.0000000
    sqrt(2.0)           3.0000000           sqrt(2.0)
    2.0000000           sqrt(2.0)           1.0000000]
  
display(eigen!(a))