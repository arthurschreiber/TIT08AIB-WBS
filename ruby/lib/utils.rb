def more_general?(hyp1, hyp2)
  hyp1.zip(hyp2).all? { |a, b| a == b || a == :* }
end