ssum = function(arg1, arg2, arg3){
  result = 0
  
  if (arg3 >= 1 & arg1%%1==0 & arg2%%1==0){
    if (arg1 > arg2){
      vec = seq(from = arg1, to = arg2, by = -arg3)
    }
    if (arg1 < arg2){
      vec = seq(from = arg1, to = arg2, by = arg3)
    }
    
    result = sum(vec^2)
  
  } else {
    result = 'wrong inputs'
  }
  
  return(result)
}

ssum(6, 3, 0)
