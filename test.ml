let f = 
  fun x -> x * 2 in 
f 10;;

let rec f = 
  fun x -> 
    if 10 < x 
    then 1 
    else 10 + x * f (x + 1) in 
f 1;;

let x = 
  let rec f = 
    fun x -> 
      if 10 < x 
      then 1 
      else 10 + x * f (x + 1) in 
  f 1 in
x + 223;;

hfadshn kjlas;;
