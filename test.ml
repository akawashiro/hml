1 + 2 * 3 + 4;;
let f = fun x -> x * 2 in f 10;;
let rec f = fun x -> if 10 < x then 1 else x * f (x + 1) in f 1;;
let rec f = fun x -> fun y -> x + y in (f 10) 5;;
let x = 10 in
  let f = fun x -> x in
f x;;
