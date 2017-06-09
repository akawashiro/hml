let f = fun x -> x * 2 in f 10;;
let rec f = fun x -> if 10 < x then 1 else  x * f (x + 1) in f 1;;
