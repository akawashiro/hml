let rec f = fun x -> fun y -> x + y
in let g = (f 1) in g 10;;
