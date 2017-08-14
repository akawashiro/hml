let rec f = fun x -> if 10 < x then x else x + f (x+1) in f 1;;
let rec f = fun x -> if x < 10 then 1 else 2 in f 5;;

if 10 < 11 then 1 else 2;;

let x = 8 in
let rec f = fun x -> x + 2 in
f x;;



