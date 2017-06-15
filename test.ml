let rec makeAdder = fun x -> fun w ->
  let  rec adder = fun y -> fun s -> x + y + s + w in
  adder in
((((makeAdder 3) 7) 3) 7);;
let rec adder = fun x -> fun y -> x + y in
let rec makeAdder = fun x -> adder x in
(makeAdder 3) 8;;
let x = (let f = fun y -> fun z -> y + z in f 10 20) in x;;
1 + 2 * 3 + 4;;
let f = fun x -> x * 2 in f 10;;
let rec f = fun x -> if 10 < x then 1 else x * f (x + 1) in f 1;;
let rec f = fun x -> fun y -> x + y in (f 10) 5;;
let x = 10 in
  let f = fun x -> x in
f x;;
