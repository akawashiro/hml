let rec f = fun x -> if 10 < x then 1 else x * f (x + 1) in f 1;;
let rec f = fun x -> fun y -> x + y in (f 8) 5;;
let x = 8 in
  let f = fun x -> x in
f x;;
1234567888;;
let rec makeAdder = fun x -> fun w ->
  let rec adder = fun y -> fun s -> x + y * s + w in
  adder in
((((makeAdder 1) 7) 3) 7);;
let rec adder = fun x -> fun y -> x + y in
let rec makeAdder = fun x -> adder x in
(makeAdder 1) 8;;
let x = (let f = fun y -> fun z -> y + z in f 8 20) in x;;
1;;
1 + 2;;
1 + 2 * 4;;
