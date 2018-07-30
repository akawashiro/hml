let rec make_adder x =
  let rec adder y = x + y in
  adder in
(make_adder 3) 7
