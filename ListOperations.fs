module ListOperations
let makeUnique lst =
    let rec helper lst criteria =
        match lst with
        |[] -> []
        |x::xs -> if (criteria x) then x::(helper xs (fun n -> criteria n && n<>x))
                  else helper xs criteria
    in
    helper lst (fun n -> true)

let removeConseq lst =
    let rec helper lst prev =
        match lst with
        |[] -> []
        |x::xs -> if (x=prev) then helper xs x
                  else x::helper xs x
    in
    match lst with
    |[] -> []
    |x::xs -> x::(helper xs x)

