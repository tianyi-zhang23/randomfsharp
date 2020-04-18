module Prime
let allPrimes until=
    let rec helper i notPrime =
        let nextNotPrime n = (n%i=0) || (notPrime n) 
        in
        if i>until then []
        else if notPrime i then helper (i+1) nextNotPrime
        else i::(helper (i+1) nextNotPrime)
    in
    helper 2 (fun i -> false)

