module BinaryHeap
let argMin func argList =
    let rec helper minArg min argList =
        match argList with
        |[] -> minArg
        |x::xs -> if (func x) < min then helper x (func x) xs else helper minArg min xs
    in
    helper (List.head argList) (func (List.head argList)) (List.tail argList)

     
type heap<'k,'v> = {Elements: array<option<'k*'v>>;Heapsize: int ref} //a max heap

let parent i = i/2
let left i = 2*i
let right i = 2*i+1
let makeNull<'k,'v> maxSize = {Elements=[|for i in 0..maxSize -> None|];Heapsize= ref 0}:heap<'k,'v>
let swap aHeap i j = let tmp = aHeap.Elements.[i] in
                     Array.set aHeap.Elements i (aHeap.Elements.[j])
                     Array.set aHeap.Elements j tmp

let siftUp x aHeap = let i = ref x in //done iteratively here. Could have been a recursion
                     while !i>1 &&
                     let (parentKey,_)=aHeap.Elements.[parent !i].Value in
                     let (xKey,_)=aHeap.Elements.[!i].Value in parentKey > xKey do
                        swap aHeap (parent !i) !i
                        i:=parent !i

let rec heapify x aHeap =
    if x <= !aHeap.Heapsize then
        let y = argMin (fun n -> fst aHeap.Elements.[n].Value) (List.filter (fun n -> n <= !aHeap.Heapsize) [x;left x;right x]) in
        if y<>x then swap aHeap x y; heapify y aHeap;

exception HeapFullException
let insert (key,value) aHeap =
    if !aHeap.Heapsize = aHeap.Elements.Length-1 then raise HeapFullException
    else aHeap.Heapsize:= !aHeap.Heapsize + 1
         Array.set aHeap.Elements !aHeap.Heapsize (Some((key,value)))
         siftUp !aHeap.Heapsize aHeap

exception HeapEmptyException
let deleteMin aHeap = //of type heap -> Option
    if !aHeap.Heapsize = 0 then raise HeapEmptyException
    else
    let toReturn = aHeap.Elements.[1] in
        Array.set aHeap.Elements 1 aHeap.Elements.[!aHeap.Heapsize];
        aHeap.Heapsize:=(!aHeap.Heapsize)-1;
        heapify 1 aHeap;
        toReturn

let printTree aHeap =
    let rec printTab n = if n>0 then printf "\t|"; printTab (n-1); in
    let rec printNode i numTabs=
        if i <= !aHeap.Heapsize
        then
            printTab numTabs
            printf "-%A\n" aHeap.Elements.[i].Value
            printNode (2*i) (numTabs+1)
            printNode (2*i+1) (numTabs+1)
    in
    printNode 1 0

