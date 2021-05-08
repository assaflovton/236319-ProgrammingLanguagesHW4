
(*assaf lovton 209844414 assaflovton@campus.technion.ac.il eden dembinsky 212227888 edendem@campus.technion.ac.il*)

datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

local 
    fun slice_list [] =nil|
    slice_list (x::xs) = if(x=")") then nil else x::(slice_list(xs));
    fun no_other_end []=true| 
    no_other_end (x::xs) = if(x = ")") then false else no_other_end (xs);
    fun after_slice [] =nil|
    after_slice (x::xs) = if((x = ")") andalso (no_other_end(xs))) then (xs) 
    else after_slice(xs);
    fun remove_last [] = nil|
    remove_last (x::xs) = if((x = ")") andalso (no_other_end(xs))) then nil
    else x::remove_last(xs);
    fun parse_aux [] = ATOM (NIL)|
    parse_aux (x::xs) = if(x=")") then parse_aux(xs) else
    if(isNumber x) then CONS(ATOM(NUMBER(atoi(x))),(parse_aux(xs)))
    else if x = "(" then CONS(parse_aux((slice_list (xs))),parse_aux((after_slice (xs)))) else
    CONS(ATOM(SYMBOL(x)),(parse_aux(xs)));
in
    fun parse [] = ATOM (NIL)|
    parse (x::xs) = (parse_aux ((remove_last(xs))));
end;
