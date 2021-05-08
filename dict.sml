
(*assaf lovton 209844414 assaflovton@campus.technion.ac.il eden dembinsky 212227888 edendem@campus.technion.ac.il*)

datatype ('a, 'b) dictionary = 
    Nil
  | Dict of {key: 'a, value: 'b} list;
exception ItemNotPresent;


local
    fun return_list Nil = nil | return_list (Dict (l)) = l;
    fun return_key {key = k, value = v} = k;
    fun return_value {key = k, value = v} = v;
in
    fun insert Nil k v = Dict [{key = k, value = v}]
    | insert (Dict ([])) k v = Dict [{key = k, value = v}]
    | insert (Dict(x :: xs)) k v =
        if k = return_key(x) then Dict({key = k, value = v} :: xs)
        else Dict(x :: return_list(insert (Dict xs) k v));
        
    fun find Nil k = raise ItemNotPresent | 
    find (Dict ([])) k = raise ItemNotPresent |
    find (Dict(x :: xs)) k =
        if k = return_key(x) then return_value(x) else find (Dict(xs)) k;

    fun remove (Nil) k = raise ItemNotPresent | 
    remove (Dict []) k = raise ItemNotPresent |
    remove (Dict [{key = ke,value = vl}]) k = if (ke = k) then Nil
        else (Dict [{key = ke,value = vl}])|
    remove (Dict(x :: xs)) k =
        if k = return_key(x) then Dict(xs)
        else Dict(x :: return_list(remove (Dict xs) k));

    fun keys Nil = nil | 
    keys (Dict ([])) = nil |
    keys (Dict (x::nil)) = (return_key(x))::nil |
    keys (Dict (x::xs)) = (return_key(x))::(keys (Dict(xs)));
    
    fun values Nil = nil | 
    values (Dict ([])) = nil |
    values (Dict (x::nil)) = (return_value(x))::nil |
    values (Dict (x::xs)) = (return_value(x))::(values(Dict(xs)));

end;