datatype (''key, 'value) dictionary = dict of (''key * 'value) list;

exception Key_Already_Exists;
exception Key_Doest_Exist

val empty = dict([]);

val test = dict([(1,"one"), (2,"two"),(3,"three")]);

fun key_exists _ (dict []) = false
    | key_exists key (dict ((xa,_)::xs)) =
        if key = xa then true
        else key_exists key (dict xs);

fun add (key, value) (dict xs) = 
    if key_exists key (dict xs) then raise Key_Already_Exists
    else dict((key,value)::xs);
    
fun remove key (dict []) = dict []
    | remove key (dict ((xa,xb)::xs)) = 
        let fun find _ [] r = dict r
            | find k ((xa,xb)::xs) r =
            if k <> xa then find key xs ((xa,xb)::r)
            else dict (xs@r)
        in find key ((xa,xb)::xs) [] end;
(*This changes order, so may not be the neatest solution, even though a dictionary doesn't have to be ordered*)

fun get _ (dict []) = raise Key_Doest_Exist
    | get key (dict((xa,xb)::xs)) = 
        if key = xa then xb
        else get key (dict xs);

fun set _ _ (dict []) = raise Key_Doest_Exist
    | set key value (dict((xa,xb)::xs)) = 
        if key = xa then dict((xa,value)::xs)
        else set key value (dict xs);
