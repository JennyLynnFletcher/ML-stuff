datatype (''key * 'value) dict_tree = Lf | Br of (''key * 'value) * (''key * 'value) dict_tree

exception Key_Already_Exists;
exception Key_Doesnt_Exist;

val empty = Lf;

fun add (key, value) empty = Br (key, value) Lf;
