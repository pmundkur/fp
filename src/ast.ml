type a' located_nod =
    { nod : a';
      location : Location.t }

fun nod_of loc_nod =
    loc_nod.nod

fun located_nod_of nod loc =
    { nod = nod;
      location = loc }

