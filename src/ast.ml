type 'a located_nod =
    { nod : 'a;
      location : Location.t }

let nod_of loc_nod =
  loc_nod.nod

let location_of loc_nod =
  loc_nod.location

let located_nod_of nod loc =
  { nod = nod;
    location = loc }


