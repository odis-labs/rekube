module M = {
  let make = (~name, ~version, ()) =>
    ignore((name, version))
};

let m = M {
  "name": "m1",
  "version": "something"
};

let m2 = M({
  "name": "m1",
  "version": "something"
});


