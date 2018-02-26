let counter = ref 0

let newname () = (counter := !counter + 1;  "a"^string_of_int(!counter))