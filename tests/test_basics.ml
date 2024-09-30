let () =
  let open Anvil.O in
  let point = "Point" in
  let player = "Player" in
  let tree = "Tree" in
  let plant = "Plant" in
  let world = "World" in
  let history =
    let int = !!"int" in
    let string = !!"string" in
    let list t = "list" $$ [ t ] in
    [ [ record point [ "x", int; "y", int ]
      ; record player [ "position", !point; "health", int ]
      ; record world [ "players", list !player; "health", int ]
      ]
    ; [ record point [ "x", int; "y", int; "z", int ] ]
    ; [ record player [ "position", !point; "health", int; "name", string ] ]
    ; [ variant tree [ "Birch", []; "Oak", []; "Elm", [] ]
      ; variant plant [ "Tree", [ !tree ]; "Rock", [] ]
      ]
    ; [ variant tree [ "Birch", []; "Oak", []; "Elm", []; "Maple", [] ] ]
    ]
  in
  print_string (Anvil.generate ~history ~type_order:[ point; player; tree; plant; world ])
;;
