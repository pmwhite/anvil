let () =
  let history =
    let open Anvil.O in
    let int = !!"int" in
    let string = !!"string" in
    let point = "Point" in
    let player = "Player" in
    let world = "World" in
    let list t = "list" $$ [ t ] in
    [ [ record point [ "x", int; "y", int ]
      ; record player [ "position", !point; "health", int ]
      ; record world [ "players", list !player; "health", int ]
      ]
    ; [ record point [ "x", int; "y", int; "z", int ] ]
    ; [ record player [ "position", !point; "health", int; "name", string ] ]
    ]
  in
  print_string (Anvil.generate ~history ~type_order:[ "Point"; "Player"; "World" ])
;;
