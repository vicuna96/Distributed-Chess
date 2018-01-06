let my_ip () = let open Unix in
  (()|>gethostname|>gethostbyname).h_addr_list.(0)

let my_ip_string () =
  () |> my_ip |> Unix.string_of_inet_addr


    (* Credit goes to Prof. Clarkson for the idea of this implementation *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Chess Engine.\n");
  print_endline "Please enter the color, IP address and port number
                        of the server to join.\n";
  print_endline "For example, to connect as 'black', to the ip address
                127.0.0.1 and port 12345 you write
                'black 127.0.0.1 12345'.\n";
  print_endline "Or if you would like to play againt the computer, then
                write either 'white ai' or 'black ai'";
  print_endline "Note, by default you can input 'white1' or 'black1' to
                  play as white or black respectively if you have ran
                  'make server' on a different terminal on this computer.";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | str ->
    let lst = str|>String.lowercase_ascii|>String.split_on_char ' ' in
    let ip = my_ip_string () in
    match lst|>List.hd with
    | "black" when List.nth lst 1 = "ai" ->
      Unix.execv "gui.byte" [|"";"Black";"";"";"ai"|]
    | "black" ->
    (* Open the GUI window and launch the game loop. *)
      Unix.execv "gui.byte" [|"";"Black";List.nth lst 1;List.nth lst 2;""|]
    | "white" when List.nth lst 1 = "ai" ->
      Unix.execv "gui.byte" [|"";"White";"";"";"ai"|]
    | "white" ->
      Unix.execv "gui.byte" [|"";"White";List.nth lst 1;List.nth lst 2;""|]
    | "white1" ->
      Unix.execv "gui.byte" [|"";"White";ip;"12344";""|]
    | "black1"  ->
      Unix.execv "gui.byte" [|"";"Black";ip;"12344";""|]
    | _ -> print_endline "Could not identify user, please try again "; ()

let () = main ()
