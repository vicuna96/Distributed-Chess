(* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   Note, this can be tested on out on linux by running the following
   commands on the terminal:
   1) To compile run 'ocamlbuild server.byte -lib unix'
   2) To run the byte file './server.byte <port>
      where <port> is replaced by desired port number
      in which you want to run the server.
      recommended: ./server.byte 12345
   3) To connect to the server using linux we can, for now,
      utilize the telnet client by running the following command:
      'telnet <ip> <port>'. <ip> is replaced by the ip in which
      the server is running, and <port> by the port from 2 above.
      You can also replace <ip> by the computer name (ex. vm3110-2017fa).

   The server works on a turn by turn setting. Two people must be connected
   at any time for it to function. The server first reads input
   from the first person that connected and then from the second person.
   The second person should not send a request until they have received
   a "response" from the server even at the beginning of the
   exchange of information.

   *** Implementation influenced by the textbook "Developing Applications
   *** with Objective Caml", by CHAILLOUX, MANOURY, PAGANO


   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< *)
(* [my_ip ()] returns the Unix.inet_addr of the computer *)
let my_ip () = let open Unix in
  (()|>gethostname|>gethostbyname).h_addr_list.(0)


let my_ip_string () =
  () |> my_ip |> Unix.string_of_inet_addr


(* [create_socket ()] is the file_descr of a newly created socket
 * in the current computer at the [port] given in the first argument
 * of the terminal. This socket is binded and take at most 3
 * requests at a time.
 * requires: port is a valid port                                 *)
let create_socket () = let open Unix in
  let port = Sys.argv.(1)|>int_of_string in
  let ad = ADDR_INET (my_ip (),port) in
  let desc = socket (ad|>domain_of_sockaddr) SOCK_STREAM 0 in
  bind desc ad; listen desc 3; desc

(* [get_port y] is the ip address of the internet address [y]
 * requires: [y] is of type Unix.ADDR_INET.                *)
let get_ip = function
  | Unix.ADDR_INET (x,_) -> x|>Unix.string_of_inet_addr
  | _ -> failwith "not addr_inet"

(* [connected] is a ref that will take all the file_descr of the
 * clinets connected to the server in [server_last] below.      *)
let connected = ref []

(* [server func] sets up a sreaming server that will take 2 clients and accept
 * at most 3 requests at a time and will use [func] to process their requests.*)
let server func = let open Unix in
  let desc = create_socket () in      (* file descriptor of the socket *)
  print_string "IP: ";
  ()|>my_ip_string|>print_endline;
  "Port: "^Sys.argv.(1)|>print_endline;
  while true do                       (* Server is "listening" for new clients *)
    let (desc',ad') = accept desc in  (* Get descriptor of new client *)
    connected:=desc'::(!connected);
    print_string "Client IP: ";
    print_endline (ad'|>get_ip);
    if List.length (!connected) = 2 then
    match fork () with
    | 0 ->
      begin
        match fork () with
        | 0 ->
          let in_channels = List.map in_channel_of_descr (!connected) in
          let out_channels = List.map out_channel_of_descr (!connected) in
          let nth = List.nth in_channels in
          func out_channels (nth 1) (nth 0) desc;
          ignore(List.map close_in in_channels);
          ignore(List.map close_out out_channels); exit 0
        | _ -> exit 0
      end
    | x -> close desc'; ignore(waitpid [] x);
            shutdown desc SHUTDOWN_RECEIVE; exit 0
  done

(* [propagate_turn out ic1 ic2] reads input from ic1, and flushes it on
   the first element of [out], then reads input from [ic2] and flushes
   on the second element of [out]. *)
let propagate_turn out_channels ic1 ic2 desc=
  let out1 = List.nth out_channels 1 in
  let out2 = List.nth out_channels 0 in
  try
    while true do
      let func x y = output_string y (x^"\n"); flush y in
      let move1 = input_line ic1 in
      func move1 out2;
      let move2 = input_line ic2 in
      func move2 out1
    done
  with
  | _ -> Printf.printf "Quitted\n";flush stdout; exit 0;;


(* [propagate out ic1 ic2] reads input from ic1, and flushes said input_line
   on all out_channels in [out], then reads input from [ic2] and flushes
   said input on all out_channels in [out]. *)
let propagate_all out_channels ic1 ic2=
  try
    while true do
      let func x y= output_string y (x^"\n"); flush y in
      let move1 = input_line ic1 in
      ignore(List.map (func move1) out_channels);
      let move2 = input_line ic2 in
      ignore(List.map (func move2) out_channels);
    done
  with
  | _ -> Printf.printf "Quitted\n";flush stdout; exit 0;;

(* [communicate_server ()] executes [server] with [propagate_turn]
 * while handling Unix errors.                                    *)
let communicate_server () =
  Unix.handle_unix_error server propagate_turn;;

communicate_server ()
