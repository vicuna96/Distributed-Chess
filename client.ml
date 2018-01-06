(* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *
 * The two functions below, in combination with [Pervasives.input_line],
 * [Pervasives.output_string], [Pervasives.flush], and
 * [Pervasives.shutdown_connection], in the right order, will do the trick
 * for chess.
 *
 * <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< *)

(* [make_addr ip s_port] will create an internet address with the give
 * [ip] and [port].                                                 *)
let make_addr ip s_port = let open Unix in
  let ip' = ip|>inet_addr_of_string in
  let port' = s_port|>int_of_string in
  ADDR_INET(ip',port')

(* [open_con ip s_port] is are the in_channel and out_channels (ie. [ic,oc])
 * to send information at the location described by [ip] and [s_port]
 * requires: socket at [ip,s_port] is listening and of type [Unix.SOCK_STREAM]*)
let open_con ip s_port =
  try
    let server = make_addr ip s_port in
    Unix.open_connection server  (* Outputs ic,oc *)
  with
  | _ -> Printf.eprintf "%s : Bad server\n" ip ; exit 2

(* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *
 * The functions below are probably useless, but we'll leave them here for now
 * in case they become useful later on.
 *
 * <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< *)

let s_socket ip s_port= let open Unix in
  let addr = make_addr ip s_port in
  let sock = socket (addr|>domain_of_sockaddr) SOCK_STREAM 0 in
  bind sock addr; addr

let make_addr' () = let open Unix in            (*  for testing on VM *)
  let ip = inet_addr_loopback in
  let port = Sys.argv.(2)|>int_of_string in
  ADDR_INET(ip,port)

let make_socket l_port = let open Unix in
  let port' = l_port|>int_of_string in
  let ad = ADDR_INET (inet_addr_loopback,port') in
  socket (ad|>domain_of_sockaddr) SOCK_STREAM 0

let connected ip s_port l_port =
  let server = make_addr ip s_port in
  let socket = make_socket l_port in
  Unix.connect socket server

let connected' () =
  let server = make_addr'() in
  let socket = make_socket Sys.argv.(1) in
  Unix.connect socket server;;
