(* [open_con ip s_port] is are the in_channel and out_channels (ie. [ic,oc])
 * to send information at the location described by [ip] and [s_port]
 * requires: socket at [ip,s_port] is listening and of type [Unix.SOCK_STREAM]*)
val open_con : string -> string -> in_channel*out_channel
