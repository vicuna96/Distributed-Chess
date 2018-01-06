To compile and run the program you must first compile the source code. This is done by running the command 'make' in the folder containing the source files. 

After this, a computer with Unix OS and visible IP address (i.e. not a virtual machine unless you take care of port forwarding, etc.) must set up the server for play. To do this, you run 'make server', which will set up a socket by default in the port '12344' in the host's ip address. Should the user desire to set up a server in a different port, the user can run './server.byte port' after compiling server (i.e. after running 'make') where port is replaced by the integer of a unused port.

To initiate the game, the user must run the command 'make main', which will start a repl prompting the user for input. The repl gives instruction on what to input. NOTE: for correct functioning of the game, the person playing 'white' must run 'make main' and finish connecting to the server (once the game pops up you have connected to the server) before the person playing 'black' connects.

All of the following commands are possible inputs to the repl, where ip and port are replaced by the integer values representing the ip and port number of the server respectively:
- white ip port (must happen before the second player connects to the repl)
- black ip port (must occur after the first player connects to the repl)
- white1 (Can only be ran from the same computer as the server, and assumes the same default port 	'12344' as the server. In addition, it must also be ran before the player playing black 		connects.)
- black1 (Can only be ran from the same computer as the server, and assumes the same default port 	  '12344' as the server. In addition, it must also be ran after the player playing white 		connects.)

If the player feels more comfortable with the terminal, the following command overwrite the commands above by running 'make' and any of these
- ocamlbuild server.byte -lib unix (after comlpeting, run the following) ./server.byte port
- ./gui.byte player ip port (player can only be White | Black, and the same restriction apply on who 	connects to the server first).
