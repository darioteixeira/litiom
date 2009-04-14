(********************************************************************************)
(**	Litiom_blocks module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt


(********************************************************************************)
(* Type definitions.								*)
(********************************************************************************)

(**	Standard input type for source elements.
*)
type in_t = unit


(**	Standard output type for sink elements.
*)
type out_t = [`Div] XHTML.M.elt list


(**	Shorthand for the type of server parameters.
*)
type sp_t = Eliom_sessions.server_params


(**	The basic block type.  It is parameterised over
	the type of the input ['i] and the output ['o].
*)
type ('i, 'o) t = sp_t -> 'i -> 'o Lwt.t


(********************************************************************************)
(* Constructor functions.							*)
(********************************************************************************)

(**	Generic function to create new nodes.
*)
let node f = f


(**	Function to create source nodes.  This is essentially the same
	as the generic creator function {!node}, except that the input
	type is constrained to be {!in_t}.
*)
let source = node


(**	Function to create sink nodes (ie, terminal nodes).  This is essentially
	the same as the generic creator function {!node}, except that the output
	type is constrained to be {!out_t}.
*)
let sink = node


(********************************************************************************)
(* Predefined nodes.								*)
(********************************************************************************)

(**	A {i no-operation} generic predefined node.  It
	basically takes its input and returns it unchanged.
*)
let nop params x = node (Lwt.return x)


(**	Also a {i no-operation} predefined node.  Its type is
	however constrained to be used solely as a source node.
*)
let root params x = source (Lwt.return x)


(********************************************************************************)
(* Plumbing functions.								*)
(********************************************************************************)

(**	This function connects the output of a node into the input of another.
	It can be seen as replacing the two nodes with a single	node that has
	as input the input of the first node, and as output the output of the
	second node.

	{v
			    | 'i
			    |
			=========
			|   a	|			    | 'i
			=========			    |
			    |				=========
			    | 'j	Pipe ==>	|   P	|
			    |				=========
			=========			    |
			|   b   |			    | 'o
			=========
			    |
			    | 'o
	v}
*)
let pipe f g sp x =
	f sp x >>= fun f_res -> g sp f_res


(**	Operator equivalent to the {!pipe} function.
*)
let (>>>) = pipe


(**	This function splits the tree, feeding the output of a node into a list
	of child nodes.  Note that to insure a valid XHTML tree, all child nodes
	must be sinks (or a composition of equivalent type, of course).  Also
	note that in the diagram below, the list of child nodes is represented
	by two nodes, "b" and "c".

	{v
			    | 'i						
			    |							
			=========						
			|   a	|						
			=========						
			    |							
			    | 'j			    | 'i		
			    |				    |			
			    |				=========		
			   / \		Splitter ==>	|   S	|		
			  /   \				=========		
			 /     \			    |			
		 	/       \			    | out_t		
		       /	 \						
		      /		  \						
		     /		   \						
		    /		    \						
		    |		    |						
		    | 'j	    | 'j					
		    |		    |						
		=========	=========					
		|   b	|	|   c	|					
		=========	=========					
		    |		    |						
		    | out_t	    | out_t					
	v}
*)
let splitter f children sp x =
	let bind_all l =
		let rec aux accu = function
			| [] -> Lwt.return (List.rev accu)
			| hd::tl -> hd >>= (fun res -> aux (res::accu) tl)
		in aux [] l in
	f sp x >>= fun f_res ->
	let threads = List.map (fun g -> g sp f_res) children in
	bind_all threads >>= fun bound ->
	Lwt.return (List.flatten bound)


(**	This function is identical to a splitter, with the difference that
	it places the result of all the child nodes into a DIV container.
*)
let container ?(a=[]) f children sp x =
	splitter f children sp x >>= fun res ->
	Lwt.return [XHTML.M.div ~a res]


(**	This function performs a "spawn & join".  It works very much like
	a splitter in feeding the output of a node into other nodes; it is
	different however, in the sense that instead of a list, only two
	child nodes are accepted; also, it takes the output of the two
	spawned nodes and feeds into a fourth node.

	{v
			    | 'i						
			    |							
			=========						
			|   a	|						
			=========						
			    |							
			    | 'j			    | 'i		
			    |				    |			
			    |				=========		
			   / \		Spawner ==>	|   S	|		
			  /   \				=========		
			 /     \			    |			
		 	/       \			    | 'o		
		       /	 \						
		      /		  \						
		     /		   \						
		    /		    \						
		    |		    |						
		    | 'j	    | 'j					
		    |		    |						
		=========	=========					
		|   b	|	|   c	|					
		=========	=========					
		    |		    |						
		    | 'k	    | 'l					
		    |		    |						
		    \		    /						
		     \		   /						
		      \		  /						
		       \	 /						
			\	/						
			 \     /						
			  \   /							
			   \ /							
			    |							
			    | 'k * 'l 						
			    |							
			=========						
			|   d	|						
			=========						
			    |							
			    | 'o						
	v}
*)
let spawner f (g1, g2) h sp x =
	f sp x >>= fun f_res ->
	let thread1 = g1 sp f_res
	and thread2 = g2 sp f_res in
	thread1 >>= fun g1_res ->
	thread2 >>= fun g2_res ->
	h sp (g1_res, g2_res)


(**	Suppose we have a node "a" that outputs values of type ['j option].
	Depending on whether the output of "a" is set or not (in other words,
	whether its output is [Some thing] or [None]), this function will either
	invoke node "b" with [thing] or node "c" with a unit value.  Therefore,
	it can be used to enforce that a certain path of the tree is only run
	if a certain optional value is set.

	{v
			    | 'i						
			    |							
			=========						
			|   a	|						
			=========						
			    |							
			    | 'j option			    | 'i		
			    |				    |			
			    |				=========		
			   / \		Splitter ==>	|   S	|		
			  /   \				=========		
			 /     \			    |			
		 	/       \			    | out_t		
		       /	 \						
		      /		  \						
		     /		   \						
		    /		    \						
		    |		    |						
		    | 'j	    | unit
		    |		    |						
		=========	=========					
		|   b	|	|   c	|					
		=========	=========					
		    |		    |						
		    | out_t	    | out_t					
	v}
*)
let enforcer f (g, h) sp x =
	f sp x >>= fun res ->
	match res with
		| Some thing	-> g sp thing
		| None 		-> h sp ()


(********************************************************************************)
(* Execution functions.								*)
(********************************************************************************)

(**	This function will "run" the frozen-in-closure tree of functions, thus
	producing the XHTML page.  Note that besides the tree of functions, we
	must also provide the standard server parameters defined by Eliom.
*)
let run_tree sp f = f sp ()

