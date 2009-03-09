(********************************************************************************)
(**	Litiom_blocks module.

	Uses the arrow formalism to build a simple algebra of Lwt functions
	plumbed together.  It allows for simple composition of functions,
	forking & joining, and placing the result of a list of computations
	inside containers.  The end is to enable the construction of page
	fragments via the composition of simple building blocks.

	Copyright (c) 2008 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Introduction}							*)
(********************************************************************************)

(**	There are two key concepts to understand in [Litiom_blocks]: nodes, and
	the plumbing primitives that link nodes together.  We shall address each
	of these two concepts in turn, and end with a practical example.

	You can think of nodes as the basic building blocks that constitute a page.
	The [Litiom_blocks] module makes extensive use of the [Lwt] library and expects
	that all nodes return a value of type ['a Lwt.t].  This requirement exists
	so that the module is able to issue concurrently the threads corresponding to
	blocks in independent branches of the tree.  Moreover, nodes can be divided
	into three broad categories:

	{ul
		{li	Source nodes, taking as input the server parameters and a value of type
			[unit], and returning a value of type ['o Lwt.t].  There's typically only
			one source node per tree, corresponding to the tree's root.}
		{li	Processing nodes, taking as input the server parameters and a value of
			type ['i] and returning a value of type ['o Lwt.t].}
		{li	Sink nodes, taking as input the server parameters and a value of type ['i]
			and returning a value of type [[`Div] XHTML.M.elt list].  These are the
			nodes that terminate the tree, sitting at its leaves.}}

	Depending on whether the block is a generic processing node, a source, or a sink, use
	respectively the {!node}, {!source}, or {!sink} constructor functions.  Each of these
	constructors takes only one parameter: the function that defines the block.  This function
	should take two parameters as input: one is the familiar [Eliom_sessions.server_params]
	common in [Eliom], the other is a value of either type [unit] or a generic ['i] depending
	on the type of node.

	To define the structure of the tree, you must connect ("plumb") the various nodes
	together.  Note that for this to happen, their return and input types must match
	(Ocaml's type checker will make sure of that!).  [Litiom_blocks] defines a number
	of plumbing primitives that should cover the needs of most web sites:

	{ul
		{li	Pipe: connects the output of a node into the input of another node.
			The [>>>] operator is also defined, allowing one to pipe nodes
			together in a convenient way (ex: [node1 >>> node2 >>> node3]).}
		{li	Splitter: connects the output of a node into the input of a list
			of sink nodes.}
		{li	Container: like the splitter, except that the list of sink nodes
			is placed into a [DIV] container.}
		{li	Spawner: feeds the output of a node into two separate child nodes,
			then waiting for these to complete so that their output can be
			combined into a pair and fed into yet another node.}
		{li	Enforcer: while also splitting a tree in two, only one of those
			paths is actually taken, depending on whether the output of a
			node is [Some value] or [None].}}

	Let us now consider a practical example.  Suppose that all pages on our site are
	arranged into four distinct parts, two of which are always the same regardless of
	the logged in user, and other two which are personalised according to the user.
	The former parts are the header and footer areas, while the latter are the menu
	and a contents area.  Moreover, the contents area is different for the various
	pages that compose the website.  The page's CSS arranges these areas as the figure
	shows:

	{v
	-----------------------------------------
	|		Header			|
	-----------------------------------------
	|	|				|
	|	|				|
	| Menu	|	Contents		|
	|	|				|
	|	|				|
	-----------------------------------------
	|		Footer			|
	-----------------------------------------
	v}

	In XHTML terms, the structure of the page is expressed as follows:

	{v
	<body>
		<div id="header"> ... </div>

		<div id="user_area">
			<div id="menu"> ... </div>

			<div id="contents"> ... </div>
		</div>

		<div id="footer"> ... </div>
	</body>
	v}

	Let us start by declaring the service:

	{v
	let foo_service =
		Eliom_services.new_service
			~path: ["foo"]
			~get_params: Eliom_parameters.unit
			()
	v}

	And the various blocks that constitute a page:

	{v
	let get_uid sp () =
		Lwt.return (Random.int 10)

	let menu sp uid =
		Lwt.return [div [h1 [pcdata (Printf.sprintf "Menu for user %d!" uid)]]]

	let content sp uid =
		Lwt.return [div [h1 [pcdata (Printf.sprintf "Content for user %d!" uid)]]]

	let header sp () =
		Lwt.return [div [h1 [pcdata "Generic header"]]]

	let footer sp () =
		Lwt.return [div [h1 [pcdata "Generic footer"]]]
	v}

	Now we can define the tree structure.  Note that because the {!splitter} plumbing
	primitive requires an origin node, and we don't actually have one, we use the
	predefined {!root} node (a {i no-op}) as a stand-in:

	{v
	let tree =
		splitter
			root
			[
			sink header;
			container ~a:[a_id "user_area"]
				(source get_uid)
				[
				(sink menu);
				(sink content)
				];
			sink footer
			]
	v}

	We can now invoke the {!run_tree} function to actually execute the above
	defined tree in the service handler:

	{v
	let foo_handler sp () () =
		run_tree sp tree >>= fun page ->
		Lwt.return
			(html
				(head (title (pcdata "foo")) [])
				(body page))	
	v}

	Finally, we register the service:

	{v
	let () =
	        Eliom_predefmod.Xhtml.register foo_service foo_handler
	v}
*)


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type in_t = unit

type out_t = [`Div] XHTML.M.elt list

type sp_t = Eliom_sessions.server_params

type ('i, 'o) t


(********************************************************************************)
(**	{2 Constructor functions}						*)
(********************************************************************************)

val node:	(sp_t -> 'i -> 'o Lwt.t) ->
		('i, 'o) t

val source:	(sp_t -> in_t -> 'o Lwt.t) ->
		(in_t, 'o) t

val sink:	(sp_t -> 'i -> out_t Lwt.t) ->
		('i, out_t) t


(********************************************************************************)
(**	{2 Predefined nodes}							*)
(********************************************************************************)

val nop:	('i, 'i) t

val root:	(in_t, in_t) t


(********************************************************************************)
(**	{2 Plumbing functions}							*)
(********************************************************************************)

val pipe:	('i, 'j) t ->
		('j, 'o) t ->
		('i, 'o) t

val (>>>):	('i, 'j) t ->
		('j, 'o) t ->
		('i, 'o) t

val splitter:	('i, 'j) t ->
		('j, out_t) t list ->
		('i, out_t) t

val container:	?a:[< XHTML.M.common] XHTML.M.attrib list ->
		('i, 'j) t ->
		('j, out_t) t list ->
		('i, out_t) t

val spawner:	('i, 'j) t ->
		('j, 'k) t * ('j, 'l) t ->
		(('k * 'l), 'o) t ->
		('i, 'o) t

val enforcer:	('i, 'j option) t ->
		('j, out_t) t * (unit, out_t) t ->
		('i, out_t) t


(********************************************************************************)
(**	{2 Execution functions}							*)
(********************************************************************************)

val run_tree:	sp_t ->
		(in_t, out_t) t ->
		out_t Lwt.t

