(********************************************************************************)
(* Litiom_blocks demo.								*)
(********************************************************************************)

open Lwt
open XHTML.M
open Litiom_blocks


(********************************************************************************)

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


(********************************************************************************)

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


(********************************************************************************)

let foo_service =
	Eliom_services.new_service
	~path: [""]
	~get_params: Eliom_parameters.unit
	()

let foo_handler sp () () =
	run_tree sp tree >>= fun page ->
	Lwt.return
		(html
		(head (title (pcdata "foo")) [])
		(body page))

let () =
	Eliom_predefmod.Xhtml.register foo_service foo_handler

