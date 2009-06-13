(********************************************************************************)
(* Demo of a fairly complex 3-step wizard.					*)
(********************************************************************************)

open Lwt
open XHTML.M
open Eliom_parameters
open Litiom_wizard


(********************************************************************************)

exception Empty_string
exception Negative_int of int
exception Odd_int


(********************************************************************************)

let add_entry (x, y) =
	if (x mod 2) = 0
	then Lwt.return ()
	else Lwt.fail Odd_int


(********************************************************************************)

let common =
	let cancelled_content sp =
		Lwt.return
			(html
			(head (title (pcdata "Wizard cancelled")) [])
			(body [p [pcdata "You cancelled!"]]))
	and error_content sp exc_list =
		Lwt.return
			(html
			(head (title (pcdata "Wizard error")) [])
			(body [p [pcdata "There was an error!"]]))
	and failed_content sp exc =
		Lwt.return
			(html
			(head (title (pcdata "Wizard failed")) [])
			(body [p [pcdata "There was an exception!"]]))
	in Steps.make_common
		~path: [""]
		~get_params: Eliom_parameters.unit
		~cancelled_content
		~error_content
		~failed_content
		()


(********************************************************************************)

let step3 =
	let carrier ~carry_in:(x, y) sp () () =
		add_entry (x, y) >>= fun () ->
		Lwt.return (`Proceed ()) in
	let normal_content ~carry_in:(x, y) ~carry_out sp () () =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 3")) [])
			(body	[
				h1 [pcdata "Final:"];
				p [pcdata ("X is " ^ (string_of_int x))];
				p [pcdata ("Y is " ^ y)];
				])) in
	let failed_content sp = function
			| Odd_int ->
				Lwt.return
					(html
					(head (title (pcdata "Wizard step 3 (failure)")) [])
					(body [p [pcdata "The integer is odd!"]]))
			| exc ->
				Lwt.fail exc
	in Steps.make_last
		~common
		~carrier
		~normal_content
		~failed_content
		~post_params: Eliom_parameters.unit
		()


(********************************************************************************)

let step2 =
	let carrier ~carry_in sp () (x, y) =
		if x > 0
		then match y with
			| ""	-> Lwt.fail Empty_string
			| y	-> Lwt.return (`Proceed (x, y))
		else Lwt.fail (Negative_int x) in
	let normal_content ~carry_in ~carry_out ~form sp () (x, y) =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 2")) [])
			(body	[
				h1 [pcdata "Preview:"];
				p [pcdata ("X is " ^ (string_of_int x))];
				p [pcdata ("Y is " ^ y)];
				form;
				]))
	in Steps.make_intermediate
		~common
		~carrier
		~form_maker: Forms.empty
		~normal_content
		~post_params: (Eliom_parameters.int "x" ** Eliom_parameters.string "y")
		~next: step3
		()


(********************************************************************************)

let step1 =
	let form_maker ~carry_in ~carry_out (enter_x, enter_y) =
		Lwt.return
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_x"] [pcdata "Enter int X:"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_x"] ~input_type:`Text ~name:enter_x ();
				label ~a:[a_for "enter_y"] [pcdata "Enter string Y:"];
				Eliom_predefmod.Xhtml.string_input ~a:[a_id "enter_y"] ~input_type:`Text ~name:enter_y ();
				]
			] in
	let normal_content ~carry_in ~carry_out ~form sp () () =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 1")) [])
			(body [form]))
	in Steps.make_first
		~common
		~carrier: Carriers.none
		~form_maker
		~normal_content
		~next: step2
		()

