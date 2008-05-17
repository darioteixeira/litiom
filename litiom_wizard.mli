(********************************************************************************)
(**	Litiom_wizard module.

	Copyright (c) 2008 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{2 Introduction}							*)
(********************************************************************************)

(**	This module aims to simplify the construction of wizard-like interactions
	in websites.  By {i wizard-like}, we mean that this interaction is akin to
	that commonly found on GUI applications, where the user is presented with
	an ordered sequence of dialog steps.

	Wizard-like interactions can of course be built directly with [Eliom].
	More specifically, the developer can make use of attached coservices
	registered in session tables, taking advantage of the continuation-based
	programming paradigm offered by [Eliom].  Since this is a fairly common
	and repetitive pattern, it makes sense to "ditch that boilerplate" and
	to create routines to automate the task.  Herein lies the rationale for
	the {!Litiom_wizard} module.

	So what does {!Litiom_wizard} offer?  Essentially, the developer only
	needs to specify the non-repetitive contents of each dialog's form.
	{!Litiom_wizard} will automatically take care of adding and managing
	the "Cancel" and "Proceed" buttons that animate the wizard, handling
	the "discardable forms" problem that arises when the user presses
	"Cancel" without filling a form, and of course, registering and invoking
	each successive step in the wizard.

	There are two sets of facilities offered by {!Litiom_wizard}, encapsulated
	into two separate submodules.  The {!Raw_steps} submodule offers low-level
	means of specifying the contents of the wizard, while the {!Standard}
	submodule offers a simpler, higher-level (albeit less flexible) alternative.
	This document continues with a brief tutorial on each of these interfaces.
*)


(********************************************************************************)
(**	{3 Low-level interface}							*)
(********************************************************************************)

(**	The {!Raw_steps} module provides a low-level interface to the construction
	of a wizard.  By {i low-level}, it is meant that the user must declare
	each wizard step explicitly and separately; moreover, the wizard steps
	must be declared in reverse sequential order (a tell-tale sign that the
	underlying [Eliom] mechanism is not made completely opaque to the user
	of the module).

	We begin by declaring the blocks that constitute the site.  {!Litiom_wizard}
	relies on {!Litiom_blocks}, and therefore these blocks must be built to
	satisfy the requirements of the latter module.  The first block, [get_login]
	is a source node that returns the currently logged in user (or suppose it
	does; to simplify the example if just returns a random integer).  The four
	subsequent blocks are sinks, returning actual XHTML content:

	{v
	let get_login sp () =
		Lwt.return (Random.int 10)

	let header _ _ =
		Lwt.return [div [h1 [pcdata "Header"]]]

	let footer _ _ =
		Lwt.return [div [h1 [pcdata "Footer"]]]

	let cancelled _ _ =
		Lwt.return [div [h1 [pcdata "Cancelled!"]]]

	let error exc_list = fun _ _ ->
		Lwt.return [div [h1 [pcdata "Error!"]]]
	v}


	We then define the standard handler, using the plumbing primitives from {!Litiom_blocks}:

	{v
	let standard_handler sp canvas_tree =
		let tree = container
				(source get_login)
				[
				sink header;
				canvas_tree;
				sink footer
				]
		in run_tree sp tree >>= fun page_body ->
		Lwt.return
			(html
			(head (title (pcdata "")) [])
			(body page_body))
	v}

	We can now declare the various steps of the wizard.

	{v
	let fallback =
        Eliom_services.new_service
                ~path: [""]
                ~get_params: Eliom_parameters.unit
                ()
	v}

	The last step must be declared first.  Note the use of function {!Raw_steps.make_last}:

	{v
	let step3 =
		let step3_contents ~carry =
			let (((), a), b) = carry
			in [p [pcdata (Printf.sprintf "%d + %d = %d" a b (a + b))]]
		in Litiom_wizard.Raw_steps.make_last
			~fallback
			~tree_builder: standard_handler
			~cancel_canvas: (sink cancelled)
			~error_canvas: (fun exc_list -> sink (error exc_list))
			~params: (Eliom_parameters.int "b")
			~carrier: Litiom_wizard.Carriers.carry_both
			~form_contents: step3_contents
	v}

	Now the intermediate step:

	{v
	let step2 =
		let step2_contents ~carry enter_b =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_b"] [pcdata "Enter number 'B':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_b"] ~input_type:`Text ~name:enter_b ();
				]
			]
		in Litiom_wizard.Raw_steps.make_middle
			~fallback
			~tree_builder: standard_handler
			~cancel_canvas: (sink cancelled)
			~error_canvas: (fun exc_list -> sink (error exc_list))
			~params: (Eliom_parameters.int "a")
			~carrier: Litiom_wizard.Carriers.carry_both
			~form_contents: step2_contents
			~next_step_register: step3
	v}

	And finally the first step of the wizard:

	{v
	let step1 =
		let step1_contents ~carry enter_a =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_a"] [pcdata "Enter number 'A':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_a"] ~input_type:`Text ~name:enter_a ();
				]
			]
		in Litiom_wizard.Raw_steps.make_first
			~fallback
			~tree_builder: standard_handler
			~carrier: Litiom_wizard.Carriers.carry_none
			~form_contents:step1_contents
			~next_step_register: step2
	v}
*)


(********************************************************************************)
(**	{3 High-level interface}						*)
(********************************************************************************)

(**	TO BE DONE.
*)

(********************************************************************************)
(**	{2 Private submodules (to be removed from .mli)}			*)
(********************************************************************************)

module type SUBMIT =
  sig
    type t = Proceed | Cancel
    val of_string : string -> t
    val to_string : t -> string
    val param_label : string
    val param :
      (t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameters.param_name)
      Eliom_parameters.params_type
    val make_controls :
      [< t Eliom_parameters.setoneopt ] Eliom_parameters.param_name ->
      [> `Fieldset ] XHTML.M.elt
  end
module Submit : SUBMIT
module Canvas :
  sig
    val inter :
      form_contents:(carry:'a ->
                     'b -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step:(unit, 'c, [< Eliom_services.post_service_kind ],
                 [< Eliom_services.suff ], 'd,
                 'b *
                 [< Submit.t Eliom_parameters.setoneopt ]
                 Eliom_parameters.param_name,
                 [< Eliom_services.registrable ])
                Eliom_services.service ->
      carry:'a ->
      Eliom_sessions.server_params -> 'e -> [> `Div ] XHTML.M.elt list Lwt.t
    val final :
      form_contents:(carry:'a ->
                     [< `A
                      | `Abbr
                      | `Acronym
                      | `Address
                      | `B
                      | `Bdo
                      | `Big
                      | `Blockquote
                      | `Br
                      | `Button
                      | `Cite
                      | `Code
                      | `Del
                      | `Dfn
                      | `Div
                      | `Dl
                      | `Em
                      | `Fieldset
                      | `Form
                      | `H1
                      | `H2
                      | `H3
                      | `H4
                      | `H5
                      | `H6
                      | `Hr
                      | `I
                      | `Img
                      | `Input
                      | `Ins
                      | `Kbd
                      | `Label
                      | `Map
                      | `Noscript
                      | `Object
                      | `Ol
                      | `P
                      | `PCDATA
                      | `Pre
                      | `Q
                      | `Samp
                      | `Script
                      | `Select
                      | `Small
                      | `Span
                      | `Strong
                      | `Sub
                      | `Sup
                      | `Table
                      | `Textarea
                      | `Tt
                      | `Ul
                      | `Var ]
                     XHTML.M.elt list) ->
      carry:'a -> 'b -> 'c -> [> `Div ] XHTML.M.elt list Lwt.t
  end
module Handler :
  sig
    val initial :
      carrier:(unit -> unit -> 'a) ->
      next_step_register:(carry:'a -> 'b -> 'c) ->
      tree_builder:('b -> ('d, Litiom_blocks.out_t) Litiom_blocks.t -> 'e) ->
      canvas:(next_step:'c ->
              carry:'a ->
              Litiom_blocks.sp_t -> 'd -> Litiom_blocks.out_t Lwt.t) ->
      'b -> unit -> unit -> 'e
    val inter :
      carrier:('a -> 'b -> 'c) ->
      next_step_register:(carry:'c -> 'd -> 'e) ->
      cancel_canvas:('f, Litiom_blocks.out_t) Litiom_blocks.t ->
      tree_builder:('d -> ('f, Litiom_blocks.out_t) Litiom_blocks.t -> 'g) ->
      canvas:(next_step:'e ->
              carry:'c ->
              Litiom_blocks.sp_t -> 'f -> Litiom_blocks.out_t Lwt.t) ->
      carry:'a -> 'd -> unit -> 'b * Submit.t -> 'g
    val final :
      carrier:('a -> 'b -> 'c) ->
      cancel_canvas:('d, Litiom_blocks.out_t) Litiom_blocks.t ->
      tree_builder:('e -> ('d, Litiom_blocks.out_t) Litiom_blocks.t -> 'f) ->
      canvas:(carry:'c ->
              Litiom_blocks.sp_t -> 'd -> Litiom_blocks.out_t Lwt.t) ->
      carry:'a -> 'e -> unit -> 'b * Submit.t -> 'f
  end
module Error_handler :
  sig
    val inter :
      cancel_canvas:'a ->
      error_canvas:('b -> 'a) ->
      tree_builder:(Eliom_sessions.server_params -> 'a -> 'c Lwt.t) ->
      Eliom_sessions.server_params -> 'b -> 'c Lwt.t
  end
module Register :
  sig
    val initial :
      fallback:('a, 'b, [< Eliom_services.internal_service_kind ],
                [< Eliom_services.suff ], 'c, 'd, [ `Registrable ])
               Eliom_services.service ->
      handler:(Eliom_sessions.server_params ->
               'a -> 'b -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      unit
    val inter :
      fallback:('a, unit,
                [ `Attached of
                    [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
                    Eliom_services.a_s ],
                [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
               Eliom_services.service ->
      handler:(carry:'d ->
               Eliom_sessions.server_params ->
               'a -> 'e * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      error_handler:(Eliom_sessions.server_params ->
                     (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      params:('e, [ `WithoutSuffix ], 'f) Eliom_parameters.params_type ->
      carry:'d ->
      Eliom_sessions.server_params ->
      ('a, 'e * Submit.t,
       [> `Attached of
            [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
       'b, 'c, 'f * [ `One of Submit.t ] Eliom_parameters.param_name,
       [> `Registrable ])
      Eliom_services.service
  end


(********************************************************************************)
(**	{2 Public submodules}							*)
(********************************************************************************)

module Raw_steps :
  sig
    val make_first :
      fallback:(unit, unit, [< Eliom_services.internal_service_kind ],
                [< Eliom_services.suff ], 'a, 'b, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('c, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(unit -> unit -> 'd) ->
      form_contents:(carry:'d ->
                     'e -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step_register:(carry:'d ->
                          Eliom_sessions.server_params ->
                          (unit, 'f, [< Eliom_services.post_service_kind ],
                           [< Eliom_services.suff ], 'g,
                           'e *
                           [< Submit.t Eliom_parameters.setoneopt ]
                           Eliom_parameters.param_name,
                           [< Eliom_services.registrable ])
                          Eliom_services.service) ->
      unit
    val make_middle :
      fallback:(unit, unit,
                [ `Attached of
                    [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
                    Eliom_services.a_s ],
                [< Eliom_services.suff ] as 'a, 'b, unit, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('c, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:('d -> 'e -> 'f) ->
      form_contents:(carry:'f ->
                     'g -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step_register:(carry:'f ->
                          Eliom_sessions.server_params ->
                          (unit, 'h, [< Eliom_services.post_service_kind ],
                           [< Eliom_services.suff ], 'i,
                           'g *
                           [< Submit.t Eliom_parameters.setoneopt ]
                           Eliom_parameters.param_name,
                           [< Eliom_services.registrable ])
                          Eliom_services.service) ->
      cancel_canvas:('c, Litiom_blocks.out_t) Litiom_blocks.t ->
      error_canvas:((string * exn) list ->
                    ('c, Litiom_blocks.out_t) Litiom_blocks.t) ->
      params:('e, [ `WithoutSuffix ], 'j) Eliom_parameters.params_type ->
      carry:'d ->
      Eliom_sessions.server_params ->
      (unit, 'e * Submit.t,
       [> `Attached of
            [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
       'a, 'b, 'j * [ `One of Submit.t ] Eliom_parameters.param_name,
       [> `Registrable ])
      Eliom_services.service
    val make_last :
      fallback:(unit, unit,
                [ `Attached of
                    [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
                    Eliom_services.a_s ],
                [< Eliom_services.suff ] as 'a, 'b, unit, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('c, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:('d -> 'e -> 'f) ->
      form_contents:(carry:'f ->
                     [< `A
                      | `Abbr
                      | `Acronym
                      | `Address
                      | `B
                      | `Bdo
                      | `Big
                      | `Blockquote
                      | `Br
                      | `Button
                      | `Cite
                      | `Code
                      | `Del
                      | `Dfn
                      | `Div
                      | `Dl
                      | `Em
                      | `Fieldset
                      | `Form
                      | `H1
                      | `H2
                      | `H3
                      | `H4
                      | `H5
                      | `H6
                      | `Hr
                      | `I
                      | `Img
                      | `Input
                      | `Ins
                      | `Kbd
                      | `Label
                      | `Map
                      | `Noscript
                      | `Object
                      | `Ol
                      | `P
                      | `PCDATA
                      | `Pre
                      | `Q
                      | `Samp
                      | `Script
                      | `Select
                      | `Small
                      | `Span
                      | `Strong
                      | `Sub
                      | `Sup
                      | `Table
                      | `Textarea
                      | `Tt
                      | `Ul
                      | `Var ]
                     XHTML.M.elt list) ->
      cancel_canvas:('c, Litiom_blocks.out_t) Litiom_blocks.t ->
      error_canvas:((string * exn) list ->
                    ('c, Litiom_blocks.out_t) Litiom_blocks.t) ->
      params:('e, [ `WithoutSuffix ], 'g) Eliom_parameters.params_type ->
      carry:'d ->
      Eliom_sessions.server_params ->
      (unit, 'e * Submit.t,
       [> `Attached of
            [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
       'a, 'b, 'g * [ `One of Submit.t ] Eliom_parameters.param_name,
       [> `Registrable ])
      Eliom_services.service
  end
module Carriers :
  sig
    val carry_both : 'a -> 'b -> 'a * 'b
    val carry_previous : 'a -> 'b -> 'a
    val carry_current : 'a -> 'b -> 'b
    val carry_none : 'a -> 'b -> unit
  end
