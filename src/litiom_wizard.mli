(********************************************************************************)
(**	Litiom_wizard module.

	This module offers routines aiming to automate and simplify
	the construction of wizard-like interactions in websites.

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

	Wizard-like interactions can of course be built directly with [Eliom].  More
	specifically, the developer can make use of attached coservices registered
	in session tables, thereby taking advantage of the continuation-based
	programming paradigm offered by [Eliom].  However, since this is a fairly
	common and repetitive pattern, it makes sense to "scratch the boilerplate"
	and to create routines to automate the task.  Herein lies the rationale
	for the {!Litiom_wizard} module.

	So what does {!Litiom_wizard} offer?  Essentially, the developer only
	needs to specify the non-repetitive contents of each dialog's form.
	{!Litiom_wizard} will automatically take care of adding and managing
	the "Cancel" and "Proceed" buttons that animate the wizard, handling
	the "discardable forms" problem that arises when the user presses
	"Cancel" without filling a form, and of course, registering and invoking
	each successive step in the wizard.

	The above described facilities are offered by the {!Steps} submodule, and
	this document continues with a tutorial explaining how that submodule should
	be used.  The tutorial is based on a simple example: a three-step wizard
	that asks for an integer [a] on the first step, another integer [b] on the
	second step, and presents the result of [a+b] on the third and last step.
*)


(********************************************************************************)
(**	{3 Tutorial}								*)
(********************************************************************************)

(**	The {!Steps} module provides a moderately low-level interface to the
	construction of a wizard.  By {i low-level}, it is meant that the user
	must declare each wizard step explicitly and separately; moreover, the
	wizard steps must be declared in reverse sequential order (a tell-tale
	sign that the underlying [Eliom] mechanism is not rendered completely
	opaque to the user of the module).

	We begin by declaring the blocks that constitute the site.  {!Litiom_wizard}
	relies on {!Litiom_blocks}, and therefore these blocks must be built to
	satisfy the requirements of the latter module.  The first block, [get_login]
	is a source node that returns the currently logged in user (or suppose it
	does; to simplify the example if just returns a fixed integer).  The two
	blocks that follow, [header] and [footer] will appear in every page.  Finally,
	the blocks [cancelled_canvas] and [error_frame] are the special blocks that
	should be shown should the user cancel the wizard or an error situation occur,
	respectively.  Note that with the exception of [get_login], all blocks are
	are sinks, returning actual XHTML content:

	{v
	let get_login sp () =
		Lwt.return 10

	let header _ _ =
		Lwt.return [div [h1 [pcdata "header"]]]

	let footer _ _ =
		Lwt.return [div [h1 [pcdata "footer"]]]

	let cancelled_canvas _ _ =
		Lwt.return [div [h1 [pcdata "Cancelled!"]]]

	let error_frame exc_list = fun _ _ ->
		Lwt.return [div [h1 [pcdata "Error!"]]]
	v}


	We then define the standard page builder, using the plumbing primitives from
	{!Litiom_blocks}.  Note that the every page is composed of three visible boxes:
	a constant header and footer, and a "frame" box which varies from page to page.

	{v
	let standard_handler ~page_title sp frame_tree =
		let tree =
			container
				(source get_login)
				[
				sink header;
				frame_tree;
				sink footer
				]
		in run_tree sp tree >>= fun page_body ->
		Lwt.return
			(html
				(head (title (pcdata page_title)) [])
				(body page_body))
	v}


	We must also declare the fallback for the wizard steps.  This fallback will be
	passed to all the functions that create the wizard steps.

	{v
	let fallback =
        Eliom_services.new_service
                ~path: [""]
                ~get_params: Eliom_parameters.unit
                ()
	v}

	
	We can at last create the various steps of the wizard.  Remember that the steps
	must be declared in reverse order.  We therefore begin with the last, created by
	function {!Steps.make_last}.  As for the parameters to this function, here's
	what you should know:

	{ul
		{li [fallback] is the customary [Eliom] fallback for expired services, etc.}
		{li [tree_builder] is a function that constructs a page.  This function takes
		two arguments: the usual [sp] server parameters, and a [frame_tree] containing
		the block that defines the frame.}
		{li [carrier] defines how the step's parameters should be given to the [contents]
		function and eventually carried over to the next step.  Each step (except the first)
		will typically have two sets of parameters: those carried over from the previous step,
		and the steps specific to the current step.  The function specified by [carrier] takes
		the two sets of parameters and should return the value to be passed on to the next step.
		You can define your own carrier function, or use one of those provided by the {!Carriers}
		module.}
		{li [contents] is the function that produces the step's contents.  For all steps except
		the last, the contents should be a form; as for the last step, the contents are simply
		an XHTML element.  As for the parameters to the [contents] function itself, they are
		as follows:
		{ul
			{li [sp] are the server parameters, as defined by [Eliom].}
			{li [bp] are the block parameter, as commonly used with {!Litiom_blocks}.}
			{li [gp] are the GET parameters of the service.}
			{li [pp] are POST parameters of the service.}
			{li [carry] is the result of the [carrier] function described above.}}}
		{li [cancelled_canvas] is the special frame that should be displayed (instead of [contents])
		should the user press the "Cancel" button.}
		{li [error_frame] is the special frame that should be displayed (instead of [contents])
		should there be an error in the forms parameters.}
		{li [params] are the step's specific (POST) parameters.  These should be specified in the
		[Eliom_parameters] format.}}

	{v
	let step3 =
		let step3_contents ~sp ~bp ~gp ~pp ~carry =
			let (a, b) = carry
			in      [
				p [pcdata (Printf.sprintf "%d + %d = %d" a b (a + b))];
				p [pcdata (Printf.sprintf "Login = %d" bp)]
				]
		in Litiom_wizard.Steps.make_last
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 3/3")
			~carrier: Litiom_wizard.Carriers.carry_both
			~contents: step3_contents
			~cancelled_canvas: (sink cancelled_canvas)
			~error_frame: (fun exc_list -> sink (error_frame exc_list))
			~params: (Eliom_parameters.int "b")
	v}


	Since this example contains a total of only three steps, there is only one intermediate
	(ie, neither initial nor final) step.  It must be created with the {!Steps.make_middle}
	function.  Should there have been more intermediate steps, all of them would likewise have
	been created via this function.  Note that the function's parameters are nearly identical
	to those described for {!Steps.make_last}, differing only in the addition of the
	[next_step_register] parameter.

	{v
	let step2 =
		let step2_contents ~sp ~bp ~gp ~pp ~carry enter_b =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_b"] [pcdata "Enter number 'B':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_b"] ~input_type:`Text ~name:enter_b ();
				]
			]
		in Litiom_wizard.Steps.make_middle
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 2/3")
			~carrier: Litiom_wizard.Carriers.carry_current
			~contents: step2_contents
			~next_step_register: step3
			~cancelled_canvas: (sink cancelled_canvas)
			~error_frame: (fun exc_list -> sink (error_frame exc_list))
			~params: (Eliom_parameters.int "a")
	v}


	Finally, we create the first step of the wizard.  Note that the first step
	does not take any POST parameters, and does not require the provision of
	special frames for error and cancellation situations.

	{v
	let step1 =
		let step1_contents ~sp ~bp ~gp ~pp ~carry enter_a =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_a"] [pcdata "Enter number 'A':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_a"] ~input_type:`Text ~name:enter_a ();
				]
			]
		in Litiom_wizard.Steps.make_first
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 1/3")
			~carrier: Litiom_wizard.Carriers.carry_none
			~contents: step1_contents
			~next_step_register: step2
	v}
	*)


(********************************************************************************)
(**	{2 Private submodules (later to be removed from .mli)}			*)
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
module Frame :
  sig
    val inter :
      contents:(sp:Eliom_sessions.server_params ->
                bp:'a ->
                gp:'b ->
                pp:'c ->
                carry:'d -> 'e -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step:('b, 'f, [< Eliom_services.post_service_kind ],
                 [< Eliom_services.suff ], 'g,
                 'e *
                 [< Submit.t Eliom_parameters.setoneopt ]
                 Eliom_parameters.param_name,
                 [< Eliom_services.registrable ])
                Eliom_services.service ->
      gp:'b ->
      pp:'c ->
      carry:'d ->
      Eliom_sessions.server_params -> 'a -> [> `Div ] XHTML.M.elt list Lwt.t
    val final :
      contents:(sp:'a ->
                bp:'b ->
                gp:'c ->
                pp:'d ->
                carry:'e ->
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
      gp:'c ->
      pp:'d -> carry:'e -> 'a -> 'b -> [> `Div ] XHTML.M.elt list Lwt.t
  end
module Handler :
  sig
    val initial :
      carrier:(unit -> unit -> 'a) ->
      next_step_register:(carry:'a -> 'b -> 'c) ->
      tree_builder:('b -> ('d, Litiom_blocks.out_t) Litiom_blocks.t -> 'e) ->
      frame:(next_step:'c ->
             gp:'f ->
             pp:unit ->
             carry:'a ->
             Litiom_blocks.sp_t -> 'd -> Litiom_blocks.out_t Lwt.t) ->
      'b -> 'f -> unit -> 'e
    val inter :
      carrier:('a -> 'b -> 'c) ->
      next_step_register:(carry:'c -> 'd -> 'e) ->
      cancelled_canvas:('f, Litiom_blocks.out_t) Litiom_blocks.t ->
      tree_builder:('d -> ('f, Litiom_blocks.out_t) Litiom_blocks.t -> 'g) ->
      frame:(next_step:'e ->
             gp:'h ->
             pp:'b ->
             carry:'c ->
             Litiom_blocks.sp_t -> 'f -> Litiom_blocks.out_t Lwt.t) ->
      carry:'a -> 'd -> 'h -> 'b * Submit.t -> 'g
    val final :
      carrier:('a -> 'b -> 'c) ->
      cancelled_canvas:('d, Litiom_blocks.out_t) Litiom_blocks.t ->
      tree_builder:('e -> ('d, Litiom_blocks.out_t) Litiom_blocks.t -> 'f) ->
      frame:(gp:'g ->
             pp:'b ->
             carry:'c ->
             Litiom_blocks.sp_t -> 'd -> Litiom_blocks.out_t Lwt.t) ->
      carry:'a -> 'e -> 'g -> 'b * Submit.t -> 'f
  end
module Error_handler :
  sig
    val inter :
      cancelled_canvas:'a ->
      error_frame:('b -> 'a) ->
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

module Carriers :
  sig
    val carry_both : 'a -> 'b -> 'a * 'b
    val carry_previous : 'a -> 'b -> 'a
    val carry_current : 'a -> 'b -> 'b
    val carry_none : 'a -> 'b -> unit
  end
module Steps :
  sig
    val make_first :
      fallback:('a, unit, [< Eliom_services.internal_service_kind ],
                [< Eliom_services.suff ], 'b, 'c, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('d, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(unit -> unit -> 'e) ->
      contents:(sp:Litiom_blocks.sp_t ->
                bp:'d ->
                gp:'a ->
                pp:unit ->
                carry:'e -> 'f -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step_register:(carry:'e ->
                          Eliom_sessions.server_params ->
                          ('a, 'g, [< Eliom_services.post_service_kind ],
                           [< Eliom_services.suff ], 'h,
                           'f *
                           [< Submit.t Eliom_parameters.setoneopt ]
                           Eliom_parameters.param_name,
                           [< Eliom_services.registrable ])
                          Eliom_services.service) ->
      unit
    val make_middle :
      fallback:('a, unit,
                [ `Attached of
                    [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
                    Eliom_services.a_s ],
                [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('d, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:('e -> 'f -> 'g) ->
      contents:(sp:Litiom_blocks.sp_t ->
                bp:'d ->
                gp:'a ->
                pp:'f ->
                carry:'g -> 'h -> Xhtmltypes.form_content XHTML.M.elt list) ->
      next_step_register:(carry:'g ->
                          Eliom_sessions.server_params ->
                          ('a, 'i, [< Eliom_services.post_service_kind ],
                           [< Eliom_services.suff ], 'j,
                           'h *
                           [< Submit.t Eliom_parameters.setoneopt ]
                           Eliom_parameters.param_name,
                           [< Eliom_services.registrable ])
                          Eliom_services.service) ->
      cancelled_canvas:('d, Litiom_blocks.out_t) Litiom_blocks.t ->
      error_frame:((string * exn) list ->
                   ('d, Litiom_blocks.out_t) Litiom_blocks.t) ->
      params:('f, [ `WithoutSuffix ], 'k) Eliom_parameters.params_type ->
      carry:'e ->
      Eliom_sessions.server_params ->
      ('a, 'f * Submit.t,
       [> `Attached of
            [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
       'b, 'c, 'k * [ `One of Submit.t ] Eliom_parameters.param_name,
       [> `Registrable ])
      Eliom_services.service
    val make_last :
      fallback:('a, unit,
                [ `Attached of
                    [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
                    Eliom_services.a_s ],
                [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
               Eliom_services.service ->
      tree_builder:(Eliom_sessions.server_params ->
                    ('d, Litiom_blocks.out_t) Litiom_blocks.t ->
                    Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:('e -> 'f -> 'g) ->
      contents:(sp:Litiom_blocks.sp_t ->
                bp:'d ->
                gp:'a ->
                pp:'f ->
                carry:'g ->
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
      cancelled_canvas:('d, Litiom_blocks.out_t) Litiom_blocks.t ->
      error_frame:((string * exn) list ->
                   ('d, Litiom_blocks.out_t) Litiom_blocks.t) ->
      params:('f, [ `WithoutSuffix ], 'h) Eliom_parameters.params_type ->
      carry:'e ->
      Eliom_sessions.server_params ->
      ('a, 'f * Submit.t,
       [> `Attached of
            [> `Internal of [> `Coservice ] * [> `Post ] ] Eliom_services.a_s ],
       'b, 'c, 'h * [ `One of Submit.t ] Eliom_parameters.param_name,
       [> `Registrable ])
      Eliom_services.service
  end
