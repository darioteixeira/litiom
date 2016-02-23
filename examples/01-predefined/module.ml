open Eliom_content.Html5.F

module Coucou = Litiom_type.Int32   (* Try replacing with Int64 or Float *)

let coucou_handler x () =
    Lwt.return
        (html
        (head (title (pcdata "Coucou")) [])
        (body [p [pcdata ("X = " ^ Coucou.to_string x)]]))

let coucou_service =
    Eliom_registration.Html5.register_service
        ~path:["coucou"]
        ~get_params:(Coucou.param "x")
        coucou_handler

let coucou_form enter_x =
    [
    fieldset
        [
        label [pcdata "Enter X:"];
        Coucou.input ~input_type:`Text ~name:enter_x ();
        (* Alternative formulation: Form.input ~input_type:`Text ~name:enter_x Coucou.typ; *)
        ]
    ]

let main_handler () () =
    Lwt.return
        (html
        (head (title (pcdata "Main")) [])
        (body [Form.get_form coucou_service coucou_form]))

let main_service =
    Eliom_registration.Html5.register_service
        ~path:[]
        ~get_params:Eliom_parameter.unit
        main_handler

