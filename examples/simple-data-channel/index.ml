open Brr
open Rtc
module Pc = PeerConnection

(* ~~~ State ~~~ *)

type t = {
  connect_btn : El.t;
  disconnect_btn : El.t;
  send_btn : El.t;
  message : El.t;
  receive : El.t;
  sc : DataChannel.t option;
  rc : DataChannel.t option;
  local : Pc.t option;
  remote : Pc.t option;
}

let (state : t option ref) = ref None

let get_state () = Option.get !state

let set_state f =
  let s' = get_state () in
  state := Some (f s')

(* ~~~ Some helpers ~~~ *)

let get_element_by_id id =
  Document.find_el_by_id G.document (Jstr.v id) |> Option.get

let set_disabled v el =
  El.set_at (Jstr.v "disabled") (if v then Some (Jstr.v "true") else None) el

let handle_status_change _ =
  let state = get_state () in
  match state.sc with
  | None -> ()
  | Some sc -> (
      match DataChannel.get_ready_state sc with
      | Open ->
          set_disabled false state.message;
          El.set_has_focus true state.message;
          set_disabled false state.send_btn;
          set_disabled false state.disconnect_btn;
          set_disabled true state.connect_btn
      | _ ->
          set_disabled true state.message;
          set_disabled true state.send_btn;
          set_disabled true state.disconnect_btn;
          set_disabled false state.connect_btn)

let handle_receive_message ev =
  let el = El.p [] in
  let txt = El.txt (Brr_io.Message.Ev.data @@ Ev.as_type ev) in
  let state = get_state () in
  El.set_children el [ txt ];
  El.append_children state.receive [ el ]

let receive_channel_callback e =
  let channel = Pc.Ev.DataChannel.channel (Ev.as_type e) in
  DataChannel.Ev.set_on_message handle_receive_message channel;
  DataChannel.Ev.set_on_open
    (fun _ -> Console.log [ Jstr.v "Receive is open!" ])
    channel

let send_message _ =
  let state = get_state () in
  let msg = El.prop El.Prop.value state.message in
  DataChannel.send msg (Option.get state.sc);
  El.set_prop El.Prop.value (Jstr.v "") state.message;
  El.set_has_focus true state.message

let connect_peers _ =
  let open Fut.Syntax in
  let local = Pc.create () in
  set_state (fun state -> { state with local = Some local });
  let send_channel = Pc.create_data_channel ~label:"sendChannel" local in
  set_state (fun state -> { state with sc = Some send_channel });
  DataChannel.Ev.set_on_open handle_status_change send_channel;
  DataChannel.Ev.set_on_close handle_status_change send_channel;
  (* Remote Channel *)
  let remote = Pc.create () in
  set_state (fun state -> { state with remote = Some remote });
  Pc.Ev.set_on_data_channel receive_channel_callback remote;
  Pc.Ev.set_on_ice_candidate
    (fun e ->
      let candidate = Pc.Ev.Ice.candidate (Ev.as_type e) in
      let+ t = Pc.add_ice_candidate ~candidate ~connection:remote () in
      Console.log_if_error ~use:() t)
    local;
  Pc.Ev.set_on_ice_candidate
    (fun e ->
      let candidate = Pc.Ev.Ice.candidate (Ev.as_type e) in
      let+ t = Pc.add_ice_candidate ~candidate ~connection:local () in
      Console.log_if_error ~use:() t)
    remote;
  let init () =
    let open Fut.Result_syntax in
    let* offer = Pc.create_offer local in
    let* () = Pc.set_local_description offer local in
    let* () =
      Pc.(
        set_remote_description
          (get_local_description local |> Option.get)
          remote)
    in
    let* answer = Pc.create_answer remote in
    let* () = Pc.(set_local_description answer remote) in
    Pc.(
      set_remote_description (get_local_description remote |> Option.get) local)
  in
  ignore (init ())

let disconnect_peers _ =
  let state = get_state () in
  Option.iter DataChannel.close state.sc;
  Option.iter DataChannel.close state.rc;
  Option.iter Pc.close state.local;
  Option.iter Pc.close state.remote;
  set_state (fun state ->
      { state with rc = None; sc = None; local = None; remote = None });
  set_disabled true state.message;
  set_disabled true state.send_btn;
  set_disabled true state.disconnect_btn;
  set_disabled false state.connect_btn

let startup () =
  let connect_btn = get_element_by_id "connectButton" in
  let disconnect_btn = get_element_by_id "disconnectButton" in
  let send_btn = get_element_by_id "sendButton" in
  let message = get_element_by_id "message" in
  let receive = get_element_by_id "receivebox" in
  state :=
    Some
      {
        connect_btn;
        disconnect_btn;
        send_btn;
        message;
        receive;
        sc = None;
        rc = None;
        local = None;
        remote = None;
      };
  Ev.listen Ev.click connect_peers (El.as_target connect_btn);
  Ev.listen Ev.click disconnect_peers (El.as_target disconnect_btn);
  Ev.listen Ev.click send_message (El.as_target send_btn)

let () = startup ()
