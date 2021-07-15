open Brr
open Rtc
module Pc = PeerConnection
module Ms = Brr_io.Media.Stream
module Md = Brr_io.Media.Devices

(* ~~~ Helpers ~~~ *)
let get_element_by_id id =
  Document.find_el_by_id G.document (Jstr.v id) |> Option.get

let set_disabled v el =
  El.set_at (Jstr.v "disabled") (if v then Some (Jstr.v "true") else None) el

let set_src_object el ms =
  let el = Brr_io.Media.El.of_el el in
  Brr_io.Media.El.(set_src_object el (Some (Provider.of_media_stream ms)))

(* ~~~ Initial setup ~~~ *)
let start_button = get_element_by_id "startButton"

let call_button = get_element_by_id "callButton"

let hangup_button = get_element_by_id "hangupButton"

let local_stream : Ms.t option ref = ref None

(* ~~~ Peer Connection ~~~ *)

let pc1 : Pc.t option ref = ref None

let pc2 : Pc.t option ref = ref None

let get_name pc =
  let pc1 = Option.get !pc1 in
  if Jv.equal (Pc.to_jv pc) (Pc.to_jv pc1) then "pc1" else "pc2"

let get_other_pc pc =
  let pc1 = Option.get !pc1 in
  if Jv.equal (Pc.to_jv pc) (Pc.to_jv pc1) then Option.get !pc2 else pc1

(* ~~~ Videos ~~~ *)

let local_video = get_element_by_id "localVideo"

let remote_video = get_element_by_id "remoteVideo"

let offer = Pc.offer

let () =
  set_disabled true call_button;
  set_disabled true hangup_button

let start _ =
  Console.log [ Jstr.v "Requesting local steam" ];
  set_disabled true start_button;
  let constraints =
    Ms.Constraints.(v ~audio:(`Yes None) ~video:(`Yes None) ())
  in
  let stream = Md.(get_user_media (of_navigator G.navigator) constraints) in
  Fut.await stream (function
    | Ok stream ->
        set_src_object local_video stream;
        local_stream := Some stream;
        set_disabled false call_button
    | Error err -> Console.warn [ Jv.Error.message err ])

let on_ice_candidate pc ev =
  let connection = get_other_pc @@ Option.get !pc in
  let ev = Ev.as_type ev in
  let candidate = Pc.Ev.Ice.candidate ev in
  let prom = Pc.add_ice_candidate ~candidate ~connection () in
  Fut.await prom (function
    | Ok () -> Console.log [ Jstr.v "Ice candidate added" ]
    | Error err ->
        Console.error [ Pc.to_jv (Option.get !pc); Jv.Error.message err ])

let got_remote_stream ev =
  let ev = Ev.as_type ev in
  let streams = Pc.Ev.Track.streams ev in
  try
    let s = Array.get streams 0 in
    set_src_object remote_video s
  with _ -> Console.error [ Jstr.v "No video stream!" ]

let create_offer_success sd =
  let open Fut.Result_syntax in
  let pc1 = Option.get !pc1 in
  let pc2 = Option.get !pc2 in
  let* _ = Pc.set_local_description sd pc1 in
  let* _ = Pc.set_remote_description sd pc2 in
  let* answer = Pc.create_answer pc2 in
  let* _ = Pc.set_local_description answer pc2 in
  Pc.set_remote_description answer pc1

let call _ =
  let open Fut.Result_syntax in
  set_disabled true call_button;
  set_disabled false hangup_button;
  Console.log [ Jstr.v "Staring call" ];
  match !local_stream with
  | Some local_stream ->
      let pc1' = Pc.create () in
      pc1 := Some pc1';
      Ev.listen Pc.Ev.icecandidate (on_ice_candidate pc1) (Pc.as_target pc1');
      let pc2' = Pc.create () in
      Pc.Ev.set_on_close (fun _ -> Console.log [ Jstr.v "Closed" ]) pc2';
      pc2 := Some pc2';
      Ev.listen Pc.Ev.icecandidate (on_ice_candidate pc2) (Pc.as_target pc2');
      Ev.listen Pc.Ev.track got_remote_stream (Pc.as_target pc2');
      List.iter
        (fun track ->
          let (_ : RtpSender.t) =
            Pc.add_track ~stream:local_stream ~track ~connection:pc1' ()
          in
          ())
        (Ms.get_tracks local_stream);
      let sd =
        let* offer = Pc.create_offer pc1' in
        create_offer_success offer
      in
      Fut.await sd (function
        | Ok _ -> ()
        | Error err -> Console.error [ Jv.Error.message err ])
  | None -> Console.warn [ Jstr.v "Expecting a local stream?" ]

let hangup _ =
  Console.log [ Jstr.v "Ending call" ];
  let pc1' = Option.get !pc1 in
  let pc2' = Option.get !pc2 in
  Pc.close pc1';
  Pc.close pc2';
  Console.log [ Pc.to_jv pc2' ];
  pc1 := None;
  pc2 := None;
  set_disabled true hangup_button;
  set_disabled false call_button

let () =
  Ev.listen Ev.click start (El.as_target start_button);
  Ev.listen Ev.click call (El.as_target call_button);
  Ev.listen Ev.click hangup (El.as_target hangup_button)
