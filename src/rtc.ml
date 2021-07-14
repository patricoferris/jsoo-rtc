open Brr

let ( >>= ) a f = Option.map f a

module IceServer = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

module IdentityAssertion = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let get_idp t = Jv.get t "idp" |> Jv.to_jstr

  let get_name t = Jv.get t "name" |> Jv.to_jstr
end

module IceCandidate = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  (** {2:props Properties}*)

  let get_candidate t = Jv.get t "candidate" |> Jv.to_jstr

  let get_component t =
    Jv.get t "component" |> Jv.to_string |> function
    | "rtp" -> `RTP
    | "rtcp" -> `RTCP
    | _ -> raise (Invalid_argument "RTP or RTCP expected")

  let get_foundation t = Jv.get t "foundation" |> Jv.to_jstr

  let get_ip t = Jv.get t "ip" |> Jv.to_jstr

  let get_port t = Jv.get t "port" |> Jv.to_int

  let get_priority t = Jv.get t "priority" |> Jv.to_int

  let get_address t = Jv.get t "address" |> Jv.to_jstr

  let get_protocol t =
    Jv.get t "protocol" |> Jv.to_string |> function
    | "tcp" -> `TCP
    | "udp" -> `UDP
    | _ -> raise (Invalid_argument "UDP or TCP expected")

  let get_related_port t = Jv.find t "relatedPort" |> Option.map Jv.to_int

  let get_sdp_mid t = Jv.find t "sdpMid" |> Option.map Jv.to_jstr

  let get_sdp_m_line_index t =
    let ml = Jv.find t "sdpMLineIndex" in
    Option.map Jv.to_int ml

  let get_tcp_type t =
    let ml = Option.map Jv.to_string @@ Jv.find t "tcpType" in
    Option.bind ml (function
      | "active" -> Some `Active
      | "passive" -> Some `Passive
      | "so" -> Some `So
      | _ -> None)

  (** {2:meths Methods} *)

  let to_json t : Brr.Json.t = Jv.call t "toJson" [||]
end

module DtlsTransport = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let get_ice_transport t = Jv.get t "iceTransport"

  let get_state t =
    Jv.get t "state" |> Jv.to_string |> function
    | "new" -> `New
    | "connecting" -> `Connecting
    | "connected" -> `Connected
    | "closed" -> `Closed
    | "failed" -> `Failed
    | _ -> raise (Invalid_argument "Unknown state")
end

module RtpSender = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let get_dtmf t = Jv.find t "dtmf"

  let get_track t = Option.map Media.StreamTrack.of_jv @@ Jv.find t "track"

  let get_transport t = Option.map DtlsTransport.of_jv @@ Jv.find t "transport"
end

module RtpReceiver = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

module RtpTransceiver = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type direction = Sendrecv | Sendonly | Recvonly | Inactive

  let direction_of_string_opt = function
    | "sendrecv" -> Some Sendrecv
    | "sendonly" -> Some Sendonly
    | "recvonly" -> Some Recvonly
    | "inactive" -> Some Inactive
    | _ -> None

  let direction_to_string = function
    | Sendrecv -> "sendrecv"
    | Sendonly -> "sendonly"
    | Recvonly -> "recvonly"
    | Inactive -> "inactive"

  let get_current_direction t =
    let cd = Jv.find t "currentDirection" |> Option.map Jv.to_string in
    Option.bind cd direction_of_string_opt

  let get_direction t =
    let cd = Jv.find t "direction" |> Option.map Jv.to_string in
    Option.bind cd direction_of_string_opt

  let set_direction d t =
    Ok (Jv.set t "direction" (Jv.of_string (direction_to_string d)))

  let get_mid t = Jv.find t "mid" |> Option.map Jv.to_jstr

  let get_receiver t = Jv.get t "receiver" |> RtpReceiver.of_jv

  let get_sender t = Jv.get t "sender" |> RtpSender.of_jv

  let get_stopped t = Jv.get t "stopped" |> Jv.to_bool

  (** {2 Methods} *)

  (** TODO *)
  let set_codec_preferences _t = ()

  let stop t = Jv.call t "stop" [||] |> ignore
end

module SctpTransport = struct
  type t = Jv.t

  external as_target : t -> Brr.Ev.target = "%identity"

  include (Jv.Id : Jv.CONV with type t := t)

  let get_max_channels t = Jv.get t "maxChannels" |> Jv.to_int

  let get_max_message_size t = Jv.get t "maxMessageSize" |> Jv.to_int

  module State = struct
    type t = [ `Connected | `Connecting | `Closed ]

    let of_string = function
      | "connected" -> `Connected
      | "connecting" -> `Connecting
      | "closed" -> `Closed
      | _ -> raise (Invalid_argument "Unknown state string")
  end

  let state = Ev.Type.create (Jstr.v "state")

  let get_state t = Jv.get t "state" |> Jv.to_string |> State.of_string

  let get_transport t = Jv.get t "transport" |> DtlsTransport.of_jv
end

module DataChannel = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  type opts = Jv.t

  let opts ?ordered ?max_packet_life_time ?max_retransmits ?protocol ?negotiated
      ?id () =
    let o = Jv.obj [||] in
    Jv.Bool.set_if_some o "ordered" ordered;
    Jv.Int.set_if_some o "maxPacketLifeTime" max_packet_life_time;
    Jv.Int.set_if_some o "maxRetransmits" max_retransmits;
    Jv.Jstr.set_if_some o "protocol" protocol;
    Jv.Bool.set_if_some o "negotiated" negotiated;
    Jv.Int.set_if_some o "id" id;
    o

  module State = struct
    type t = Connecting | Open | Closing | Closed

    let to_string = function
      | Connecting -> "connecting"
      | Open -> "open"
      | Closing -> "closing"
      | Closed -> "closed"

    let of_string = function
      | "connecting" -> Connecting
      | "open" -> Open
      | "closing" -> Closing
      | "closed" -> Closed
      | _ -> raise (Invalid_argument "Unknown data channel state")
  end

  let get_ready_state t =
    Jv.get t "readyState" |> Jv.to_string |> State.of_string

  module Ev = struct
    let set_on_open f t = Jv.set t "onopen" (Jv.repr f)

    let set_on_message f t = Jv.set t "onmessage" (Jv.repr f)

    let set_on_close f t = Jv.set t "onclose" (Jv.repr f)
  end

  let send s t = Jv.call t "send" [| Jv.of_jstr s |] |> ignore

  let send_blob s t = Jv.call t "send" [| Blob.to_jv s |] |> ignore

  let send_array s t = Jv.call t "send" [| Tarray.Buffer.to_jv s |] |> ignore

  let close t = Jv.call t "close" [||] |> ignore
end

module PeerConnection = struct
  module Bundle = struct
    type t = [ `MaxBundle | `MaxCompat | `Balanced ]

    let to_string = function
      | `MaxBundle -> "max-bundle"
      | `MaxCompat -> "max-compat"
      | `Balanced -> "balanced"

    let to_jstr bundle = Jstr.v @@ to_string bundle
  end

  module Certificate = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let expires t = Jv.get t "expires" |> Jv.to_jstr
  end

  module IceTransportPolicy = struct
    type t = [ `All | `Relay ]

    let to_string = function `All -> "all" | `Relay -> "relay"

    let to_jstr bundle = Jstr.v @@ to_string bundle
  end

  module RtcpMuxPolicy = struct
    type t = [ `Require ]

    let to_string = function `Require -> "require"

    let to_jstr bundle = Jstr.v @@ to_string bundle
  end

  type config = Jv.t

  let config ?bundle_policy ?certificates ?ice_candidate_pool_size ?ice_servers
      ?ice_transport_policy ?peer_identity ?rtcp_mux_policy () =
    let o = Jv.obj [||] in
    Jv.Jstr.set_if_some o "bundlePolicy" (bundle_policy >>= Bundle.to_jstr);
    Jv.set_if_some o "certificates"
      (certificates >>= Jv.of_array Certificate.to_jv);
    Jv.Int.set_if_some o "iceCandidatePoolSize" ice_candidate_pool_size;
    Jv.set_if_some o "iceServers" (ice_servers >>= Jv.of_array IceServer.to_jv);
    Jv.Jstr.set_if_some o "iceTransportPolicy"
      (ice_transport_policy >>= IceTransportPolicy.to_jstr);
    Jv.Jstr.set_if_some o "peerIdentity" peer_identity;
    Jv.Jstr.set_if_some o "rtcpMuxPolicty"
      (rtcp_mux_policy >>= RtcpMuxPolicy.to_jstr);
    o

  type t = Jv.t

  external as_target : t -> Brr.Ev.target = "%identity"

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?(opts = Jv.undefined) () =
    Jv.new' (Jv.get Jv.global "RTCPeerConnection") [| opts |]

  let can_trickle_ice_candidates t =
    Jv.get t "canTrickleIceCandidates" |> Jv.to_bool

  module State = struct
    type t =
      | New
      | Connecting
      | Connected
      | Disconnected
      | Failed
      | Closed  (** State of a peer connection. TODO: describe each *)

    let to_string = function
      | New -> "new"
      | Connecting -> "connecting"
      | Connected -> "connected"
      | Disconnected -> "disconnected"
      | Failed -> "failed"
      | Closed -> "closed"

    let of_string = function
      | "new" -> New
      | "connecting" -> Connecting
      | "connected" -> Connected
      | "disconnected" -> Disconnected
      | "failed" -> Failed
      | "closed" -> Closed
      | _ -> raise (Invalid_argument "Unknown peer connection state")

    let to_jstr t = Jstr.v (to_string t)

    let of_jstr s = Jstr.to_string s |> of_string
  end

  let get_connection_state t =
    Jv.get t "connectionState" |> Jv.to_string |> State.of_string

  module Sd = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let get_type t =
      Jv.get t "type" |> Jv.to_string |> function
      | "answer" -> `Answer
      | "offer" -> `Offer
      | "pranswer" -> `Pranswer
      | "rollback" -> `Rollback
      | _ -> raise (Invalid_argument "Unknown session type")

    let get_sdp t = Jv.get t "sdp" |> Jv.to_jstr
  end

  let get_current_local_description t =
    Jv.get t "currentLocalDescription" |> Sd.of_jv

  let get_current_remote_description t =
    Jv.get t "currentRemoteDescription" |> Sd.of_jv

  module IceState = struct
    type t =
      | New
      | Checking
      | Connected
      | Completed
      | Failed
      | Disconnected
      | Closed

    let to_string = function
      | New -> "new"
      | Checking -> "checking"
      | Connected -> "connected"
      | Completed -> "completed"
      | Disconnected -> "disconnected"
      | Failed -> "failed"
      | Closed -> "closed"

    let of_string = function
      | "new" -> New
      | "checking" -> Checking
      | "connected" -> Connected
      | "completed" -> Completed
      | "disconnected" -> Disconnected
      | "failed" -> Failed
      | "closed" -> Closed
      | _ -> raise (Invalid_argument "Unknown ice state")

    let to_jstr t = Jstr.v (to_string t)

    let of_jstr s = Jstr.to_string s |> of_string
  end

  let get_ice_connection_state t =
    Jv.get t "iceConnectionState" |> Jv.to_string |> IceState.of_string

  let get_ice_gathering_state t =
    Jv.get t "iceGatheringState" |> Jv.to_string |> function
    | "new" -> `New
    | "gathering" -> `Gathering
    | "complete" -> `Complete
    | _ -> raise (Invalid_argument "Unknown gathering state")

  let get_local_description t =
    Jv.find t "localDescription" |> Option.map Sd.of_jv

  let get_peer_identity t =
    Jv.get t "peerIdentity" |> Fut.of_promise ~ok:IdentityAssertion.of_jv

  let get_pending_local_description t =
    Jv.get t "pendingLocalDescription" |> Sd.of_jv

  let get_pending_remote_description t =
    Jv.get t "pendingRemoteDescription" |> Sd.of_jv

  let get_remote_description t = Jv.get t "remoteDescription" |> Sd.of_jv

  let get_sctp t = Jv.find t "sctp" |> Option.map SctpTransport.of_jv

  module SignalingState = struct
    type t =
      | Stable
      | HaveLocalOffer
      | HaveRemoteOffer
      | HaveLocalPranswer
      | HaveRemotePranswer
      | Closed

    let to_string = function
      | Stable -> "stable"
      | HaveLocalOffer -> "have-local-offer"
      | HaveRemoteOffer -> "have-remote-offer"
      | HaveLocalPranswer -> "have-local-pranswer"
      | HaveRemotePranswer -> "have-remote-pranswer"
      | Closed -> "closed"

    let of_string = function
      | "stable" -> Stable
      | "have-local-offer" -> HaveLocalOffer
      | "have-remote-offer" -> HaveRemoteOffer
      | "have-local-pranswer" -> HaveLocalPranswer
      | "have-remote-pranswer" -> HaveRemotePranswer
      | "closed" -> Closed
      | _ -> raise (Invalid_argument "Unknown signaling state")

    let to_jstr t = Jstr.v (to_string t)

    let of_jstr s = Jstr.to_string s |> of_string
  end

  let get_signaling_state t =
    Jv.get t "signalingState" |> Jv.to_string |> SignalingState.of_string

  (** {2:events Events} *)

  module Ev = struct
    let set_on_open f t = Jv.set t "onopen" (Jv.repr f)

    let set_on_message f t = Jv.set t "onmessage" (Jv.repr f)

    let set_on_close f t = Jv.set t "onclose" (Jv.repr f)

    module Ice = struct
      type t = Jv.t

      let candidate t = Jv.get t "candidate" |> IceCandidate.of_jv
    end

    let set_on_ice_candidate f t = Jv.set t "onicecandidate" (Jv.repr f)

    module DataChannel = struct
      type t = Jv.t

      let channel t = Jv.get t "channel" |> DataChannel.of_jv
    end

    let set_on_data_channel f t = Jv.set t "ondatachannel" (Jv.repr f)
  end

  let add_ice_candidate ?candidate ~connection () =
    let candidate =
      Jv.of_option ~none:Jv.undefined IceCandidate.of_jv candidate
    in
    Jv.call connection "addIceCandidate" [| candidate |]
    |> Fut.of_promise ~ok:(fun _ -> ())

  let add_track ?stream ~track ~connection () =
    let stream = Jv.of_option ~none:Jv.undefined Media.Stream.to_jv stream in
    Jv.call connection "addTrack" [| Media.StreamTrack.to_jv track; stream |]
    |> RtpSender.of_jv

  let close t = Jv.call t "close" |> ignore

  type offer = Jv.t

  let offer ?ice_restart ?voice_activity_detection () =
    let o = Jv.obj [||] in
    Jv.Bool.set_if_some o "iceRestart" ice_restart;
    Jv.Bool.set_if_some o "voiceActivityDetection" voice_activity_detection;
    o

  let create_offer ?offer t =
    let offer = Jv.of_option ~none:Jv.undefined Jv.Id.of_jv offer in
    Jv.call t "createOffer" [| offer |] |> Fut.of_promise ~ok:Sd.of_jv

  let create_answer ?answer t =
    let offer = Jv.of_option ~none:Jv.undefined Jv.Id.of_jv answer in
    Jv.call t "createAnswer" [| offer |] |> Fut.of_promise ~ok:Sd.of_jv

  let create_data_channel ?opts ~label t =
    let opts = Jv.of_option ~none:Jv.undefined Jv.Id.to_jv opts in
    Jv.call t "createDataChannel" [| Jv.of_string label; opts |]
    |> DataChannel.of_jv

  let get_ready_state t =
    Jv.get t "readyState" |> Jv.to_string |> DataChannel.State.of_string

  let generate_certificate algo =
    let cert = Brr_webcrypto.Crypto_algo.to_jv algo in
    let create =
      Jv.get Jv.global "RTCPeerConnection" |> fun pc ->
      Jv.get pc "generateCertificate"
    in
    Jv.apply create [| cert |] |> Fut.of_promise ~ok:Certificate.of_jv

  let get_senders t =
    Jv.call t "createSenders" [||] |> Jv.to_array RtpSender.to_jv

  module StatsReport = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)
  end

  let get_stats ?selector t =
    let selector =
      Jv.of_option ~none:Jv.undefined Media.StreamTrack.to_jv selector
    in
    Jv.call t "getStats" [| selector |] |> Fut.of_promise ~ok:StatsReport.of_jv

  let get_transceivers t =
    Jv.call t "getTransceivers" [||] |> Jv.to_array RtpTransceiver.of_jv

  let remove_track sender t =
    Jv.call t "removeTrack" [| RtpSender.to_jv sender |] |> ignore

  let restart_ice t = Jv.call t "restartIce" [||] |> ignore

  let set_local_description sd t =
    Jv.call t "setLocalDescription" [| Sd.to_jv sd |]
    |> Fut.of_promise ~ok:(fun _ -> ())

  let set_remote_description sd t =
    Jv.call t "setRemoteDescription" [| Sd.to_jv sd |]
    |> Fut.of_promise ~ok:(fun _ -> ())
end
