open Brr
(** {!Jsoo WebRTC}

    OCaml bindings to the browser's WebRTC interface. *)

module IceServer : sig
  type t

  include Jv.CONV with type t := t
end

module IdentityAssertion : sig
  type t

  include Jv.CONV with type t := t

  val get_idp : t -> Jstr.t
  (** [get_idp t] is the provider of the identity assertion *)

  val get_name : t -> Jstr.t
  (** [get_name t] indicates the name of the identity assertion *)
end

module IceCandidate : sig
  type t
  (** A candidate internet connectivity establishment configuration which can be
      used to establish a {!PeerConnection}. *)

  include Jv.CONV with type t := t

  (** {2:props Properties}*)

  val get_candidate : t -> Jstr.t
  (** [get_candidate t] returns the transport address in the format described in
      {{:https://datatracker.ietf.org/doc/html/rfc5245} RFC5245}. *)

  val get_component : t -> [ `RTP | `RTCP ]
  (** [get_component t] identifies the candidate as either for RTP or RTCP. *)

  val get_foundation : t -> Jstr.t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate/foundation}
      See the docs} *)

  val get_ip : t -> Jstr.t
  (** [get_ip t] is the IP address of the candidate *)

  val get_port : t -> int
  (** [get_port t] is the integer port number of the candidate *)

  val get_priority : t -> int
  (** [get_priority t] is the candidates priority *)

  (* can be long ? *)

  val get_address : t -> Jstr.t
  (** [get_address t] is the candidate's address *)

  val get_protocol : t -> [ `TCP | `UDP ]
  (** [get_protocol t] is the candidates protocol *)

  val get_related_port : t -> int option
  (** The port number of the reflexive or relay candidates of this candidate *)

  val get_sdp_mid : t -> Jstr.t option
  (** [get_sdp_mid t] is the candidate's media stream identification tag *)

  val get_sdp_m_line_index : t -> int option
  (** {{:https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate/sdpMLineIndex}
      See docs} *)

  val get_tcp_type : t -> [ `Active | `Passive | `So ] option
  (** If a TCP connection this is the type of connection. [`Active] means the
      transport will open an outbound connectiion but won't receive incoming
      connection requests, [`Passive] is the opposite and [`So] means the
      transport will try to open a connection simultaneously with the peer. *)

  (** {2:meths Methods} *)

  val to_json : t -> Brr.Json.t
  (** [to_json t] is the JSON representing the configuration of the IceCandidate
      configuration *)
end

module DtlsTransport : sig
  type t
  (** Datagram Transport Layer Security description *)

  include Jv.CONV with type t := t

  val get_ice_transport : t -> Jv.t (* TODODODODODO *)

  val get_state : t -> [ `New | `Connecting | `Connected | `Closed | `Failed ]
end

module RtpSender : sig
  type t
  (** An interface for controling and obtaining details to a
      {!Media.StreamTrack} *)

  include Jv.CONV with type t := t

  val get_dtmf : t -> Jv.t option
  (** {{:https://developer.mozilla.org/en-US/docs/Glossary/DTMF} Dual-tone
      Multi-frequency signaling} -- TODO encodee
      {{:https://developer.mozilla.org/en-US/docs/Web/API/RTCDTMFSender} the
      object}. *)

  val get_track : t -> Media.StreamTrack.t option
  (** The {!Media.StreamTrack} being handled bu this RTP sender. *)

  val get_transport : t -> DtlsTransport.t option
end

module RtpReceiver : sig
  type t
end

module RtpTransceiver : sig
  type t
  (** A permanent pairing of a Sender and Receiver *)

  include Jv.CONV with type t := t

  (** {2 Properties} *)

  type direction = Sendrecv | Sendonly | Recvonly | Inactive

  val get_current_direction : t -> direction option
  (** [get_current_direction t] of the transceiver *)

  val get_direction : t -> direction option
  (** [get_direction t] is the transceiver's preferred direction *)

  val set_direction : direction -> t -> (unit, [ `Msg of string ]) result
  (** [set_direction t] sets the direction of the transceiver *)

  val get_mid : t -> Jstr.t option
  (** The media ID of the m-line associated with the transceiver *)

  val get_receiver : t -> RtpReceiver.t

  val get_sender : t -> RtpSender.t

  val get_stopped : t -> bool

  (** {2 Methods} *)

  val set_codec_preferences : t -> unit
  (** TODO *)

  val stop : t -> unit
  (** Permanently stops the transceiver *)
end

module SctpTransport : sig
  type t
  (** Stream Control Transmission Protocol description *)

  external as_target : t -> Brr.Ev.target = "%identity"
  (** [as_target t] converts [t] to an event target *)

  include Jv.CONV with type t := t

  val get_max_channels : t -> int
  (** [get_max_channels t] indicates the maximum number of {!DataChannels} that
      can be open simultaneously *)

  val get_max_message_size : t -> int
  (** [get_max_message_size t] indicate in bytes, the maximum size of a message
      than can be sent over the {!DataChannel} *)

  module State : sig
    type t = [ `Connected | `Connecting | `Closed ]
  end

  val state : State.t Brr.Ev.Type.t

  val get_state : t -> State.t
  (** [get_state t] is the state of the SCTP transport *)

  val get_transport : t -> DtlsTransport.t
  (** [get_transport t] returns the {!DtlsTransport} object used for the
      transmission and receipt of the packets *)
end

(** {2 Data Channels}

    The data channels allows us to represent a network channel which can be used
    for bidirectional peer-to-peer transfer of data. *)

module DataChannel : sig
  type t
  (** A certificate used by the connection for authentication *)

  include Jv.CONV with type t := t

  type opts

  val opts :
    ?ordered:bool ->
    ?max_packet_life_time:int ->
    ?max_retransmits:int ->
    ?protocol:Jstr.t ->
    ?negotiated:bool ->
    ?id:int ->
    unit ->
    opts

  module State : sig
    type t = Connecting | Open | Closing | Closed

    val to_string : t -> string

    val of_string : string -> t
  end

  val get_ready_state : t -> State.t

  module Ev : sig
    val set_on_open : (Ev.void Ev.t -> 'a) -> t -> unit

    val set_on_message : (Brr_io.Message.Ev.t Ev.t -> 'a) -> t -> unit

    val set_on_close : (Ev.void Ev.t -> 'a) -> t -> unit
  end

  val send : Jstr.t -> t -> unit

  val send_blob : Blob.t -> t -> unit

  val send_array : Tarray.Buffer.t -> t -> unit

  val close : t -> unit
end

(** {2 Peer Connections}*)

module PeerConnection : sig
  module Bundle : sig
    (** The bundle policy is part of the RTCConfiguration for peer connections.
        Bundle is a feature of the Session Description Protocol (SDP) that
        allows you to specify a media flow to have the same source and
        destination addresses and ports, along with the same udnerlyign transfer
        protocol. {{:https://webrtcstandards.info/sdp-bundle/} WebRTC Standards
        has further information}.*)

    type t = [ `MaxBundle | `MaxCompat | `Balanced ]
    (** Bundle policy type *)

    val to_string : t -> string
    (** [to_string b] converts the bundle policy to a string *)

    val to_jstr : t -> Jstr.t
    (** [to_jstr] is [Jstr.v @@ to_string bundle] *)
  end

  module Certificate : sig
    type t = Jv.t
    (** A certificate used by the connection for authentication *)

    include Jv.CONV with type t := t

    val expires : t -> Jstr.t
    (** [expires t] is the expiration date of the certificate [t] *)
  end

  module IceTransportPolicy : sig
    type t = [ `All | `Relay ]
    (** Possible ICE transport policies. [`All] lets any candidate be considered
        whereas [`Relay] only considers those relayed through some other server
        (typically STUN or TURN servers) *)

    val to_string : t -> string
    (** [to_string b] converts the ICE transport policy to a string *)

    val to_jstr : t -> Jstr.t
    (** [to_jstr] is [Jstr.v @@ to_string ice] *)
  end

  module RtcpMuxPolicy : sig
    type t = [ `Require ]

    val to_string : t -> string
    (** [to_string b] converts the RTCP mux policy to a string *)

    val to_jstr : t -> Jstr.t
    (** [to_jstr] is [Jstr.v @@ to_string policy] *)
  end

  type config
  (** RTCConfiuration type *)

  val config :
    ?bundle_policy:Bundle.t ->
    ?certificates:Certificate.t array ->
    ?ice_candidate_pool_size:int ->
    ?ice_servers:IceServer.t array ->
    ?ice_transport_policy:IceTransportPolicy.t ->
    ?peer_identity:Jstr.t ->
    ?rtcp_mux_policy:RtcpMuxPolicy.t ->
    unit ->
    config
  (** Builder for the RTCConfiguration dictionary *)

  type t
  (** An RTCPeerConnection *)

  include Jv.CONV with type t := t

  external as_target : t -> Brr.Ev.target = "%identity"
  (** [as_target t] converts [t] to an event target *)

  val create : ?opts:config -> unit -> t
  (** [create ?config ()] is a new RTC peer connection using the optional
      [config] configuration *)

  (* {2:props Properties} *)

  val can_trickle_ice_candidates : t -> bool
  (** The [bool] returned indicates whether or not the remote peer can accept
      {{:https://datatracker.ietf.org/doc/html/draft-ietf-mmusic-trickle-ice}
      trickled ice candidates}. *)

  module State : sig
    type t =
      | New
      | Connecting
      | Connected
      | Disconnected
      | Failed
      | Closed  (** State of a peer connection. TODO: describe each *)

    val to_string : t -> string

    val of_string : string -> t

    val to_jstr : t -> Jstr.t

    val of_jstr : Jstr.t -> t
  end

  val get_connection_state : t -> State.t

  module Sd : sig
    type t
    (** Session description object *)

    val get_type : t -> [ `Answer | `Offer | `Pranswer | `Rollback ]

    val get_sdp : t -> Jstr.t
  end

  val get_current_local_description : t -> Sd.t
  (** [current_local_description t] returns session description protocol if it
      can be parsed otherwise and error along with the underlying SDP in string
      format *)

  val get_current_remote_description : t -> Sd.t
  (** same as {!current_local_description} only describing the remote end of the
      connection *)

  module IceState : sig
    type t =
      | New
      | Checking
      | Connected
      | Completed
      | Failed
      | Disconnected
      | Closed  (** State of an ICE agent *)

    val to_string : t -> string

    val of_string : string -> t

    val to_jstr : t -> Jstr.t

    val of_jstr : Jstr.t -> t
  end

  val get_ice_connection_state : t -> IceState.t
  (** [ice_connection_state t] is the ICE agent state for peer connection [t] *)

  val get_ice_gathering_state : t -> [ `New | `Gathering | `Complete ]
  (** [ice_gathering_state t] describes peer connection [t]'s ICE gathering
      state. [`New] means the peer connection was just created, [`Gathering]
      means the ICE agent is in the process or gathering candidates and
      [`Completed] means it has finished. *)

  val get_local_description : t -> Sd.t option
  (** [local_description t] describes the session for the local end of the
      connection *)

  val get_peer_identity : t -> IdentityAssertion.t Fut.or_error
  (** [peer_identity t] returns a promise that resolves to an identity assertion
      containing the string identifying the remote peer. *)

  val get_pending_local_description : t -> Sd.t
  (** [pending_local_description t] returns the session description protocol if
      it can be parsed otherwise and error along with the underlying SDP in
      string format for a potential future description not the current one *)

  val get_pending_remote_description : t -> Sd.t
  (** same as {!pending_local_description} only describing the remote end of the
      connection *)

  val get_remote_description : t -> Sd.t
  (** same as {!local_description} only for describing the remote end of the
      conneciton *)

  val get_sctp : t -> SctpTransport.t option
  (** [sctp t] returns the {!SctpTransport} describing the SCTP transport over
      which SCTP data is being sent and received. It will be [None] if it has
      not been negotiated *)

  val get_ready_state : t -> DataChannel.State.t
  (** [get_ready_state]*)

  module SignalingState : sig
    type t =
      | Stable
      | HaveLocalOffer
      | HaveRemoteOffer
      | HaveLocalPranswer
      | HaveRemotePranswer
      | Closed

    val to_string : t -> string

    val of_string : string -> t

    val to_jstr : t -> Jstr.t

    val of_jstr : Jstr.t -> t
  end

  val get_signaling_state : t -> SignalingState.t
  (** [signaling_state t] descibes the state of the signaling process on the
      local end of the connection while connecting or reconnecting to another
      peer *)

  (** {2:events Events} *)

  module Ev : sig
    (** {3 Handlers}*)

    val set_on_open : (Ev.void Ev.t -> 'a) -> t -> unit

    val set_on_message : (Brr_io.Message.Ev.t Ev.t -> 'a) -> t -> unit

    val set_on_close : (Ev.void Ev.t -> 'a) -> t -> unit

    module Ice : sig
      type t

      val candidate : t -> IceCandidate.t
    end

    val set_on_ice_candidate : (Ice.t Ev.t -> 'a) -> t -> unit

    module DataChannel : sig
      type t

      val channel : t -> DataChannel.t
    end

    val set_on_data_channel : (DataChannel.t Ev.t -> 'a) -> t -> unit
  end

  (** {2:methods Methods}

      Methods are the functions you can call on your peer connection. For the
      latest documentation
      {{:https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnection}
      check the MDN pages}. *)

  val add_ice_candidate :
    ?candidate:IceCandidate.t -> connection:t -> unit -> unit Fut.or_error
  (** [add_ice_candidate ~candidate ~connection ()] will add the [canidate] to
      the [connection]'s remote description. If no [candidate] is specified then
      an [end-of-candidates] indicator is added signaling all remote candidates
      have been sent. *)

  val add_track :
    ?stream:Media.Stream.t ->
    track:Media.StreamTrack.t ->
    connection:t ->
    unit ->
    RtpSender.t
  (** [add_track ~track ~connection ()] adds a new [track] to the set of tracks
      that will be sent to the peer *)

  val close : t -> unit
  (** Closes the peer connection *)

  type offer

  val offer :
    ?ice_restart:bool -> ?voice_activity_detection:bool -> unit -> offer

  val create_offer : ?offer:offer -> t -> Sd.t Fut.or_error
  (** Initiate the creation of an SDP offer *)

  val create_answer : ?answer:offer -> t -> Sd.t Fut.or_error
  (** Creates an SDP answer to an offer received from a remote peer during the
      offer/answer negotiation *)

  val create_data_channel :
    ?opts:DataChannel.opts -> label:string -> t -> DataChannel.t

  val generate_certificate :
    Brr_webcrypto.Crypto_algo.t -> Certificate.t Fut.or_error
  (** A static function that creates an X.509 certificate corresponding private
      key *)

  val get_senders : t -> RtpSender.t array
  (** Returns an array RTP senders, each responsible for sending one track's
      data *)

  module StatsReport : sig
    type t
    (** {{:https://developer.mozilla.org/en-US/docs/Web/API/RTCStatsReport}
        StatsReport} *)

    include Jv.CONV with type t := t
  end

  val get_stats :
    ?selector:Media.StreamTrack.t -> t -> StatsReport.t Fut.or_error
  (** [get_stats t] will returns the stats for the whole connection *)

  val get_transceivers : t -> RtpTransceiver.t array
  (** [get_transceivers t] gets all of the peer connection's transceivers in the
      order they were added. You could stop all of the transceivers associated
      with a peer connection using:

      {[ Array.iter RtpTransceiver.stop (get_transceivers t) ]}*)

  val remove_track : RtpSender.t -> t -> unit
  (** [remove_track sender t] will remove [sender] from the peer connection [t],
      this will stop sending media from the track for the local end of the
      connection. *)

  val restart_ice : t -> unit
  (** [restart_ice t] will request that ICE candidate gathering be redone on
      both ends of the connection. You might want to use this on a state change
      and the state is [Failed]. *)

  val set_local_description : Sd.t -> t -> unit Fut.or_error

  val set_remote_description : Sd.t -> t -> unit Fut.or_error
end
