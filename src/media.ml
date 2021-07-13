module StreamTrack = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end

module Stream = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)
end
