module type Src_sig = sig
  (** Data source *)

  type t
  val read_bytes : t -> size:int -> offset:int -> string option Lwt.t
  val write_bytes : t -> string -> offset:int -> unit Lwt.t
end

module type Conv_sig = sig
  (** Data conversion *)

  val get_int32 : string -> int -> int32
  val set_int32 : string -> int -> int32 -> unit
  val get_int64 : string -> int -> int64
  val set_int64 : string -> int -> int64 -> unit
end

module type S = sig
  type src_t

  val read :
    src_t ->
    size:int ->
    offset:int ->
    get:(string -> int -> 'a) -> conv:('a -> 'b) -> 'b option Lwt.t
  val write :
    src_t ->
    'a ->
    size:int ->
    offset:int ->
    set:(string -> int -> 'b -> 'c) -> conv:('a -> 'b) -> unit Lwt.t
  (** Arbitrary value IO *)

  val four_byte :
    (int32 -> 'a) ->
    ('b -> int32) ->
    (src_t -> int -> 'a option Lwt.t) * (src_t -> int -> 'b -> unit Lwt.t)
  val eight_byte :
    (int64 -> 'a) ->
    ('b -> int64) ->
    (src_t -> int -> 'a option Lwt.t) * (src_t -> int -> 'b -> unit Lwt.t)
  (** Reading and writing values of a specific size *)

  val read_float32 : src_t -> int -> float option Lwt.t
  val write_float32 : src_t -> int -> float -> unit Lwt.t
  val read_int32 : src_t -> int -> int32 option Lwt.t
  val write_int32 : src_t -> int -> int32 -> unit Lwt.t
  val read_float64 : src_t -> int -> float option Lwt.t
  val write_float64 : src_t -> int -> float -> unit Lwt.t
  val read_int64 : src_t -> int -> int64 option Lwt.t
  val write_int64 : src_t -> int -> int64 -> unit Lwt.t
  (** [read_* src index] and [write_* src index x] for numeric types *)
end

module Make(Src : Src_sig)(Conv : Conv_sig) : S with type src_t = Src.t

module Fd : sig
  (** IO using file descriptors *)

  module Src : sig
    type t = Lwt_unix.file_descr
    val read_bytes :
      Lwt_unix.file_descr ->
      size:int -> offset:int -> string option Lwt.t
    val write_bytes :
      Lwt_unix.file_descr -> string -> offset:int -> unit Lwt.t
  end

  module LittleEndian : S with type src_t = Lwt_unix.file_descr
  module BigEndian : S with type src_t = Lwt_unix.file_descr
end

module Io : sig
  (** IO using Lwt's input and output channels *)

  module Src : sig
    type t = Lwt_io.input_channel * Lwt_io.output_channel
    val read_bytes :
      Lwt_io.input_channel * 'a ->
      size:int -> offset:int -> string option Lwt.t
    val write_bytes :
      'a * Lwt_io.output_channel -> string -> offset:int -> unit Lwt.t
  end

  module LittleEndian :
    S with type src_t = Lwt_io.input_channel * Lwt_io.output_channel
  module BigEndian :
    S with type src_t = Lwt_io.input_channel * Lwt_io.output_channel
end
