let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

external identity : 'a -> 'a = "%identity"

module type Src_sig = sig
  type t

  val read_bytes : t -> size:int -> offset:int -> Bytes.t option Lwt.t
  val write_bytes : t -> Bytes.t -> offset:int -> unit Lwt.t
end

module type Conv_sig = sig
  val get_int32 : Bytes.t -> int -> int32
  val set_int32 : Bytes.t -> int -> int32 -> unit
  val get_int64 : Bytes.t -> int -> int64
  val set_int64 : Bytes.t -> int -> int64 -> unit
end

module type S = sig
  type src_t

  val read :
    src_t ->
    size:int ->
    offset:int ->
    get:(Bytes.t -> int -> 'a) -> conv:('a -> 'b) -> 'b option Lwt.t
  val write :
    src_t ->
    'a ->
    size:int ->
    offset:int ->
    set:(Bytes.t -> int -> 'b -> 'c) -> conv:('a -> 'b) -> unit Lwt.t
  val four_byte :
    (int32 -> 'a) ->
    ('b -> int32) ->
    (src_t -> int -> 'a option Lwt.t) * (src_t -> int -> 'b -> unit Lwt.t)
  val eight_byte :
    (int64 -> 'a) ->
    ('b -> int64) ->
    (src_t -> int -> 'a option Lwt.t) * (src_t -> int -> 'b -> unit Lwt.t)
  val read_float32 : src_t -> int -> float option Lwt.t
  val write_float32 : src_t -> int -> float -> unit Lwt.t
  val read_int32 : src_t -> int -> int32 option Lwt.t
  val write_int32 : src_t -> int -> int32 -> unit Lwt.t
  val read_float64 : src_t -> int -> float option Lwt.t
  val write_float64 : src_t -> int -> float -> unit Lwt.t
  val read_int64 : src_t -> int -> int64 option Lwt.t
  val write_int64 : src_t -> int -> int64 -> unit Lwt.t
end

module Make(Src : Src_sig)(Conv : Conv_sig) = struct
  type src_t = Src.t

  let read fd ~size ~offset ~get ~conv =
    Src.read_bytes fd ~size ~offset >|= fun maybe_bytes ->
    match maybe_bytes with
    | None -> None
    | Some bytes ->
      Some (
        get bytes 0
        |> conv
      )

  let write fd x ~size ~offset ~set ~conv =
    let bytes = Bytes.make size ' ' in
    set bytes size (conv x);
    Src.write_bytes fd bytes ~offset

  let four_byte r w =
    (fun fd offset -> read fd ~size:4 ~offset ~get:Conv.get_int32 ~conv:r),
    (fun fd offset -> write fd ~size:4 ~offset ~set:Conv.set_int32 ~conv:w)

  let eight_byte r w =
    (fun fd offset -> read fd ~size:8 ~offset ~get:Conv.get_int64 ~conv:r),
    (fun fd offset -> write fd ~size:8 ~offset ~set:Conv.set_int64 ~conv:w)

  let read_float32, write_float32 =
    four_byte Int32.float_of_bits Int32.bits_of_float
  let read_int32, write_int32 =
    four_byte identity identity

  let read_float64, write_float64 =
    eight_byte Int64.float_of_bits Int64.bits_of_float
  let read_int64, write_int64 =
    eight_byte identity identity
end

module Fd = struct
  module Src = struct
    type t = Lwt_unix.file_descr

    let read_bytes fd ~size ~offset =
      let buffer = Bytes.make size ' ' in
      let byte_offset = offset * size in
      Lwt_unix.lseek fd byte_offset Unix.SEEK_SET >>= fun _ ->
      Lwt_unix.read fd buffer 0 size >>= fun bytes_read ->
      let result = if bytes_read = size then Some buffer else None in
      Lwt.return result

    let write_bytes fd buffer ~offset =
      let size = Bytes.length buffer in
      let byte_offset = offset * size in
      Lwt_unix.lseek fd byte_offset Unix.SEEK_SET >>= fun _ ->
      let rec write_loop offset remaining_bytes =
        if remaining_bytes = 0 then
          Lwt.return_unit
        else (
          Lwt_unix.write fd buffer offset remaining_bytes
          >>= fun bytes_written ->
          write_loop (offset + bytes_written) (remaining_bytes - bytes_written)
        )
      in
      write_loop 0 size
  end

  module LittleEndian = Make(Src)(EndianBytes.LittleEndian)
  module BigEndian = Make(Src)(EndianBytes.BigEndian)
end

module Io = struct
  module Src = struct
    type t = Lwt_io.input_channel * Lwt_io.output_channel

    let read_bytes (input, _) ~size ~offset =
      let buffer = Bytes.make size '.' in
      let byte_offset = offset * size in
      Lwt_io.set_position input (Int64.of_int byte_offset) >>= fun () ->
      Lwt_io.read_into input buffer 0 size >>= fun bytes_read ->
      let result = if bytes_read = size then Some buffer else None in
      Lwt.return result

    let write_bytes (_, output) buffer ~offset =
      let size = Bytes.length buffer in
      let byte_offset = offset * size in
      Lwt_io.set_position output (Int64.of_int byte_offset) >>= fun () ->
      Lwt_io.write_from_exactly output buffer 0 size
  end

  module LittleEndian = Make(Src)(EndianBytes.LittleEndian)
  module BigEndian = Make(Src)(EndianBytes.BigEndian)
end
