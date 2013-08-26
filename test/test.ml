open Bigarray
let ( >>= ) = Lwt.( >>= )

let () =
  let max_value = 1_000 in
  (* Create some test values *)
  let file = "test.data" in
  Lwt_main.run (
    Lwt_io.with_file
      ~flags:[Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY]
      ~mode:Lwt_io.output
      file
      (
        fun out ->
          let rec loop i =
            if i >= 0 then (
              Lwt_io.write_float32 out (float i) >>= fun () ->
              loop (i - 1)
            )
            else
              Lwt.return_unit
          in
          loop max_value
      )
  );
  (* File descriptor base *)
  Lwt_main.run (
    Lwt_unix.openfile file [Unix.O_RDONLY] 0o644 >>= fun fd ->
    let rec loop i =
      if i >= 0 then (
        Lwt_binio.Fd.LittleEndian.read_float32 fd i >>= fun maybe ->
        match maybe with
        | None -> Lwt_io.printl "File descriptor interface - BAD!"
        | Some x ->
          let expected = float_of_int (max_value - i) in
          if x = expected then (
            loop (i - 1)
          )
          else (
            Lwt_io.printl "File descriptor interface - NOT GOOD!" >>= fun () ->
            Lwt_io.printlf "Got %f, expected %f" x expected
          )
      )
      else
        Lwt_io.printl "File descriptor interface - ok"
    in
    loop max_value >>= fun () ->
    Lwt_unix.close fd
  );
  (* Lwt_io-based *)
  Lwt_main.run (
    Lwt_io.with_file ~mode:Lwt_io.input file (
      fun input ->
        let output =
          Lwt_io.make ~mode:Lwt_io.output (fun _ _ _ -> Lwt.return 0)
        in
        let fd = input, output in
        let rec loop i =
          if i >= 0 then (
            Lwt_binio.Io.LittleEndian.read_float32 fd i >>= fun maybe ->
            match maybe with
            | None -> Lwt_io.printl "IO interface - BAD!"
            | Some x ->
              let expected = float_of_int (max_value - i) in
              if x = expected then (
                loop (i - 1)
              )
              else (
                Lwt_io.printl "IO interface - NOT GOOD!" >>= fun () ->
                Lwt_io.printlf "Got %f, expected %f" x expected
              )
          )
          else
            Lwt_io.printl "IO interface - ok"
        in
        loop max_value
    )
  )

