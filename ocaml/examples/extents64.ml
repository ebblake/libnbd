open Printf

let () =
  NBD.with_handle (
    fun nbd ->
      NBD.add_meta_context nbd "base:allocation";
      NBD.connect_command nbd
                          ["nbdkit"; "-s"; "--exit-with-parent"; "-r";
                           "sparse-random"; "8G"];

      (* Read the extents and print them. *)
      let size = NBD.get_size nbd in
      let cap =
        match NBD.get_extended_headers_negotiated nbd with
        | true -> size
        | false -> 0x8000_0000_L in
      let fetch_offset = ref 0_L in
      while !fetch_offset < size do
        let remaining = Int64.sub size !fetch_offset in
        let fetch_size = min remaining cap in
        NBD.block_status_64 nbd fetch_size !fetch_offset (
          fun meta _ entries err ->
            printf "nbd_block_status callback: meta=%s err=%d\n" meta !err;
            if meta = "base:allocation" then (
              printf "index\t%16s %16s %s\n" "offset" "length" "flags";
              for i = 0 to Array.length entries - 1 do
                let len = fst entries.(i)
                and flags =
                  match snd entries.(i) with
                  | 0_L -> "data"
                  | 1_L -> "hole"
                  | 2_L -> "zero"
                  | 3_L -> "hole+zero"
                  | unknown -> sprintf "unknown (%Ld)" unknown in
                printf "%d:\t%16Ld %16Ld %s\n" i !fetch_offset len flags;
                fetch_offset := Int64.add !fetch_offset len
              done;
            );
            0
        ) (* NBD.block_status *)
      done
  )
