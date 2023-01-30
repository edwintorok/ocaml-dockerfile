open Dockerfile_opam

let current d =
    let family = Distro.os_family_of_distro d in
    match Sys.os_type with
    | "Unix" -> family = `Linux
    | "Win32" -> family = `Windows
    | "Cygwin" -> family = `Cygwin
    | _ -> false

let () =
    let arch = `X86_64 in (* TODO: current *)
    let distros =
    Distro.active_distros arch
    |> List.filter current
    in
    let opam_hashes = {opam_2_0_hash = "2.0"; opam_2_1_hash = "2.1"; opam_master_hash = "2.1"} in
    let dockerfiles =
        distros
        |> List.map @@ gen_opam2_distro ~arch ~cache:true ~opam_hashes |> List.map snd in
    Dockerfile.(empty
    @@@ dockerfiles)
    |> Dockerfile.string_of_t
    |> print_endline
