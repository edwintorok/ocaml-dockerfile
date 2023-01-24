(*
 * Copyright (c) 2015-2016 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** OPAM-specific Dockerfile rules. *)

val run_as_opam : ('a, unit, string, Dockerfile.t) format4 -> 'a
(** [run_as_opam fmt] runs the command specified by the [fmt]
    format string as the [opam] user. *)

val install_opam_from_source :
  ?add_default_link:bool ->
  ?prefix:string ->
  ?enable_0install_solver:bool ->
  branch:string ->
  hash:string ->
  unit ->
  Dockerfile.t
(** Commands to install OPAM via a source code checkout from GitHub.
    The [branch] can be a git tag or branch (e.g. [2.0] for opam 2.0.x or [2.1] for
    the opam 2.1.x).
    The binaries are installed under [<prefix>/bin/opam-<branch>],
    defaulting to [/usr/local/bin].
    If [add_default_link] is true (the default), then the [opam-<branch>]
    binary is hardlinked to [opam].  Set it to false if you want to install
    multiple opam binaries from different branches in the same container.
    If [enable_0install_solver] is true (false by default), then the [builtin-0install]
    solver should be accessible in the resulting opam binary. *)

type opam_hashes = {
  opam_2_0_hash : string;
  opam_2_1_hash : string;
  opam_master_hash : string;
}

val auto_cache : Ocaml_version.arch -> Distro.t -> Dockerfile.t -> Dockerfile.t
(** [auto_cache arch distro dockerfile] adds automatic caching to [dockerfile].

    It will create the following mounts depending on whether the command runs as root
    or as a regular user:

    /var/cache for [root]. According to the FHS applications must be able to cope with arbitrary files being deleted,
        which is the same requirement that docker cache mounts have.
        However a root application can create a lockfile elsewhere and have a reasonable expectation that it is the only
        one using it (e.g. package managers).
        So use a distro-specific cache id, and mount type locked.

    /home/opam/.cache for [opam] user.
    This is not locked (even on a regular machine there could be multiple applications running with same homedir,
    so applications must manage locking themselves if needed).
    It uses a very coarse cache ID that includes just the OS type
    (the homedir may have been on an NFS share, so applications must be able to cope with a shared homedir between
    different distros already).

    When using this ensure that any commands you run do not clean caches (e.g. 'yum clean all'), otherwise
    they will also clean the cache of other builds.

    @see <https://refspecs.linuxfoundation.org/FHS_3.0/fhs/ch05s05.html> File Hierarchy Standard /var/cache
    @see <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html> XDG_CACHE_HOME on $HOME/.cache
*)

val gen_opam2_distro :
  ?win10_revision:Distro.win10_lcu ->
  ?winget:string ->
  ?clone_opam_repo:bool ->
  ?arch:Ocaml_version.arch ->
  ?labels:(string * string) list ->
  ?enable_auto_cache:bool ->
  opam_hashes:opam_hashes ->
  Distro.t ->
  string * Dockerfile.t
(** [gen_opam2_distro ?enable_auto_cache ~opam_hashes d] will generate a Dockerfile
   for Linux distribution [d] with opam 2.0, opam 2.1, opam 2.2 and opam master,
   per hash given in parameter.
   @return a tuple of the Docker tag and the Dockerfile.
   If [clone_opam_repo] is true (the default) then the Dockerfile will also git
   clone the official opam-repository into [/home/opam/opam-repository].
   If [arch] is not specified, it defaults to the base image that is assumed
   to be multiarch (the main exception to this is i386, which requires different
   base images from amd64).
   If [enable_auto_cache] is true (the default) then Docker BuildKit / Podman 4.x cache mounts
   will be used to cache system package installation, and opam package downloads.
   For native Windows distributions, if [winget] is omitted, then winget
   will be build in an prepended build stage. If specified, then
   winget will be pulled from the [winget] external image.

*)

val all_ocaml_compilers :
  string -> Ocaml_version.arch -> Distro.t -> string * Dockerfile.t
(** [all_ocaml_compilers hub_id arch distro] will generate an opam2
  container that has all the recent OCaml compilers installed into a
  distribution [distro] on architecture [arch]. *)

val separate_ocaml_compilers :
  string -> Ocaml_version.arch -> Distro.t -> (string * Dockerfile.t) list
(** [separate_ocaml_compilers hub_id arch distro] will install a list of
  Dockerfiles that build individual OCaml compiler versions and their
  variants (e.g. flambda) in separate containers. *)

val deprecated : Dockerfile.t
(** [deprecated] is a minimal container that outputs a deprecation error. This
   is used to replace unsupported containers on the Hub rather than leaving an
   unmaintained distribution lying around with possible security holes. *)

val multiarch_manifest :
  target:string -> platforms:(string * string) list -> string
(** [multiarch_manifest ~target ~platforms] will generate a manifest-tool compliant yaml file to
  build a [target] on the given multiarch [platforms]. *)
