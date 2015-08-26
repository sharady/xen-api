(*
 * Copyright (C) 2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Check the VDI read write support for rrd-stats vdi *)

open Client
open Quicktest_common

let start session_id sr =

	let t = make_test "Check VDI.copy delta handling" 1 in
	start t;

	(* Pattern to write on the vdi *)
	let pattern = [(100,'a'); (200,'b'); (300,'c'); (500,'d'); (800,'e'); (1300,'a'); (896,'c');]
	in

	(* Create a 4 MiB disk on src_sr *)
	let vdi = Client.VDI.create ~rpc:!rpc ~session_id ~name_label:"quicktest rrd stats VDI"
		~name_description:"Used by the rrd-stats VDI write/read test"
		~sR:sr ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
		~_type:`rrd ~sharable:false ~read_only:false
		~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in

	debug t "Created a 4MiB test disk";
	(* Write the pattern string on the vdi *)
	let write_pattern size patt =
		let data = String.make (1024 * size) patt in
		Client.VDI.write_rrd ~rpc:!rpc ~session_id ~sr:sr ~vdi:vdi ~text:data
	in

	List.iter (fun (a, b) -> write_pattern a b) pattern;
	debug t "Pattern written on 4MiB test disk";

	(* Read the pattern on the vdi *)
	let index = ref 0 in
	let read_pattern text size patt =
		for i = !index to ((size * 1024 * 1024) - 1) do
			if text.[i] <> patt then begin
				let msg = Printf.sprintf "VDI offset %d has %c: expected %c" i text.[i] patt in
				failed t msg;
				failwith msg
			end;
			incr index
		done;
	in

	let pattern_retrieved = Client.VDI.read_rrd ~rpc:!rpc ~session_id ~sr:sr ~vdi:vdi in
	List.iter (fun (a, b) -> read_pattern pattern_retrieved a b) pattern;
	debug t "Pattern read from 4MiB test disk";
	Client.VDI.destroy ~rpc:!rpc ~session_id ~self:vdi;

success t

