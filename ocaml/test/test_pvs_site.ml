(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit
open Test_common

let name = "my_pvs_site"

let test_unlicensed () =
	let __context = make_test_database ~features:[] () in
	assert_raises
		Api_errors.(Server_error (license_restriction, ["PVS_proxy"]))
		(fun () -> Xapi_pvs_site.introduce ~__context ~name)

let test_introduce () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	assert_equal name (Db.PVS_site.get_name ~__context ~self:pvs_site);
	assert_equal [] (Db.PVS_site.get_cache_storage ~__context ~self:pvs_site)

let test_forget_ok () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	Xapi_pvs_site.forget ~__context ~self:pvs_site;
	assert_equal (Db.is_valid_ref __context pvs_site) false

let test_forget_stopped_proxy () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	let (_: API.ref_PVS_proxy) =
		make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:false () in
	Xapi_pvs_site.forget ~__context ~self:pvs_site;
	assert_equal (Db.is_valid_ref __context pvs_site) false

let test_forget_running_proxy () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	let pvs_proxy =
		make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
	assert_raises_api_error
		Api_errors.pvs_site_contains_running_proxies
		~args:[Ref.string_of pvs_proxy]
		(fun () -> Xapi_pvs_site.forget ~__context ~self:pvs_site)

let test_forget_server () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	let pvs_server = make_pvs_server ~__context ~site:pvs_site () in
	assert_raises_api_error
		Api_errors.pvs_site_contains_servers
		~args:[Ref.string_of pvs_server]
		(fun () -> Xapi_pvs_site.forget ~__context ~self:pvs_site)

let test_forget_running_proxy_and_server () =
	let __context = make_test_database () in
	let pvs_site = Xapi_pvs_site.introduce ~__context ~name in
	let pvs_proxy =
		make_pvs_proxy ~__context ~site:pvs_site ~currently_attached:true () in
	let (_: API.ref_PVS_server) = make_pvs_server ~__context ~site:pvs_site () in
	assert_raises_api_error
		Api_errors.pvs_site_contains_running_proxies
		~args:[Ref.string_of pvs_proxy]
		(fun () -> Xapi_pvs_site.forget ~__context ~self:pvs_site)

let test_add_local_sr () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let site      = XF.introduce ~__context ~name in
	let sr1       = make_sr ~__context ~shared:false () in
	let sr2       = make_sr ~__context ~shared:false () in
	let cache ()  = DF.get_cache_storage ~__context ~self:site in
	( XF.add_cache_storage ~__context ~self:site ~value:sr1
	; XF.add_cache_storage ~__context ~self:site ~value:sr2
	; assert_equal true (List.mem sr1 @@ cache ())
	; assert_equal true (List.mem sr2 @@ cache ())
	; assert_raises_api_error Api_errors.pvs_site_sr_already_added
		(fun () -> XF.add_cache_storage ~__context ~self:site ~value:sr1)
	; assert_raises_api_error Api_errors.pvs_site_sr_already_added
		(fun () -> XF.add_cache_storage ~__context ~self:site ~value:sr2)
	)

let test_add_shared_sr () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let site      = XF.introduce ~__context ~name in
	let sr1       = make_sr ~__context ~shared:true () in
	let cache ()  = DF.get_cache_storage ~__context ~self:site in
	( XF.add_cache_storage ~__context ~self:site ~value:sr1
	; assert_equal true (List.mem sr1 @@ cache ())
	; assert_raises_api_error Api_errors.pvs_site_sr_already_added
		(fun () -> XF.add_cache_storage ~__context ~self:site ~value:sr1)
	)

let test_add_mixed_sr () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let site      = XF.introduce ~__context ~name in
	let cache ()  = DF.get_cache_storage ~__context ~self:site in
	let sr1       = make_sr ~__context ~shared:true  () in
	let sr2       = make_sr ~__context ~shared:false () in
	( XF.add_cache_storage ~__context ~self:site ~value:sr1
	; XF.add_cache_storage ~__context ~self:site ~value:sr2
	; assert_equal true (List.mem sr1 @@ cache ())
	; assert_equal true (List.mem sr2 @@ cache ())
	; assert_raises_api_error Api_errors.pvs_site_sr_already_added
		(fun () -> XF.add_cache_storage ~__context ~self:site ~value:sr1)
	; assert_raises_api_error Api_errors.pvs_site_sr_already_added
		(fun () -> XF.add_cache_storage ~__context ~self:site ~value:sr2)
	)

let test_remove_local_sr () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let site      = XF.introduce ~__context ~name in
	let sr1       = make_sr ~__context ~shared:false () in
	let sr2       = make_sr ~__context ~shared:false () in
	let sr3       = make_sr ~__context ~shared:false () in
	let cache ()  = DF.get_cache_storage ~__context ~self:site in
		( XF.add_cache_storage ~__context ~self:site ~value:sr1
		; XF.add_cache_storage ~__context ~self:site ~value:sr2
		; assert_equal true  (List.mem sr1 @@ cache ())
		; assert_equal true  (List.mem sr2 @@ cache ())
		; assert_equal false (List.mem sr3 @@ cache ())
		; assert_raises_api_error Api_errors.sr_not_in_pvs_site
			(fun () -> XF.remove_cache_storage ~__context ~self:site ~value:sr3)
		; XF.remove_cache_storage ~__context ~self:site ~value:sr1
		; assert_equal true (List.mem sr2 @@ cache ())
		; XF.remove_cache_storage ~__context ~self:site ~value:sr2
		; assert_equal [] (cache ())
		; assert_raises_api_error Api_errors.sr_not_in_pvs_site
			(fun () -> XF.remove_cache_storage ~__context ~self:site ~value:sr2)
		; assert_raises_api_error Api_errors.sr_not_in_pvs_site
			(fun () -> XF.remove_cache_storage ~__context ~self:site ~value:sr1)
		)

let test_remove_shared_sr () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let site      = XF.introduce ~__context ~name in
	let sr1       = make_sr ~__context ~shared:true () in
	let sr2       = make_sr ~__context ~shared:true () in
	let cache ()  = DF.get_cache_storage ~__context ~self:site in
		( XF.add_cache_storage ~__context ~self:site ~value:sr1
		; assert_equal true (List.mem sr1 @@ cache ())
		; assert_raises_api_error Api_errors.sr_not_in_pvs_site
			(fun () -> XF.remove_cache_storage ~__context ~self:site ~value:sr2)
		; XF.remove_cache_storage ~__context ~self:site ~value:sr1
		; assert_equal [] (cache ())
		; assert_raises_api_error Api_errors.sr_not_in_pvs_site
			(fun () -> XF.remove_cache_storage ~__context ~self:site ~value:sr1)
		)

let test_set_name () =
	let module XF = Xapi_pvs_site in
	let module DF = Db.PVS_site  in
	let __context = make_test_database () in
	let name1     = "name1" in
	let name2     = "name2" in
	let site      = XF.introduce ~__context ~name:name1 in
		( assert_equal name1 (DF.get_name ~__context ~self:site)
		; XF.set_name ~__context ~self:site ~value:name2
		; assert_equal name2 (DF.get_name ~__context ~self:site)
		; ignore@@make_pvs_proxy ~__context ~site:site ~currently_attached:true ()
		; assert_raises_api_error Api_errors.pvs_site_contains_running_proxies
			(fun () -> XF.set_name ~__context ~self:site ~value:name1)
		)


let test =
	"test_pvs_site" >:::
		[
			"test_unlicensed" >:: test_unlicensed;
			"test_introduce" >:: test_introduce;
			"test_forget_ok" >:: test_forget_ok;
			"test_forget_stopped_proxy" >:: test_forget_stopped_proxy;
			"test_forget_running_proxy" >:: test_forget_running_proxy;
			"test_forget_server" >:: test_forget_server;
			"test_forget_running_proxy_and_server" >::
				test_forget_running_proxy_and_server;

			"test_add_local_sr"     >:: test_add_local_sr;
			"test_add_shared_sr"    >:: test_add_shared_sr;
			"test_add_mixed_sr"     >:: test_add_mixed_sr;
			"test_remove_local_sr"  >:: test_remove_local_sr;
			"test_remove_shared_sr" >:: test_remove_shared_sr;
			"test_set_name"         >:: test_set_name;

		]