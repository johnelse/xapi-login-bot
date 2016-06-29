open Lwt

let thread_count = ref 100
let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"
let json = ref false

let login_loop id rpc username password =
  lwt () = Lwt_io.printlf "Thread %4d starting" id in
  let rec login_loop' id rpc uname pwd attempt =
    lwt () = Lwt_io.printlf "Thread %4d attempt %8d" id attempt in
    lwt session =
      try_lwt
        Xen_api_lwt_unix.Session.login_with_password
          ~rpc
          ~uname:username
          ~pwd:password
          ~version:"1"
        >>= (fun session -> return (Some session))
      with _ -> return None
    in
    login_loop' id rpc username password (attempt + 1)
  in
  login_loop' id rpc username password 1

let make_thread_id_list max =
  let rec make_thread_id_list' max acc =
    if max <= 0 then acc
    else make_thread_id_list' (max - 1) (max :: acc)
  in
  make_thread_id_list' max []

let main thread_count json uri username password =
  let open Xen_api_lwt_unix in
  let rpc =
    if json
    then make_json uri
    else make uri
  in
  let thread_ids = make_thread_id_list thread_count in
  Lwt_list.iter_p
    (fun id -> login_loop id rpc username password)
    thread_ids

let () =
  Arg.parse [
    "-n", Arg.Set_int thread_count, (Printf.sprintf "Number of threads to use (default %d)" !thread_count);
    "-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
    "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
    "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
    "-j", Arg.Set json, (Printf.sprintf "Use jsonrpc rather than xmlrpc (default %b)" !json);
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
    "Tool for simulating a large number of parallel xapi login attempts";

  Lwt_main.run (main !thread_count !json !uri !username !password)
