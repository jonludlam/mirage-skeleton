
module Xs = Xs_client_lwt.Client(OS.Xs.IO)

module Main (C: V1_LWT.CONSOLE) = struct
  open Lwt

  let suspend c =
    Xs.make () >>= fun client -> 
    Xs.suspend client >>= fun _ ->  
    OS.Sched.suspend () >>= fun cancelled -> 
    C.log c (Printf.sprintf "cancelled=%d" cancelled);
    return cancelled

  let start c = 
    C.log_s c (Printf.sprintf "xs_watch ()") >>= fun () -> 
    Xs.make () >>= fun client -> 
    let rec inner () = 
      Xs.(immediate client (fun h -> directory h "control")) >>= fun dir -> 
      begin if List.mem "shutdown" dir then begin
        Xs.(immediate client (fun h -> read h "control/shutdown")) >>= fun msg ->
	C.log_s c (Printf.sprintf "Got control message: %s" msg) >>= fun () ->
	match msg with
	| "suspend" -> 
          Xs.(immediate client (fun h -> rm h "control/shutdown")) >>= fun _ -> 
          suspend c >>= fun _ -> 
          C.log_s c "About to read domid" >>= fun _ ->
          Xs.(immediate client (fun h -> read h "domid")) >>= fun domid -> 
          C.log_s c (Printf.sprintf "We're back: domid=%s" domid) >>= fun _ -> 
          return true
      | "poweroff" -> 
          OS.Sched.shutdown OS.Sched.Poweroff;
          return false (* Doesn't get here! *)
      | "reboot" ->
          OS.Sched.shutdown OS.Sched.Reboot;
          return false (* Doesn't get here! *)
      | "halt" ->
          OS.Sched.shutdown OS.Sched.Poweroff;
          return false
      | "crash" ->
          OS.Sched.shutdown OS.Sched.Crash;
          return false
      | _ -> 
          return false
      end else return false end >>= fun _ ->
      OS.Time.sleep 1.0 >>= fun _ ->
      inner ()
    in inner ()

end
