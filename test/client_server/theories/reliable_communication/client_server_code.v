(* This file is automatically generated from the OCaml source file
<repository_root>/ml_sources/reliable_communication/client_server_code.ml *)

From aneris.aneris_lang Require Import ast.
From aneris.aneris_lang.lib Require Import queue_code.
From aneris.aneris_lang.lib Require Import map_code.
From aneris.aneris_lang.lib.serialization Require Import serialization_code.
From aneris.aneris_lang.lib Require Import network_util_code.

Definition tag_int_ser := prod_serializer string_serializer int_serializer.

Definition idmsg_ser ser :=
  sum_serializer int_serializer (prod_serializer int_serializer ser).

Definition gen_msg_ser ser := sum_serializer tag_int_ser (idmsg_ser ser).

(**  ********************** CLIENT AND SERVER SOCKETS *********************** * *)

(**  UDP socket bundled together with serialization.
    This should be useful to avoid propagation of serialization as
    a parameter to objects using sockets, since any object containing
    the socket would already have access to serialization.
    Note also that the creation and binding of the socket is merged into
    one step. * *)

Definition make_skt ser deser : val :=
  λ: "sa",
  let: "sh" := NewSocket #PF_INET #SOCK_DGRAM #IPPROTO_UDP in
  SocketBind "sh" "sa";;
  ("sh", ((s_ser (gen_msg_ser ser)), (s_deser (gen_msg_ser deser))), (s_ser ser)).

(**  Client socket is just a alias of skt, with purpose to make the API
    more clear w.r. to distinction with server_socket (passive socket).  *)

Definition make_client_skt ser deser : val :=
  λ: "sa", make_skt ser deser "sa".

(**  Server socket carries data  (client-addr table, established connection queue)
    that are usually in charge of kernel, but here are simulated on top of UDP  *)

Definition make_server_skt ser deser : val :=
  λ: "sa", let: "skt" := make_skt ser deser "sa" in
            ref (InjL "skt").

Definition make_new_channel_descr : val :=
  λ: "ser",
  let: "sbuf" := ref ((queue_empty #()), #0) in
  let: "rbuf" := ref (queue_empty #()) in
  let: "slk" := newlock #() in
  let: "rlk" := newlock #() in
  ("sbuf", "slk", ("rbuf", "rlk"), ref "ser").

(**  *********************** AUXIALIARY FUNCTIONS *************************** * *)

Definition send_from_chan_loop : val :=
  λ: "skt" "sa" "c",
  let: "sdata" := Fst (Fst "c") in
  let: "sbuf" := Fst "sdata" in
  let: "slk" := Snd "sdata" in
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "_deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  let: "send_msg" := λ: "m",
  let: "msg" := "ser" (InjR (InjR "m")) in
  SendTo "sh" "msg" "sa";;
  #() in
  letrec: "loop" <> :=
    #() (* unsafe (fun () -> Unix.sleepf 0.5); *);;
    acquire "slk";;
    queue_iter "send_msg" (Fst ! "sbuf");;
    release "slk";;
    "loop" #() in
    "loop" #().

Definition prune_sendbuf_at_ack : val :=
  λ: "slk" "sidLBloc" "sendbuf" "msg_ack",
  let: "sidLB" := ! "sidLBloc" in
  (if: "msg_ack" ≤ "sidLB"
   then  #()
   else
     acquire "slk";;
     let: "p" := ! "sendbuf" in
     let: "qe" := Fst "p" in
     let: "ub" := Snd "p" in
     "sidLBloc" <- "msg_ack";;
     "sendbuf" <- ((queue_drop "qe" ("msg_ack" - "sidLB")), "ub");;
     release "slk").

Definition process_data_on_chan : val :=
  λ: "skt" "sa" "sidLB" "ackId" "c" "msg",
  let: "cdata" := Fst "c" in
  let: "sbuf" := Fst (Fst "cdata") in
  let: "slk" := Snd (Fst "cdata") in
  let: "rbuf" := Fst (Snd "cdata") in
  let: "rlk" := Snd (Snd "cdata") in
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "_deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  let: "ackid" := ! "ackId" in
  match: "msg" with
    InjL "id" => prune_sendbuf_at_ack "slk" "sidLB" "sbuf" "id"
  | InjR "cmsg" =>
      let: "mid" := Fst "cmsg" in
      let: "mbody" := Snd "cmsg" in
      let: "<>" := (if: "mid" = "ackid"
       then
         acquire "rlk";;
         "rbuf" <- (queue_add ("mid", "mbody") ! "rbuf");;
         "ackId" <- ("mid" + #1);;
         release "rlk"
       else  #()) in
      let: "msg_ack" := "ser" (InjR (InjL ! "ackId")) in
      SendTo "sh" "msg_ack" "sa";;
      #()
  end.

Definition client_recv_on_chan_loop : val :=
  λ: "skt" "sa" "sidLB" "ackId" "c",
  let: "sh" := Fst (Fst "skt") in
  let: "_ser" := Fst (Snd (Fst "skt")) in
  let: "deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  letrec: "loop" <> :=
    let: "msg" := unSOME (ReceiveFrom "sh") in
    assert: ("sa" = (Snd "msg"));;
    match: "deser" (Fst "msg") with
      InjL "_abs" => #()
    | InjR "sm" => process_data_on_chan "skt" "sa" "sidLB" "ackId" "c" "sm"
    end;;
    "loop" #() in
    "loop" #().

Definition client_conn_step : val :=
  λ: "skt" "req" "repl_tag" "saddr",
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  let: "req_msg" := "ser" (InjL "req") in
  SendTo "sh" "req_msg" "saddr";;
  #();;
  letrec: "handler" "msg" "src" :=
    match: "deser" "msg" with
      InjL "repl" =>
      let: "tag" := Fst "repl" in
      let: "data" := Snd "repl" in
      (if: ("tag" = "repl_tag") && ("src" = "saddr")
       then  "data"
       else
         #() (* unsafe (fun () -> Unix.sleepf 0.5); *);;
         SendTo "sh" "req_msg" "saddr";;
         #();;
         listen "sh" "handler")
    | InjR "_sm" => listen "sh" "handler"
    end in
    listen "sh" "handler".

Definition server_conn_step_to_open_new_conn : val :=
  λ: "srv_skt" "bdy" "clt_addr",
  let: "skt" := Fst (Fst "srv_skt") in
  let: "connMap" := Snd (Fst "srv_skt") in
  let: "_chanQueue" := Fst (Snd "srv_skt") in
  let: "_connlk" := Snd (Snd "srv_skt") in
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "_deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  match: "bdy" with
    InjL "im" =>
    (if: ((Fst "im") = #"INIT") && ((Snd "im") = #0)
     then
       let: "cookie" := Rand #4294967296 in
       "connMap" <- (map_insert "clt_addr" (InjL "cookie") ! "connMap");;
       let: "init_ack" := "ser" (InjL (#"INIT-ACK", "cookie")) in
       SendTo "sh" "init_ack" "clt_addr";;
       #()
     else  assert: #false)
  | InjR "_abs" => assert: #false
  end.

Definition server_conn_step_to_establish_conn : val :=
  λ: "srv_skt" "cookie" "bdy" "clt_addr",
  let: "skt" := Fst (Fst "srv_skt") in
  let: "connMap" := Snd (Fst "srv_skt") in
  let: "chanQueue" := Fst (Snd "srv_skt") in
  let: "connlk" := Snd (Snd "srv_skt") in
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "_deser" := Snd (Snd (Fst "skt")) in
  let: "serf" := Snd "skt" in
  match: "bdy" with
    InjL "im" =>
    (if: ((Fst "im") = #"COOKIE") && ((Snd "im") = "cookie")
     then
       let: "sidLB" := ref #0 in
       let: "ackId" := ref #0 in
       let: "chan_descr" := make_new_channel_descr "serf" in
       Fork (send_from_chan_loop "skt" "clt_addr" "chan_descr");;
       "connMap" <- (map_insert "clt_addr"
                     (InjR ("chan_descr", "cookie", ("sidLB", "ackId")))
                     ! "connMap");;
       let: "cookie_ack" := "ser" (InjL (#"COOKIE-ACK", #0)) in
       SendTo "sh" "cookie_ack" "clt_addr";;
       #();;
       acquire "connlk";;
       "chanQueue" <- (queue_add ("chan_descr", "clt_addr") ! "chanQueue");;
       release "connlk"
     else
       (if: ((Fst "im") = #"INIT") && ((Snd "im") = #0)
       then
         let: "init_ack" := "ser" (InjL (#"INIT-ACK", "cookie")) in
         SendTo "sh" "init_ack" "clt_addr";;
         #()
       else  assert: #false))
  | InjR "_abs" => assert: #false
  end.

Definition server_conn_step_process_data : val :=
  λ: "srv_skt" "chan_data" "bdy" "clt_addr",
  let: "skt" := Fst (Fst "srv_skt") in
  let: "sh" := Fst (Fst "skt") in
  let: "ser" := Fst (Snd (Fst "skt")) in
  let: "_deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  let: "chan_descr" := Fst (Fst "chan_data") in
  let: "cookie" := Snd (Fst "chan_data") in
  let: "sidLB" := Fst (Snd "chan_data") in
  let: "ackId" := Snd (Snd "chan_data") in
  match: "bdy" with
    InjL "im" =>
    (if: ((Fst "im") = #"COOKIE") && ((Snd "im") = "cookie")
     then
       let: "cookie_ack" := "ser" (InjL (#"COOKIE-ACK", #0)) in
       SendTo "sh" "cookie_ack" "clt_addr";;
       #()
     else  #())
  | InjR "sm" =>
      process_data_on_chan "skt" "clt_addr" "sidLB" "ackId" "chan_descr" "sm"
  end.

(**  ******** ESTABLISHING CONNECTION (server_listen, accept, connect) ****** * *)

Definition server_recv_on_listening_skt_loop : val :=
  λ: "srv_skt",
  let: "skt" := Fst (Fst "srv_skt") in
  let: "connMap" := Snd (Fst "srv_skt") in
  let: "_conn_data" := Snd "srv_skt" in
  let: "sh" := Fst (Fst "skt") in
  let: "_ser" := Fst (Snd (Fst "skt")) in
  let: "deser" := Snd (Snd (Fst "skt")) in
  let: "_s" := Snd "skt" in
  letrec: "loop" <> :=
    let: "msg" := unSOME (ReceiveFrom "sh") in
    let: "m" := Fst "msg" in
    let: "clt_addr" := Snd "msg" in
    let: "bdy" := "deser" "m" in
    match: map_lookup "clt_addr" ! "connMap" with
      NONE => server_conn_step_to_open_new_conn "srv_skt" "bdy" "clt_addr"
    | SOME "data" =>
        match: "data" with
          InjL "cookie" =>
          server_conn_step_to_establish_conn "srv_skt" "cookie" "bdy"
          "clt_addr"
        | InjR "p" =>
            server_conn_step_process_data "srv_skt" "p" "bdy" "clt_addr"
        end
    end;;
    "loop" #() in
    "loop" #().

Definition server_listen : val :=
  λ: "srv_skt",
  match: ! "srv_skt" with
    InjL "skt" =>
    let: "connMap" := ref (map_empty #()) in
    let: "connQueue" := ref (queue_empty #()) in
    let: "connlk" := newlock #() in
    "srv_skt" <- (InjR ("connQueue", "connlk"));;
    let: "srv_skt_passive" := ("skt", "connMap", ("connQueue", "connlk")) in
    Fork (server_recv_on_listening_skt_loop "srv_skt_passive")
  | InjR "_queue" => assert: #false
  end.

Definition accept : val :=
  λ: "srv_skt",
  match: ! "srv_skt" with
    InjL "_skt" => assert: #false
  | InjR "queue" =>
      let: "chanQueue" := Fst "queue" in
      let: "connlk" := Snd "queue" in
      letrec: "aux" <> :=
        acquire "connlk";;
        (if: queue_is_empty ! "chanQueue"
         then
           #() (* unsafe (fun () -> Unix.sleepf 1.0); *);;
           release "connlk";;
           "aux" #()
         else
           let: "q" := unSOME (queue_take_opt ! "chanQueue") in
           let: "c" := Fst (Fst "q") in
           let: "clt_addr" := Snd (Fst "q") in
           let: "tl" := Snd "q" in
           "chanQueue" <- "tl";;
           #();;
           release "connlk";;
           ("c", "clt_addr")) in
        "aux" #()
  end.

Definition connect : val :=
  λ: "skt" "srv_addr",
  SetReceiveTimeout (Fst (Fst "skt")) #1 #0;;
  let: "cookie" := client_conn_step "skt" (#"INIT", #0) #"INIT-ACK"
                   "srv_addr" in
  let: "_ack" := client_conn_step "skt" (#"COOKIE", "cookie") #"COOKIE-ACK"
                 "srv_addr" in
  SetReceiveTimeout (Fst (Fst "skt")) #0 #0;;
  let: "sidLB" := ref #0 in
  let: "ackId" := ref #0 in
  let: "c" := make_new_channel_descr (Snd "skt") in
  Fork (send_from_chan_loop "skt" "srv_addr" "c");;
  Fork (client_recv_on_chan_loop "skt" "srv_addr" "sidLB" "ackId" "c");;
  "c".

(**  ******************* DATA TRANSFER (send, try_recv, recv) *************** * *)

Definition send : val :=
  λ: "c" "mbody",
  let: "sdata" := Fst (Fst "c") in
  let: "sbuf" := Fst "sdata" in
  let: "slk" := Snd "sdata" in
  acquire "slk";;
  let: "p" := ! "sbuf" in
  let: "qe" := Fst "p" in
  let: "ub" := Snd "p" in
  "sbuf" <- ((queue_add ("ub", "mbody") "qe"), ("ub" + #1));;
  release "slk".

Definition try_recv : val :=
  λ: "c",
  let: "rdata" := Snd (Fst "c") in
  let: "rbuf" := Fst "rdata" in
  let: "rlk" := Snd "rdata" in
  acquire "rlk";;
  let: "mo" := queue_take_opt ! "rbuf" in
  let: "res" := match: "mo" with
    NONE => NONE
  | SOME "p" =>
      let: "msg" := Fst "p" in
      let: "tl" := Snd "p" in
      "rbuf" <- "tl";;
      SOME (Snd "msg")
  end in
  release "rlk";;
  "res".

Definition recv : val :=
  λ: "c",
  letrec: "aux" <> :=
    match: try_recv "c" with
      NONE => "aux" #()
    | SOME "res" => "res"
    end in
    "aux" #().