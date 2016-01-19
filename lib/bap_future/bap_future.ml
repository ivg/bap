open Core_kernel.Std

module Std = struct

  module Promise = struct
    type 'a t = {
      made: 'a -> unit;
      filled: bool ref;
    }

    let create made = {made; filled = ref false;}

    let fulfill p x = 
      p.made x;
      p.filled := true

    let is_fulfilled p = !(p.filled)
  end

  type 'a promise = 'a Promise.t

  module Future = struct
    type 'a waiters = {
      mutable waiters : ('a -> unit) list
    }

    type direct = [`full   | `wait ]
    type linked = [ direct | `link ]

    type 'a t = ('a,linked) future
    and ('a,'s) future = { mutable cell : ('a,'s) cell }
    and ('a,_) cell =
      | Full : 'a         -> ('a,[> `full]) cell
      | Wait : 'a waiters -> ('a,[> `wait]) cell
      | Link : 'a t       -> ('a,[> `link]) cell

    let full x = {cell = Full x}

    let rec unlink future : ('a,direct) cell = match future.cell with
      | Full _ as c -> c
      | Wait _ as c -> c
      | Link f -> unlink f

    let cell f = unlink f

    let decide v x = match cell v with
      | Full _ -> invalid_arg "future is already decided"
      | Wait {waiters} ->
        v.cell <- Full x;
        List.iter waiters ~f:(fun f -> f x)


    let empty () = {cell = Wait {waiters=[]}}

    let create () =
      let future = empty () in
      future, Promise.create (decide future)

    let upon v f = match cell v with
      | Full x -> f x
      | Wait w -> w.waiters <- f :: w.waiters

    let rec link t t' =
      match t.cell, t'.cell with
      | Link t,_  -> link t t'
      | _,Link t' -> link t t'
      | _,Full s -> t.cell <- Full s
      | Full x,_ -> decide t' x
      | Wait wx, Wait wy ->
        wx.waiters <- (wx.waiters @ wy.waiters);
        t'.cell <- Link t

    let bind v f : 'b t = match cell v with
      | Full x -> f x
      | Wait w ->
        let u = empty () in
        w.waiters <- (fun x -> link u (f x)) :: w.waiters;
        u

    let map v ~f = match cell v with
      | Full x -> full (f x)
      | Wait w ->
        let u = empty () in
        w.waiters <- (fun x -> link u (full (f x))) :: w.waiters;
        u

    let is_decided = function
      | {cell=Full _} -> true
      | _ -> false

    let peek v = match cell v with
      | Full x -> Some x
      | Wait _ -> None

    let peek_exn v = match cell v with
      | Full x -> x
      | Wait _ -> invalid_arg "peek_exn: empty future"

    module Monad = Monad.Make(struct
        type nonrec 'a t = 'a t
        let bind = bind
        let map = `Custom map
        let return = full
      end)

    module App = Applicative.Of_monad(struct
        type nonrec 'a t = 'a t
        include Monad
      end)

    module Args = Applicative.Make_args(struct
        type nonrec 'a t = 'a t
        include App
      end)
    include App
    include Monad
  end

  type 'a future = 'a Future.t

  module Channel = struct
    type 'a t = 'a -> unit
  end

  type 'a channel = 'a Channel.t

  type 'a signal = Signal of ('a -> unit)

  module Signal = struct
    type 'a t = 'a signal
    let send (Signal send) x = send x

    let repeat (Signal send) ~times x = 
      let rec repeat' cnt = 
        if cnt < times then (
          send x; 
          repeat' (cnt + 1)) in
      repeat' 0

  end

  module Stream = struct
    module Id = Int64
    type id = Id.t


    type 'a t = {
      subs : (id -> 'a -> unit) Id.Table.t;
      mutable last_id : id;
      mutable on_subs: (id -> unit) list;
      mutable on_unsubs: (id -> unit) list;
      mutable waiters : (unit -> unit) list;
    }

    let add t f =
      t.last_id <- Id.succ t.last_id;
      Hashtbl.add_exn t.subs ~key:t.last_id ~data:f;
      List.iter t.on_subs ~f:(fun f -> f t.last_id);
      t.last_id

    let subscribe t f =
      let f id x = f x in
      add t f

    let watch s f = ignore (add s f)

    let observe s f =
      let f id x = f x in
      ignore (add s f)

    let unsubscribe t id =
      Hashtbl.remove t.subs id;
      List.iter t.on_unsubs ~f:(fun f -> f id)

    let publish t event =
      Hashtbl.iter t.subs ~f:(fun ~key:id ~data:notify ->
          try notify id event with exn ->
            unsubscribe t id;
            raise exn)

    let create () =
      let stream = {
        subs = Id.Table.create ();
        last_id = Id.zero;
        on_subs = [];
        on_unsubs = [];
        waiters = []
      } in
      stream, Signal (publish stream)

    let wait t =
      List.iter t.waiters ~f:(fun f -> f ())

    let on_wait t f =
      t.waiters <- f :: t.waiters

    let on_subscribe t f =
      t.on_subs <- f :: t.on_subs

    let on_unsubscribe t f =
      t.on_unsubs <- f :: t.on_unsubs

    let is_closed t = Hashtbl.is_empty t.subs

    let has_subscribers t = not (is_closed t)

    let unfold' ~init ~f =
      let stream,Signal push = create () in
      let state = ref init in
      let send () =
        let q,s = f !state in
        state := s;
        Queue.iter q ~f:push in
      stream, Signal send

    let unfold ~init ~f =
      let stream,Signal publish = create () in
      let state = ref init in
      let send () =
        let x,s = f !state in
        state := s;
        publish x in
      stream, Signal send

    let unfold_until ~init ~f =
      let stream,Signal publish = create () in
      let finish, promise  = Future.create () in
      let state = ref init in
      let terminated = ref false in
      let send () = 
        if not !terminated then           
          match f !state with
          | None -> 
            terminated := true;
            Promise.fulfill promise ()
          | Some (x,s) ->
            state := s;
            publish x in
      stream, Signal send, finish
     
    let from f =
      let stream,Signal send = create () in
      let send () = f () |> send in
      stream, Signal send

    let link s t f =
      let k = ref None in
      on_subscribe t (fun _ -> match !k with
          | Some _ -> ()
          | None -> k := Some (subscribe s f));
      on_unsubscribe t (fun _ ->
          if not (has_subscribers t) then match !k with
            | None -> ()
            | Some key ->
              unsubscribe s key;
              k := None);
      on_wait t (fun () -> wait s)

    let map' s ~f =
      let t,Signal publish = create () in
      let rec push q = match Queue.dequeue q with
        | None -> ()
        | Some x -> publish x; push q in
      let step x = push (f x) in
      link s t step;
      t

    let filter_map s ~f =
      let empty = Queue.create () in
      map' s ~f:(fun x -> match f x with
          | None -> empty
          | Some x -> Queue.singleton x)

    let map s ~f =
      map' s ~f:(fun x -> Queue.singleton (f x))

    let filter s ~f =
      filter_map s ~f:(fun x -> Option.some_if (f x) x)

    let ratio x y = let x,y = max x y, min x y in x / y

    let merge s1 s2 ~f =
      let s, Signal publish  = create () in
      let capacity = 4096 in
      let q1 = Queue.create ~capacity () in
      let q2 = Queue.create ~capacity () in
      let drop () =
        let drop q = ignore (Queue.dequeue_exn q) in
        drop q1; drop q2 in
      let step src q x =
        Queue.enqueue q x;
        let rec process () =
          match Queue.(peek q1, peek q2) with
          | Some x, Some y ->
            publish (f x y);
            drop ();
            process ()
          | _ -> () in
        process ();
        let len,l1,l2 = Queue.(length q, length q1, length q2) in
        if Queue.capacity q = len then wait src;
        if l1 > capacity && l2 > capacity && ratio l1 l2 > 2 &&
           (len > l1 || len > l2) then  wait src in
      link s1 s (step s1 q1);
      link s2 s (step s1 q2);
      s

    let once s =
      let s',Signal publish = create () in
      let k = ref None in
      on_subscribe s' (fun _ -> match !k with
          | Some _ -> ()
          | None -> k := Some (subscribe s (fun x ->
              publish x;
              Option.iter !k ~f:(unsubscribe s);
              k := None)));
      on_unsubscribe s' (fun _ -> match !k with
          | None -> ()
          | Some k' -> 
            unsubscribe s k'; 
            k := None);
      on_wait s' (fun () -> wait s);
      s'

    let parse s ~init ~f =
      let s',Signal publish = create () in
      let state = ref init in
      let step x =
        let x,s = f !state x in
        state := s;
        Option.iter x ~f:publish in
      link s s' step;
      s'

    let listen x f = ignore (subscribe x f)

    let of_list xs = unfold_until ~init:xs ~f:(function
        | [] -> None
        | x::xs -> Some (x, xs))

    let of_array xs =
      let n = Array.length xs in
      unfold_until ~init:0 ~f:(fun i ->
          if i < n then Some (xs.(i), (i + 1))
          else None)

    let of_sequence xs = unfold_until ~init:xs ~f:Sequence.next

    let zip = merge ~f:(fun x y -> x,y)

    let concat_map s ~f =
      let t,Signal push = create () in
      link s t (fun x ->
          observe (f x) push);
      t

    let repeat x = from (fun () -> x)

    let concat s = concat_map s ~f:ident

    let sync ~clk dat =
      let buf = Queue.create  () in
      let stream, Signal push = create () in
      link dat stream (fun x -> Queue.enqueue buf x);
      link clk stream (fun () ->
          let xs = Queue.to_list buf in
          Queue.clear buf;
          push xs);
      stream

    let apply fs xs = merge fs xs ~f:(fun f x -> f x)

    let upon finished s : 'a future =
      let result = ref None in
      observe s (fun x -> result := Some x);
      let future,promise = Future.create () in
      Future.upon finished (fun () -> match !result with
          | None -> ()
          | Some x -> Promise.fulfill promise x);
      future

    let before event xs = 
      let future, promise = Future.create () in
      let buf = Queue.create () in     
      let id = subscribe xs (Queue.enqueue buf) in
      Future.upon event
        (fun () -> 
           unsubscribe xs id;
           Promise.fulfill promise (Queue.to_list buf)); 
      future

    let last_before e xs n =
      let buf = Queue.create () in
      let add_value x = 
        if Queue.length buf >= n then 
          ignore (Queue.dequeue buf);
        Queue.enqueue buf x in
      let id = subscribe xs add_value in
      let future, promise = Future.create () in
      Future.upon e 
        (fun () -> 
           unsubscribe xs id;
           Promise.fulfill promise (Queue.to_list buf));
      future

    let take xs n = 
      let buf = Queue.create () in 
      let future, promise = Future.create () in
      let add_value id x = 
        if Queue.length buf < n then Queue.enqueue buf x;
        if Queue.length buf = n then
          (unsubscribe xs id;
           Promise.fulfill promise (Queue.to_list buf)) in
      watch xs add_value;
      future

    let nth xs n = 
      let cnt = ref 0 in
      let future, promise = Future.create () in
      let f id x = 
        if !cnt = n then 
          (unsubscribe xs id;
           Promise.fulfill promise x);
        cnt := Int.succ !cnt; in
      watch xs f;
      future
      
    let find xs ~f = 
      let future, promise = Future.create () in
      let f id x = 
        if f x then 
          (unsubscribe xs id;
           Promise.fulfill promise x) in
      watch xs f;
      future

    let find_map xs ~f = 
      let future, promise = Future.create () in
      let f id x = match f x with
        | None -> ()
        | Some y ->
          unsubscribe xs id;
          Promise.fulfill promise y in
      watch xs f;
      future

    let hd xs = 
      let future, promise = Future.create () in
      let f id x = 
        unsubscribe xs id;
        Promise.fulfill promise x in
      watch xs f;
      future

    let tl xs =
      let s, Signal publish = create () in
      let ign = ref true in
      let f x = 
        if !ign then ign := false
        else publish x in
      link xs s f;
      s

    let foldw ?(stride = 1) xs width ~init ~f = 
      let s, Signal publish = create () in
      let nxt = ref None in
      let res = ref init in
      let cnt = ref Int.zero in
      let try_push x = 
        cnt := Int.succ !cnt;
        res := f !res x;
        if !cnt = width then
          (cnt := Int.zero;
           nxt := Some Int.zero;
           publish !res;
           res := init) in
      let f x = match !nxt with 
        | None -> try_push x
        | Some nxt' ->
          let nxt'' = Int.succ nxt' in
          if nxt'' = stride then
            (nxt := None;
             try_push x) 
          else nxt := Some nxt'' in
      link xs s f;
      s

    let split xs ~f = 
      let s, Signal push = create () in
      let s', Signal push' = create () in
      link xs s (fun x -> push (fst (f x)));
      link xs s' (fun x -> push' (snd (f x)));
      s,s'

    let unzip xs = split xs ~f:ident

    let frame ~clk dat ~init ~f = 
      let s = sync ~clk dat in
      let s', Signal push = create () in
      let f xs = push (List.fold_left ~init ~f xs) in
      link s s' f;
      s'

    let sample ~clk dat = 
      let s = sync ~clk dat in
      let s', Signal push = create () in
      let f = function
        | [] -> push None
        | hd :: _ -> push (Some hd) in
      link s s' f;
      s'

  end

  type 'a stream = 'a Stream.t
end
