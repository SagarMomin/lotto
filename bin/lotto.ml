open! Core
open! Async

module Common_options = struct
  open Command.Spec

  let tickets_file =
    flag
      "ticket-file"
      (required Filename.arg_type)
      ~doc:
        "[FILE] A csv like file that has contains the all the numbers: in the form of\n\n\
        \        num,num,num,num,num,num\n\n\
        \        num,num,num,num,num,num"
  ;;

  let winning_ticket =
    flag
      "winning-number"
      (required string)
      ~doc:
        "[STRING] Winning number as a string in the form of:\n\n\
        \     num,num,num,num,num,num"
  ;;
end

module Lottery = struct
  module Ticket = struct
    type t =
      { white_balls : Int.Set.t
      ; magic : int
      }

    let t_of_int_list l =
      { white_balls = List.slice l 0 (-1) |> Int.Set.of_list; magic = List.last_exn l }
    ;;

    let to_string { white_balls; magic } =
      sprintf
        "%s,%d"
        (Set.to_list white_balls
        |> List.map ~f:Int.to_string_hum
        |> String.concat ~sep:",")
        magic
    ;;

    let load_file ticket_file =
      In_channel.read_lines ticket_file
      |> List.map ~f:(String.split ~on:',')
      |> List.filter_map ~f:(fun ticket ->
             if List.length ticket <> 6
             then (
               printf "Invalid ticket %s\n:" @@ String.concat ~sep:"," ticket;
               None)
             else Some (List.map ticket ~f:Int.of_string))
      |> List.map ~f:t_of_int_list
    ;;

    let see_what_matches ~(winning_ticket : t) (t : t) =
      let { white_balls; magic } = t in
      let { white_balls = winning_white_balls; magic = winning_magic } = winning_ticket in
      let num_match = Set.inter white_balls winning_white_balls |> Set.length in
      let does_magic_match = magic = winning_magic in
      t, num_match, does_magic_match
    ;;
  end

  type t =
    | Megamillions
    | Powerball

  let calculate_winnings ~t (ticket, white_balls_matched, magic_ball) =
    let winnings =
      match t with
      | Megamillions ->
        (match magic_ball, white_balls_matched with
        | true, 5 -> 99999999999
        | true, 4 -> 10_000
        | true, 3 -> 200
        | true, 2 -> 10
        | true, 1 -> 4
        | true, 0 -> 2
        | false, 5 -> 1_000_000
        | false, 4 -> 500
        | false, 3 -> 10
        | _ -> 0)
      | Powerball ->
        (match magic_ball, white_balls_matched with
        | true, 5 -> 99999999999
        | true, 4 -> 50_000
        | true, 3 -> 100
        | true, 2 -> 7
        | true, 1 -> 4
        | true, 0 -> 4
        | false, 5 -> 1_000_000
        | false, 4 -> 100
        | false, 3 -> 7
        | _ -> 0)
    in
    if winnings > 0 then Some (ticket, winnings) else None
  ;;
end

let find_winners ~ticket_file winning_ticket =
  let tickets = Lottery.Ticket.load_file ticket_file in
  let winning_ticket =
    String.split ~on:',' winning_ticket
    |> List.map ~f:Int.of_string
    |> Lottery.Ticket.t_of_int_list
  in
  List.map tickets ~f:(Lottery.Ticket.see_what_matches ~winning_ticket)
;;

let check_tickets ~lottery ~ticket_file ~winning_ticket =
  let winners =
    List.filter_map
      (find_winners ~ticket_file winning_ticket)
      ~f:(Lottery.calculate_winnings ~t:lottery)
  in
  List.iter winners ~f:(fun (ticket, money) ->
      printf !"WOW! %{Lottery.Ticket} just won you $%{Int}\n" ticket money);
  List.unzip winners
  |> snd
  |> List.fold ~init:0 ~f:Int.( + )
  |> printf "For a grand total of $%d!\nYou're rich!\n"
;;

let lotto ~lottery =
  Command.async
    ~summary:"Check lotto tickets and see if there is a winner"
    (let open Command.Let_syntax in
    let%map_open ticket_file = Common_options.tickets_file
    and winning_ticket = Common_options.winning_ticket in
    fun () ->
      check_tickets ~lottery ~ticket_file ~winning_ticket;
      Deferred.unit)
;;

let command =
  Command.group
    ~summary:"Check to see how much money was won in the lotery"
    [ "megamillions", lotto ~lottery:Megamillions; "powerball", lotto ~lottery:Powerball ]
;;

let () = Command.run command
