type date = int * int * int;

fun is_older((y1 : int, m1 : int, d1 : int), (y2 : int, m2 : int, d2 : int)) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else if d1 < d2 then true
  else false;

fun number_in_month(dates : date list, month : int) =
  if null dates then 0
  else
    let val r_num = number_in_month(tl dates, month);
        val d = hd dates
    in
       if #2 d = month then 1 + r_num
       else r_num
    end;

fun number_in_months(dates : date list, months : int list) =
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months);

fun dates_in_month(dates : date list, month : int) =
  if null dates then []
  else
    let val r_list = dates_in_month(tl dates, month);
        val d = hd dates
    in
       if #2 d = month then d :: r_list
       else r_list
    end;

fun dates_in_months(dates : date list, months : int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months);

fun get_nth(xs : string list, n : int) =
  if n = 1 then hd xs
  else get_nth(tl xs, n - 1);

fun date_to_string((y : int, m : int, d : int)) =
  let val month_list = ["January", "February", "March", "April", "May",
                        "June", "July", "August", "September", "October",
                        "November", "December"]
  in
    get_nth(month_list, m) ^ " " ^ (Int.toString d) ^ ", " ^ (Int.toString y)
  end;

fun number_before_reaching_sum(sum : int, xs : int list) =
  if sum <= hd xs then 0
  else 1 + number_before_reaching_sum(sum - (hd xs), tl xs);

fun what_month(day : int) =
  let val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_of_months) + 1
  end;

fun month_range(day1 : int, day2 : int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2);

fun oldest(dates : date list) =
  if null dates then NONE
  else
    let val oldest_rem = oldest(tl dates);
        val d = hd dates;
    in
      if (isSome oldest_rem andalso is_older(d, valOf oldest_rem)) orelse
         (not (isSome oldest_rem))
      then SOME d
      else oldest_rem
    end;

(* challenge problems *)

fun remove_duplicate(xs : int list) =
  let fun remove(x : int, ys : int list) =
        if null ys then []
        else
          if x = hd ys then remove(x, tl ys)
          else (hd ys) :: remove(x, tl ys);
  in
    if null xs then []
    else (hd xs) :: remove_duplicate(remove(hd xs, xs))
  end;

fun number_in_months_challenge(dates : date list, months : int list) =
  number_in_months(dates, remove_duplicate months);

fun dates_in_months_challenge(dates : date list, months : int list) =
  dates_in_months(dates, remove_duplicate months);

fun reasonable_date((y : int, m : int, d : int)) =
  let val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
      fun get_nth(xs : int list, n : int) =
        if n = 1 then hd xs
        else get_nth(tl xs, n - 1);
      fun is_leap_year(y : int) =
        y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0);
  in
    if y <= 0 orelse m < 1 orelse m > 12 orelse d < 1 then false
    else if d <= get_nth(days_of_months, m) then true
    else if m = 2 andalso d = 29 andalso (is_leap_year y) then true
    else false
  end;
