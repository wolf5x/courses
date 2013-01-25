(* problem 1 *)
fun is_older(day1 : int*int*int, day2 : int*int*int) = 
  if #1 day1 <> #1 day2 then #1 day1 < #1 day2
  else if #2 day1 <> #2 day2 then #2 day1 < #2 day2
  else  #3 day1 < #3 day2

(* problem 2 *)
fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates then 0
  else if #2 (hd dates) = month then 1+number_in_month(tl dates, month)
  else number_in_month(tl dates, month)

(* problem 3 *)
fun number_in_months(dates : (int*int*int) list, months : int list) = 
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* problem 4 *)
fun dates_in_month(dates : (int*int*int) list, month : int) = 
  if null dates then []
  else if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

(* problem 5 *)
fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* problem 6 *)
fun get_nth(strs : string list, n : int) = 
  if n = 1 then hd strs
  else get_nth(tl strs, n-1)

(* problem 7 *)
fun date_to_string(date : int*int*int) = 
  let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* problem 8 *)
fun number_before_reaching_sum(sum : int, numbers : int list) = 
  if null numbers orelse sum <= hd numbers then 0
  else 1+number_before_reaching_sum(sum-(hd numbers), tl numbers)

(* problem 9 *)
fun what_month(day : int) =
  let val days_of_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in 1+number_before_reaching_sum(day, days_of_months)
  end

(* problem 10 *)
fun month_range(day1 : int, day2 : int) = 
  if day1 > day2 then []
  else what_month(day1)::month_range(day1+1, day2)

(* problem 11 *)
fun oldest(dates : (int*int*int) list) = 
  if null dates then NONE
  else 
    let val tl_ans = oldest(tl dates)
    in 
      if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
      else SOME (hd dates)
    end

(* challenge problem 12 *)
(* delete elems equal to number in lst *)
fun delete(lst : int list, number : int) = 
  if null lst then []
  else if number = hd lst then delete(tl lst, number)
  else (hd lst)::delete(tl lst, number)

(* remove duplicate elems in lst *)
fun unique(lst : int list) = 
  if null lst then []
  else (hd lst)::unique(delete(tl lst, hd lst))

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
  number_in_months(dates, unique(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
  dates_in_months(dates, unique(months))
  
(* challenge problem 13 *)
fun reasonable_date(date : int*int*int) = 
  let 
    val days_of_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    fun is_leap_year(year : int) = 
      year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    fun get_nth(lst : int list, n : int) = 
      if n = 1 then hd lst
      else get_nth(tl lst, n-1)
  in
    if #1 date <= 0 then false
    else if #2 date < 1 orelse #2 date > 12 then false
    else if #3 date < 1 then false
    else if #2 date = 2 andalso is_leap_year(#1 date) then #3 date <= 29
    else #3 date <= get_nth(days_of_months, #2 date)
  end
  
