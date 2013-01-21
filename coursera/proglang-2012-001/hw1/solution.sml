fun is_older(day1 : int*int*int, day2 : int*int*int) = 
  if #1 day1 <> #1 day2 then #1 day1 < #1 day2
  else if #2 day1 <> #2 day2 then #2 day1 < #2 day2
  else  #3 day1 < #3 day2

fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates then 0
  else if #2 (hd dates) = month then 1+number_in_month(tl dates, month)
  else number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) = 
  if null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) = 
  if null dates then []
  else if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months then []
  else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

fun get_nth(strs : string list, n : int) = 
  if null strs orelse n <= 0 then "" (* If strs has less than n elements then return null *)
  else if n = 1 then hd strs
  else get_nth(tl strs, n-1)

fun date_to_string(date : int*int*int) = 
  let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) = 
  if null numbers orelse sum <= hd numbers then 0
  else 1+number_before_reaching_sum(sum-(hd numbers), tl numbers)

fun what_month(day : int) =
  let val days_of_months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum(day-1, days_of_months)
  end
 
fun month_range(day1 : int, day2 : int) = 
  if day1 > day2 then []
  else what_month(day1)::month_range(day1+1, day2)

fun oldest(dates : (int*int*int) list) = 
  if null dates then NONE
  else 
    let val tl_ans = oldest(tl dates)
    in 
      if isSome tl_ans andalso is_older(valueOf tl_ans, hd dates) then tl_ans
      else SOME (hd dates)
    end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =

  




  
