is_older((1,2,3), (1,2,3));
is_older((1,2,3), (1,2,2));
is_older((1,2,3), (1,2,4));
is_older((1,2,3), (1,1,3));
is_older((1,2,3), (1,3,3));
is_older((1,2,3), (0,2,3));
is_older((1,2,3), (2,2,3));

number_in_month([(1,10,2), (10,1,2), (1,2,10), (10,2,10)], 10);
number_in_month([(1,2,10), (1,10,2), (1,10,3), (2,10,4), (10,1,10)], 10);
number_in_month([], 10);

number_in_months([(1,2,10), (1,3,10), (1,4,10), (10,4,10)], [2,4]);
number_in_months([(1,2,10)], []);
number_in_months([], [1,2,3,4,5,6,7,8,9,10]);

dates_in_month([(1,10,2), (10,1,2), (1,2,10), (10,2,10)], 10);
dates_in_month([(1,2,10), (1,10,2), (1,10,3), (2,10,4), (10,1,10)], 10);
dates_in_month([], 10);

dates_in_months([(1,2,10), (1,3,10), (1,4,10), (10,4,10)], [2,4]);
dates_in_months([(1,2,10)], []);
dates_in_months([], [1,2,3,4,5,6,7,8,9,10]);

get_nth(["a","b","cc","d"], 4);
get_nth(["a","b","cc","d"], 1);
get_nth(["a"], 1);

date_to_string((1999,2,1));
date_to_string((2014,12,31));
date_to_string((2000,1,10));

number_before_reaching_sum(10, [1,2,3,4]);
number_before_reaching_sum(10, [1,2,3,5]);
number_before_reaching_sum(10, [1,2,3,3]);
number_before_reaching_sum(10, [1,20,3,4]);
number_before_reaching_sum(10, [10,2,3,4]);
number_before_reaching_sum(10, [11,2,3,4]);

what_month(1);
what_month(31);
what_month(32);
what_month(59);
what_month(60);
what_month(334);
what_month(335);
what_month(365);

month_range(30,35);
month_range(88,92);
month_range(365,365);
month_range(211, 215);
month_range(8,7);
month_range(1,365);

oldest([]);
oldest([(1000,1,2)]);
oldest([(1,2,3),(1,2,4),(1,1,4),(2,1,4),(2,2,1),(1,1,1)]);
oldest([(1,2,3),(1,2,4),(1,1,4),(1,1,1),(2,2,1),(1,2,1)]);
oldest([(1,1,1),(1,2,4),(1,1,4),(2,1,4),(2,2,1),(1,1,2)]);

unique([2,4,2,4,5,6,2,4,2,2,4,4]);

number_in_months_challenge([(1,2,10), (1,3,10), (1,4,10), (10,4,10)],[2,4,2,4,5,6,2,4,2,2,4,4]);
number_in_months_challenge([(1,2,10)], []);
number_in_months_challenge([], [1,2,3,4,5,1,1,7,7,1,1,1,7,8,9,10]);

dates_in_months_challenge([(1,2,10), (1,3,10), (1,4,10), (10,4,10)],[2,4,2,4,5,6,2,4,2,2,4,4]);
dates_in_months_challenge([(1,2,10)], []);
dates_in_months_challenge([], [1,2,3,4,5,1,1,7,7,1,1,1,7,8,9,10]);

reasonable_date(1000,2,29);
reasonable_date(1200,2,29);
reasonable_date(1024,2,29);
reasonable_date(0,1,1);
reasonable_date(1,1,1);
reasonable_date(2013,2,29);
reasonable_date(2013,0,10);
reasonable_date(2013,6,31);
reasonable_date(2013,7,31);
reasonable_date(2013,12,10);
reasonable_date(2013,13,10);
reasonable_date(2013,12,31);
