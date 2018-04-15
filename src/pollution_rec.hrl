%%%-------------------------------------------------------------------
%%% @author Marcin Maleńczuk
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("Marcin Maleńczuk").

-record(monitor, {locations = #{}, stations = #{}}).
-record(station, {name = "", location, measurements = #{}}).
-record(measurement, {type, value, datetime}).