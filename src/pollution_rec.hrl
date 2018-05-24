%%%-------------------------------------------------------------------
%%% @author Marcin Malenczuk
%%% @copyright (C) 2018 Marcin Malenczuk
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("Marcin Malenczuk").

-record(monitor, {stations = #{}}).
-record(station, {name = "", location, measurements = #{}}).
-record(measurement, {type, value, datetime}).