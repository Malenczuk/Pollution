%%%-------------------------------------------------------------------
%%% @author Marcin Malenczuk
%%% @copyright (C) 2018 Marcin Malenczuk
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Marcin Malenczuk").

-include_lib("eunit/include/eunit.hrl").
-include("pollution_rec.hrl").
-compile(export_all).

start_stop_server_test() ->
  ?assertEqual(true, pollution_server:start()),
  ?assertEqual({ok, {monitor, #{}}}, pollution_server:stop()).

addStation_test() ->
  pollution_server:start(),
  Station1 = #station{name = "1", location = {1, 1}},
  Station2 = #station{name = "2", location = {2, 2}},

  {ok, Monitor1} = pollution_server:addStation("1", {1, 1}),

  ?assertEqual(
    Station1,
    maps:get({"1", {1, 1}}, Monitor1#monitor.stations)
  ),

  {ok, Monitor2} = pollution_server:addStation("2", {2, 2}),

  ?assertEqual(
    #{{"1", {1, 1}} => Station1, {"2", {2, 2}} => Station2},
    Monitor2#monitor.stations
  ),

  ?assertEqual(
    {error, "Station already exists"},
    pollution_server:addStation("2", {2, 2})
  ),
  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),
  Time = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  {ok, Monitor2} = pollution_server:addValue("1", Time, "pm10", 50),

  ?assertEqual(
    {monitor,
      #{{"1", {1, 1}} =>
      {station, "1",
        {1, 1},
        #{{Time, "pm10"} =>
        {measurement, "pm10", 50, Time}}}}},
    Monitor2
  ),

  ?assertEqual(
    {error, "Measurement of that type already exists at that date"},
    pollution_server:addValue("1", Time, "pm10", 75)
  ),

  ?assertEqual(
    {error, "No station with that name"},
    pollution_server:addValue("2", Time, "pm10", 50)
  ),

  ?assertEqual(
    {error, "No station at that location"},
    pollution_server:addValue({2, 2}, Time, "pm10", 50)
  ),
  pollution_server:stop().

removeValue_test() ->
  pollution_server:start(),
  DateTime = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  pollution_server:addValue("1", DateTime, "pm10", 50),
  {ok, Monitor3} = pollution_server:removeValue("1", DateTime, "pm10"),

  ?assertEqual(
    {monitor,
      #{{"1", {1, 1}} =>
      {station, "1",
        {1, 1},
        #{}}}},
    Monitor3
  ),

  ?assertEqual(
    {error, "Measurement of that type doesn't exists at that date"},
    pollution_server:removeValue("1", DateTime, "pm25")
  ),
  pollution_server:stop().

getOneValue_test() ->
  pollution_server:start(),
  DateTime = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  pollution_server:addValue("1", DateTime, "pm10", 50),

  ?assertEqual(
    {ok, 50},
    pollution_server:getOneValue("1", DateTime, "pm10")
  ),

  ?assertEqual(
    {error, "Measurement of that type doesn't exists at that date"},
    pollution_server:getOneValue("1", DateTime, "pm25")
  ),
  pollution_server:stop().

getStationMean_test() ->
  pollution_server:start(),
  {Date, {H, M, S}} = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  pollution_server:addStation("2", {2, 2}),
  pollution_server:addValue("1", {Date, {H, M, S}}, "pm10", 50),
  pollution_server:addValue("1", {Date, {H + 1, M, S}}, "pm10", 60),
  pollution_server:addValue("1", {Date, {H + 2, M, S}}, "pm10", 70),
  pollution_server:addValue("1", {Date, {H + 2, M, S}}, "pm25", 50),
  pollution_server:addValue("2", {Date, {H, M, S}}, "pm10", 50),

  ?assertEqual(
    {ok, 60.0},
    pollution_server:getStationMean("1", "pm10")
  ),
  pollution_server:stop().

getDailyMean_test() ->
  pollution_server:start(),
  {{Y, M, D}, Time} = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  pollution_server:addStation("2", {2, 2}),
  pollution_server:addValue("1", {{Y, M, D}, Time}, "pm10", 50),
  pollution_server:addValue("1", {{Y, M, D}, Time}, "pm25", 90),
  pollution_server:addValue("1", {{Y, M, D + 1}, Time}, "pm10", 60),
  pollution_server:addValue("2", {{Y, M, D}, Time}, "pm25", 60),

  ?assertEqual(
    {ok, 75.0},
    pollution_server:getDailyMean({Y, M, D}, "pm25")
  ),
  pollution_server:stop().

getMinMaxValue_test() ->
  pollution_server:start(),
  {Date, {H, M, S}} = calendar:local_time(),
  pollution_server:addStation("1", {1, 1}),
  pollution_server:addStation("2", {2, 2}),
  pollution_server:addValue("1", {Date, {H, M, S}}, "pm10", 50),
  pollution_server:addValue("1", {Date, {H + 1, M, S}}, "pm10", 60),
  pollution_server:addValue("1", {Date, {H + 2, M, S}}, "pm10", 70),
  pollution_server:addValue("1", {Date, {H + 2, M, S}}, "pm25", 50),
  pollution_server:addValue("2", {Date, {H, M, S}}, "pm10", 100),

  ?assertEqual(
    {ok, {50, 70}},
    pollution_server:getMinMaxValue("1", Date ,"pm10")
  ),

  ?assertEqual(
    {ok, {nomeasurement, nomeasurement}},
    pollution_server:getMinMaxValue("2", Date ,"pm25")
  ),
  pollution_server:stop().
