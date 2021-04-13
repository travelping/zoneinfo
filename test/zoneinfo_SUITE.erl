-module(zoneinfo_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

all() ->
    [basic, utc, summer, winter].

init_per_suite(Config) ->
    application:ensure_all_started(zoneinfo),
    Config.

end_per_suite(_Config) ->
    ok.

basic() ->
    [{doc, "Test local to universal conversion and back"}].

basic(_Config) ->
    LocalTime = calendar:local_time(),
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),

    [TZ1] = localtime:from_local_time(LocalTime),
    LocalTime = localtime:to_local_time(TZ1),
    UniversalTime = localtime:to_universal_time(TZ1),
    ok.

utc() ->
    [{doc, "Test UTC functions"}].

utc(_Config) ->
    UniversalTime = calendar:universal_time(),
    LocalTime = calendar:universal_time_to_local_time(UniversalTime),

    {_, _, _} = TZ1 = localtime:from_universal_time(UniversalTime, "Etc/UTC"),
    UniversalTime = localtime:to_universal_time(TZ1),
    LocalTime = localtime:to_local_time(localtime:to_local_time(TZ1, "localtime")),
    ok.

summer() ->
    [{doc, "Test impossible time in winter to summer time switch"}].

summer(_Config) ->
    [] = localtime:from_local_time({{2021,3,28},{2,30,0}}, "Europe/Berlin"),
    [] = localtime:from_local_time({{2021,3,28},{3,30,0}}, <<"CET">>, "Europe/Berlin"),
    ok.

winter() ->
    [{doc, "Test duplicate time in summer to winter time switch"}].

winter(_Config) ->
    [{{{2021,10,31},{0,30,0}},7200,<<"CEST">>},
     {{{2021,10,31},{1,30,0}},3600,<<"CET">>}] =
	localtime:from_local_time({{2021,10,31},{2,30,0}}, "Europe/Berlin"),
    [{{{2021,10,31},{1,30,0}},3600,<<"CET">>}] =
	localtime:from_local_time({{2021,10,31},{2,30,0}}, <<"CET">>, "Europe/Berlin"),
    [{{{2021,10,31},{0,30,0}},7200,<<"CEST">>}] =
	localtime:from_local_time({{2021,10,31},{2,30,0}}, <<"CEST">>, "Europe/Berlin"),
    ok.
