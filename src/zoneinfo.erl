-module(zoneinfo).

-export([load/1, debug_zoneinfo/1]).

-record(ttinfo, {ut_off, is_dst, desig_idx, is_std, is_ut}).
-record(zoneinfo, {tt, tt_info, zone_abrev, leap_sec}).

-define(SECONDS_PER_DAY, 86400).
-define(DAYS_FROM_0_TO_1970, 719528).
-define(SECONDS_FROM_0_TO_1970, (?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY)).

load(File) ->
    {ok, Data} = file:read_file(File),
    parse_header_v0(Data).

parse_header_v0(<<"TZif", Version:8, _:15/bytes,
	       TTisutCnt:32, TTisstdCnt:32, LeapCnt:32,
	       TimeCnt:32, TypeCnt:32, CharCnt:32, Rest0/binary>>) ->
    <<TTbin:(TimeCnt * 4)/bytes,
      NextTTIdxBin:TimeCnt/bytes,
      TTinfoBin:(TypeCnt * 6)/bytes,
      ZoneAbrevBin:CharCnt/bytes,
      LeapSecBin:(LeapCnt * 8)/bytes,
      IsStdBin:TTisstdCnt/bytes,
      IsUtBin:TTisutCnt/bytes,
      Rest1/binary>> = Rest0,

    ZoneInfo =
	#zoneinfo{
	   tt = lists:zip([?SECONDS_FROM_0_TO_1970 + X || <<X:32/signed>> <= TTbin],
			  binary_to_list(NextTTIdxBin)),
	   tt_info = lists:zipwith3(
		       fun({UtOff, IsDst, DesigIdx}, IsStd, IsUt) ->
			       #ttinfo{
				  ut_off = UtOff,
				  is_dst = to_bool(IsDst),
				  desig_idx = DesigIdx,
				  is_std = to_bool(IsStd),
				  is_ut = to_bool(IsUt)}
		       end,
		       [{UtOff, IsDst, DesigIdx} ||
			   <<UtOff:32/signed, IsDst:8, DesigIdx:8>> <= TTinfoBin],
		       binary_to_list(IsStdBin),
		       binary_to_list(IsUtBin)),
	   zone_abrev = parse_zone_abrev(0, ZoneAbrevBin, #{}),
	   leap_sec = [{Trans, Corr} || <<Trans:32, Corr:32/signed>> <= LeapSecBin]
	  },
    case Version of
	$2 -> parse_header_v2(Rest1);
	_ -> ZoneInfo
    end.

parse_header_v2(<<"TZif", $2:8, _:15/bytes,
	       TTisutCnt:32, TTisstdCnt:32, LeapCnt:32,
	       TimeCnt:32, TypeCnt:32, CharCnt:32, Rest0/binary>>) ->
    <<TTbin:(TimeCnt * 8)/bytes,
      NextTTIdxBin:TimeCnt/bytes,
      TTinfoBin:(TypeCnt * 6)/bytes,
      ZoneAbrevBin:CharCnt/bytes,
      LeapSecBin:(LeapCnt * 12)/bytes,
      IsStdBin:TTisstdCnt/bytes,
      IsUtBin:TTisutCnt/bytes,
      _/binary>> = Rest0,

    #zoneinfo{
       tt = lists:zip([?SECONDS_FROM_0_TO_1970 + X || <<X:64/signed>> <= TTbin],
		      binary_to_list(NextTTIdxBin)),
       tt_info =
	   maps:from_list(
	     lists:zip(
	       lists:seq(1, TypeCnt),
	       lists:zipwith3(
		 fun({UtOff, IsDst, DesigIdx}, IsStd, IsUt) ->
			 #ttinfo{
			    ut_off = UtOff,
			    is_dst = to_bool(IsDst),
			    desig_idx = DesigIdx,
			    is_std = to_bool(IsStd),
			    is_ut = to_bool(IsUt)}
		 end,
		 [{UtOff, IsDst, DesigIdx} ||
		     <<UtOff:32/signed, IsDst:8, DesigIdx:8>> <= TTinfoBin],
		 binary_to_list(IsStdBin),
		 binary_to_list(IsUtBin)))),
       zone_abrev = parse_zone_abrev(0, ZoneAbrevBin, #{}),
       leap_sec = [{Trans, Corr} || <<Trans:32, Corr:32/signed>> <= LeapSecBin]
      }.

to_bool(Int) -> Int > 0.

fmt_tt_info(Design, ZoneInfo) ->
    #ttinfo{
       ut_off = UtOff, is_dst = IsDst, desig_idx = DesigIdx,
       is_std = IsStd, is_ut = IsUt} =
	maps:get(Design, ZoneInfo#zoneinfo.tt_info),
    io_lib:format("~s, isdst=~p gmtoff=~p isstd=~p isut=~p",
		  [maps:get(DesigIdx, ZoneInfo#zoneinfo.zone_abrev),
		   IsDst, UtOff, IsStd, IsUt]).

debug_zoneinfo(ZoneInfo) ->
    io:format("TT:~n"),
    lists:foreach(
      fun({Secs, Design}) ->
	      DateTime = calendar:gregorian_seconds_to_datetime(Secs),
	      io:format("~p: ~s~n", [DateTime, fmt_tt_info(Design, ZoneInfo)])
      end, ZoneInfo#zoneinfo.tt),
    io:format("TTinfo: ~p~n", [ZoneInfo#zoneinfo.tt_info]),
    io:format("ZoneAbrev: ~p~n", [ZoneInfo#zoneinfo.zone_abrev]),
    io:format("LeapSec: ~p~n", [ZoneInfo#zoneinfo.leap_sec]),
    ok.

parse_zone_abrev(_, <<>>, ZoneAbrev) ->
    ZoneAbrev;
parse_zone_abrev(Pos, ZoneAbrevBin, ZoneAbrev) ->
    [Zone, Next] = binary:split(ZoneAbrevBin, <<0>>),
    parse_zone_abrev(Pos + size(Zone) + 1, Next, ZoneAbrev#{Pos => Zone}).
