-module(eircd_helpers).
-export([
	split/1,
	split/2,
	split/3,
	channel_strip_name/1,
	seconds_since_epoch/0,
	seconds_since_epoch/1
]).

split(String, Limit, Pattern) ->
	re:split(String, Pattern, [{return, list}, {parts, Limit}]).

split(String, Pattern) when is_list(Pattern) ->
	split(String, infinity, Pattern);

split(String, Limit) ->
	split(String, Limit, "\\s+").

split(String) ->
	split(String, infinity).

channel_strip_name(Name) ->
	case Name of
		[$#|Tail] -> Tail;
		Other -> Other
	end.

seconds_since_epoch() ->
	seconds_since_epoch(calendar:universal_time()).
seconds_since_epoch(Datetime) ->
	calendar:datetime_to_gregorian_seconds(Datetime) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).