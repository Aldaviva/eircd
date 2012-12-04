-record(user, {
	process,
	nickname,
	socket,
	modes = sets:new(),
	username,
	hostname,
	servername,
	realname
}).

-record(channel, {
	process,
	name,
	creation_date = calendar:universal_time(),
	modes = sets:new(),
	topic,
	topic_date,
	users = sets:new()
}).