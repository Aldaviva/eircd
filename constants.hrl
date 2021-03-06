-define(SERVER_HOSTNAME, "127.0.0.1").
-define(SERVER_VERSION, "eircd0.0.2").
-define(SERVER_BUILD_TIME, "Sat Dec 01 04:57:14 2012").

-define(CRLF, [13,10]).

% http://www.irchelp.org/irchelp/rfc/rfc2812.txt
-define(IRC_WELCOME_NAME, 					001).
-define(IRC_WELCOME_SERVER, 				002).
-define(IRC_WELCOME_SERVER_CREATION_DATE,	003).
-define(IRC_WELCOME_CAPABILITIES, 			004).
-define(IRC_MOTD_START, 					375).
-define(IRC_MOTD_BODY, 						372).
-define(IRC_MOTD_END, 						376).
-define(IRC_ERROR, 							421).
-define(IRC_USER_MODES,						221).
-define(IRC_CHANNEL_MODES, 					324).
-define(IRC_CHANNEL_CREATION_DATE, 			329).
-define(IRC_WHO_BODY, 						352).
-define(IRC_WHO_END, 						315).
-define(IRC_TIME, 							391).
-define(IRC_CHANNEL_LIST_START, 			321).
-define(IRC_CHANNEL_LIST_BODY, 				322).
-define(IRC_CHANNEL_LIST_END, 				323).
-define(IRC_NAMES_BODY,						353).
-define(IRC_NAMES_END,						366).