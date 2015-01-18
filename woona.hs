import Network
import System.IO
import Data.List
import qualified Control.Exception as Ex

ircServer = ("pinkiepie.ponychat.net", 6667)
channels = [ "#testing123" ]

data IRCClient = IRCClient { username :: String
                           , realname :: String
                           , nickname :: String
			   } deriving (Show)
client = IRCClient { username = "woonabot", realname = "Woona Bot", nickname = "woonabot" }

-- prefix = servername / (nickname [ userdata ])
data Prefix = ServerName String | UserPrefix String (Maybe UserData) deriving (Eq, Show)
-- userdata = [ username ] hostname
data UserData = UserData (Maybe String) String deriving (Eq, Show)

type Command = Maybe String
type Params = Maybe [String]

-- Connect to IRC server and start listen&process loop
main = withSocketsDo $ do
	putStrLn $ "Connecting to " ++ (show ircServer) ++ "..."
	handle <- connectTo (fst ircServer) (PortNumber $ snd ircServer)
	putStrLn $ "Authenticating with " ++ (show client)
	-- Authenticate to the server
	hPutStrLn handle $ "USER " ++ username client ++ " 8 * : " ++ realname client
	hPutStrLn handle $ "NICK " ++ nickname client
	-- Start loop
	listenForeverOn handle `Ex.catch` handleErr
	where
	handleErr :: IOError -> IO ()
	handleErr e = putStrLn $ "Couldn't connect to " ++ (show ircServer)

-- Listen to incoming messages and do something according to the message
listenForeverOn :: Handle -> IO ()
listenForeverOn handle = do
	line <- hGetLine handle
	putStrLn $ "Received line: " ++ line
	let maybeMsg = process . parseIRCLine $ line
	--let parsed = parseIRCLine line
	--putStrLn $ "Parsed: " ++ (show parsed)
	--let maybeMsg = process parsed
	if maybeMsg /= Nothing then do
		let (Just msg) = maybeMsg in do
			hPutStrLn handle msg
			putStrLn $ "=> Sending: " ++ msg
		listenForeverOn handle
	else
		listenForeverOn handle

{- BNF for IRC message:
 - 	message    ::= [ ":" prefix SPACE ] command [ params ] crlf
 -	prefix     ::= servername / (nickname [ [ "!" user ] "@" host ])
 -	command    ::= 1*letter / 3digit
 - 	params     ::= *14(SPACE middle) [ SPACE ":" trailing ]
 -		   ::= 14(SPACE middle) [ SPACE [ ":" ] trailing ]
 -	middle     ::= nospcrlfcl *( ":" / nospcrlfcl )
 -	nospcrlfcl ::= %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
 -			; i.e. any octet except NUL, CR, LF, " " and ":"
 -	trailing   ::= *( ":" / " " / nospcrlfcl)
 -	SPACE      ::= %x20
 -	crlf       ::= %x0D %x0A
 -}
parseIRCLine :: String -> (Maybe Prefix, Command, Params)
parseIRCLine "" = (Nothing, Nothing, Nothing)
parseIRCLine line = 
		let
			spl = words line
			prefix = if head line == ':'
				then Just $ parsePrefix . tail $ head spl
				else Nothing
			command = if prefix == Nothing
				then Just $ head spl
				else Just $ spl !! 1
			params = if prefix == Nothing
				then parseParams $ tail spl
				else parseParams $ tail . tail $ spl

		in (prefix, command, params)
		where
		parsePrefix :: String -> Prefix
		parsePrefix pfx = 
			if '@' `elem` pfx then 
				let
					nickname = takeWhile (/='!') pfx
					hostname = tail $ dropWhile (/='@') pfx
					username = tail $ dropWhile (/='!') $ takeWhile (/='@') pfx
				in
					if '!' `elem` pfx
						then UserPrefix nickname (Just $ UserData (Just username) hostname)
						else UserPrefix nickname (Just $ UserData Nothing hostname)
			else ServerName pfx
		parseParams :: [String] -> Params
		parseParams [] = Nothing
		parseParams par = Just $ (words f) ++ [if length s > 0 then tail s else s]
			where (f, s) = span (/=':') $ unwords par
			

-- Do the actual message processing. Return the line to write to the server or nothing
process :: (Maybe Prefix, Command, Params) -> Maybe String
-- Answer to PINGs
process (_, Just "PING", Just par) = Just $ "PONG " ++ (head par)
-- Autojoin channels after End-Of-MOTD
process (_, Just "376", _) = Just $ intercalate "\r\n" .  map (\chan -> "JOIN " ++ chan) $ channels
-- Other actions
process (Just (UserPrefix nick (Just (UserData (Just user) host))), Just "PRIVMSG", Just [chan, msg])
	| msg == "whoami" = Just $ "PRIVMSG " ++ chan ++ " :You are user " ++ user ++ "@" ++ host ++ " with nickname " ++ nick
process (Just (UserPrefix nick _), Just "PRIVMSG", Just [chan, msg])
	| msg == "hi " ++ nickname client = Just $ "PRIVMSG " ++ chan ++ " :Hi, " ++ nick
process _ = Nothing
