import Network
import System.IO
import Text.Printf
import System.Random (randomRIO)
import Data.List (isPrefixOf)

server = "irc.rizon.net"
port = 6667
chan = "#cincofacetimepartysnoozer"
nick = "thelifeoftheparty"

banter = ["Sounds good.", "Thanks for coming.", "Okay.", "I understand.", "Sure, why not?"]

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick ++ " 0 * :a friendly guy")
  write h "JOIN" chan
  listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s
    idx <- randomRIO (0,4)
    if (isPing s) then (pong s) else if (isPersonal s) then (write h ("PRIVMSG " ++ chan) (":" ++ (banter !! idx))) else (return ())
  where forever = sequence_ . repeat
        isPersonal x = (nick ++ ": ") `isPrefixOf` (clean x)
	isPing = isPrefixOf "PING :"
	pong x = write h "PONG" (':' : drop 6 x)
	clean = drop 1 . dropWhile (/= ':') . drop 1
