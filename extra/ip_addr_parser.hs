import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isHexDigit)
import Numeric (readHex)
-- main = interact (parse (parse_ip_addr <* skipSpaces <* eof))

-- https://blog.dave.tf/post/ip-addr-parsing/
main = mapM_ print $ map (parse (parse_ip_addr <* skipSpaces <* eof)) testCases

testCases = [
    "1:2:3:4:5:6:7:8",
    "1:2:3:4:5:6:7:f",
    "192.168.0.1", 
    "::", -- 0:0:0:0:0:0:0:0
    "::1", -- 0:0:0:0:0:0:0:1
    "::1:1", -- 0:0:0:0:0:0:1:1
    "1:2::3:4", -- 1:2:0:0:0:0:3:4
    "1:2::3:f", -- 1:2:0:0:0:0:3:f
    "1:2:3:4:5:6:77.77.88.88", -- 1:2:3:4:5:6:4d4d:5858
    "fe80::1.2.3.4" -- fe80:0:0:0:0:0:102:304
    ]

-- parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser
-- parse = readP_to_S

parse_ip_addr = parse_ipv4_addr +++ parse_ipv6_addr

parse_ipv6_addr = ipv6_elision +++ ipv6_enclosing_ipv4
parse_ipv4_addr = ipv4_hex +++ ipv4_quads

ipv6_addr, ipv6_elision :: ReadP [Int]
-- ipv6_elision = ([] <$ string "::") +++ (optional (string "::") *> ipv6_addr)
-- ipv6_elision = ([] <$ string "::") <++ ((string "::") *> ipv6_addr)
ipv6_elision = (string "::") *> ipv6_addr

ipv6_enclosing_ipv4 = do
    ipv6_section <- ipv6_addr
    string "::" <++ string ":" 
    ipv4_section <- parse_ipv4_addr
    return (ipv6_section ++ ipv4_section)

ipv6_addr = sepBy (fst . head . readHex <$> many1 (satisfy isHexDigit)) (string "::" <++ string ":")

ipv4_hex, ipv4_quads :: ReadP [Int]
ipv4_quads = sepBy1 (read <$> many1 (satisfy isDigit)) (char '.')
ipv4_hex = sepBy1 (fst . head . readHex <$> many1 (satisfy isHexDigit)) (char ':')