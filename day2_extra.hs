module Main where

input = ["DULUDRDDDRLUDURUUULRRRURDRDULRUDDUDRULUDDUDRLDULRRLRDRUDUUULUUDLRURDUDDDDRDLLLLULRDLDRDLRLULRUURDDUULUDLRURRDDRDDRDDLDRDLLUURDRUULRRURURRDLRLLLUDULULULULUDRLLRUDUURLDRLRLRDRRDRLLLDURRDULDURDDRLURRDURLRRRLDLLLDRUUURLRDLDLLLLRDURRLDLULRLDDLDLURLRRDDRUDDUULRURRUDLRDLDUURDDDDRLRURUDULUDLRRLLLLLRDRURLLDLDULUUDLUDDDRLLDRRUDLLURRUUDDRRLLRRLDDDURLDRDRLURRRRDRRRDDUDULULDURRUUURRRDULUUUDDRULDRLLRDLDURLURRLLRUUUULRDURLLDDRLLDLRLRULUUDRURUDLLURUDDRDURLRDRRRDURLDDRDRLRLLURULUUULUDDDULDLRDDDRDLLRRLDRDULLUUUDLDDLDDDLLLLLLLDUDURURDURDRUURRRDDRDUDLULDURDUDURDDDRULDURURURRLURLURLUURLULDLLRUULURDDRLRDDLRDLRRR",
	"LUURLRUDRRUDLLDLUDDURULURLUUDUUDDRLUULRDUDDUULDUUDRURDDRRDRLULLRDRDLRLLUURRUULRLDRULUDLDUUDDDRDDLRDLULDRLDUULDLRDLLLDLDLRDUULUDURRULLRLDUDRLLLULUUUULUUDUUURRRDULLUURUDRRLDURRUULDRDULDUDRDUUULUUDDRLUDRLDLDRUUURDLDUDRUDUURLLRRLRLLRRLDULDDULUDUUURULDDUDUDRURRDLULRUDDURDLDLLRRRLDRLULLLRUULDUDLUUDURRLLLRLUDURRDDLDRDDDLURDLDRRUDUDLUDULULRUUUDLUURLLRLDDLURULDURDLRRDDDDURLDDLLDDULLLRLDLDULDUUDDRLDUURDDLDLUUDULRRLRLUURURUURLRLURUURLDRUURLLRDDUUUDULUDDDRDRLDRDRRLRLDULLRRUDLURULULRDRURURLULDUDLRURLRDDRULDDLRD",
	"LUDRULUULRRDDDDRRDUURUDDRLDDLDRDURRURULRDLDLDUUDRRDUUDUDLLLRRLDUDDRLDDLRRLRDRLUDLULUDDUUDULDUUULUDLDDURLDURUDLDRUUDRLRRLDLDDULDUUDDLDDLLURDRLRUURDDRUDDUDLDRRLRUDRUULRRRLRULULURDLRRURDRLRULDDDRDUULLURUUUURUDDLRRRRRDURLULDLUULUDRRUDUDRRDDRURDURLRLUDDLDLRRULUDLDDRLDDLDDDLLLLRDLLUULDDLULDLDRDDUDLURUDLDLDDRRUUDDDLRLLLDRRDDDUURDUDURUURRDRLLDUDLDUULLDLDLLUULLRRULDLDRURLDULDRUURDURRURDLRDLLLDRRUDRUUDRURLUDDRURLDURRDLUUDLUUDULLLDDDDRRDLLLDLURULDDRDLUUURRDRRUUDDUL",
	"DUUULDUDDDURLLULDDLLUDURLLLURULULURUURDRURLRULLLLDRDDULRRDRRLLLRDDDUULLRRURRULLDDURRRLRDDLULDULLDUDLURRDLDDLURDLRLLDRURLLRLLRRRDRRRURURUUDDLLDDLDDDLRLURUUUULRDLUDDDURLLDDRLDRRLLUDUUULRLLDRRRLRUUDLDUULRLUDRULLLLDUDLLUUDDRUURLURUDRDDDLRURUDRLULLULUUDLDURDULRRDRLDURUULRDRRRDRDRRLRLRDDUULLRDLDURDDDULURRLULDDURDURDDUDURDLLUUULUDULRDDLDRDRUDLLUURDLRDURURULURULLDRLLRRULDLULULDLULRURLRRLUDLLLRLUDLURLULDULDRLLLDLDDDDRDRLRRLRDULUUDULDDLDURDLLLDDDDLLUURRDURLDLUDDLULRUUUDDRRLDLLLRDLLDRRRDDLULLURDDRRRRLDLRLLLRL",
	"LULLRRDURRLDUUDRRURLURURRRLRDRUULUULURLLURRDRULRDURDDDDUULLLLDUULDLULURDRLDLULULDRLLDLLRLRULURUDRUUDULRULLLUDRULUDRLLUDLDRRDRUUURURLRDURDRLRDDDURLURRDLRUUUDUURULULDLUULRDLRRRDRDRLLLDLRRDRLLDDULDRUDRRLULLRDLDUDDULRDDLULRURULRLLLULDLLLLRDLDRURUDUURURLDRLUULLDUDULUDDDULUDLRUDDUDLULLUULUUURULURRULRDDURDDLURLRRDRDLDULRLRDRRRULRDDDRLLDDDDRRRRDRDLULUURDURULDLRDULDUDLDURUDLUDLUDDDUDURDURDDURLLRUDUURRRUDRRRRULLLLDDDLUULLUULRRRULDLURDLULRULDRLR"]

instructionToNumber :: Char -> Char -> Char
instructionToNumber '1' dir =
  case dir of
    'U' -> '1'
    'D' -> '3'
    'R' -> '1'
    'L' -> '1'

instructionToNumber '2' dir =
  case dir of
    'U' -> '2'
    'D' -> '6'
    'R' -> '3'
    'L' -> '2'

instructionToNumber '3' dir =
  case dir of
    'U' -> '1'
    'D' -> '7'
    'R' -> '4'
    'L' -> '2'

instructionToNumber '4' dir =
  case dir of
    'U' -> '4'
    'D' -> '8'
    'R' -> '4'
    'L' -> '3'

instructionToNumber '5' dir =
  case dir of
    'U' -> '5'
    'D' -> '5'
    'R' -> '6'
    'L' -> '5'

instructionToNumber '6' dir =
  case dir of
    'U' -> '2'
    'D' -> 'A'
    'R' -> '7'
    'L' -> '5'

instructionToNumber '7' dir =
  case dir of
    'U' -> '3'
    'D' -> 'B'
    'R' -> '8'
    'L' -> '6'

instructionToNumber '8' dir =
  case dir of
    'U' -> '4'
    'D' -> 'C'
    'R' -> '9'
    'L' -> '7'

instructionToNumber '9' dir =
  case dir of
    'U' -> '9'
    'D' -> '9'
    'R' -> '9'
    'L' -> '8'

instructionToNumber 'A' dir =
  case dir of
    'U' -> '6'
    'D' -> 'A'
    'R' -> 'B'
    'L' -> 'A'

instructionToNumber 'B' dir =
  case dir of
    'U' -> '7'
    'D' -> 'D'
    'R' -> 'C'
    'L' -> 'A'

instructionToNumber 'C' dir =
  case dir of
    'U' -> '8'
    'D' -> 'C'
    'R' -> 'C'
    'L' -> 'B'

instructionToNumber 'D' dir =
  case dir of
    'U' -> 'B'
    'D' -> 'D'
    'R' -> 'D'
    'L' -> 'D'

instructionToNumber current dir = error "Invalid input"

decoded :: [Char] -> [Char]
decoded instructions = 
    '5' : zipWith instructionToNumber (decoded instructions) instructions

main = do
  let
    result = map last (map decoded input)

  print result
