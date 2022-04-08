-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
char_to_int  '0' = 0
char_to_int  '1' = 1
char_to_int  '2' = 2
char_to_int  '3' = 3
char_to_int  '4' = 4
char_to_int  '5' = 5
char_to_int  '6' = 6
char_to_int  '7' = 7
char_to_int  '8' = 8
char_to_int  '9' = 9

repeat_char :: Char -> Integer -> String
repeat_char n x
    | x<=0 =[]
    | otherwise = n : repeat_char n (x-1) 

decode :: String -> String
decode [] = []
decode [x] = error "something wrong"
decode (x:y:xs)=repeat_char x (char_to_int y) ++ decode xs



-- Part B

int_to_char :: Integer -> Char
int_to_char 0 = '0' 
int_to_char 1 = '1' 
int_to_char 2 = '2' 
int_to_char 3 = '3' 
int_to_char 4 = '4' 
int_to_char 5 = '5' 
int_to_char 6 = '6' 
int_to_char 7 = '7' 
int_to_char 8 = '8' 
int_to_char 9 = '9' 
 
length_char :: Char -> String -> Integer
length_char n [] = 0
length_char n (x:xs) = 
    if n == x
    then 1 + length_char n xs
    else 0

drop_char :: Char -> String -> String
drop_char n [] = []
drop_char n (x:xs) = 
    if n == x
    then drop_char n xs
    else x:xs

encode :: String -> String
encode [] = []
encode (x:xs) = x : int_to_char (length_char x (x:xs)) : encode(drop_char x xs)

-- Part C

listint_to_int xs = total (xs, 0)
  where
        total ([], n) = n
        total (x:xs, n) = total (xs, (n * 10 + x))

char_to_int_complex_base [] = []
char_to_int_complex_base (x:xs) = (char_to_int x) : char_to_int_complex_base xs

char_to_int_complex list = listint_to_int (char_to_int_complex_base list)


int_to_listint 0 = []
int_to_listint x = int_to_listint (x `div` 10) ++ [x `mod` 10]

int_to_char_complex_base [] = []
int_to_char_complex_base (x:xs) = int_to_char x : int_to_char_complex_base xs

int_to_char_complex x =int_to_char_complex_base ( int_to_listint x)

complex_encode :: String -> String
complex_encode [] = []
complex_encode (x:xs) =
    if int_to_char_complex (length_char x (x:xs)) == "1"
    then x : complex_encode (drop_char x xs)
    else x : int_to_char_complex (length_char x (x:xs)) ++ complex_encode (drop_char x xs)

elem_own e [] = False
elem_own e (x:xs)
    | e == x =True
    | otherwise = elem_own e xs

split_string (x:xs)
    | (char_to_int_complex x) `elem_own` [1]  = char_to_int_complex x
    | otherwise = 4

simple_to_non_base [] = []
simple_to_non_base (x:xs) = init(init(simple_to_non (x:xs++['c'])))


simple_to_non [] = []
simple_to_non [x] = x : '1' : []
simple_to_non (x:y:xs) =
    if x `elem_own` ['a'..'z'] && y `elem_own` ['a'..'z'] 
    then x  : ['1']  ++  simple_to_non (y:xs)
    else x : simple_to_non (y:xs)



decode_a_n [ ] = [ ]
decode_a_n (x:y:[]) = [y]
decode_a_n (x:y:z:xs) =
    if z `elem_own` ['a'..'z'] 
    then [ y ]
    else  y:decode_a_n (y:z:xs) 


left_by_decode [] = []
left_by_decode (x:y:[]) = []
left_by_decode (x:y:z:xs) =
    if z `elem_own` ['a'..'z'] 
    then z:xs
    else left_by_decode (y:z:xs) 

decode_base [] = []
decode_base (x:xs)= repeat_char x (char_to_int_complex (decode_a_n(x:xs))) ++ decode_base(left_by_decode(x:xs))

complex_decode :: String -> String
complex_decode  x = decode_base (simple_to_non_base x)
