-- Do not alter the following line
module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where


type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A


transaction_to_string :: Transaction -> String
 
transaction_to_string ('B',a,b,c,d) = "Bought " ++ show a ++ " units of " ++ c ++ " for " ++ show b  ++ " pounds each on day " ++ show d
transaction_to_string ('S',a,b,c,d) = "Sold " ++ show a ++ " units of " ++ c ++ " for " ++ show b  ++ " pounds each on day " ++ show d


trade_report_list :: [Transaction] -> [String]
trade_report_list x = map transaction_to_string x


stock_test :: String -> Transaction -> Bool
stock_test x (a,b,c,d,e) = 
    if d == x
    then True
    else False

get_trades :: String -> [Transaction] -> [Transaction]
get_trades x y = filter (stock_test x) y


trade_report :: String -> [Transaction] -> String
trade_report x y = unlines (trade_report_list (get_trades x y))


-- Part B


update_money :: Transaction -> Int -> Int

update_money ('B',a,b,c,d) x = x - a * b
update_money ('S',a,b,c,d) x = x + a * b
update_money (y,a,b,c,d) x = x


profit :: [Transaction] -> String -> Int
profit x y = foldr (\ z acc -> update_money  z acc) 0 (get_trades y x)


profit_report :: [String] -> [Transaction] -> String
profit_report str log = unlines (zipWith (\ x y -> x ++": "++y ) str (map (\x -> show (profit log x)) str))


-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"


type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]


search _ [] = error "element not found"
search x ((a,b):xs) = if x == a then b else search x xs

date_to_price str x y =  (search str y) !! (x-1)

chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

list_insert [] _ _ = []
list_insert (x:xs) 0 y = x:y:xs
list_insert (x:xs) n y = x:list_insert xs (n-1) y

insert_price_base (a:b:c:d) y = list_insert (a:b:c:d) 1 (show (date_to_price c (read (last (a:b:c:d)) :: Int) y))

insert_price log prices = map (\x -> insert_price_base x prices) ( chunks 4 ( words log ) )

replace_buy_and_sell [] = []
replace_buy_and_sell (x:xs) =
    if x == "BUY"
    then "B":replace_buy_and_sell(xs)
    else if x == "SELL"
    then "S":replace_buy_and_sell(xs)
    else x:replace_buy_and_sell(xs)

cnv [] = []
cnv (a:b:c:d:e:o) = (head a,read b :: Int,read c :: Int,d,read e :: Int) : cnv o

distinct [] = []
distinct (x : xs) 
    | x `elem` (distinct xs) = distinct xs
    | otherwise = x : distinct xs

show_str x = distinct( concat (map init (map tail (map tail (chunks 4 ( words x ))))))


complex_profit_report :: String -> Prices -> String
complex_profit_report log prices = profit_report (show_str log) (concat (map cnv (chunks 5 (replace_buy_and_sell (concat (insert_price log prices))))))
