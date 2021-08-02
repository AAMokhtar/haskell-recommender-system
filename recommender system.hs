import System.Random
import System.IO.Unsafe
-- data
users :: [String]
users = ["user1", "user2", "user3", "user4"]

items :: [String]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]

purchasesHistory :: [(String,[[String]])]
purchasesHistory = [ 
    ("user1", [["item1", "item2", "item3"],["item1", "item2", "item4"]]) , 
    ("user2", [["item2", "item5"], ["item4", "item5"]]) ,
    ("user3", [["item3", "item2"]]) , 
    ("user4", []) 
    ]

--helpers

equals :: String -> String -> Bool
equals [] (c:r) = False
equals (c:r) [] = False
equals [] [] = True
equals (c1:r1) (c2:r2) |c1 /= c2 = False
    | otherwise = equals r1 r2

--functions

-- random
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

-- recommend
------------------------------------------------------------------------------------------------------------------------
recommend :: String -> [String] -> String
recommend user cart = recommendHelper user cart (randomZeroToX 1)

recommendHelper user cart i |(recommendHelperHelperHelper user cart i) /= "" = recommendHelperHelperHelper user cart i
    |otherwise = recommendRandomItem (randomZeroToX ((length items)-1)) items 0

recommendHelperHelper user cart |cart /= [] = recommendBasedOnItemsInCart user cart
    |otherwise = recommendEmptyCart user

recommendHelperHelperHelper user cart i |i == 0 = recommendHelperHelper user cart
    |otherwise = recommendBasedOnUsers user

recommendRandomItem i [] curr = ""
recommendRandomItem i (it:ir) curr |curr == i = it
    |otherwise = recommendRandomItem i ir (curr+1)
------------------------------------------------------------------------------------------------------------------------
-- freqListItems
freqListItems:: String -> [(String, Int)]
freqListItems name =freqListItemshelper name purchasesHistory

-- get user purchases
freqListItemshelper name ((n,i):t) |equals name n =  getfreqll items i
 |otherwise = freqListItemshelper name t

-- frequency from all user carts (all items)
getfreqll [] c = []
getfreqll (i:r) c |getfreql i c /= 0 = (i,getfreql i c):getfreqll r c
    |otherwise = getfreqll r c

-- frequency from all carts (single item)
getfreql i [] = 0
getfreql i (c1:cr) |exists i c1 = getfreq i c1 + getfreql i cr
    |otherwise = getfreql i cr

-- frequency of an item in a single cart
getfreq item [] = 0 
getfreq item (h:t) |equals h item =  getfreq item t 
    |otherwise = 1 + getfreq item t
    
-- item is a member of a cart
exists item [] = False
exists item (h:t) |equals item h = True
    |otherwise = exists item t

 -- freqListCart
freqListCart:: String ->[String] -> [(String, Int)]
freqListCart u c = freqListItemsCart items (fillFreqListCart c u)
--frequency of all items from cart frequency list
freqListItemsCart [] fl = []
freqListItemsCart (it:ir) fl |freqListItemsCartHelper it fl /= 0 = (it,freqListItemsCartHelper it fl):freqListItemsCart ir fl
    |otherwise = freqListItemsCart ir fl
--frequency of one item from cart frequency list
freqListItemsCartHelper it [] = 0
freqListItemsCartHelper it ((n,fl):r) |existsModified it fl = (getfreqCart it fl) + freqListItemsCartHelper it r
    |otherwise = freqListItemsCartHelper it r

--frequency of one item from a list inside the cart frequency list
getfreqCart it [] = 0
getfreqCart it ((h,freq):t) |equals it h = freq + getfreqCart it t
    |otherwise = getfreqCart it t
--item is a member of a list inside the cart frequency list
existsModified item [] = False
existsModified item ((h,freq):t) |equals item h = True
    |otherwise = existsModified item t

--get frequency list
fillFreqListCart [] name = []
fillFreqListCart (i:r) name = (fillhelp1 i (getUserCarts name purchasesHistory) items): fillFreqListCart r name

--freqListCartAndItems
freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems u c = merge (freqListCartAndItemsHelper (freqListItems u) (freqListCart u c)) (freqListCartAndItemsHelperItems (freqListItems u) (freqListCart u c))

-- items that have no matching elements in the other list
freqListCartAndItemsHelperItems ifl [] = []
freqListCartAndItemsHelperItems ifl ((n,freq):ir) |not(existsModified n ifl) = (n,freq + freqListCartAndItemsHelperHelper n ifl):freqListCartAndItemsHelperItems ifl ir
    |otherwise = freqListCartAndItemsHelperItems ifl ir

-- add items of cart fl to matching items in items fl
freqListCartAndItemsHelper [] cfl = []
freqListCartAndItemsHelper ((n,freq):ir) cfl = (n,freq + freqListCartAndItemsHelperHelper n cfl):freqListCartAndItemsHelper ir cfl
freqListCartAndItemsHelperHelper n [] = 0
freqListCartAndItemsHelperHelper n ((nc,freq):ir) |equals n nc = freq
    |otherwise = freqListCartAndItemsHelperHelper n ir

--merge two lists
merge :: [a] -> [a] -> [a]
merge xs []= xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- recommendEmptyCart
-- 1 to 4 (1) 5 to 8 (2) 9 to 10 (3) 11 to 12 (4)
recommendEmptyCart :: String -> String
recommendEmptyCart u = recommendEmptyCartHelper (randomZeroToX (freqsum (freqListItems u))-1) (freqListItems u)

recommendEmptyCartHelper indx [] = ""
recommendEmptyCartHelper indx ((n,freq):r) |indx >= freq = recommendEmptyCartHelper (indx - freq) r
 | otherwise = n

--sum of frequency 
freqsum [] = 0
freqsum ((n,freq):r) = freq + freqsum r
--recommendBasedOnItemsInCart
recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart u c = recommendEmptyCartHelper (randomZeroToX (freqsum (freqListCartAndItems u c))-1) (freqListCartAndItems u c)

--freqListUsers
freqListUsers:: String -> [(String, Int)]
freqListUsers u = freqListUsersHelper (purchasesIntersection (fillFreqList u) (getAllUsersStatsWithoutindx (getuser (fillFreqList u) (getAllUsersStats purchasesHistory)) (getAllUsersStats purchasesHistory) 1)) items
freqListUsersHelper :: [[(String, [(String, Int)])]] -> [String] -> [(String, Int)]
freqListUsersHelper fl [] = []
freqListUsersHelper fl (it:ir) |(freqListUsersHelperHelper fl it) /= 0 = (it,freqListUsersHelperHelper fl it):freqListUsersHelper fl ir
    |otherwise = freqListUsersHelper fl ir

freqListUsersHelperHelper :: [[(String,[(String,Int)])]] -> String -> Int
freqListUsersHelperHelper [] it = 0
freqListUsersHelperHelper (curr:nxt) it = freqListUsersHelperHelperHelper curr it + freqListUsersHelperHelper nxt it

freqListUsersHelperHelperHelper :: [(String,[(String,Int)])] -> String -> Int
freqListUsersHelperHelperHelper [] it = 0
freqListUsersHelperHelperHelper ((i,l):r) it = freqListUsersHelperHelperHelperHelper l it + freqListUsersHelperHelperHelper r it

freqListUsersHelperHelperHelperHelper :: [(String,Int)] -> String -> Int
freqListUsersHelperHelperHelperHelper [] it = 0
freqListUsersHelperHelperHelperHelper ((n,freq):r) it |n == it = freq + freqListUsersHelperHelperHelperHelper r it
    |otherwise = freqListUsersHelperHelperHelperHelper r it

getAllUsersStatsWithoutindx indx [] curr = []
getAllUsersStatsWithoutindx indx (st:sr) curr |indx == curr = getAllUsersStatsWithoutindx (indx) sr (curr+1)
    |otherwise = st:getAllUsersStatsWithoutindx (indx) sr (curr+1)

--recommendBasedOnUsers
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers u = recommendEmptyCartHelper (randomZeroToX (freqsum (freqListUsers u))-1) (freqListUsers u)
--purchasesIntersection
purchasesIntersection :: [(String, [(String, Int)])] -> [(String, [(String,[(String, Int)])])] -> [[(String, [(String, Int)])]] 
purchasesIntersection fl [] = []
purchasesIntersection fl ((n,fl2):rest) = purchasesIntersectionHelper fl fl2:purchasesIntersection fl rest

--intersection of two fl
purchasesIntersectionHelper :: [([Char],[([Char],Int)])] -> [([Char],[([Char],Int)])] -> [([Char], [([Char], Int)])] 
purchasesIntersectionHelper [] [] = []
purchasesIntersectionHelper ((n1,c1):r1) ((n2,c2):r2) |c1 /= [] && c2 /= [] = (n1,combine c1 c2):purchasesIntersectionHelper r1 r2
    |otherwise = purchasesIntersectionHelper r1 r2

combine c1 c2 = merge (freqListCartAndItemsHelper c1 c2) (freqListCartAndItemsHelperItems c1 c2)

-- get the user place in users list
getuser r [] = -100
getuser r ((n,fl):fr) | r == fl = 1
 |otherwise = 1+ getuser r fr
-- createEmptyFreqList
createEmptyFreqList :: [a] -> [(a, [b])]

createEmptyFreqList [] = []
createEmptyFreqList (item:rest) = (item,[]):createEmptyFreqList rest

-- fillFreqList
fillFreqList :: String -> [(String,[(String,Int)])]
fillFreqList name =  fillFreqListhelp (createEmptyFreqList items) name

-- helper
fillFreqListhelp :: [(String,[(String,Int)])] -> String -> [(String,[(String,Int)])]
fillFreqListhelp [] name = []
fillFreqListhelp ((i,lr):r) name = (fillhelp1 i (getUserCarts name purchasesHistory) items): fillFreqListhelp r name

-- frequency of each item with i 
fillhelp1 :: String -> [[String]] -> [String] -> (String,[(String,Int)])
fillhelp1 i cs (it:ir) = (i,fillhelp2 i cs (it:ir))
fillhelp2 :: String -> [[String]] -> [String] -> [(String,Int)]
fillhelp2 i cs [] = []
fillhelp2 i cs (it:ir) |fillhelp3 i cs it /= 0 = (it,fillhelp3 i cs it) : fillhelp2 i cs ir
    |otherwise = fillhelp2 i cs ir

-- frequency with one item with i 
fillhelp3 :: String -> [[String]] -> String ->Int
fillhelp3 i [] it = 0
fillhelp3 i (c1:cr) it|exists i c1 && exists it c1 && not(equals i it)= inversegetfreq it c1 + fillhelp3 i cr it
    |otherwise = fillhelp3 i cr it

-- frequency of an item in a list
inversegetfreq :: String -> [String] ->Int
inversegetfreq item [] = 0 
inversegetfreq item (h:t) |equals h item = 1 + inversegetfreq item t 
    |otherwise = inversegetfreq item t

-- get user carts
getUserCarts :: String -> [(String,[[String]])] -> [[String]]
getUserCarts name [] = []
getUserCarts name ((n,i):t) |equals name n = i
 |otherwise = getUserCarts name t

-- getAllUsersStats
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats purchasesHistory = getAllUsersStatsHelp users
getAllUsersStatsHelp :: [String] -> [(String,[(String,[(String,Int)])])]
getAllUsersStatsHelp [] = []
getAllUsersStatsHelp (u1:ur) = (u1,fillFreqList u1):getAllUsersStatsHelp ur




