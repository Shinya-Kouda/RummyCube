--ラミィキューブ
module RummyCube
( diff1
, remove2run
, takeRuns2
, modify
, modify2
, zipTrue
, removeList
, takeGroups
, removeGroups
, Color(..)
) where
import Data.List
--状態
----キューブ
------場所（山、プレイヤー１~４、場）
data Color = Red | Blue | Orange | Black deriving (Show, Eq)
data State = State { mount :: [(Int,Color)]
                   , player1 :: ([(Int,Color)],Bool)
                   , player2 :: ([(Int,Color)],Bool)
                   , player3 :: ([(Int,Color)],Bool)
                   , player4 :: ([(Int,Color)],Bool)
                   , field :: [[(Int,Color)]]
                   , nextPlayer :: Int
                   , finished :: Bool} deriving (Show)
--遷移
----方策に従って考えたときに出すものがあるなら出し、ないならカードを引く
--方策
----条件を満たしつつつながらないキューブを最低にする組み合わせを出力する
----最初で30を超えるものが出せない→引く
----最初に30を超えるものがあるとき出す
----貪欲法
------出せるものは出す
----一発狙い
------手元でつながったものはとっておき、つながってないもので出せるものを出す
------行動が終わった後につながっていないものをなるべく少なくなるようにする
--policy True = True
--policy mount player1 player2 player3 player4 field nextPlayer finished
--    | nextPlayer == 1 = policy mount player1 player2 player3 player4 field 2

--action :: [Int] -> ([Int],Bool) -> [[Int]] -> ([Int] , [Int] , [[Int]])
--action [] player field = [] player field
--action mount ([],clear30) field = mount ([],clear30) field


diff1 :: [Int] -> [Bool]
diff1 [] = []
diff1 (x:xs)
    | xs == [] = []
    | otherwise = (head xs - x == 1) : (diff1 xs)

remove2run :: [Bool] -> [Bool]
remove2run [] = []
remove2run list@(x:xs)
    | (head $ map length $ group list) <= 1 = False : remove2run xs
    | otherwise = (head $ group list) ++ (remove2run $ concat $ tail $ group list)

modify :: [Bool] -> [Bool] -> String
modify [] _ = ""
modify _ [] = ""
modify (x1:x2:[]) (y1:y2:[])
    | x1 == False && y1 == True && x2 == False && y2 == False = "True],[False]]"
    | otherwise = (show $ x1 || y1) ++ "," ++ (show $ x2 || y2) ++ "]]"
modify (x1:x2:xs) (y1:y2:ys)
    | x1 == False && y1 == False && x2 == True && y2 == False = "False],[" ++ modify (x2:xs) (y2:ys)
    | x1 == False && y1 == True && x2 == True && y2 == False = "True],[" ++ modify (x2:xs) (y2:ys)
    | x1 == False && y1 == True && x2 == False && y2 == False = "True],[" ++ modify (x2:xs) (y2:ys)
    | otherwise = (show $ x1 || y1) ++ "," ++ modify (x2:xs) (y2:ys)

modify2 :: String -> [[Bool]]
modify2 str = read ("[[" ++ str) :: [[Bool]]

takeRuns :: [Int] -> [Bool] -> [Int]
takeRuns groupedCube takeList = [a | (a,b) <- zip groupedCube takeList, b == True]

zipTrue :: [Bool] -> [Int] -> [Int]
zipTrue _ [] = []
zipTrue [] _ = []
zipTrue (b:bs) (i:is)
    | b == False = zipTrue bs is
    | b == True = i:(zipTrue bs is)

takeRuns2 :: [[Bool]] -> [Int] -> ([Bool] -> [Int] -> [Int]) -> [[Int]]
takeRuns2 [[]] _ _ = []
takeRuns2 _ [] _ = []
takeRuns2 (bll:blls) il zipTrue
    | (zipTrue bll il) == [] = (takeRuns2 blls (drop (length bll) il) zipTrue)
    | otherwise = (zipTrue bll il) : (takeRuns2 blls (drop (length bll) il) zipTrue)

removeList :: [Int] -> [Int] -> [Int]
removeList list [] = list
removeList [] _ = []
removeList list@(l:ls) runs@(r:rs)
    | l `elem` runs = removeList ls [x|x<-runs,x/=l]
    | otherwise = l : (removeList ls runs)

takeGroups :: [(Int,Color)] -> [[(Int,Color)]]
takeGroups list = [ map head . group $ xs| xs <- [[(m,c)|(m,c)<-list,m == n] | n <- [1..25]], (length $ map head $ group $ xs) >= 3 ]

removeGroups :: [(Int,Color)] -> [(Int,Color)] -> [(Int,Color)]
removeGroups list [] = list
removeGroups [] _ = []
removeGroups list@(l:ls) groups@(g:gs)
    | l `elem` groups = removeGroups ls [x|x<-groups,x/=l]
    | otherwise = l : (removeGroups ls groups)