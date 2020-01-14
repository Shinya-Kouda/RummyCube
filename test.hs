import RummyCube
import Data.List

list :: [Int]
list = [1,3,5,19,20,6,8,1,3,2,4,11,14,6,23,22,21]
--list = [1,3,4,6,9,11,13]
--list = [1,2,3,4,5,6,7,8,9,10,12,13,14,15,17,20,21,22]

groupedCube = map head . group $ sort list
before = diff1 groupedCube
after = remove2run before
bools = modify2 $ modify (after ++ [False]) (True:after)
result = takeRuns2 bools groupedCube zipTrue
removedList = removeList list (concat result)

list2 :: [(Int,Color)]
list2 = [(1,Blue)
        ,(3,Blue)
        ,(5,Blue)
        ,(19,Blue)
        ,(20,Blue)
        ,(6,Blue)
        ,(8,Blue)
        ,(1,Blue)
        ,(3,Blue)
        ,(2,Blue)
        ,(4,Blue)
        ,(11,Blue)
        ,(14,Blue)
        ,(6,Blue)
        ,(23,Blue)
        ,(22,Blue)
        ,(21,Blue)
        ,(1,Red)
        ,(3,Red)
        ,(4,Red)
        ,(6,Red)
        ,(9,Red)
        ,(11,Red)
        ,(13,Red)
        ,(1,Orange)
        ,(2,Orange)
        ,(3,Orange)
        ,(4,Orange)
        ,(5,Orange)
        ,(6,Orange)
        ,(7,Orange)
        ,(8,Orange)
        ,(9,Orange)
        ,(10,Orange)
        ,(12,Orange)
        ,(13,Orange)
        ,(14,Orange)
        ,(15,Orange)
        ,(17,Orange)
        ,(20,Orange)
        ,(21,Orange)
        ,(22,Orange)
        ]



result2 = removeGroups list2 (concat . takeGroups $ list2)

findRunOrGroup :: (Int, Color) -> [[(Int, Color)]] -> ([(Int, Color)],[[(Int, Color)]])

arrange :: ([(Int, Color)],[[(Int, Color)]],[[(Int, Color)]]) -> [([[(Int, Color)]],[[(Int, Color)]])]
arrange ([],myCubes,field) = [(myCubes,field)]
arrange (_,[],_) = []
arrange (wantToUse@(w:ws),myCubes,field)
    | 
