import Data.List
import Data.Functor

build_graph :: [String] -> [(String,String)]
build_graph [] = []
build_graph (src:dst:rest) = (src,dst):build_graph rest

add_sink :: [(String,String)] -> [(String,String)]
add_sink l = 
    foldl (\acc t -> (t,""):acc) l (get_tasks l)

get_tasks :: [(String,String)] -> [String] 
get_tasks = foldl (\acc (src,dst) -> acc `union` [src,dst]) [] 

get_dsts :: [(String,String)] -> [String] 
get_dsts = foldl (\acc (src,dst) -> dst:acc) [] 

top_sort :: [(String,String)] -> Maybe [String]
top_sort [] = Just ([])  
top_sort g = 
    let t = head (sortBy compare  ((get_tasks g) \\ (get_dsts g))) in
    let rest = top_sort (filter (\(src,dst) -> src /= t && dst /= t) g) in
    case rest of
      Nothing -> Nothing
      Just l -> Just(t:l)

read_all_lines' :: IO [String] 
-- read_all_lines = do
--     all_lines <- getContents
--     return (lines all_lines)
read_all_lines' = lines $ getContents
   
main :: IO ()
main = do 
  case top_sort $ add_sink $ build_graph $ read_all_lines' of
    Nothing -> putStrLn "cycle"
    Just l -> mapM_ (\s -> putStrLn s) l
  
