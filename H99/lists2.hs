dupli :: [a] -> [a]
dupli [] = []
dupli (h:t) = [h] ++ dupli t

dupliMonada :: [a] -> [a]
dupliMonada s = s >>= \t -> [t, t]

test_dupli = dupli [1,2,3]
test_dupli2 = dupliMonada [1,2,3]


do_n :: a -> Int -> [a]
do_n _ 0 = []
do_n s n = [s] ++ do_n s (n-1)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (h:t) n = do_n h n ++ repli t n 

repliMonade :: [a] -> Int -> [a]
repliMonade s n = do
                    i<-s
                    do_n i n


repliMonade2 :: [a] -> Int -> [a]
repliMonade2 s n = s >>= \t -> do_n t n




test_repli = repli [1,2,3] 5
test_repli2 = repliMonade [1,2,3] 5
test_repli3 = repliMonade2 [1,2,3] 5



slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice l a b = [x | (x,y) <- zip l [1..] , a <= y && y<=b]

test_slice = slice ['a','b','c','d','e','f','g','h','i','k'] 3 7


rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs


removeAt :: Int -> [a] -> (a,[a])
removeAt n l = ( l!!n , take (n-1) l ++ drop n l )

test_remove = removeAt 2 "abcd"


data Many = Multiple Int Char | Single Char

decode_help :: Many -> String
decode_help (Single x) = [x]
decode_help (Multiple 0 _ ) = []
decode_help (Multiple n x ) = [x] ++ decode_help (Multiple (n-1) x)

decodeModified :: [Many] -> String
decodeModified [] = []
decodeModified (h:t) = decode_help h ++ decodeModified t

test_decode = decodeModified  [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']


decodeModifiedMonade :: [Many] -> String
decodeModifiedMonade l = do 
                            i<-l
                            decode_help i

test_decode2 = decodeModifiedMonade  [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
