module Polimorfismo where

    --Funcion dosVeces de forma polimorfica

   -- dosVecesPolimorfica :: (a->a)->a->a
    --dosVecesPolimorfica f x = f (f x)

    --Combinar 2 listas de tuplas
    mezcla :: [(a,b)]->[(c,d)]->[((a,c),(b,d))]
    mezcla [] _ = []
    mezcla _ [] = []
    mezcla ((x1,x2):xs) ((y1,y2):ys) = ((x1,y1), (x2,y2)): mezcla xs ys

    mezcla' :: [(a,b)]->[(c,d)]->[((a,c),(b,d))]
    mezcla' lista1 lista2 = [mezclaAux (lista1!!i) (lista2!!i)| i <- [0..(length lista1)-1]]

    mezclaAux :: (a,b)->(c,d)->((a,c),(b,d))
    mezclaAux (x,y) (u,v) = ((x,u),(y,v))

    --Funcion init
    init' :: [a]->[a]
    init' [] = error "Lista Vacia"
    init' [_] = []
    init' (x:xs) = x:init' xs 

    last' :: [a]->a
    last' [] = error "Lista Vacia"
    last' [x] = x
    last' (_:xs) = last xs

    --zipWith polimorfismo
    zipWith' :: (a->b->c)->[a]->[b]->[c]
    zipWith' _ [] _ = []
    zipWith' _ _ [] = []
    zipWith' f (x:xs) (y:ys) = (f x y): (zipWith' f xs ys)