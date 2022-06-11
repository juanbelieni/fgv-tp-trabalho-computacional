module Main where
import           Control.Monad                  ( replicateM, foldM )
import           System.Random                  ( randomIO )
import           Text.Printf                    ( printf )

-- Valores

p :: Float
p = 0.28

a :: Float
a = 3.2

n :: Int
n = 570

m :: Int
m = 3750

-- Distribuições

newtype Dist a
  = Dist { gerar :: IO a }

instance Functor Dist where
  fmap f (Dist g) = Dist (fmap f g)

uniforme :: Dist Float
uniforme = Dist (randomIO :: IO Float)

geom :: Float -> Dist Int
geom p = (\x -> ceiling $ logBase (1 - p) (1 - x)) <$> uniforme

-- Utilitários

media :: Fractional a => [a] -> a
media xs = sum xs / fromIntegral (length xs)

desvio :: Floating a => [a] -> a
desvio xs =
  let m = media xs
  in  sqrt $ sum ((** 2) . (m -) <$> xs) / fromIntegral (length xs)

format :: Float -> String
format = printf "%f"

-- Simulações

simularFigurinhas :: IO ()
simularFigurinhas = do
  xs <- replicateM 1000 $ fromIntegral . sum <$> mapM
    (\i -> gerar $ geom (fromIntegral (n - i) / fromIntegral n))
    [0 .. n - 1]

  let probabilidadeMaiorQueM =
        fromIntegral (length $ filter (> fromIntegral m) xs) / 1000

  putStrLn "Figurinhas"
  putStrLn $ "Média da simulação:\t\t" ++ format (media xs)
  putStrLn "Média teórica:\t\t\t3946.52550846739"
  putStrLn $ "Desvio padrão da simulação:\t" ++ format (desvio xs)
  putStrLn "Desvio padrão teórico:\t\t727.958139262227"
  putStrLn $ "P(No. fig > M):\t\t\t" ++ format probabilidadeMaiorQueM

main :: IO ()
main = do
  simularFigurinhas
