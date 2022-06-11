module Main where
import           Control.Monad                  ( replicateM )
import           Data.List                      ( sort )
import           Data.Vector.Unboxed            ( fromList )
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

exponencial :: Float -> Dist Float
exponencial lambda = (\x -> -log x / lambda) <$> uniforme

tempoChegada :: Dist Float
tempoChegada = exponencial 0.1

tempoAtendimento :: Dist Float
tempoAtendimento = g . f <$> uniforme
 where
  f x = 3 / (((1 - x) / (1 - p)) ** (1 / a))
  g x = max x 3

-- Utilitários

media :: Fractional a => [a] -> a
media xs = sum xs / fromIntegral (length xs)

desvio :: Floating a => [a] -> a
desvio xs =
  let m = media xs
  in  sqrt $ sum ((** 2) . (m -) <$> xs) / fromIntegral (length xs)

frac :: Num a => (a -> Bool) -> [a] -> Float
frac p xs = (fromIntegral . length $ filter p xs) / fromIntegral (length xs)

format :: Float -> String
format = printf "%f"

-- Simulações

simularTempoDeAtendimento :: IO ()
simularTempoDeAtendimento = do
  xs <- replicateM 10000 (gerar tempoAtendimento)

  putStrLn "Tempo de atendimento"
  putStrLn "Média calculada:\t\t3.98182"
  putStrLn $ "Média obtida na simulação:\t" ++ format (media xs)

simularTempoDeEspera :: IO ()
simularTempoDeEspera = do
  cs <- replicateM 10000 (gerar tempoChegada)
  as <- replicateM 10000 (gerar tempoAtendimento)

  let cs' = reverse $ foldl (\cs' c -> c + head cs' : cs') [head cs] (tail cs)
      ias = reverse $ foldl
        (\ias (ck, akm1) -> max (head ias + akm1) ck : ias)
        [head cs']
        (zip (tail cs') as)
      fas                = zipWith (+) ias as
      es                 = zipWith (-) ias cs'
      fracQueEspera      = frac (> 0) es
      fracQueEsperaMais3 = frac (>= 3) es
      fracTempoOcioso    = sum es / last fas

  putStrLn "\nTempo de espera"
  putStrLn $ "Média calculada:\t\t" ++ format (media es)
  putStrLn $ "Desvio padrão:\t\t\t" ++ format (desvio es)
  putStrLn $ "Frac. que espera:\t\t" ++ format fracQueEspera
  putStrLn $ "Frac. que espera mais de 3:\t" ++ format fracQueEsperaMais3
  putStrLn $ "Frac. tempo ocioso:\t\t" ++ format fracTempoOcioso

simularComDoisAtendentes :: IO ()
simularComDoisAtendentes = do
  cs <- replicateM 10000 (fromIntegral . ceiling <$> gerar tempoChegada)
  as <- replicateM 10000 (fromIntegral . ceiling <$> gerar tempoAtendimento)

  let cs' = reverse $ foldl (\cs' c -> c + head cs' : cs') [head cs] (tail cs)
      fas = reverse $ foldl
        (\fas (c, a) ->
          let ia = max (minimum . take 2 . reverse . sort $ fas) c
          in  ia + a : fas
        )
        (reverse $ take 2 $ zipWith (+) cs' as)
        (zip (drop 2 cs') (drop 2 as))
      ias                = zipWith (-) fas as
      es                 = zipWith (-) ias cs'
      fracQueEspera      = frac (> 0) es
      fracQueEsperaMais3 = frac (>= 3) es
      fracTempoOcioso    = sum es / last ias

  putStrLn "\nCom dois atendentes"
  putStrLn $ "Média calculada:\t\t" ++ format (media es)
  putStrLn $ "Desvio padrão:\t\t\t" ++ format (desvio es)
  putStrLn $ "Frac. que espera:\t\t" ++ format fracQueEspera
  putStrLn $ "Frac. que espera mais de 3:\t" ++ format fracQueEsperaMais3
  putStrLn $ "Frac. tempo ocioso:\t\t" ++ format fracTempoOcioso

main :: IO ()
main = do
  simularTempoDeAtendimento
  simularTempoDeEspera
  simularComDoisAtendentes


