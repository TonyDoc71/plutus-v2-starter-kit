module Main where

import Cardano.Api
import Options.Applicative
import Hello.Contract

data Options = Options
    { outputFile :: String }

parser :: Parser FilePath
parser = argument str (metavar "FILENAME" <> help "Output filename for the Plutus script")

main :: IO ()
main = do
    filename <- execParser (info parser briefDesc)
    result <- writeFileTextEnvelope filename Nothing serializedScript
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> return ()
