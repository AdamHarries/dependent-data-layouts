module MatrixMarket

import Data.Vect
import Prelude.File
import Text.Parser

parseFile : (fp: String) -> IO (Either FileError String)
parseFile fp = do
	cont <- readFile fp
	pure cont
	-- putStrLn cont 
	-- pure (Left fp)


parse : (fc: String) -> Type 
parse fc = Intl

git diff 48f011b 5cfe73a