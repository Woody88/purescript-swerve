module Test.Stream where 

import Effect 
import Node.Stream (Readable)

foreign import newStream :: String -> Effect (Readable ())