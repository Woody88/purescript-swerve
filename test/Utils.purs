module Test.Utils where  

import Effect 
import Node.Stream (Readable)

foreign import newStream :: String -> Effect (Readable ())