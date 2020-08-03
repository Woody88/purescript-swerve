module Swerve.API.RequestMethod where 

foreign import kind Verb
foreign import data CONNECT :: Verb
foreign import data HEAD    :: Verb
foreign import data GET     :: Verb
foreign import data POST    :: Verb
foreign import data PATCH   :: Verb
foreign import data PUT     :: Verb
foreign import data DELETE  :: Verb
foreign import data OPTIONS :: Verb
foreign import data TRACE   :: Verb

-- 
data Method (verb :: Verb) (specs :: # Type)

type Connect specs = Method CONNECT 
type Head    specs = Method HEAD 
type Get     specs = Method GET 
type Pose    specs = Method POST 
type Patch   specs = Method PATCH 
type Put     specs = Method PUT
type Delete  specs = Method DELETE 
type Options specs = Method OPTIONS 
type Trace   specs = Method TRACE 