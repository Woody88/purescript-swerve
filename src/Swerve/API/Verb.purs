module Swerve.API.Verb where 

foreign import kind VERB
foreign import data CONNECT :: VERB
foreign import data HEAD    :: VERB
foreign import data GET     :: VERB
foreign import data POST    :: VERB
foreign import data PATCH   :: VERB
foreign import data PUT     :: VERB
foreign import data DELETE  :: VERB
foreign import data OPTIONS :: VERB
foreign import data TRACE   :: VERB

data Verb (verb :: VERB) (path :: Symbol) (specs :: # Type)

type Connect = Verb CONNECT 
type Head    = Verb HEAD 
type Get     = Verb GET 
type Pose    = Verb POST 
type Patch   = Verb PATCH 
type Put     = Verb PUT
type Delete  = Verb DELETE 
type Options = Verb OPTIONS 
type Trace   = Verb TRACE 