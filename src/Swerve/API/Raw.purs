module Swerve.API.Raw where 

type Raw path = Raw' path ()

data Raw' (path :: Symbol) (specs :: # Type)