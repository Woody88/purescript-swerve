module Test.Server where 

import Prelude 

import Test.Auth as Auth 
import Test.BasicAuth as BasicAuth
import Test.Server.General as General
import Test.Server.Verb as Verb
import Test.Spec (Spec, describe)

specs :: Spec Unit 
specs = describe "Server" do 
  BasicAuth.spec 
  Auth.spec 
  General.spec 
  Verb.spec