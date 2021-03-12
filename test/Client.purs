module Test.Client where 

import Prelude 

import Test.Client.General as General 
import Test.Spec (Spec, describe)

specs :: Spec Unit 
specs = describe "Client" do 
  General.spec