module Swerve.API.Status.KnownStatus where 

import Network.HTTP.Types.Status
import Type.Proxy (Proxy)

-- | Type level string that corresponds to a HTTP status code 
class KnownStatus (status :: Symbol) where 
  statusVal :: Proxy status -> Status 

instance status100 :: KnownStatus "100" where
  statusVal _ = status100

instance status101 :: KnownStatus "101" where
  statusVal _ = status101

instance status200 :: KnownStatus "200" where
  statusVal _ = status200

instance status201 :: KnownStatus "201" where
  statusVal _ = status201

instance status202 :: KnownStatus "202" where
  statusVal _ = status202

instance status203 :: KnownStatus "203" where
  statusVal _ = status203

instance status204 :: KnownStatus "204" where
  statusVal _ = status204

instance status205 :: KnownStatus "205" where
  statusVal _ = status205

instance status206 :: KnownStatus "206" where
  statusVal _ = status206

instance status300 :: KnownStatus "300" where
  statusVal _ = status300

instance status301 :: KnownStatus "301" where
  statusVal _ = status301

instance status302 :: KnownStatus "302" where
  statusVal _ = status302

instance status303 :: KnownStatus "303" where
  statusVal _ = status303

instance status304 :: KnownStatus "304" where
  statusVal _ = status304

instance status305 :: KnownStatus "305" where
  statusVal _ = status305

instance status307 :: KnownStatus "307" where
  statusVal _ = status307

instance status308 :: KnownStatus "308" where
  statusVal _ = status308

instance status400 :: KnownStatus "400" where
  statusVal _ = status400

instance status401 :: KnownStatus "401" where
  statusVal _ = status401

instance status402 :: KnownStatus "402" where
  statusVal _ = status402

instance status403 :: KnownStatus "403" where
  statusVal _ = status403

instance status404 :: KnownStatus "404" where
  statusVal _ = status404

instance status405 :: KnownStatus "405" where
  statusVal _ = status405

instance status406 :: KnownStatus "406" where
  statusVal _ = status406

instance status407 :: KnownStatus "407" where
  statusVal _ = status407

instance status408 :: KnownStatus "408" where
  statusVal _ = status408

instance status409 :: KnownStatus "409" where
  statusVal _ = status409

instance status410 :: KnownStatus "410" where
  statusVal _ = status410

instance status411 :: KnownStatus "411" where
  statusVal _ = status411

instance status412 :: KnownStatus "412" where
  statusVal _ = status412

instance status413 :: KnownStatus "413" where
  statusVal _ = status413

instance status414 :: KnownStatus "414" where
  statusVal _ = status414

instance status415 :: KnownStatus "415" where
  statusVal _ = status415

instance status416 :: KnownStatus "416" where
  statusVal _ = status416

instance status417 :: KnownStatus "417" where
  statusVal _ = status417

instance status418 :: KnownStatus "418" where
  statusVal _ = status418

instance status422 :: KnownStatus "422" where
  statusVal _ = status422

instance status426 :: KnownStatus "426" where
  statusVal _ = status426

instance status428 :: KnownStatus "428" where
  statusVal _ = status428

instance status429 :: KnownStatus "429" where
  statusVal _ = status429

instance status431 :: KnownStatus "431" where
  statusVal _ = status431

instance status500 :: KnownStatus "500" where
  statusVal _ = status500

instance status501 :: KnownStatus "501" where
  statusVal _ = status501

instance status502 :: KnownStatus "502" where
  statusVal _ = status502

instance status503 :: KnownStatus "503" where
  statusVal _ = status503

instance status504 :: KnownStatus "504" where
  statusVal _ = status504

instance status505 :: KnownStatus "505" where
  statusVal _ = status505

instance status511 :: KnownStatus "511" where
  statusVal _ = status511