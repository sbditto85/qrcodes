module Main where

import Lib

main :: IO ()
main =
  testItOut Nothing
  -- testItOut $ Just "otpauth%3A%2F%2Ftotp%2FLVTAUTH%3Ademo%40liveviewtech.com%3Fsecret%3D3EQODDJZEDCY3XORXFGULDNJDGWV3NGI2WY6RX%26issuer%3DLVT"

-- 6H8cNH1cwtnSQcjFkWm25lz7

-- otpauth://totp/LVTAUTH:demo@liveviewtech.com?secret=3EQODDJZEDCY3XORXFGULDNJDGWV3NGI2WY6RX&issuer=LVT

-- otpauth://totp/LVTAUTH:demo@liveviewtech.com?secret=GZEDQY2OJAYWG53UNZJVCY3KIZVVO3JSGVWHUNY&issuer=LVT
-- GZEDQY2OJAYWG53UNZJVCY3KIZVVO3JSGVWHUNY

-- otpauth%3A%2F%2Ftotp%2FLVTAUTH%3Ademo%40liveviewtech.com%3Fsecret%3D3EQODDJZEDCY3XORXFGULDNJDGWV3NGI2WY6RX%26issuer%3DLVT
