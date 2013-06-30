module HaSpades.Serverlist.BnSURLs where

import Network.URL

buildAndShootServices :: Host
buildAndShootServices = Host { protocol = HTTP False
                             , host     = "services.buildandshoot.com"
                             , port     = Nothing
                             }

getBnS :: String -> URL -- version number | 'powerthirst'
getBnS gameVer = URL { url_type   = Absolute $ buildAndShootServices
                     , url_path   = "serverlist.json"
                     , url_params = [("version", gameVer)]
                     }
