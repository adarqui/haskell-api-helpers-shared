# haskell-api-helpers

Some api helpers.


## Misc

```
:set -XOverloadedStrings
let opts = ApiOptions "https://localhost" "api" (Just "1") (Just "z-authorization") defaultWreqOptions True
runWith (getAt ([("key","value")] :: [(String,String)]) []) opts
```
