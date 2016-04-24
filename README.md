# haskell-api-helpers

Some api helpers.


## Misc

```
let opts = ApiOptions "https://localhost" "api" (Just "1") (Just "z-authorization") defaultWreqOpions True
runWith (getAt [] ([("key","value")] :: [(String,String)]) []) opts
```
