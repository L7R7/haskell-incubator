# servant-type-roles

This is very much inspired by [https://github.com/haskell-servant/servant-auth/issues/172#issuecomment-680699441](https://github.com/haskell-servant/servant-auth/issues/172#issuecomment-680699441).

Small demo project showing how a web API with several endpoints only accessible if the user is authenticated and has a certain permission can be implemented using Servant and type level combinators for the permissions.

Uses a cookie-based/JWT/tbd authentication.

The project contains a test suite that shows the intended behavior
