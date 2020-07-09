# Type-level printf

This module provides a function `printf`, that has an ambiguous type, but it
allows us to write some things like

```
>>> printf @ (Bool :<< " -> " :<< Int :<< " Top right thing should be symbol") True 4
```
And get
```
"True -> 4 Top right thing should be symbol" :: String
```

with `TypeApplications` and `DataKinds`.

This function uses Show instances for `Int` and `Bool` in this case.

All possible errors with this functions are type-level.

There is also a `pure_printf` function with normal type, but it requires to
construct a proxy value for such type.
