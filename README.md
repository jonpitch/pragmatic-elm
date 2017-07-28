# Pragmatic Elm
This is the demo app built from the [Pragmatic Studio](https://pragmaticstudio.com/) course [Building Web Apps with Elm](https://pragmaticstudio.com/courses/elm).

## To Build
- Start the server
```
cd server
npm install
node server.js
```

- Build Project
```
elm-package install
elm live Bingo.elm --open --debug --output=bingo.js
```