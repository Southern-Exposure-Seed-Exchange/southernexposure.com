# Development guide

- Run backend command in 1 terminal

- Run webpack command in another terminal
```
clear; npm run -- watch --host 127.0.0.1 --server-type http
```

- Run tailwind command in another terminal
```
clear; npm run tailwind
```

# Working with static page

- Write the static page view in `Mock` module, and use it for local development
- Convert elm view code to html (using chatgpt or other), then test the final result with the admin panel
- Remove the mock code, use the generated html to update in the prod server


## Note:
- The browser won't be automatically refresh. You'll have to 'ctrl+r'
- Elm hot reload only work with elm-webpack-loader version 6
