# Development guide

- Run backend command in 1 terminal

- Run webpack command in another terminal
```
clear; npm run -- watch --host 127.0.0.1 --server-type http
```

- Run tailwind command in another terminal
```
clear; npm run tailwind-watch
```

# Working with static page

- Write the static page view in `Mock` module, and use it for local development
- Update `staticPageView` to use one of the Mock view function, to test it
- When everything is ready, copy the html content from the browser and input it into the admin panel
- A list of generated html can be found in `client/src/Mock` which contain `home.html`. More will be added when other static pages are ready.


## Note:
- The browser won't be automatically refresh. You'll have to 'ctrl+r'
- Elm hot reload only work with elm-webpack-loader version 6
