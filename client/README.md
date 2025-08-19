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

## Code format

- You can run `npm run format` to format all the elm code which use `elm-format` in the background
- If using Vscode, you can install elm extension and set this configuration in settings.json:
```json
    "[elm]": {
        "editor.formatOnSave": true
    }
```

## Working with static page

- Homepage can be edited in `src/Mock/home.html`
- Other pages are copied from the production at the moments
- After updating the html, update them in the admin panel


## Note:
- The browser won't be automatically refresh. You'll have to 'ctrl+r'
- Elm hot reload only work with elm-webpack-loader version 6
