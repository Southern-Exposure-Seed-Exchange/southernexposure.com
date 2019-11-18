var path = require('path');
var webpack = require('webpack');
var { CleanWebpackPlugin } = require('clean-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var SriWebpackPlugin = require('webpack-subresource-integrity');
var FaviconsWebpackPlugin = require('favicons-webpack-plugin');
var ImageminWebpackPlugin = require('imagemin-webpack-plugin').default;

var isProduction = process.env.NODE_ENV === 'production';


module.exports = {
  mode: isProduction ? 'production' : 'development',
  entry: {
    vendor: [
      'bootstrap',
      'font-awesome-sass-loader',
    ],
    styles: [
      './src/styles.sass',
    ],
    app: [
      './src/index.js',
    ],
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].' + (isProduction ? '[contenthash]' : '[hash]') + '.js',
    crossOriginLoading: 'anonymous',
  },

  optimization: {
    runtimeChunk: 'single'
  },

  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/,],
        use: [
          {
            loader: 'elm-webpack-loader',
            options: {
              debug: !isProduction,
              optimize: isProduction,
              cwd: __dirname,
            },
          },
        ],
      },
      {
        test: /\.js$/,
        exclude: [/node_modules/,],
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ["@babel/preset-env", {
                "targets": {
                  "browsers": [">= 1%",]
                },
              }],
            ],
          },
        },
      },
      {
        test: /\.sass$/,
        exclude: [/node_modules/,],
        use: [ 'style-loader', 'css-loader', 'postcss-loader', 'sass-loader' ],
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 10000,
              mimetype: 'application/font-woff',
              name: '[name].[hash].[ext]'
            },
          },
        ],
      },
      {
        test: /\.(jpg|png|svg|ttf|eot)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[name].[hash].[ext]',
              outputPath: 'static/',
            },
          },
        ],
      },
    ],
    noParse: [/.elm$/]
  },

  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      inject: false,
      template: require('html-webpack-template'),

      mobile: true,
      lang: 'en-US',

      title: 'Southern Exposure Seed Exchange',
      links: ['https://fonts.googleapis.com/css?family=Crimson+Text|Glass+Antiqua'],
      scripts: ['https://checkout.stripe.com/checkout.js'],
      xhtml: true,
      hash: false,
      baseHref: '/',
    }),
    new CopyWebpackPlugin([
      { from: 'src/static',
        to: 'static',
        ignore: ['PoorRichard.ttf'],
      },
    ]),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
      Popper: 'popper.js/dist/umd/popper.js',
    }),
    new SriWebpackPlugin({
      hashFuncNames: ['sha512'],
      enabled: isProduction,
    }),
    new FaviconsWebpackPlugin({
      logo: './src/favicon.png',
      prefix: 'static/favicon-[hash]/',
      cache: true,
      inject: 'force',
      favicons: {
        appName: "Southern Exposure Seed Exchange",
        appShortName: "SESE Store",
        appDescription: "Southern Exposure's Retail Store",
        developerName: "Pavan Rikhi",
        developerURL: "https://github.com/Southern-Exposure-Seed-Exchange/southernexposure.com/",
        background: "#ffffff00",
        theme_color: "#158312",
        version: "v0.9.0",
      },
    }),
    new ImageminWebpackPlugin({
      test: /^.*favicon.*\.png$/,
      disable: !isProduction,
    }),
  ],

  devServer: {
    inline: true,
    host: '0.0.0.0',
    disableHostCheck: true,
    historyApiFallback: true,
    stats: {
      colors: true,
      chunks: false,
    },
    proxy: {
      '/api/*': {
        target: 'http://localhost:3000',
        changeOrigin: true,
        pathRewrite: { "^/api/": "/" },
      },
      '/media/*': {
        target: 'http://localhost:3000',
        changeOrigin: true,
      }
    },
  },

}

/* Returns true if the module is an NPM dependency. */
function isExternal(module) {
  var context = module.context;

  if (typeof context !== 'string') {
    return false;
  }
  return context.indexOf('node_modules') !== -1;
}
