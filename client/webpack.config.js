var path = require('path');
var webpack = require('webpack');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');

var isProduction = process.env.NODE_ENV === 'production';


module.exports = {
  entry: {
    vendor: [
      'bootstrap',
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
    filename: '[name].' + (isProduction ? '[chunkHash]' : '[hash]') + '.js',
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
              warn: true,
              verbose: true,
              debug: !isProduction,
            },
          },
        ],
      },

      {
        test: /\.sass$/,
        exclude: [/node_modules/,],
        use: [ 'style-loader', 'css-loader', 'sass-loader' ],
      }

    ],
    noParse: [/.elm$/]
  },

  plugins: [
    new CleanWebpackPlugin(['dist']),
    new HtmlWebpackPlugin({
      inject: false,
      template: require('html-webpack-template'),

      appMountId: 'main',
      mobile: true,
      lang: 'en-US',

      title: 'Southern Exposure Seed Exchange',
      xhtml: true,
      hash: false,
    }),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
      Tether: 'tether',
    }),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'vendor',
      minChunks: function(module) { return isExternal(module); }
    }),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'runtime',
    }),
  ],

  devServer: {
    inline: true,
    host: '0.0.0.0',
    stats: {
      colors: true,
      chunks: false,
    },
  },

}

function isExternal(module) {
  var context = module.context;

  if (typeof context !== 'string') {
    return false;
  }
  return context.indexOf('node_modules') !== -1;
}
