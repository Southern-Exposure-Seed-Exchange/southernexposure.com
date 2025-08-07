var path = require('path');
var webpack = require('webpack');
var { CleanWebpackPlugin } = require('clean-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var SriWebpackPlugin = require('webpack-subresource-integrity');
var FaviconsWebpackPlugin = require('favicons-webpack-plugin');
var ImageminWebpackPlugin = require('imagemin-webpack-plugin').default;
var CspHtmlWebpackPlugin = require('csp-html-webpack-plugin');
var MiniCssExtractPlugin = require('mini-css-extract-plugin');
var TerserJSPlugin = require('terser-webpack-plugin');
var OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
var RobotsTxtPlugin = require('robotstxt-webpack-plugin');

var isProduction = process.env.NODE_ENV === 'production';
var isHelcimProduction = process.env.HELCIM_ENV === 'production';

var helcimUrl = isHelcimProduction ? "https://southern-exposure-seed-exchange.myhelcim.com" : "https://test-southern-exposure-seed-exchange.myhelcim.com";

const GA_MEASUREMENT_ID = process.env.GA_MEASUREMENT_ID;


module.exports = {
  mode: isProduction ? 'production' : 'development',
  entry: {
    styles: [
      './src/styles.sass',
      'bootstrap',
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
    runtimeChunk: 'single',
    minimizer: [new TerserJSPlugin({})],
    minimize: isProduction,
    splitChunks: {
      chunks: 'all',
      automaticNameDelimiter: '.',
    },
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
        use: [
          { loader: MiniCssExtractPlugin.loader,
            options: {
            },
          }, 'css-loader', 'postcss-loader', 'sass-loader'
        ],
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
      links: [
        'https://fonts.googleapis.com/css?family=Crimson+Text|Glass+Antiqua',
        { rel: "canonical", href: "https://www.southernexposure.com" }

      ],
      scripts:
        [ 'https://secure.helcim.app/helcim-pay/services/start.js',
          { "src": 'https://www.googletagmanager.com/gtag/js?id=' + GA_MEASUREMENT_ID,
            "async": true
          }
        ],
      meta: [
        // TODO: Add ability to override domains
        { property: "og:site_name", content: "Southern Exposure Seed Exchange" },
        { property: "og:type", content: "website" },
        { property: "og:image", content: "https://www.southernexposure.com/static/img/logos/sese.png" },
        { property: "og:title",
          content: "Southern Exposure Seed Exchange, Saving the Past for the Future" },
        { property: "og:description", content: "" },
        { property: "og:url", content: "https://www.southernexposure.com/" },
      ],
      xhtml: true,
      hash: false,
      baseHref: '/',
    }),
    new CopyWebpackPlugin({patterns: [
      { from: 'src/static',
        to: 'static',
        globOptions: {ignore: ['PoorRichard.ttf']},
      },
    ],}),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
    }),
    new webpack.DefinePlugin({
      GA_MEASUREMENT_ID: JSON.stringify(GA_MEASUREMENT_ID),
      helcimUrl: JSON.stringify(helcimUrl),
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
        version: "v1.04.08",
      },
    }),
    new CspHtmlWebpackPlugin({
      'base-uri': "'self'",
      'upgrade-insecure-requests': [],
      'default-src': ["'self'", "data:"],
      'object-src': "'none'",
      'connect-src': ["'self'", "https://api.helcim.app", "https://www.google-analytics.com", "https://www.googletagmanager.com" ],
      'frame-src': ["'self'", "https://www.farmraiser.com", "https://secure.helcim.app"],
      'script-src': ["'self'", "'unsafe-inline'", "'unsafe-eval'", "https://www.googletagmanager.com", "https://www.google-analytics.com/"],
      'img-src': ["'self'", "data:", "https://www.googletagmanager.com", "https://www.google-analytics.com/", "https://stats.g.doubleclick.net/", "https://www.google.com"],
      'font-src': ["'self'", "data:", "https://fonts.gstatic.com"],
      'style-src': ["'self'", "'unsafe-inline'", "https://fonts.googleapis.com"],
    }, {
      hashEnabled: {
        'style-src': false,
      },
      nonceEnabled: {
        'style-src': false,
      },
    }),
    new ImageminWebpackPlugin({
      test: /^.*favicon.*\.png$/,
      disable: !isProduction,
    }),
    new MiniCssExtractPlugin({
      filename: '[name].[contenthash].css',
    }),
    new RobotsTxtPlugin({
      policy: [
        { userAgent: "*",
          crawlDelay: 2,
          allow: isProduction ? "/" : false,
          disallow: isProduction ? ["/admin"] : "/"
        },
      ],
      sitemap: "https://www.southernexposure.com/sitemap-index.xml",
      host: "https://www.southernexposure.com",
    }),
  ],

  devServer: {
    host: '0.0.0.0',
    allowedHosts: 'all',
    historyApiFallback: true,
    proxy: {
      '/api/*': {
        target: 'http://127.0.0.1:3000',
        secure: false,
        changeOrigin: true,
        pathRewrite: { "^/api/": "/" },
      },
      '/media/*': {
        target: 'http://127.0.0.1:3000',
        secure: false,
        changeOrigin: true,
      },
      '/blog/*': {
        target: 'https://vps.southernexposure.com/blog/',
        changeOrigin: true,
      },
      '/newsletter/*': {
        target: 'http://vps.southernexposure.com/newsletter/',
        changeOrigin: true,
      }
    },
    https: {
      key: '../local.southernexposure.com+2-key.pem',
      cert: '../local.southernexposure.com+2.pem',
    }
  },

}
