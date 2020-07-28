module.exports = {
  plugins: [
    require('postcss-easy-import'),
    require('tailwindcss')('./pages/assets/css/tailwind.config.js'),
    require('autoprefixer'),
  ],
}
