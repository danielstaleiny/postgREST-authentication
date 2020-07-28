const purgecss = require('@fullhuman/postcss-purgecss')({
  content: ['_site/**/*.html'],
  // Include any special characters you're using in this regular expression
  defaultExtractor: (content) => content.match(/[A-Za-z0-9-_:\.\/]+/g) || [],
})

module.exports = {
  plugins: [
    require('postcss-easy-import'),
    require('tailwindcss')('./pages/assets/css/tailwind.config.js'),
    require('autoprefixer'),
    ...(process.env.NODE_ENV === 'production' ? [purgecss] : []),
  ],
}
