'use strict'

module.exports = (content, value) => {
  return `<div>
<p>${value}</p>
${content}
</div`
}
