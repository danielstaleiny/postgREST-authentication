'use strict'

module.exports = (ctx, id, title) => {
  return `
<div class="toggle pointer">  
  <input type="checkbox" value="selected" id="${id}" class="toggle-input hidden">
  <label for="${id}" class="toggle-label">
${title}
</label> 
  
  <div role="toggle" class="toggle-content">
${ctx}
 </div>
</div>
`
}
