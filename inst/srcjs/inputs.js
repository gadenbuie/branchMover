function disableAllChangeButtons() {
  document
    .querySelectorAll('.js-change-branch')
    .forEach(function(el) {
      el.setAttribute('disabled', true)
      el.classList.add('btn-disabled')
    })
}

function enableAllChangeButtons() {
  document
    .querySelectorAll('.js-change-branch')
    .forEach(function(el) {
      el.innerHTML = 'Change Default Branch'
      el.removeAttribute('disabled')
      el.classList.remove('btn-disabled')
    })
}

function addSpinner(el) {
  el.innerHTML = '<span class=\"spinner-border spinner-border-sm\"' +
    'role=\"status\" aria-hidden=\"true\"></span> Changing Branch'
}

$('#repos').on('click', '.js-change-branch', function(ev) {
  const {repo, action} = ev.target.dataset
  Shiny.setInputValue('change_branch', {repo, action})
  addSpinner(ev.target)
})

$(document).on('change', '#new_default', function(ev) {
  window.branchMoverNewDefaultBranch = ev.target.value
})

Shiny.addCustomMessageHandler('set_change_branch_state', function(x) {
  x === 'disable' ? disableAllChangeButtons() : enableAllChangeButtons()
})
