$(document).ready(function() {
  const icons = {
    "success": '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-check-square-fill text-success" viewBox="0 0 16 16"><path d="M2 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2zm10.03 4.97a.75.75 0 0 1 .011 1.05l-3.992 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.75.75 0 0 1 1.08-.022z"/></svg>',
    "danger": '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-exclamation-circle-fill text-danger" viewBox="0 0 16 16"><path d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zM8 4a.905.905 0 0 0-.9.995l.35 3.507a.552.552 0 0 0 1.1 0l.35-3.507A.905.905 0 0 0 8 4zm.002 6a1 1 0 1 0 0 2 1 1 0 0 0 0-2z"/></svg>'
  }

  Shiny.addCustomMessageHandler("toastify", function({title, body, state}) {
    const toast = document.createElement("div")
    toast.classList = "toast align-items-center"
    toast.setAttribute("role", "alert")
    toast.setAttribute("aria-live", "assertive")
    toast.setAttribute("aria-atomic", "true")
    toast.setAttribute("data-bs-animation", "true")
    toast.setAttribute("data-bs-autohide", "true")
    toast.setAttribute("data-bs-delay", "5000")

    toast.innerHTML = `<div class="toast-header">
      <strong class="me-auto">${title}</strong>
      <button type="button" class="btn-close me-2 m-auto" data-bs-dismiss="toast" aria-label="Close"></button>
      </div>
      <div class="toast-body d-flex align-items-center"><div class="me-2">${icons[state]}</div><div>${body}</div></div>
    </div>`

    const toaster = document.getElementById("toaster")
    toaster.appendChild(toast)
    var toasted = bootstrap.Toast.getOrCreateInstance(toast)
    toasted.show()

    toast.addEventListener("hidden.bs.toast", function() {
      toasted.dispose()
    })
  })
})
