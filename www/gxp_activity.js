// Controlled mode only. shinymanager's timeout counts only changed inputs as
// activity, so a user reading results is signed out without warning. This
// counts mouse, keyboard and scrolling as activity (at most one signal a
// minute) and warns two minutes before the sign-out.
(function () {
  var script = document.currentScript;
  var timeoutMs = parseFloat(script.getAttribute("data-timeout") || "15") * 60000;
  var warnMs = 2 * 60000, last = Date.now(), lastSent = 0, box = null, timer = null;

  function send() {
    if (!(window.Shiny && Shiny.setInputValue)) return;
    lastSent = Date.now();
    Shiny.setInputValue(".shinymanager_timeout", lastSent, { priority: "event" });
  }
  function activity() {
    last = Date.now();
    hide();
    if (last - lastSent > 60000) send();
  }
  function hide() { if (box) { box.remove(); box = null; } }
  function show(left) {
    if (!box) {
      box = document.createElement("div");
      box.setAttribute("role", "alertdialog");
      box.className = "position-fixed top-0 start-50 translate-middle-x mt-3 p-3 bg-white border rounded shadow";
      box.style.zIndex = 2000;
      box.innerHTML = '<p class="mb-2" id="gxp-timeout-text"></p>' +
        '<button type="button" class="btn btn-primary btn-sm">Stay signed in</button>';
      box.querySelector("button").addEventListener("click", function () { lastSent = 0; activity(); });
      document.body.appendChild(box);
    }
    var m = Math.floor(left / 60000), s = Math.floor((left % 60000) / 1000);
    box.querySelector("#gxp-timeout-text").textContent =
      "You will be signed out in " + m + ":" + (s < 10 ? "0" : "") + s +
      " because nothing has happened for a while.";
  }
  ["mousemove", "mousedown", "keydown", "scroll", "wheel", "touchstart"].forEach(function (ev) {
    document.addEventListener(ev, activity, { passive: true, capture: true });
  });
  // Shiny raises its events through jQuery, which native listeners do not see
  $(document).on("shiny:inputchanged", function (e) {
    if (e.name !== ".shinymanager_timeout") last = Date.now();
  });
  timer = setInterval(function () {
    var left = timeoutMs - (Date.now() - last);
    if (left <= warnMs && left > 0) show(left); else if (left > warnMs) hide();
  }, 1000);

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("gxp_logout", function (msg) {
      Shiny.setInputValue(".shinymanager_logout", Date.now(), { priority: "event" });
    });
  });
})();
