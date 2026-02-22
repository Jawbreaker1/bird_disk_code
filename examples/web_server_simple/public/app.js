(function () {
  var page = document.body.getAttribute("data-page");
  var links = document.querySelectorAll(".nav a");
  for (var i = 0; i < links.length; i += 1) {
    var href = links[i].getAttribute("href");
    if ((page === "home" && href === "/") ||
        (page === "features" && href === "/features") ||
        (page === "about" && href === "/about")) {
      links[i].classList.add("active");
    }
  }

  var pill = document.getElementById("status-pill");
  if (pill) {
    fetch("/api/status")
      .then(function (r) { return r.json(); })
      .then(function (data) { pill.textContent = "served " + data.served; })
      .catch(function () {
        var now = new Date();
        pill.textContent = "live " + now.toLocaleTimeString();
      });
  }
})();
