/* Resizable left / right panes + bottom plot panel */

(function () {
  var STORAGE_KEY = "smt-panel-widths";
  var STORAGE_KEY_H = "smt-plot-height";
  var MIN_SIDE = 180;
  var MIN_CENTER = 240;
  var MIN_PLOT = 80;
  var MIN_GRAPH = 120;
  var DEFAULT_PLOT = 220;

  function loadWidths() {
    try {
      var raw = localStorage.getItem(STORAGE_KEY);
      if (!raw) return { left: 260, right: 360 };
      var parsed = JSON.parse(raw);
      return {
        left: Math.max(MIN_SIDE, parsed.left || 260),
        right: Math.max(MIN_SIDE, parsed.right || 360)
      };
    } catch (e) {
      return { left: 260, right: 360 };
    }
  }

  function saveWidths(w) {
    try {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(w));
    } catch (e) {}
  }

  function applyWidths(left, right) {
    var main = document.querySelector(".smt-main");
    var leftEl = document.querySelector(".smt-left");
    var rightEl = document.querySelector(".smt-right");
    if (!main || !leftEl || !rightEl) return;
    leftEl.style.width = left + "px";
    leftEl.style.flex = "0 0 " + left + "px";
    rightEl.style.width = right + "px";
    rightEl.style.flex = "0 0 " + right + "px";
  }

  function clampPair(left, right, total) {
    var splitters = 10; // ~2 * 5px
    var maxSide = Math.max(MIN_SIDE, total - MIN_CENTER - MIN_SIDE - splitters);
    left = Math.min(Math.max(left, MIN_SIDE), maxSide);
    right = Math.min(Math.max(right, MIN_SIDE), maxSide);
    if (left + right + MIN_CENTER + splitters > total) {
      var overflow = left + right + MIN_CENTER + splitters - total;
      if (right >= left) right = Math.max(MIN_SIDE, right - overflow);
      else left = Math.max(MIN_SIDE, left - overflow);
    }
    return { left: left, right: right };
  }

  function initSplitters() {
    var main = document.querySelector(".smt-main");
    if (!main || main.__smtSplitBound) return;
    main.__smtSplitBound = true;

    var widths = loadWidths();
    var fitted = clampPair(widths.left, widths.right, main.getBoundingClientRect().width || 1200);
    applyWidths(fitted.left, fitted.right);

    var splitters = main.querySelectorAll(".smt-splitter");
    splitters.forEach(function (splitter) {
      splitter.addEventListener("mousedown", function (ev) {
        if (ev.button !== 0) return;
        ev.preventDefault();
        var side = splitter.getAttribute("data-side");
        var startX = ev.clientX;
        var leftEl = document.querySelector(".smt-left");
        var rightEl = document.querySelector(".smt-right");
        var startLeft = leftEl.getBoundingClientRect().width;
        var startRight = rightEl.getBoundingClientRect().width;

        document.body.classList.add("smt-resizing");
        splitter.classList.add("is-active");

        function onMove(e) {
          var dx = e.clientX - startX;
          var total = main.getBoundingClientRect().width;
          var nextLeft = startLeft;
          var nextRight = startRight;
          if (side === "left") nextLeft = startLeft + dx;
          else nextRight = startRight - dx;
          var clamped = clampPair(nextLeft, nextRight, total);
          applyWidths(clamped.left, clamped.right);
        }

        function onUp() {
          document.body.classList.remove("smt-resizing");
          splitter.classList.remove("is-active");
          document.removeEventListener("mousemove", onMove);
          document.removeEventListener("mouseup", onUp);
          saveWidths({
            left: Math.round(leftEl.getBoundingClientRect().width),
            right: Math.round(rightEl.getBoundingClientRect().width)
          });
          window.dispatchEvent(new Event("resize"));
        }

        document.addEventListener("mousemove", onMove);
        document.addEventListener("mouseup", onUp);
      });
    });

    window.addEventListener("resize", function () {
      var total = main.getBoundingClientRect().width;
      var left = document.querySelector(".smt-left").getBoundingClientRect().width;
      var right = document.querySelector(".smt-right").getBoundingClientRect().width;
      var clamped = clampPair(left, right, total);
      applyWidths(clamped.left, clamped.right);
    });
  }

  function loadPlotHeight() {
    try {
      var raw = localStorage.getItem(STORAGE_KEY_H);
      if (!raw) return DEFAULT_PLOT;
      var h = parseInt(raw, 10);
      return isFinite(h) ? h : DEFAULT_PLOT;
    } catch (e) {
      return DEFAULT_PLOT;
    }
  }

  function savePlotHeight(h) {
    try {
      localStorage.setItem(STORAGE_KEY_H, String(Math.round(h)));
    } catch (e) {}
  }

  function clampPlotHeight(h, wrap) {
    if (!wrap) return Math.max(MIN_PLOT, h);
    var total = wrap.getBoundingClientRect().height;
    var title = wrap.querySelector(".smt-center-title");
    var split = wrap.querySelector(".smt-hsplitter");
    var chrome = 8;
    if (title) chrome += title.getBoundingClientRect().height + 8;
    if (split) chrome += split.getBoundingClientRect().height + 8;
    var maxPlot = Math.max(MIN_PLOT, total - MIN_GRAPH - chrome);
    return Math.min(Math.max(h, MIN_PLOT), maxPlot);
  }

  function applyPlotHeight(h) {
    var panel = document.querySelector(".smt-edge-plot-panel");
    if (!panel) return;
    var wrap = document.querySelector(".smt-canvas-wrap");
    h = clampPlotHeight(h, wrap);
    panel.style.flexGrow = "0";
    panel.style.flexShrink = "0";
    panel.style.flexBasis = h + "px";
    panel.style.height = h + "px";
    panel.style.maxHeight = "none";
    panel.style.minHeight = MIN_PLOT + "px";
  }

  function initPlotSplitter() {
    var wrap = document.querySelector(".smt-canvas-wrap");
    var splitter = document.querySelector(".smt-hsplitter");
    var panel = document.querySelector(".smt-edge-plot-panel");
    if (!wrap || !splitter || !panel || splitter.__smtHSplitBound) return;
    splitter.__smtHSplitBound = true;

    applyPlotHeight(loadPlotHeight());

    splitter.addEventListener("mousedown", function (ev) {
      if (ev.button !== 0) return;
      ev.preventDefault();
      ev.stopPropagation();
      var startY = ev.clientY;
      var startH = panel.getBoundingClientRect().height;

      document.body.classList.add("smt-resizing-v");
      splitter.classList.add("is-active");

      function onMove(e) {
        // Dragging the splitter up expands the plot; down shrinks it
        var dy = startY - e.clientY;
        applyPlotHeight(startH + dy);
      }

      function onUp() {
        document.body.classList.remove("smt-resizing-v");
        splitter.classList.remove("is-active");
        document.removeEventListener("mousemove", onMove);
        document.removeEventListener("mouseup", onUp);
        var finalH = panel.getBoundingClientRect().height;
        savePlotHeight(finalH);
        applyPlotHeight(finalH);
        window.dispatchEvent(new Event("resize"));
      }

      document.addEventListener("mousemove", onMove);
      document.addEventListener("mouseup", onUp);
    });

    window.addEventListener("resize", function () {
      applyPlotHeight(panel.getBoundingClientRect().height || loadPlotHeight());
    });
  }

  function initAll() {
    initSplitters();
    initPlotSplitter();
  }

  $(document).on("shiny:connected", function () {
    setTimeout(initAll, 50);
  });
  $(function () {
    setTimeout(initAll, 50);
  });
  // conditionalPanel (model view) may mount later
  $(document).on("shiny:value shiny:idle", function () {
    setTimeout(initPlotSplitter, 30);
  });
})();
