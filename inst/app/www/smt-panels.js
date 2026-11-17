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
    // Keep CSS max-height (calc) so the canvas cannot be crushed
    panel.style.maxHeight = "";
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

  function adjustBrowseTables() {
    // scrollX tables init while a center view is hidden — fix widths on show
    if (!window.jQuery || !jQuery.fn || !jQuery.fn.dataTable) return;
    [".smt-data-wrap", ".smt-prepare-wrap"].forEach(function (sel) {
      var root = document.querySelector(sel);
      if (!root) return;
      var panel = root.closest(".shiny-panel-conditional");
      if (panel && window.getComputedStyle(panel).display === "none") return;
      jQuery(root).find("table.dataTable").each(function () {
        try {
          var api = jQuery(this).DataTable();
          if (api) {
            api.columns.adjust();
            if (api.scroller && api.scroller.measure) api.scroller.measure();
          }
        } catch (e) {}
      });
    });
  }

  function initViewToggleAdjust() {
    var toggle = document.querySelector(".smt-view-toggle");
    if (!toggle || toggle.__smtDtAdjustBound) return;
    toggle.__smtDtAdjustBound = true;
    toggle.addEventListener("change", function () {
      setTimeout(adjustBrowseTables, 40);
      setTimeout(adjustBrowseTables, 200);
    });
  }

  function readEdgeRelKind() {
    var el = document.getElementById("edge_rel_kind");
    if (!el) return "linear";
    var v = el.value;
    if (v) return String(v);
    // selectize keeps the value on the original <select>
    try {
      if (el.selectize && el.selectize.getValue) {
        v = el.selectize.getValue();
        if (v) return String(v);
      }
    } catch (e) {}
    return "linear";
  }

  function syncEdgePriorRel() {
    var host = document.querySelector(".smt-edge-prior-inline");
    if (!host) return;
    host.setAttribute("data-rel", readEdgeRelKind());
  }

  function initEdgePriorRel() {
    syncEdgePriorRel();
    var el = document.getElementById("edge_rel_kind");
    if (el && !el.__smtPriorRelBound) {
      el.__smtPriorRelBound = true;
      el.addEventListener("change", syncEdgePriorRel);
    }
    if (window.jQuery && !window.__smtPriorRelShiny) {
      window.__smtPriorRelShiny = true;
      $(document).on("change", "#edge_rel_kind", syncEdgePriorRel);
      $(document).on("shiny:inputchanged", function (ev) {
        if (ev && ev.name === "edge_rel_kind") syncEdgePriorRel();
      });
    }
    // selectize / late paint
    setTimeout(syncEdgePriorRel, 50);
    setTimeout(syncEdgePriorRel, 250);
  }

  function initAll() {
    initSplitters();
    initPlotSplitter();
    initViewToggleAdjust();
    initEdgePriorRel();
  }

  function smtDebugCopy(kind, detail) {
    try {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(
          "smt_debug_event",
          {
            kind: kind,
            detail: detail == null ? "" : String(detail),
            t: Date.now()
          },
          { priority: "event" }
        );
      }
    } catch (e) {}
    if (window.console && console.debug) {
      console.debug("[smt-copy]", kind, detail);
    }
  }

  function showCopyToast(message, isError) {
    var existing = document.getElementById("smt-copy-toast");
    if (existing) existing.remove();
    var toast = document.createElement("div");
    toast.id = "smt-copy-toast";
    toast.className =
      "smt-copy-toast" + (isError ? " smt-copy-toast-error" : "");
    toast.setAttribute("role", "status");
    toast.textContent = message == null ? "" : String(message);
    document.body.appendChild(toast);
    // Force reflow so the enter transition runs
    void toast.offsetWidth;
    toast.classList.add("is-visible");
    window.setTimeout(function () {
      toast.classList.remove("is-visible");
      window.setTimeout(function () {
        if (toast.parentNode) toast.parentNode.removeChild(toast);
      }, 220);
    }, 2200);
  }

  function showCopyFallbackDialog(text, label) {
    var existing = document.getElementById("smt-copy-fallback");
    if (existing) existing.remove();
    var wrap = document.createElement("div");
    wrap.id = "smt-copy-fallback";
    wrap.className = "smt-copy-fallback";
    wrap.innerHTML =
      '<div class="smt-copy-fallback-card">' +
      "<h4>Copy " +
      (label || "text") +
      "</h4>" +
      "<p>Clipboard access was blocked. Select all and copy (Ctrl/Cmd+C), or use the button.</p>" +
      '<textarea class="smt-copy-fallback-ta"></textarea>' +
      '<div class="smt-copy-fallback-actions">' +
      '<button type="button" class="smt-btn smt-btn-primary smt-copy-fallback-do">Copy</button>' +
      '<button type="button" class="smt-btn smt-btn-ghost smt-copy-fallback-close">Close</button>' +
      "</div></div>";
    document.body.appendChild(wrap);
    var ta = wrap.querySelector(".smt-copy-fallback-ta");
    ta.value = text == null ? "" : String(text);
    ta.focus();
    ta.select();
    wrap.querySelector(".smt-copy-fallback-close").onclick = function () {
      wrap.remove();
    };
    wrap.querySelector(".smt-copy-fallback-do").onclick = function () {
      ta.focus();
      ta.select();
      var ok = false;
      try {
        ok = document.execCommand("copy");
      } catch (e) {
        ok = false;
      }
      smtDebugCopy("fallback_copy", ok ? "ok" : "fail");
      if (ok) {
        wrap.remove();
        showCopyToast((label || "Text") + " copied to clipboard");
      } else {
        showCopyToast("Copy failed — try Ctrl/Cmd+C", true);
      }
    };
    wrap.addEventListener("click", function (ev) {
      if (ev.target === wrap) wrap.remove();
    });
  }

  function copyTextSync(text) {
    text = text == null ? "" : String(text);
    var ta = document.createElement("textarea");
    ta.value = text;
    ta.setAttribute("readonly", "");
    ta.style.position = "fixed";
    ta.style.left = "-9999px";
    document.body.appendChild(ta);
    ta.focus();
    ta.select();
    var ok = false;
    try {
      ok = document.execCommand("copy");
    } catch (e) {
      ok = false;
    }
    document.body.removeChild(ta);
    return ok;
  }

  function copyTextToClipboard(text, label) {
    text = text == null ? "" : String(text);
    label = label || "TeX";
    // Prefer sync execCommand while we still have the user-gesture stack
    // (async clipboard.writeText after a Shiny round-trip is often blocked).
    if (copyTextSync(text)) {
      smtDebugCopy("copy_text", "execCommand_ok");
      showCopyToast(label + " copied to clipboard");
      return true;
    }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(
        function () {
          smtDebugCopy("copy_text", "clipboard_api_ok");
          showCopyToast(label + " copied to clipboard");
        },
        function (err) {
          smtDebugCopy("copy_text_fail", err && err.message);
          showCopyFallbackDialog(text, label);
        }
      );
      return true;
    }
    smtDebugCopy("copy_text_fail", "no_clipboard");
    showCopyFallbackDialog(text, label);
    return false;
  }

  function copyHtmlToClipboard(html, text, label) {
    html = html == null ? "" : String(html);
    text = text == null || text === "" ? html.replace(/<[^>]+>/g, "") : String(text);
    label = label || "Word";
    var ok = false;
    var div = document.createElement("div");
    div.contentEditable = "true";
    div.style.position = "fixed";
    div.style.left = "-9999px";
    div.innerHTML = html;
    document.body.appendChild(div);
    var range = document.createRange();
    range.selectNodeContents(div);
    var sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);
    try {
      ok = document.execCommand("copy");
    } catch (e) {
      ok = false;
    }
    sel.removeAllRanges();
    document.body.removeChild(div);
    if (ok) {
      smtDebugCopy("copy_html", "execCommand_ok");
      showCopyToast(label + " HTML copied — paste into Word");
      return true;
    }
    if (copyTextSync(text)) {
      smtDebugCopy("copy_html", "plain_fallback_ok");
      showCopyToast(label + " text copied to clipboard");
      return true;
    }
    if (
      window.ClipboardItem &&
      navigator.clipboard &&
      navigator.clipboard.write
    ) {
      try {
        var item = new ClipboardItem({
          "text/html": new Blob([html], { type: "text/html" }),
          "text/plain": new Blob([text], { type: "text/plain" })
        });
        navigator.clipboard.write([item]).then(
          function () {
            smtDebugCopy("copy_html", "clipboard_api_ok");
            showCopyToast(label + " HTML copied — paste into Word");
          },
          function (err) {
            smtDebugCopy("copy_html_fail", err && err.message);
            showCopyFallbackDialog(text, label);
          }
        );
        return true;
      } catch (e) {
        /* fall through */
      }
    }
    smtDebugCopy("copy_html_fail", "no_clipboard");
    showCopyFallbackDialog(text, label);
    return false;
  }

  function readJsonEl(id) {
    var el = document.getElementById(id);
    if (!el) return null;
    try {
      return JSON.parse(el.textContent || el.innerText || "");
    } catch (e) {
      smtDebugCopy("json_parse_fail", id);
      return null;
    }
  }

  window.smtCopyTextFromEl = function (id) {
    var payload = readJsonEl(id);
    if (payload == null) {
      smtDebugCopy("copy_text_missing", id);
      return false;
    }
    var text = typeof payload === "string" ? payload : payload.text || "";
    return copyTextToClipboard(text, "TeX");
  };

  window.smtCopyHtmlFromEl = function (id) {
    var payload = readJsonEl(id);
    if (!payload || typeof payload !== "object") {
      smtDebugCopy("copy_html_missing", id);
      return false;
    }
    return copyHtmlToClipboard(payload.html || "", payload.text || "", "Word");
  };

  // Register handlers once Shiny is available (script may load before it)
  function ensureResizeHandler() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
    if (window.__smtResizePlotsBound) return;
    window.__smtResizePlotsBound = true;
    Shiny.addCustomMessageHandler("smt_resize_plots", function () {
      function fire() {
        window.dispatchEvent(new Event("resize"));
      }
      fire();
      setTimeout(fire, 80);
      setTimeout(fire, 250);
    });
    // Legacy server-push copy (often blocked without user gesture) → fallback dialog
    Shiny.addCustomMessageHandler("smt_copy_text", function (msg) {
      copyTextToClipboard(msg && msg.text, "TeX");
    });
    Shiny.addCustomMessageHandler("smt_copy_html", function (msg) {
      copyHtmlToClipboard(msg && msg.html, msg && msg.text, "Word");
    });
  }

  function boot() {
    try {
      ensureResizeHandler();
      initAll();
    } catch (e) {
      if (window.console && console.warn) {
        console.warn("smt-panels boot failed", e);
      }
    }
  }

  // Head scripts run before .smt-main exists — poll until splitters bind.
  (function bootLoop(attempt) {
    boot();
    var main = document.querySelector(".smt-main");
    if (main && main.__smtSplitBound) return;
    if (attempt < 60) {
      setTimeout(function () {
        bootLoop(attempt + 1);
      }, 100);
    }
  })(0);

  if (window.jQuery) {
    $(document).on("shiny:connected shiny:idle shiny:value", function () {
      boot();
    });
    $(boot);
  } else {
    document.addEventListener("DOMContentLoaded", boot);
    window.addEventListener("load", boot);
  }
})();
