/**
 * Noder-style HTML graph editor.
 * Ports are children of each node card — they cannot move independently.
 * Categorical predictors expose one output port per non-reference level.
 */
(function () {
  var nodesLayer = null;
  var svgLayer = null;
  var wrap = null;

  var nodes = {};   // id -> {id,label,x,y,latent,el,levels,base}
  var edges = [];   // {id,from,to,level,label,color,width,dashes,status}
  var selected = { nodes: [], edges: [] };

  var draggingNode = null;
  var dragOffset = { x: 0, y: 0 };
  var linking = null; // { fromId, level, start, cursor }
  var CANVAS_MARGIN = 48;

  function ensureDom() {
    wrap = document.getElementById("smt_graph");
    if (!wrap) return false;
    if (!wrap.__smtReady) {
      wrap.innerHTML =
        '<svg class="smt-conn-svg" id="smt_conn_svg"></svg>' +
        '<div class="smt-nodes-layer" id="smt_nodes_layer"></div>';
      wrap.__smtReady = true;
      bindCanvasEvents();
    }
    nodesLayer = document.getElementById("smt_nodes_layer");
    svgLayer = document.getElementById("smt_conn_svg");
    return !!(nodesLayer && svgLayer);
  }

  function cssEscape(s) {
    if (window.CSS && CSS.escape) return CSS.escape(String(s));
    return String(s).replace(/["\\]/g, "\\$&");
  }

  function portCenter(nodeId, which, level, toPort) {
    var n = nodes[nodeId];
    if (!n || !n.el) return null;
    var port = null;
    if (which === "in") {
      // level = response category port on a split categorical DV
      if (level) {
        port = n.el.querySelector('.port.input[data-level="' + cssEscape(level) + '"]');
      }
      if (!port && toPort) {
        port = n.el.querySelector('.port.input[data-to-port="' + cssEscape(toPort) + '"]');
      }
      if (!port) port = n.el.querySelector(".port.input:not([data-level])");
      if (!port) port = n.el.querySelector(".port.input");
    } else if (level) {
      port = n.el.querySelector('.port.output[data-level="' + cssEscape(level) + '"]');
    } else {
      port = n.el.querySelector(".port.output:not([data-level])") ||
        n.el.querySelector(".port.output");
    }
    if (!port) return null;
    var pr = port.getBoundingClientRect();
    var wr = wrap.getBoundingClientRect();
    return {
      x: pr.left + pr.width / 2 - wr.left + wrap.scrollLeft,
      y: pr.top + pr.height / 2 - wr.top + wrap.scrollTop
    };
  }

  function nodeAtPoint(clientX, clientY) {
    var el = document.elementFromPoint(clientX, clientY);
    if (!el) return null;
    var port = el.closest(".port");
    if (port) {
      return {
        nodeId: port.getAttribute("data-node-id"),
        portType: port.getAttribute("data-port-type"),
        level: port.getAttribute("data-level") || null,
        toPort: port.getAttribute("data-to-port") || null
      };
    }
    var node = el.closest(".smt-node");
    if (node) {
      return {
        nodeId: node.getAttribute("data-node-id"),
        portType: null,
        level: null,
        toPort: null
      };
    }
    return null;
  }

  function bezier(x1, y1, x2, y2) {
    var dx = Math.max(40, Math.abs(x2 - x1) * 0.45);
    return "M " + x1 + " " + y1 +
      " C " + (x1 + dx) + " " + y1 +
      ", " + (x2 - dx) + " " + y2 +
      ", " + x2 + " " + y2;
  }

  function setContentSize(w, h) {
    if (!nodesLayer || !svgLayer || !wrap) return;
    var cw = Math.max(Math.ceil(w), wrap.clientWidth);
    var ch = Math.max(Math.ceil(h), wrap.clientHeight);
    nodesLayer.style.width = cw + "px";
    nodesLayer.style.height = ch + "px";
    svgLayer.setAttribute("width", cw);
    svgLayer.setAttribute("height", ch);
    svgLayer.style.width = cw + "px";
    svgLayer.style.height = ch + "px";
  }

  /**
   * Grow the scrollable canvas so every node sits inside a margin.
   * Top/left growth is done by shifting all nodes into positive space
   * and compensating scroll so the view stays stable under the cursor.
   */
  function ensureContentBounds() {
    if (!wrap || !nodesLayer) return false;
    var ids = Object.keys(nodes);
    if (!ids.length) {
      setContentSize(wrap.clientWidth, wrap.clientHeight);
      return false;
    }

    var minX = Infinity;
    var minY = Infinity;
    var maxX = -Infinity;
    var maxY = -Infinity;
    ids.forEach(function (id) {
      var n = nodes[id];
      var w = (n.el && n.el.offsetWidth) || 120;
      var h = (n.el && n.el.offsetHeight) || 60;
      minX = Math.min(minX, n.x);
      minY = Math.min(minY, n.y);
      maxX = Math.max(maxX, n.x + w);
      maxY = Math.max(maxY, n.y + h);
    });

    var dx = 0;
    var dy = 0;
    if (minX < CANVAS_MARGIN) dx = CANVAS_MARGIN - minX;
    if (minY < CANVAS_MARGIN) dy = CANVAS_MARGIN - minY;
    if (dx || dy) {
      ids.forEach(function (id) {
        var n = nodes[id];
        n.x += dx;
        n.y += dy;
        n.el.style.left = n.x + "px";
        n.el.style.top = n.y + "px";
      });
      maxX += dx;
      maxY += dy;
      // Keep the same world point under the viewport after the origin shift
      wrap.scrollLeft += dx;
      wrap.scrollTop += dy;
      if (linking && linking.start) {
        linking.start.x += dx;
        linking.start.y += dy;
      }
      if (linking && linking.cursor) {
        linking.cursor.x += dx;
        linking.cursor.y += dy;
      }
    }

    setContentSize(maxX + CANVAS_MARGIN, maxY + CANVAS_MARGIN);
    return !!(dx || dy);
  }

  function redrawEdges() {
    if (!svgLayer) return;
    // Content size is owned by ensureContentBounds; keep SVG at least as large
    var w = Math.max(
      wrap.clientWidth,
      nodesLayer ? nodesLayer.offsetWidth : 0,
      wrap.scrollWidth
    );
    var h = Math.max(
      wrap.clientHeight,
      nodesLayer ? nodesLayer.offsetHeight : 0,
      wrap.scrollHeight
    );
    svgLayer.setAttribute("width", w);
    svgLayer.setAttribute("height", h);
    svgLayer.style.width = w + "px";
    svgLayer.style.height = h + "px";

    var html = "";
    edges.forEach(function (e) {
      var a = portCenter(e.from, "out", e.level || null);
      var b = portCenter(e.to, "in", e.toLevel || null, e.toPort || null);
      if (!a || !b) return;
      var selectedEdge = selected.edges.indexOf(e.id) >= 0;
      var dash = e.dashes ? ' stroke-dasharray="6 4"' : "";
      var color = e.color || "#6B4CE6";
      var width = e.width || 2;
      if (selectedEdge) width += 1.5;
      var d = bezier(a.x, a.y, b.x, b.y);
      // Wide invisible stroke for easier clicking
      html += '<path class="smt-edge-hit" data-edge-id="' + e.id + '"' +
        ' d="' + d + '" stroke="transparent" stroke-width="14" fill="none"/>';
      html += '<path class="smt-edge-path' + (selectedEdge ? " selected" : "") +
        '" data-edge-id="' + e.id + '"' +
        ' d="' + d + '"' +
        ' stroke="' + color + '" stroke-width="' + width + '"' + dash +
        ' fill="none" marker-end="url(#smt-arrow)"/>';
      if (e.label) {
        var mx = (a.x + b.x) / 2;
        var my = (a.y + b.y) / 2 - 8;
        var lab = escapeXml(String(e.label));
        // Dual text: fat white copy behind (reliable halo for × / →× labels).
        // data-edge-id so clicks on the coefficient text select the link.
        html += '<text class="smt-edge-label-halo" data-edge-id="' + e.id +
          '" x="' + mx + '" y="' + my +
          '" text-anchor="middle">' + lab + "</text>";
        html += '<text class="smt-edge-label" data-edge-id="' + e.id +
          '" x="' + mx + '" y="' + my +
          '" text-anchor="middle">' + lab + "</text>";
      }
    });

    if (linking && linking.cursor) {
      html += '<path class="smt-temp-edge" d="' +
        bezier(linking.start.x, linking.start.y, linking.cursor.x, linking.cursor.y) +
        '" stroke="#6B4CE6" stroke-width="2" stroke-dasharray="5 4" fill="none"/>';
    }

    svgLayer.innerHTML =
      '<defs><marker id="smt-arrow" markerWidth="8" markerHeight="8" refX="6" refY="3" orient="auto">' +
      '<path d="M0,0 L6,3 L0,6 Z" fill="#6B4CE6"/></marker></defs>' + html;
  }

  function escapeXml(s) {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  function normalizeLevels(raw) {
    if (!raw) return [];
    if (Array.isArray(raw)) {
      return raw.map(function (x) {
        if (x == null) return "";
        if (typeof x === "object") return String(x[0] != null ? x[0] : x);
        return String(x);
      }).filter(Boolean);
    }
    return [];
  }

  function createNodeEl(n) {
    var el = document.createElement("div");
    el.dataset.nodeId = n.id;
    el.style.left = (n.x || 80) + "px";
    el.style.top = (n.y || 80) + "px";

    // Interaction operator: circle with ×, two inputs + one output
    if (n.interaction) {
      el.className = "smt-node smt-interaction";
      var mark = document.createElement("div");
      mark.className = "smt-interaction-mark";
      mark.textContent = "\u00D7";
      mark.title = n.label || "Interaction";
      var caption = document.createElement("div");
      caption.className = "smt-interaction-caption";
      caption.textContent = n.label || n.id;

      ["a", "b"].forEach(function (portId) {
        var inIx = document.createElement("div");
        inIx.className = "port input ix-port ix-port-" + portId;
        inIx.dataset.portType = "input";
        inIx.dataset.nodeId = n.id;
        inIx.dataset.toPort = portId;
        inIx.title = "Factor " + portId.toUpperCase() + " — drop a predictor here";
        el.appendChild(inIx);
      });

      var outIx = document.createElement("div");
      outIx.className = "port output";
      outIx.dataset.portType = "output";
      outIx.dataset.nodeId = n.id;
      outIx.title = "Product — drag to a dependent variable";
      el.appendChild(mark);
      el.appendChild(caption);
      el.appendChild(outIx);
      return el;
    }

    el.className = "smt-node" + (n.latent ? " latent" : " observed");
    if (!n.latent && n.unmapped) el.classList.add("smt-unmapped");

    var levels = normalizeLevels(n.levels);
    var isCat = levels.length > 0;
    var catMode = (n.catMode === "omnibus") ? "omnibus" : "split";
    if (isCat) {
      el.classList.add("smt-categorical");
      if (catMode === "omnibus") el.classList.add("smt-cat-omnibus");
    }

    var title = document.createElement("div");
    title.className = "smt-node-title";
    var titleRow = document.createElement("div");
    titleRow.className = "smt-node-title-row";
    var titleText = document.createElement("span");
    titleText.className = "smt-node-title-text";
    titleText.textContent = n.label || n.id;
    titleRow.appendChild(titleText);
    if (!n.latent && n.unmapped) {
      var unmapIcon = document.createElement("span");
      unmapIcon.className = "smt-node-unmap-icon";
      unmapIcon.title = "Not mapped to a data column";
      unmapIcon.setAttribute("aria-label", "Not mapped to a data column");
      unmapIcon.innerHTML =
        '<svg width="12" height="12" viewBox="0 0 12 12" aria-hidden="true">' +
        '<path d="M3.2 8.8a2.4 2.4 0 0 1 0-3.4L4.5 4.1" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>' +
        '<path d="M8.8 3.2a2.4 2.4 0 0 1 0 3.4L7.5 7.9" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>' +
        '<path d="M2 2l8 8" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/>' +
        "</svg>";
      titleRow.appendChild(unmapIcon);
    }
    title.appendChild(titleRow);

    var subs = [];
    if (n.latent) subs.push({ text: "latent", cls: "smt-node-sub-latent" });
    // Unmapped mark is the title-row icon only (no redundant "unmapped" subtitle)
    if (isCat && n.base) {
      subs.push({ text: "base: " + n.base, cls: "smt-node-sub-base" });
    }
    if (n.model) {
      el.classList.add("smt-dv");
      if (String(n.model).indexOf("glm") === 0) el.classList.add("smt-dv-glm");
      else if (String(n.model).indexOf("lm") === 0) el.classList.add("smt-dv-lm");
      else if (String(n.model).indexOf("lavaan") === 0) el.classList.add("smt-dv-lavaan");
      subs.push({ text: n.model, cls: "smt-node-sub-model" });
    }
    if (n.r2) {
      subs.push({ text: n.r2, cls: "smt-node-sub-r2" });
    }
    subs.forEach(function (item) {
      var sub = document.createElement("div");
      sub.className = "smt-node-sub " + item.cls;
      sub.textContent = item.text;
      title.appendChild(sub);
    });

    el.appendChild(title);

    if (isCat) {
      var modeBar = document.createElement("div");
      modeBar.className = "smt-cat-mode";
      modeBar.title = "How this factor enters models";
      ["split", "omnibus"].forEach(function (mode) {
        var btn = document.createElement("button");
        btn.type = "button";
        btn.className = "smt-cat-mode-btn" + (mode === catMode ? " active" : "");
        btn.textContent = mode;
        btn.dataset.mode = mode;
        btn.title = mode === "omnibus"
          ? "Omnibus: joint factor as predictor; categorical DV not supported"
          : "Split: per-level out (predictor) and in (DV one-vs-rest)";
        btn.addEventListener("mousedown", function (ev) {
          ev.preventDefault();
          ev.stopPropagation();
        });
        btn.addEventListener("click", function (ev) {
          ev.preventDefault();
          ev.stopPropagation();
          if (mode === catMode) return;
          Shiny.setInputValue("smt_node_cat_mode", {
            id: n.id,
            mode: mode
          }, { priority: "event" });
        });
        modeBar.appendChild(btn);
      });
      el.appendChild(modeBar);

      if (catMode === "omnibus") {
        var inOmni = document.createElement("div");
        inOmni.className = "port input";
        inOmni.dataset.portType = "input";
        inOmni.dataset.nodeId = n.id;
        inOmni.title = "Input";
        el.appendChild(inOmni);
        var outOmni = document.createElement("div");
        outOmni.className = "port output";
        outOmni.dataset.portType = "output";
        outOmni.dataset.nodeId = n.id;
        outOmni.title = "Omnibus factor — drag to a dependent variable";
        el.appendChild(outOmni);
      } else {
        var portsWrap = document.createElement("div");
        portsWrap.className = "smt-level-ports";
        levels.forEach(function (level) {
          var row = document.createElement("div");
          row.className = "smt-level-row";
          var inPort = document.createElement("div");
          inPort.className = "port input level";
          inPort.dataset.portType = "input";
          inPort.dataset.nodeId = n.id;
          inPort.dataset.level = level;
          inPort.title = "DV: predict " + level + " vs other categories";
          var lab = document.createElement("span");
          lab.className = "smt-level-label";
          lab.textContent = level;
          var outPort = document.createElement("div");
          outPort.className = "port output level";
          outPort.dataset.portType = "output";
          outPort.dataset.nodeId = n.id;
          outPort.dataset.level = level;
          outPort.title = "Contrast vs base (" + (n.base || "?") + "): " + level;
          row.appendChild(inPort);
          row.appendChild(lab);
          row.appendChild(outPort);
          portsWrap.appendChild(row);
        });
        el.appendChild(portsWrap);
      }
    } else {
      var inPort = document.createElement("div");
      inPort.className = "port input";
      inPort.dataset.portType = "input";
      inPort.dataset.nodeId = n.id;
      inPort.title = "Input";
      el.appendChild(inPort);
      var outPort = document.createElement("div");
      outPort.className = "port output";
      outPort.dataset.portType = "output";
      outPort.dataset.nodeId = n.id;
      outPort.title = "Output — drag to connect";
      el.appendChild(outPort);
    }

    return el;
  }

  function renderGraph(data) {
    if (!ensureDom()) return;

    var keepSel = selected;

    nodesLayer.innerHTML = "";
    nodes = {};
    edges = ((data && data.edges) || []).map(function (e) {
      return Object.assign({}, e, {
        level: e.level || "",
        toPort: e.toPort || "",
        toLevel: e.toLevel || ""
      });
    });

    ((data && data.nodes) || []).forEach(function (n) {
      var el = createNodeEl(n);
      nodesLayer.appendChild(el);
      nodes[n.id] = {
        id: n.id,
        label: n.label,
        x: n.x || 80,
        y: n.y || 80,
        latent: !!n.latent,
        unmapped: !!n.unmapped,
        base: n.base || "",
        levels: normalizeLevels(n.levels),
        el: el
      };
      if (keepSel.nodes.indexOf(n.id) >= 0) el.classList.add("selected");
    });

    selected = keepSel;
    ensureContentBounds();
    redrawEdges();
  }

  function allNodePositions() {
    return Object.keys(nodes).map(function (id) {
      return { id: id, x: nodes[id].x, y: nodes[id].y };
    });
  }

  function localPoint(ev) {
    var wr = wrap.getBoundingClientRect();
    return {
      x: ev.clientX - wr.left + wrap.scrollLeft,
      y: ev.clientY - wr.top + wrap.scrollTop
    };
  }

  function emitDebug(kind, extra) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    var payload = extra || {};
    payload.kind = kind;
    payload.t = Date.now();
    payload.selectedNodes = selected.nodes.slice();
    payload.selectedEdges = selected.edges.slice();
    Shiny.setInputValue("smt_debug_event", payload, { priority: "event" });
  }

  function emitSelection() {
    var payload = {
      nodes: selected.nodes.slice(),
      edges: selected.edges.slice()
    };
    emitDebug("emitSelection", { payload: payload });
    Shiny.setInputValue("graph_selected", payload, { priority: "event" });
  }

  function clearSelection(opts) {
    var silent = opts && opts.silent;
    Object.keys(nodes).forEach(function (id) {
      nodes[id].el.classList.remove("selected");
    });
    selected = { nodes: [], edges: [] };
    redrawEdges();
    // silent=true when about to select something else — avoid a blank
    // intermediate graph_selected that races the real selection / plot.
    if (!silent) emitSelection();
  }

  function bindCanvasEvents() {
    wrap.addEventListener("mousedown", function (ev) {
      if (ev.button !== 0) return;
      var target = ev.target;
      var tag = (target && target.tagName) || "?";
      var cls = (target && target.getAttribute && target.getAttribute("class")) || "";
      emitDebug("mousedown", {
        tag: tag,
        className: String(cls).slice(0, 120),
        edgeIdAttr: target && target.getAttribute
          ? (target.getAttribute("data-edge-id") || "")
          : "",
        closestEdge: !!(target && target.closest && target.closest(
          ".smt-edge-path, .smt-edge-hit, .smt-edge-label, .smt-edge-label-halo"
        ))
      });

      var port = target.closest(".port");
      if (port && port.classList.contains("output")) {
        ev.preventDefault();
        ev.stopPropagation();
        var fromId = port.dataset.nodeId;
        var level = port.dataset.level || "";
        var start = portCenter(fromId, "out", level || null);
        linking = { fromId: fromId, level: level, start: start, cursor: start };
        wrap.classList.add("smt-linking");
        redrawEdges();
        return;
      }

      if (target.closest(".smt-cat-mode")) {
        return;
      }

      var nodeEl = target.closest(".smt-node");
      if (nodeEl && !target.closest(".port")) {
        var id = nodeEl.dataset.nodeId;
        var n = nodes[id];
        if (!n) return;
        ev.preventDefault();
        draggingNode = id;
        dragOffset = {
          x: localPoint(ev).x - n.x,
          y: localPoint(ev).y - n.y
        };
        if (!ev.shiftKey && selected.nodes.indexOf(id) < 0) {
          clearSelection({ silent: true });
          selected.nodes = [id];
          nodeEl.classList.add("selected");
          emitSelection();
        } else if (ev.shiftKey) {
          if (selected.nodes.indexOf(id) < 0) {
            selected.nodes.push(id);
            nodeEl.classList.add("selected");
            emitSelection();
          }
        }
        nodeEl.classList.add("dragging");
        return;
      }

      // Paths, hit targets, and coefficient labels all select the edge.
      // Labels sit on top of the stroke — without this, clicks look like a no-op.
      var edgeEl = target.closest(
        ".smt-edge-path, .smt-edge-hit, .smt-edge-label, .smt-edge-label-halo"
      );
      if (edgeEl) {
        var eid = edgeEl.getAttribute("data-edge-id");
        emitDebug("edgeClickCandidate", {
          eid: eid || "",
          edgeElClass: edgeEl.getAttribute("class") || ""
        });
        if (eid) {
          clearSelection({ silent: true });
          selected.edges = [eid];
          redrawEdges();
          emitSelection();
          return;
        }
      }

      if (target === wrap || target === svgLayer || target === nodesLayer) {
        emitDebug("clearSelectionBackground", {
          targetIs: target === wrap ? "wrap"
            : (target === svgLayer ? "svg" : "nodesLayer")
        });
        clearSelection();
      }
    });

    document.addEventListener("mousemove", function (ev) {
      if (linking) {
        linking.cursor = localPoint(ev);
        redrawEdges();
        return;
      }
      if (draggingNode && nodes[draggingNode]) {
        var p = localPoint(ev);
        var n = nodes[draggingNode];
        // Allow past the origin; ensureContentBounds expands top/left via a world shift
        n.x = p.x - dragOffset.x;
        n.y = p.y - dragOffset.y;
        n.el.style.left = n.x + "px";
        n.el.style.top = n.y + "px";
        ensureContentBounds();
        redrawEdges();
      }
    });

    document.addEventListener("mouseup", function (ev) {
      if (linking) {
        var hit = nodeAtPoint(ev.clientX, ev.clientY);
        var fromId = linking.fromId;
        var level = linking.level || "";
        linking = null;
        wrap.classList.remove("smt-linking");
        redrawEdges();
        if (hit && hit.nodeId && hit.nodeId !== fromId) {
          if (!hit.portType || hit.portType === "input") {
            var toPort = hit.toPort || "";
            var toLevel = hit.level || "";
            // Dropping on interaction body: fill first free input port
            var targetNode = nodes[hit.nodeId];
            if (targetNode && targetNode.el &&
                targetNode.el.classList.contains("smt-interaction") && !toPort) {
              var used = {};
              edges.forEach(function (ed) {
                if (ed.to === hit.nodeId && ed.toPort) used[ed.toPort] = true;
              });
              if (!used.a) toPort = "a";
              else if (!used.b) toPort = "b";
              toLevel = "";
            }
            // Split categorical DV: must land on a level input port
            if (targetNode && targetNode.el &&
                targetNode.el.classList.contains("smt-categorical") &&
                !targetNode.el.classList.contains("smt-cat-omnibus") &&
                !toLevel) {
              linking = null;
              return;
            }
            Shiny.setInputValue("smt_new_edge", {
              from: fromId,
              to: hit.nodeId,
              level: level,
              toPort: toPort,
              toLevel: toLevel,
              nonce: Date.now()
            }, { priority: "event" });
          }
        }
        return;
      }

      if (draggingNode) {
        var n = nodes[draggingNode];
        if (n) {
          n.el.classList.remove("dragging");
          // Final bounds pass (margin on all sides); may shift peers after top/left growth
          ensureContentBounds();
          redrawEdges();
          Shiny.setInputValue("smt_node_moved", {
            id: n.id,
            x: n.x,
            y: n.y,
            // Always send every node: a top/left expansion shifts the whole graph
            positions: allNodePositions(),
            nonce: Date.now()
          }, { priority: "event" });
        }
        draggingNode = null;
      }
    });

    document.addEventListener("keydown", function (ev) {
      var isDelete = ev.key === "Delete" || ev.code === "Delete";
      var isBackspace = ev.key === "Backspace" || ev.code === "Backspace";
      if (!isDelete && !isBackspace) return;
      if (!selected.nodes.length && !selected.edges.length) return;

      var t = ev.target;
      if (t) {
        var tag = (t.tagName || "").toUpperCase();
        var editing =
          tag === "TEXTAREA" ||
          t.isContentEditable ||
          (tag === "INPUT" &&
            /^(text|search|password|email|url|number)$/i.test(t.type || "text"));
        // Backspace must never steal from text fields. Delete/Suppr should still
        // remove the canvas selection when focus jumped into the inspector
        // (selects / numerics) after clicking a node.
        if (isBackspace && (editing || tag === "INPUT" || tag === "SELECT")) return;
        if (isDelete && (tag === "TEXTAREA" || t.isContentEditable)) return;
      }

      ev.preventDefault();
      Shiny.setInputValue("smt_delete_selection", {
        nonce: Date.now()
      }, { priority: "event" });
    });
  }

  Shiny.addCustomMessageHandler("smt_render_graph", function (data) {
    var tries = 0;
    (function attempt() {
      if (ensureDom()) {
        var d = data || { nodes: [], edges: [] };
        emitDebug("renderGraph", {
          nNodes: (d.nodes && d.nodes.length) || 0,
          nEdges: (d.edges && d.edges.length) || 0
        });
        renderGraph(d);
        return;
      }
      if (tries++ < 40) setTimeout(attempt, 50);
    })();
  });

  if (window.jQuery) {
    jQuery(document).on("shiny:connected", function () {
      ensureDom();
    });
  } else {
    document.addEventListener("DOMContentLoaded", ensureDom);
  }
})();
