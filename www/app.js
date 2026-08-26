/* =============================================================================
   rGenomeTrackUI — app.js  v0.2
   Minimal JS: copy buttons, path click-to-copy, table accessibility
   ============================================================================= */

(function () {
  'use strict';

  /* --------------------------------------------------------------------------
     Copy-to-clipboard helper
     -------------------------------------------------------------------------- */
  function copyText(text, btn) {
    if (!navigator.clipboard) {
      // Fallback for older browsers
      var ta = document.createElement('textarea');
      ta.value = text;
      ta.style.position = 'fixed';
      ta.style.opacity = '0';
      document.body.appendChild(ta);
      ta.focus();
      ta.select();
      try { document.execCommand('copy'); } catch (e) {}
      document.body.removeChild(ta);
      flashBtn(btn, 'Copied!');
      return;
    }
    navigator.clipboard.writeText(text).then(function () {
      flashBtn(btn, 'Copied!');
    });
  }

  function flashBtn(btn, msg) {
    if (!btn) return;
    var original = btn.innerHTML;
    btn.innerHTML = '<i class="fa fa-check"></i> ' + msg;
    btn.disabled = true;
    setTimeout(function () {
      btn.innerHTML = original;
      btn.disabled = false;
    }, 1800);
  }

  /* --------------------------------------------------------------------------
     Copy buttons: data-copy-target="#outputId"
     -------------------------------------------------------------------------- */
  document.addEventListener('click', function (e) {
    var btn = e.target.closest('[data-copy-target]');
    if (!btn) return;
    var targetId = btn.getAttribute('data-copy-target').replace(/^#/, '');
    var el = document.getElementById(targetId);
    if (el) {
      copyText(el.innerText || el.textContent, btn);
    }
  });

  /* --------------------------------------------------------------------------
     Path blocks: click to copy
     -------------------------------------------------------------------------- */
  document.addEventListener('click', function (e) {
    var pb = e.target.closest('.rt-path-block');
    if (!pb) return;
    copyText(pb.innerText || pb.textContent, null);
    pb.title = 'Copied!';
    pb.style.borderColor = 'var(--rt-accent)';
    pb.style.color = 'var(--rt-accent)';
    setTimeout(function () {
      pb.title = 'Click to copy';
      pb.style.borderColor = '';
      pb.style.color = '';
    }, 1500);
  });

  /* --------------------------------------------------------------------------
     Shiny message: show toast notification
     Shiny.addCustomMessageHandler('rt_toast', ...)
     -------------------------------------------------------------------------- */
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('rt_toast', function (msg) {
      var cls = msg.type || 'message';
      Shiny.notifications.show({
        html: msg.text,
        type: cls,
        duration: msg.duration || 3000,
        closeButton: true
      });
    });

    /* Shiny message to flash a button after action */
    Shiny.addCustomMessageHandler('rt_flash_btn', function (msg) {
      var btn = document.getElementById(msg.id);
      if (btn) flashBtn(btn, msg.label || 'Done!');
    });

    Shiny.addCustomMessageHandler('rt_upload_received', function (msg) {
      var status = document.getElementById(msg.id);
      if (!status) return;
      var count = msg.count || 1;
      status.innerHTML = '<i class="fa fa-check-circle" aria-hidden="true"></i> ' +
        '<span><strong>' + count + ' fichier' + (count > 1 ? 's prêts.' : ' prêt.') +
        '</strong><br><small>Vous pouvez maintenant ' + (count > 1 ? 'les ' : 'l\'') +
        'ajouter au registre.</small></span>';
      status.classList.add('is-active', 'is-ready');
    });

    Shiny.addCustomMessageHandler('rt_init_figure_viewers', function () {
      initFigureViewers();
    });

    Shiny.addCustomMessageHandler('rt_scroll_to', function (msg) {
      var target = msg && msg.id ? document.getElementById(msg.id) : null;
      if (!target) return;
      target.scrollIntoView({behavior: 'smooth', block: 'start'});
      target.classList.add('rt-focus-flash');
      setTimeout(function () { target.classList.remove('rt-focus-flash'); }, 1200);
    });
  }

  /* --------------------------------------------------------------------------
     File uploads: visible feedback from Finder selection until Shiny receives
     the completed temporary file. Shiny's own progress bar remains the source
     of truth for the percentage.
     -------------------------------------------------------------------------- */
  function initUploadFeedback() {
    var input = document.getElementById('inputs-file_upload');
    var status = document.getElementById('inputs-upload_status');
    if (!input || !status || input.dataset.rtUploadFeedback === '1') return;
    input.dataset.rtUploadFeedback = '1';

    input.addEventListener('change', function () {
      status.classList.remove('is-ready');
      if (!input.files || !input.files.length) {
        status.textContent = '';
        status.classList.remove('is-active');
        return;
      }
      var count = input.files.length;
      var totalBytes = Array.prototype.reduce.call(input.files, function (sum, file) {
        return sum + file.size;
      }, 0);
      var mb = totalBytes / (1024 * 1024);
      var size = mb >= 1024 ? (mb / 1024).toFixed(1) + ' Go' : mb.toFixed(1) + ' Mo';
      var names = Array.prototype.map.call(input.files, function (file) { return file.name; });
      var selectionLabel = count === 1 ? names[0] : count + ' fichiers : ' + names.slice(0, 3).join(', ') +
        (count > 3 ? '…' : '');
      status.innerHTML = '<span class="rt-upload-spinner" aria-hidden="true"></span>' +
        '<span><strong>Transfert en cours…</strong><br><small>' +
        selectionLabel.replace(/[&<>"']/g, function (c) {
          return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[c];
        }) + ' · ' + size + '</small></span>';
      status.classList.add('is-active');
    });

  }

  /* --------------------------------------------------------------------------
     Animate stat cards on load (simple fade-in)
     -------------------------------------------------------------------------- */
  function animateStatCards() {
    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
    var cards = document.querySelectorAll('.stat-card');
    cards.forEach(function (card, i) {
      card.style.opacity = '0';
      card.style.transform = 'translateY(10px)';
      card.style.transition = 'opacity 0.35s ease, transform 0.35s ease';
      setTimeout(function () {
        card.style.opacity = '1';
        card.style.transform = 'translateY(0)';
      }, 80 + i * 55);
    });
  }

  /* --------------------------------------------------------------------------
     Zoomable result figures (no external viewer dependency)
     -------------------------------------------------------------------------- */
  function initFigureViewers() {
    document.querySelectorAll('.rt-figure-viewer').forEach(function (viewer) {
      if (viewer.dataset.rtViewerReady === '1') return;
      var stage = viewer.querySelector('.rt-figure-stage');
      var img = viewer.querySelector('.rt-zoomable-figure');
      var label = viewer.querySelector('.rt-zoom-reset');
      if (!stage || !img) return;

      viewer.dataset.rtViewerReady = '1';
      var zoom = 1;
      var dragging = false;
      var startX = 0;
      var startY = 0;
      var startLeft = 0;
      var startTop = 0;

      function applyZoom(nextZoom) {
        zoom = Math.max(0.5, Math.min(8, nextZoom));
        img.style.width = (zoom * 100) + '%';
        img.style.maxWidth = 'none';
        if (label) label.textContent = Math.round(zoom * 100) + ' %';
      }

      viewer.querySelector('.rt-zoom-in').addEventListener('click', function () {
        applyZoom(zoom * 1.25);
      });
      viewer.querySelector('.rt-zoom-out').addEventListener('click', function () {
        applyZoom(zoom / 1.25);
      });
      label.addEventListener('click', function () { applyZoom(1); });
      viewer.querySelector('.rt-zoom-fullscreen').addEventListener('click', function () {
        if (document.fullscreenElement) {
          document.exitFullscreen();
        } else if (document.webkitFullscreenElement && document.webkitExitFullscreen) {
          document.webkitExitFullscreen();
        } else if (viewer.requestFullscreen) {
          viewer.requestFullscreen();
        } else if (viewer.webkitRequestFullscreen) {
          viewer.webkitRequestFullscreen();
        }
      });
      stage.addEventListener('wheel', function (event) {
        if (!event.ctrlKey && !event.metaKey) return;
        event.preventDefault();
        applyZoom(event.deltaY < 0 ? zoom * 1.12 : zoom / 1.12);
      }, {passive: false});
      stage.addEventListener('pointerdown', function (event) {
        if (zoom <= 1) return;
        dragging = true;
        startX = event.clientX;
        startY = event.clientY;
        startLeft = stage.scrollLeft;
        startTop = stage.scrollTop;
        stage.classList.add('is-dragging');
        stage.setPointerCapture(event.pointerId);
      });
      stage.addEventListener('pointermove', function (event) {
        if (!dragging) return;
        stage.scrollLeft = startLeft - (event.clientX - startX);
        stage.scrollTop = startTop - (event.clientY - startY);
      });
      stage.addEventListener('pointerup', function () {
        dragging = false;
        stage.classList.remove('is-dragging');
      });
      applyZoom(1);
    });
  }

  function initTrackRowSorting() {
    document.querySelectorAll('.rt-track-order-table').forEach(function (container) {
      var table = container.querySelector('table.dataTable');
      var tbody = table ? table.querySelector('tbody') : null;
      if (!table || !tbody || tbody.dataset.rtSortable === '1') return;
      tbody.dataset.rtSortable = '1';
      var dragged = null;

      function prepareRows() {
        tbody.querySelectorAll('tr').forEach(function (row) {
          row.draggable = true;
          row.title = 'Glisser pour modifier la position';
        });
      }

      prepareRows();
      tbody.addEventListener('dragstart', function (event) {
        dragged = event.target.closest('tr');
        if (!dragged) return;
        dragged.classList.add('rt-track-dragging');
        event.dataTransfer.effectAllowed = 'move';
        event.dataTransfer.setData('text/plain', 'track-order');
      });
      tbody.addEventListener('dragover', function (event) {
        if (!dragged) return;
        event.preventDefault();
        var target = event.target.closest('tr');
        if (!target || target === dragged) return;
        var rect = target.getBoundingClientRect();
        var after = event.clientY > rect.top + rect.height / 2;
        tbody.insertBefore(dragged, after ? target.nextSibling : target);
      });
      tbody.addEventListener('drop', function (event) {
        if (!dragged) return;
        event.preventDefault();
        var dt = window.jQuery && jQuery.fn.dataTable ? jQuery(table).DataTable() : null;
        if (!dt || typeof Shiny === 'undefined') return;
        var ids = Array.prototype.map.call(tbody.querySelectorAll('tr'), function (row) {
          var data = dt.row(row).data();
          return data ? String(data[0]) : '';
        }).filter(Boolean);
        var inputId = container.getAttribute('data-order-input');
        if (inputId) Shiny.setInputValue(inputId, ids, {priority: 'event'});
      });
      tbody.addEventListener('dragend', function () {
        if (dragged) dragged.classList.remove('rt-track-dragging');
        dragged = null;
      });
      if (window.jQuery) {
        jQuery(table).on('draw.dt', prepareRows);
      }
    });
  }

  function initTrackCardSorting() {
    document.querySelectorAll('.rt-track-card-list').forEach(function (list) {
      if (list.dataset.rtCardSorting === '1') return;
      list.dataset.rtCardSorting = '1';
      var dragged = null;
      var didDrag = false;

      function sendSelection() {
        if (typeof Shiny === 'undefined') return;
        var inputId = list.getAttribute('data-selection-input');
        var ids = Array.prototype.map.call(list.querySelectorAll('.rt-track-sort-card.is-selected'), function (card) {
          return card.getAttribute('data-track-id');
        }).filter(Boolean);
        if (inputId) Shiny.setInputValue(inputId, ids, {priority: 'event'});
      }

      list.addEventListener('click', function (event) {
        var card = event.target.closest('.rt-track-sort-card');
        if (!card || didDrag) return;
        if (event.ctrlKey || event.metaKey) {
          card.classList.toggle('is-selected');
        } else {
          list.querySelectorAll('.rt-track-sort-card.is-selected').forEach(function (item) {
            item.classList.remove('is-selected');
          });
          card.classList.add('is-selected');
        }
        sendSelection();
      });

      list.addEventListener('dragstart', function (event) {
        dragged = event.target.closest('.rt-track-sort-card');
        if (!dragged) return;
        didDrag = true;
        dragged.classList.add('rt-track-dragging');
        event.dataTransfer.effectAllowed = 'move';
        event.dataTransfer.setData('text/plain', dragged.getAttribute('data-track-id') || 'track');
      });
      list.addEventListener('dragover', function (event) {
        if (!dragged) return;
        event.preventDefault();
        var target = event.target.closest('.rt-track-sort-card');
        if (!target || target === dragged) return;
        var rect = target.getBoundingClientRect();
        var after = event.clientY > rect.top + rect.height / 2;
        target.parentNode.insertBefore(dragged, after ? target.nextSibling : target);
      });
      list.addEventListener('drop', function (event) {
        if (!dragged || typeof Shiny === 'undefined') return;
        event.preventDefault();
        var ids = Array.prototype.map.call(list.querySelectorAll('.rt-track-sort-card'), function (card) {
          return card.getAttribute('data-track-id');
        }).filter(Boolean);
        var inputId = list.getAttribute('data-order-input');
        if (inputId) Shiny.setInputValue(inputId, ids, {priority: 'event'});
      });
      list.addEventListener('dragend', function () {
        if (dragged) dragged.classList.remove('rt-track-dragging');
        dragged = null;
        setTimeout(function () { didDrag = false; }, 0);
      });
    });
  }

  /* --------------------------------------------------------------------------
     Shiny reconnect / init hooks
     -------------------------------------------------------------------------- */
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('rt_init_page', function () {
      animateStatCards();
    });

    $(document).on('shiny:idle', function () {
      animateStatCards();
      initFigureViewers();
      initTrackRowSorting();
      initTrackCardSorting();
    });
  }

  /* --------------------------------------------------------------------------
     Init on DOM ready
     -------------------------------------------------------------------------- */
  document.addEventListener('DOMContentLoaded', function () {
    // Add title to path blocks
    document.querySelectorAll('.rt-path-block').forEach(function (pb) {
      pb.title = 'Click to copy';
    });
    initUploadFeedback();
    initFigureViewers();
    initTrackRowSorting();
    initTrackCardSorting();
    // Les figures Shiny sont injectées après le chargement initial. Observer
    // le DOM garantit que les contrôles sont branchés dès leur apparition,
    // indépendamment de l'ordre des événements shiny:idle.
    var figureObserver = new MutationObserver(function (mutations) {
      var hasAddedNodes = mutations.some(function (mutation) {
        return mutation.addedNodes && mutation.addedNodes.length > 0;
      });
      if (hasAddedNodes) {
        initFigureViewers();
        initTrackRowSorting();
        initTrackCardSorting();
      }
    });
    figureObserver.observe(document.body, {childList: true, subtree: true});
  });

  $(document).on('shiny:connected', initUploadFeedback);

  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('rt_select_track_card', function (msg) {
      var root = msg && msg.container ? document.getElementById(msg.container) : null;
      var list = root ? root.closest('.rt-track-card-list') : null;
      if (!list || !msg.id) return;
      list.querySelectorAll('.rt-track-sort-card').forEach(function (card) {
        card.classList.toggle('is-selected', card.getAttribute('data-track-id') === String(msg.id));
      });
      var inputId = list.getAttribute('data-selection-input');
      if (inputId) Shiny.setInputValue(inputId, [String(msg.id)], {priority: 'event'});
    });
  }

})();
