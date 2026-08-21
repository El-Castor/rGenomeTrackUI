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
      status.innerHTML = '<i class="fa fa-check-circle" aria-hidden="true"></i> ' +
        '<span><strong>Fichier prêt.</strong><br><small>Vous pouvez maintenant l\'ajouter au registre.</small></span>';
      status.classList.add('is-active', 'is-ready');
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
      var file = input.files[0];
      var mb = file.size / (1024 * 1024);
      var size = mb >= 1024 ? (mb / 1024).toFixed(1) + ' Go' : mb.toFixed(1) + ' Mo';
      status.innerHTML = '<span class="rt-upload-spinner" aria-hidden="true"></span>' +
        '<span><strong>Transfert en cours…</strong><br><small>' +
        file.name.replace(/[&<>"']/g, function (c) {
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
     Shiny reconnect / init hooks
     -------------------------------------------------------------------------- */
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('rt_init_page', function () {
      animateStatCards();
    });

    $(document).on('shiny:idle', function () {
      animateStatCards();
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
  });

  $(document).on('shiny:connected', initUploadFeedback);

})();
