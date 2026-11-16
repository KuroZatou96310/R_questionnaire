(function () {
  if (window.questionnaireDragDropInstalled) return;
  window.questionnaireDragDropInstalled = true;
  let dragged = null, sourceGroup = null, target = null, after = false;
  let pending = null, lastPointer = null, suppressClick = false;
  const root = () => document.getElementById('question_list_ui');
  function clearTarget(keep = null) {
    const keepGroup = keep?.closest('.group-card');
    document.querySelectorAll('.drop-group-target').forEach((el) => {
      if (el !== keepGroup) el.classList.remove('drop-group-target');
    });
    document.querySelectorAll('.drop-insert-target').forEach((button) => {
      if (button === keep) return;
      button.classList.remove('drop-insert-target');
      button.textContent = button.dataset.idleLabel || '設問を追加';
    });
  }
  function reset() {
    dragged?.classList.remove('dragging');
    dragged = sourceGroup = target = pending = lastPointer = null;
    clearTarget();
  }
  function setInsertTarget(button) {
    if (!button?.matches('.insert-question-button')) return false;
    target = button;
    button.classList.add('drop-insert-target');
    button.textContent = 'ここに挿入';
    const group = button.closest('.group-card');
    if (group && group !== sourceGroup) group.classList.add('drop-group-target');
    return true;
  }
  function setTarget(event) {
    const board = root();
    const eventTarget = event.target instanceof Element ? event.target : event.target?.parentElement;
    const hovered = eventTarget;
    const insertButton = hovered?.closest('.insert-question-button');
    const card = hovered?.closest('.question-card');
    const row = card?.closest('.question-row');
    const group = hovered?.closest('.group-card');
    if (!dragged || !board || !hovered || !board.contains(hovered)) return false;
    if (insertButton) return setInsertTarget(insertButton);
    if (row === dragged) return false;
    if (row) {
      const targetGroup = card.closest('.group-card');
      const list = targetGroup?.querySelector(':scope > .question-list');
      if (!list) return false;
      const rect = row.getBoundingClientRect(); after = event.clientY >= rect.top + rect.height / 2;
      const slot = after
        ? (row.nextElementSibling?.matches('.insert-question-button') ? row.nextElementSibling : list.querySelector(':scope > .insert-question-button-final'))
        : (row.previousElementSibling?.matches('.insert-question-button') ? row.previousElementSibling : null);
      return setInsertTarget(slot);
    }
    if (group) {
      const list = group.querySelector(':scope > .question-list');
      if (!list) return false;
      return setInsertTarget(list.querySelector(':scope > .insert-question-button-final'));
    }
    return false;
  }
  function sendLayout() {
    const board = root(); if (!board || !window.Shiny) return;
    const groups = Array.from(board.querySelectorAll('.group-card')).map((group) => ({
      group_id: group.dataset.groupId,
      question_ids: Array.from(group.querySelectorAll(':scope > .question-list > .question-row')).map((row) => row.dataset.questionId)
    }));
    const read = (id) => document.getElementById(id);
    const value = (id) => read(id)?.value ?? null;
    const checked = (id) => read(id)?.checked ?? null;
    const question_drafts = Array.from(board.querySelectorAll('.question-row')).map((row) => {
      const id = row.dataset.questionId;
      return {
        id,
        title: value(`question_edit_title_${id}`),
        desc: value(`question_edit_desc_${id}`),
        type: value(`question_edit_type_${id}`),
        options: value(`question_edit_options_${id}`),
        min: value(`question_edit_min_${id}`),
        max: value(`question_edit_max_${id}`),
        randomize_choices: checked(`question_randomize_choices_${id}`),
        required: checked(`question_required_${id}`)
      };
    });
    Shiny.setInputValue('group_layout', JSON.stringify({ groups, question_drafts }), { priority: 'event' });
  }
  function scrollDuringDrag(event) {
    if (!dragged) return;
    const delta = event.deltaY * (event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? window.innerHeight : 1);
    let element = event.target instanceof Element ? event.target : event.target?.parentElement;
    let scroller = null;
    while (element && element !== document.body) {
      const overflowY = window.getComputedStyle(element).overflowY;
      const canScroll = delta < 0 ? element.scrollTop > 0 : element.scrollTop + element.clientHeight < element.scrollHeight - 1;
      if ((overflowY === 'auto' || overflowY === 'scroll') && canScroll) {
        scroller = element;
        break;
      }
      element = element.parentElement;
    }
    event.preventDefault();
    if (scroller) scroller.scrollTop += delta;
    else window.scrollBy(0, delta);
    if (lastPointer) requestAnimationFrame(() => updateTarget(lastPointer.x, lastPointer.y));
  }
  document.addEventListener('wheel', scrollDuringDrag, { capture: true, passive: false });
  function updateTarget(x, y) {
    target = null;
    const hovered = document.elementFromPoint(x, y);
    if (setTarget({ target: hovered, clientY: y })) clearTarget(target);
    else clearTarget();
  }
  function finishDrag(x, y) {
    updateTarget(x, y);
    if (!dragged || !target) { reset(); return; }
    const list = target.closest('.question-list');
    const reference = target.classList.contains('insert-question-button-final') ? target : target.nextElementSibling;
    if (list && reference !== dragged) list.insertBefore(dragged, reference);
    suppressClick = true;
    setTimeout(() => { suppressClick = false; }, 0);
    reset();
    sendLayout();
  }
  document.addEventListener('click', (event) => {
    if (!suppressClick) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    suppressClick = false;
  }, true);
  document.addEventListener('pointerdown', (event) => {
    if (event.button !== 0 || event.pointerType !== 'mouse') return;
    const eventTarget = event.target instanceof Element ? event.target : event.target?.parentElement;
    const card = eventTarget?.closest('.question-card');
    if (!card || !root()?.contains(card) || eventTarget?.closest('button,input,textarea,select,label')) return;
    pending = {
      card,
      row: card.closest('.question-row'),
      group: card.closest('.group-card'),
      pointerId: event.pointerId,
      startX: event.clientX,
      startY: event.clientY
    };
  });
  document.addEventListener('pointermove', (event) => {
    if (!pending || event.pointerId !== pending.pointerId) return;
    if (!dragged) {
      if (Math.hypot(event.clientX - pending.startX, event.clientY - pending.startY) < 5) return;
      dragged = pending.row;
      sourceGroup = pending.group;
      try { pending.card.setPointerCapture(event.pointerId); } catch (_) {}
      dragged?.classList.add('dragging');
    }
    event.preventDefault();
    lastPointer = { x: event.clientX, y: event.clientY };
    updateTarget(lastPointer.x, lastPointer.y);
  });
  document.addEventListener('pointerup', (event) => {
    if (!pending || event.pointerId !== pending.pointerId) return;
    if (dragged) finishDrag(event.clientX, event.clientY);
    else pending = null;
  });
  document.addEventListener('pointercancel', (event) => {
    if (pending && event.pointerId === pending.pointerId) reset();
  });
})();
