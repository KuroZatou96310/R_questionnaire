(function () {
  if (window.questionnaireDragDropInstalled) return;
  window.questionnaireDragDropInstalled = true;
  let dragged = null, sourceGroup = null, target = null, after = false;
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
  function reset() { dragged?.classList.remove('dragging'); dragged = sourceGroup = target = null; clearTarget(); }
  function setInsertTarget(button, y) {
    if (!button?.matches('.insert-question-button')) return false;
    target = button;
    button.classList.add('drop-insert-target');
    button.textContent = 'ここに挿入';
    if (button.classList.contains('insert-question-button-first') && Number.isFinite(y)) {
      const list = button.closest('.question-list');
      const rect = list.getBoundingClientRect();
      button.style.top = `${y - rect.top + list.scrollTop - list.clientTop}px`;
    }
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
      return setInsertTarget(slot, after ? rect.bottom : rect.top);
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
  document.addEventListener('dragstart', (event) => {
    const eventTarget = event.target instanceof Element ? event.target : event.target?.parentElement;
    const card = eventTarget?.closest('.question-card');
    if (eventTarget?.closest('button,input,textarea,select,label')) { event.preventDefault(); return; }
    if (!card || !root()?.contains(card)) { event.preventDefault(); return; }
    dragged = card.closest('.question-row'); sourceGroup = card.closest('.group-card');
    event.dataTransfer.setData('text/plain', card.dataset.questionId); event.dataTransfer.effectAllowed = 'move';
    requestAnimationFrame(() => dragged?.classList.add('dragging'));
  });
  document.addEventListener('dragover', (event) => {
    if (!dragged) return;
    target = null;
    if (setTarget(event)) {
      clearTarget(target);
      event.preventDefault(); event.dataTransfer.dropEffect = 'move';
    } else clearTarget();
  });
  document.addEventListener('drop', (event) => {
    if (!dragged) return;
    target = null;
    if (!setTarget(event)) { clearTarget(); reset(); return; }
    clearTarget(target);
    event.preventDefault();
    const list = target.closest('.question-list');
    const reference = target.classList.contains('insert-question-button-final') ? target : target.nextElementSibling;
    if (reference !== dragged) list.insertBefore(dragged, reference);
    reset(); sendLayout();
  });
  document.addEventListener('dragend', reset);
})();
