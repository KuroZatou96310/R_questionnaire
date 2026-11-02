(function () {
  if (window.questionnaireDragDropInstalled) return;
  window.questionnaireDragDropInstalled = true;
  let dragged = null, sourceGroup = null, target = null, after = false, mode = null, indicator = null;
  const root = () => document.getElementById('question_list_ui');
  function clear() {
    document.querySelectorAll('.drop-group-target,.drop-target').forEach((el) => el.classList.remove('drop-group-target', 'drop-target'));
    indicator?.remove(); indicator = null;
  }
  function reset() { dragged?.classList.remove('dragging'); dragged = sourceGroup = target = mode = null; clear(); }
  function markLine(parent, y) {
    if (!indicator || indicator.parentElement !== parent) {
      indicator?.remove(); indicator = document.createElement('div'); indicator.className = 'drop-position-indicator'; parent.appendChild(indicator);
    }
    const rect = parent.getBoundingClientRect();
    indicator.style.top = `${y - rect.top + parent.scrollTop - parent.clientTop}px`;
  }
  function setTarget(event) {
    const board = root();
    const eventTarget = event.target instanceof Element ? event.target : event.target?.parentElement;
    const hovered = eventTarget;
    const card = hovered?.closest('.question-card');
    const group = hovered?.closest('.group-card');
    if (!dragged || !board || !hovered || !board.contains(hovered)) return false;
    if (card && card !== dragged) {
      const targetGroup = card.closest('.group-card');
      const rect = card.getBoundingClientRect(); after = event.clientY >= rect.top + rect.height / 2;
      if (targetGroup === sourceGroup) {
        target = card; mode = 'question'; markLine(targetGroup.querySelector(':scope > .question-list'), after ? rect.bottom : rect.top);
      } else {
        target = card; mode = 'join-question'; targetGroup.classList.add('drop-group-target');
        markLine(targetGroup.querySelector(':scope > .question-list'), after ? rect.bottom : rect.top);
      }
      return true;
    }
    if (group && group !== sourceGroup) {
      const list = group.querySelector(':scope > .question-list');
      if (!list) return false;
      target = list.lastElementChild?.matches('.question-card') ? list.lastElementChild : list;
      mode = 'join-question'; after = true; group.classList.add('drop-group-target');
      markLine(list, list.getBoundingClientRect().bottom); return true;
    }
    return false;
  }
  function sendLayout() {
    const board = root(); if (!board || !window.Shiny) return;
    const layout = Array.from(board.querySelectorAll('.group-card')).map((group) => ({
      group_id: group.dataset.groupId,
      question_ids: Array.from(group.querySelectorAll(':scope > .question-list > .question-card')).map((q) => q.dataset.questionId)
    }));
    Shiny.setInputValue('group_layout', JSON.stringify(layout), { priority: 'event' });
  }
  document.addEventListener('dragstart', (event) => {
    const handle = event.target.closest?.('.drag-handle') || event.composedPath().find((node) => node instanceof Element && node.matches('.drag-handle'));
    const card = handle?.closest('.question-card');
    if (!card || !root()?.contains(card)) { event.preventDefault(); return; }
    dragged = card; sourceGroup = card.closest('.group-card');
    event.dataTransfer.setData('text/plain', card.dataset.questionId); event.dataTransfer.effectAllowed = 'move';
    requestAnimationFrame(() => card.classList.add('dragging'));
  });
  document.addEventListener('dragover', (event) => {
    if (!dragged) return;
    clear();
    if (setTarget(event)) { event.preventDefault(); event.dataTransfer.dropEffect = 'move'; }
  });
  document.addEventListener('drop', (event) => {
    if (!dragged) return;
    clear();
    if (!setTarget(event)) { reset(); return; }
    event.preventDefault();
    if (mode === 'question' || mode === 'join-question') {
      const list = target.closest('.question-list');
      const reference = target === list ? null : (after ? target.nextSibling : target);
      const oldGroup = dragged.closest('.group-card');
      const oldGap = dragged.nextElementSibling;
      if (mode === 'join-question' && oldGap?.matches('.insert-question-button')) oldGap.remove();
      if (reference !== dragged) list.insertBefore(dragged, reference);
      if (mode === 'join-question' && oldGroup.querySelectorAll('.question-card').length === 0) {
        const afterGroup = oldGroup.nextElementSibling;
        if (afterGroup?.matches('.insert-question-button')) afterGroup.remove();
        oldGroup.remove();
      }
    } else {
      const reference = after ? target.nextSibling : target;
      if (reference !== sourceGroup) root().insertBefore(sourceGroup, reference);
    }
    reset(); sendLayout();
  });
  document.addEventListener('dragend', reset);
})();
