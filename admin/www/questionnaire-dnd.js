(function () {
  if (window.questionnaireDragDropInstalled) return;
  window.questionnaireDragDropInstalled = true;

  let draggedQuestion = null;
  let sourceGroup = null;
  let dropTarget = null;
  let dropMode = null;
  let dropAfter = false;
  let dropIndicator = null;
  const root = () => document.getElementById('question_list_ui');

  function clearMarks() {
    document.querySelectorAll('.drop-group-target,.drop-target').forEach((element) => {
      element.classList.remove('drop-group-target', 'drop-target');
    });
    dropIndicator?.remove();
    dropIndicator = null;
  }

  function resetDrag() {
    draggedQuestion?.classList.remove('dragging');
    draggedQuestion = null;
    sourceGroup = null;
    dropTarget = null;
    dropMode = null;
    clearMarks();
  }

  function setDropTarget(target, mode, after = false) {
    if (target !== dropTarget || mode !== dropMode || after !== dropAfter) clearMarks();
    dropTarget = target;
    dropMode = mode;
    dropAfter = after;
  }

  function showInsertionPosition(list, clientY) {
    if (!dropIndicator || dropIndicator.parentElement !== list) {
      dropIndicator?.remove();
      dropIndicator = document.createElement('div');
      dropIndicator.className = 'drop-position-indicator';
      list.appendChild(dropIndicator);
    }
    const listRect = list.getBoundingClientRect();
    dropIndicator.style.top = `${clientY - listRect.top + list.scrollTop - list.clientTop}px`;
  }

  // Dragover と drop の両方で同じ座標判定を行い、表示位置と確定位置を揃える。
  function updateDropTarget(event) {
    const output = root();
    if (!draggedQuestion || !output) return false;

    const hovered = document.elementFromPoint(event.clientX, event.clientY);
    const insertButton = hovered?.closest('.insert-question-button');
    const question = hovered?.closest('.question-card');
    const group = hovered?.closest('.group-card');
    let list = hovered?.closest('.question-list');

    if (insertButton && list && output.contains(insertButton)) {
      setDropTarget(insertButton, 'insert-gap');
      insertButton.classList.add('drop-target');
      if (list.closest('.group-card') !== sourceGroup) list.closest('.group-card').classList.add('drop-group-target');
      const rect = insertButton.getBoundingClientRect();
      showInsertionPosition(list, rect.top + rect.height / 2);
      return true;
    }

    if (question && question !== draggedQuestion && output.contains(question)) {
      const targetGroup = question.closest('.group-card');
      const rect = question.getBoundingClientRect();
      const after = event.clientY >= rect.top + rect.height / 2;
      const mode = targetGroup === sourceGroup ? 'reorder-question' : 'join-group';
      setDropTarget(question, mode, after);
      if (mode === 'join-group') {
        targetGroup.classList.add('drop-group-target');
      }
      list = targetGroup.querySelector(':scope > .question-list');
      showInsertionPosition(list, after ? rect.bottom : rect.top);
      return true;
    }

    if (group && output.contains(group) && group !== sourceGroup) {
      list = group.querySelector(':scope > .question-list');
      if (list) {
        setDropTarget(list, 'join-group');
        group.classList.add('drop-group-target');
        const listRect = list.getBoundingClientRect();
        showInsertionPosition(list, listRect.top + list.clientTop + list.clientHeight);
        return true;
      }
    }

    setDropTarget(null, null);
    return false;
  }

  function sendLayout() {
    const output = root();
    if (!output || !window.Shiny) return;
    const layout = Array.from(output.querySelectorAll('.group-card')).map((group) => ({
      group_id: group.dataset.groupId,
      question_ids: Array.from(group.querySelectorAll(':scope > .question-list > .question-card'))
        .map((question) => question.dataset.questionId)
    }));
    Shiny.setInputValue('group_layout', JSON.stringify(layout), { priority: 'event' });
  }

  function insertAtTarget(question) {
    if (dropMode === 'insert-gap') {
      const list = dropTarget.closest('.question-list');
      const nextQuestion = dropTarget.nextElementSibling?.matches('.question-card')
        ? dropTarget.nextElementSibling
        : null;
      if (nextQuestion !== question) list.insertBefore(question, nextQuestion);
      return;
    }

    if (dropMode === 'reorder-question' || dropMode === 'join-group') {
      const list = dropTarget.closest('.question-list');
      const reference = dropAfter ? dropTarget.nextSibling : dropTarget;
      if (reference !== question) list.insertBefore(question, reference);
    }
  }

  function removeEmptyGroup(group) {
    if (group && !group.querySelector('.question-card')) group.remove();
  }

  document.addEventListener('dragstart', (event) => {
    const question = event.target.closest('.question-card');
    if (!question || !root()?.contains(question)) return;
    draggedQuestion = question;
    sourceGroup = question.closest('.group-card');
    event.dataTransfer.setData('text/plain', question.dataset.questionId);
    event.dataTransfer.effectAllowed = 'move';
    requestAnimationFrame(() => question.classList.add('dragging'));
  });

  document.addEventListener('dragover', (event) => {
    if (!draggedQuestion) return;
    if (updateDropTarget(event)) event.preventDefault();
  });

  document.addEventListener('drop', (event) => {
    if (!draggedQuestion) return;
    if (!updateDropTarget(event) || !dropTarget || !dropMode) {
      resetDrag();
      return;
    }
    event.preventDefault();
    const oldGroup = sourceGroup;
    insertAtTarget(draggedQuestion);
    removeEmptyGroup(oldGroup);
    resetDrag();
    sendLayout();
  });

  document.addEventListener('dragend', resetDrag);
})();
