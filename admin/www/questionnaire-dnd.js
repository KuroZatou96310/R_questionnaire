(function () {
  if (window.questionnaireDragDropInstalled) return;
  window.questionnaireDragDropInstalled = true;

  let draggedQuestion = null;
  let sourceGroup = null;
  let dropTarget = null;
  let dropMode = null;
  let dropAfter = false;
  const root = () => document.getElementById('question_list_ui');

  function clearMarks() {
    document.querySelectorAll('.drop-before,.drop-after,.drop-group,.drop-group-target,.drop-target').forEach((el) => {
      el.classList.remove('drop-before', 'drop-after', 'drop-group', 'drop-group-target', 'drop-target');
    });
  }

  function resetDrag() {
    if (draggedQuestion) draggedQuestion.classList.remove('dragging');
    draggedQuestion = null;
    sourceGroup = null;
    dropTarget = null;
    dropMode = null;
    clearMarks();
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
    const output = root();
    if (!draggedQuestion || !output) return;

    const targetQuestion = event.target.closest('.question-card');
    const targetList = event.target.closest('.question-list');
    const targetInsert = event.target.closest('.insert-question-button');
    if (targetInsert && targetList && output.contains(targetInsert)) {
      event.preventDefault();
      clearMarks();
      dropTarget = targetInsert;
      dropMode = 'insert-gap';
      targetInsert.classList.add('drop-target');
      const targetGroup = targetList.closest('.group-card');
      if (targetGroup !== sourceGroup) targetGroup.classList.add('drop-group-target');
      return;
    }

    if (targetQuestion && targetQuestion !== draggedQuestion && output.contains(targetQuestion)) {
      event.preventDefault();
      dropTarget = targetQuestion;
      const targetGroup = targetQuestion.closest('.group-card');
      const rect = targetQuestion.getBoundingClientRect();
      const ratio = Math.max(0, Math.min(1, (event.clientY - rect.top) / rect.height));
      dropAfter = ratio >= 0.5;
      clearMarks();

      if (targetGroup === sourceGroup) {
        dropMode = 'reorder-question';
        targetQuestion.classList.add(dropAfter ? 'drop-after' : 'drop-before');
      } else {
        dropMode = 'join-group';
        targetGroup.classList.add('drop-group-target');
      }
      targetQuestion.classList.add(dropAfter ? 'drop-after' : 'drop-before');
      return;
    }

    if (targetList && output.contains(targetList) && targetList.closest('.group-card') !== sourceGroup) {
      event.preventDefault();
      dropTarget = targetList;
      dropMode = 'join-group';
      clearMarks();
      targetList.classList.add('drop-target');
      targetList.closest('.group-card').classList.add('drop-group-target');
      return;
    }

    dropTarget = null;
    dropMode = null;
    clearMarks();
  });

  document.addEventListener('drop', (event) => {
    if (!draggedQuestion || !dropMode || !dropTarget) return;
    event.preventDefault();
    const question = draggedQuestion;
    const oldGroup = sourceGroup;
    const targetGroup = dropTarget.matches('.question-list')
      ? dropTarget.closest('.group-card')
      : dropTarget.closest('.group-card');

    if (dropMode === 'insert-gap') {
      const list = dropTarget.closest('.question-list');
      const nextQuestion = dropTarget.nextElementSibling?.matches('.question-card')
        ? dropTarget.nextElementSibling
        : null;
      if (nextQuestion !== question) list.insertBefore(question, nextQuestion);
      removeEmptyGroup(oldGroup);
    } else if (dropMode === 'reorder-question') {
      const reference = dropAfter ? dropTarget.nextSibling : dropTarget;
      if (reference !== question) dropTarget.parentElement.insertBefore(question, reference);
    } else if (dropMode === 'join-group') {
      const list = targetGroup.querySelector('.question-list');
      if (dropTarget.matches('.question-card')) {
        const reference = dropAfter ? dropTarget.nextSibling : dropTarget;
        if (reference !== question) list.insertBefore(question, reference);
      } else {
        list.appendChild(question);
      }
      removeEmptyGroup(oldGroup);
    }

    resetDrag();
    sendLayout();
  });

  document.addEventListener('dragend', resetDrag);
  document.addEventListener('drop', (event) => {
    if (!root()?.contains(event.target)) resetDrag();
  });
})();
