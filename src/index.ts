import { StateDefinition, StateMapping, History } from '../@types/module'
import { transitionConstructor } from './transition'

export function createStateMachine (stateDefinitions: StateDefinition[]) {
  const stateDefinitionById: StateMapping = {}
  stateDefinitions.forEach(s => {
    stateDefinitionById[s.id] = {
      ...s,
      transitions: s.transitions || []
    } as StateDefinition
  })

  const transition = transitionConstructor(stateDefinitionById)

  // TODO: add check for if the reverts where legal
  function checkValidity (history: History) {
    return history.every((state, index) => {
      const _state = stateDefinitionById[state]

      if (_state.isInternal) return true // skip internals
      return transition.isAllowed(history.slice(0, index), _state.id)
    })
  }

  return { transition, checkValidity }
}
