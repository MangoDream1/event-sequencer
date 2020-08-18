import { InternalState, StateDefinition, StateMapping } from '../@types/module'
import { transitionConstructor } from './transition'
import { revertConstructor } from './revert'

export function createStateMachine (stateDefinitions: StateDefinition[]) {
  const stateById: StateMapping = {}
  stateDefinitions.forEach(s => {
    stateById[s.id] = {
      ...s,
      transitions: s.transitions || [],
      revertTransitions: s.revertTransitions || []
    } as InternalState
  })

  const transition = transitionConstructor(stateById)
  const revert = revertConstructor(stateById)

  // TODO: add check for if the reverts where legal
  function checkValidity (history: InternalState[]) {
    return history.every((state, index) => {
      if (state.isReverted) return true
      if (state.isInternal) return true // skip internals
      return transition.isAllowed(history.slice(0, index), state.id)
    })
  }

  return { transition, revert, checkValidity }
}
