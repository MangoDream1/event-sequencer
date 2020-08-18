import { InternalState, StateDefinition, StateMapping, State } from '../@types/module'
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
  function checkValidity (history: State[]) {
    return history.every((state, index) => {
      const _state = stateById[state.id]

      if (_state.isReverted) return true
      if (_state.isInternal) return true // skip internals
      return transition.isAllowed(history.slice(0, index), _state.id)
    })
  }

  return { transition, revert, checkValidity }
}
