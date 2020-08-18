import { State, StateId, StateMapping } from '../@types/module'
import { transitionHelper, findLatestState } from './utils'

export function revertConstructor (stateById: StateMapping) {
  const { isValidDestination, getOptions } = transitionHelper(stateById, 'revertTransitions')

  // TODO: decide on: possible want to return tuple with [success, State[]]; instead of throwing
  function exec (history: State[], dest: StateId): State[] {
    const currentStateId = findLatestState(stateById, history)?.id
    if (!currentStateId) throw new Error('Not allowed; no current state found')
    const currentState = stateById[currentStateId]

    const [valid] = isValidDestination(currentState, dest)
    if (!valid) throw new Error(`Not allowed; no path to destination; ${currentState.id} to ${dest}`)

    const revertToIndex = history.length - [...history].reverse().findIndex(h => h.id === dest && h.isReverted === false)
    if (revertToIndex < 0) throw new Error('Revert destination not in history')

    return history.map((state, index) => {
      if (index < revertToIndex) return state
      return { ...state, isReverted: true }
    })
  }

  function isAllowed (history: State[], dest: StateId): boolean {
    try {
      exec(history, dest) // transition does the check inside; there are no side effect
      return true
    } catch (error) {
      return false
    }
  }

  return { exec, getOptions, isAllowed }
}
