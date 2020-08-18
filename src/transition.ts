import { State, StateId, StateMapping } from '../@types/module'
import { transitionHelper, findLatestState } from './utils'

export function transitionConstructor (stateById: StateMapping) {
  const { isValidDestination, getOptions } = transitionHelper(stateById, 'transitions')

  function _isOrAllowed (history: State[], dest: StateId) {
    const destState = stateById[dest]
    if (!destState.OR || !destState.OR.length) return true // no OR; therefore all allowed
    if (destState.id === history[history.length - 1].id) return true

    const lookBackLength = destState.OR.length
    return history.slice(-lookBackLength).some(h => destState.OR.includes(h.id)) // any true enough
  }

  function _isAndAllowed (history: State[], dest: StateId) {
    const destState = stateById[dest]
    if (!destState.AND || !destState.AND.length) return true // no AND; therefore all allowed
    if (destState.id === history[history.length - 1].id) return true

    const lookBackLength = destState.AND.length
    return history.slice(-lookBackLength).every(h => destState.AND.includes(h.id)) // every needs to be true
  }

  // TODO: decide on: possible want to return tuple with [success, State[]]; instead of throwing
  function exec (history: State[], dest: StateId): State[] {
    const currentStateId = findLatestState(stateById, history)?.id
    if (!currentStateId) { // if history empty; only beginning state allowed
      const state = stateById[dest]
      if (state.isBeginning) {
        return [stateById[dest]]
      }
      throw new Error('Not allowed; history is empty, no beginning')
    }
    const currentState = stateById[currentStateId]

    const [valid, path] = isValidDestination(currentState, dest)
    if (!valid) throw new Error(`Not allowed; no path to destination; ${currentState.id} to ${dest}`)

    const newHistory = [...history.slice(0, -1)] // remove last, since first in path always currentState

    path.forEach(stateId => {
      const isAllowed = _isOrAllowed(newHistory, stateId) && _isAndAllowed(newHistory, stateId)
      if (!isAllowed) throw new Error(`Not allowed; dest ${stateId} requisites not met`)
      newHistory.push({ id: stateId })
    })

    return newHistory
  }

  function isAllowed (history: State[], dest: StateId): boolean {
    try {
      exec(history, dest) // transition does the check inside; there are no side effect
      return true
    } catch (error) {
      return false
    }
  }

  function getAllTransitions (): [StateId, StateId][] {
    const beginnings = Object.values(stateById).filter(s => s.isBeginning)

    const recursion = (state: StateId, options: [StateId, StateId][]) : [StateId, StateId][] => {
      const stateOptions = getOptions(state)
      if (!stateOptions.length) return options

      return stateOptions.flatMap(option => {
        const transition: [StateId, StateId] = [state, option]
        if (options.some(([A, B]) => state === A && option === B)) return options
        if (state === option) return options

        return recursion(option, [...options, transition])
      })
    }

    const allOptions = beginnings.flatMap(b => recursion(b.id, []))
    return allOptions.reduce<[StateId, StateId][]>((allOptions, [A, B]) => {
      if (allOptions.some(([C, D]) => A === C && B === D)) return allOptions
      return [...allOptions, [A, B]]
    }, [])
  }

  function getCurrentOptions (history: State[]): StateId[] {
    const currentState = findLatestState(stateById, history)
    if (!currentState) return Object.values(stateById).filter(s => s.isBeginning).map(s => s.id) // empty; then only beginning states
    return getOptions(currentState.id)
  }

  return { exec, isAllowed, getAllTransitions, getOptions, getCurrentOptions }
}
