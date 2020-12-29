import { History, StateId, StateMapping, StateDefinition, Path } from '../@types/module'

function _findLatestState (stateById: StateMapping, history: History) {
  return [...history].reverse().find(state => {
    const _state = stateById[state]
    return !_state.isInternal
  })
}

export function transitionConstructor (stateDefinitionById: StateMapping) {
  function _findTransitionRoutes (state: StateDefinition, path = [] as Path): ([StateDefinition, Path])[] {
    const states = state.transitions.map(s => stateDefinitionById[s])

    const externalTransitions: [StateDefinition, Path][] = states.filter(s => !s.isInternal).map(s => ([s, [...path, state.id, s.id]]))

    const internalStates = states.filter(s => s.isInternal)
    const internalTransitions = internalStates.flatMap(s => _findTransitionRoutes(s, [...path, state.id]))

    return [...externalTransitions, ...internalTransitions]
  }

  function _isValidDestination (current: StateDefinition, dest: StateId): [boolean, Path] {
    const transitionRoutes = _findTransitionRoutes(current)
    const connection = transitionRoutes.find(([state]) => state.id === dest)

    if (!connection) { // not possible to reach directly; invalid
      return [false, []]
    }

    return [true, connection[1]]
  }

  function _getOptions (state: StateId): StateId[] {
    const _state = stateDefinitionById[state]
    return Array.from(new Set(_findTransitionRoutes(_state).flatMap(x => {
      return x[1].slice(1)
    }))).reduce<StateId[]>((states, state) => {
      if (stateDefinitionById[state].isInternal) return states
      states.push(state)
      return states
    }, [])
  }

  function _findInvalid (history: History): [number, number][] {
    const set = new Set(history)
    if (set.size === history.length) return [] // no duplicates; therefore no reverts
    const dupIndexes = history.reduce<{[key: string]: number[]}>((dupIndexes, s, index) => {
      if (!dupIndexes[s]) dupIndexes[s] = []
      dupIndexes[s] = [...dupIndexes[s], index]
      return dupIndexes
    }, {})

    let alreadyInvalid: [number, number] = [0, 0]
    return Object.values(dupIndexes).reduce<[number, number][]>((invalidedRanges, indexes) => {
      const jumps = indexes.reduce<[number, number][]>((jumps, c, i) => {
        const nextI = i + 1
        if (nextI >= indexes.length) return jumps // if uneven; then last one is latest and cannot be invalidated

        // filter out repeats
        if (c + 1 === indexes[nextI]) return jumps

        // filter out those that are already invalidated
        const jump: [number, number] = [c, indexes[nextI]]
        if (c > alreadyInvalid[0] && c < alreadyInvalid[1]) return jumps
        alreadyInvalid = jump

        return [...jumps, jump]
      }, [])

      return [...invalidedRanges, ...jumps]
    }, [])
  }

  function _removeInvalid (history: History, invalidRanges: [number, number][]): History {
    if (invalidRanges.length === 0) return history // nothing invalid; all good

    const start = 0
    const end = history.length

    const validRanges = invalidRanges.reduce<[number, number][]>((validRanges, c, i) => {
      const nextI = i + 1

      if (i === 0) validRanges = [[start, c[0]]]
      if (nextI >= invalidRanges.length) return [...validRanges, [c[1], end]]

      return [...validRanges, [c[1], invalidRanges[nextI][0]]]
    }, [])

    return validRanges.flatMap(([x, y]) => history.slice(x, y))
  }

  function _isAndAllowed (history: History, dest: StateId) {
    const destState = stateDefinitionById[dest]
    if (!destState.AND || !destState.AND.length) return true // no AND; therefore all allowed
    if (destState.id === history[history.length - 1]) return true // for repeating states with AND requirement

    const lookBackLength = destState.AND.length
    return history.slice(-lookBackLength).every(h => destState.AND?.includes(h)) // every needs to be true
  }

  function exec (history: History, dest: StateId, removeInvalid = false): History {
    const currentStateId = _findLatestState(stateDefinitionById, history)
    if (!currentStateId) { // if history empty; only beginning state allowed
      const state = stateDefinitionById[dest]
      if (state.isBeginning) {
        return [stateDefinitionById[dest].id]
      }
      throw new Error('Not allowed; history is empty, no beginning')
    }
    const currentState = stateDefinitionById[currentStateId]

    const [valid, path] = _isValidDestination(currentState, dest)
    if (!valid) throw new Error(`Not allowed; no path to destination; ${currentState.id} to ${dest}`)

    // TODO: clean this mess
    const neuteredHistory = [...history.slice(0, -1)] // remove last, since first in path always currentState
    path.forEach(stateId => {
      const invalid = _findInvalid(neuteredHistory)
      const _history = _removeInvalid(neuteredHistory, invalid)

      const isAllowed = _isAndAllowed(_history, stateId)
      if (!isAllowed) {
        throw new Error(`Not allowed; dest ${stateId} requisites not met`)
      }

      neuteredHistory.push(stateId)
    })

    // filter out internals
    let newHistory = neuteredHistory
    if (!removeInvalid) newHistory = [...history.slice(0, -1), ...path]
    return newHistory.filter(id => !stateDefinitionById[id].isInternal)
  }

  function isAllowed (history: History, dest: StateId): boolean {
    try {
      exec(history, dest) // transition does the check inside; there are no side effect
      return true
    } catch (error) {
      return false
    }
  }

  function getAllTransitions (): [StateId, StateId][] {
    const beginnings = Object.values(stateDefinitionById).filter(s => s.isBeginning)

    const recursion = (state: StateId, options: [StateId, StateId][]) : [StateId, StateId][] => {
      const stateOptions = _getOptions(state)
      if (!stateOptions.length) return options

      return stateOptions.flatMap(option => {
        const transition: [StateId, StateId] = [state, option]
        if (options.some(([A, B]) => state === A && option === B)) return options
        // if (state === option) return options

        return recursion(option, [...options, transition])
      })
    }

    const allOptions = beginnings.flatMap(b => recursion(b.id, []))

    // remove duplicates
    return allOptions.reduce<[StateId, StateId][]>((allOptions, [A, B]) => {
      if (allOptions.some(([C, D]) => A === C && B === D)) return allOptions
      return [...allOptions, [A, B]]
    }, [])
  }

  function getCurrentOptions (history: History): StateId[] {
    const currentState = _findLatestState(stateDefinitionById, history)
    if (!currentState) return Object.values(stateDefinitionById).filter(s => s.isBeginning).map(s => s.id) // empty; then only beginning states
    return _getOptions(currentState)
  }

  return { exec, isAllowed, getAllTransitions, getCurrentOptions }
}
