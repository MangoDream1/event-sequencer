import { EventDefinition, EventMapping, EventSequence, EventId } from '../@types/module'

export type EventSequencer = typeof createEventSequencer

export function createEventSequencer (stateDefinitions: EventDefinition[]) {
  const stateDefinitionById: EventMapping = {}
  stateDefinitions.forEach(s => {
    stateDefinitionById[s.id] = {
      ...s,
      transitions: s.transitions || []
    } as EventDefinition
  })

  function _findTransitionRoutes (state: EventDefinition, path = [] as EventSequence): ([EventDefinition, EventSequence])[] {
    const states = state.transitions.map(s => stateDefinitionById[s])

    const externalTransitions: [EventDefinition, EventSequence][] = states.filter(s => !s.isInternal).map(s => ([s, [...path, state.id, s.id]]))

    const internalStates = states.filter(s => s.isInternal)
    const internalTransitions = internalStates.flatMap(s => _findTransitionRoutes(s, [...path, state.id]))

    return [...externalTransitions, ...internalTransitions]
  }

  function _isValidDestination (current: EventDefinition, dest: EventId): [boolean, EventSequence] {
    const transitionRoutes = _findTransitionRoutes(current)
    const connection = transitionRoutes.find(([state]) => state.id === dest)

    if (!connection) { // not possible to reach directly; invalid
      return [false, []]
    }

    return [true, connection[1]]
  }

  function _getStateOptions (state: EventId): EventId[] {
    const _state = stateDefinitionById[state]
    return Array.from(new Set(_findTransitionRoutes(_state).flatMap(x => {
      return x[1].slice(1)
    }))).reduce<EventId[]>((states, state) => {
      if (stateDefinitionById[state].isInternal) return states
      states.push(state)
      return states
    }, [])
  }

  function _findInvalid (history: EventSequence): [number, number][] {
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

  function _removeInvalid (history: EventSequence, invalidRanges: [number, number][]): EventSequence {
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

  function _isAndAllowed (history: EventSequence, dest: EventId) {
    const destState = stateDefinitionById[dest]
    if (!destState.AND || !destState.AND.length) return true // no AND; therefore all allowed
    if (destState.id === history[history.length - 1]) return true // for repeating states with AND requirement

    const lookBackLength = destState.AND.length
    return history.slice(-lookBackLength).every(h => destState.AND?.includes(h)) // every needs to be true
  }

  // uses transition since all checks in there
  function _isAllowed (history: EventSequence, dest: EventId): Boolean {
    try {
      transition(history, dest)
      return true
    } catch (error) {
      return false
    }
  }

  function _findLatestState (stateById: EventMapping, history: EventSequence) {
    return [...history].reverse().find(state => {
      const _state = stateById[state]
      return !_state.isInternal
    })
  }

  /**
   * Transitions to `dest`
   * @param history history array until now
   * @param dest the destination state
   * @param removeInvalid flag to remove the invalid states; default false
   */
  function transition (history: EventSequence, dest: EventId, removeInvalid = false): EventSequence {
    const currentStateId = _findLatestState(stateDefinitionById, history)

    if (!currentStateId) { // if history empty; only beginning state allowed
      const state = stateDefinitionById[dest]
      if (!state.isBeginning) {
        throw new Error('Not allowed; history is empty, only beginning allowed')
      }
      return [stateDefinitionById[dest].id]
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

  /**
   * Gets all the allowed transitions in the state machine
   */
  function getAllTransitions (): [EventId, EventId][] {
    const beginnings = Object.values(stateDefinitionById).filter(s => s.isBeginning)

    const recursion = (state: EventId, options: [EventId, EventId][]) : [EventId, EventId][] => {
      const stateOptions = _getStateOptions(state)
      if (!stateOptions.length) return options

      return stateOptions.flatMap(option => {
        const transition: [EventId, EventId] = [state, option]
        if (options.some(([A, B]) => state === A && option === B)) return options

        return recursion(option, [...options, transition])
      })
    }

    const allOptions = beginnings.flatMap(b => recursion(b.id, []))

    // remove duplicates
    return allOptions.reduce<[EventId, EventId][]>((allOptions, [A, B]) => {
      if (allOptions.some(([C, D]) => A === C && B === D)) return allOptions
      return [...allOptions, [A, B]]
    }, [])
  }

  /**
   * Gets all the allowed transitions in that point of time based on history array
   * @param history history array
   */
  function getTransitions (history: EventSequence): EventId[] {
    const currentState = _findLatestState(stateDefinitionById, history)
    if (!currentState) return Object.values(stateDefinitionById).filter(s => s.isBeginning).map(s => s.id) // empty; then only beginning states
    return _getStateOptions(currentState)
  }

  /**
   * Checks if the history is valid; i.e. the transitions where allowed at each point in time
   * @param history history array
   */
  function checkValidity (history: EventSequence) {
    return history.every((state, index) => {
      return _isAllowed(history.slice(0, index), state)
    })
  }

  return {
    getAllTransitions,
    getTransitions,
    checkValidity,
    transition
  }
}
