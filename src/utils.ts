import { History, InternalState, StateId, StateMapping, Path } from '../@types/module'

export function transitionHelper (stateDefinitionById: StateMapping, transitionType: 'transitions' | 'revertTransitions') {
  function findTransitionRoutes (state: InternalState, path = [] as Path): ([InternalState, Path])[] {
    if (state.isEnd) return []

    const states = state[transitionType].map(s => stateDefinitionById[s])

    const externalTransitions: [InternalState, Path][] = states.filter(s => !s.isInternal).map(s => ([s, [...path, state.id, s.id]]))

    const internalStates = states.filter(s => s.isInternal)
    const internalTransitions = internalStates.flatMap(s => findTransitionRoutes(s, [...path, state.id]))

    return [...externalTransitions, ...internalTransitions]
  }

  function isValidDestination (current: InternalState, dest: StateId): [boolean, Path] {
    const transitionRoutes = findTransitionRoutes(current)
    const connection = transitionRoutes.find(([state]) => state.id === dest)

    if (!connection) { // not possible to reach directly; invalid
      return [false, []]
    }

    return [true, connection[1]]
  }

  function getOptions (state: StateId): StateId[] {
    const _state = stateDefinitionById[state]
    return Array.from(new Set(findTransitionRoutes(_state).flatMap(x => {
      return x[1].slice(1)
    }))).reduce<StateId[]>((states, state) => {
      if (stateDefinitionById[state].isInternal) return states
      states.push(state)
      return states
    }, [])
  }

  return { findTransitionRoutes, isValidDestination, getOptions }
}

export function findLatestState (stateById: StateMapping, history: History) {
  return [...history].reverse().find(state => {
    const _state = stateById[state]
    return !_state.isInternal
  })
}
