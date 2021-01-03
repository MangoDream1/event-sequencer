import { EventDefinition, EventMapping, EventSequence, EventId } from './types'

export type EventSequencer = ReturnType<typeof createEventSequencer>

export function createEventSequencer (eventDefinitions: EventDefinition[]) {
  const idToEventDefinition: EventMapping = {}
  eventDefinitions.forEach(s => {
    idToEventDefinition[s.id] = {
      ...s,
      transitions: s.transitions || []
    } as EventDefinition
  })

  function _findTransitionRoutes (event: EventDefinition, path = [] as EventSequence): ([EventDefinition, EventSequence])[] {
    const events = event.transitions.map(s => idToEventDefinition[s])

    const externalTransitions: [EventDefinition, EventSequence][] = events.filter(s => !s.isInternal).map(s => ([s, [...path, event.id, s.id]]))

    const internalevents = events.filter(s => s.isInternal)
    const internalTransitions = internalevents.flatMap(s => _findTransitionRoutes(s, [...path, event.id]))

    return [...externalTransitions, ...internalTransitions]
  }

  function _areEventsConnected (from: EventId, to: EventId): [boolean, EventSequence] {
    const fromDefinition = idToEventDefinition[from]
    const transitionRoutes = _findTransitionRoutes(fromDefinition)
    const connection = transitionRoutes.find(([event]) => event.id === to)

    if (!connection) { // not possible to reach directly; invalid
      return [false, []]
    }

    return [true, connection[1]]
  }

  function _getEventOptions (event: EventId): EventId[] {
    const _event = idToEventDefinition[event]
    return Array.from(new Set(_findTransitionRoutes(_event).flatMap(x => {
      return x[1].slice(1)
    }))).reduce<EventId[]>((events, event) => {
      if (idToEventDefinition[event].isInternal) return events
      events.push(event)
      return events
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
    const destEvent = idToEventDefinition[dest]
    if (!destEvent.AND || !destEvent.AND.length) return true // no AND; therefore all allowed
    if (destEvent.id === history[history.length - 1]) return true // for repeating events with AND requirement

    const lookBackLength = destEvent.AND.length
    return history.slice(-lookBackLength).every(h => destEvent.AND?.includes(h)) // every needs to be true
  }

  function _findLatestEvent (eventById: EventMapping, history: EventSequence) {
    return [...history].reverse().find(event => {
      const _event = eventById[event]
      return !_event.isInternal
    })
  }

  /**
   * Checks if the transition is allowed; returns boolean
   * Note: it uses the `transition` function due to it containing all the checks
   * @param history
   * @param dest
   */
  function isTransitionAllowed (history: EventSequence, dest: EventId): boolean {
    try {
      transition(history, dest)
      return true
    } catch (error) {
      return false
    }
  }

  /**
   * Transitions to `dest` and updates the history accordingly
   * @param history history array
   * @param dest the destination event
   * @param removeInvalid flag to remove the invalid events; default false
   */
  function transition (history: EventSequence, dest: EventId, removeInvalid = false): EventSequence {
    const currentEventId = _findLatestEvent(idToEventDefinition, history)

    if (!currentEventId) { // if history empty; only beginning event allowed
      const event = idToEventDefinition[dest]
      if (!event.isBeginning) {
        throw new Error('Not allowed; history is empty, only beginning allowed')
      }
      return [idToEventDefinition[dest].id]
    }

    const [valid, path] = _areEventsConnected(currentEventId, dest)
    if (!valid) throw new Error(`Not allowed; no path to destination; ${currentEventId} to ${dest}`)

    let newHistory = [...history.slice(0, -1)] // remove last, since first in path always currentEvent
    path.forEach(eventId => {
      const invalid = _findInvalid(newHistory)
      const _history = _removeInvalid(newHistory, invalid)

      const isAllowed = _isAndAllowed(_history, eventId)
      if (!isAllowed) {
        throw new Error(`Not allowed; dest ${eventId} requisites not met`)
      }

      newHistory.push(eventId)
    })

    // filter out internals
    if (removeInvalid) {
      const newInvalids = _findInvalid(newHistory)
      newHistory = _removeInvalid(newHistory, newInvalids)
    }
    return newHistory.filter(id => !idToEventDefinition[id].isInternal)
  }

  /**
   * Gets all the allowed transitions in the event machine
   */
  function getAllTransitions (): [EventId, EventId][] {
    const beginnings = Object.values(idToEventDefinition).filter(s => s.isBeginning)

    const recursion = (event: EventId, options: [EventId, EventId][]) : [EventId, EventId][] => {
      const eventOptions = _getEventOptions(event)
      if (!eventOptions.length) return options

      return eventOptions.flatMap(option => {
        const transition: [EventId, EventId] = [event, option]
        if (options.some(([A, B]) => event === A && option === B)) return options

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
  function getAllowedTransitions (history: EventSequence): EventId[] {
    const currentEvent = _findLatestEvent(idToEventDefinition, history)
    if (!currentEvent) return Object.values(idToEventDefinition).filter(s => s.isBeginning).map(s => s.id) // empty; then only beginning events
    return _getEventOptions(currentEvent).filter(event => isTransitionAllowed(history, event))
  }

  /**
   * Get all external events registered
   */
  function getAllEvents (): EventId[] {
    return Object.values(idToEventDefinition).reduce<EventId[]>((externalEvents, event) => {
      if (!event.isInternal) {
        externalEvents.push(event.id)
      }

      return externalEvents
    }, [])
  }

  /**
   * Checks if the history is valid; i.e. the transitions where allowed at each point in time
   * @param history history array
   */
  function checkValidity (history: EventSequence) {
    return history.every((event, index) => {
      return isTransitionAllowed(history.slice(0, index), event)
    })
  }

  return {
    getAllTransitions,
    getAllowedTransitions,
    checkValidity,
    transition,
    isTransitionAllowed,
    getAllEvents
  }
}
