import { Event, EventId } from './types'

interface Transition {
  from: EventId,
  to: EventId,
  path: EventId[],
}

export class EventSequencer {
  #idToEvent: Record<string, Event> = {}
  #fromToDestToTransition: Record<string, Record<string, Transition>> = {}
  #beginningEvents: EventId[] = []

  constructor (events: Event[]) {
    this.#idToEvent = events.reduce((idToEvent, e) => {
      idToEvent[e.id] = e
      return idToEvent
    }, this.#idToEvent)

    events.forEach(e => {
      if (e.isBeginning) { // store beginning ids
        this.#beginningEvents.push(e.id)
      }

      if (e.isInternal) return

      const transitions = this._findEventTransitions(e)
      transitions.forEach(t => {
        if (!this.#fromToDestToTransition[t.from]) this.#fromToDestToTransition[t.from] = {}
        this.#fromToDestToTransition[t.from][t.to] = t
      })
    })
  }

  private _findEventTransitions (event: Event, path = [] as EventId[]): Transition[] {
    const events = event.transitions.map(s => this.#idToEvent[s])

    const externalTransitions: Transition[] = events.filter(to => !to.isInternal).map(to => {
      return { to: to.id, from: path[0] || event.id, path: [...path, event.id, to.id] }
    })

    const internalEvents = events.filter(to => to.isInternal)
    const internalTransitions = internalEvents.flatMap(to => this._findEventTransitions(to, [...path, event.id]))

    return [...externalTransitions, ...internalTransitions]
  }

  private _findInvalid (history: EventId[]): [number, number][] {
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

  private _removeInvalid (history: EventId[], invalidRanges: [number, number][]): EventId[] {
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

  private _isAndAllowed (history: EventId[], dest: EventId) {
    const destEvent = this.#idToEvent[dest]
    if (!destEvent.AND || !destEvent.AND.length) return true // no AND; therefore all allowed
    if (destEvent.id === history[history.length - 1]) return true // for repeating events with AND requirement

    const lookBackLength = destEvent.AND.length
    return history.slice(-lookBackLength).every(h => destEvent.AND?.includes(h)) // every needs to be true
  }

  private _findLatestEvent (history: EventId[]) {
    return [...history].reverse().find(event => {
      const _event = this.#idToEvent[event]
      return !_event.isInternal
    })
  }

  /**
   * Checks if the transition to dest is allowed; returns boolean
   * Note: it uses the `transition` function due to it containing all the checks
   * @param history
   * @param dest
   */
  isTransitionAllowed (history: EventId[], destId: EventId): boolean {
    try {
      this.executeTransition(history, destId)
      return true
    } catch (error) {
      return false
    }
  }

  /**
   * Transitions to `dest` and updates the history accordingly
   * @param history history array
   * @param destId the destination event
   * @param removeInvalid flag to remove the invalid events; default false
   */
  executeTransition (history: EventId[], destId: EventId, removeInvalid = false): EventId[] {
    const fromId = this._findLatestEvent(history)

    if (!fromId) { // if history empty; only beginning event allowed
      const event = this.#idToEvent[destId]
      if (!event.isBeginning) {
        throw new Error('Not allowed; history is empty, only beginning allowed')
      }
      return [this.#idToEvent[destId].id]
    }

    const transition = this.#fromToDestToTransition[fromId]?.[destId]

    if (!transition) {
      throw new Error(`Not allowed; no path between ${fromId} and ${destId}`)
    }

    let newHistory = [...history.slice(0, -1)] // remove last, since first in path always currentEvent
    transition.path.forEach(eventId => {
      const invalid = this._findInvalid(newHistory)
      const _history = this._removeInvalid(newHistory, invalid)

      const isAllowed = this._isAndAllowed(_history, eventId)
      if (!isAllowed) {
        throw new Error(`Not allowed; dest ${eventId} requisites not met`)
      }

      newHistory.push(eventId)
    })

    // filter out internals
    if (removeInvalid) {
      const newInvalids = this._findInvalid(newHistory)
      newHistory = this._removeInvalid(newHistory, newInvalids)
    }
    return newHistory.filter(id => !this.#idToEvent[id].isInternal)
  }

  /**
   * Gets all the allowed transitions in the event machine
   */
  getAllTransitions (): [null | EventId, EventId][] {
    const transitions: [EventId, EventId][] = Object.values(this.#fromToDestToTransition).flatMap(dest => Object.values(dest)).map(t => [t.from, t.to])
    const beginningTransitions: [null, EventId][] = this.#beginningEvents.map(id => [null, id])
    return [...beginningTransitions, ...transitions]
  }

  /**
   * Gets all the allowed destinations in that point of time based on history array
   * @param history history array
   */
  getAllowedDestinations (history: EventId[]): EventId[] {
    const currentEvent = this._findLatestEvent(history)
    if (!currentEvent) return this.#beginningEvents

    const transitions = Object.values(this.#fromToDestToTransition[currentEvent])
    return transitions.filter(t => this.isTransitionAllowed(history, t.to)).map(t => t.to)
  }

  /**
   * Get all external events registered
   */
  getAllEvents (): EventId[] {
    return Object.values(this.#idToEvent).reduce<EventId[]>((externalEvents, event) => {
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
  checkValidity (history: EventId[]) {
    return history.every((event, index) => {
      return this.isTransitionAllowed(history.slice(0, index), event)
    })
  }
}
