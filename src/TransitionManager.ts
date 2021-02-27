import { EventSequencer } from './EventSequencer'
import { EventId } from './types'

type TransitionId = string
interface Transition {
  id: TransitionId,
  from: EventId | null,
  to: EventId,
  alias?: string,
}

export class TransitionManager {
  sequencer: EventSequencer
  #destToIncomingTransitions: Record<string, Transition[]> = {}
  #idToTransition: Record<TransitionId, Transition> = {}

  constructor (sequencer: EventSequencer) {
    this.sequencer = sequencer

    this.sequencer.getAllTransitions().forEach(([from, to]) => {
      const transition = { id: this._createTransitionId(from, to), from, to }
      this.#idToTransition[transition.id] = transition

      if (!this.#destToIncomingTransitions[to]) this.#destToIncomingTransitions[to] = []
      this.#destToIncomingTransitions[to].push(transition)
    })
  }

  private _createTransitionId (from: EventId | null, to: EventId) {
    if (!from) return to
    return `${from}-${to}`
  }

  getAllowedTransitions (history: EventId[]) {
    const allowedDestinations = this.sequencer.getAllowedDestinations(history)
    const current = history[history.length - 1]

    return allowedDestinations.flatMap(dest => this.#destToIncomingTransitions[dest]
      .filter(t => !current || t.from === current)
    ).map(t => t.id)
  }

  getAllTransitions () {
    return Object.values(this.#idToTransition).map(t => t.id)
  }

  executeTransition (history: EventId[], transitionId: TransitionId, removeInvalid = false) {
    const transition = this.#idToTransition[transitionId]
    return this.sequencer.executeTransition(history, transition.to, removeInvalid)
  }

  isTransitionAllowed (history: EventId[], transitionId: TransitionId) {
    const transition = this.#idToTransition[transitionId]
    return this.sequencer.isTransitionAllowed(history, transition.to)
  }
}
