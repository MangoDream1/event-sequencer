export type Event = {
  id: EventId,
  transitions: EventId[],
  AND?: EventId[]
  isInternal?: boolean,
  isBeginning?: boolean,
}

export interface Transition {
  from: EventId,
  to: EventId,
  id: TransitionId,
  path: EventId[],
}

export type EventId = string
export type TransitionId = string
