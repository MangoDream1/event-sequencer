export type EventMapping = {[id: string]: EventDefinition}

export interface EventDefinition {
  id: EventId,
  transitions: EventId[],
  AND?: EventId[]
  isInternal?: boolean,
  isBeginning?: boolean,
  isRepeatable?: boolean
}

export type EventId = string
export type EventSequence = EventId[]
