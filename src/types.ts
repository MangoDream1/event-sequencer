export type Event = {
  id: EventId,
  transitions: EventId[],
  AND?: EventId[]
  isInternal?: boolean,
  isBeginning?: boolean,
}

export type EventId = string
