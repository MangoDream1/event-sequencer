
export type StateMapping = {[id: string]: StateDefinition}

export interface StateDefinition {
  id: StateId,
  transitions: StateId[],
  AND?: StateId[]
  isInternal?: boolean,
  isBeginning?: boolean,
}

export type StateId = string
export type Path = StateId[]
export type HistoryArray = StateId[]
