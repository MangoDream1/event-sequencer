
export interface State {
  id: StateId
  isReverted?: boolean
}

export type InternalState = AndState | OrState
export type StateDefinition = AndState | OrState

export type StateMapping = {[id: string]: InternalState}

interface AndState extends BaseState {
  AND: StateId[]
  OR?: never
}

interface OrState extends BaseState {
  OR: StateId[]
  AND?: never
}

interface BaseState {
  id: StateId,
  transitions: StateId[],
  revertTransitions: StateId[],
  isInternal?: boolean,
  isBeginning?: boolean,
  isEnd?: boolean,
  isReverted?: boolean
}

export type StateId = string
export type Path = StateId[]
