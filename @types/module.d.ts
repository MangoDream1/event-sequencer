
export type InternalState = AndGuardState | OrGuardState | GuardLessState
export type StateDefinition = AndGuardState | OrGuardState | GuardLessState

export type StateMapping = {[id: string]: InternalState}

interface GuardLessState extends BaseState {
  AND?: never
  OR?: never
}

interface AndGuardState extends BaseState {
  AND: StateId[]
  OR?: never
}

interface OrGuardState extends BaseState {
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
}

export type StateId = string
export type Path = StateId[]
export type History = StateId[]
