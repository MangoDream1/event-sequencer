import { StateDefinition, Path } from '../@types/module'
import { createStateMachine } from './index'

const testStates = [
  {
    id: 'start',
    isBeginning: true,
    transitions: ['beginning']
  }, {
    id: 'beginning',
    AND: ['start'],
    transitions: ['optionStart']
  }, {
    id: 'optionStart',
    isInternal: true,
    AND: ['beginning'],
    transitions: ['option1', 'option2a']
  }, {
    id: 'option1',
    AND: ['optionStart'],
    transitions: ['optionEnd']
  }, {
    id: 'option2a',
    AND: ['optionStart'],
    transitions: ['option2b']
  }, {
    id: 'option2b',
    AND: ['option2a'],
    transitions: ['optionEnd']
  }, {
    id: 'optionEnd',
    isInternal: true,
    OR: ['option1', 'option2b'],
    transitions: ['middle']
  }, {
    id: 'middle',
    AND: ['optionEnd'],
    transitions: ['parallelStart', 'middle'],
    revertTransitions: ['optionStart']
  }, {
    id: 'parallelStart',
    isInternal: true,
    AND: ['middle'],
    transitions: ['parallel1', 'parallel2', 'parallelUnneeded']
  }, {
    id: 'parallelUnneeded',
    isInternal: true,
    AND: ['middle'],
    transitions: ['parallel2']
  }, {
    id: 'parallel1',
    AND: ['parallelStart'],
    transitions: ['parallel2', 'parallelEnd']
  }, {
    id: 'parallel2',
    AND: ['parallelStart'],
    transitions: ['parallel1', 'parallelEnd']
  }, {
    id: 'parallelEnd',
    isInternal: true,
    AND: ['parallel1', 'parallel2'],
    transitions: ['end']
  }, {
    id: 'end',
    isEnd: true,
    AND: ['parallelEnd']
  }
] as StateDefinition[]

describe('state machine', () => {
  const testSM = createStateMachine(testStates)

  function transitionAlongPath (path: Path) {
    let history = []
    path.forEach((state) => {
      history = testSM.transition.exec(history, state)
    })
    return history
  }

  describe('transition', () => {
    test('success; getAllTransitions', () => {
      expect(testSM.transition.getAllTransitions()).toStrictEqual([['start', 'beginning'], ['beginning', 'option1'], ['option1', 'middle'], ['middle', 'parallel1'], ['parallel1', 'parallel2'], ['parallel2', 'parallel1'], ['parallel1', 'end'], ['parallel2', 'end'], ['middle', 'parallel2'], ['beginning', 'option2a'], ['option2a', 'option2b'], ['option2b', 'middle']])
    })

    test('success; empty', () => {
      let history = []
      const options = testSM.transition.getCurrentOptions(history)
      expect(options).toStrictEqual(['start'])

      history = testSM.transition.exec(history, 'start')
      expect(history.map(i => i.id)).toStrictEqual(['start'])
    })

    test('success; full loop', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'parallel1', 'parallel2', 'end']
      const pathWithInternals = [
        'start', 'beginning', 'optionStart', 'option1', 'optionEnd', 'middle', 'parallelStart', 'parallel1', 'parallel2', 'parallelEnd', 'end']

      const history = transitionAlongPath(path)
      expect(history.map(i => i.id)).toStrictEqual(pathWithInternals)

      expect(testSM.checkValidity(history)).toBe(true)
    })

    test('failed; no path', () => {
      const path = ['start', 'beginning', 'option1', 'parallel1']
      expect(() => transitionAlongPath(path)).toThrowError('no path')
    })

    test('failed; requisites not met', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'parallel1', 'end']
      expect(() => transitionAlongPath(path)).toThrowError('requisites not met')
    })

    test('success; or', () => {
      const pathA = ['start', 'beginning', 'option1', 'middle']
      const pathB = ['start', 'beginning', 'option2a', 'option2b', 'middle']
      expect(() => transitionAlongPath(pathA)).not.toThrowError()
      expect(() => transitionAlongPath(pathB)).not.toThrowError()
    })

    test('success; multiple options', () => {
      const path = ['start', 'beginning', 'option1', 'middle']
      const history = transitionAlongPath(path)

      const options = testSM.transition.getCurrentOptions(history)

      expect(options).toStrictEqual(['middle', 'parallel1', 'parallel2'])
    })

    test('success; repeating', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'middle', 'middle', 'middle']
      expect(() => transitionAlongPath(path)).not.toThrowError()
    })
  })
})
