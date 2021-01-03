import { EventDefinition, EventSequence } from '../@types/module'
import { createEventSequencer } from './eventSequencer'

const testStates = [
  {
    id: 'start',
    isBeginning: true,
    transitions: ['beginning']
  }, {
    id: 'beginning',
    transitions: ['optionStart']
  }, {
    id: 'optionStart',
    isInternal: true,
    transitions: ['option1', 'option2a']
  }, {
    id: 'option1',
    transitions: ['optionEnd']
  }, {
    id: 'option2a',
    transitions: ['option2b']
  }, {
    id: 'option2b',
    transitions: ['optionEnd']
  }, {
    id: 'optionEnd',
    isInternal: true,
    transitions: ['middle']
  }, {
    id: 'middle',
    transitions: ['parallelStart', 'middle', 'beginning']
  }, {
    id: 'parallelStart',
    isInternal: true,
    transitions: ['parallel1', 'parallel2', 'parallelUnneeded']
  }, {
    id: 'parallelUnneeded',
    isInternal: true,
    transitions: ['parallel2']
  }, {
    id: 'parallel1',
    transitions: ['parallel2', 'parallelEnd']
  }, {
    id: 'parallel2',
    transitions: ['parallel1', 'parallelEnd']
  }, {
    id: 'parallelEnd',
    isInternal: true,
    AND: ['parallel1', 'parallel2'],
    transitions: ['end', 'beginning']
  }, {
    id: 'end',
    transitions: ['end']
  }
] as EventDefinition[]

describe('state machine', () => {
  const testSM = createEventSequencer(testStates)

  function transitionAlongPath (path: EventSequence): EventSequence {
    let history = []
    path.forEach((state) => {
      history = testSM.transition(history, state)
    })
    return history
  }

  describe('transition', () => {
    test('success; getAllTransitions', () => {
      expect(testSM.getAllTransitions()).toStrictEqual([
        ['start', 'beginning'], ['beginning', 'option1'], ['option1', 'middle'], ['middle', 'middle'], ['middle', 'beginning'],
        ['beginning', 'option2a'], ['option2a', 'option2b'], ['option2b', 'middle'], ['middle', 'parallel1'], ['parallel1', 'parallel2'],
        ['parallel2', 'parallel1'], ['parallel1', 'end'], ['end', 'end'], ['parallel1', 'beginning'], ['parallel2', 'end'],
        ['parallel2', 'beginning'], ['middle', 'parallel2']
      ])
    })

    test('success; empty', () => {
      const emptyHistory = []
      const options = testSM.getTransitions(emptyHistory)
      expect(options).toStrictEqual(['start'])

      const history = testSM.transition(emptyHistory, 'start')
      expect(history).toStrictEqual(['start'])
    })

    test('success; full loop', () => {
      const path = [
        'start', 'beginning', 'option1', 'middle', 'middle', 'beginning', 'option2a', 'option2b',
        'middle', 'parallel1', 'parallel2', 'beginning', 'option1', 'middle', 'parallel1', 'parallel2', 'end'
      ]

      const newHistory = [
        'start', 'beginning', 'option1', 'middle', 'middle',
        'beginning', 'option2a', 'option2b', 'middle',
        'parallel1', 'parallel2', 'beginning', 'option1',
        'middle', 'parallel1', 'parallel2', 'end'
      ]

      const history = transitionAlongPath(path)
      expect(history).toStrictEqual(newHistory)
    })

    test('success; valid chain', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'parallel1', 'parallel2', 'end']
      const pathWithInternals = [
        'start', 'beginning', 'option1', 'middle', 'parallel1', 'parallel2', 'end']

      const history = transitionAlongPath(path)
      expect(history).toStrictEqual(pathWithInternals)

      expect(testSM.checkValidity(history)).toBe(true)
    })

    test('failed; no path', () => {
      const path = ['start', 'beginning', 'option1', 'parallel1']
      expect(() => transitionAlongPath(path)).toThrowError('no path')
    })

    test('failed; requisites not met', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'parallel1', 'end']

      expect(() => transitionAlongPath(path)).toThrowError('Not allowed; dest parallelEnd requisites not met')
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

      const options = testSM.getTransitions(history)

      expect(options).toStrictEqual(['middle', 'beginning', 'parallel1', 'parallel2'])
    })

    test('success; repeating', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'middle', 'middle', 'middle']
      expect(() => transitionAlongPath(path)).not.toThrowError()
    })

    test('failure; repeating', () => {
      const path = ['start', 'beginning', 'option1', 'option1', 'option1', 'option1', 'option1']
      expect(() => transitionAlongPath(path)).toThrowError()
    })

    test('success; repeating with AND requirement', () => {
      const path = ['start', 'beginning', 'option1', 'middle', 'parallel1', 'parallel2', 'end', 'end', 'end']
      expect(() => transitionAlongPath(path)).not.toThrowError()
    })
  })
})
