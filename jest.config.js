// For a detailed explanation regarding each configuration property, visit:
// https://jestjs.io/docs/en/configuration.html

module.exports = {
  coveragePathIgnorePatterns: [
    '/node_modules/',
    'index.ts'
  ],
  preset: 'ts-jest',
  roots: [
    '<rootDir>/src/'
  ],
  testEnvironment: 'node',
  globals: {
    'ts-jest': {
      diagnostics: false
    }
  }
}
