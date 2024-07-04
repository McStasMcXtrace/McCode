const assert = require('assert')

const { version, peerDependencies } = require('./package.json')

const orbitControlsMinorVersion = version.split('.')[1]
const threeMinorVersion = peerDependencies.three.split('.')[1]

assert.equal(orbitControlsMinorVersion, threeMinorVersion, 'minor version is the same as threejs release')
