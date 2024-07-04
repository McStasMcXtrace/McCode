# three-lut
> @daron1337's [THREE.Lut](https://github.com/mrdoob/three.js/blob/b9b1638fa194ff10f9bc0c89395898d9daa1961e/examples/js/math/Lut.js) as an npm package

## Install
`npm install --save three three-lut`

## Usage
```javascript
const THREE = require('three') // required peer dependency
require('three-lut')

const colors = 16
const mode = 'rainbow'

const lookupTable = new THREE.Lut(mode, colors)
lookupTable.getColor(1.7)
/**
 * => {
 *   "r": 1,
 *   "g": 0.3125000000000001,
 *   "b": 0
 * }
 */
```

### Versioning
This package uses an unusual versioning system to better support ThreeJS's (lack of) versioning. The major version of this repo will line up with ThreeJS breaking releases (69.0.0 => r69). Often the module will continue to work (i.e. 69.0.0 should work with r70).

The minor will be reserved for any new features, and patch for bug fixes and documentation/readme updates. In some rare cases, a minor feature may introduce a breaking change; so it's generally safest to use tilde or --save-exact for this module.

If you see any version issues, open a ticket!
