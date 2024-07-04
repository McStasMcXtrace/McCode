**DEPRECATED**

[three-js] exposes real modules now via three/examples/jsm/...
For example to import the Orbit, do

```js
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls"
```

# three-orbitcontrols

> is the [three.js] OrbitControls from official repo examples

## Installation

To install with npm do

```bash
npm install three
npm install three-orbitcontrols
```

## Usage

All credit goes to [OrbitControls.js][original_orbitcontrols] contributors.
See also [official OrbitControls documentation][orbitcontrols_documentation].

I have just **stolen** the code and modified to export it as a module so you can do something like

```javascript
const THREE = require('three')
const OrbitControls = require('three-orbitcontrols')
// ES6 also works, i.e.
// import OrbitControls from 'three-orbitcontrols'

// Init THREE scene (add your code)

const camera = new THREE.PerspectiveCamera(75, width / height, 0.1, 1000)
camera.position.z = 5

const renderer = new THREE.WebGLRenderer({ canvas })

const controls = new OrbitControls(camera, renderer.domElement)
controls.enableDamping = true
controls.dampingFactor = 0.25
controls.enableZoom = false
```

Please note that:

1. You call `OrbitControls` directly instead of `THREE.OrbitControls`.
2. This package does not depend directly on [three.js], which is declared as a peer dependency.

See also examples:

- [CommonJS example](https://github.com/fibo/three-orbitcontrols/tree/master/example.js): clone this repo, install deps and launch `npm run example_commonjs`.
- [TypeScript example](https://github.com/fibo/three-orbitcontrols/tree/master/example.ts): clone this repo, install deps and launch `npm run example_typescript`.

## Changelog

See [OrbiControls.js history here](https://github.com/mrdoob/three.js/commits/master/examples/js/controls/OrbitControls.js).

Please also note that this repo's minor version equals [three.js] release number.

## Motivation

There is another package similar to this one: [three-orbit-controls].
I decided to create another package with a different approach, see [this issue for the rationale](https://github.com/mattdesl/three-orbit-controls/issues/17).

I am using this package for my [3d tic tac toe canvas](https://github.com/fibo/tris3d-canvas): see also online [demo](http://g14n.info/tris3d-canvas/example/).

<!--
I am using this package for my [3d tic tac toe](http://tris3d.net) online game.
-->

## License

License is the same as [three.js], i.e. [MIT].

[original_orbitcontrols]: https://github.com/mrdoob/three.js/tree/master/examples/js/controls/OrbitControls.js "OrbitControls.js"
[orbitcontrols_documentation]: https://threejs.org/docs/#examples/controls/OrbitControls "OrbitControls documentation"
[three.js]: http://threejs.org/ "three.js"
[MIT]: https://github.com/mrdoob/three.js/blob/master/LICENSE "three.js license"
[three-orbit-controls]: https://www.npmjs.com/package/three-orbit-controls "three-orbit-controls"
