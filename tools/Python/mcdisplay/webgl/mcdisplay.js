// add circle to xy plane
// 		radius
// 		center - a THREE.Vector3 instance
var addCircle = function(plane, x, y, z, radius, parent, linecolor)
{
    var wrapper = new THREE.Object3D();
    var m = new THREE.Matrix4()
    if (plane == 'xy') { m.makeRotationZ( Math.PI/2 ); }
    if (plane == 'xz') { m.makeRotationY( Math.PI/2 ); }
    if (plane == 'yz') { m.makeRotationX( Math.PI/2 ); }

    var segments = 48;
    var circleGeometry = new THREE.CircleGeometry( radius, segments );
    circleGeometry.vertices.shift(); // removes center vertex
    var material = new THREE.MeshBasicMaterial( {color: linecolor} );
    material.side = THREE.DoubleSide;

    var circle = new THREE.Line( circleGeometry, material ); // THREE.Mesh results in solid coloring
    circle.position.x = x;
    circle.position.y = y;
    circle.position.z = z;

    circle.applyMatrix(m);
    parent.add( circle );
}
// add sphere
//		radius
//		wseg 	- width segments
//		hseg 	- height segments
var addSphere = function(radius, wseg, hseg)
{
    var geometry = new THREE.SphereGeometry(radius, wseg, hseg);
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    var sphereMaterial = new THREE.MeshLambertMaterial({ color: 0xCC0000 });
    var sphere = new THREE.Mesh( geometry, sphereMaterial );
    scene.add( sphere );
}
// add pointlight
// 		center 	- THREE.Vector3 instance
var addLight = function(center, scene)
{
    // pointlight (required to light up sphereMaterial)
    var pointLight = new THREE.PointLight(0xFFFFFF);
    pointLight.position.x = center.x;
    pointLight.position.y = center.y;
    pointLight.position.z = center.z;
    scene.add(pointLight);
}

// initialize everything
//
var init = function(rootnode, campos, controltarget)
{
    scene = new THREE.Scene();

    camera = new THREE.PerspectiveCamera(60, window.innerWidth/window.innerHeight, 0.1, 100);
    camera.position.x = campos.x; // -50;
    camera.position.y = campos.y; // 0;
    camera.position.z = campos.z; // 50;

    // NOTE: initial camera view direction is along the x axis

    renderer = new THREE.WebGLRenderer();
    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);

    controls = new THREE.OrbitControls(camera);
    controls.target.x = - campos.x/2; //1;
    controls.target.y = 0; //0;
    controls.target.z = campos.z; //49;

    camera.lookAt(controls.target);

    addLight(new THREE.Vector3(10, 50, 130), scene)
    scene.add(rootnode);
}
// add multiline
// 		arrVector3 - an array of THREE.Vector3 instances
var addMultiLineV3 = function(arrVector3, parent, linecolor)
{
    var multilinematerial = new THREE.LineBasicMaterial({color: linecolor});
    var multilinegeometry = new THREE.Geometry();
    for (var i = 0; i < arrVector3.length; i++)
    {
        multilinegeometry.vertices.push(arrVector3[i]);
    }
    var multiline = new THREE.Line(multilinegeometry, multilinematerial);
    parent.add(multiline);
}
// add multiline
// 		arrVector3 - an array of THREE.Vector3 instances
var addMultiLineRay = function(arrVector3, parent, transform, linecolor)
{
    var multilinematerial = new THREE.LineBasicMaterial({color: linecolor});
    var multilinegeometry = new THREE.Geometry();
    for (var i = 0; i < arrVector3.length; i++)
    {
        multilinegeometry.vertices.push(arrVector3[i]);
    }
    var multiline = new THREE.Line(multilinegeometry, multilinematerial);
    multiline.applyMatrix(transform);
    parent.add(multiline);
}
// add multiline proxy method
//		points - array of single points
var addMultiLine = function(points, parent, linecolor)
{
    vectors = [];
    for (var i = 0; i < points.length / 3; i++)
    {
        points[i];
        //v = new THREE.Vector3(points[i*3], points[i*3+1], points[i*3+2]);
        v = new THREE.Vector3(points[i*3], points[i*3+1], points[i*3+2]);
        vectors.push(v);
    }
    addMultiLineV3(vectors, parent, linecolor);
}

// COMPONENTS

// add a component origo
//		pos - position of component ore
var addComponent = function(compname)
{
    var comp = new THREE.Object3D();
    root.add(comp);
    compnodes[compname] = comp;
    return comp;
}

//
compColors = [0xffd700, 0x00ffff, 0x00ff00, 0x800000, 0xffff00, 0xff7373, 0xffa500, 0xf08080];
iColor = -1;
var getNextComponentColor = function()
{
    iColor += 1;
    if (iColor >= compColors.length)
    {
        iColor = 0;
    }
    return compColors[iColor];
}

var scene;
var camera
var renderer;
var controls;

var compnodes = {};
var root = new THREE.Object3D();
init(root, new THREE.Vector3(-26.2633008742, 26.2833008742, 26.2833008742), new THREE.Vector3());

// generate coordinate transforms to use on the componen-specific ray segments below
var transformPoints = function(apoints, transform)
{
    var geometry = new THREE.Geometry();
    for (i = 0; i < apoints.length; i++)
    {
        geometry.vertices.push(apoints[i]);
    }
    geometry.applyMatrix(transform);
    return geometry.vertices.slice();
}

var rayColor = 0x00ffff;
var aVertices;
var aCompVertices
var rayobj;
var allRays = [];

// show all rays in arrayOfRays
var showRays = function(arrayOfRays, root)
{
	for (var i = 0; i < arrayOfRays.length; i++)
	{
		root.add(arrayOfRays[i]);
	}
}
// remove all rays in arrayOfRays from scene tree
var hideRays = function(arrayOfRays, root)
{
	for (var i = 0; i < arrayOfRays.length; i++)
	{
		root.remove(arrayOfRays[i]);
	}
}
//
iRay = -1;
var showNextRay = function(arrayOfRays, root)
{

	lastRay = iRay
	iRay += 1;
	if (iRay >= arrayOfRays.length)
	{
		iRay = 0;
	}
	root.add(arrayOfRays[iRay]);
	root.remove(arrayOfRays[lastRay]);
}

showRays(allRays, root);

// renderloop
function render() {
	requestAnimationFrame(render);
	renderer.render(scene, camera);

	showNextRay(allRays, root);
}
render();
