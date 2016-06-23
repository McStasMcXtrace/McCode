// transforms vertices by and returns the transformed
//      apoints  -  an array of vertices
//      transform  -  matrix4
var transformPoints = function(apoints, transform)
{
    var geometry = new THREE.Geometry();
    geometry.vertices = apoints;
    geometry.applyMatrix(transform);
    return geometry.vertices.slice();
}
// "main" class which is a collection of scene graph setup routives and data objects
//
var Main = function ()
{
    this.scene;
    this.camera;
    this.renderer;
    this.controls;

    this.compnodes = {};
    this.rootnode = new THREE.Object3D();

    this.compColors = [0xffd700, 0x00ffff, 0x00ff00, 0x800000, 0xffff00, 0xff7373, 0xffa500, 0xf08080];
    this.iColor = -1;

    this.rayColor = 0x00ffff;
    this.aCompVertices
    this.rayobj;
    this.raynodes = [];

    this.iRay = -1;
}
// add circle to xy, xz or yz plane
//
Main.prototype.addCircle = function(plane, x, y, z, radius, parent, linecolor)
{
    console.log(plane, x, y, z, radius);
    if (radius == 0)
    {
        return;
    }

    var wrapper = new THREE.Object3D();
    var m = new THREE.Matrix4()
    if (plane == 'xy') { } // identity
    if (plane == 'xz') { m.makeRotationX( Math.PI/2 ); }
    if (plane == 'yz') { m.makeRotationY( Math.PI/2 ); }

    var segments = 48;
    var circleGeometry = new THREE.CircleGeometry( radius, segments );
    circleGeometry.vertices.shift(); // removes center vertex
    var material = new THREE.MeshBasicMaterial( {color: linecolor} );
    material.side = THREE.DoubleSide;

    var circle = new THREE.Line( circleGeometry, material ); // THREE.Mesh results in solid coloring

    circle.applyMatrix(m);

    circle.position.x = x;
    circle.position.y = y;
    circle.position.z = z;

    parent.add( circle );
}
// add sphere
//		radius
//		wseg 	- width segments
//		hseg 	- height segments
Main.prototype.addSphere = function(radius, wseg, hseg)
{
    var geometry = new THREE.SphereGeometry(radius, wseg, hseg);
    var material = new THREE.MeshBasicMaterial( {color: 0xffff00} );
    var sphereMaterial = new THREE.MeshLambertMaterial({ color: 0xCC0000 });
    var sphere = new THREE.Mesh( geometry, sphereMaterial );
    this.scene.add( sphere );
}
// add pointlight
// 		center 	- THREE.Vector3 instance
Main.prototype.addLight = function(center)
{
    // pointlight (required to light up sphereMaterial)
    var pointLight = new THREE.PointLight(0xFFFFFF);
    pointLight.position.x = center.x;
    pointLight.position.y = center.y;
    pointLight.position.z = center.z;
    this.scene.add(pointLight);
}
// initialize the scene
//
Main.prototype.init = function(campos)
{
    this.scene = new THREE.Scene();

    this.camera = new THREE.PerspectiveCamera(60, window.innerWidth/window.innerHeight, 0.1, 100);
    this.camera.position.x = campos.x; // -50;
    this.camera.position.y = campos.y; // 0;
    this.camera.position.z = campos.z; // 50;

    // NOTE: initial camera view direction is along the x axis

    this.renderer = new THREE.WebGLRenderer();
    this.renderer.setSize(window.innerWidth, window.innerHeight);

    element = document.getElementById("3dcanvas");
    console.log(element);
    document.getElementById("3dcanvas").appendChild(this.renderer.domElement);

    this.controls = new THREE.OrbitControls(this.camera);
    this.controls.target.x = - campos.x/2; //1;
    this.controls.target.y = 0; //0;
    this.controls.target.z = campos.z; //49;

    this.camera.lookAt(this.controls.target);

    this.addLight(new THREE.Vector3(10, 50, 130))
    this.scene.add(this.rootnode);
}
//  set a bounding box around the components
//
Main.prototype.setBoundingBox = function()
{
    var box = new THREE.BoxHelper( this.rootnode, 0x808080 );
    this.scene.add( box );
}
// add multiline
// 		arrVector3  -  an array of THREE.Vector3 instances
Main.prototype.addMultiLineV3 = function(arrVector3, parent, linecolor)
{
    var multilinematerial = new THREE.LineBasicMaterial({color: linecolor});
    var multilinegeometry = new THREE.Geometry();
    multilinegeometry.vertices = arrVector3;
    var multiline = new THREE.Line(multilinegeometry, multilinematerial);
    parent.add(multiline);
}
// add multiline proxy method
//		points  -  array of single points
Main.prototype.addMultiLine = function(points, parent, linecolor)
{
    vectors = [];
    for (var i = 0; i < points.length / 3; i++)
    {
        points[i];
        v = new THREE.Vector3(points[i*3], points[i*3+1], points[i*3+2]);
        vectors.push(v);
    }
    this.addMultiLineV3(vectors, parent, linecolor);
}
// add a ray node
//
Main.prototype.addRayNode = function(rayObj, vertices)
{
    var multilinematerial = new THREE.LineBasicMaterial({color: 0xffffff});
    var multilinegeometry = new THREE.Geometry();
    multilinegeometry.vertices = vertices;
    var multiline = new THREE.Line(multilinegeometry, multilinematerial);
    rayObj.add(multiline);
    this.raynodes.push(rayObj);
}
// add a component origo
//		compname  -  component name for the reference dict
Main.prototype.addComponent = function(compname)
{
    var comp = new THREE.Object3D();
    this.rootnode.add(comp);
    this.compnodes[compname] = comp;
    return comp;
}
// returns a cyclically new color intended for a component draw node
//
Main.prototype.getNextComponentColor = function()
{
    this.iColor += 1;
    if (this.iColor >= this.compColors.length)
    {
        this.iColor = 0;
    }
    return this.compColors[this.iColor];
}
// show all rays in this.raynodes
//
Main.prototype.showAllRays = function()
{
	for (var i = 0; i < this.raynodes.length; i++)
	{
		this.rootnode.add(this.raynodes[i]);
	}
}
// remove all rays in this.raynodes from the scene graph
//
Main.prototype.hideraynodes = function()
{
	for (var i = 0; i < this.raynodes.length; i++)
	{
		this.rootnode.remove(this.raynodes[i]);
	}
}
// iterates to attach the next ray in the global ray sequence
//
Main.prototype.showNextRay = function()
{
    if (this.raynodes.length == 0) { return; }

	lastRay = this.iRay;
	this.iRay += 1;
	if (this.iRay >= this.raynodes.length)
	{
		this.iRay = 0;
	}
	this.rootnode.add(this.raynodes[this.iRay]);
	this.rootnode.remove(this.raynodes[lastRay]);
}
// iterates to attach the next ray in the global ray sequence
//
Main.prototype.hideRay = function(idx)
{
    if (this.raynodes.length == 0) { return idx; }

	if (idx >= this.raynodes.length | idx < 0)
	{
        throw "idx out of range"
	}
	this.rootnode.remove(this.raynodes[idx]);
}
// iterates to attach the next ray in the global ray sequence
//
Main.prototype.showRay = function(idx)
{
    if (this.raynodes.length == 0) { return idx; }

	if (idx >= this.raynodes.length | idx < 0)
	{
        throw "idx out of range"
	}
	this.rootnode.add(this.raynodes[idx]);
}

// loads json data into the scene graph
//      instrdata    -  json instrument definition data
//      neutrondata  -  json neutron data
//      main         -  reference to scene wrapper
var TraceLoader = function(instrdata, neutrondata, main)
{
    this.main = main;
    this.instrdata = instrdata;
    this.neutrondata = neutrondata;
}
// synchronously load instrument component draw data
//
TraceLoader.prototype.loadInstr = function()
{
    var main = this.main;

    // INSTRUMENT
    var instr = this.instrdata;
    var instname = instr['name'];
    var abspath = instr['abspath'];

    // COMPONENTS
    var comps = instr['components'];
    var comp;
    var comp_node;
    var compname;
    var m4;
    var acolor;
    for (var i = 0; i < comps.length; i++) {
        comp = comps[i];

        compname = comp['name'];
        m4 = comp['m4'];
        comp_node = main.addComponent(compname);
        acolor = main.getNextComponentColor();

        drawcalls = comp['drawcalls']
        var call;
        for (var j = 0; j < drawcalls.length; j++) {
            call = drawcalls[j];

            key = call['key'];
            args = call['args'];

            if (key == 'multiline') {
                main.addMultiLine(args, comp_node, acolor);
            }
            if (key == 'circle') {
                main.addCircle(args[0], args[1], args[2], args[3], args[4], comp_node, acolor);
            }
        }

        comp_matrix = new THREE.Matrix4();
        comp_matrix.set(m4[0], m4[1], m4[2], m4[3], m4[4], m4[5], m4[6], m4[7], m4[8], m4[9], m4[10], m4[11], m4[12], m4[13], m4[14], m4[15]);
        comp_node.applyMatrix(comp_matrix);
    }
}
// load neutrons
//
TraceLoader.prototype.loadNeutrons = function()
{
    var main = this.main;

    // RAYS
    var rays = this.neutrondata['rays'];
    var ray;
    var aVertices;
    for (var i = 0; i < rays.length; i++) {
        ray = rays[i];

        rayobj = new THREE.Object3D();
        aVertices = [];
        var aCompVertices;
        var compname;

        var groups =  ray['groups'];
        var group;
        for (var j = 0; j < groups.length; j++) {
            group = groups[j];
            compname = group['compname'];
            aCompVertices = [];

            // NEUTRON STATES
            var events = group['events'];
            var localstate;
            var args;
            for (var k = 0; k < events.length; k++) {
                localstate = events[k];

                args = localstate['args'];
                aCompVertices.push(new THREE.Vector3(args[0], args[1], args[2]));
            }

            // transform these vertices by component matrix and add to vertex container for this ray
            aVertices = aVertices.concat(transformPoints(aCompVertices, main.compnodes[compname].matrix));
        }
        // add ray as a multiline
        main.addRayNode(rayobj, aVertices);
        //main.addMultiLineV3(aVertices, rayobj, main.rayColor);
    }
}
//  program controller
//      campos_x/y/z  -  determines initial camera position, this is used with --inspect
var Controller = function(campos_x, campos_y, campos_z)
{
    this.campos = new THREE.Vector3(campos_x, campos_y, campos_z);
    this.main = new Main();
    this.loader = new TraceLoader(MCDATA_instrdata, MCDATA_neutrondata, this.main);
    this.viewmodel = new ViewModel(numRays = MCDATA_neutrondata["numrays"]);
}
Controller.prototype.setUpdateGuiFunc = function(updateGuiFunc)
{
    this.updateGuiFunc = updateGuiFunc;
}
//  main program execution loops are set up here
//
Controller.prototype.run = function()
{
    // init mcdisplay
    this.main.init(this.campos);

    // execution loops
    var _this = this;
    var renderLoop = function()
    {
    	requestAnimationFrame(renderLoop);
    	_this.main.renderer.render(_this.main.scene, _this.main.camera);
    }
    var dataLoop = function()
    {
        setTimeout(dataLoop, 1000/_this.viewmodel.raysPrSec);
        if (_this.viewmodel.playBack == PlayBack.ALL)
        {
            _this.showAllRays();
            return;
        }

        if (_this.viewmodel.playBack == PlayBack.RUN)
        {
            _this.incSingleRay();
        }
        if (_this.viewmodel.playBack == PlayBack.PAUSE)
        {
            // TODO: use viewmodel.updateVersion to skip this step if needed
            _this.showCurrentRay();
        }
        if (viewmodel.getDisplayMode() == DisplayMode.SINGLE)
        {
            _this.hidePrevRays();
        }
    }
    var updateGuiLoop = function()
    {
        setTimeout(updateGuiLoop, 100);
        _this.updateGuiFunc();
    }

    // initiate timed execution loops
    renderLoop();
    dataLoop();
    updateGuiLoop();

    // load data - possibly heavy
    this.loader.loadInstr();
    this.loader.loadNeutrons();
    this.main.setBoundingBox();
}
Controller.prototype.showraynodes = function()
{
    this.main.showraynodes();
}
Controller.prototype.hidePrevRays = function()
{
    var arrLast = this.viewmodel.shiftNonFirstRayIdxs();
    for (var i = 0; i < arrLast.length; i++)
    {
        this.main.hideRay(arrLast[i]);
    }
}
Controller.prototype.incSingleRay = function()
{
    this.viewmodel.setRayIdx(this.viewmodel.getRayIdx() + 1)
    var retidx = this.main.showRay(this.viewmodel.getRayIdx());
}
Controller.prototype.showCurrentRay = function()
{
    this.main.showRay(this.viewmodel.getRayIdx());
}

//  enum for playback state
//
PlayBack = { RUN : 0, PAUSE : 1, ALL : 3 };

//
//
DisplayMode = { SINGLE : 0, KEEP : 1 }

//  viewmodel for keeping control data free of the gui
//
var ViewModel = function(numRays)
{
    this.playBack = PlayBack.RUN;
    this.displayMode = DisplayMode.SINGLE;

    this.numRays = numRays;
    this.rayIdx = [-1];
    this.raysPrSec = 5;

    this.updateVersion = 0; // incremented on update
}
ViewModel.prototype.getUpdateVersion = function()
{
    return this.updateVersion;
}
ViewModel.prototype.setRayIdx = function(idx)
{
    // the socalled "js modulus bug" for negative numbers, using ((n % m) + m) % m;
    idx = ((idx % this.numRays) + this.numRays) % this.numRays;
    this.rayIdx.push(idx);
    this.updateVersion += 1;
}
ViewModel.prototype.getRayIdx = function(idx)
{
    return this.rayIdx[this.rayIdx.length-1];
}
ViewModel.prototype.shiftNonFirstRayIdxs = function()
{
    if (this.rayIdx.length <= 1)
    {
        return [];
    }
    var arr = [];
    for (var i=0; i < this.rayIdx.length -1; i++)
    {
        arr.push(this.rayIdx.shift());
    }
    this.updateVersion += 1;
    return arr;
}
ViewModel.prototype.setPlayBack = function(playBack)
{
    this.playBack = playBack;
    this.updateVersion += 1;
}
ViewModel.prototype.getPlayBack = function()
{
    return this.playBack;
}
ViewModel.prototype.setDisplayMode = function(displayMode)
{
    this.displayMode = displayMode;
    this.updateVersion += 1;
}
ViewModel.prototype.getDisplayMode = function()
{
    return this.displayMode;
}
