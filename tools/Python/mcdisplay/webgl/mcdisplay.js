// "main" class containing just about everything at this point
//
var Main = function () {
    this.scene;
    this.camera;
    this.renderer;
    this.controls;

    this.compnodes = {};
    this.rootnode = new THREE.Object3D();

    this.compColors = [0xffd700, 0x00ffff, 0x00ff00, 0x800000, 0xffff00, 0xff7373, 0xffa500, 0xf08080];
    this.iColor = -1;

    this.rayColor = 0x00ffff;
    this.aVertices;
    this.aCompVertices
    this.rayobj;
    this.allRays = [];

    this.iRay = -1;
}

    // add circle to xy plane
    // 		radius
    // 		center - a THREE.Vector3 instance
    Main.prototype.addCircle = function(plane, x, y, z, radius, parent, linecolor)
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

    // initialize everything
    //
    Main.prototype.init = function(campos, controltarget)
    {
        this.scene = new THREE.Scene();

        this.camera = new THREE.PerspectiveCamera(60, window.innerWidth/window.innerHeight, 0.1, 100);
        this.camera.position.x = campos.x; // -50;
        this.camera.position.y = campos.y; // 0;
        this.camera.position.z = campos.z; // 50;

        // NOTE: initial camera view direction is along the x axis

        this.renderer = new THREE.WebGLRenderer();
        this.renderer.setSize(window.innerWidth, window.innerHeight);

        document.body.appendChild(this.renderer.domElement);

        this.controls = new THREE.OrbitControls(this.camera);
        this.controls.target.x = - campos.x/2; //1;
        this.controls.target.y = 0; //0;
        this.controls.target.z = campos.z; //49;

        this.camera.lookAt(this.controls.target);

        this.addLight(new THREE.Vector3(10, 50, 130))
        this.scene.add(this.rootnode);
    }
    // add multiline
    // 		arrVector3 - an array of THREE.Vector3 instances
    Main.prototype.addMultiLineV3 = function(arrVector3, parent, linecolor)
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
    // add multiline proxy method
    //		points - array of single points
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

    // COMPONENTS

    // add a component origo
    //		pos - position of component ore
    Main.prototype.addComponent = function(compname)
    {
        var comp = new THREE.Object3D();
        this.rootnode.add(comp);
        this.compnodes[compname] = comp;
        return comp;
    }
    Main.prototype.getNextComponentColor = function()
    {
        this.iColor += 1;
        if (this.iColor >= this.compColors.length)
        {
            this.iColor = 0;
        }
        return this.compColors[this.iColor];
    }


    // generate coordinate transforms to use on the componen-specific ray segments below
    Main.prototype.transformPoints = function(apoints, transform)
    {
        var geometry = new THREE.Geometry();
        for (i = 0; i < apoints.length; i++)
        {
            geometry.vertices.push(apoints[i]);
        }
        geometry.applyMatrix(transform);
        return geometry.vertices.slice();
    }
    // show all rays in arrayOfRays
    Main.prototype.showAllRays = function()
    {
    	for (var i = 0; i < this.allRays.length; i++)
    	{
    		this.rootnode.add(this.allRays[i]);
    	}
    }
    // remove all rays in arrayOfRays from scene tree
    Main.prototype.hideAllRays = function()
    {
    	for (var i = 0; i < this.allRays.length; i++)
    	{
    		this.rootnode.remove(this.allRays[i]);
    	}
    }
    // iterates rays
    Main.prototype.showNextRay = function()
    {
    	lastRay = this.iRay;
    	this.iRay += 1;
    	if (this.iRay >= this.allRays.length)
    	{
    		this.iRay = 0;
    	}
    	this.rootnode.add(this.allRays[this.iRay]);
    	this.rootnode.remove(this.allRays[lastRay]);
    }
