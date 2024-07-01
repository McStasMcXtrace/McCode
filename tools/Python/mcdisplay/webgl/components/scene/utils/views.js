export const views = [
    // Main view Perspective camera
    {
      left: 0.0,
      bottom: 0.5,
      width: 1.0,
      height: 0.5,
      camera: "PerspectiveCamera",
      view: "primary",
      initialCamPos: [1, 2, 3],
      fov: 60,
      updateCamera: function ( camera, scene, mouseX ) {

        camera.position.x += mouseX * 0.05;
        camera.position.x = Math.max( Math.min( camera.position.x, 2000 ), - 2000 );
        camera.lookAt( scene.position );

      },
      updateControls: function(controls){
        controls.update();
      }
    },
    // PyQtGraph replacements using Ortographic cameras
    // Top view - XZ
    {
      left: 0.0,
      bottom: 0.0,
      width: 0.5,
      height: 0.5,
      camera: "OrthographicCamera",
      view: "top2D",
      initialCamPos: [0, 1, 1],
      fov: 60,
      updateCamera: function ( camera, scene, mouseX ) {

        camera.position.x += mouseX * 0.05;
        camera.position.x = Math.max( Math.min( camera.position.x, 2000 ), - 2000 );
        camera.lookAt( scene.position );

      },
      updateControls: function(controls){
        controls.update();
      }
    },
    // Back view - XY
    {
      left: 0.5,
      bottom: 0.25,
      width: 0.5,
      height: 0.25,
      camera: "OrthographicCamera",
      view: "back2D",
      initialCamPos: [1, 0, 1],
      fov: 60,
      updateCamera: function ( camera, scene, mouseX ) {

        camera.position.x = Math.max( Math.min( camera.position.x, 2000 ), - 2000 );
        camera.lookAt( scene.position );

      },
      updateControls: function(controls){
        controls.update();
      }
    },
    //Side view - YZ
    {
      left: 0.5,
      bottom: 0.0,
      width: 0.5,
      height: 0.25,
      camera: "OrthographicCamera",
      view: "side2D",
      initialCamPos: [0, 0, 1],
      fov: 60,
      updateCamera: function ( camera, scene, mouseX ) {

        camera.position.x += mouseX * 0.05;
        camera.position.x = Math.max( Math.min( camera.position.x, 2000 ), - 2000 );
        camera.lookAt( scene.position );

      },
      updateControls: function(controls){
        controls.update();
      }
    },
  ];