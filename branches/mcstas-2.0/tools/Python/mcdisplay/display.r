# built-in packages (tested with R v2.15.0)
require(utils)

# OpenGL for R (tested with rgl v0.92.880)
require(rgl)


.First <- function() {
  csvComps = commandArgs()[2]
  csvLines = commandArgs()[3]

  ## parse components
  tbl <- read.csv(csvComps)
  ## parse instrument lines
  lns <- read.csv(csvLines)

  ## prepare the OpenGL renderer
  rgl.open()
  rgl.clear()

  ## plot instrument names
  ## rgl.texts(tbl$x, tbl$y + 0.2, tbl$z, tbl$name, color="green")

  ## plot instrument lines
  rgl.lines(lns, color=lns$c)

  ## setup box around scene
  rgl.bbox(color=c('#333377', 'black'), emission='#333377',
           specular='#3333FF', shininess=5, alpha=0.8)

  ## output as EPS
  rgl.postscript('scene.eps', fmt='eps')

  ## output as WebGL (requires a recent version of rgl)
  ## writeWebGL(dir=file.path("webGL"))

}
