# Simple Polygon Viewer

##### About

The tag line of Lazarus is "write once, compile anywhere", allowing developers to rapidly develop software for Linux, MacOS and Windows. However, developing OpenGL applications represents a challenge, as MacOS drops deprecated features from OpenGL (the *core* profile). Therefore, to support all three major desktop platforms one must either target OpenGL 2.1 (the last legacy version supported by MacOS) or OpenGL 3.3 *core*. This is unlike Linux and Windows, where the drivers support the legacy OpenGL features (so you can mix and match modern functions with deprecated functions).

There are nice [tutorials for legacy OpenGL](http://wiki.freepascal.org/OpenGL_Tutorial). However, those tutorials describe many tutorial features that do not exist in the core specification (e.g. GL_QUADS, glVertex3f, GL_MODELVIEW). The three tutorials below illustrate how to write modern OpenGL applications. A nice feature of Core OpenGL is that it is very similar to the OpenGL ES versions used by many phones and tablets.

##### Compiling

The three sample applications should compile easily: launch Lazarus and choose Project/OpenProject, then select Run/Run to compile and execute the project. However, there is one wrinkle for MacOS users: Lazarus uses the Carbon widgetset by default, but you must use the Cocoa widgetset to exploit OpenGL core. To change the widgetset, select Project/ProjectOptions, select the "Additions and Overrides" tab and click the LCLWidgetType pull-down to select "Cocoa". Once you do this, the applications should compile easily.

##### Project 1: Basic

The project basic.lpr shows a 2D square. You can drag the mouse to spin the square. Since Core OpenGL does not support GL_QUADS, we draw the square using a [GL_TRIANGLE_STRIP](http://stackoverflow.com/questions/16882474/is-there-a-clear-performance-difference-between-gl-quads-and-gl-triangle-strip).

![alt tag](https://raw.githubusercontent.com/neurolabusc/OpenGLCore/master/basic.jpg)

##### Project 2: Cube

The project cube.lpr shows a colored 3D cube. You can drag the mouse to spin the cube. Since Core OpenGL does not support GL_MODELVIEW and GL_PROJECTIONVIEW, this project uses equivalent functions (nGL_MODELVIEW and nGL_PROJECTIONVIEW) from the unit gl_core_utils.

![alt tag](https://raw.githubusercontent.com/neurolabusc/OpenGLCore/master/cube.jpg)

##### Project 3: Render

The project render.lpr create volume renderings - by default it generates a 'borg' cube, but you can load any NIfTI format image, for example the brain image included with this project. This project is a simple extension of the cube project: note that the colors of the cube in the previous project map their XYZ position as red, green and blue. We can use these 3 dimensions to map the three dimensions of our 3D textures.

Volume renderers are often [two pass, but can also be computed in a single pass](http://prideout.net/blog/?p=64). This project can be compiled for either mode, depending on whether the compiler directive '{$DEFINE TWO_PASS}' is enabled or not. There is no  meaningful performance difference between these two modes (the single pass eliminates a 2D texture lookup per ray, but for complex volumes we are computing thousands of 3D texture lookups). However, the single pass method is simpler to implement as you do not need to manage a framebuffer for the cube's back face.

![alt tag](https://raw.githubusercontent.com/neurolabusc/OpenGLCore/master/render.jpg)

##### Recent Versions

 - 1/2017 Initial release

##### License

 This software uses the [BSD 2-Clause license](https://opensource.org/licenses/BSD-2-Clause)

##### Links

 - [plyview is a simple Core OpenGL application that can display OBJ and PLY format meshes](https://github.com/neurolabusc/plyview)

 - [Surf Ice is an advanced mesh viewing application that can be compiled for either legacy or core OpenGL](https://github.com/neurolabusc/surf-ice)

# OpenGLCoreTutorials
