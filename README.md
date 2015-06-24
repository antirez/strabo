Strabo is a program that generates textured images and 3D models starting from
digital elevation maps (DEMs) in `HGT` format, as provided for free by the
[U.S. Geological Survey](http://www.usgs.gov/).

The program takes in input a set of HGT files, and generates a TIF or VRML
file as output. It has no graphical interface, and works using the command
line interface. It is easy to compile with most ANSI C compilers, and was
used with success with Linux, Windows and OSX.

The project was started in 2008 by Ester Cavalcanti (my wife) with my (Salvatore
Sanfilippo) help in the coding side, while Ester was graduating in
architecture at the University of Florence. During the years many students
at the University of Florence used this tool in order to greate 3D models
and textures of the Italian territory.

The code is released under the BSD license.

No update of the code is planned, but merging of pull requests will be performed. Forking the project in order to improve it is encouraged.

Features
---

* 2D maps and textures generations.
* UTM projection with interpolation.
* 2D contour lines maps creation.
* Heatmap style contour graph.
* Coastline detection.
* 3D VRML output.
* HGT errors and noise fixing via interpolation.
* Cropping.
* Experimental labeling of specific points.

Usage
---

The following example generates images of the Sicily region of Italy.
To start you need to download the HGT files using the `getsquare.sh` shell script:

    ./getsquare.sh N36E014

In order to cover the whole Sicily, you need the following squares:

* N36E014.hgt
* N36E015.hgt
* N37E012.hgt
* N37E013.hgt
* N37E014.hgt
* N37E015.hgt
* N38E012.hgt
* N38E013.hgt
* N38E014.hgt
* N38E015.hgt

In order to generate a 2D image with texture and UTM project use the following command line:

    ./strabo n38e012.hgt-n36e015.hgt utm fix texture prescaling 4

The program will generate a file called **output.tif** that will look like the following image:

![Strabo Example 1](http://antirez.com/misc/strabo_1.png)

The following command line will generate an image showing contour lines:

    ./strabo n38e012.hgt-n36e015.hgt utm fix curves curvestep 250 prescaling 4

![Strabo Example 2](http://antirez.com/misc/strabo_2.png)

To generate a VRML 3D mesh, just use the **vrml** option.

Full list of options
---

The program generates the full list of options when called without arguments.
Certain options and modes are obvious, others may need some more experimentation
or reading the source code. Unfortunately we can't provide better documentation
than that at the momoment, since we don't use the project anymore for years.


