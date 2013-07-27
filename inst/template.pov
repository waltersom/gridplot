
#include "colors.inc"
#include "textures.inc"
#include "glass.inc" 
    
#include "euler.pov"
#include "axes.pov"
// Global settings
global_settings {assumed_gamma 2.2
	max_trace_level 100 
	ambient_light 0.3*<1.0,1.0,1.0>}

background {color White}
light_source {<2, -3, 10> color White}

//dimensions
#declare Scale = 0.01; //units will be in microns now
#declare R = 1 * Scale;
#declare AR = 2;

#declare RA = R * AR;
#declare Camera = 4;

//// Ground plane 
//plane { y, -1000*R
//finish {  F_Glass10 }	
//no_shadow
//no_reflection
//}


// Camera settings
camera { // orthographic
location <0* Scale*Camera , 0* Scale* Camera ,Scale* Camera >
right <4/3, 0, 0>
       look_at  <0.0 , 0, 0.0>}
       
#declare Sphere =	sphere {
		<0,0,0>, 1
  pigment { rgbf <0.97,0.98,1,0.8> } // A blue-tinted glass
  finish
  { phong 0.5 phong_size 400 // A highlight
    reflection 0.03  // Glass reflects a bit
  }
  interior {I_Glass}
   }

// Particle
#declare Ellipsoid =	object {
	Sphere
	scale <RA, R, R>        
	
}

#declare Cylinder =	object {
cylinder
    {
        <0, -R, 0>, <0, R, 0>, R
  pigment { rgbf <0.97,0.98,1,0.9> } // A blue-tinted glass
  finish
  { phong 0.9 phong_size 4 // A highlight
    reflection 0.03  // Glass reflects a bit
  }
//  interior {I_Glass}
    }
}
