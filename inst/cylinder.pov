
#include "template.pov" 

//// Ground plane 
//plane { y, -1000*R
//finish {  F_Glass10 }	
//no_shadow
//no_reflection
//}


object{ Cylinder translate<0,0,0> EulerRotate(<90, 30, 0>)}
       
//object{ AxisXYZ( 2*R, 2*R, 2*R, Texture_A_Dark, Texture_A_Light, R*0.01)
// EulerRotate(<90, 30, 0>)
// } 
