/* (c) Dave Gilbert dave@treblig.org 2010
   except for inner core of maths taken from example at:
      http://www.skytopia.com/project/fractal/mandelbulb.html

  This spits out a voxel array on stdout, 1 byte per voxel (255 if > iteration limit
  else 0).
 */
#include <stdio.h>

#include <math.h>

const unsigned int size=400;
#define RANGE 1.2
const double xlow=-RANGE;
const double xhigh=RANGE;
const double ylow=-RANGE;
const double yhigh=RANGE;
const double zlow=-RANGE;
const double zhigh=RANGE;

const unsigned int maxit=6;
const double mandpow=6.0;

double valInRange(double low, double high, unsigned int size, unsigned int off)
{
  return low+((high-low)/(double)size)*(double)off;
}

unsigned int doPoint(double cx, double cy, double cz)
{
  double x,y,z;
  double newx,newy,newz;
  double r,theta,phi;
  unsigned int i;

  for(i=0,x=0.0,y=0.0,z=0.0;(i<maxit) && (x*x+y*y+z*z) < 2.0;i++)
  {
    double rpow;

    r = sqrt(x*x + y*y + z*z );
    theta = atan2(sqrt(x*x + y*y) , z);
    phi = atan2(y,x);

    /* These maths from http://www.skytopia.com/project/fractal/mandelbulb.html */
    rpow = pow(r,mandpow);
    newx = rpow * sin(theta*mandpow) * cos(phi*mandpow);
    newy = rpow * sin(theta*mandpow) * sin(phi*mandpow);
    newz = rpow * cos(theta*mandpow);

    x=newx+cx;
    y=newy+cy;
    z=newz+cz;
  }

  return i;
}

int main()
{
  unsigned int x,y,z;

  for(z=0;z<size;z++) {
    double fz=valInRange(zlow, zhigh, size, z);
    fprintf(stderr,"fz=%lf\n", fz);
    for(y=0;y<size;y++) {
      double fy=valInRange(ylow, yhigh, size, y);
      for(x=0;x<size;x++) {
        double fx=valInRange(xlow, xhigh, size, x);
        unsigned int val=doPoint(fx,fy,fz);
        putchar((val>=(maxit-1))?255:0);
      }
    }
  }
}

