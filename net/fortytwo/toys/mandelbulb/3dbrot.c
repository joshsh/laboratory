/* (c) Dave Gilbert dave@treblig.org 2010
   except for inner core of maths taken from example at:
      http://www.skytopia.com/project/fractal/mandelbulb.html

  This spits out a voxel array on stdout, 1 byte per voxel (255 if > iteration limit
  else 0).
	  
  "Hollowing" code (c) 2011 by Joshua Shinavier
 */
#include <stdio.h>
#include <stdlib.h>

#include <math.h>

#define SIZE 444
#define RANGE 1.2
const double xlow=-RANGE;
const double xhigh=RANGE;
const double ylow=-RANGE;
const double yhigh=RANGE;
const double zlow=-RANGE;
const double zhigh=RANGE;

const unsigned int maxit=6;
const double mandpow=6.0;

unsigned char voxels[SIZE][SIZE][SIZE];

#define HOLLOW

#ifdef HOLLOW
#define THICKNESS 0.075
unsigned char buffer[SIZE][SIZE][SIZE];
#endif

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

  for(i=0,x=0.0,y=0.0,z=0.0;(i<maxit) && (x*x+y*y+z*z) < RANGE;i++)
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

unsigned int min(unsigned int a, unsigned int b) {
    return a > b ? b : a;
}

unsigned int max(unsigned int a, unsigned int b) {
    return a > b ? a : b;
}

#ifdef HOLLOW
int isBoundaryPoint(int z, int y, int x) {
  return
    !voxels[z][y][x]
    && z > 0 && y > 0 && x > 0 && z < SIZE - 1 && y < SIZE - 1 && x < SIZE - 1
    && (voxels[z-1][y][x]
    || voxels[z+1][y][x]
    || voxels[z][y-1][x]
    || voxels[z][y+1][x]
    || voxels[z][y][x-1]
    || voxels[z][y][x+1]
    );
}

void makeHollow() {
  unsigned int x, y, z, xn, yn, zn, radius = THICKNESS * SIZE / (RANGE * 2);
  fprintf(stderr, "hollowing out center of bulb\n");
  fprintf(stderr, "radius = %d\n", radius);
  
  for (z = 0; z < SIZE; z++) {
    for (y = 0; y < SIZE; y++) {
      for (x = 0; x < SIZE; x++) {
        buffer[z][y][x] = 0;
      }
    }
  }

  for (z = 0; z < SIZE; z++) {
    fprintf(stderr, "z = %d\n", z);
    for (y = 0; y < SIZE; y++) {
      for (x = 0; x < SIZE; x++) {
        if (isBoundaryPoint(z, y, x)) {
          for (zn = max(0, z - radius); zn < min(SIZE, z + radius); zn++) {
            for (yn = max(0, y - radius); yn < min(SIZE, y + radius); yn++) {
              for (xn = max(0, x - radius); xn < min(SIZE, x + radius); xn++) {
                buffer[zn][yn][xn] = 1;
              }
            }
          }
        }
      }
    }
  }
  
  for (z = 0; z < SIZE; z++) {
    for (y = 0; y < SIZE; y++) {
      for (x = 0; x < SIZE; x++) {
	if (voxels[z][y][x]) {
          voxels[z][y][x] = buffer[z][y][x];
	}
      }
    }
  }
}
#endif

int main()
{
  unsigned int x,y,z,b,volume;
  unsigned int minx, miny, minz, maxx, maxy, maxz;
  double scale = 2 * RANGE / SIZE;
  
  for(z=0;z<SIZE;z++) {
    double fz=valInRange(zlow, zhigh, SIZE, z);
    fprintf(stderr,"fz=%lf\n", fz);
    for(y=0;y<SIZE;y++) {
      double fy=valInRange(ylow, yhigh, SIZE, y);
      for(x=0;x<SIZE;x++) {
        double fx=valInRange(xlow, xhigh, SIZE, x);
        unsigned int val=doPoint(fx,fy,fz);
	voxels[z][y][x] = (val >= (maxit - 1)) ? 1 : 0;
      }
    }
  }
  
#ifdef HOLLOW
  makeHollow();
#endif
  
  fprintf(stderr, "writing voxel array\n");
  volume = 0;
  for (z = 0; z < SIZE; z++) {
    for (y = 0; y < SIZE; y++) {
      for (x = 0; x < SIZE; x++) {
        b = voxels[z][y][x];
        putchar(b ? 255 : 0);
        if (b) {
          volume++;
	  minx = min(x, minx);
	  maxx = max(x, maxx);
	  miny = min(y, miny);
	  maxy = max(y, maxy);
	  minz = min(z, minz);
	  maxz = max(z, maxz);
	}
      }
    }
  }
  fprintf(stderr, "volume: %f (%d voxels)\n",
    (volume * scale * scale * scale),
    volume);
  fprintf(stderr, "z,y,x dimensions: %f, %f, %f (%d, %d, %d voxels)\n",
    (maxz - minz) * scale,
    (maxy - miny) * scale,
    (maxx - minx) * scale,
    maxz - minz,
    maxy - miny,
    maxx - minx);
    
  exit(0);
}

