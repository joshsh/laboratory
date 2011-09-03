/* (c) Dave Gilbert dave@treblig.org 2010

  Free to use for any purpose as long as you acknowlege the source and don't
  hold me responsible for the result

  Create a really dumb .stl file on stdout from a binary file consisting of one byte
   per voxel

 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/mman.h>

const size_t size=256;
#define nvoxels (size*size*size)

static char* voxels;


double topos(int i)
{
  return (double)i/(double)(size);
}

void dotri(int x0, int y0, int z0,
	   int x1, int y1, int z1,
	   int x2, int y2, int z2,
	   int nx, int ny, int nz)
{
  printf("facet normal %d %d %d\n  outer loop\n", nx, ny, nz);
  printf("    vertex %lf %lf %lf\n", topos(x0), topos(y0), topos(z0));
  printf("    vertex %lf %lf %lf\n", topos(x1), topos(y1), topos(z1));
  printf("    vertex %lf %lf %lf\n", topos(x2), topos(y2), topos(z2));
  printf("  end loop\n  end facet\n");
}

void doface(int x, int y, int z,
	    int x0, int y0, int z0,  
	    int x1, int y1, int z1,  
	    int x2, int y2, int z2,  
	    int x3, int y3, int z3,
            int nx, int ny, int nz)
{
  dotri(x+x0, y+y0, z+z0, x+x1, y+y1, z+z1, x+x2, y+y2, z+z2, nx,ny,nz);
  dotri(x+x2, y+y2, z+z2, x+x3, y+y3, z+z3, x+x0, y+y0, z+z0, nx,ny,nz);
}

int getvoxel(int x, int y, int z)
{
   if ((x>=size) || (x<0)) return 0;
   if ((y>=size) || (y<0)) return 0;
   if ((z>=size) || (z<0)) return 0;
   return (int)voxels[x+y*size+z*size*size];
}

static void dovoxel(int x, int y, int z)
{
  if (getvoxel(x,y,z)!=0) {
    if (getvoxel(x,y,z-1)==0)
    	doface(x,y,z, 0,1,0, 1,1,0, 1,0,0, 0,0,0, 0,0,-1); // In the Z plane at the back  
    if (getvoxel(x,y,z+1)==0)
    	doface(x,y,z, 0,0,1, 1,0,1, 1,1,1, 0,1,1, 0,0,1); // In the Z plane at the front
    if (getvoxel(x-1,y,z)==0)
    	doface(x,y,z, 0,0,1, 0,1,1, 0,1,0, 0,0,0, -1,0,0); // On the left hand side
    if (getvoxel(x+1,y,z)==0)
    	doface(x,y,z, 1,0,0, 1,1,0, 1,1,1, 1,0,1, 1,0,0); // On the right hand side
    if (getvoxel(x,y-1,z)==0)
    	doface(x,y,z, 0,0,0, 1,0,0, 1,0,1, 0,0,1, 0,-1,0); // On the top
    if (getvoxel(x,y+1,z)==0)
    	doface(x,y,z, 0,1,1, 1,1,1, 1,1,0, 0,1,0, 0,1,0); // On the bottom
  }
}

int main()
{
  int x,y,z;

  voxels=malloc(nvoxels);
  if (voxels==NULL) {
	fprintf(stderr,"Failed to malloc memory for voxel array\n");
	exit(1);
  }

  if (read(0, voxels, nvoxels)!=nvoxels) {
	fprintf(stderr,"Failed to read full voxel array from stdin\n");
	exit(1);
  }

  printf("solid davessolid\n");

  for(z=0;z<size;z++)
    for(y=0;y<size;y++)
      for(x=0;x<size;x++) {
	dovoxel(x,y,z);
      }

  printf("endsolid davessolid\n");
}

