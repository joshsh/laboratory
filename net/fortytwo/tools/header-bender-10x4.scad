/*
OpenSCAD design for a header-bender tool
Copyright 2018 by Joshua Shinavier
*/

cornerRoundingRes = 20;
//cornerRoundingRes = 8;

nrows = 5;
ncols = 11;
nstacks = 5;

// Estimated shrinkage (relative to ideal dimensions)
shrinkage = 0.275;
// Actual shrinkage could be plus or minus this error
shrinkageError = 0.02;
// Avoids ambiguity when positive and negative space meets at boundaries
overflow = 0.01;

idealHoleSpacing = 0.1;
idealHoleDiameter = 0.042;

holeSpacing = correct(idealHoleSpacing);
funnelDepth = correct(0.15);

L = max(nrows, ncols, nstacks) * holeSpacing;
e = shrinkageError;
D = correct(idealHoleDiameter);
holeDiameter = (D + e * L) / (1 - e);

cornerRadius = holeDiameter / 2;
// Just small enough that another row of pins can slide in beyond the border
border = holeSpacing - holeDiameter;

height = (nrows - 1) * holeSpacing + 2 * border;
width = (ncols - 1) * holeSpacing + 2 * border;
thickness = (nstacks - 1) * holeSpacing + 2 * border;

function correct(dim) = dim * (1 + shrinkage);

module roundedRect(width, height, thick, radius) {
  w = width - 2*radius;
  h = height - 2*radius;
  d = thick - 2 * radius;
  translate([radius, radius, 0]) {
    cube([w, h, thick]);
  }
  translate([radius, 0, radius]) {
    cube([w, height, d]);
  }
  translate([0, radius, radius]) {
    cube([width, h, d]);
  }
  translate([0,0,radius]) {
    translate([radius, radius, 0]) {
      cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      cube([w, h, d]);
      sphere(r=radius, $fn=cornerRoundingRes);
      translate([0, 0, d]) {
        sphere(r=radius, $fn=cornerRoundingRes);
      }
    }
    translate([width-radius, radius, 0]) {
      cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      sphere(r=radius, $fn=cornerRoundingRes);
      translate([0, 0, d]) {
        sphere(r=radius, $fn=cornerRoundingRes);
      }
    }
    translate([radius, height-radius, 0]) {
      cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      sphere(r=radius, $fn=cornerRoundingRes);
      translate([0, 0, d]) {
        sphere(r=radius, $fn=cornerRoundingRes);
      }
    }
    translate([width-radius, height-radius, 0]) {
      cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      sphere(r=radius, $fn=cornerRoundingRes);
      translate([0, 0, d]) {
        sphere(r=radius, $fn=cornerRoundingRes);
      }
    }
  }
  rotate([90,0,0]) {
    rotate([0,90,0]) {
      translate([0,0,radius]) {
        translate([radius, radius, 0]) {
          cylinder(r=radius, h=w, $fn=cornerRoundingRes);
        }
        translate([height-radius, radius, 0]) {
          cylinder(r=radius, h=w, $fn=cornerRoundingRes);
        }
        translate([radius, thick-radius, 0]) {
          cylinder(r=radius, h=w, $fn=cornerRoundingRes);
        }
        translate([height-radius, thick-radius, 0]) {
          cylinder(r=radius, h=w, $fn=cornerRoundingRes);
        }
      }
    }
  }
  translate([0, height-radius, 0]) {
    rotate([90,0,0]) {
      translate([radius, radius, 0]) {
        cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      }
      translate([width-radius, radius, 0]) {
        cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      }
      translate([radius, height-radius, 0]) {
        cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      }
      translate([width-radius, height-radius, 0]) {
        cylinder(r=radius, h=d, $fn=cornerRoundingRes);
      }
    }
  }
} 

module originReference() {
  translate([-cornerRadius,-cornerRadius,-cornerRadius]) {
    cube([cornerRadius*2, cornerRadius*2, cornerRadius*4]);
  }
}

module blank() {
  roundedRect(width, height, thickness, cornerRadius);
  //originReference();
}

module holes(rows, cols, thickness, coneDepth) {
  r = holeDiameter / 2;
  r2 = sqrt(2*holeSpacing*holeSpacing) / 2;
  t = thickness + 2 * overflow;
  for (i=[0:(rows-1)]) {
    for (j=[0:(cols-1)]) {
      translate([border+r + j*holeSpacing, border+r + i*holeSpacing,0]) {
        translate([0,0,-overflow]) {
          cylinder(h=t, r=r, $fn=cornerRoundingRes);
        }
        if (coneDepth > 0) {
          translate([0, 0, thickness - coneDepth]) {
            cylinder(h=coneDepth, r1=0, r2=r2, $fn=cornerRoundingRes);
          }
        }
      }
    }
  }
}

module verticalHoles() {
  holes(nrows, ncols, thickness, 0);
}

module horizontalHoles() {
  rotate([90,0,0]) {
    translate([0,0,-height]) {
      holes(nstacks, ncols, height, 0);
    }
  }
}

module transverseHoles() {
  rotate([90,0,0]) {
    rotate([0,90,0]) {
      holes(nstacks, nrows, width, funnelDepth);
    }
  }
}

difference() {
  blank();
  union() {
    verticalHoles();
    horizontalHoles();
    transverseHoles();
  }
}
