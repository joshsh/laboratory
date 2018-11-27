/*
OpenSCAD design for a perf board template
Copyright 2018 by Joshua Shinavier
*/

// note: there will actually be nrows + 1 rows and nrows + 1 columns
nrows = 9;

// estimated shrinkage (relative to ideal dimensions)
shrinkage = 0.03;
// actual shrinkage could be plus or minus this error
shrinkageError = 0.02;

idealHoleSpacing = 0.1;
idealHoleDiameter = 0.042;

holeSpacing = correct(idealHoleSpacing);

L = nrows * holeSpacing;
e = shrinkageError;
D = correct(idealHoleDiameter);
holeDiameter = (e * L + D) / (1 - e);

// shrinkage is not important for thickness
thickness = 0.06;
rimThickness = thickness*2;

cornerRadius = holeDiameter / 2;
border = (holeSpacing - holeDiameter) * 2 / 3;

totalHeight = nrows * holeSpacing + holeDiameter + 2*border;
totalWidth = nrows * holeSpacing + holeDiameter + 2*border;

cornerRoundingRes = 20;

function correct(length) = length * (1 + shrinkage);

module roundedRect(width, height, thick, radius) {
  w = width - 2*radius;
  h = height - 2*radius;
  translate([radius, radius, 0]) {
    cylinder(r=radius, h=thick, $fn=cornerRoundingRes);
    cube([w, h, thick]);
  }
  translate([width-radius, radius, 0]) {
    cylinder(r=radius, h=thick, $fn=cornerRoundingRes);
  }
  translate([radius, height-radius, 0]) {
    cylinder(r=radius, h=thick, $fn=cornerRoundingRes);
  }
  translate([width-radius, height-radius, 0]) {
    cylinder(r=radius, h=thick, $fn=cornerRoundingRes);
  }
  translate([0,radius,0]) {
    cube([width, h, thick]);
  }
  translate([radius,0,0]) {
    cube([w, height, thick]);
  }
} 

module blank() {
  roundedRect(totalWidth, totalHeight, rimThickness, cornerRadius);
}

module cutout() {
  translate([border, border, thickness]) {
    cube([totalWidth, totalHeight, rimThickness]);
  }
}

module holes() {
  r = holeDiameter / 2;
  for (i=[0:nrows]) {
    for (j=[0:nrows]) {
      translate([border+r + j*holeSpacing,border+r + i*holeSpacing,0]) {
        cylinder(r=r, h=thickness, $fn=cornerRoundingRes);
      }
    }
  }
}

difference() {
  blank();
  union() {
    cutout();
    holes();
  }
}
