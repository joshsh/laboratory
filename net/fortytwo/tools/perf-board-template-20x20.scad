/*
OpenSCAD design for a perf board template
Copyright 2018 by Joshua Shinavier
*/

nrows = 20;
ncols = 20;

holeDiameter = 0.042;
holeSpacing = 0.1;
thickness = 0.06;
rimThickness = thickness*2;
cornerRadius = holeDiameter / 2;
border = holeSpacing - holeDiameter;

totalHeight = nrows * holeSpacing + holeDiameter + 2*border;
totalWidth = ncols * holeSpacing + holeDiameter + 2*border;

cornerRoundingRes = 20;


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
    for (j=[0:ncols]) {
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
