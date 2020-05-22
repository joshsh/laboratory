/*
OpenSCAD design for the space surrounding the planned spare bedroom.

Reference datum of the design is the first corner (leading to the family room)
on the right-hand side as you walk in the front door; floor level, on the inside
of the the stone moulding (about 1cm thick).

"Front" is toward the front door, "back" toward the hill, "left" toward the
garden shed, and "right" toward the patio area.

All measurements are in centimeters.
*/

apron = 200; // TODO; use the actual boundaries of our plot
smidge = 3;

bridgeWidth = 112; // ignoring the carpeted lip

firstFloorCeilingHeight = 242; // from floor to ceiling, by tape measure
firstFloorHeight = 264;
roomWidth = 193;
secondFloorHeight = 243;
wallThickness = 12;

centralPostOffset = 193; // with measuring tape from reference datum to left face of the cental post, disregarding moulding
centralPostWidth = 17.5; // with a ruler, in the right-left direction

backWallOffset = 393+317; // measured in two steps with a measuring tape (reference datum y to start of wooden flooring in master bedroom, and from there to the back wall, ignoring moulding)

balconyCeilingOffset = 31; // distance from the top of the balcony arch to the (non-sloping) ceiling
balconyRightLimit = centralPostOffset-2.5; // measured with a ruler

dividerCeilingOffset = 94; // height of the segment of wall dividing the two portions of the sloped ceiling

entryFrontWallThickness = 11; // approximated using a ruler at the front door, disregarding mouldings
entryFrontWallWidth = 206; // measured above the door
entryLength = 320; // measured on ground floor, ignoring moulding thickness

familyRoomEntryWidth = 87.5; // opening between dining room and family room, ignoring moulding

frontDoorHeight = 203.5; // from the top of the door (the moving part) to the floor
frontDoorWidth = 85.5; // width of the moving part; not the frame
frontDoorLeftOffset = 51; // from the right face of the left wall to the left edge of the moving part

frontPorchDip = 15.5; // front porch is lower than ground floor by this amount
frontPorchLength = 119; // from outer surface of middle front wall to edge of top step
frontPorchWidth = 194; // sticks out slightly beyond the end of the right front wall
frontPorchStep1Depth = 13.5; // first step down from the porch is this deep
frontPorchStep1Length = 153; // from outer surface of middle front wall to edge of 1st step down from front porch
frontPorchStep2Depth = 27; // second step down from the porch (onto the walkway) is this deep

kitchenRightWallOffset = 597; // with a measuring tape from the reference datum on the ground floor to the left face of the right outer wall

livingRoomRightCeilingHeight = 263; // distance from the floor to the sloped ceiling on the far right side of the living room
livingRoomFloorDip = 18; // living room floor is lower than ground floor by this amount
livingRoomRightWallOffset = 137.5; // left face of living room right wall extends this far beyond the left face of the kitchen right wall
livingRoomBackWallThickness = 12; // with a ruler; the middle wall is the one the railing is cut out of
livingRoomLeftWallLength = 106.5; // from outer face of middle front wall to outer face of right front wall

patioDoorHeight = 201; // with a measuring tape; floor to inner surface of door frame, ignoring floor moulding
patioDoorOffset = 65; // with a measuring tape; distance from the front face of the living room back wall to the inner left surface of the door frame
patioDoorWidth = 85; // with a measuring tape; inner width of door frame
patioPorchMiddleStepLength = 257; // measuring tape
patioPorchMiddleStepDip = 2 + 16.5;
patioPorchMiddleStepWidth = 152.5;
patioPorchTopStepDip = 2; // roughly measured with a level and a ruler; the top step of the patio is slightly lower than the 1st floor
patioPorchTopStepLength = 227; // measuring tape
patioPorchTopStepWidth = 122.5;
patioDip = 2 + 2*16.5; // with ruler

stairwellOffset = 345; // with a measuring tape; distance from the reference datum y to the back of the stairwell; not the same as the start of the kitchen front wall

studyLeftWallOffset = 315+2+2+72; // distance between the right face of the left outer wall and the left face of the left inner wall
studyRightWallThickness = 12; // measured with a ruler

windowBase = 61;
windowHeight = 146;
windowLength = 134;
windowLip = 7.5; // lateral offset of window from inside of middle front wall
windowOffset = 178.5; // offset of window from inside of left front wall

secondFloorThickness = firstFloorHeight - firstFloorCeilingHeight;
studyRightWallLength = entryLength-windowLip+windowOffset;
totalHeight = firstFloorHeight + secondFloorHeight;

// from the reference datum z (uppermost surface of the ground floor) down to the ground outside
groundDip = frontPorchDip + frontPorchStep2Depth;

// just convenient assumptions; not verified
kitchenBackWallThickness = entryFrontWallThickness;
livingRoomFrontWallThickness = entryFrontWallThickness;
livingRoomLeftWallThickness = entryFrontWallThickness;
livingRoomRightWallThickness = entryFrontWallThickness;
kitchenRightWallThickness = entryFrontWallThickness;
studyLeftWallThickness = entryFrontWallThickness;
studyFrontWallThickness = entryFrontWallThickness;

// back left corner of the house at exterior ground level
backLeftX = -studyRightWallThickness-studyLeftWallOffset-studyLeftWallThickness;
backLeftY = -backWallOffset - kitchenBackWallThickness;
backLeftZ = -groundDip;
//houseWidth = 
//houseLength = 
wallHeight = groundDip + totalHeight; // from exterior ground to 2nd floor ceiling
houseLength = (studyRightWallLength + studyFrontWallThickness) - backLeftY; // TODO; garage not accounted for
houseWidth = studyLeftWallThickness+studyLeftWallOffset+studyRightWallThickness+kitchenRightWallOffset+livingRoomRightWallOffset+livingRoomRightWallThickness;
backWallWidth = houseWidth - livingRoomRightWallThickness - livingRoomRightWallOffset + kitchenRightWallThickness;

module entryFrontWall() {
  difference() {
    translate([0, entryLength, -groundDip]) {
      color("white")
      cube(size = [entryFrontWallWidth, entryFrontWallThickness, wallHeight]);
    }
    // front door hole
    translate([frontDoorLeftOffset, entryLength-smidge, 0]) {
      cube(size=[frontDoorWidth, entryFrontWallThickness+2*smidge, frontDoorHeight]);
    }
  }
}

module frontPorch() {
  translate([0, entryLength+entryFrontWallThickness, -groundDip]) {
    cube(size=[frontPorchWidth, frontPorchLength, groundDip-frontPorchDip]);
    cube(size=[frontPorchWidth, frontPorchStep1Length, groundDip-frontPorchDip-frontPorchStep1Depth]);
  }
}

// note: does not take grade into account; assuming level ground may not be safe
module ground() {
  translate([backLeftX - apron, backLeftY - apron, backLeftZ - smidge]) {
    color("green")
    cube(size=[houseWidth + 2*apron, houseLength + 2*apron, smidge]);
  }
}

module groundFloor() {
  translate([0, -backWallOffset, backLeftZ]) {
    union() {
      cube(size=[roomWidth+wallThickness, backWallOffset+entryLength, groundDip]);
      cube(size=[kitchenRightWallOffset, backWallOffset, groundDip]);
    }
  }
}

module kitchenBackWall() {
  translate([backLeftX, backLeftY, backLeftZ]) {
    color("white")
    cube(size=[backWallWidth, kitchenBackWallThickness, wallHeight]);
  }
}

module kitchenRightWall() {
  translate([kitchenRightWallOffset, 0, 0]) {
    difference() {
      translate([0, -backWallOffset, -groundDip]) {
        color("white")
        cube(size=[kitchenRightWallThickness, backWallOffset+livingRoomBackWallThickness, wallHeight]);
      }
      translate([-smidge, livingRoomBackWallThickness-patioDoorOffset-patioDoorWidth, 0]) {
        cube(size=[kitchenRightWallThickness + 2*smidge, patioDoorWidth, patioDoorHeight]);
      }
    }
  }
}

module livingRoomBackWall() {
  difference() {
    translate([backLeftX, 0, backLeftZ]) {
      color("white")
      cube(size=[houseWidth, livingRoomBackWallThickness, wallHeight]);
    }
    union() {
      translate([0, -smidge, 0]) {
        // arch under the balcony
        cube(size=[centralPostOffset, livingRoomBackWallThickness + 2*smidge, firstFloorCeilingHeight]);
        // arch above the balcony
        translate([0, 0, firstFloorHeight]) {
          cube(size=[balconyRightLimit, livingRoomBackWallThickness + 2*smidge, secondFloorHeight-balconyCeilingOffset]);
        }
        // arch connecting the living room and dining room
        translate([centralPostOffset+centralPostWidth, 0, 0]) {
          cube(size=[kitchenRightWallOffset-(centralPostOffset+centralPostWidth), livingRoomBackWallThickness + 2*smidge, firstFloorCeilingHeight]);
        }
      }
    }
  }
}

module livingRoomFloor() {
  length = entryLength-livingRoomBackWallThickness+livingRoomLeftWallLength+entryFrontWallThickness-livingRoomFrontWallThickness;
  width = kitchenRightWallOffset + livingRoomRightWallOffset - entryFrontWallWidth;
  translate([entryFrontWallWidth, livingRoomBackWallThickness, -groundDip]) {
    cube(size=[width, length, groundDip - livingRoomFloorDip]);
  }
}

module livingRoomFrontWall() {
  width = kitchenRightWallOffset+livingRoomRightWallOffset+livingRoomRightWallThickness - (entryFrontWallWidth-livingRoomLeftWallThickness);

  translate([entryFrontWallWidth-livingRoomLeftWallThickness, entryLength+entryFrontWallThickness+livingRoomLeftWallLength-livingRoomFrontWallThickness, -groundDip]) {
    color("white")
    cube(size=[width, livingRoomFrontWallThickness, wallHeight]);
  }
}

module livingRoomLeftWall() {
  translate([entryFrontWallWidth-livingRoomLeftWallThickness, livingRoomBackWallThickness, 0]) {
    difference() {
      translate([0, 0, -groundDip]) {
        color("white")
        cube(size=[livingRoomLeftWallThickness, entryLength-livingRoomBackWallThickness+entryFrontWallThickness+livingRoomLeftWallLength, wallHeight]);
      }
      // arch between entry room and living room
      translate([-smidge, 0, 0]) {
        cube(size=[livingRoomLeftWallThickness + 2*smidge, entryLength-livingRoomBackWallThickness, totalHeight-dividerCeilingOffset]);
      }
    }
  }
}

module livingRoomRightWall() {
  length = entryLength+entryFrontWallThickness+livingRoomLeftWallLength;
  translate([kitchenRightWallOffset+livingRoomRightWallOffset, 0, -groundDip]) {
    color("white")
    cube(size=[livingRoomRightWallThickness, length, wallHeight]);
  }
}

module livingRoomSlopedCeiling() {
  dh = totalHeight+livingRoomFloorDip-livingRoomRightCeilingHeight;
  dl = kitchenRightWallOffset + livingRoomRightWallOffset;
  angle = atan(dh/dl);
  translate([0, -smidge, totalHeight]) {
    rotate([0,angle,0]) {
    //rotate([0,15,0]) {
      color("blue")
      cube(size=[1000, 600, 500]);
    }
  }
}

//patioDip = 2 + 2*16.5; // with ruler

module patio() {
  translate([kitchenRightWallOffset + kitchenRightWallThickness, 0, -groundDip]) {
    union(){
      // top step
      translate([0, -patioPorchTopStepLength, 0]) {
        cube(size=[patioPorchTopStepWidth, patioPorchTopStepLength, groundDip-patioPorchTopStepDip]);
      }
      // middle (lower) step
      translate([0, -patioPorchMiddleStepLength, 0]) {
        cube(size=[patioPorchMiddleStepWidth, patioPorchMiddleStepLength, groundDip-patioPorchMiddleStepDip]);
      }
      // patio surface
      // TODO: use actual patio diminsions
      translate([0, -patioPorchMiddleStepLength-apron, groundDip-patioDip-smidge]) {
        cube(size=[patioPorchMiddleStepWidth + apron, patioPorchMiddleStepLength + apron, smidge]);
      }
    }
  }
}

module secondFloor() {
  translate([0, -backWallOffset, firstFloorCeilingHeight]) {
    translate([-studyRightWallThickness-studyLeftWallOffset, 0, 0]) {
      cube(size=[studyLeftWallOffset, backWallOffset+studyRightWallLength, secondFloorThickness]);
    }
    difference() {
      cube(size=[kitchenRightWallOffset, backWallOffset, secondFloorThickness]);
      translate([0, backWallOffset-stairwellOffset, -smidge]) {
        cube(size=[centralPostOffset, stairwellOffset - (bridgeWidth - livingRoomBackWallThickness), secondFloorThickness + 2*smidge]);
      }
    }
  }
}

module studyBackWall() {
  //color("white")
  // TODO
}

module studyFrontWall() {
  translate([-studyRightWallThickness-studyLeftWallOffset-studyLeftWallThickness, studyRightWallLength, firstFloorHeight]) {
    color("white")
    cube(size=[studyRightWallThickness+studyLeftWallOffset+studyLeftWallThickness, studyFrontWallThickness, secondFloorHeight]);
  }
}

module studyLeftWall() {
  translate([backLeftX, -backWallOffset, -groundDip]) {
    color("white")
    cube(size=[studyLeftWallThickness, backWallOffset+studyRightWallLength, wallHeight]);
  }
}

module studyRightWall() {
  translate([-studyRightWallThickness, 0, 0]) {
    difference() {
      translate([0, -backWallOffset, -groundDip]) {
        color("white")
        cube(size = [studyRightWallThickness, backWallOffset+studyRightWallLength, wallHeight]);
      }
      translate([-smidge, 0, 0]) {
        union() {
          // family room entrance
          translate([0, -familyRoomEntryWidth, 0]) {
            cube(size=[studyRightWallThickness+2*smidge, familyRoomEntryWidth, firstFloorCeilingHeight]);
          }
          // opening next to the stairwell
          translate([0, -stairwellOffset, firstFloorHeight]) {
            cube(size=[studyRightWallThickness+2*smidge, stairwellOffset, secondFloorHeight+smidge]);
          }
          // study room balcony hole
          translate([0, studyRightWallLength-windowOffset-windowLength, firstFloorHeight+windowBase]) {
            cube([studyRightWallThickness+2*smidge, windowLength, windowHeight]);
          }
        }
      }
    }
  }
}

module walls() {
 difference() {
 //union() {
   union() {
     entryFrontWall();

     kitchenBackWall();
     kitchenRightWall();

     livingRoomBackWall();
     livingRoomFrontWall();
     livingRoomRightWall();
     livingRoomLeftWall();

     studyBackWall();
     studyFrontWall();
     studyRightWall();
     studyLeftWall();
   }
   livingRoomSlopedCeiling();
 }
}

union() {
  walls();

  ground();
  groundFloor();
  livingRoomFloor();
  secondFloor();

  frontPorch();
  patio();
}

