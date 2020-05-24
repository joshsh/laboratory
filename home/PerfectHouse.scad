/*
OpenSCAD design for the space surrounding the planned spare bedroom.

Reference datum of the design is the first corner (leading to the family room)
on the right-hand side as you walk in the front door; floor level, on the inside
of the the stone moulding (about 1cm thick).

"Front" is toward the front door, "back" toward the hill, "left" toward the
garden shed, and "right" toward the patio area.

All measurements are in centimeters.
*/
           
apron = 400; // TODO; use the actual boundaries of our plot
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

bathDoorOffset = 9.5; // distance from the back face of the living room back wall to the moving part (of the door leading from the balcony to the master bath area), measured with a ruler
bathDoorWidth = 76; // width of the moving part, measured with a tape
bathDoorHeight = 204; // from the tiled floor to the top of the moving part, measured with a tape

dividerCeilingOffset = 94; // height of the segment of wall dividing the two portions of the sloped ceiling

entryFrontWallThickness = 11; // approximated using a ruler at the front door, disregarding mouldings
entryFrontWallWidth = 206; // measured above the door
entryLength = 320; // measured on ground floor, ignoring moulding thickness

familyRoomEntryWidth = 87.5; // opening between dining room and family room, ignoring moulding

firePitHeight = 56.5; // from stone floor to top of pit, roughly measured with cover still on
firePitLength = 108; // roughly measured with the cover still on
firePitWidth = 108; // roughly measured with the cover still on
firePitXOffset = 252; // measured with a tape using patio grid lines as a reference
firePitYOffset = 94-70; // measured with a tape using patio grid lines as a reference

frontDoorHeight = 203.5; // from the top of the door (the moving part) to the floor
frontDoorWidth = 85.5; // width of the moving part; not the frame
frontDoorLeftOffset = 51; // from the right face of the left wall to the left edge of the moving part

frontPorchDip = 15.5; // front porch is lower than ground floor by this amount
frontPorchLength = 119; // from outer surface of middle front wall to edge of top step
frontPorchWidth = 194; // sticks out slightly beyond the end of the right front wall
frontPorchStep1Depth = 13.5; // first step down from the porch is this deep
frontPorchStep1Length = 153; // from outer surface of middle front wall to edge of 1st step down from front porch
frontPorchStep2Depth = 27; // second step down from the porch (onto the walkway) is this deep

guestBathroomDoorOffset = 9; // distance, while standing inside of the bathroom, from the front face of the living room back wall to the moving part of the door, measured with a ruler
guestBathroomDoorWidth = 66; // width of the moving part, measured with a tape
guestBathroomDoorHeight = 204; // from the floor to the top of the moving part, measured with a tape

/*
Slope determined using six measurements. The planter is very straight, and error is low.
Note that the outer planter (on the street side of the gate) has a slightly different orientation.

x1=0; x2=257; x3=257+395; y1=369; y2=415; y3=485;
(y2-y1)/(x2-x1)
  [1] 0.1789883
(y3-y2)/(x3-x2)
  [1] 0.1772152
s = (y3-y1)/(x3-x1)
s
  [1] 0.1779141
err = y2 - (369 + s*257)
err
  [1] 0.2760736
*/
innerPlanterSlope = 0.1779141;
innerPlanterOffset = 369; // main surface of the planter begins this far beyond the kitchen right wall. The surface is slightly uneven, so that individual measurements may be off by a centimeter or two. The overhanging lip of the planter is ignored.
innerPlanterThickness = 26; // width of the bricks minus the overhanging lip, both measured with a tape
innerPlanterLength = 1005; // approximate, measured with a tape
innerPlanterLead = 5; // inner planter starts about this far toward the street side of reference datum Y, measured with a tape along the planter's length
innerPlanterStep1Height = 54; // measured with a tape. Very short, so there is little variation.
innerPlanterStep1Length = 24; // measured with a tape, ignoring the overhanging lip
innerPlanterStep2Height = 74; // measured with a tape. There is about a 1 cm height difference end to end.
innerPlanterStep2Length = 426; // measured with a tape, ignoring the overhanging lip
innerPlanterStep3Height = 94; // measured with a tape. There is at least a 1 cm height difference end to end.
innerPlanterBarbLength = 59; // length of the extra piece which sticks out at a right angle to the end of the planter near the garden, ignoring the overhanging lip. Measured with a tape

kidRoomRightWallOffset = 103; // from 2nd floor origin to right face of kid room right wall, measured with a tape
kitchenRightWallOffset = 597; // with a measuring tape from the reference datum on the ground floor to the left face of the right outer wall

livingRoomRightCeilingHeight = 263; // distance from the floor to the sloped ceiling on the far right side of the living room
livingRoomFloorDip = 18; // living room floor is lower than ground floor by this amount
livingRoomRightWallOffset = 137.5; // left face of living room right wall extends this far beyond the left face of the kitchen right wall
livingRoomBackWallThickness = 12; // with a ruler; the middle wall is the one the railing is cut out of
livingRoomLeftWallLength = 106.5; // from outer face of middle front wall to outer face of right front wall

livingRoomRoofVerticalOffset = 300; // distance from the patio porch upper surface to the lowermost point where the roof beam emerges from the side of the house, roughly measured with a tape
livingRoomRoofBackOverhang = 28; // on the back (patio) side, the main part of the roof overhangs the back face of the living room back wall by this much, measured with a tape
livingRoomRoofFrontOverhang = 56; // on the front (street) side, the main part of the roof overhangs the front face of the living room front wall by this much, measured with a tape
livingRoomRoofUpperOverhang = 2; // on either side of the living room roof, there is a thinner layer which overhangs the main layer by an additional 2 cm, measured with a ruler
livingRoomRoofUpperThickness = 5.5; // the thickness of the upper layer of the living room roof (which overhangs the lower layer by 2 cm), measured with a ruler
livingRoomRoofTotalThickness = 21; // the total thickness of the living room roof, i.e. the portion which hangs out over the patio and gate, measured with a ruler
livingRoomRoofRightOverhangLength = 183; // the total distance, on the lowermost surface of the roof, from the right surface of the living room right wall to the imaginary lower right point of the roof, measured with a tape. This imaginary point is blunted by a few centimeters.

masterBathroomSideWindowRightOffset = 206; // from the front face of back wall to the outer interior window frame, measured with a tape
masterBathroomSideWindowTopOffset = 32; // from the ceiling to the outer interior window frame, measured with a tape
masterBathroomSideWindowHeight = 88; // dimension of the outer interior window frame, measured with a tape
masterBathroomSideWindowWidth = 120; // dimension of the outer interior window frame, measured with a tape

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

patioGateHeight = 185.5; // height of the middle post, measured up from the rock with a tape
patioGateWidth = 222; // from the house to the first high post, measured with a tape
patioGateThickness = 8.5; // measured with a ruler. The slight (2 cm) offset of the first post from the edge of the house is ignored; subsequent posts are closer to the imaginary line coming out from the house

patioWindowWidth = 57; // dimension of the outermost interior window frame, measured with a tape
patioWindowHeight = 146; // dimension of the outermost interior window frame, measured with a tape
patioWindowRightOffset = 47; // from the outermost interior window frame to the left face of the kitchen right wall, measured with a tape
patioWindowBottomOffset = 77; // from the floor to the outermost interior window frame, measured with a tape

stairwellOffset = 345; // with a measuring tape; distance from the reference datum y to the back of the stairwell; not the same as the start of the kitchen front wall

studyLeftWallOffset = 315+2+2+72; // distance between the right face of the left outer wall and the left face of the left inner wall
studyRightWallThickness = 12; // measured with a ruler

studyWindowCeilingOffset = 32; // ceiling to interior moulding around the window, measured with a tape
studyWindowHeight = 97; // outer dimension of the interior moulding around the window, measured with a tape
studyWindowLeftWallOffset = 74; // from right face of study left wall to outside of interior moulding around the window, with a measuring tape
studyWindowWidth = 129; // outer dimension of the interior moulding around the window, measured with a tape
studyArchOffset = 134; // from reference datum Y to back face of study room arch, measured with a tape
studyDoorLeftOffset = 6.5; // distance from the kids' room right wall to the moving part, measured with a ruler
studyDoorWidth = 76.5; // dimension of the moving part, measured with a tape
studyDoorHeight = 205; // from the floor to the top of the moving part, measured with a tape

studyBackWallOffset = 38; // from front face of study room arch to front face of study room back wall, measured with a tape

windowBase = 61;
windowHeight = 146;
windowLength = 134;
windowLip = 7.5; // lateral offset of window from inside of middle front wall
windowOffset = 178.5; // offset of window from inside of left front wall

secondFloorThickness = firstFloorHeight - firstFloorCeilingHeight;
studyRightWallLength = entryLength-windowLip+windowOffset;
totalHeight = firstFloorHeight + secondFloorHeight;
livingRoomRightWallLength = entryLength+entryFrontWallThickness+livingRoomLeftWallLength;

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
studyArchThickness = livingRoomBackWallThickness;
studyBackWallThickness = livingRoomBackWallThickness;
kidRoomRightWallThickness = livingRoomBackWallThickness;

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
backRightX = backLeftX + backWallWidth;
frontRightX = kitchenRightWallOffset + livingRoomRightWallOffset + livingRoomRightWallThickness;

dh = totalHeight+livingRoomFloorDip-livingRoomRightCeilingHeight;
dl = kitchenRightWallOffset + livingRoomRightWallOffset;
livingRoomRoomAngle = atan(dh/dl);

gardenWallLength = 621+503+790; // measured with a tape in three segments. The leftmost end of the wall is covered in ivy and difficult to measure; I assumed it is the same as the beginning of the wooden fence (which, however, is leaning inward)
gardenWallThickness = 20; // TODO
gardenWallHeight = 82; // average of two measurements; the heigh varies by at least 2 cm end to end
gardenWallLeftOffset = 356; // distance from the back left corner of the house, on the Y axis, to the garden wall. Measured with a tape
gardenWallRightOffset = 291; // distance from the back right corner of the house, on the Y axis, to the garden wall. Measured with a tape
gardenWallRightOffsetLength = 496; // distance between the right end of the garden wall and the gardenWallRightOffset point, measured with a tape (and eyeballed, on the X axis)
x1 = 0; x2 = backRightX - backLeftX; y1 = 291; y2 = 356;
gardenWallAngle = atan((y2-y1)/(x2-x1));

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

module firePit() {
  translate([backRightX+firePitXOffset, backLeftY-firePitYOffset-firePitLength, -patioDip]) {
    color("gray")
    cube(size=[firePitWidth, firePitLength, firePitHeight]);
  }
}

module frontPorch() {
  translate([0, entryLength+entryFrontWallThickness, -groundDip]) {
    cube(size=[frontPorchWidth, frontPorchLength, groundDip-frontPorchDip]);
    cube(size=[frontPorchWidth, frontPorchStep1Length, groundDip-frontPorchDip-frontPorchStep1Depth]);
  }
}

module gardenWall() {
  translate([backRightX, backLeftY-gardenWallRightOffset, -patioDip]) {
    rotate([0, 0, gardenWallAngle]) {
      translate([-gardenWallLength+gardenWallRightOffsetLength, 0, 0]) {
        color("gray")
        cube(size=[gardenWallLength, gardenWallThickness, gardenWallHeight]);
      }
    }
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

module innerPlanter() {
  angle = atan(innerPlanterSlope);
  translate([kitchenRightWallOffset+kitchenRightWallThickness+innerPlanterOffset, 0, -patioDip]) {
    rotate([0,0,-90+angle]) {
      translate([-innerPlanterLead, 0, 0]) {
        union() {
          color("brown")
          cube(size=[innerPlanterLength, innerPlanterThickness, innerPlanterStep1Height]);
          translate([innerPlanterStep1Length, 0, 0]) {
            color("brown")
            cube(size=[innerPlanterLength-innerPlanterStep1Length, innerPlanterThickness, innerPlanterStep2Height]);
          }
          translate([innerPlanterStep1Length+innerPlanterStep2Length, 0, 0]) {
            color("brown")
            cube(size=[innerPlanterLength-innerPlanterStep1Length-innerPlanterStep2Length, innerPlanterThickness, innerPlanterStep3Height]);
          }
          translate([innerPlanterLength-innerPlanterThickness, -innerPlanterBarbLength, 0]) {
            color("brown")
            cube(size=[innerPlanterThickness, innerPlanterThickness+innerPlanterBarbLength, innerPlanterStep3Height]);
          }
        }
      }
    }
  }
}

module kidRoomRightWall() {
  length = backWallOffset+kitchenBackWallThickness+studyArchOffset+studyArchThickness+studyBackWallOffset;
  translate([-kidRoomRightWallOffset-kidRoomRightWallThickness, 0, firstFloorHeight]) {
    difference() {
      translate([0, backLeftY, 0]) {
        color("white")
        cube(size=[kidRoomRightWallThickness, length, secondFloorHeight]);
      }
      translate([-smidge, livingRoomBackWallThickness+guestBathroomDoorOffset, 0]) {
        cube(size=[kidRoomRightWallThickness+2*smidge, guestBathroomDoorWidth, guestBathroomDoorHeight]);
      }
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
      union() {
        // patio door
        translate([-smidge, livingRoomBackWallThickness-patioDoorOffset-patioDoorWidth, 0]) {
          cube(size=[kitchenRightWallThickness + 2*smidge, patioDoorWidth, patioDoorHeight]);
        }
        // master bath side window
        translate([-smidge, backLeftY+kitchenBackWallThickness+masterBathroomSideWindowRightOffset, totalHeight-masterBathroomSideWindowTopOffset-masterBathroomSideWindowHeight]) {
          cube(size=[kitchenRightWallThickness + 2*smidge, masterBathroomSideWindowWidth, masterBathroomSideWindowHeight]);
        }
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
    translate([0, -smidge, 0]) {
      union() {
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
        // arch leading from the balcony to the study room
        translate([-kidRoomRightWallOffset, 0, firstFloorHeight]) {
          cube(size=[kidRoomRightWallOffset-studyRightWallThickness, livingRoomBackWallThickness + 2*smidge, secondFloorHeight+smidge]);
        }
        // patio window
        translate([kitchenRightWallOffset+patioWindowRightOffset, 0, patioWindowBottomOffset-livingRoomFloorDip]) {
          cube(size=[patioWindowWidth, livingRoomBackWallThickness + 2*smidge, patioWindowHeight]);
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
      translate([0, -livingRoomBackWallThickness+backLeftY, -groundDip]) {
        color("white")
        cube(size=[livingRoomLeftWallThickness, -backLeftY+entryLength+entryFrontWallThickness+livingRoomLeftWallLength, wallHeight]);
      }
      translate([-smidge, 0, 0]) {
        union() {
          // arch between entry room and living room
          cube(size=[livingRoomLeftWallThickness + 2*smidge, entryLength-livingRoomBackWallThickness, totalHeight-dividerCeilingOffset]);
          // door to master bath area
          translate([0, -livingRoomBackWallThickness-bathDoorOffset-bathDoorWidth, firstFloorHeight]) {
            cube(size=[livingRoomLeftWallThickness + 2*smidge, bathDoorWidth, bathDoorHeight]);
          }
        }
      }
    }
  }
}

module livingRoomRightWall() {
  translate([kitchenRightWallOffset+livingRoomRightWallOffset, 0, -groundDip]) {
    color("white")
    cube(size=[livingRoomRightWallThickness, livingRoomRightWallLength, wallHeight]);
  }
}

module livingRoomRoof() {
  length = livingRoomRightWallLength + livingRoomRoofBackOverhang + livingRoomRoofFrontOverhang;
  translate([backRightX, -livingRoomRoofBackOverhang, livingRoomRoofVerticalOffset-patioPorchTopStepDip]) {
    rotate([0, livingRoomRoomAngle, 0]) {
      union() {
        color("white")
        cube(size=[livingRoomRoofRightOverhangLength, length, livingRoomRoofTotalThickness]);
        translate([0, -livingRoomRoofUpperOverhang, livingRoomRoofTotalThickness-livingRoomRoofUpperThickness]) {
          color("white")
          cube(size=[livingRoomRoofRightOverhangLength, length + 2*livingRoomRoofUpperOverhang, livingRoomRoofUpperThickness]);
        }
      }
    }
  }
  // TODO: the overhanging end of the roof does not form a right angle but is cut off by a vertical plane intersecting the lowermost point of the current roof. You need to calculate the X at which that plane starts.
}

module livingRoomSlopedCeiling() {
  translate([0, -smidge, totalHeight]) {
    rotate([0, livingRoomRoomAngle, 0]) {
      color("white")
      cube(size=[1000, 600, 500]);
    }
  }
}

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
      translate([0, -patioPorchMiddleStepLength-3*apron, groundDip-patioDip-smidge]) {
        cube(size=[patioPorchMiddleStepWidth + 1.5*apron, patioPorchMiddleStepLength + 3*apron, smidge]);
      }
    }
  }
}

module patioGate() {
  translate([frontRightX, 0, -patioDip]) {
    color("brown")
    cube(size=[patioGateWidth, patioGateThickness, patioGateHeight]);
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

module studyArch() {
  translate([-kidRoomRightWallOffset, studyArchOffset, firstFloorHeight]) {
    difference() {
      color("white")
      cube(size=[kidRoomRightWallOffset, studyArchThickness, secondFloorHeight]);
      translate([studyDoorLeftOffset, -smidge, 0]) {
        cube(size=[studyDoorWidth, studyArchThickness+2*smidge, studyDoorHeight]);
      }
    }
  }
}

module studyBackWall() {
  translate([backLeftX+studyLeftWallThickness, studyArchOffset+studyArchThickness+studyBackWallOffset-studyBackWallThickness, firstFloorHeight]) {
    color("white")
    cube(size=[studyLeftWallOffset-kidRoomRightWallOffset, studyBackWallThickness, secondFloorHeight]);
  }
}

module studyFrontWall() {
  difference() {
    translate([-studyRightWallThickness-studyLeftWallOffset-studyLeftWallThickness, studyRightWallLength, firstFloorHeight]) {
      color("white")
      cube(size=[studyRightWallThickness+studyLeftWallOffset+studyLeftWallThickness, studyFrontWallThickness, secondFloorHeight]);
    }
    // front window of the study
    translate([backLeftX+studyLeftWallThickness+studyWindowLeftWallOffset, studyRightWallLength-smidge, totalHeight-studyWindowCeilingOffset-studyWindowHeight]) {
      cube(size=[studyWindowWidth, studyFrontWallThickness + 2*smidge, studyWindowHeight]);
    }
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
  union() {
    kitchenBackWall();
    kitchenRightWall();

    difference() {
      union() {
        entryFrontWall();

        livingRoomBackWall();
        livingRoomFrontWall();
        livingRoomRightWall();
        livingRoomLeftWall();
      }

      livingRoomSlopedCeiling();
    }

    studyArch();
    studyBackWall();
    studyFrontWall();
    studyRightWall();
    studyLeftWall();

    kidRoomRightWall();
  }
}

union() {
  walls();

  ground();
  groundFloor();
  livingRoomFloor();
  livingRoomRoof();
  secondFloor();

  frontPorch();
  patio();
  patioGate();
  firePit();
  innerPlanter();
  gardenWall();
}
