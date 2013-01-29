// FlyingWithTheFlock sketch by Joshua Shinavier
// Modifies the CloudsAreLooming sketch by Kyle McDonald:
//     http://openprocessing.org/visuals/?visualID=6753

import toxi.geom.*;
import fullscreen.*; 

FullScreen fs;
Vec3D globalOffset, avg, eye;
public float neighborhood, viscosity, speed, turbulence, cameraRate, rebirthRadius, spread, independence, dofRatio;
public int n, rebirth;
public boolean averageRebirth, paused, fullScreen;
Vector particles;
 
boolean recording;

void setup() {
  size(720, 480, P3D);
   
  setParameters();
  makeControls();
   
  avg = new Vec3D();
  globalOffset = new Vec3D(0, 1. / 3, 2. / 3);
   
  particles = new Vector();
  for(int i = 0; i < n; i++)
    particles.add(new Particle());
    
  // Create the fullscreen object
  fs = new FullScreen(this); 
  fs.setFullScreen(false);
}

void draw() { 
  if (fullScreen)
    fs.setFullScreen(true);
  
  avg = new Vec3D();
  for(int i = 0; i < particles.size(); i++) {
    Particle cur = ((Particle) particles.get(i));
    avg.addSelf(cur.position);
  }
  avg.scaleSelf(1. / particles.size());
 
  Particle leader = (Particle) particles.get(0);
  eye = leader.position;
  Vec3D center = eye.add(leader.velocity.getNormalized());
  camera(eye.x(), eye.y(), eye.z(), center.x(), center.y(), center.z(), 0, 0, 1);
 
  background(0);
  noFill();
  hint(DISABLE_DEPTH_TEST);
  for(int i = 0; i < particles.size(); i++) {
    Particle cur = ((Particle) particles.get(i));
    if(!paused)
      cur.update();
    if (i > 0)
      cur.draw();
  }
   
  for(int i = 0; i < rebirth; i++)
    randomParticle().resetPosition();
   
  if(particles.size() > n)
    particles.setSize(n);
  while(particles.size() < n)
    particles.add(new Particle());
     
  globalOffset.addSelf(
    turbulence / neighborhood,
    turbulence / neighborhood,
    turbulence / neighborhood);
}
 
Particle randomParticle() {
  return ((Particle) particles.get((int) random(particles.size())));
}
 
void keyPressed() {
  if(key == 'p')
    paused = !paused;
}

