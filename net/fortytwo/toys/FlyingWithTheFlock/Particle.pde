// FlyingWithTheFlock sketch by Joshua Shinavier
// Modifies the CloudsAreLooming sketch by Kyle McDonald:
//     http://openprocessing.org/visuals/?visualID=6753

Vec3D centeringForce = new Vec3D();
 
class Particle {
  Vec3D position, velocity, force;
  Vec3D localOffset;
  int red, green, blue;
  
  Particle() {
    resetPosition();
    velocity = new Vec3D();
    force = new Vec3D();
    localOffset = Vec3D.randomVector();
    
    float r = random(0.0, 1.0);
    float g = random(0.0, 1.0);
    float b = random(0.0, 1.0);
    float s = 255 * (r + g + b);
    red = (int) (r * s);
    green = (int) (g * s);
    blue = (int) (b * s);
  }
  
  void resetPosition() {
    position = Vec3D.randomVector();
    position.scaleSelf(random(rebirthRadius));
    if(particles.size() == 0)
      position.addSelf(new Vec3D(0, 0, 0));
    else
      position.addSelf(randomParticle().position);
  }
  
  void draw() {
    float distToEye = eye.distanceTo(position);
    float weight = 100.0 / distToEye;
    if (weight < 1) weight = 1;
    strokeWeight(weight);
    
    float fade = distToEye <= 1 ? 1.0 : pow(1 / distToEye, 0.1);
    if (fade < 0.2) fade = 0.2;
    float r = red * fade;
    float g = green * fade;
    float b = blue * fade;
    stroke(r, g, b);
    
    point(position.x, position.y, position.z);
  }
  
  void applyFlockingForce() {
    force.addSelf(
      noise(
        position.x / neighborhood + globalOffset.x + localOffset.x * independence,
        position.y / neighborhood,
        position.z / neighborhood)
        - .5,
      noise(
        position.x / neighborhood,
        position.y / neighborhood + globalOffset.y  + localOffset.y * independence,
        position.z / neighborhood)
        - .5,
      noise(
        position.x / neighborhood,
        position.y / neighborhood,
        position.z / neighborhood + globalOffset.z + localOffset.z * independence)
        - .5);
  }
  
  void applyViscosityForce() {
    force.addSelf(velocity.scale(-viscosity));
  }
  
  void applyCenteringForce() {
    centeringForce.set(position);
    centeringForce.subSelf(avg);
    float distanceToCenter = centeringForce.magnitude();
    centeringForce.normalize();
    centeringForce.scaleSelf(-distanceToCenter / (spread * spread));
    force.addSelf(centeringForce);
  }
  
  void update() {
    force.clear();
    applyFlockingForce();
    applyViscosityForce();
    applyCenteringForce();
    velocity.addSelf(force); // mass = 1
    position.addSelf(velocity.scale(speed / 40.0));
  }
}
