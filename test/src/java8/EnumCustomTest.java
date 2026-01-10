/**
 * Test enum with custom fields and methods.
 */
public class EnumCustomTest {
    
    public enum Planet {
        MERCURY(3.303e+23, 2.4397e6),
        VENUS(4.869e+24, 6.0518e6),
        EARTH(5.976e+24, 6.37814e6),
        MARS(6.421e+23, 3.3972e6);
        
        private final double mass;   // in kilograms
        private final double radius; // in meters
        
        // Constructor with parameters
        Planet(double mass, double radius) {
            this.mass = mass;
            this.radius = radius;
        }
        
        // Custom method
        public double getMass() {
            return mass;
        }
        
        public double getRadius() {
            return radius;
        }
        
        // Calculate surface gravity
        public double surfaceGravity() {
            double G = 6.67300E-11;
            return G * mass / (radius * radius);
        }
    }
    
    public static void main(String[] args) {
        // Test custom fields via getter
        Planet earth = Planet.EARTH;
        double mass = earth.getMass();
        double radius = earth.getRadius();
        System.out.println("Earth mass: " + mass);
        System.out.println("Earth radius: " + radius);
        
        // Test custom method
        double gravity = earth.surfaceGravity();
        System.out.println("Earth surface gravity: " + gravity);
        
        // Verify gravity is approximately 9.8 m/s^2 (allow 5% error)
        if (gravity < 9.3 || gravity > 10.3) {
            throw new RuntimeException("Earth gravity calculation wrong: " + gravity);
        }
        
        // Test all planets
        Planet[] planets = Planet.values();
        for (int i = 0; i < planets.length; i++) {
            Planet p = planets[i];
            System.out.println(p.name() + ": mass=" + p.getMass() + 
                             ", radius=" + p.getRadius() + 
                             ", gravity=" + p.surfaceGravity());
        }
        
        System.out.println("EnumCustomTest passed!");
    }
}
