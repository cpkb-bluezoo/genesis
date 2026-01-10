/**
 * Test enum with constructor parameters.
 */
public class EnumConstructorTest {
    
    public enum Planet {
        MERCURY(3.303e+23, 2.4397e6),
        EARTH(5.976e+24, 6.37814e6);
        
        private final double mass;
        private final double radius;
        
        Planet(double mass, double radius) {
            this.mass = mass;
            this.radius = radius;
        }
        
        public double getMass() {
            return mass;
        }
        
        public double getRadius() {
            return radius;
        }
    }
    
    public static void main(String[] args) {
        Planet earth = Planet.EARTH;
        System.out.println("Earth mass: " + earth.getMass());
        System.out.println("Earth radius: " + earth.getRadius());
        
        // Verify values are set
        if (earth.getMass() < 5e24 || earth.getMass() > 6e24) {
            throw new RuntimeException("Earth mass incorrect");
        }
        
        System.out.println("EnumConstructorTest passed!");
    }
}
