/**
 * Test bridge methods for covariant return types.
 * When a subclass overrides a method with a more specific return type,
 * the compiler must generate a bridge method with the parent's signature
 * that delegates to the child's implementation.
 */
class Animal {
    public Animal getSelf() {
        return this;
    }
}

class Dog extends Animal {
    // Covariant override - returns Dog instead of Animal
    @Override
    public Dog getSelf() {
        return this;
    }
    
    public void bark() {
        System.out.println("Woof!");
    }
}

public class BridgeMethodTest {
    public static void main(String[] args) {
        // Test 1: Direct call to Dog.getSelf() returns Dog
        Dog dog = new Dog();
        Dog sameDog = dog.getSelf();
        if (sameDog != dog) {
            throw new RuntimeException("Test 1 failed: getSelf() should return same instance");
        }
        
        // Test 2: Call through Animal reference - requires bridge method
        Animal animal = dog;
        Animal result = animal.getSelf();  // Calls bridge method, returns Animal
        if (result != dog) {
            throw new RuntimeException("Test 2 failed: getSelf() via Animal should return same instance");
        }
        
        // Test 3: Can cast result back to Dog
        Dog resultDog = (Dog) result;
        resultDog.bark();  // Should work
        
        System.out.println("BridgeMethodTest passed!");
    }
}

