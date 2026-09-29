/**
 * Bug: an array-typed argument passed directly to a varargs parameter
 * (no wrapping needed - the array itself becomes the varargs array)
 * was only recognized as such when its element type EXACTLY matched
 * the varargs parameter's element type (or the parameter's element
 * type was exactly java.lang.Object) - never when the argument's
 * element type merely IMPLEMENTS/EXTENDS the parameter's element type
 * (ordinary array covariance, e.g. "Dog[]" passed to
 * "Animal... animals" where Dog implements Animal). Without that,
 * codegen fell back to treating the single array argument as ONE
 * vararg element to wrap into a new 1-element array - storing the
 * whole array into a slot that expects a single element of the
 * (unrelated, from the array's point of view) parameter type:
 * "ArrayStoreException: [LDog;" the moment the call actually ran.
 * Confirmed against gumdrop's own BasicFTPFileSystem.openForWriting(),
 * whose "FileChannel.open(filePath, options)" passes a
 * StandardOpenOption[] where java.nio.file.channels.FileChannel.open's
 * own "OpenOption... options" is declared (StandardOpenOption
 * implements OpenOption).
 */
public class ArrayCovariantVarargsPassthroughVerifyTest {
    interface Animal {
        String name();
    }

    static class Dog implements Animal {
        public String name() {
            return "Dog";
        }
    }

    static int countAnimals(Animal... animals) {
        return animals.length;
    }

    static int countDogs(Dog[] dogs) {
        return countAnimals(dogs);
    }

    public static void main(String[] args) {
        Dog[] dogs = new Dog[]{new Dog(), new Dog(), new Dog()};
        int n = countDogs(dogs);
        if (n != 3) {
            throw new RuntimeException("expected 3, got " + n);
        }
    }
}
