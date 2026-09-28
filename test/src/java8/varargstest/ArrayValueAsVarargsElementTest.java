package varargstest;

/*
 * Regression test: passing an array value (byte[]) as a single element to
 * a varargs parameter of a different, unrelated array type (Object...)
 * used to be wrongly rejected. byte[] is not assignable to Object[] (so
 * this isn't a "pass the whole array directly" call), but the byte[]
 * value itself is a valid single Object element - javac accepts this,
 * wrapping it as one element of the varargs array. See genesis history
 * for details (search "Individual vararg - compare against element type"
 * in semantic.c, both in the candidate-scoring pass and the post-selection
 * argument check).
 */
public class ArrayValueAsVarargsElementTest {
    Object encode(Object... args) {
        return args;
    }

    void run() {
        byte[] data = new byte[] { 1, 2, 3 };
        Object result = encode("SET", "key", data);
    }
}
