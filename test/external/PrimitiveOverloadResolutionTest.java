import overloadlib.Checker;
import static overloadlib.Checker.check;

/*
 * Two related bugs in overload resolution for a CLASSFILE-loaded method
 * (compiled separately, referenced via -cp - see overloadlib/Checker.java
 * and run-tests.sh), both only ever exposed when more than one applicable
 * overload exists and neither is an exact-type match:
 *
 * 1. type_needs_boxing() (type.c) only recognized a target that was
 *    EXACTLY a primitive's own wrapper class (e.g. int -> Integer), never
 *    a broader reference type the wrapper is also assignable to (Object,
 *    Number, ...) - even though type_assignable() already handled those
 *    correctly. A caller scoring overload candidates (find_best_method_
 *    by_types() in semantic.c) used type_needs_boxing() specifically to
 *    give a boxing conversion LOWER priority than a primitive widening
 *    conversion (per JLS 15.12.2's phase ordering) - when it wrongly said
 *    "no boxing needed here", that lower-priority scoring never kicked
 *    in, so a boxing-requiring overload like check(String,Object,Object)
 *    could tie with (and, by candidate-list order, beat) a widening-only
 *    overload like check(String,long,long) for a char/byte argument
 *    pair.
 * 2. Once (1) was fixed, a SEPARATE, previously-masked bug surfaced:
 *    every primitive-to-primitive WIDENING conversion (char->long,
 *    int->double, ...) was scored in one flat bucket regardless of how
 *    far it widens, so check(String,long,long) and check(String,double,
 *    double) - both applicable via widening for an int argument pair -
 *    tied too, even though long is strictly more specific than double
 *    (JLS 15.12.2.5: long converts to double, but not vice versa).
 *
 * Confirmed against gumdrop's own Base64DecoderTest, whose
 * "assertEquals(msg, 'H', someByte)" and "assertEquals(msg, 1,
 * dst.position())" depend on JUnit's real assertEquals(String,long,long)
 * winning over both its Object,Object and (deprecated, always-failing)
 * double,double siblings - this test mirrors that exact overload shape.
 */
public class PrimitiveOverloadResolutionTest {
    public static void main(String[] args) {
        byte b = 72;
        check("char/byte pair", 'H', b);
        if (!"long".equals(Checker.lastCalled)) {
            throw new RuntimeException(
                "expected check(String,char,byte) to resolve to the long,long overload, got " +
                Checker.lastCalled);
        }

        int i = 1;
        check("int/int pair", i, i);
        if (!"long".equals(Checker.lastCalled)) {
            throw new RuntimeException(
                "expected check(String,int,int) to resolve to the long,long overload (not double,double), got " +
                Checker.lastCalled);
        }

        System.out.println("PrimitiveOverloadResolutionTest passed!");
    }
}
