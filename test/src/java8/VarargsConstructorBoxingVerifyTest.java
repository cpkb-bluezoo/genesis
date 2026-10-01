import java.util.Arrays;

/*
 * GitHub issue #3: calling a varargs CONSTRUCTOR whose varargs element
 * type is a reference type (Object..., or a generic <T> Ctor(T... args))
 * with primitive literal arguments produced a VerifyError ("Bad type on
 * operand stack ... Type integer ... is not assignable to
 * 'java/lang/Object'") at runtime - the constructor-call codegen path
 * used to carry its own, separate "store each vararg" loop that stored
 * every element with a plain, unconditional AASTORE and no boxing step at
 * all, unlike the varargs METHOD-call codegen path (which already boxed
 * correctly).
 *
 * By the time this test was written, codegen_varargs_tail() (codegen_expr.c)
 * had already been unified into one function shared by method calls,
 * constructor calls and enum constant construction - its own doc comment
 * says the constructor path "used to carry its own, much weaker copy of
 * this logic" - so this exact repro already passes. No dedicated
 * regression test existed for it though (the existing
 * VarargsConstructorDescriptorVerifyTest/EnumVarargsConstructorVerifyTest
 * cover other varargs-constructor gaps, not boxing specifically), so this
 * closes that gap and guards against a future regression.
 */
public class VarargsConstructorBoxingVerifyTest {
    static class Box {
        final Object[] items;
        Box(Object... items) {
            this.items = items;
        }
    }

    static class GenericBox<T> {
        final T[] items;
        @SafeVarargs
        GenericBox(T... items) {
            this.items = items;
        }
    }

    public static void main(String[] args) {
        Box box = new Box(1, 2, 3, "four");
        String result = Arrays.toString(box.items);
        if (!"[1, 2, 3, four]".equals(result)) {
            throw new RuntimeException("expected [1, 2, 3, four] but got " + result);
        }

        GenericBox<Object> generic = new GenericBox<Object>(1, 2, 3, "four");
        String genericResult = Arrays.toString(generic.items);
        if (!"[1, 2, 3, four]".equals(genericResult)) {
            throw new RuntimeException("expected [1, 2, 3, four] but got " + genericResult);
        }

        System.out.println("VarargsConstructorBoxingVerifyTest passed!");
    }
}
