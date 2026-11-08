/*
 * A @FunctionalInterface whose single abstract method is inherited has its
 * signature built to compare with the others: a signature longer than the
 * buffer it was built in (here about 1100 characters) must not overflow it.
 */
class LongParameterTypeNumber00 {}
class LongParameterTypeNumber01 {}
class LongParameterTypeNumber02 {}
class LongParameterTypeNumber03 {}
class LongParameterTypeNumber04 {}
class LongParameterTypeNumber05 {}
class LongParameterTypeNumber06 {}
class LongParameterTypeNumber07 {}
class LongParameterTypeNumber08 {}
class LongParameterTypeNumber09 {}
class LongParameterTypeNumber10 {}
class LongParameterTypeNumber11 {}
class LongParameterTypeNumber12 {}
class LongParameterTypeNumber13 {}
class LongParameterTypeNumber14 {}
class LongParameterTypeNumber15 {}
class LongParameterTypeNumber16 {}
class LongParameterTypeNumber17 {}
class LongParameterTypeNumber18 {}
class LongParameterTypeNumber19 {}
class LongParameterTypeNumber20 {}
class LongParameterTypeNumber21 {}
class LongParameterTypeNumber22 {}
class LongParameterTypeNumber23 {}
class LongParameterTypeNumber24 {}
class LongParameterTypeNumber25 {}
class LongParameterTypeNumber26 {}
class LongParameterTypeNumber27 {}
class LongParameterTypeNumber28 {}
class LongParameterTypeNumber29 {}
class LongParameterTypeNumber30 {}
class LongParameterTypeNumber31 {}
class LongParameterTypeNumber32 {}
class LongParameterTypeNumber33 {}
class LongParameterTypeNumber34 {}
class LongParameterTypeNumber35 {}
class LongParameterTypeNumber36 {}
class LongParameterTypeNumber37 {}
class LongParameterTypeNumber38 {}
class LongParameterTypeNumber39 {}

interface LongSignatureBase {
    void run(
        LongParameterTypeNumber00 p0,
        LongParameterTypeNumber01 p1,
        LongParameterTypeNumber02 p2,
        LongParameterTypeNumber03 p3,
        LongParameterTypeNumber04 p4,
        LongParameterTypeNumber05 p5,
        LongParameterTypeNumber06 p6,
        LongParameterTypeNumber07 p7,
        LongParameterTypeNumber08 p8,
        LongParameterTypeNumber09 p9,
        LongParameterTypeNumber10 p10,
        LongParameterTypeNumber11 p11,
        LongParameterTypeNumber12 p12,
        LongParameterTypeNumber13 p13,
        LongParameterTypeNumber14 p14,
        LongParameterTypeNumber15 p15,
        LongParameterTypeNumber16 p16,
        LongParameterTypeNumber17 p17,
        LongParameterTypeNumber18 p18,
        LongParameterTypeNumber19 p19,
        LongParameterTypeNumber20 p20,
        LongParameterTypeNumber21 p21,
        LongParameterTypeNumber22 p22,
        LongParameterTypeNumber23 p23,
        LongParameterTypeNumber24 p24,
        LongParameterTypeNumber25 p25,
        LongParameterTypeNumber26 p26,
        LongParameterTypeNumber27 p27,
        LongParameterTypeNumber28 p28,
        LongParameterTypeNumber29 p29,
        LongParameterTypeNumber30 p30,
        LongParameterTypeNumber31 p31,
        LongParameterTypeNumber32 p32,
        LongParameterTypeNumber33 p33,
        LongParameterTypeNumber34 p34,
        LongParameterTypeNumber35 p35,
        LongParameterTypeNumber36 p36,
        LongParameterTypeNumber37 p37,
        LongParameterTypeNumber38 p38,
        LongParameterTypeNumber39 p39);
}

@FunctionalInterface
interface LongSignatureFunction extends LongSignatureBase {
}

public class FunctionalInterfaceLongSignatureTest {
    public static void main(String[] args) throws Exception {
        if (LongSignatureFunction.class.getMethod("run", paramTypes()) == null) {
            System.out.println("FAILED");
            System.exit(1);
        }
    }

    static Class<?>[] paramTypes() throws Exception {
        java.lang.reflect.Method[] methods = LongSignatureBase.class.getDeclaredMethods();
        return methods[0].getParameterTypes();
    }
}
