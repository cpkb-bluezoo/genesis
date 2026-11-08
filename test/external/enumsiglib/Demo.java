/* Compiled by REAL javac against genesis's Color: Color.class is a Class<Color>, which only
 * converts to Class<? extends Enum<?>> when Color's superclass is the parameterised Enum<Color>. */
public class Demo {
    Class<? extends Enum<?>> type() { return Color.class; }
    Class<? extends Enum<?>> nested() { return Color.Nested.class; }
}
