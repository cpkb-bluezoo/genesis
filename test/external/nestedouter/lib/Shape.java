package nestedouter.lib;

/* A class whose nested class extends it. */
public class Shape {

    public String name() {
        return "shape";
    }

    public static class Circle extends Shape {
        public int corners() {
            return 0;
        }
    }
}
