public class TargetLambda {
    public static void main(String[] args) {
        Runnable r = () -> System.out.println("lambda");
        r.run();
    }
}
