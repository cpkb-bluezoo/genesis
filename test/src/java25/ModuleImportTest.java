import module java.base;

public class ModuleImportTest {
    public static void main(String[] args) {
        List<String> list = List.of("a", "b");
        if (list.size() != 2) {
            throw new RuntimeException("list size=" + list.size());
        }
        Map<String, Integer> map = Map.of("x", 1);
        if (map.get("x") != 1) {
            throw new RuntimeException("map get failed");
        }
        System.out.println("All module import tests passed!");
    }
}
