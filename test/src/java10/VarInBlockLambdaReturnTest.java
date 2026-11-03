import java.util.List;
import java.util.stream.Collectors;

public class VarInBlockLambdaReturnTest {
    static class Row {
        final String name;

        Row(String name) {
            this.name = name;
        }
    }

    public static void main(String[] args) {
        List<String> out = List.of("a").stream().map(s -> {
            var row = new Row(s);
            return row;
        }).map(row -> row.name).collect(Collectors.toList());
        if (!out.equals(List.of("a"))) {
            throw new AssertionError(out);
        }
        System.out.println("VarInBlockLambdaReturnTest passed!");
    }
}
