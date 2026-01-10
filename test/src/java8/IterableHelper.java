import java.util.List;

public class IterableHelper {
    public static int countList(List list) {
        int count = 0;
        for (Object item : list) {
            count = count + 1;
        }
        return count;
    }
}

