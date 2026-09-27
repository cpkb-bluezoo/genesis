import java.util.ArrayList;
import java.util.List;

@SuppressWarnings({"unchecked", "rawtypes"})
public class SuppressWarningsArrayTest {

    @SuppressWarnings({"unchecked", "rawtypes"})
    void useRawList() {
        List list = new ArrayList();
        list.add("ok");
    }

    public static void main(String[] args) {
        new SuppressWarningsArrayTest().useRawList();
        System.out.println("SuppressWarningsArrayTest OK");
    }
}
