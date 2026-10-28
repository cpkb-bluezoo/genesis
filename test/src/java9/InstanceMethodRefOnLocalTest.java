import java.util.ArrayList;
import java.util.List;

public class InstanceMethodRefOnLocalTest {
    interface FileSet {
        List<String> names();
    }

    interface Session {
        void associateFile(String name);
    }

    static void registerFiles(Session queueSession, FileSet token) {
        if (token.names() != null) {
            token.names().forEach(queueSession::associateFile);
        }
    }

    public static void main(String[] args) {
        Session session = name -> {};
        FileSet files = () -> {
            List<String> list = new ArrayList<>();
            list.add("a.dat");
            return list;
        };
        registerFiles(session, files);
        System.out.println("InstanceMethodRefOnLocalTest passed!");
    }
}
