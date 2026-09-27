import java.util.concurrent.Callable;
import javax.security.auth.Subject;

/** Generic inference for Subject.callAs(Subject, Callable<T>). */
public class SubjectCallAsTest {

    public static void main(String[] args) throws Exception {
        Subject subject = new Subject();
        String s = Subject.callAs(subject, new Callable<String>() {
            @Override
            public String call() {
                return "ok";
            }
        });
        if (!"ok".equals(s)) {
            throw new RuntimeException("expected ok");
        }
        System.out.println("SubjectCallAsTest OK");
    }
}
