/**
 * Bug: a multi-catch handler's own STACKMAP FRAME (recorded at the
 * handler's entry point) deliberately used a blanket "java/lang/Throwable"
 * for the exception pushed onto the stack there - a safe type valid for
 * any alternative - while the LOCAL VARIABLE's own declared type (used
 * for method-call resolution elsewhere) used the properly-computed LUB
 * (e.g. Exception). Per the real JVM verifier's rules, the type recorded
 * at a handler's own entry frame becomes the VERIFIED type of whatever
 * local an immediately-following ASTORE stores it into, from that point
 * forward - genesis's own internal bookkeeping recording ANY LATER frame
 * in the same catch body using the LUB (Exception) instead therefore
 * created a guaranteed, unavoidable mismatch the moment a later frame
 * existed - e.g. a NESTED try/catch inside the multi-catch body, whose
 * own handler entry needs a frame that also describes this outer local.
 * VerifyError: "Stack map does not match the one at exception handler
 * ... Type 'Throwable' ... not assignable to 'Exception'". Confirmed
 * against gumdrop's own MessageIndex.save(), whose outer
 * "catch (IOException | RuntimeException e)" wraps a try-with-resources
 * followed by its own nested
 * "try { Files.deleteIfExists(tempPath); } catch (IOException
 * deleteFailed) { e.addSuppressed(deleteFailed); }" - exactly this shape.
 */
public class MultiCatchNestedHandlerFrameVerifyTest {
    static void save(java.nio.file.Path indexPath) throws java.io.IOException {
        java.nio.file.Path parent = indexPath.getParent();
        String prefix = indexPath.getFileName().toString() + "-";
        java.nio.file.Path tempPath = java.nio.file.Files.createTempFile(parent, prefix, ".tmp");

        try {
            try (java.io.DataOutputStream out = new java.io.DataOutputStream(
                    new java.io.BufferedOutputStream(java.nio.file.Files.newOutputStream(tempPath)))) {
                out.writeInt(42);
            }
            java.nio.file.Files.move(tempPath, indexPath,
                    java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        } catch (java.io.IOException | RuntimeException e) {
            try {
                java.nio.file.Files.deleteIfExists(tempPath);
            } catch (java.io.IOException deleteFailed) {
                e.addSuppressed(deleteFailed);
            }
            throw e;
        }
    }

    public static void main(String[] args) throws Exception {
        java.nio.file.Path dir = java.nio.file.Files.createTempDirectory("mcnhft");
        java.nio.file.Path idx = dir.resolve("index.dat");
        save(idx);
        if (java.nio.file.Files.size(idx) != 4) {
            throw new RuntimeException("expected 4 bytes, got " + java.nio.file.Files.size(idx));
        }
    }
}
