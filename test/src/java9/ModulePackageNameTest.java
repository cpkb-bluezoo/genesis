public class ModulePackageNameTest {
	static String name(java.lang.module.ModuleDescriptor descriptor) {
		return descriptor.name();
	}

	public static void main(String[] args) {
		java.lang.module.ModuleDescriptor descriptor = java.lang.module.ModuleDescriptor.newModule("demo").build();
		java.lang.module.ModuleDescriptor.Builder builder = java.lang.module.ModuleDescriptor.newModule("other");
		if (!"demo".equals(name(descriptor)) || !"other".equals(builder.build().name())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
