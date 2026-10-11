#!/usr/bin/env python3
#
# model.py - the Spring Boot build, read without Gradle
#
# Copyright (C) 2026 Chris Burdess <dog@gnu.org>
#
# This file is part of genesis; see COPYING for the license terms.
#
# Reads the project layout and the dependency declarations of a Spring Boot
# source tree (the build.gradle files of core/, module/, starter/ and
# test-support/) and answers the questions a compile and test run needs:
# which projects exist, in which order they compile, and which classpath each
# source set has. The rules mirror what Gradle's java-library plugin and
# Spring Boot's own conventions do:
#
#   api, implementation, compileOnly, optional   main compile classpath
#   api, implementation, runtimeOnly, optional   main runtime classpath
#   testImplementation, testCompileOnly          test compile classpath, on top of
#   testRuntimeOnly                              the main one (without compileOnly)
#
# A project dependency exports the classes of that project and its `api`
# dependencies, transitively, to the compile classpath, and all but `optional`
# of its dependencies to the runtime classpath. External versions come from
# the Spring Boot BOM (the published pom) and the internal BOM (a Gradle DSL
# file).

import glob
import os
import re
import shutil
import sys
import zipfile

import maven

MAIN_COMPILE = ("api", "implementation", "compileOnly", "optional", "compileOnlyApi")
MAIN_RUNTIME = ("api", "implementation", "runtimeOnly", "optional")
TEST_COMPILE = ("testImplementation", "testCompileOnly")
TEST_RUNTIME = ("testImplementation", "testRuntimeOnly")
EXPORT = ("api", "compileOnlyApi")
FIX_COMPILE = ("testFixturesApi", "testFixturesImplementation", "testFixturesCompileOnly")
FIX_RUNTIME = ("testFixturesApi", "testFixturesImplementation", "testFixturesRuntimeOnly")
FIX_EXPORT = ("testFixturesApi",)

ROOTS = ("core", "module", "starter", "test-support", "configuration-metadata")


class GDep:
    """One dependency declaration of a build.gradle."""

    def __init__(self, config, kind, target, exclusions, transitive):
        self.config = config
        self.kind = kind            # "project", "fixtures", "external" or "gradle"
        self.target = target        # ":core:spring-boot" or "group:artifact[:version[:classifier]]"
        self.exclusions = exclusions
        self.transitive = transitive


# Set by the driver: check_digest(coord, sha256) compares a download with the lock file (and
# records it there when it is new).
check_digest = None

# Where the Gradle distribution is kept. The driver sets it inside the Spring Boot tree (whose
# .gitignore already ignores .gradle), so nothing of Gradle's lands anywhere else on the machine.
gradle_dir = None


def _wrapper_distribution_url(root):
    path = os.path.join(root, "gradle", "wrapper", "gradle-wrapper.properties")
    try:
        with open(path) as f:
            for line in f:
                if line.startswith("distributionUrl="):
                    return line.split("=", 1)[1].strip().replace("\\:", ":")
    except OSError:
        pass
    return None


def gradle_distribution_jars(root):
    """The jars of the Gradle distribution the source tree's own wrapper names (gradleApi(),
    gradleTestKit()), which are not on Maven Central.

    The distribution is downloaded once into `gradle_dir`, checked against the lock file and
    unpacked there, so a clean machine gets the same jars as any other. SB_GRADLE_HOME names an
    already unpacked one instead (not checked). Empty when the tree names none."""
    home = os.environ.get("SB_GRADLE_HOME")
    if not home:
        url = _wrapper_distribution_url(root)
        if not url:
            return []
        name = os.path.basename(url)
        base = gradle_dir or os.path.join(maven.CACHE, "gradle")
        zip_path = os.path.join(base, name)
        home = os.path.join(base, name[:-len(".zip")] if name.endswith(".zip") else name)
        done = home + ".unpacked"
        if not os.path.exists(done):
            os.makedirs(base, exist_ok=True)
            if not (os.path.exists(zip_path) and os.path.getsize(zip_path) > 0):
                maven.download(url, zip_path)
            digest = maven.sha256(zip_path)
            if check_digest:
                check_digest("gradle-dist:" + name, digest)
            shutil.rmtree(home, ignore_errors=True)
            with zipfile.ZipFile(zip_path) as z:
                for member in z.namelist():
                    target = os.path.realpath(os.path.join(base, member))
                    if not target.startswith(os.path.realpath(base) + os.sep):
                        raise RuntimeError("unsafe path in %s: %s" % (name, member))
                z.extractall(base)
            open(done, "wb").close()
        # the archive holds one directory, gradle-<version>
        inner = os.path.join(base, "gradle-" + name.split("-")[1])
        home = inner if os.path.isdir(os.path.join(inner, "lib")) else home
    ver = os.path.basename(home.rstrip("/"))
    jars = glob.glob(os.path.join(home, "lib", "*.jar")) + glob.glob(os.path.join(home, "lib", "plugins", "*.jar"))
    return [maven.LocalArtifact(os.path.basename(j)[:-4], ver, j) for j in sorted(jars)]


class Project:
    def __init__(self, path, directory):
        self.path = path
        self.dir = directory
        self.name = path.rsplit(":", 1)[1]
        self.deps = []

    def configs(self, names, kinds=None):
        return [d for d in self.deps if d.config in names and (kinds is None or d.kind in kinds)]

    def has_source(self, source_set):
        d = os.path.join(self.dir, "src", source_set, "java")
        for _, _, files in os.walk(d):
            if any(f.endswith(".java") for f in files):
                return True
        return False


def strip_comments(text):
    """The text without // and /* */ comments (string literals are kept as they are)."""
    out = []
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c in "\"'":
            j = i + 1
            while j < n and text[j] != c and text[j] != "\n":
                j += 2 if text[j] == "\\" else 1
            out.append(text[i:j + 1])
            i = j + 1
        elif text.startswith("//", i):
            j = text.find("\n", i)
            i = n if j < 0 else j
        elif text.startswith("/*", i):
            j = text.find("*/", i + 2)
            out.append("\n" * text.count("\n", i, n if j < 0 else j))
            i = n if j < 0 else j + 2
        else:
            out.append(c)
            i += 1
    return "".join(out)


def _block(text, start):
    """The text between the brace at text[start] and its match."""
    depth = 0
    i = start
    while i < len(text):
        c = text[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return text[start + 1:i], i + 1
        elif c in "\"'":
            j = text.find(c, i + 1)
            i = j if j > 0 else i
        i += 1
    return text[start + 1:], len(text)


_DECL = re.compile(r"""^[ \t]*(\w+)[ \t]*\(?[ \t]*(project|testFixtures|platform|enforcedPlatform|files|gradle\w+|["'])""",
                   re.M)


def parse_dependencies(text):
    """The GDep declarations of the dependencies { } blocks of a build.gradle."""
    out = []
    for m in re.finditer(r"^dependencies\s*\{", text, re.M):
        body, _ = _block(text, m.end() - 1)
        pos = 0
        while pos < len(body):
            d = _DECL.search(body, pos)
            if not d:
                break
            config, first = d.group(1), d.group(2)
            line_end = body.find("\n", d.start())
            line_end = len(body) if line_end < 0 else line_end
            line = body[d.start():line_end]
            # A closure opens on the declaration's own line.
            closure = ""
            brace = line.rfind("{")
            end = line_end
            if brace > 0 and line.rstrip().endswith("{"):
                closure, end = _block(body, d.start() + brace)
            pos = max(end, line_end + 1)
            if first in ("gradleTestKit", "gradleApi"):
                out.append(GDep(config, "gradle", first, [], True))
                continue
            if first in ("platform", "enforcedPlatform", "files") or first.startswith("gradle"):
                continue
            exclusions = [(g, a) for g, a in re.findall(
                r"exclude\s*\(?\s*group:\s*[\"']([^\"']+)[\"']\s*,\s*module:\s*[\"']([^\"']+)[\"']", closure)]
            exclusions += [(g, "*") for g in re.findall(
                r"exclude\s*\(?\s*group:\s*[\"']([^\"']+)[\"']\s*\)?\s*$", closure, re.M)]
            transitive = not re.search(r"transitive\s*=\s*false", closure)
            rel = d.end() - d.start()
            arg = line[rel - len(first):] if first in ("project", "testFixtures") else line[rel - 1:]
            if first == "project":
                m2 = re.match(r"project\(\s*(?:path:\s*)?[\"']([^\"']+)[\"']", arg)
                if m2:
                    out.append(GDep(config, "project", m2.group(1), exclusions, transitive))
            elif first == "testFixtures":
                m2 = re.match(r"testFixtures\(\s*project\(\s*(?:path:\s*)?[\"']([^\"']+)[\"']", arg)
                if m2:
                    out.append(GDep(config, "fixtures", m2.group(1), exclusions, transitive))
            else:
                m2 = re.match(r"[\"']([^\"']+)[\"']", arg)
                if m2:
                    out.append(GDep(config, "external", m2.group(1), exclusions, transitive))
    return out


def gradle_properties(root):
    props = {}
    path = os.path.join(root, "gradle.properties")
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#") and "=" in line:
                    k, v = line.split("=", 1)
                    props[k.strip()] = v.strip()
    return props


def load_projects(root):
    props = gradle_properties(root)
    projects = {}
    for top in ROOTS:
        base = os.path.join(root, top)
        if not os.path.isdir(base):
            continue
        for name in sorted(os.listdir(base)):
            d = os.path.join(base, name)
            gradle = os.path.join(d, "build.gradle")
            if not os.path.isfile(gradle):
                continue
            p = Project(":%s:%s" % (top, name), d)
            with open(gradle, encoding="utf-8") as f:
                p.deps = parse_dependencies(strip_comments(f.read()))
            for dep in p.deps:
                dep.target = re.sub(r"\$\{(\w+)\}|\$(\w+)",
                                    lambda m: props.get(m.group(1) or m.group(2), m.group(0)), dep.target)
            projects[p.path] = p
    return projects


# ----------------------------------------------------------------------------
# Versions

def internal_bom(root):
    """group:artifact -> version from the internal BOM, a Gradle DSL file."""
    path = os.path.join(root, "platform", "spring-boot-internal-dependencies", "build.gradle")
    versions = {}
    imports = []
    props = gradle_properties(root)
    with open(path, encoding="utf-8") as f:
        text = strip_comments(f.read())
    for lib in re.finditer(r"library\(\s*\"[^\"]+\"\s*,\s*\"([^\"]+)\"\s*\)\s*\{", text):
        version = re.sub(r"\$\{(\w+)\}", lambda m: props.get(m.group(1), m.group(0)), lib.group(1))
        body, _ = _block(text, lib.end() - 1)
        for grp in re.finditer(r"group\(\s*\"([^\"]+)\"\s*\)\s*\{", body):
            gbody, _ = _block(body, grp.end() - 1)
            for mods in re.finditer(r"modules\s*=\s*\[([^\]]*)\]", gbody):
                for a in re.findall(r"\"([^\"]+)\"", mods.group(1)):
                    versions[(grp.group(1), a)] = version
            for b in re.finditer(r"bom\(\s*\"([^\"]+)\"", gbody):
                imports.append((grp.group(1), b.group(1), version))
    # A bom("x") entry imports the pom of group:x:version, whose managed
    # versions then apply to everything it lists.
    for g, a, v in imports:
        pom = maven.Pom.get(g, a, v)
        for (mg, ma, _t, _c), dep in pom.managed.items():
            versions.setdefault((mg, ma), dep.version)
    return versions


def managed_versions(root, boot_version):
    """(group, artifact) -> version for every managed artifact."""
    versions = {}
    pom = maven.Pom.get("org.springframework.boot", "spring-boot-dependencies", boot_version)
    if not pom.exists:
        raise RuntimeError("spring-boot-dependencies %s not found" % boot_version)
    for (g, a, _t, _c), dep in pom.managed.items():
        versions.setdefault((g, a), dep.version)
    for k, v in internal_bom(root).items():
        versions[k] = v
    return versions


# ----------------------------------------------------------------------------
# Classpaths

class Build:
    def __init__(self, root, boot_version):
        self.root = root
        self.projects = load_projects(root)
        self.managed = managed_versions(root, boot_version)
        self._ext = {}

    # -- external artifacts

    def externals(self, project, configs, runtime):
        """Artifacts for the external declarations of `project` in `configs`."""
        roots = []
        for d in project.configs(configs, ("external",)):
            parts = d.target.split(":")
            roots.append(maven.Dep(parts[0], parts[1], parts[2] if len(parts) > 2 else None,
                                   exclusions=d.exclusions))
        arts = list(self._resolve_roots(roots, runtime))
        if project.configs(configs, ("gradle",)):
            arts += gradle_distribution_jars(self.root)
        return arts

    def _resolve_roots(self, roots, runtime):
        key = (tuple((r.group, r.artifact, r.version, r.exclusions) for r in roots), runtime)
        if key not in self._ext:
            self._ext[key] = maven.resolve(roots, self.managed, runtime) if roots else []
        return self._ext[key]

    # -- project dependencies

    def _projects_of(self, project, configs, fixtures=False):
        out = []
        for d in project.configs(configs, ("project", "fixtures")):
            if d.target in self.projects and (d.kind == "fixtures") == fixtures:
                out.append(self.projects[d.target])
        return out

    def exported(self, project, seen=None):
        """(projects, external artifacts) a consumer sees from `project`: the
        project itself and its api dependencies, transitively."""
        seen = seen if seen is not None else set()
        if project.path in seen:
            return [], []
        seen.add(project.path)
        projs = [project]
        arts = list(self.externals(project, EXPORT, False))
        for dep in self._projects_of(project, EXPORT):
            ps, ars = self.exported(dep, seen)
            projs += ps
            arts += ars
        return projs, arts

    def runtime_of(self, project, seen=None):
        """(projects, artifacts) on the runtime classpath of a consumer of `project`."""
        seen = seen if seen is not None else set()
        if project.path in seen:
            return [], []
        seen.add(project.path)
        projs = [project]
        arts = list(self.externals(project, ("api", "implementation", "runtimeOnly"), True))
        for dep in self._projects_of(project, ("api", "implementation", "runtimeOnly")):
            ps, ars = self.runtime_of(dep, seen)
            projs += ps
            arts += ars
        return projs, arts

    # -- the classpaths of a project's source sets

    def main_compile(self, p):
        projs, arts = [], list(self.externals(p, MAIN_COMPILE, False))
        for dep in self._projects_of(p, MAIN_COMPILE):
            ps, ars = self.exported(dep)
            projs += ps
            arts += ars
        return _dedupe(projs), _dedupe_art(arts)

    def main_runtime(self, p):
        projs, arts = [], list(self.externals(p, MAIN_RUNTIME, True))
        for dep in self._projects_of(p, MAIN_RUNTIME):
            ps, ars = self.runtime_of(dep)
            projs += ps
            arts += ars
        return _dedupe(projs), _dedupe_art(arts)

    def fixtures_compile(self, p):
        """testFixtures of a project: its main classes, its main compile classpath and
        its own testFixtures configurations. (projects, artifacts, other fixtures)"""
        projs, arts = self.main_compile(p)
        projs = [p] + projs
        arts = list(arts) + list(self.externals(p, FIX_COMPILE, False))
        fixtures = []
        for dep in self._projects_of(p, FIX_COMPILE):
            ps, ars = self.exported(dep)
            projs += ps
            arts += ars
        for dep in self._projects_of(p, FIX_COMPILE, fixtures=True):
            ps, ars, fx = self.fixtures_exported(dep)
            projs += ps
            arts += ars
            fixtures += fx
        return _dedupe(projs), _dedupe_art(arts), _dedupe(fixtures)

    def fixtures_exported(self, f, seen=None):
        """(projects, artifacts, fixtures) a consumer of f's testFixtures sees at compile
        time: f's main classes and api, and f's testFixturesApi."""
        seen = seen if seen is not None else set()
        if f.path in seen:
            return [], [], []
        seen.add(f.path)
        projs, arts = self.exported(f)
        arts = list(arts) + list(self.externals(f, FIX_EXPORT, False))
        fixtures = [f]
        for dep in self._projects_of(f, FIX_EXPORT):
            ps, ars = self.exported(dep)
            projs += ps
            arts += ars
        for dep in self._projects_of(f, FIX_EXPORT, fixtures=True):
            ps, ars, fx = self.fixtures_exported(dep, seen)
            projs += ps
            arts += ars
            fixtures += fx
        return projs, arts, fixtures

    def fixtures_runtime(self, f, seen=None):
        seen = seen if seen is not None else set()
        if f.path in seen:
            return [], [], []
        seen.add(f.path)
        projs, arts = self.runtime_of(f)
        arts = list(arts) + list(self.externals(f, FIX_RUNTIME, True))
        fixtures = [f]
        for dep in self._projects_of(f, FIX_RUNTIME):
            ps, ars = self.runtime_of(dep)
            projs += ps
            arts += ars
        for dep in self._projects_of(f, FIX_RUNTIME, fixtures=True):
            ps, ars, fx = self.fixtures_runtime(dep, seen)
            projs += ps
            arts += ars
            fixtures += fx
        return projs, arts, fixtures

    def test_compile(self, p):
        projs, arts = self.main_compile(p)
        projs = [p] + projs
        arts = arts + list(self.externals(p, ("testImplementation", "testCompileOnly"), False))
        for dep in self._projects_of(p, ("testImplementation", "testCompileOnly")):
            ps, ars = self.exported(dep)
            projs += ps
            arts += ars
        fixtures = []
        wanted = list(self._projects_of(p, ("testImplementation", "testCompileOnly"), fixtures=True))
        if p.has_source("testFixtures"):
            wanted.append(p)
        for dep in wanted:
            ps, ars, fx = self.fixtures_exported(dep)
            projs += ps
            arts += ars
            fixtures += fx
        return _dedupe(projs), _dedupe_art(arts), _dedupe(fixtures)

    def test_runtime(self, p):
        projs, arts = self.main_runtime(p)
        projs = [p] + projs
        arts = arts + list(self.externals(p, TEST_RUNTIME, True))
        for dep in self._projects_of(p, TEST_RUNTIME):
            ps, ars = self.runtime_of(dep)
            projs += ps
            arts += ars
        fixtures = []
        wanted = list(self._projects_of(p, TEST_RUNTIME, fixtures=True))
        if p.has_source("testFixtures"):
            wanted.append(p)
        for dep in wanted:
            ps, ars, fx = self.fixtures_runtime(dep)
            projs += ps
            arts += ars
            fixtures += fx
        launcher = maven.Dep("org.junit.platform", "junit-platform-launcher")
        arts += self._resolve_roots([launcher], True)
        return _dedupe(projs), _dedupe_art(arts), _dedupe(fixtures)

    # -- ordering

    def compile_order(self, paths):
        """The given project paths in dependency order (main source sets)."""
        order, state = [], {}

        def visit(path):
            if state.get(path):
                return
            state[path] = 1
            p = self.projects[path]
            for dep in self._projects_of(p, MAIN_COMPILE + ("runtimeOnly",)):
                visit(dep.path)
            order.append(path)

        for path in paths:
            visit(path)
        return [p for p in order if p in set(paths)]


def _dedupe(items):
    seen, out = set(), []
    for x in items:
        if x.path not in seen:
            seen.add(x.path)
            out.append(x)
    return out


def _dedupe_art(arts):
    seen, out = {}, []
    for a in arts:
        k = (a.group, a.artifact, a.classifier)
        if k not in seen:
            seen[k] = a
            out.append(a)
    return out


if __name__ == "__main__":
    # model.py <spring-boot-tree> <version> [project]  - print a project's classpaths
    build = Build(sys.argv[1], sys.argv[2])
    print(len(build.projects), "projects")
    if len(sys.argv) > 3:
        p = build.projects[sys.argv[3]]
        for name, (ps, ars) in (("main compile", build.main_compile(p)),
                                ("main runtime", build.main_runtime(p))):
            print(name, "projects:", [x.path for x in ps])
            print(name, "artifacts:", [a.coord for a in ars if a.has_jar])
