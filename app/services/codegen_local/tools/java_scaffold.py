import os
from pathlib import Path
from loguru import logger

def scaffold_java_solution(output_path: Path, source_path: Path, project_name: str) -> str:
    """Scaffold a Java Spring Boot project structure.
    
    Args:
        output_path: Path to the output directory
        source_path: Path to the source directory (for JCL scanning)
        project_name: Name of the project
        
    Returns:
        Summary of created files
    """
    logger.info(f"Scaffolding Java solution for {project_name} at {output_path}")
    
    if (output_path / "pom.xml").exists():
        return "Java solution already initialized (pom.xml found). Skipping scaffold."
        
    output_path.mkdir(parents=True, exist_ok=True)
    
    # 1. Create Folder Structure
    # Package: com.example.migration
    base_package_path = output_path / "src" / "main" / "java" / "com" / "example" / "migration"
    test_package_path = output_path / "src" / "test" / "java" / "com" / "example" / "migration"
    
    dirs = [
        base_package_path / "core" / "entities",
        base_package_path / "core" / "services",
        base_package_path / "core" / "exceptions",
        base_package_path / "core" / "config",
        base_package_path / "infrastructure" / "repositories",
        base_package_path / "infrastructure" / "io",
        base_package_path / "worker" / "jobs",
        test_package_path / "core",
        output_path / "src" / "main" / "resources",
        output_path / "scripts" / "jobs",
        output_path / "data" / "input",
        output_path / "data" / "output",
        output_path / ".mvn" / "wrapper",
    ]
    
    for d in dirs:
        d.mkdir(parents=True, exist_ok=True)
        
    # 2. Create pom.xml
    pom_content = f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>3.2.0</version>
        <relativePath/> <!-- lookup parent from repository -->
    </parent>
    <groupId>com.example</groupId>
    <artifactId>{project_name.lower().replace(' ', '-')}</artifactId>
    <version>0.0.1-SNAPSHOT</version>
    <name>{project_name}</name>
    <description>Mainframe to Java Migration Project</description>
    <properties>
        <java.version>17</java.version>
    </properties>
    <dependencies>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter</artifactId>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
        </dependency>
        <dependency>
            <groupId>com.h2database</groupId>
            <artifactId>h2</artifactId>
            <scope>runtime</scope>
        </dependency>
        <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <optional>true</optional>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
                <configuration>
                    <excludes>
                        <exclude>
                            <groupId>org.projectlombok</groupId>
                            <artifactId>lombok</artifactId>
                        </exclude>
                    </excludes>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
"""
    (output_path / "pom.xml").write_text(pom_content, encoding="utf-8")
    
    # 3. JCL Discovery & Worker Job Scaffolding
    jcl_jobs = []
    if source_path.exists():
        for jcl_file in sorted(source_path.rglob("*.jcl")):
            if not jcl_file.is_file():
                continue
            class_name = jcl_file.stem.capitalize()   # SETLJOB -> Setljob
            script_name = jcl_file.stem.lower()       # SETLJOB -> setljob
            jcl_jobs.append((class_name, script_name, jcl_file.name))
            
    # Stub IJob Interface (Function Interface)
    ijob_path = base_package_path / "worker" / "jobs" / "BatchJob.java"
    ijob_content = """package com.example.migration.worker.jobs;

public interface BatchJob {
    int execute(String[] args);
}
"""
    ijob_path.write_text(ijob_content, encoding="utf-8")

    scaffolded_jobs = []
    
    # Generate Job Stubs
    for class_name, script_name, jcl_filename in jcl_jobs:
        # Java Class Stub
        job_file = base_package_path / "worker" / "jobs" / f"{class_name}.java"
        if not job_file.exists():
            job_java = f"""// Source: {jcl_filename}
package com.example.migration.worker.jobs;

import org.springframework.stereotype.Component;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component("{class_name}")
public class {class_name} implements BatchJob {{

    @Override
    public int execute(String[] args) {{
        // TODO: Implement job steps from {jcl_filename}
        log.error("Job {class_name} not yet implemented.");
        throw new UnsupportedOperationException("Job {class_name} logic missing.");
    }}
}}
"""
            job_file.write_text(job_java, encoding="utf-8")
            
        # PS1 Script Skeleton
        ps1_file = output_path / "scripts" / "jobs" / f"run-{script_name}.ps1"
        if not ps1_file.exists():
            jar_name = f"{project_name.lower().replace(' ', '-')}-0.0.1-SNAPSHOT.jar"
            ps1 = f"""# run-{script_name}.ps1 — Converted from: {jcl_filename}
# Usage: ./run-{script_name}.ps1 [args]

$ErrorActionPreference = "Stop"
$script:MaxRC = 0

# 1. Verification
if (-not (Test-Path "target/{jar_name}")) {{
    Write-Error "Build artifact target/{jar_name} not found. Run './mvnw package -DskipTests' first."
}}

Write-Host "=== Executing Job: {class_name} ==="

# 2. Execution
# We pass all arguments from PS1 ($args) to the Java application
# The Java App uses CommandLineRunner to route '--job {class_name}' to the right Bean.
java -jar "target/{jar_name}" --job "{class_name}" $args

# 3. Return Code Handling
if ($LASTEXITCODE -ne 0) {{
    Write-Error "Job {class_name} failed with RC $LASTEXITCODE"
}}
exit $LASTEXITCODE
"""
            ps1_file.write_text(ps1, encoding="utf-8")
            
        scaffolded_jobs.append(f"{class_name} ({jcl_filename})")

    # 4. Create Application.java with Job Routing Logic
    # We need a way to run specific jobs via CLI args, similar to .NET
    app_class = """package com.example.migration;

import com.example.migration.worker.jobs.BatchJob;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;

import java.util.Arrays;

@SpringBootApplication
public class Application {

    public static void main(String[] args) {
        System.exit(SpringApplication.exit(SpringApplication.run(Application.class, args)));
    }

    @Bean
    public CommandLineRunner run(ApplicationContext ctx) {
        return args -> {
            if (args.length == 0) return; // Allow normal boot for tests/web

            String jobName = null;
            for (int i = 0; i < args.length; i++) {
                if ("--job".equals(args[i]) && i + 1 < args.length) {
                    jobName = args[i + 1];
                    break;
                }
            }

            if (jobName != null) {
                try {
                    BatchJob job = ctx.getBean(jobName, BatchJob.class);
                    int rc = job.execute(args);
                    System.exit(rc);
                } catch (Exception e) {
                    System.err.println("Job execution failed: " + e.getMessage());
                    System.exit(1);
                }
            }
        };
    }
}
"""
    (base_package_path / "Application.java").write_text(app_class, encoding="utf-8")
    
    # 5. Create application.properties
    props = """spring.application.name=migration-app
logging.level.root=INFO
logging.level.com.example.migration=DEBUG
spring.datasource.url=jdbc:h2:mem:testdb
spring.datasource.driverClassName=org.h2.Driver
spring.datasource.username=sa
spring.datasource.password=password
spring.jpa.database-platform=org.hibernate.dialect.H2Dialect
spring.h2.console.enabled=true
"""
    (output_path / "src" / "main" / "resources" / "application.properties").write_text(props, encoding="utf-8")
    
    # 6. Create .gitignore
    gitignore = """
.mvn/wrapper/maven-wrapper.jar
**/target/
!.mvn/wrapper/maven-wrapper.properties
!mvnw
!mvnw.cmd
**.idea
**.vscode
**.class
*.log
*.iml
"""
    (output_path / ".gitignore").write_text(gitignore, encoding="utf-8")
    
    # Summary
    summary = [
        f"Created Java solution structure at {output_path}",
        f"Pre-scaffolded {len(scaffolded_jobs)} Worker Jobs + PS1 scripts:",
    ]
    for j in scaffolded_jobs:
        summary.append(f"  - {j}")
        
    return "\n".join(summary)
