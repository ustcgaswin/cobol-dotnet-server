import os
from pathlib import Path
from loguru import logger

def scaffold_java_solution(output_path: Path, source_path: Path, project_name: str) -> str:
    """Scaffold a Java Spring Boot project structure.
    
    Args:
        output_path: Path to the output directory
        source_path: Path to the source directory (for JCL scanning if needed)
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
    
    # 3. Create Application.java
    app_class = """package com.example.migration;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

}
"""
    (base_package_path / "Application.java").write_text(app_class, encoding="utf-8")
    
    # 4. Create application.properties
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
    
    # 5. Create .gitignore
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
    
    # 6. Stub mvnw scripts (Text only for now, binary JAR missing)
    # Ideally we'd copy these from a template, but for now we write placeholders 
    # instructing to install maven or use existing wrapper if available.
    # Realistically, downloading the wrapper jar via python is possible but larger scope.
    
    return f"Created Java solution structure at {output_path} with pom.xml, src directories, and default config."
