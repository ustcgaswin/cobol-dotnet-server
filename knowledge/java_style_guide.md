# Java Code Style Guide (Spring Boot)

## Naming
- Classes: `PascalCase`
- Methods/Variables: `camelCase`
- Constants: `UPPER_SNAKE_CASE`

## Structure
- Package: `com.example.migration`
- Components: `@Service`, `@Repository`, `@Component`

## Dependency Injection
- Use **Constructor Injection** with `final` fields.
```java
@Service
@RequiredArgsConstructor
public class MyService {
    private final MyRepository repository;
}
```

## Logging
- Use SLF4J (via Lombok `@Slf4j`)
```java
log.info("Processing record {}", id);
```
