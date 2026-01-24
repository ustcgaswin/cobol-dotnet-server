"""Exceptions for the code generation service."""


class CodegenException(Exception):
    """Base exception for code generation operations."""
    
    def __init__(self, message: str):
        self.message = message
        super().__init__(message)


class PrerequisiteError(CodegenException):
    """Phase A prerequisites not met (missing file_summaries.md, dependency_graph.md)."""
    
    def __init__(self, missing_files: list[str]):
        self.missing_files = missing_files
        message = f"Phase A prerequisites missing: {', '.join(missing_files)}. Run Phase A first."
        super().__init__(message)


class SolutionInitError(CodegenException):
    """Failed to initialize .NET solution structure."""
    
    def __init__(self, reason: str):
        message = f"Failed to initialize solution: {reason}"
        super().__init__(message)


class BuildError(CodegenException):
    """dotnet build command failed."""
    
    def __init__(self, exit_code: int, output: str):
        self.exit_code = exit_code
        self.output = output
        message = f"Build failed with exit code {exit_code}"
        super().__init__(message)


class FileWriteError(CodegenException):
    """Failed to write generated code file."""
    
    def __init__(self, filepath: str, reason: str):
        self.filepath = filepath
        message = f"Failed to write {filepath}: {reason}"
        super().__init__(message)


class ConversionError(CodegenException):
    """Error during component conversion."""
    
    def __init__(self, component: str, reason: str):
        self.component = component
        message = f"Failed to convert {component}: {reason}"
        super().__init__(message)


class GeneratedCodeNotFoundError(CodegenException):
    """Generated code folder does not exist (codegen not run yet)."""
    
    def __init__(self, project_id: str):
        self.project_id = project_id
        message = f"Generated code not found for project {project_id}. Run code generation first."
        super().__init__(message)


class InvalidFileIdError(CodegenException):
    """File ID is malformed (invalid Base64)."""
    
    def __init__(self, file_id: str):
        self.file_id = file_id
        message = f"Invalid file ID: {file_id}"
        super().__init__(message)


class GeneratedFileNotFoundError(CodegenException):
    """File ID is valid but file doesn't exist."""
    
    def __init__(self, file_id: str, path: str):
        self.file_id = file_id
        self.path = path
        message = f"File not found: {path}"
        super().__init__(message)

