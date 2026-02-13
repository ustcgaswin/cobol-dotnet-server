"""Solution management tools for the code generation agent.

Handles .NET solution initialization and file writing.
"""

import uuid
import shutil
import os
from pathlib import Path

from langchain.tools import tool
from loguru import logger


def create_solution_tools(project_id: str, output_path: str, source_path: str) -> list:
    """Create solution management tools.
    
    Args:
        project_id: Project ID for scoping file operations
        output_path: Absolute path to the output directory for generated code
        source_path: Absolute path to the source directory (for listing components)
        
    Returns:
        List of LangChain tools
    """
    output_dir = Path(output_path)
    source_dir = Path(source_path)
    
    @tool("list_batch_components")
    def list_batch_components() -> str:
        """List all JCL Jobs and Procedures that need to be converted.
        
        Scans the source directory for .jcl (Jobs) and .proc/.prc (Procedures).
        
        Returns:
            Formatted list of batch components found in source.
        """
        try:
            if not source_dir.exists():
                return f"Error: Source directory not found at {source_dir}"
            
            jobs = []
            procs = []
            
            # Case-insensitive extensions
            for item in sorted(source_dir.rglob("*")):
                if not item.is_file():
                    continue
                
                ext = item.suffix.lower()
                if ext == ".jcl":
                    jobs.append(item.name)
                elif ext in (".proc", ".prc"):
                    procs.append(item.name)
            
            if not jobs and not procs:
                return "No batch components (JCL/PROC) found in source directory."
                
            report = f"Found {len(jobs)} Jobs and {len(procs)} Procedures:\n"
            
            if jobs:
                report += "\nJOBS (Require .ps1 script AND Worker/Jobs code):\n"
                for j in jobs:
                    report += f"- {j}\n"
            
            if procs:
                report += "\nPROCEDURES (May be converted to shared code or scripts):\n"
                for p in procs:
                    report += f"- {p}\n"
                    
            return report
            
        except Exception as e:
            logger.error(f"list_batch_components error: {e}")
            return f"Error listing batch components: {e}"
    
    @tool("initialize_solution")
    def initialize_solution(solution_name: str) -> str:
        """Initialize a .NET solution with the standard project structure.
        
        Creates:
        - ConvertedBatch.sln
        - src/Core/Core.csproj
        - src/Infrastructure/Infrastructure.csproj
        - src/Worker/Worker.csproj
        - Folder structure for Entities, Services, etc.
        
        Call this ONCE at the start before writing any code files.
        
        Args:
            solution_name: Name for the solution (e.g., "ConvertedBatch")
            
        Returns:
            Success message with created structure
        """
        try:
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Generate GUIDs for projects
            core_guid = str(uuid.uuid4()).upper()
            infra_guid = str(uuid.uuid4()).upper()
            worker_guid = str(uuid.uuid4()).upper()
            tests_guid = str(uuid.uuid4()).upper()
            
            # Create .sln file
            sln_content = f'''Microsoft Visual Studio Solution File, Format Version 12.00
# Visual Studio Version 17
VisualStudioVersion = 17.0.31903.59
MinimumVisualStudioVersion = 10.0.40219.1
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Core", "src\\Core\\Core.csproj", "{{{core_guid}}}"
EndProject
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Infrastructure", "src\\Infrastructure\\Infrastructure.csproj", "{{{infra_guid}}}"
EndProject
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Worker", "src\\Worker\\Worker.csproj", "{{{worker_guid}}}"
EndProject
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Tests", "tests\\Tests.csproj", "{{{tests_guid}}}"
EndProject
Global
	GlobalSection(SolutionConfigurationPlatforms) = preSolution
		Debug|Any CPU = Debug|Any CPU
		Release|Any CPU = Release|Any CPU
	EndGlobalSection
	GlobalSection(ProjectConfigurationPlatforms) = postSolution
		{{{core_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{core_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{core_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{core_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
		{{{infra_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{infra_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{infra_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{infra_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
		{{{worker_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{worker_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{worker_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{worker_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
		{{{tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
	EndGlobalSection
EndGlobal
'''
            (output_dir / f"{solution_name}.sln").write_text(sln_content)
            
            # Create Core.csproj
            core_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Core</RootNamespace>
  </PropertyGroup>
</Project>
'''
            core_path = output_dir / "src" / "Core"
            core_path.mkdir(parents=True, exist_ok=True)
            (core_path / "Core.csproj").write_text(core_csproj)
            
            # Create Core subdirectories
            (core_path / "Entities").mkdir(exist_ok=True)
            (core_path / "Services").mkdir(exist_ok=True)
            (core_path / "Interfaces").mkdir(exist_ok=True)
            (core_path / "Enums").mkdir(exist_ok=True)
            (core_path / "Configuration").mkdir(exist_ok=True)
            
            # Create Infrastructure.csproj
            infra_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Infrastructure</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\Core\\Core.csproj" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Microsoft.EntityFrameworkCore.SqlServer" Version="8.0.0" />
    <PackageReference Include="Serilog" Version="3.1.1" />
    <PackageReference Include="Serilog.Sinks.Console" Version="5.0.1" />
    <PackageReference Include="Serilog.Sinks.File" Version="5.0.0" />
  </ItemGroup>
</Project>
'''
            infra_path = output_dir / "src" / "Infrastructure"
            infra_path.mkdir(parents=True, exist_ok=True)
            (infra_path / "Infrastructure.csproj").write_text(infra_csproj)
            
            # Create Infrastructure subdirectories
            (infra_path / "Data").mkdir(exist_ok=True)
            (infra_path / "Storage").mkdir(exist_ok=True)
            (infra_path / "Repositories").mkdir(exist_ok=True)
            
            # Create Core subdirectories (Interfaces/Repositories created by default under Interfaces, 
            # but let's be explicit if needed, or just let the agent create them as needed.)
            # Actually, let's just make sure the Interfaces folder exists (it is already there).
            # We can add a specialized folder for repo interfaces if we want strict structure.
            (core_path / "Interfaces" / "Repositories").mkdir(parents=True, exist_ok=True)
            
            # Create Worker.csproj
            worker_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Worker</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\Core\\Core.csproj" />
    <ProjectReference Include="..\\Infrastructure\\Infrastructure.csproj" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Microsoft.Extensions.Hosting" Version="8.0.0" />
  </ItemGroup>
</Project>
'''
            worker_path = output_dir / "src" / "Worker"
            worker_path.mkdir(parents=True, exist_ok=True)
            (worker_path / "Worker.csproj").write_text(worker_csproj)
            (worker_path / "Jobs").mkdir(exist_ok=True)
            
            # Scaffold IJob interface
            ijob_content = '''namespace ConvertedBatch.Worker.Jobs;

/// <summary>
/// Common interface for all batch job classes.
/// Each JCL job is implemented as a class that implements this interface.
/// </summary>
public interface IJob
{
    Task<int> ExecuteAsync(string[] args);
}
'''
            (worker_path / "Jobs" / "IJob.cs").write_text(ijob_content)
            
            # Scaffold single Program.cs entry point
            program_content = '''using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using ConvertedBatch.Worker.Jobs;

var builder = Host.CreateApplicationBuilder(args);

// === Service & Repository Registration ===
// TODO: Register Core services and Infrastructure repositories here

// === Job Registration ===
// TODO: Register job classes here, e.g.:
// builder.Services.AddKeyedTransient<IJob, SetlJob>("SetlJob");

var host = builder.Build();

var jobName = args.FirstOrDefault()
    ?? throw new ArgumentException("Usage: dotnet run -- <JobName> [--step StepName] [--input file] [--output file]");

var job = host.Services.GetRequiredKeyedService<IJob>(jobName);
var remainingArgs = args.Skip(1).ToArray();
var exitCode = await job.ExecuteAsync(remainingArgs);
return exitCode;
'''
            (worker_path / "Program.cs").write_text(program_content)
            
            # Create Tests.csproj
            tests_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\src\\Core\\Core.csproj" />
    <ProjectReference Include="..\\src\\Infrastructure\\Infrastructure.csproj" />
    <ProjectReference Include="..\\src\\Worker\\Worker.csproj" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit" Version="2.6.2" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.5.4" />
    <PackageReference Include="Moq" Version="4.20.70" />
    <PackageReference Include="FluentAssertions" Version="6.12.0" />
  </ItemGroup>
</Project>
'''
            tests_path = output_dir / "tests"
            tests_path.mkdir(parents=True, exist_ok=True)
            (tests_path / "Tests.csproj").write_text(tests_csproj)

            # Create Test subdirectories
            (tests_path / "Core" / "Services").mkdir(parents=True, exist_ok=True)
            (tests_path / "Infrastructure" / "Repositories").mkdir(parents=True, exist_ok=True)
            (tests_path / "Worker" / "Jobs").mkdir(parents=True, exist_ok=True)
            
            # Create scripts directories
            scripts_path = output_dir / "scripts" / "jobs"
            scripts_path.mkdir(parents=True, exist_ok=True)
            (output_dir / "scripts" / "common").mkdir(parents=True, exist_ok=True)
            
            # Create data directories
            (output_dir / "data" / "input").mkdir(parents=True, exist_ok=True)
            (output_dir / "data" / "output").mkdir(parents=True, exist_ok=True)
            
            logger.info(f"Initialized .NET solution: {solution_name}")
            
            return f"""Successfully initialized solution '{solution_name}' with structure:
{solution_name}.sln
src/
  Core/ (Entities, Services, Interfaces, Enums)
  Infrastructure/ (Data, Storage)
  Worker/Jobs/
tests/
scripts/jobs/
data/ (input, output)

You can now write code files using write_code_file()."""
            
        except Exception as e:
            logger.error(f"initialize_solution error: {e}")
            return f"Error initializing solution: {e}"
    
    @tool("write_code_file")
    def write_code_file(relative_path: str, content: str) -> str:
        """Write a code file to the generated solution.
        
        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/Entities/Customer.cs")
            content: The complete file content
            
        Returns:
            Success or error message
        """
        ALLOWED_EXTENSIONS = {
            ".cs", ".csproj", ".sln",
            ".ps1", ".sh", ".bat",
            ".md",
            ".json", ".yaml", ".yml", ".config", ".xml",
            ".gitignore", ".dockerignore", ".editorconfig"
        }

        try:
            # Validate path doesn't escape
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"
            
            # Validate extension
            ext = target.suffix.lower()
            if ext not in ALLOWED_EXTENSIONS:
                return (f"Error: Extension '{ext}' is not allowed. "
                        f"Allowed: {', '.join(sorted(ALLOWED_EXTENSIONS))}")
            
            # Block arbitrary markdown files (prevent pollution)
            if ext == ".md" and target.name not in ("README.md", "setup.md"):
                return "Error: Only README.md and setup.md are allowed. No other markdown files."
            
            # Enforce directory structure (Agent cannot create new folders)
            if not target.parent.exists():
                return f"Error: Directory '{target.parent.name}' does not exist. You must use the existing folder structure."
            
            # Write file
            target.write_text(content, encoding="utf-8")
            
            logger.info(f"Wrote code file: {relative_path}")
            return f"Successfully wrote: {relative_path}"
            
        except Exception as e:
            logger.error(f"write_code_file error: {e}")
            return f"Error writing file: {e}"
    
    @tool("create_directory")
    def create_directory(relative_path: str) -> str:
        """Create a directory in the generated solution.
        
        Args:
            relative_path: Path relative to local-migration/
            
        Returns:
            Success or error message
        """
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"
            
            target.mkdir(parents=True, exist_ok=True)
            return f"Created directory: {relative_path}"
            
        except Exception as e:
            logger.error(f"create_directory error: {e}")
            return f"Error creating directory: {e}"
    
    @tool("list_generated_files")
    def list_generated_files() -> str:
        """List all files generated so far in the solution.
        
        Use this to check what's already been created (useful for resuming).
        
        Returns:
            List of generated files with sizes
        """
        try:
            if not output_dir.exists():
                return "No files generated yet. Call initialize_solution() first."
            
            files = []
            for item in sorted(output_dir.rglob("*")):
                if item.is_file():
                    rel_path = item.relative_to(output_dir)
                    size = item.stat().st_size
                    files.append(f"{rel_path} ({size} bytes)")
            
            if not files:
                return "Output directory exists but contains no files"
            
            return f"Generated files ({len(files)} total):\n" + "\n".join(files)
            
        except Exception as e:
            logger.error(f"list_generated_files error: {e}")
            return f"Error listing files: {e}"

    @tool("remove_file")
    def remove_file(relative_path: str) -> str:
        """Remove a specific file from the solution.
        
        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/OldClass.cs")
            
        Returns:
            Success or error message
        """
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"
            
            if not target.exists():
                return f"Error: File '{relative_path}' does not exist"
            
            if not target.is_file():
                return f"Error: Path '{relative_path}' is not a file"
            
            target.unlink()
            logger.info(f"Removed file: {relative_path}")
            return f"Successfully removed file: {relative_path}"
            
        except Exception as e:
            logger.error(f"remove_file error: {e}")
            return f"Error removing file: {e}"

    @tool("remove_directory")
    def remove_directory(relative_path: str, recursive: bool = False) -> str:
        """Remove a directory from the solution.
        
        Args:
            relative_path: Path relative to local-migration/
            recursive: If True, remove directory and all contents. If False, fails if not empty.
            
        Returns:
            Success or error message
        """
        import stat
        
        def remove_readonly(func, path, _):
            """Clear the readonly bit and reattempt the removal"""
            os.chmod(path, stat.S_IWRITE)
            func(path)
            
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"
            
            if not target.exists():
                return f"Error: Directory '{relative_path}' does not exist"
            
            if not target.is_dir():
                return f"Error: Path '{relative_path}' is not a directory"
            
            if recursive:
                # Use onerror to handle read-only files (common on Windows)
                shutil.rmtree(target, onerror=remove_readonly)
                logger.info(f"Removed directory (recursive): {relative_path}")
                return f"Successfully removed directory (recursive): {relative_path}"
            else:
                try:
                    target.rmdir()
                    logger.info(f"Removed directory: {relative_path}")
                    return f"Successfully removed directory: {relative_path}"
                except OSError:
                    return f"Error: Directory '{relative_path}' is not empty. Set recursive=True to force."
            
        except Exception as e:
            logger.error(f"remove_directory error: {e}")
            return f"Error removing directory: {e}"
    
    return [
        initialize_solution,
        write_code_file,
        # create_directory, # Disabled to enforce structure
        list_generated_files,
        remove_file,
        list_batch_components,
    ]
