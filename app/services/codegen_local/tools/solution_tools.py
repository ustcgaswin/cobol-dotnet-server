"""Solution management tools for the code generation agent.

Handles .NET solution initialization and file writing.
"""

import re
import uuid
from pathlib import Path

from langchain.tools import tool
from loguru import logger

from app.services.codegen_local.tools.source_file_tools import get_source_read_registry


def create_solution_tools(project_id: str, output_path: str, source_path: str) -> list:
    """Create solution management tools.
    
    Args:
        project_id: Project ID for scoping file operations
        output_path: Absolute path to the output directory for generated code
        source_path: Absolute path to the source directory (for listing components)
        
    Returns:
        List of LangChain tools
    """
    output_dir = Path(output_path).resolve()
    source_dir = Path(source_path).resolve()
    
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
            core_tests_guid = str(uuid.uuid4()).upper()
            infra_tests_guid = str(uuid.uuid4()).upper()
            worker_tests_guid = str(uuid.uuid4()).upper()
            
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
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Core.Tests", "tests\\Core.Tests\\Core.Tests.csproj", "{{{core_tests_guid}}}"
EndProject
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Infrastructure.Tests", "tests\\Infrastructure.Tests\\Infrastructure.Tests.csproj", "{{{infra_tests_guid}}}"
EndProject
Project("{{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}}") = "Worker.Tests", "tests\\Worker.Tests\\Worker.Tests.csproj", "{{{worker_tests_guid}}}"
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
		{{{core_tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{core_tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{core_tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{core_tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
		{{{infra_tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{infra_tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{infra_tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{infra_tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
		{{{worker_tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
		{{{worker_tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
		{{{worker_tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
		{{{worker_tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
	EndGlobalSection
EndGlobal
'''
            (output_dir / f"{solution_name}.sln").write_text(sln_content)
            
            # Create Core.csproj with code analysis enabled
            core_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Core</RootNamespace>
    <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    <EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
    <AnalysisLevel>latest-recommended</AnalysisLevel>
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
            (core_path / "Interfaces" / "Repositories").mkdir(parents=True, exist_ok=True)
            (core_path / "Interfaces" / "Services").mkdir(parents=True, exist_ok=True)
            (core_path / "Enums").mkdir(exist_ok=True)
            (core_path / "Configuration").mkdir(exist_ok=True)
            (core_path / "Exceptions").mkdir(exist_ok=True)
            
            # Create Infrastructure.csproj with security analysis
            infra_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Infrastructure</RootNamespace>
    <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    <EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
    <AnalysisLevel>latest-recommended</AnalysisLevel>
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
            
            # Create Worker.csproj with analysis
            worker_csproj = '''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <RootNamespace>ConvertedBatch.Worker</RootNamespace>
    <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    <EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
    <AnalysisLevel>latest-recommended</AnalysisLevel>
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
            
            # Shared test package references
            test_packages = '''  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit" Version="2.6.2" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.5.4" />
    <PackageReference Include="Moq" Version="4.20.70" />
    <PackageReference Include="FluentAssertions" Version="6.12.0" />
  </ItemGroup>'''

            tests_path = output_dir / "tests"
            tests_path.mkdir(parents=True, exist_ok=True)

            # Core.Tests — unit tests for Core services
            core_tests_csproj = f'''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
    <RootNamespace>ConvertedBatch.Core.Tests</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\..\\src\\Core\\Core.csproj" />
  </ItemGroup>
{test_packages}
</Project>
'''
            core_tests_path = tests_path / "Core.Tests"
            core_tests_path.mkdir(parents=True, exist_ok=True)
            (core_tests_path / "Core.Tests.csproj").write_text(core_tests_csproj)
            (core_tests_path / "Services").mkdir(exist_ok=True)

            # Infrastructure.Tests — repository and data tests
            infra_tests_csproj = f'''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
    <RootNamespace>ConvertedBatch.Infrastructure.Tests</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\..\\src\\Core\\Core.csproj" />
    <ProjectReference Include="..\\..\\src\\Infrastructure\\Infrastructure.csproj" />
  </ItemGroup>
{test_packages}
</Project>
'''
            infra_tests_path = tests_path / "Infrastructure.Tests"
            infra_tests_path.mkdir(parents=True, exist_ok=True)
            (infra_tests_path / "Infrastructure.Tests.csproj").write_text(infra_tests_csproj)
            (infra_tests_path / "Repositories").mkdir(exist_ok=True)

            # Worker.Tests — job integration tests
            worker_tests_csproj = f'''<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
    <RootNamespace>ConvertedBatch.Worker.Tests</RootNamespace>
  </PropertyGroup>
  <ItemGroup>
    <ProjectReference Include="..\\..\\src\\Core\\Core.csproj" />
    <ProjectReference Include="..\\..\\src\\Infrastructure\\Infrastructure.csproj" />
    <ProjectReference Include="..\\..\\src\\Worker\\Worker.csproj" />
  </ItemGroup>
{test_packages}
</Project>
'''
            worker_tests_path = tests_path / "Worker.Tests"
            worker_tests_path.mkdir(parents=True, exist_ok=True)
            (worker_tests_path / "Worker.Tests.csproj").write_text(worker_tests_csproj)
            (worker_tests_path / "Jobs").mkdir(exist_ok=True)
            
            # Create scripts directories
            scripts_path = output_dir / "scripts" / "jobs"
            scripts_path.mkdir(parents=True, exist_ok=True)
            (output_dir / "scripts" / "common").mkdir(parents=True, exist_ok=True)
            
            # Create data directories
            (output_dir / "data" / "input").mkdir(parents=True, exist_ok=True)
            (output_dir / "data" / "output").mkdir(parents=True, exist_ok=True)
            
            # Create .editorconfig for consistent code style enforcement
            editorconfig = '''# EditorConfig — enforces consistent code style across all projects
root = true

[*.cs]
indent_style = space
indent_size = 4
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true

# Naming rules
dotnet_naming_rule.private_fields_should_be_camel_case.severity = warning
dotnet_naming_rule.private_fields_should_be_camel_case.symbols = private_fields
dotnet_naming_rule.private_fields_should_be_camel_case.style = camel_case_underscore
dotnet_naming_symbol.private_fields.applicable_kinds = field
dotnet_naming_symbol.private_fields.applicable_accessibilities = private
dotnet_naming_style.camel_case_underscore.required_prefix = _
dotnet_naming_style.camel_case_underscore.capitalization = camel_case

# Code quality
dotnet_diagnostic.CA1062.severity = warning
dotnet_diagnostic.CA2100.severity = error
dotnet_diagnostic.CA2211.severity = warning
dotnet_diagnostic.CA1822.severity = suggestion

# Security: SQL injection prevention
dotnet_diagnostic.CA2100.severity = error

# Async best practices
dotnet_diagnostic.CA2007.severity = suggestion
dotnet_diagnostic.CA1849.severity = warning

# Dispose pattern
dotnet_diagnostic.CA1816.severity = warning
dotnet_diagnostic.CA2000.severity = warning
'''
            (output_dir / ".editorconfig").write_text(editorconfig)
            
            # Create Directory.Build.props for solution-wide settings
            build_props = '''<Project>
  <PropertyGroup>
    <!-- Solution-wide code analysis -->
    <EnableNETAnalyzers>true</EnableNETAnalyzers>
    <AnalysisLevel>latest-recommended</AnalysisLevel>
    <!-- Nullable reference types enabled everywhere -->
    <Nullable>enable</Nullable>
  </PropertyGroup>
</Project>
'''
            (output_dir / "Directory.Build.props").write_text(build_props)
            
            # Create .gitignore
            gitignore = '''bin/
obj/
*.user
*.suo
.vs/
*.DotSettings.user
'''
            (output_dir / ".gitignore").write_text(gitignore)
            
            logger.info(f"Initialized .NET solution: {solution_name}")
            
            return f"""Successfully initialized solution '{solution_name}' with structure:
{solution_name}.sln
.editorconfig       (code style enforcement)
Directory.Build.props (solution-wide analyzers)
src/
  Core/ (Entities, Services, Interfaces, Interfaces/Repositories, Interfaces/Services, Enums, Configuration, Exceptions)
  Infrastructure/ (Data, Storage, Repositories)
  Worker/Jobs/
tests/
  Core.Tests/Services/              (service unit tests)
  Infrastructure.Tests/Repositories/ (repository tests)
  Worker.Tests/Jobs/                (job tests)
scripts/ (jobs, common)
data/ (input, output)

Code analysis is enabled — the build will flag code quality and security issues.
You can now write code files using write_code_file(). Use read_generated_file() to inspect your own output."""
            
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
            ".json", ".config", ".xml",
            ".gitignore", ".editorconfig"
        }

        # Filenames the agent must NEVER create (infra / devops files)
        FORBIDDEN_FILENAMES = {
            "dockerfile", "docker-compose.yml", "docker-compose.yaml",
            "config.yml", "config.yaml",
            ".dockerignore",
            "deploy.ps1", "deploy.sh", "deploy.yml", "deploy.yaml",
            "ci.yml", "ci.yaml", "pipeline.yml", "pipeline.yaml",
            "makefile",
        }

        try:
            # Validate path doesn't escape
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"

            # ----- Forbidden filename blocklist -----
            if target.name.lower() in FORBIDDEN_FILENAMES:
                return (f"Error: FORBIDDEN file '{target.name}'. "
                        "This is an infrastructure/DevOps file that must NOT be part of the generated solution.")

            # Validate extension
            ext = target.suffix.lower()
            if ext not in ALLOWED_EXTENSIONS:
                return (f"Error: Extension '{ext}' is not allowed. "
                        f"Allowed: {', '.join(sorted(ALLOWED_EXTENSIONS))}")

            # Block arbitrary markdown files (prevent pollution)
            if ext == ".md" and target.name.lower() not in ("readme.md", "setup.md", "process_flow.md"):
                return (f"Error: Content Policy Violation. Writing '{target.name}' is FORBIDDEN.\n"
                        "Allowed Markdown: 'README.md', 'setup.md', 'process_flow.md'.\n"
                        "DO NOT write intermediate plans, thoughts, or status files.")

            # Block intermediate PowerShell scripts (prevent ad-hoc fix scripts)
            if ext == ".ps1":
                parent_dir = target.parent
                # Check if path ends with scripts/jobs or scripts/common
                # Robust check across OS
                is_jobs_dir = parent_dir.name == "jobs" and parent_dir.parent.name == "scripts"
                is_common_dir = parent_dir.name == "common" and parent_dir.parent.name == "scripts"
                
                if is_jobs_dir:
                    if not target.name.startswith("run-"):
                        return (f"Error: Content Policy Violation. Writing '{target.name}' to 'scripts/jobs/' is FORBIDDEN.\n"
                                "Requirement: Scripts in 'scripts/jobs/' MUST start with 'run-' (e.g. 'run-job1.ps1').")
                elif is_common_dir:
                    pass # Allow common scripts
                else:
                    return (f"Error: Content Policy Violation. Writing '{target.name}' to '{parent_dir.name}' is FORBIDDEN.\n"
                            "Allowed Locations:\n"
                            "1. 'scripts/jobs/' (MUST start with 'run-')\n"
                            "2. 'scripts/common/' (Shared logic)\n"
                            "ALL other .ps1 files (e.g. ad-hoc inputs/tests) are PROHIBITED.")

            # Enforce directory structure (Agent cannot create new folders)
            if not target.parent.exists():
                return f"Error: Directory '{target.parent.name}' does not exist. You must use the existing folder structure."

            # === PROVENANCE ENFORCEMENT ===
            # Service and Job files MUST have a // Source: tag referencing a
            # source file that was actually read via view_source_file().
            is_service = "/Services/" in relative_path and ext == ".cs"
            is_job = "/Jobs/" in relative_path and ext == ".cs" and target.name != "IJob.cs"

            if is_service or is_job:
                source_tag = re.search(r'//\s*Source:\s*([\w.\-]+)', content)
                registry = get_source_read_registry(project_id)

                if not source_tag:
                    return (f"Error: {target.name} is missing a '// Source: FILENAME.ext' comment at the top. "
                            "Every Service and Job file MUST reference the mainframe source it was converted from.")

                source_name = source_tag.group(1).strip()
                source_key = source_name.lower()

                if source_key not in registry:
                    return (f"Error: {target.name} references Source: {source_name} but you have NOT read that file yet. "
                            f"Call view_source_file('{source_name}') first, then rewrite this file with real logic.")

                ri = registry[source_key]
                cov = (ri['lines_covered'] / max(ri['total_lines'], 1)) * 100
                logger.info(f"[Provenance] {target.name} ← {source_name} ({cov:.0f}% read)")

            # === JCL STEM VALIDATION FOR WORKER JOBS ===
            # Worker Job filenames must correspond to actual JCL source file stems.
            # e.g. SETLJOB.jcl → stem "setljob" → Worker class must be Setljob.cs
            if is_job and source_dir.exists():
                jcl_stems = {f.stem.lower() for f in source_dir.rglob("*.jcl") if f.is_file()}
                job_stem = target.stem.lower()
                if jcl_stems and job_stem not in jcl_stems:
                        return (f"Error: Worker Job '{target.name}' does not match any JCL source file. "
                                f"Available JCL stems: {', '.join(sorted(jcl_stems))}. "
                                "The Worker Job class name MUST match the JCL job name.")

            # Write file
            target.write_text(content, encoding="utf-8")

            logger.info(f"Wrote code file: {relative_path}")
            return f"Successfully wrote: {relative_path}"
            
        except Exception as e:
            logger.error(f"write_code_file error: {e}")
            return f"Error writing file: {e}"
    
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
            
            # Simple tree generator
            def tree(dir_path: Path, prefix: str = ""):
                lines = []
                # Helper to sort directories first, then files
                def sort_key(p): return (not p.is_dir(), p.name.lower())
                
                try:
                    contents = sorted(dir_path.iterdir(), key=sort_key)
                except Exception: return []

                for i, path in enumerate(contents):
                    is_last = (i == len(contents) - 1)
                    connector = "└── " if is_last else "├── "
                    
                    if path.is_dir():
                        if path.name in (".git", ".vs", "bin", "obj"): continue # Skip noise
                        lines.append(f"{prefix}{connector}{path.name}/")
                        extension = "    " if is_last else "│   "
                        lines.extend(tree(path, prefix + extension))
                    else:
                        size = path.stat().st_size
                        lines.append(f"{prefix}{connector}{path.name} ({size} B)")
                return lines

            tree_lines = tree(output_dir)
            
            if not tree_lines:
                return "Output directory exists but contains no files"
            
            return f"Generated Solution Structure ({len(tree_lines)} items):\nroot/\n" + "\n".join(tree_lines)
            
        except Exception as e:
            logger.error(f"list_generated_files error: {e}")
            return f"Error listing files: {e}"

    @tool("read_generated_file")
    def read_generated_file(relative_path: str, start_line: int = 1, end_line: int = 100) -> str:
        """Read a file from the generated solution. Use this to inspect your own output.

        Args:
            relative_path: Path relative to local-migration/ (e.g., "src/Core/Services/FsmainService.cs")
            start_line: First line to read (1-indexed, default 1)
            end_line: Last line to read (1-indexed, default 100)

        Returns:
            The requested lines with line numbers, or error message
        """
        try:
            target = (output_dir / relative_path).resolve()
            if not str(target).startswith(str(output_dir)):
                return f"Error: Path '{relative_path}' is outside output directory"

            if not target.exists():
                return f"Error: File '{relative_path}' does not exist. Use list_generated_files() to see available files."

            if not target.is_file():
                return f"Error: Path '{relative_path}' is not a file"

            lines = target.read_text(encoding="utf-8", errors="replace").splitlines()
            total_lines = len(lines)

            if start_line < 1:
                start_line = 1
            if end_line > total_lines:
                end_line = total_lines
            if start_line > end_line:
                return f"Error: Invalid line range ({start_line}-{end_line})"

            selected = lines[start_line - 1:end_line]
            result_lines = [f"{i}: {line}" for i, line in enumerate(selected, start=start_line)]

            return f"File: {relative_path} (lines {start_line}-{end_line} of {total_lines})\n" + "\n".join(result_lines)

        except Exception as e:
            logger.error(f"read_generated_file error: {e}")
            return f"Error reading file: {e}"

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

    return [
        initialize_solution,
        write_code_file,
        list_generated_files,
        read_generated_file,
        remove_file,
        list_batch_components,
    ]
