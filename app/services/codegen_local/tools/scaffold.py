"""Deterministic .NET solution scaffolding.

Called by service.py BEFORE the agent starts.  Every file produced here is
pure template — no LLM involvement.  The agent then fills in the real
business logic on top of this skeleton.
"""

import uuid
from pathlib import Path

from loguru import logger


def scaffold_solution(
    output_dir: Path,
    source_dir: Path,
    solution_name: str,
) -> str:
    """Create the full .NET 8 solution scaffold deterministically.

    Idempotent — skips if a .sln file already exists in *output_dir*.

    Creates:
    - .sln, .csproj files, folder structure
    - IJob.cs interface
    - Program.cs with actual job registrations (from JCL discovery)
    - Worker Job **stub** files for every JCL source file
    - PS1 script **skeletons** for every JCL source file
    - .editorconfig, Directory.Build.props, .gitignore

    Args:
        output_dir:    Resolved absolute path where the solution is generated.
        source_dir:    Resolved absolute path to the project source files.
        solution_name: Human-readable name for the .sln file.

    Returns:
        Summary string describing what was created (or "already initialised").
    """
    # ---- Idempotency gate ----
    if output_dir.exists() and list(output_dir.glob("*.sln")):
        existing = next(output_dir.glob("*.sln"))
        return (
            f"Solution already initialised ({existing.name}).  "
            "You can start writing code immediately with write_code_file()."
        )

    try:
        output_dir.mkdir(parents=True, exist_ok=True)

        # ================================================================
        # 1. .sln file
        # ================================================================
        core_guid = str(uuid.uuid4()).upper()
        infra_guid = str(uuid.uuid4()).upper()
        worker_guid = str(uuid.uuid4()).upper()
        core_tests_guid = str(uuid.uuid4()).upper()
        infra_tests_guid = str(uuid.uuid4()).upper()

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
Global
\tGlobalSection(SolutionConfigurationPlatforms) = preSolution
\t\tDebug|Any CPU = Debug|Any CPU
\t\tRelease|Any CPU = Release|Any CPU
\tEndGlobalSection
\tGlobalSection(ProjectConfigurationPlatforms) = postSolution
\t\t{{{core_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
\t\t{{{core_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
\t\t{{{core_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
\t\t{{{core_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
\t\t{{{infra_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
\t\t{{{infra_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
\t\t{{{infra_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
\t\t{{{infra_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
\t\t{{{worker_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
\t\t{{{worker_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
\t\t{{{worker_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
\t\t{{{worker_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
\t\t{{{core_tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
\t\t{{{core_tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
\t\t{{{core_tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
\t\t{{{core_tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
\t\t{{{infra_tests_guid}}}.Debug|Any CPU.ActiveCfg = Debug|Any CPU
\t\t{{{infra_tests_guid}}}.Debug|Any CPU.Build.0 = Debug|Any CPU
\t\t{{{infra_tests_guid}}}.Release|Any CPU.ActiveCfg = Release|Any CPU
\t\t{{{infra_tests_guid}}}.Release|Any CPU.Build.0 = Release|Any CPU
\tEndGlobalSection
EndGlobal
'''
        (output_dir / f"{solution_name}.sln").write_text(sln_content)

        # ================================================================
        # 2. src/Core
        # ================================================================
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

        for sub in ("Entities", "Services", "Interfaces", "Enums", "Configuration", "Exceptions"):
            (core_path / sub).mkdir(exist_ok=True)
        (core_path / "Interfaces" / "Repositories").mkdir(parents=True, exist_ok=True)
        (core_path / "Interfaces" / "Services").mkdir(parents=True, exist_ok=True)

        # ================================================================
        # 3. src/Infrastructure
        # ================================================================
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
        for sub in ("Data", "Storage", "Repositories"):
            (infra_path / sub).mkdir(exist_ok=True)

        # ================================================================
        # 4. src/Worker
        # ================================================================
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

        # IJob interface
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

        # ================================================================
        # 5. tests/
        # ================================================================
        test_packages = '''  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit" Version="2.6.2" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.5.4" />
    <PackageReference Include="Moq" Version="4.20.70" />
    <PackageReference Include="FluentAssertions" Version="6.12.0" />
  </ItemGroup>'''

        tests_path = output_dir / "tests"
        tests_path.mkdir(parents=True, exist_ok=True)

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

        # ================================================================
        # 6. scripts/ & data/
        # ================================================================
        scripts_path = output_dir / "scripts" / "jobs"
        scripts_path.mkdir(parents=True, exist_ok=True)
        (output_dir / "scripts" / "common").mkdir(parents=True, exist_ok=True)
        (output_dir / "data" / "input").mkdir(parents=True, exist_ok=True)
        (output_dir / "data" / "output").mkdir(parents=True, exist_ok=True)

        # ================================================================
        # 7. Config files (.editorconfig, Directory.Build.props, .gitignore)
        # ================================================================
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

        gitignore = '''bin/
obj/
*.user
*.suo
.vs/
*.DotSettings.user
'''
        (output_dir / ".gitignore").write_text(gitignore)

        # ================================================================
        # 8. JCL Discovery → pre-scaffold Worker Jobs + PS1 scripts
        # ================================================================
        jcl_jobs: list[tuple[str, str, str]] = []  # (class_name, script_name, jcl_filename)
        if source_dir.exists():
            for jcl_file in sorted(source_dir.rglob("*.jcl")):
                if not jcl_file.is_file():
                    continue
                class_name = jcl_file.stem.capitalize()   # SETLJOB → Setljob
                script_name = jcl_file.stem.lower()        # SETLJOB → setljob
                jcl_jobs.append((class_name, script_name, jcl_file.name))

        scaffolded_jobs: list[str] = []
        for class_name, script_name, jcl_filename in jcl_jobs:
            # --- Worker Job stub ---
            job_file = worker_path / "Jobs" / f"{class_name}.cs"
            if not job_file.exists():
                job_cs = (
                    f"// Source: {jcl_filename}\n"
                    f"// Implements: TODO — tag with F-IDs after reading source\n"
                    f"namespace ConvertedBatch.Worker.Jobs;\n"
                    f"\n"
                    f"/// <summary>\n"
                    f"/// Worker job converted from {jcl_filename}.\n"
                    f"/// TODO: Read the JCL source and implement step logic.\n"
                    f"/// </summary>\n"
                    f"public class {class_name} : IJob\n"
                    f"{{\n"
                    f"    public {class_name}()\n"
                    f"    {{\n"
                    f"    }}\n"
                    f"\n"
                    f"    public async Task<int> ExecuteAsync(string[] args)\n"
                    f"    {{\n"
                    f"        // TODO: Implement job steps from {jcl_filename}\n"
                    f"        throw new NotImplementedException(\n"
                    f"            \"Job not yet implemented — read {jcl_filename} and fill in logic\");\n"
                    f"    }}\n"
                    f"}}\n"
                )
                job_file.write_text(job_cs, encoding="utf-8")

            # --- PS1 script skeleton ---
            ps1_file = scripts_path / f"run-{script_name}.ps1"
            if not ps1_file.exists():
                ps1 = (
                    f"# run-{script_name}.ps1 — Converted from: {jcl_filename}\n"
                    f"# TODO: Fill in steps after analysing the JCL source\n"
                    f'param([string]$DataDir = "./data")\n'
                    f'$ErrorActionPreference = "Stop"\n'
                    f"$script:MaxRC = 0\n"
                    f"function Update-MaxRC($rc) {{ if ($rc -gt $script:MaxRC) {{ $script:MaxRC = $rc }} }}\n"
                    f"\n"
                    f"# TODO: Add STEP sections here based on JCL steps in {jcl_filename}\n"
                    f'Write-Host "=== run-{script_name}.ps1 — NOT YET IMPLEMENTED ==="\n'
                    f"exit 1\n"
                )
                ps1_file.write_text(ps1, encoding="utf-8")

            scaffolded_jobs.append(f"{class_name} ({jcl_filename})")

        # --- Program.cs with actual job registrations ---
        if jcl_jobs:
            registrations = "\n".join(
                f'builder.Services.AddKeyedTransient<IJob, {cn}>("{cn}");'
                for cn, _, _ in jcl_jobs
            )
            program_cs = (
                "using Microsoft.Extensions.DependencyInjection;\n"
                "using Microsoft.Extensions.Hosting;\n"
                "using ConvertedBatch.Worker.Jobs;\n"
                "\n"
                "var builder = Host.CreateApplicationBuilder(args);\n"
                "\n"
                "// === Service & Repository Registration ===\n"
                "// TODO: Register Core services and Infrastructure repositories here\n"
                "\n"
                "// === Job Registration ===\n"
                f"{registrations}\n"
                "\n"
                "var host = builder.Build();\n"
                "\n"
                "var jobName = args.FirstOrDefault()\n"
                '    ?? throw new ArgumentException("Usage: dotnet run -- <JobName> [--step StepName] [--input file] [--output file]");\n'
                "\n"
                'var job = host.Services.GetRequiredKeyedService<IJob>(jobName);\n'
                "var remainingArgs = args.Skip(1).ToArray();\n"
                "var exitCode = await job.ExecuteAsync(remainingArgs);\n"
                "return exitCode;\n"
            )
        else:
            program_cs = (
                "using Microsoft.Extensions.DependencyInjection;\n"
                "using Microsoft.Extensions.Hosting;\n"
                "using ConvertedBatch.Worker.Jobs;\n"
                "\n"
                "var builder = Host.CreateApplicationBuilder(args);\n"
                "\n"
                "// === Service & Repository Registration ===\n"
                "// TODO: Register Core services and Infrastructure repositories here\n"
                "\n"
                "// === Job Registration ===\n"
                "// TODO: Register job classes here, e.g.:\n"
                '// builder.Services.AddKeyedTransient<IJob, SampleJob>("SampleJob");\n'
                "\n"
                "var host = builder.Build();\n"
                "\n"
                "var jobName = args.FirstOrDefault()\n"
                '    ?? throw new ArgumentException("Usage: dotnet run -- <JobName> [--step StepName] [--input file] [--output file]");\n'
                "\n"
                'var job = host.Services.GetRequiredKeyedService<IJob>(jobName);\n'
                "var remainingArgs = args.Skip(1).ToArray();\n"
                "var exitCode = await job.ExecuteAsync(remainingArgs);\n"
                "return exitCode;\n"
            )
        (worker_path / "Program.cs").write_text(program_cs, encoding="utf-8")

        logger.info(
            f"Scaffolded .NET solution '{solution_name}' with "
            f"{len(jcl_jobs)} pre-scaffolded JCL jobs"
        )

        # ---- Build summary ----
        summary_lines = [
            f"Successfully initialised solution '{solution_name}' with structure:",
            f"{solution_name}.sln",
            ".editorconfig       (code style enforcement)",
            "Directory.Build.props (solution-wide analysers)",
            "src/",
            "  Core/ (Entities, Services, Interfaces, Enums, Configuration, Exceptions)",
            "  Infrastructure/ (Data, Storage, Repositories)",
            "  Worker/Jobs/",
            "tests/",
            "  Core.Tests/Services/",
            "  Infrastructure.Tests/Repositories/",
            "scripts/ (jobs, common)",
            "data/ (input, output)",
        ]
        if scaffolded_jobs:
            summary_lines.append("")
            summary_lines.append(
                f"Pre-scaffolded {len(scaffolded_jobs)} Worker Jobs + PS1 scripts "
                "(stubs — fill in real logic):"
            )
            for j in scaffolded_jobs:
                summary_lines.append(f"  - {j}")
            summary_lines.append("")
            summary_lines.append(
                "Program.cs already contains job registrations.  "
                "You only need to add service/repository DI registrations."
            )
        summary_lines.append("")
        summary_lines.append(
            "Code analysis is enabled — the build will flag code quality "
            "and security issues."
        )
        summary_lines.append(
            "You can now write code files using write_code_file(). "
            "Use read_generated_file() to inspect your own output."
        )
        return "\n".join(summary_lines)

    except Exception as e:
        logger.error(f"scaffold_solution error: {e}")
        return f"Error initialising solution: {e}"
