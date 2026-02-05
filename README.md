# COBOL Converter API

## System Architecture

```mermaid
flowchart TB
    subgraph Input["📁 Source Files"]
        COBOL["COBOL"]
        CPY["Copybooks"]
        JCL["JCL"]
        PLI["PL/I"]
        REXX["REXX"]
        CA7["CA-7"]
        DCLGEN["DCLGEN"]
        PARMLIB["PARMLIB"]
        ASM["Assembly"]
    end

    subgraph PhaseA["🔧 Phase A: Parsing"]
        Parser["ParserService"]
        Summarizer["SummarizerService"]
        DepExtractor["DependencyExtractor"]
    end

    subgraph Artifacts["📂 Artifacts"]
        PA["parsed_outputs/"]
        FS["file_summaries.md"]
        DG["dependency_graph.md"]
    end

    Input --> Parser
    Parser --> PA
    PA --> Summarizer
    PA --> DepExtractor
    Summarizer --> FS
    DepExtractor --> DG

    style Input fill:#e1f5fe
    style PhaseA fill:#fff3e0
    style Artifacts fill:#f3e5f5
```

```mermaid
flowchart TB
    subgraph AnalystAgent["🤖 Phase B: Analyst Agent"]
        direction LR
        Analyst["AnalystService<br/>(LangGraph)"]
        
        subgraph ReadTools["Reading"]
            AT1["list_artifacts"]
            AT2["read_artifact"]
            AT3["grep_artifact"]
            AT9["search_docs (RAG)"]
        end
        
        subgraph WriteTools["Writing"]
            AT4["submit_catalog_entry"]
            AT5["submit_job_chain"]
            AT6["submit_overview"]
            AT7["submit_data_flow"]
            AT8["submit_gap"]
        end
    end

    subgraph AnalystInput["Input"]
        FS2["file_summaries.md"]
        DG2["dependency_graph.md"]
    end

    subgraph AnalystOutput["Output: system_context/"]
        FC["functionality_catalog.md"]
        JC["job_chains.md"]
        SO["system_overview.md"]
        DF["data_flows.md"]
        GP["gaps.md"]
    end

    AnalystInput --> Analyst
    Analyst --> ReadTools
    Analyst --> WriteTools
    WriteTools --> AnalystOutput

    style AnalystAgent fill:#e8f5e9
    style AnalystInput fill:#f3e5f5
    style AnalystOutput fill:#fff8e1
    style ReadTools fill:#e3f2fd
    style WriteTools fill:#fce4ec
```

```mermaid
flowchart TB
    subgraph CodegenAgent["🚀 Phase C: Codegen Agent"]
        Codegen["CodegenLocalService<br/>(LangGraph)"]
    end

    subgraph Tools["Agent Tools"]
        direction LR
        
        subgraph Reading["📖 Reading"]
            R1["list_artifacts"]
            R2["read_artifact"]
            R3["view_source_file"]
            R4["grep_source"]
        end
        
        subgraph Knowledge["📚 Knowledge"]
            K1["read_conversion_guide"]
            K2["read_style_guide"]
            K3["lookup_utility"]
            K4["search_knowledge"]
            K5["search_docs (RAG)"]
        end
        
        subgraph Context["🎯 Context"]
            X1["read_functionality_catalog"]
            X2["read_job_chains"]
            X3["read_data_flows"]
        end
        
        subgraph Solution["🔨 Solution"]
            S1["initialize_solution"]
            S2["write_code_file"]
            S3["create_directory"]
        end
        
        subgraph Build["⚙️ Build"]
            B1["run_dotnet_build"]
            B2["run_dotnet_test"]
        end
        
        subgraph Status["📊 Status"]
            ST1["log_component_status"]
            ST2["log_issue"]
        end
    end

    subgraph CodegenOutput["📦 Output: code-migration/"]
        SLN[".NET Solution"]
        Tests["xUnit Tests"]
        Summary["CONVERSION_SUMMARY.md"]
        Flow["process_flow.md"]
    end

    Codegen --> Reading & Knowledge & Context
    Codegen --> Solution & Build & Status
    Solution --> CodegenOutput

    style CodegenAgent fill:#e3f2fd
    style Reading fill:#fff3e0
    style Knowledge fill:#f3e5f5
    style Context fill:#e8f5e9
    style Solution fill:#fce4ec
    style Build fill:#fff8e1
    style Status fill:#e0f7fa
    style CodegenOutput fill:#c8e6c9
```

## MLflow Tracing (Development)

To view LangChain/LangGraph traces:

1. Enable in `.env`:
   ```
   MLFLOW_ENABLED=true
   ```

2. Start MLflow UI:
   ```bash
   uv run mlflow ui --port 5000
   ```

3. Open http://localhost:5000
