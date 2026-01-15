class ProjectKnowledgeStore:
    def __init__(self):
        self.programs = {}    # {name: json_metadata}
        self.copybooks = {}   # {name: field_details}
        self.jcl_flows = {}   # {job_name: [step1, step2]}
        self.db_tables = {}   # {table_name: dclgen_info}
        self.faiss_index = None # To be populated after Stage 1

    def add_cobol_file(self, json_data):
        name = json_data['program_name']
        self.programs[name] = json_data

    def get_variable_definition(self, prog_name, var_name):
        pass