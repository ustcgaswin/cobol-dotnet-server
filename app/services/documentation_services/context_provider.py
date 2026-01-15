# import re

# class ContextProvider:
#     def __init__(self, mapper_graph):
#         self.graph = mapper_graph

#     def get_context_for_paragraph(self, program_name, paragraph_code):
#         """
#         Finds only the variables used in this specific paragraph
#         and fetches their definitions from the Copybook/Data JSON.
#         """
#         # 1. Find all variables mentioned in this code chunk
#         words = set(re.findall(r'[A-Z0-9-]+', paragraph_code.upper()))
        
#         # 2. Get the full Data Division JSON for this program
#         prog_data = self.graph.nodes[program_name]['data']
#         data_division = prog_data.get('data_division', {})
        
#         # 3. Filter definitions: Only keep variables found in 'words'
#         relevant_defs = []
        
#         # Check Working-Storage
#         for item in data_division.get('working_storage_section', []):
#             if item['name'] in words:
#                 relevant_defs.append(item)
                
#         # 4. Check linked Copybooks
#         # (Your Python code traverses the graph to find linked copybooks)
#         linked_copybooks = [n for n in self.graph.neighbors(program_name) 
#                            if self.graph[program_name][n]['relation'] == 'uses_copybook']
        
#         for cp_name in linked_copybooks:
#             cp_data = self.graph.nodes[cp_name]['data']
#             for field in cp_data.get('record_layouts', []):
#                 if field['name'] in words:
#                     relevant_defs.append(field)

#         return relevant_defs
    
#     def get_minimal_data_context(self, code_chunk, program_json, dependency_jsons):
#         """
#         Extracts only the variable hierarchies used in a code chunk.
#         """
#         # 1. Extract all potential tokens (Variable Names) from the COBOL/PLI code
#         tokens = set(re.findall(r'[A-Z0-9-]+', code_chunk.upper()))
        
#         context_definitions = []
#         seen_vars = set()

#         def find_in_hierarchy(fields, target_tokens):
#             matches = []
#             for field in fields:
#                 # If this field is used, or one of its children is used
#                 # we keep this "branch" of the tree
#                 child_matches = find_in_hierarchy(field.get('children', []), target_tokens)
                
#                 if field['name'] in target_tokens or child_matches:
#                     # Create a copy of the field but only with relevant children
#                     minimal_field = {
#                         "level": field['level'],
#                         "name": field['name'],
#                         "pic": field.get('pic'),
#                         "usage": field.get('usage'),
#                         "children": child_matches
#                     }
#                     matches.append(minimal_field)
#             return matches

#         # 2. Search in local Working Storage
#         ws_fields = program_json.get('data_division', {}).get('working_storage_section', [])
#         context_definitions.extend(find_in_hierarchy(ws_fields, tokens))

#         # 3. Search in all linked Copybooks/DCLGENs
#         for dep_json in dependency_jsons:
#             cp_fields = dep_json.get('record_layouts', [])
#             context_definitions.extend(find_in_hierarchy(cp_fields, tokens))

#         return context_definitions

import re

class ContextProvider:
    def __init__(self, mapper_graph):
        self.graph = mapper_graph

    def get_context_for_paragraph(self, program_name, paragraph_code):
        """
        Entry point to get filtered context for a specific chunk of code.
        """
        # 1. Extract tokens (Variable Names) from code
        tokens = set(re.findall(r'[A-Z0-9-]+', paragraph_code.upper()))
        
        # 2. Get Program JSON from Graph
        if program_name not in self.graph.nodes:
            return []
        
        prog_json = self.graph.nodes[program_name]['data']
        
        # 3. Find dependencies (Copybooks/DCLGENs) via Graph Edges
        dependency_jsons = []
        neighbors = self.graph.neighbors(program_name)
        for n in neighbors:
            edge_data = self.graph.get_edge_data(program_name, n)
            if edge_data.get('relation') in ['uses_copybook', 'references_table']:
                dependency_jsons.append(self.graph.nodes[n]['data'])

        # 4. Use the Recursive Hierarchy Filter
        return self._get_dynamic_tree_context(tokens, prog_json, dependency_jsons)

    def _get_dynamic_tree_context(self, tokens, program_json, dependency_jsons):
        """
        Refined Recursive Logic: Prunes unused branches but keeps parent lineages.
        """
        def filter_hierarchy(fields):
            results = []
            for field in fields:
                # Check if this field name is used OR if any of its children are used
                child_results = filter_hierarchy(field.get('children', []))
                
                if field['name'] in tokens or child_results:
                    # Construct a minimal version of the field
                    node = {
                        "level": field.get('level'),
                        "name": field.get('name'),
                        "pic": field.get('pic'),
                        "usage": field.get('usage'),
                        "is_nullable": field.get('is_nullable'),
                        "children": child_results # Only contains relevant children
                    }
                    results.append(node)
            return results

        context = []
        
        # Process Working Storage
        ws_section = program_json.get('data_division', {}).get('working_storage_section', [])
        context.extend(filter_hierarchy(ws_section))
        
        # Process Linkage Section (Important for PLI/COBOL subroutines)
        ls_section = program_json.get('data_division', {}).get('linkage_section', [])
        context.extend(filter_hierarchy(ls_section))

        # Process all external dependencies (Copybooks/DCLGENs)
        for dep in dependency_jsons:
            # Copybooks use 'record_layouts', DCLGENs use 'record_layout'
            layout = dep.get('record_layouts') or dep.get('record_layout') or []
            context.extend(filter_hierarchy(layout))

        return context