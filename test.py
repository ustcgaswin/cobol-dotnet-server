import networkx as nx
import matplotlib.pyplot as plt
import json
import os
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter


def generate_pdf(image_path, pdf_path, title):
    c = canvas.Canvas(pdf_path, pagesize=letter)
    width, height = letter

    c.setFont("Helvetica-Bold", 16)
    c.drawString(100, height - 100, title)

    c.setFont("Helvetica", 12)
    c.drawString(100, height - 130, "The graph has been generated successfully.")

    abs_path = os.path.abspath(image_path)
    link_url = f"file:///{abs_path.replace(os.sep, '/')}"

    c.setFont("Helvetica-Oblique", 12)
    c.setFillColorRGB(0, 0, 1)

    text = "Click here to open the Graph Image"
    text_width = c.stringWidth(text, "Helvetica-Oblique", 12)

    y = height - 160
    c.drawString(100, y, text)

    c.linkURL(
        link_url,
        (100, y - 2, 100 + text_width, y + 12),
        relative=0
    )

    c.save()
    print(f"PDF created successfully: {pdf_path}")


# Load JSON
current_dir = os.path.dirname(os.path.abspath(__file__))
json_path = os.path.join(current_dir, "graph.json")

with open(json_path) as f:
    data = json.load(f)

G = nx.DiGraph()
node_types = {}

if "phases" in data:

    for phase in sorted(data["phases"], key=lambda x: x.get("sequence", 0)):
        phase_name = phase["phase_name"]
        G.add_node(phase_name)
        node_types[phase_name] = "phase"

        for step in phase.get("steps", []):
            step_id = step["step_id"]
            G.add_node(step_id)
            node_types[step_id] = "step"
            G.add_edge(phase_name, step_id)

            for dep in step.get("depends_on", []):
                G.add_edge(dep, step_id)

            for comp in step.get("technical_components", []):
                G.add_node(comp)
                node_types[comp] = "component"
                G.add_edge(step_id, comp)

    graph_title = "Process Flow Graph"

elif "domains" in data:

    for domain in data["domains"]:
        domain_name = domain["domain_name"]
        G.add_node(domain_name)
        node_types[domain_name] = "domain"

        for group in domain.get("groups", []):
            group_name = group["group_name"]
            G.add_node(group_name)
            node_types[group_name] = "group"
            G.add_edge(domain_name, group_name)

            for subgroup in group.get("subgroups", []):
                subgroup_name = subgroup["subgroup_name"]
                G.add_node(subgroup_name)
                node_types[subgroup_name] = "subgroup"
                G.add_edge(group_name, subgroup_name)

                for comp in subgroup.get("components", []):
                    G.add_node(comp)
                    node_types[comp] = "component"
                    G.add_edge(subgroup_name, comp)

    graph_title = "Architecture Hierarchy Graph"

else:
    raise ValueError("Unsupported JSON format")


# Draw graph
plt.figure(figsize=(16, 14))
pos = nx.spring_layout(G, k=0.7, iterations=100, seed=42)

node_sizes = []
node_colors = []

for node in G.nodes():
    ntype = node_types.get(node)

    if ntype == "phase":
        node_sizes.append(4000)
        node_colors.append("purple")
    elif ntype == "step":
        node_sizes.append(2500)
        node_colors.append("orange")
    elif ntype == "domain":
        node_sizes.append(4000)
        node_colors.append("red")
    elif ntype == "group":
        node_sizes.append(3000)
        node_colors.append("gold")
    elif ntype == "subgroup":
        node_sizes.append(2200)
        node_colors.append("green")
    else:
        node_sizes.append(1200)
        node_colors.append("skyblue")

nx.draw_networkx_nodes(
    G,
    pos,
    node_size=node_sizes,
    node_color=node_colors,
    edgecolors="black",
    linewidths=1.5
)

nx.draw_networkx_edges(
    G,
    pos,
    arrows=True,
    arrowstyle='-|>',
    arrowsize=18,
    width=1.5,
    alpha=0.6
)

nx.draw_networkx_labels(
    G,
    pos,
    font_size=8,
    font_weight="bold",
    font_color="white",
    bbox=dict(facecolor="black", alpha=0.6, edgecolor="none")
)

plt.title(graph_title, fontsize=16)
plt.axis("off")

image_path = os.path.join(current_dir, "graph.jpg")
plt.savefig(image_path, format="JPG", dpi=300, bbox_inches="tight")
plt.close()

print(f"Graph generated successfully: {image_path}")

pdf_path = os.path.join(current_dir, "graph_report.pdf")
generate_pdf(image_path, pdf_path, graph_title)
 