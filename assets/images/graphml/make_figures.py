"""Generate proposed SVG figures for the Week 11 Graph ML lecture."""
from math import hypot
from pathlib import Path

OUT = Path(__file__).resolve().parent
OUT.mkdir(parents=True, exist_ok=True)

DARK = "#333333"
GREY = "#9A9A9A"
LINE = "#666666"
ORANGE = "#FF8C00"
LORANGE = "#FFD9A8"
BLUE = "#4D8DC9"
LBLUE = "#D3E3F3"
WHITE = "#FFFFFF"
FONT = "Calibri, Carlito, 'Noto Sans', 'Helvetica Neue', Arial, sans-serif"


def doc(w, h, title, body):
    markers = "".join(
        f'<marker id="ah-{name}" viewBox="0 0 10 10" refX="9" refY="5" '
        f'markerWidth="4.5" markerHeight="4.5" orient="auto-start-reverse">'
        f'<path d="M0,0 L10,5 L0,10 z" fill="{color}"/></marker>'
        for name, color in (("dark", DARK), ("orange", ORANGE), ("blue", BLUE))
    )
    return (
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {w} {h}" '
        f'width="{w}" height="{h}" role="img" aria-label="{title}">\n'
        f"<title>{title}</title>\n<defs>{markers}</defs>\n"
        f"<style>circle{{stroke:{DARK};stroke-width:3}}.e{{stroke:{LINE};stroke-width:3;stroke-linecap:round}}</style>\n"
        f'<g font-family="{FONT}" fill="{DARK}" text-anchor="middle">\n{"".join(body)}</g>\n</svg>\n'
    )


def text(x, y, s, size=26, anchor="middle", weight="normal", fill=DARK, central=False):
    attrs = f'x="{x:g}" y="{y:g}" font-size="{size}"'
    if anchor != "middle":
        attrs += f' text-anchor="{anchor}"'
    if weight != "normal":
        attrs += f' font-weight="{weight}"'
    if fill != DARK:
        attrs += f' fill="{fill}"'
    if central:
        attrs += ' dominant-baseline="central"'
    return f"<text {attrs}>{s}</text>\n"


def line(x1, y1, x2, y2, color=LINE, width=3, dash=None):
    xy = f'x1="{x1:g}" y1="{y1:g}" x2="{x2:g}" y2="{y2:g}"'
    if color == LINE and width == 3 and not dash:
        return f'<line {xy} class="e"/>\n'
    d = f' stroke-dasharray="{dash}"' if dash else ""
    return f'<line {xy} stroke="{color}" stroke-width="{width}" stroke-linecap="round"{d}/>\n'


def arrow(x1, y1, x2, y2, color="dark", width=4):
    hexcolor = {"dark": DARK, "orange": ORANGE, "blue": BLUE}[color]
    return (
        f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" stroke="{hexcolor}" '
        f'stroke-width="{width}" stroke-linecap="round" marker-end="url(#ah-{color})"/>\n'
    )


def node_arrow(p, q, r, color, width=5):
    """Arrow between node centres, stopping at the node borders."""
    (x1, y1), (x2, y2) = p, q
    d = hypot(x2 - x1, y2 - y1)
    ux, uy = (x2 - x1) / d, (y2 - y1) / d
    return arrow(round(x1 + ux * (r + 3), 1), round(y1 + uy * (r + 3), 1),
                 round(x2 - ux * (r + 5), 1), round(y2 - uy * (r + 5), 1), color, width)


def node(x, y, label="", fill=WHITE, r=22, size=22, label_fill=DARK):
    out = f'<circle cx="{x:g}" cy="{y:g}" r="{r}" fill="{fill}"/>\n'
    if label:
        out += text(x, y + 1, label, size, weight="bold", fill=label_fill, central=True)
    return out


def box(x, y, w, h, lines, fill=WHITE, stroke=DARK, size=26, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    out = (f'<rect x="{x}" y="{y}" width="{w}" height="{h}" rx="12" fill="{fill}" '
           f'stroke="{stroke}" stroke-width="3"{d}/>\n')
    if isinstance(lines, str):
        lines = [(lines, size)]
    total = sum(s for _, s in lines) * 1.2
    cy = y + h / 2 - total / 2
    for label, s in lines:
        cy += s * 0.6
        out += text(round(x + w / 2, 1), round(cy, 1), label, s, central=True)
        cy += s * 0.6
    return out


def graph(pos, edges, fills=None, labels=None, r=22, dx=0, dy=0, size=22):
    fills = fills or {}
    labels = labels or {}
    p = {k: (x + dx, y + dy) for k, (x, y) in pos.items()}
    out = [line(*p[a], *p[b]) for a, b in edges]
    out += [node(*p[k], labels.get(k, ""), fills.get(k, WHITE), r, size) for k in p]
    return "".join(out), p


# Slide: Machine Learning is different ---------------------------------------
mini = {"a": (-58, -32), "b": (2, -58), "c": (58, -18), "d": (-42, 38), "e": (28, 46)}
mini_edges = [("a", "b"), ("b", "c"), ("a", "d"), ("d", "e"), ("c", "e"), ("b", "e")]
g1, _ = graph(mini, mini_edges, {k: ORANGE for k in mini}, r=16, dx=380, dy=145)
g2, _ = graph(mini, mini_edges, {k: ORANGE for k in mini}, r=16, dx=620, dy=145)
(OUT / "stats_vs_ml.svg").write_text(doc(1000, 290, "Statistics explains the network; ML uses it", [
    text(245, 42, "Statistics so far", 30, weight="bold"),
    box(25, 110, 185, 70, "Mechanisms"),
    arrow(218, 145, 290, 145),
    g1,
    text(245, 265, "Explain the network", 26),
    line(500, 25, 500, 275, GREY, 2, "8 8"),
    text(755, 42, "Machine learning", 30, weight="bold"),
    g2,
    arrow(710, 145, 782, 145),
    box(790, 110, 185, 70, "Predictions"),
    text(755, 265, "Use the network", 26),
]))

# Slide: You can do "naive" ML -----------------------------------------------
pos = {"A": (60, 115), "B": (60, 245), "C": (145, 180), "D": (230, 180), "E": (315, 180)}
g, _ = graph(pos, [("A", "B"), ("A", "C"), ("B", "C"), ("C", "D"), ("D", "E")], labels={k: k for k in pos})
rows = [("node", "degree", "betweenness", "age"),
        ("A", "2", "0", "24"), ("B", "2", "0", "31"), ("C", "3", "4", "27"),
        ("D", "2", "3", "45"), ("E", "1", "0", "38")]
cols = [(415, 75), (490, 100), (590, 150), (740, 70)]
table = [f'<rect x="415" y="60" width="395" height="40" fill="{LORANGE}"/>\n']
for i, row in enumerate(rows):
    y = 60 + i * 40
    for (x, w), value in zip(cols, row):
        table.append(text(x + w / 2, y + 21, value, 22, weight="bold" if i == 0 else "normal", central=True))
    table.append(line(415, y + 40, 810, y + 40, GREY, 1.5))
table.append(f'<rect x="415" y="60" width="395" height="240" fill="none" stroke="{DARK}" stroke-width="2"/>\n')
(OUT / "naive_ml.svg").write_text(doc(1000, 315, "Centralities and node features as columns for an ML model", [
    text(185, 40, "Network", 26, weight="bold"),
    g,
    arrow(345, 180, 400, 180),
    text(612, 40, "One row per node", 26, weight="bold"),
    *table,
    arrow(820, 180, 858, 180),
    box(865, 140, 125, 80, "ML model", size=24),
]))

# Slide: Most ML tasks are inherently node-level -----------------------------
pos = {1: (90, 120), 2: (190, 65), 3: (205, 195), 4: (315, 115), 5: (335, 255),
       6: (440, 170), 7: (530, 75), 8: (565, 235), 9: (660, 145), 10: (110, 260)}
edges = [(1, 2), (1, 3), (2, 4), (3, 4), (1, 10), (3, 10), (3, 5), (4, 6), (5, 6),
         (6, 7), (6, 8), (7, 9), (8, 9), (5, 8)]
fills = {1: ORANGE, 2: ORANGE, 4: ORANGE, 10: ORANGE, 5: BLUE, 7: BLUE, 8: BLUE, 9: BLUE}
g, _ = graph(pos, edges, fills, {3: "?", 6: "?"}, r=24, size=26)
(OUT / "node_tasks.svg").write_text(doc(1000, 320, "Predicting unknown node outcomes from the graph", [
    g,
    node(770, 95, fill=ORANGE, r=18), text(800, 104, "Outcome: yes", 26, "start"),
    node(770, 160, fill=BLUE, r=18), text(800, 169, "Outcome: no", 26, "start"),
    node(770, 225, "?", r=18, size=22), text(800, 234, "Predict", 26, "start"),
]))

# Slides: Embeddings at a high level / The embedding framework --------------
pos = {"b": (60, 105), "c": (60, 245), "a": (150, 175), "u": (245, 95), "v": (245, 255)}
g, _ = graph(pos, [("u", "a"), ("v", "a"), ("u", "b"), ("v", "c"), ("a", "b"), ("a", "c")],
             {"u": ORANGE, "v": BLUE}, {"u": "u", "v": "v"}, size=24)


def vector(x, y, values, color):
    out = []
    for i, value in enumerate(values):
        opacity = 0.25 + 0.65 * abs(value)
        out.append(f'<rect x="{x + i * 52}" y="{y}" width="52" height="44" fill="{color}" '
                   f'fill-opacity="{opacity:.2f}" stroke="{DARK}" stroke-width="2"/>\n')
        out.append(text(x + i * 52 + 26, y + 23, value, 20, central=True))
    return "".join(out)


zu, zv = [0.8, 0.1, -0.3, 0.5], [0.7, 0.2, -0.2, 0.4]
score = sum(a * b for a, b in zip(zu, zv))
(OUT / "encoder_decoder.svg").write_text(doc(1000, 350, "Encoder maps nodes to vectors; decoder scores pairs", [
    g,
    text(318, 80, "Encoder", 22), arrow(272, 95, 368, 95, "orange"),
    text(318, 240, "Encoder", 22), arrow(272, 255, 368, 255, "blue"),
    text(395, 62, "z", 26, weight="bold"), text(409, 68, "u", 18, weight="bold"),
    vector(380, 73, zu, ORANGE),
    text(395, 222, "z", 26, weight="bold"), text(409, 228, "v", 18, weight="bold"),
    vector(380, 233, zv, BLUE),
    arrow(598, 95, 655, 152), arrow(598, 255, 655, 198),
    box(662, 135, 160, 80, [("Decoder", 26), ("dot product", 19)]),
    arrow(830, 175, 870, 175),
    text(925, 177, f"{score:.2f}", 36, weight="bold", fill=ORANGE, central=True),
    text(500, 330, "Train the encoder so the score matches similarity in the graph", 25),
]))

# Slide: Random walk embeddings ----------------------------------------------
pos = {"A": (250, 80), "B": (250, 220), "C": (370, 150), "D": (490, 80), "E": (490, 220),
       "F": (610, 150), "G": (730, 80), "H": (730, 220)}
edges = [("A", "B"), ("A", "C"), ("B", "C"), ("C", "D"), ("C", "E"), ("D", "E"),
         ("D", "F"), ("E", "F"), ("F", "G"), ("F", "H"), ("G", "H")]
walk = ["A", "C", "D", "F", "G"]
fills = {k: LORANGE for k in walk}
fills["A"] = ORANGE
base = [line(*pos[a], *pos[b]) for a, b in edges]
steps = [node_arrow(pos[a], pos[b], 22, "orange", 6) for a, b in zip(walk, walk[1:])]
nodes = [node(*pos[k], k, fills.get(k, WHITE)) for k in pos]
chips = [text(300, 309, "Walk:", 26, "end", "bold")]
for i, k in enumerate(walk):
    x = 330 + i * 80
    chips.append(box(x, 285, 58, 46, k, LORANGE, size=24))
    if i < len(walk) - 1:
        chips.append(arrow(x + 62, 308, x + 76, 308, width=3))
(OUT / "random_walk.svg").write_text(doc(1000, 385, "A random walk produces a sequence of nodes", [
    *base, *steps, *nodes, *chips,
    text(500, 370, "Nodes that often appear together on walks get similar embeddings", 25),
]))

# Slide: DeepWalk -------------------------------------------------------------
words = ["the", "cat", "sat", "on", "the", "mat"]
out = [text(20, 104, "Sentence", 26, "start", "bold"), text(20, 244, "Walk", 26, "start", "bold")]
for i, word in enumerate(words):
    out.append(box(150 + i * 70, 72, 62, 46, word, "#EEEEEE", size=22))
for i, k in enumerate(walk):
    out.append(node(181 + i * 70, 235, k, LORANGE, r=24))
out += [
    arrow(578, 95, 632, 130), arrow(530, 235, 632, 200),
    box(640, 65, 160, 205, [("skip-gram", 26), ("(word2vec)", 21)], LORANGE),
    arrow(808, 95, 848, 95), text(856, 104, "word vectors", 25, "start"),
    arrow(808, 235, 848, 235), text(856, 244, "node vectors", 25, "start"),
]
(OUT / "deepwalk_corpus.svg").write_text(doc(1000, 300, "DeepWalk treats random walks like sentences", out))

# Slide: Node2Vec -------------------------------------------------------------
pos = {"u": (170, 170), "s1": (80, 95), "s2": (70, 235), "s3": (205, 280), "s4": (290, 100),
       "s5": (410, 170), "s6": (530, 95), "s7": (625, 200), "x1": (330, 270), "x2": (505, 270)}
edges = [("u", "s1"), ("u", "s2"), ("u", "s3"), ("u", "s4"), ("s1", "s2"), ("s4", "s5"),
         ("s5", "s6"), ("s6", "s7"), ("s5", "x2"), ("x2", "s7"), ("s3", "x1"), ("x1", "s5")]
base = [line(*pos[a], *pos[b]) for a, b in edges]
bfs = [node_arrow(pos["u"], pos[k], 22, "blue", 6) for k in ("s1", "s2", "s3")]
dfs = [node_arrow(pos[a], pos[b], 22, "orange", 6) for a, b in (("u", "s4"), ("s4", "s5"), ("s5", "s6"))]
fills = {"u": DARK, "s1": LBLUE, "s2": LBLUE, "s3": LBLUE, "s4": LORANGE, "s5": LORANGE, "s6": LORANGE}
nodes = [node(*pos[k], "", fills.get(k, WHITE)) for k in pos]
nodes.append(text(*pos["u"], "u", 24, weight="bold", fill=WHITE, central=True))
(OUT / "node2vec_bfs_dfs.svg").write_text(doc(1000, 320, "BFS-like walks stay local; DFS-like walks move away", [
    *base, *bfs, *dfs, *nodes,
    arrow(668, 125, 730, 125, "blue", 6), text(745, 134, "BFS: nearby nodes", 26, "start"),
    arrow(668, 205, 730, 205, "orange", 6), text(745, 214, "DFS: far-away nodes", 26, "start"),
]))

# Slide: Task-specific embeddings, end to end --------------------------------
small = {1: (-95, -50), 2: (-15, -78), 3: (75, -45), 4: (-75, 50), 5: (15, 15), 6: (95, 60)}
small_edges = [(1, 2), (2, 3), (1, 4), (4, 5), (2, 5), (3, 5), (5, 6), (3, 6)]
out = []
for i, (title, question) in enumerate([("Node level", "What is this node's label?"),
                                       ("Edge level", "Will these two connect?"),
                                       ("Graph level", "What kind of graph is this?")]):
    cx = 170 + i * 330
    out.append(text(cx, 40, title, 30, weight="bold"))
    out.append(text(cx, 295, question, 23))
    if i:
        out.append(line(cx - 165, 25, cx - 165, 305, GREY, 2, "8 8"))
    fills, labels = {}, {}
    if i == 0:
        fills, labels = {5: ORANGE}, {5: "?"}
    elif i == 1:
        fills = {4: LORANGE, 6: LORANGE}
    else:
        fills = {k: LORANGE for k in small}
    g, p = graph(small, small_edges, fills, labels, r=19, dx=cx, dy=160)
    if i == 1:
        out.append(line(*p[4], *p[6], ORANGE, 5, "10 8"))
        mx, my = (p[4][0] + p[6][0]) / 2, (p[4][1] + p[6][1]) / 2 + 30
        out.append(text(mx, my, "?", 30, weight="bold", fill=ORANGE, central=True))
    out.append(g)
    if i == 2:
        out.append(f'<rect x="{cx - 130}" y="65" width="260" height="190" rx="16" fill="none" '
                   f'stroke="{ORANGE}" stroke-width="4" stroke-dasharray="10 8"/>\n')
        out.append(node(cx + 130, 65, "?", ORANGE, 20, 24))
(OUT / "task_levels.svg").write_text(doc(1000, 315, "Node-, edge-, and graph-level prediction tasks", out))

# Slide: GCNs are generalized CNNs -------------------------------------------
cells = []
for row in range(5):
    for col in range(5):
        d = max(abs(row - 2), abs(col - 2))
        fill = ORANGE if d == 0 else LORANGE if d == 1 else WHITE
        cells.append(f'<rect x="{60 + col * 46}" y="{65 + row * 46}" width="46" height="46" '
                     f'fill="{fill}" stroke="{DARK}" stroke-width="2"/>\n')
pos = {"c": (700, 175), "n1": (600, 110), "n2": (805, 105), "n3": (735, 270), "o1": (520, 200),
       "o2": (895, 175), "o3": (865, 265), "o4": (615, 260), "o5": (705, 85)}
edges = [("c", "n1"), ("c", "n2"), ("c", "n3"), ("n1", "o1"), ("n2", "o2"), ("n3", "o3"),
         ("o2", "o3"), ("n3", "o4"), ("o1", "o4"), ("n1", "o5"), ("n2", "o5")]
g, _ = graph(pos, edges, {"c": ORANGE, "n1": LORANGE, "n2": LORANGE, "n3": LORANGE})
(OUT / "cnn_vs_gcn.svg").write_text(doc(1000, 345, "CNN neighborhoods are grid cells; GCN neighborhoods are edges", [
    text(175, 40, "Image", 30, weight="bold"),
    *cells,
    text(175, 330, "CNN: always 8 neighbors", 25),
    line(450, 25, 450, 335, GREY, 2, "8 8"),
    text(705, 40, "Graph", 30, weight="bold"),
    g,
    text(705, 330, "GCN: neighbors are defined by edges", 25),
]))

print("\n".join(sorted(str(p) for p in OUT.glob("*.svg"))))
