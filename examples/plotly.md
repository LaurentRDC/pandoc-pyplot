---
title: Example of pandoc-pyplot working with Plotly
---

This example shows a document with Python figures that can be rendered with Plotly.

```{.plotly caption="Example taken from Plotly's tutorial"}
import plotly.graph_objects as go
import numpy as np
np.random.seed(1)

N = 100
x = np.random.rand(N)
y = np.random.rand(N)
colors = np.random.rand(N)
sz = np.random.rand(N) * 30

# Inside a {.plotly} clause, you can only have one Figure.
# pandoc-pyplot will automagically find it, no matter its name, and save
# it to file
fig = go.Figure()
fig.add_trace(go.Scatter(
    x=x,
    y=y,
    mode="markers",
    marker=go.scatter.Marker(
        size=sz,
        color=colors,
        opacity=0.6,
        colorscale="Viridis"
    )
))
```