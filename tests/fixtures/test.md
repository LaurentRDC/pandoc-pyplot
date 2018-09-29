---
title: test of pandoc-pyplot
---

This is a paragraph.

```{plot_target=test.png plot_alt="yeya many words" plot_caption="this is a caption"}
import matplotlib.pyplot as plt

plt.figure()
plt.plot([1,2,3,4,5], [1,2,3,4,5])
```
