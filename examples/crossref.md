---
title: Compatibility with pandoc-crossref
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

```{#fig:myfigure .pyplot include="style.py" caption="This is a caption"}
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 1024)
y = 10*np.sin(2*np.pi*x)
n = np.random.random(size = x.shape) - 0.5

# First create an empty figure
plt.figure()

# Plot both noisy signal and expected sinusoid
plt.plot(x, y + n, 'r.')
plt.plot(x, y, 'k-')

# Plot formatting
plt.xlabel('Abcissa')
plt.ylabel('Ordinates')
plt.title('Sinusoid')
```

As you can see in @fig:myfigure, pandoc-crossref is compatible with the ouput of pandoc-pyplot.