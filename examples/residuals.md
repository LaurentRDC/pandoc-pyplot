---
title: test of pandoc-pyplot
date: 2018-09-28
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

```{.pyplot target=images/test.png}
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

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

```{.pyplot target=images/test2.jpg caption="This should be a caption"}
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 10, 1024)
y = 10*np.sin(2*np.pi*x)
n = np.random.random(size = x.shape) - 0.5

# First create an empty figure
fig, (ax1, ax2) = plt.subplots(nrows=2, ncols=1, sharex=True)

# Plot both noisy signal and expected sinusoid
ax1.plot(x, y + n, 'g.')
ax1.plot(x, y, 'k-')

# Residuals
ax2.plot(x, n, '.g')
ax2.axhline(y=0, color='k')

# Plot formatting
ax2.set_xlabel('Abcissa')
ax1.set_title('Residuals')
```

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

```{.pyplot target=images/test3.jpg include=style.py}
import numpy as np

x = np.linspace(0, 10, 1024)
y = 10*np.cos(np.pi*x)
n = (np.random.random(size = x.shape) - 0.5) * 5

# First create an empty figure
fig, (ax1, ax2) = plt.subplots(nrows=2, ncols=1, sharex=True)

# Plot both noisy signal and expected sinusoid
ax1.plot(x, y + n, 'b.')
ax1.plot(x, y, 'k-')

# Residuals
ax2.plot(x, n, '.b')
ax2.axhline(y=0, color='k')

# Plot formatting
ax2.set_xlabel('Abcissa')
ax1.set_title('Residuals in ggplot style')
```

```{.pyplot include=style.py}
import numpy as np

x = np.linspace(0, 10, 1024)
y = 10*np.cos(np.pi*x)
n = (np.random.random(size = x.shape) - 0.5) * 5

# First create an empty figure
fig, (ax1, ax2) = plt.subplots(nrows=2, ncols=1, sharex=True)

# Plot both noisy signal and expected sinusoid
ax1.plot(x, y + n, 'b.')
ax1.plot(x, y, 'k-')

# Residuals
ax2.plot(x, n, '.b')
ax2.axhline(y=0, color='k')

# Plot formatting
ax2.set_xlabel('Abcissa')
ax1.set_title('Residuals in ggplot style')
```