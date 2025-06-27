import matplotlib.pyplot as plt
import matplotlib.patches as patches

# 定义11种立方体展开图，每个图形由6个方块的位置（x, y）组成
unfoldings = [
    [(1, 0), (0, 1), (1, 1), (2, 1), (3, 1), (1, 2)],
    [(0, 0), (1, 0), (2, 0), (1, 1), (1, 2), (2, 2)],
    [(0, 1), (1, 1), (2, 1), (2, 0), (2, 2), (3, 2)],
    [(0, 1), (1, 1), (2, 1), (3, 1), (3, 0), (3, 2)],
    [(1, 0), (1, 1), (1, 2), (0, 2), (2, 2), (2, 3)],
    [(1, 0), (1, 1), (1, 2), (2, 2), (3, 2), (3, 3)],
    [(1, 0), (1, 1), (1, 2), (2, 2), (2, 3), (2, 4)],
    [(0, 1), (1, 1), (1, 2), (2, 2), (2, 1), (3, 1)],
    [(0, 1), (1, 1), (1, 2), (2, 2), (2, 3), (3, 3)],
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (3, 2)],
    [(1, 0), (1, 1), (0, 1), (1, 2), (2, 2), (1, 3)]
]

# 绘图函数
def plot_unfoldings(unfoldings):
    fig, axs = plt.subplots(3, 4, figsize=(12, 9))
    axs = axs.flatten()

    for i, unfolding in enumerate(unfoldings):
        ax = axs[i]
        for x, y in unfolding:
            rect = patches.Rectangle((x, y), 1, 1, linewidth=1, edgecolor='black', facecolor='lightblue')
            ax.add_patch(rect)
        ax.set_xlim(-1, 5)
        ax.set_ylim(-1, 6)
        ax.set_aspect('equal')
        ax.set_xticks([])
        ax.set_yticks([])
        ax.set_title(f'展开图 {i+1}')

    for j in range(len(unfoldings), len(axs)):
        axs[j].axis('off')

    plt.tight_layout()
    plt.show()

plot_unfoldings(unfoldings)
