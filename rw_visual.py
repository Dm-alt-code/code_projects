import matplotlib.pyplot as plt
from random_walk import RandomWalk
# в matplotlib  встановлено, що роздільна здатність екрана 100 пікселів на дюйм

while True:
    rw = RandomWalk(50000)  # екземпляр класу
    rw.fill_walk()  # екземпляр.метод

    keep_running = input("Змоделювати нове блукання (y/n):")
    if keep_running == 'n':
        break

    fig, ax = plt.subplots(figsize=(10, 6), dpi=128, layout='constrained')
    point_numbers = range(rw.num_points)  # екземпляр.атрибут
    ax.scatter(rw.x_values, rw.y_values, c=point_numbers,
               cmap=plt.cm.Blues, edgecolor='none', s=5)  # екземпляр.атрибут

    # виокремити першу та останню точку
    ax.scatter(0, 0, c='green', edgecolor='black', s=50)
    ax.scatter(rw.x_values[-1], rw.y_values[-1], c='red', edgecolor='black', s=50)

    # приховати вісі
    ax.get_xaxis().set_visible(False)  # видмість осі = False
    ax.get_yaxis().set_visible(False)  # видмість осі = False

    plt.savefig('random_walk.png')
    plt.show()
