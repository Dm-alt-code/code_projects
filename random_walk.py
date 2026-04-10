from random import choice


class RandomWalk:
    '''Клас, що генерує випадкове блукання'''

    def __init__(self, num_points=5000):
        '''ініціалізація атрибутів блуканняя'''
        self.num_points = num_points
        # всі блукання починаються з координати (0,0)
        self.x_values = [0]
        self.y_values = [0]

    def fill_walk(self):
        '''Обчислити всі точки блукання'''
        while len(self.x_values) < self.num_points:
            # Виріщуємо в якому напрямку рухатись і як довго рухатись
            x_direction = choice([1, -1])  # блукання праворуч (1) чи ліворуч (-1)
            x_distance = choice([0, 1, 2, 3, 4])  # як далеко просунтися в цьому напрямку
            x_step = x_direction * x_distance  # крок вздовж по вісі Ox

            y_direction = choice([1, -1])  # блукання вгору (1) чи вниз (-1)
            y_distance = choice([0, 1, 2, 3, 4])  # як далеко просунтися в цьому напрямку
            y_step = y_direction * y_distance  # крок вздовж по вісі Oy

            # Відкинути кроки, що нікуди не просуваються
            if x_step == 0 and y_step == 0:
                continue

            # Розрахувати нові координати пилкового зернятка
            x = self.x_values[-1] + x_step
            y = self.y_values[-1] + y_step

            self.x_values.append(x)
            self.y_values.append(y)
