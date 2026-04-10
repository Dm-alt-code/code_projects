'''Імпортування одного класу'''

from car import Car

# Створення екземпляру класу
my_new_car = Car('audi', 'a4', 2019)
print(my_new_car.get_descriptive_name())

my_new_car.odometer = 250
my_new_car.road_odometer()
