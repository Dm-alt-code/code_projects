'''Імпортування багатьох класів з одного модуля'''
from car import Car, ElectricCar
# from car import Car as c
# from car import ElectricCar as EC

my_beetle = Car('volkswagen', 'beetle', 2019)
print(my_beetle.get_descriptive_name())

my_tesla = ElectricCar('tesla', 'roadster', 2019)
print(my_tesla.get_descriptive_name())
