'''Клас для моделювання машини'''

class Car:
  '''Спроба змоделювати машину'''

  def __init__(self, make, model, year):
    '''Ініціалізація атрбутів, що описують машину'''
    self.make = make
    self.model = model
    self.year = year
    self.odometer = 200

  def get_descriptive_name(self):
    '''Повертає відповідну інформацію'''
    long_name = f"{self.year} {self.make} {self.model}"
    return long_name.title()

  def road_odometer(self):
    '''Вивести повідомлення з пробігом машини'''
    print(f"This car has {self.odometer} miles on it.")

  def update_odometer(self, mileage):
    '''Задати значення одометра'''
    if mileage >= self.odometer:
      self.odometer = mileage
    else:
      print('You can\'t roll back an odometer')

  def increment_odometer(self, miles):
    '''Додати значення до показника одометра'''
    self.odometer += miles


class Battery:
  '''Спроба змоделювати акумулятор електрокара'''

  def __init__(self, battery_size=75):
    '''Ініціалізація атрибутів акумулятор'''
    self.battery_size = battery_size

  def describe_battery(self):
    '''Вивести повідомлення про розмір акумулятора'''
    print(f"This car has a {self.battery_size}-KWh battery.")


class ElectricCar(Car):

  def __init__(self, make, model, year):
    '''Започаткувати атрибути батькісвького класу'''
    # super - супер клас (батьківський клас)
    super().__init__(make, model, year)
    self.battery = Battery()
