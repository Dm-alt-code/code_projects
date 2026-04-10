from car import ElectricCar

# створеня екземпляру класу
my_tesla = ElectricCar('tesla', 'model s', 2019)
print(my_tesla.get_descriptive_name())
# звернення до атрибуту battery, а потім виклик методу класу Battery
my_tesla.battery.describe_battery()
