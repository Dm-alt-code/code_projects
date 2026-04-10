from typing import Callable, Any
import sys
import re
import math
try:
	import tkinter as tk
except Exception:
	# Tkinter may be unavailable in some environments; CLI will still work with --cli
	tk = None  # type: ignore


def add(x: float, y: float) -> float:
	return x + y


def subtract(x: float, y: float) -> float:
	return x - y


def multiply(x: float, y: float) -> float:
	return x * y


def divide(x: float, y: float) -> float:
	if y == 0:
		raise ZeroDivisionError("Деление на ноль невозможно")
	return x / y


def read_number(prompt: str) -> float:
	while True:
		value = input(prompt).strip().replace(",", ".")
		try:
			return float(value)
		except ValueError:
			print("Введите корректное число. Пример: 12.5 или 7")


def choose_operation() -> tuple[str, Callable[[float, float], float]]:
	operations: dict[str, tuple[str, Callable[[float, float], float]]] = {
		"1": ("Сложение", add),
		"2": ("Вычитание", subtract),
		"3": ("Умножение", multiply),
		"4": ("Деление", divide),
	}

	print("\nВыберите операцию:")
	for key, (name, _) in operations.items():
		print(f" {key}) {name}")
	print(" 0) Выход")

	while True:
		choice = input("Ваш выбор: ").strip()
		if choice == "0":
			return ("Выход", lambda _x, _y: 0.0)
		if choice in operations:
			name, func = operations[choice]
			return (name, func)
		print("Неверный выбор. Попробуйте снова.")


def main() -> None:
	# If --cli is passed, run interactive CLI, otherwise start GUI (preferred)
	if "--cli" in sys.argv or tk is None:
		print("Простой калькулятор (сложение, вычитание, умножение, деление)")
		while True:
			name, operation = choose_operation()
			if name == "Выход":
				print("До свидания!")
				break

			x = read_number("Введите первое число: ")
			y = read_number("Введите второе число: ")

			try:
				result = operation(x, y)
				print(f"Результат ({name}): {result}\n")
			except ZeroDivisionError as error:
				print(f"Ошибка: {error}\n")
		return

	# Launch GUI
	app = CalculatorGUI()
	app.run()


# --------------------------- GUI IMPLEMENTATION ---------------------------

class CalculatorGUI:
	def __init__(self) -> None:
		if tk is None:
			raise RuntimeError("Tkinter недоступен в этой среде")
		self.root = tk.Tk()
		self.root.title("Калькулятор")
		self.root.resizable(False, False)

		self.display = tk.Entry(self.root, font=("Segoe UI", 18), justify="right", bd=8, relief="sunken")
		self.display.grid(row=0, column=0, columnspan=4, sticky="nsew", padx=8, pady=8)

		# Configure grid
		for i in range(4):
			self.root.grid_columnconfigure(i, weight=1)
		for i in range(1, 8):
			self.root.grid_rowconfigure(i, weight=1)

		self._create_buttons()

		# Function input and plotting UI
		self.func_label = tk.Label(self.root, text="f(x)=", font=("Segoe UI", 12))
		self.func_label.grid(row=6, column=0, sticky="w", padx=8)
		self.func_entry = tk.Entry(self.root, font=("Segoe UI", 12))
		self.func_entry.grid(row=6, column=1, columnspan=2, sticky="nsew", padx=4)
		self.plot_button = tk.Button(self.root, text="График", font=("Segoe UI", 12), command=self.plot_function)
		self.plot_button.grid(row=6, column=3, sticky="nsew", padx=4)

		self.canvas_width = 400
		self.canvas_height = 300
		self.canvas = tk.Canvas(self.root, width=self.canvas_width, height=self.canvas_height, bg="white", bd=2, relief="sunken")
		self.canvas.grid(row=7, column=0, columnspan=4, sticky="nsew", padx=8, pady=(0, 8))

	def _create_buttons(self) -> None:
		btn_specs = [
			{"text": "C", "row": 1, "col": 0, "cmd": self.clear, "style": {"fg": "#cc0000"}},
			{"text": "/", "row": 1, "col": 1, "cmd": lambda: self.add_char("/")},
			{"text": "*", "row": 1, "col": 2, "cmd": lambda: self.add_char("*")},
			{"text": "-", "row": 1, "col": 3, "cmd": lambda: self.add_char("-")},

			{"text": "7", "row": 2, "col": 0, "cmd": lambda: self.add_char("7")},
			{"text": "8", "row": 2, "col": 1, "cmd": lambda: self.add_char("8")},
			{"text": "9", "row": 2, "col": 2, "cmd": lambda: self.add_char("9")},
			{"text": "+", "row": 2, "col": 3, "cmd": lambda: self.add_char("+")},

			{"text": "4", "row": 3, "col": 0, "cmd": lambda: self.add_char("4")},
			{"text": "5", "row": 3, "col": 1, "cmd": lambda: self.add_char("5")},
			{"text": "6", "row": 3, "col": 2, "cmd": lambda: self.add_char("6")},
			{"text": "=", "row": 3, "col": 3, "rowspan": 3, "cmd": self.evaluate, "style": {"bg": "#2e7d32", "fg": "white"}},

			{"text": "1", "row": 4, "col": 0, "cmd": lambda: self.add_char("1")},
			{"text": "2", "row": 4, "col": 1, "cmd": lambda: self.add_char("2")},
			{"text": "3", "row": 4, "col": 2, "cmd": lambda: self.add_char("3")},

			{"text": "0", "row": 5, "col": 0, "colspan": 2, "cmd": lambda: self.add_char("0")},
			{"text": ".", "row": 5, "col": 2, "cmd": lambda: self.add_char(".")},
		]

		for spec in btn_specs:
			btn = tk.Button(
				self.root,
				text=spec["text"],
				font=("Segoe UI", 14),
				command=spec["cmd"],
				bd=4,
				relief="raised",
			)
			style = spec.get("style")
			if style:
				if "bg" in style:
					btn.configure(bg=style["bg"]) 
				if "fg" in style:
					btn.configure(fg=style["fg"]) 
			rowspan = spec.get("rowspan", 1)
			colspan = spec.get("colspan", 1)
			btn.grid(row=spec["row"], column=spec["col"], rowspan=rowspan, columnspan=colspan, sticky="nsew", padx=4, pady=4)

	def add_char(self, ch: str) -> None:
		current = self.display.get()
		# Prevent duplicate operators and start with operator
		if ch in "+-*/":
			if not current:
				return
			if current[-1] in "+-*/":
				current = current[:-1]
		self.display.delete(0, tk.END)
		self.display.insert(0, current + ch)

	def clear(self) -> None:
		self.display.delete(0, tk.END)

	def evaluate(self) -> None:
		expr = self.display.get()
		if not expr:
			return
		# Only allow digits, operators, dot, and spaces
		if not re.fullmatch(r"[\d+\-*/\.\s]+", expr):
			self._set_error("Ошибка")
			return
		try:
			# Evaluate in restricted namespace
			result = eval(expr, {"__builtins__": None}, {})
			if result == float("inf") or result == float("-inf"):
				raise ZeroDivisionError
			self.display.delete(0, tk.END)
			self.display.insert(0, str(result))
		except ZeroDivisionError:
			self._set_error("Деление на ноль")
		except Exception:
			self._set_error("Ошибка")

	def _set_error(self, message: str) -> None:
		self.display.delete(0, tk.END)
		self.display.insert(0, message)

	def run(self) -> None:
		self.root.mainloop()

	def _get_allowed_namespace(self) -> dict[str, Any]:
		# Allow safe math functions and constants, plus variable x
		allowed: dict[str, Any] = {name: obj for name, obj in math.__dict__.items() if not name.startswith("_")}
		# Common aliases
		allowed.update({
			"ln": math.log,
			"tg": math.tan,
			"ctg": lambda x: 1.0 / math.tan(x),
		})
		return allowed

	def _draw_axes(self, x_min: float, x_max: float, y_min: float, y_max: float) -> None:
		self.canvas.delete("all")
		# Map helpers
		def map_x(x: float) -> float:
			return (x - x_min) * (self.canvas_width / (x_max - x_min))
		def map_y(y: float) -> float:
			return self.canvas_height - (y - y_min) * (self.canvas_height / (y_max - y_min))

		# Axes
		zero_x = map_x(0.0)
		zero_y = map_y(0.0)
		# X axis
		if 0.0 >= y_min and 0.0 <= y_max:
			self.canvas.create_line(0, zero_y, self.canvas_width, zero_y, fill="#9e9e9e")
		# Y axis
		if 0.0 >= x_min and 0.0 <= x_max:
			self.canvas.create_line(zero_x, 0, zero_x, self.canvas_height, fill="#9e9e9e")

		# Border
		self.canvas.create_rectangle(1, 1, self.canvas_width - 1, self.canvas_height - 1, outline="#e0e0e0")

	def plot_function(self) -> None:
		expr = self.func_entry.get().strip()
		if not expr:
			return
		# Replace caret with power if user typed ^
		expr = expr.replace("^", "**")
		# Plot range
		x_min, x_max = -10.0, 10.0
		y_min, y_max = -10.0, 10.0
		self._draw_axes(x_min, x_max, y_min, y_max)

		allowed = self._get_allowed_namespace()
		points: list[tuple[float, float]] = []
		steps = self.canvas_width
		for i in range(steps):
			x = x_min + (x_max - x_min) * (i / (steps - 1))
			local_ns = {"x": x}
			try:
				value = eval(expr, {"__builtins__": None}, {**allowed, **local_ns})
				if not isinstance(value, (int, float)) or math.isnan(value) or math.isinf(value):
					raise ValueError
			except Exception:
				points.append((None, None))
				continue
			points.append((x, float(value)))

		# Map and draw polyline, breaking at discontinuities
		def map_x(x: float) -> float:
			return (x - x_min) * (self.canvas_width / (x_max - x_min))
		def map_y(y: float) -> float:
			return self.canvas_height - (y - y_min) * (self.canvas_height / (y_max - y_min))

		segment: list[float] = []
		prev_bad = True
		for x, y in points:
			if x is None or y is None:
				if not prev_bad and len(segment) >= 4:
					self.canvas.create_line(*segment, fill="#1976d2", width=2, smooth=True)
				segment = []
				prev_bad = True
				continue
			# Clip extremely large values
			if y < y_min or y > y_max:
				if not prev_bad and len(segment) >= 4:
					self.canvas.create_line(*segment, fill="#1976d2", width=2, smooth=True)
				segment = []
				prev_bad = True
				continue
			xc, yc = map_x(x), map_y(y)
			segment.extend([xc, yc])
			prev_bad = False

		if not prev_bad and len(segment) >= 4:
			self.canvas.create_line(*segment, fill="#1976d2", width=2, smooth=True)

if __name__ == "__main__":
	main()
