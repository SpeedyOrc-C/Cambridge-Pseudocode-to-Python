pi = 0
n = 100
i = 1

while True:
    pi = pi + 1 / i ** 2
    if i % 2 == 0:
        print("哈哈哈")
    i = i + 1
    if i > n: break

print((pi * 6) ** 0.5)
