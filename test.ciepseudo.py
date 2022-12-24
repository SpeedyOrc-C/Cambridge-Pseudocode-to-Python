score: list[int]
score = [... for _ in range(1, 5+1)]

for i in range(0, 4+1):
    s = input()
    score[i] = int(s)

mean: float
mean = 0
for i in range(0, 4+1):
    mean = mean + score[i]
mean = mean / 5

print("Mean score:", mean)
