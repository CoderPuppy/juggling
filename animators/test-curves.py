import matplotlib.pyplot as plt
import numpy as np

def bezier(points, i, t):
    return (0
        + 1 * (1 - t)**3 * t**0 * points[i + 0]
        + 3 * (1 - t)**2 * t**1 * points[i + 1]
        + 3 * (1 - t)**1 * t**2 * points[i + 2]
        + 1 * (1 - t)**0 * t**3 * points[i + 3]
    )

def bezier_deriv(points, i, t):
    return (0
        + 1 * (-3 * t**2 + 6 * t - 3) * points[i + 0]
        + 3 * ( 3 * t**2 - 4 * t + 1) * points[i + 1]
        + 3 * (-3 * t**2 + 2 * t    ) * points[i + 2]
        + 1 * ( 3 * t**2            ) * points[i + 3]
    )

def bezier_deriv2(points, i, t):
    return (0
        + 1 * (-6 * t + 6) * points[i + 0]
        + 3 * ( 6 * t - 4) * points[i + 1]
        + 3 * (-6 * t + 2) * points[i + 2]
        + 1 * ( 6 * t    ) * points[i + 3]
    )

def integrate(f, t_start, t_end, *, num_points=33, error=0.001):
    while num_points < 16384:
        t, step = np.linspace(t_start, t_end, num=num_points * 2 - 1, retstep=True)
        t = t.reshape((num_points * 2 - 1, 1))
        left = np.trapz(f(t[0:num_points]), dx=step)
        right = np.trapz(f(t[num_points-1:]), dx=step)
        full = np.trapz(f(t[0::2]), dx=step * 2)
        if full.shape:
            diff = np.linalg.norm(left + right - full, ord=2)
        else:
            diff = np.fabs(left + right - full)
        if diff < error:
            break
        num_points *= 2
        num_points -= 1
    return full

def arclen(points, i, t_start = 0, t_end = 1, *, num_points=33, error=0.001):
    return integrate(
        lambda t: np.linalg.norm(bezier_deriv(points, i, t), ord=2, axis=1),
        t_start, t_end, num_points=num_points, error=error
    )

points = np.array([
             [ 0, 0], [ 1, 0],
    [ 2, 3], [ 3, 3], [ 4, 3],
    [ 5, 2], [ 6, 2], [ 7, 2],
    [ 8, 3], [ 9, 3], [10, 3],
    [11, 0], [12, 0]
])
n = (points.shape[0] - 1)/3
assert n.is_integer()
n = int(n)

def eval_spline(points, t, *, f):
    i = int(t)
    if t * 3 + 1 == len(points):
        i -= 1
    assert i >= 0
    assert i * 3 + 3 < len(points)
    return f(points, i * 3, t - i)

curve_lengths = [arclen(points, i * 3, 0, 1) for i in range(n)]
spline_length = sum(curve_lengths)
print("lengths", spline_length, curve_lengths)

def invert_arclen(l, *, error=0.001):
    # print("invert_arclen", l)
    for i, curve_l in enumerate(curve_lengths):
        if l < curve_l:
            break
        l -= curve_l
    else:
        return i + 1
    t_start = 0
    t_end = 1
    l_diff = curve_l
    # print("invert_arclen", i, l)
    while True:
        # print(f"range([{t_start},{t_end}]) = {l_diff}")
        if l_diff < 0 or t_end < t_start:
            raise ValueError("Bad range")
        t = l/l_diff * (t_end - t_start) + t_start
        guess_l = arclen(points, i * 3, t_start, t)
        # print(f"guess({t}) = {guess_l} <=> {l}")
        if not (0 <= t <= 1):
            raise ValueError("Bad guess")
        if np.fabs(guess_l) <= error or np.fabs(guess_l - l) < error:
            return i + t
        elif guess_l < l:
            t_start = t
            l -= guess_l
            l_diff -= guess_l
        else:
            t_end = t
            l_diff = guess_l

n_spaced = 63
dist_spaced = spline_length / (n_spaced - 1)
spaced_ls = [dist_spaced * i for i in range(n_spaced)]
spaced_ts = [invert_arclen(l) for l in spaced_ls]
# print(spaced_ls)
print('spaced_ts', spaced_ts)
spaced_vs = [eval_spline(points, t, f=bezier) for t in spaced_ts]
print('spaced_vs', spaced_vs)

n_vars = 3 * (n_spaced - 1) # velocity, acceleration and jerk for each curve
n_eqs = 3 * (n_spaced - 2) + 3 # position, velocity, accleration join for each internal point, starting and ending velocities, last position connects
assert n_vars == n_eqs

matrix = np.zeros((n_vars, n_eqs))
values = np.zeros((n_eqs, 2))
for i in range(0, n_spaced - 2):
    before = spaced_vs[i]
    after = spaced_vs[i + 1]

    # C₀: rᵢ₊₁ - rᵢ = vᵢ + aᵢ + jᵢ
    matrix[i * 3 + 0, i * 3 + 0] = 1
    matrix[i * 3 + 0, i * 3 + 1] = 1
    matrix[i * 3 + 0, i * 3 + 2] = 1
    values[i * 3 + 0] = after - before

    # C₁: vᵢ + 2a₎ + 3jᵢ - vᵢ₊₁ = 0
    matrix[i * 3 + 1, i * 3 + 0] = 1
    matrix[i * 3 + 1, i * 3 + 1] = 2
    matrix[i * 3 + 1, i * 3 + 2] = 3
    matrix[i * 3 + 1, (i + 1) * 3 + 0] = -1

    # C₂: 2aᵢ + 6jᵢ - 2aᵢ₌₁ = 0
    matrix[i * 3 + 2, i * 3 + 1] = 1
    matrix[i * 3 + 2, i * 3 + 2] = 3
    matrix[i * 3 + 2, (i + 1) * 3 + 1] = -1

def normalize(a, *, ord=None, axis=None):
    return a / np.linalg.norm(a, ord=ord, axis=axis)

# align starting velocity
matrix[n_eqs - 3, 0] = 1
values[n_eqs - 3] = dist_spaced * normalize(bezier_deriv(points, 0, 0))

# align ending velocity
matrix[n_eqs - 2, (n_spaced - 2) * 3 + 0] = 1
matrix[n_eqs - 2, (n_spaced - 2) * 3 + 1] = 2
matrix[n_eqs - 2, (n_spaced - 2) * 3 + 2] = 3
values[n_eqs - 2] = dist_spaced * normalize(bezier_deriv(points, len(points) - 4, 1))

# connect ending point
matrix[n_eqs - 1, (n_spaced - 2) * 3 + 0] = 1
matrix[n_eqs - 1, (n_spaced - 2) * 3 + 1] = 1
matrix[n_eqs - 1, (n_spaced - 2) * 3 + 2] = 1
values[n_eqs - 1] = spaced_vs[-1] - spaced_vs[-2]

with np.printoptions(threshold = 10000):
    print('matrix', matrix)
    print('values', values)
spaced_spline = np.linalg.solve(matrix, values)
print('solved', spaced_spline)

def eval_spaced(i, t):
    return spaced_vs[i] + t * spaced_spline[3 * i] + t**2 * spaced_spline[3 * i + 1] + t**3 * spaced_spline[3 * i + 2]

# spaced_bezier_points = np.zeros((n_spaced * 3 + 1, 2))
# for i, t in enumerate(spaced_ts):
#     spaced_v = eval_spline(points, t, f=bezier)
#     print(t, spaced_v, spaced_d)
#     print(i * 3, spaced_v)
#     spaced_bezier_points[i * 3] = spaced_v
#     if i > 0:
#         print(i * 3 - 1, spaced_v - spaced_d/3)
#         spaced_bezier_points[i * 3 - 1] = spaced_v - spaced_d/3
#     if i < n_spaced - 1:
#         print(i * 3 + 1, spaced_v + spaced_d/3)
#         spaced_bezier_points[i * 3 + 1] = spaced_v + spaced_d/3
# print(spaced_bezier_points)

def plot_spline(points, *, f):
    return np.concatenate([
        f(points, i, np.linspace(0, 1, num=100).reshape((100, 1)))
        for i in range(0, len(points) - 1, 3)
    ])
# curve_ts = []
# curve_vs = []
# curve_ds = []
# curve_dds = []
# for i in range(n):
    # print(i * 3)
#     curve_t = np.linspace(0, 1, num=100).reshape((100, 1))
#     curve_ts.append(curve_t + i)
#     curve_vs.append(bezier(points, i * 3, curve_t))
#     curve_ds.append(bezier_deriv(points, i * 3, curve_t))
#     curve_dds.append(bezier_deriv2(points, i * 3, curve_t))
# spline_t = np.concatenate(curve_ts)
# spline_v = np.concatenate(curve_vs)
# spline_d = np.concatenate(curve_ds)
# spline_dd = np.concatenate(curve_dds)

def split_axes(data):
    return data[:, 0], data[:, 1]

print(np.concatenate([
    eval_spaced(i, np.linspace(0, 1, num=100).reshape((100, 1)))
    for i in range(n_spaced - 1)
]))

# fig = plt.figure()
# ax = fig.subplots()
# ax.plot(*split_axes(plot_spline(points, f=bezier)))
# ax.plot(*split_axes(np.concatenate([
#     eval_spaced(i, np.linspace(0, 1, num=100).reshape((100, 1)))
#     for i in range(n_spaced - 1)
# ])))
# # ax.plot(*split_axes(plot_spline(spaced_bezier_points)))
# # ax.plot(spline_v[:, 0], spline_v[:, 1])
# # ax.plot(points[:, 0], points[:, 1], 'o')
# # ax.plot(spaced_bezier_points[:, 0], spaced_bezier_points[:, 1], 'o')
# # ax.plot(spline_t, spline_v)
# # ax.plot(spline_t, spline_d)
# # ax.plot(spline_t, spline_dd)
# # for i, point in enumerate(points):
# #     ax.plot([i // 3, (i + 1) // 3], [point, point], linestyle='-')
# # for v in spaced_vs:
# #     ax.plot(v[0], v[1], 'o')
# plt.show()
