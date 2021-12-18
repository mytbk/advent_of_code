def add_left(p, val):
    if isinstance(p, int):
        return p + val
    else:
        car, cdr = p
        return [add_left(car, val), cdr]

def add_right(p, val):
    if isinstance(p, int):
        return p + val
    else:
        car, cdr = p
        return [car, add_right(cdr, val)]

# return (result, status, val)
# status can be 'add left', 'add right', 'exploded', 'complete', 'nop'

def try_explode(p, depth):
    #print(p, depth)
    if isinstance(p, list):
        car,cdr = p
        if isinstance(car, int) and isinstance(cdr, int):
            if depth >= 4:
                #print([car,cdr])
                return (0, 'exploded', (car, cdr))
            else:
                return ([car, cdr], 'nop', 0)

        res, status, val = try_explode(car, depth + 1)

        if status == 'exploded':
            l,r = val
            return ([0, add_left(cdr, r)], 'add left', l)
        elif status == 'add left':
            return ([res, cdr], 'add left', val)
        elif status == 'add right':
            return ([res, add_left(cdr, val)], 'complete', 0)
        elif status == 'complete':
            return ([res, cdr], 'complete', 0)

        res, status, val = try_explode(cdr, depth + 1)
        if status == 'exploded':
            l, r = val
            return ([add_right(car, l), 0], 'add right', r)
        elif status == 'add left':
            return ([add_right(car, val), res], 'complete', 0)
        elif status == 'add right':
            return ([car, res], 'add right', val)
        else:
            # nop or complete
            return ([car, res], status, val)

    else:
        return (p, 'nop', 0)

# returns IsSplit, res
def try_split(p):
    if isinstance(p, int):
        if p >= 10:
            return True, [p // 2, p - p // 2]
        else:
            return False, p
    else:
        car,cdr = p
        split, res = try_split(car)
        if split:
            return True, [res, cdr]
        
        split, res = try_split(cdr)
        return split, [car, res]

def magnitude(p):
    if isinstance(p, int):
        return p

    car, cdr = p
    return magnitude(car) * 3 + magnitude(cdr) * 2

def add_pairs(p1, p2):
    res = [p1, p2]

    while True:
        result_ex, status, val = try_explode(res, 0)
        if status != "nop":
            res = result_ex
        else:
            split, result_sp = try_split(res)
            if split:
                res = result_sp
            else:
                break

    return res

if __name__ == "__main__":
    pairs = []
    while True:
        try:
            p = eval(input())
            pairs.append(p)
        except EOFError:
            break

    res = None
    for p in pairs:
        if res is None:
            res = p
        else:
            res = add_pairs(res, p)

    print(res)
    print(magnitude(res))

    print(max([magnitude(add_pairs(pairs[i],pairs[j])) \
            for i in range(len(pairs)) for j in range(len(pairs)) \
            if i != j]))
