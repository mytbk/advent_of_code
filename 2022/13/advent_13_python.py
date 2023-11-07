#!/usr/bin/env python3

def compare(list1, list2):
    if isinstance(list1, int) and isinstance(list2, int):
        return list1 - list2

    if not isinstance(list1, list):
        return compare([list1], list2)

    if not isinstance(list2, list):
        return compare(list1, [list2])

    for i in range(len(list1)):
        if i >= len(list2):
            # list 2 runs out
            return 1

        res = compare(list1[i], list2[i])
        if res != 0:
            return res

    # list 1 runs out
    if len(list1) == len(list2):
        return 0
    else:
        return -1

def part1():
    num_lists = 0
    list1 = []
    list2 = []
    sum_idx = 0
    while True:
        try:
            input_line = input()
            if len(input_line) == 0:
                continue

            if num_lists % 2 == 0:
                list1 = eval(input_line)
            else:
                list2 = eval(input_line)

            num_lists = num_lists + 1

            if num_lists % 2 == 0:
                if compare(list1, list2) < 0:
                    sum_idx = sum_idx + num_lists // 2
        except EOFError:
            break

    print(sum_idx)

def part2():
    num_lists = 0
    all_lists = []
    while True:
        try:
            input_line = input()
            if len(input_line) == 0:
                continue

            all_lists.append(eval(input_line))
        except EOFError:
            break

    # all_lists.append([[2]])
    # all_lists.append([[6]])
    # find how many lists are smaller than [[2]] and [[6]]
    idx1 = 1
    idx2 = 2
    for L in all_lists:
        if compare(L, [[2]]) < 0:
            idx1 = idx1 + 1
            idx2 = idx2 + 1
        elif compare(L, [[6]]) < 0:
            idx2 = idx2 + 1

    print(idx1 * idx2)


if __name__ == "__main__":
    import sys
    if sys.argv[1] == "1":
        part1()
    else:
        part2()
