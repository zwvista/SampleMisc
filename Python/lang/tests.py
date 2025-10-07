def test_next():
    values = [3, 7, 10, 4, 8]
    values2 = [3, 7, 4, 8]
    # 返回列表内第一个大于9的元素，不存在的话返回 None
    number = next((n for n in values if n > 9), None) # 10
    number2 = next((n for n in values2 if n > 9), None) # None
    print(number, number2)
    # 返回列表内第一个大于9的元素的下标，不存在的话返回 -1
    index = next((i for i, n in enumerate(values) if n > 9), -1) # 2
    index2 = next((i for i, n in enumerate(values2) if n > 9), -1) # -1
    print(index, index2)


def test_map_filter():
    values = [1, 2, 3, 4, 5, 6]
    # 偶数元素的平方
    values2 = [n * n for n in values if n % 2 == 0]
    print(values2) # [4, 16, 36]
    values3 = list(map(lambda n: n * n, filter(lambda n: n % 2 == 0, values)))
    print(values3) # [4, 16, 36]


def test_all_any():
    values = [1, 3, 6, 9]
    # 是否全部是偶数
    print(all([n % 2 == 0 for n in values]))
    # 是否至少存在一个偶数
    print(any([n % 2 == 0 for n in values]))


def test_reverse():
    values = [1,2,3,4,5]
    # [::-1] 切片返回心累表
    values2 = values[::-1]
    print(values2)
    # reversed 函数返回迭代器
    values3 = list(reversed(values))
    print(values3)
    print(values)
    # reversed 方法
    values.reverse()
    print(values)


def test_zip():
    letters = ['a', 'b', 'c', 'd']
    numbers = [1, 2, 3, 4]
    print(list(zip(letters, numbers))) # [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
    for letter, number in zip(letters, numbers):
        print(letter, number)
    print(dict(zip(letters, numbers))) # {'a': 1, 'b': 2, 'c': 3, 'd': 4}


def test_unpack():
    a, b, *c = [1,2,3,4,5]
    print(a, b, c) # 1 2 [3, 4, 5]
    a, b, *_, d = [1,2,3,4,5]
    print(a, b, d) # 1 2 5


# test_next()
# test_map_filter()
# test_all_any()
# test_reverse()
# test_zip()
test_unpack()
