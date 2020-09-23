import re

s = '123-4567-89,987-6543-21'
r = re.compile(r'\d{3}-(\d{4})-\d{2}')
i = 0
pos = 0
m = r.search(s)
while m:
    if i == 0:
        print("Found matches:")
    for j in range(0, len(m.groups()) + 1):
        print(f"group {i},{j} : {m[j]}")
    pos = m.end(0)
    i += 1
    m = r.search(s, pos)

print(re.sub(r'(\d+)-(\d+)-(\d+)', r'\3-\1-\2', s))

# https://stackoverflow.com/questions/18737863/passing-a-function-to-re-sub-in-python
# https://stackoverflow.com/questions/931092/reverse-a-string-in-python
print(re.sub(r'\d+', lambda x: x.group()[::-1], s))

print(re.split('%(?:begin|next|end)%', '%begin%hello%next%world%end%'))

'''
Found matches:
group 0,0 : 123-4567-89
group 0,1 : 4567
group 1,0 : 987-6543-21
group 1,1 : 6543
89-123-4567,21-987-6543
321-7654-98,789-3456-12
['', 'hello', 'world', '']
'''
