import re

s = '123-4567-89'
m = re.search(r'\d{3}-(\d{4})-\d{2}', s)
if m:
    for i in range(0, len(m.groups()) + 1):
        print(f"group {i} : {m[i]}")
        
print(re.sub(r'(\d+)-(\d+)-(\d+)', r'\3-\1-\2', s))

# https://stackoverflow.com/questions/18737863/passing-a-function-to-re-sub-in-python
# https://stackoverflow.com/questions/931092/reverse-a-string-in-python
print(re.sub(r'\d+', lambda x: x.group()[::-1], s))

print(re.split('%(?:begin|next|end)%', '%begin%hello%next%world%end%'))

'''
group 0 : 123-4567-89
group 1 : 4567
89-123-4567
321-7654-98
['', 'hello', 'world', '']
'''
