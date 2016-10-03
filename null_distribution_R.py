#!/ifshome/yeunkim/BSS_0817/bin/python

import sys

filename = str(sys.argv[1])
print sys.argv[1]
print sys.argv[2]

with open(filename, "r") as file:
	content=file.readlines()
#file.close()


print("finished loading in file")

tmax = []
for i in range(0,content.__len__()):
	pos1 = content[i].find('t-value:')
	pos2 = content[1].find("\n")
	foo =content[i][pos1+9:pos2-1]
	#print foo
	tmax.append(content[i][pos1+9:pos2-1])

print("finished extracting t-values")
#print tmax

with open(sys.argv[2], 'w') as output:
	for t in tmax:
		print>>output, t
output.close()
