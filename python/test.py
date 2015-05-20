import sys

#write output to file to see if passing args works
file=open("C:/Share/LCC-VP/scripts/logs/test_args_py.txt", "w")
old_stdout = sys.stdout
sys.stdout = file

for arg in sys.argv:
    print arg

print("test")
park=sys.argv[1]
print(len(sys.argv))
print(sys.argv)
print park
sys.stdout = old_stdout
file.close()