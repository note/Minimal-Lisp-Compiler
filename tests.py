import os
import subprocess

def getName(filename):
	return filename.split('.')[0]
	
def getExt(filename):
	arr = filename.split('.')
	if(len(arr) < 2):
		return ''
	else:
		return arr[len(arr)-1]

path = os.path.join('lisp', 'tests')

for filename in os.listdir(path):
	if os.path.isfile(os.path.join(path, filename)):
		index = 0
		f = open(os.path.join(path, filename), 'r')
		output = open(os.path.join('lisp', 'tmp', getName(filename) + '_' + str(index) + '.lisp'), 'w')
		for line in f:
			if line[:6] == ';begin':
				output.close()
				output = open(os.path.join('lisp', 'tmp', getName(filename) + '_' + str(index) + '.res'), 'w')
				continue
			if line[:4] == ';end':
				index += 1
				output.close()
				output = open(os.path.join('lisp', 'tmp', getName(filename) + '_' + str(index) + '.lisp'), 'w')
				continue
			output.write(line)

path = os.path.join('lisp', 'tmp')

for filename in os.listdir(path):
	if(getExt(filename) == "res"):
		
		# compiling
		print "Compiling file " + getName(filename) + ".lisp:"
		if(subprocess.call(["java", "-cp", "asm-4.0.jar:bin:.:std_lib:generated", "lisp.Compiler", os.path.join("lisp", "tmp", getName(filename) + ".lisp"), "-o", "generated"]) != 0):
			print "FAIL"
			continue
		print "OK"
		
		# running
		print "Running test for " + filename + ":"
		subprocess.call(["java", "-cp", "asm-4.0.jar:bin:.:std_lib:generated", "Main"], stdout=open("out", "w"))
		
		# checking if actual results matches expected
		if(subprocess.call(["diff", "out", os.path.join("lisp", "tmp", getName(filename) + ".res")]) == 0):
			print "OK"
		
