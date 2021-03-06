import sys
import numpy as np
import math
import time

def count(remainder,die):
  count.calls += 1
  #print "remainder: " + str(remainder)
  #print "current die: " + str(die)
  #print "current rolls: " + str(count.calls)
  #print "current product: " + str(count.product)
  if remainder < 0:
     count.calls -= 1 #decrement depth
     #print "remainder < 0, go up one level"
     return #go up one level
  if remainder > 0 and count.calls >= maxCalls:
     count.calls -= 1
     #print "too many calls, go up one level"
     return
  if remainder > 0 and count.calls < maxCalls:
     count.product = count.product*die
     for dierec in dice:
       if (remainder - dierec < 0):
	  break
       if ((remainder - (6*(maxCalls-count.calls)) > 0) or ((remainder -maxCalls + count.calls) < 0)):
          break
       count(remainder - dierec,dierec)
     count.product =count.product/die
  if remainder == 0 and count.calls == maxCalls:
     count.product = count.product*die
     print "final product: " + str(count.product)
     #count.products = np.append(count.products,[count.product])
     count.productSum += count.product
     count.productSum2 += count.product*count.product
     count.productNum += 1
     count.product = count.product/die
     count.calls -= 1
     return
  count.calls -= 1
  return

#remainder: -4
#current die: 6
#current rolls: 2
#current product: 1

start = time.time()
dice = [1, 2, 3, 4, 5, 6]
maxCalls = 2
count.product = 1
count.calls = 0
count.productSum=0
count.productSum2=0
count.productNum=0
#count.products = np.array([])
faceSum = 6
for die in dice:
    print "top die is: " + str(die)
    if (faceSum - die < 0):
      break
    if ((faceSum - (6*(maxCalls-count.calls)) > 0) or ((faceSum -maxCalls + count.calls) < 0)):
      break
    count.product = 1
    count(faceSum-die,die)
#print count.products
if (count.productNum > 0):
  expectedValue = float(count.productSum)/float(count.productNum)
  expectedValue2 = float(count.productSum2)/float(count.productNum)
  stDev = float(math.sqrt(expectedValue2 - expectedValue*expectedValue))
  print "Expected value of product: " + str(expectedValue)
  print "Standard deviation of product: " + str(stDev)
  #expectedValue = np.sum(count.products)/count.products.size
  #stDev = math.sqrt(np.sum(np.dot(count.products,count.products))/count.products.size - expectedValue*expectedValue)
  #print "Expected value of product: " + str(expectedValue)
  #print "Standard deviation of product: " + str(stDev)
else:
  print "No possible combinations"
end = time.time()
print(end - start)

