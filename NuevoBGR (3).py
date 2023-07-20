import cv2 
import numpy as np

image = cv2.imread("MBDTF.JPG")
cv2.imshow("Original", image)

result = image.copy()

#cap = cv2.VideoCapture(0)
#while cap.isOpened():
#    _, frame = cap.read()

image = cv2.cvtColor(image, cv2.COLOR_RGB2HSV)

#lower_mask pintura carro
lower1 = np.array([30,115,159]) #verde 
upper1 = np.array([30,118,165]) #azul xchillon


#uppermask llantas detectadas
#lower2 = np.array([0,5,0]) #negro
#upper2 = np.array([150, 100, 20]) #azul cielo


lower_mask = cv2.inRange(image, lower1, upper1)
#upper_mask = cv2.inRange(image, lower2, upper2)



cv2.imshow("Lower mask", lower_mask)

full_mask = lower_mask #+upper_mask
result = cv2.bitwise_and(result, result, mask=full_mask)

contornos, _ = cv2.findContours(full_mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

for c in contornos:
    area = cv2.contourArea(c)
    if area > 100:
        M = cv2.moments(c)
        if (M["m00"] == 0): M["m00"] = 1
        x = int(M["m10"]/M["m00"])
        y = int(M["m01"]/M["m00"])
        cv2.circle(result, (x,y), 7, (0, 255, 0), -1)
        font = cv2.FONT_HERSHEY_SIMPLEX
        cv2.putText(result, "{},{}".format(x,y), (x+10, y), font, 0.75, (0,255,0), 1)
        nuevocontorno = cv2.convexHull(c)
        cv2.drawContours(result, [nuevocontorno], 0, (255,0,0),3)

cv2.imshow("Mask",full_mask)
cv2.imshow("Result", result)
cv2.waitKey(0)
cv2.destroyAllWindows()