from psychopy import visual, core, event, gui
import random, time

options={"Experimento":"1a","Descripcion":"Dos Figuras", "Procedimiento":"Y/N+Rating","Participante":"Usuario1"}
myDlg=gui.DlgFromDict(dictionary=options, title="Gano")
if myDlg.OK:
	print "Ok"
else:
	core.quit()

#
#WINDOW CONFIGURATION
#	
mywindow= visual.Window(monitor="testMonitor", units="cm", fullscr=True, color="white")
mymouse= event.Mouse(mywindow)
mymouse.setVisible(0)
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()


PantallaInicio=visual.ImageStim(mywindow, image="Bienvenido.png", pos=[0,0])
PantallaInicio.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)
#
#FIRST INSTRUCTIONS
#
print "Total Time"
total_time = time.time()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
Instrucciones1=visual.ImageStim(mywindow, image="Inst_1.png", pos=[0,0])
Instrucciones1.draw()
#txtResults=visual.TextStim(mywindow,text="Instrucciones:"+"\n"+"\n"+"\n"+"\n"+"Se te mostraran dos circulos de color claro rodeados por circulos de distinto tamano" +"\n"+"\n"+"\n"+"\n"+"Presiona la tecla S cuando los circulos claros SI sean del mismo tamano. "+"\n"+"\n"+"Si NO son iguales, oprime N"+"\n"+"\n"+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para continuar",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255")
#txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)

#
#FIRST EXAMPLE
#
mywindow.update()
img1=visual.ImageStim(mywindow, image="cext_over7_an.png", pos=[16,-5])
img2=visual.ImageStim(mywindow, image="cext_under7_an.png", pos=[-10,-5])
img3=visual.ImageStim(mywindow, image="ccentral_an.png", pos=[-10.2,-5], size=[2.5,2.5])
img4=visual.ImageStim(mywindow, image="ccentral_an.png", pos=[16,-5.2], size=[2,2])
txtResults=visual.TextStim(mywindow,text="Ejemplo: "+"\n"+"\n"+"\n"+"\n"+"En este caso el circulo claro de la figura derecha (el circulo central) es mas chico que el del lado izquierdo."+"\n"+"\n"+"Deberias presionar la tecla N porque NO son iguales",wrapWidth = 40,pos=[0,13], color=[0,0,0], colorSpace="rgb255")
txtRes=visual.TextStim(mywindow,text="Presiona N",wrapWidth = 30,pos=[0,-17], color=[0,0,0], colorSpace="rgb255")
img1.draw()
img2.draw()
img3.draw()
img4.draw()
txtResults.draw()
txtRes.draw()
mywindow.update()
	
userSel=" "
validkeys=['n']
while userSel !='n':
	keys=event.getKeys()
	for k in keys:
		if k in validkeys:
			userSel=k
		elif k=='escape': 
			myfile.Close()
			core.quit()
	core.wait(0.1)

#INSTRUCCIONES  (2DA PARTE)
#Respecto de la REGLITA
mywindow.update()
imgx=visual.ImageStim(mywindow, image="Reglita.png", pos=[0,1.5])
txtResults=visual.TextStim(mywindow,text="Posteriormente, se te presentara una escala como la siguiente: ",wrapWidth = 45,pos=[0,8], color=[0,0,0], colorSpace="rgb255")
txtRes=visual.TextStim(mywindow,text="Deberas teclear el numero 1, 2 o el 3, dependiendo de que tan seguro estas de tu respuesta."+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para continuar",wrapWidth = 30,pos=[0,-7], color=[0,0,0], colorSpace="rgb255")
imgx.draw()
txtResults.draw()
txtRes.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)
mywindow.update()
#
#END OF FIRST EXAMPLE
#

mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
Instrucciones2=visual.ImageStim(mywindow, image="Inst_3.png", pos=[0,0])
#txtResults=visual.TextStim(mywindow,text="Cada pareja a comparar se te mostrara SOLO POR 1 segundo"+"\n"+"\n"+"\n"+"\n"+"NO avanzaras al siguiente ensayo hasta que registres tu respuesta."+"\n"+"\n"+"\n"+"\n"+"Los ensayos estan separados por una breve pantalla blanca, espera a que aparezca una nueva pareja para volver a responder."+"\n"+"\n"+"\n"+"\n"+"Los estimulos se te presentaran en varios colores para facilitar la distincion entre ensayos. Los colores NO ESTAN CORRELACIONADOS de ninguna forma con nada."+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para continuar",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255")
#txtResults.draw()
Instrucciones2.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)

mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para comenzar el experimento",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255",alignHoriz="center")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)
#
#END OF INSTRUCTIONS
#


#
#VARIABLE INITIALIZATION
#
mybaseline=open("MirrEx1a_V2"+"_"+options["Participante"]+"_"+time.strftime("%Y-%m-%d")+"_"+"Y_N+R.csv", 'w')
right=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480]

Ver=[2, 5, 12, 15, 18, 21, 28, 31, 34, 37, 44, 47, 50, 53, 60, 63, 66, 69, 76, 79, 82, 85, 92, 95, 98, 101, 108, 111, 114, 117, 124, 127, 130, 133, 140, 143, 146, 149, 156, 159, 162, 165, 172, 175, 178, 181, 188, 191, 194, 197, 204, 207, 210, 213, 220, 223, 226, 229, 236, 239, 242, 245, 252, 255, 258, 261, 268, 271, 274, 277, 284, 287, 290, 293, 306, 309, 312, 315, 322, 325, 328, 331, 338, 341, 344, 347, 354, 357, 360, 363, 370, 373, 376, 379, 386, 389, 392, 395, 402, 405, 408, 411, 417, 420, 422, 425, 432, 435, 438, 441, 448, 451, 454, 457, 464, 467, 470, 473, 480, 483, 486, 489, 496, 499, 502, 505, 512, 515, 518, 521, 528, 531, 534, 537, 544, 547, 550, 553, 560, 563, 566, 569, 576, 579, 582, 585, 592, 595, 598, 601, 608, 611, 614, 617, 624, 627, 630, 633, 640, 638]
Az=[4, 7, 10, 13, 20, 23, 26, 29, 36, 39, 42, 45, 52, 55, 58, 61, 68, 71, 74, 77, 84, 87, 90, 93, 100, 103, 106, 109, 116, 119, 122, 125, 132, 135, 138, 141, 148, 151, 154, 157, 164, 167, 170, 173, 180, 183, 186, 189, 196, 199, 202, 205, 212, 215, 218, 221, 228, 231, 234, 237, 244, 247, 250, 253, 260, 263, 266, 269, 276, 279, 282, 285, 292, 295, 298, 301, 304, 307, 314, 317, 320, 323, 330, 333, 336, 339, 346, 349, 352, 355, 362, 365, 368, 371, 378, 381, 384, 387, 394, 397, 400, 403, 410, 412, 415, 418, 424, 427, 430, 433, 440, 443, 446, 449, 455, 459, 462, 465, 472, 475, 478, 481, 488, 491, 494, 497, 504, 507, 510, 513, 520, 523, 526, 529, 536, 539, 542, 545, 552, 555, 558, 561, 568, 571, 574, 577, 584, 587, 590, 593, 600, 603, 606, 609, 616, 619, 622, 625, 632, 636] 
Nar=[1, 8, 11, 14, 17, 24, 27, 30, 33, 40, 43, 46, 49, 56, 59, 62, 65, 72, 75, 78, 81, 88, 91, 94, 97, 104, 107, 110, 113, 120, 123, 126, 129, 136, 139, 142, 145, 152, 155, 158, 161, 168, 171, 174, 177, 184, 187, 190, 193, 200, 203, 206, 209, 216, 219, 222, 225, 232, 235, 238, 241, 248, 251, 254, 257, 264, 267, 270, 273, 280, 283, 286, 289, 296, 299, 302, 305, 308, 311, 318, 321, 324, 327, 334, 337, 340, 343, 350, 353, 356, 359, 366, 369, 372, 375, 382, 385, 388, 391, 398, 401, 404, 407, 413, 416, 419, 428, 431, 434, 437, 444, 447, 450, 453, 460, 463, 466, 469, 476, 479, 482, 485, 492, 495, 498, 501, 508, 511, 514, 517, 524, 527, 530, 533, 540, 543, 546, 549, 556, 559, 562, 565, 572, 575, 578, 581, 588, 591, 594, 597, 604, 607, 610, 613, 620, 623, 626, 629, 635, 639]
Pur=[3, 6, 9, 16, 19, 22, 25, 32, 35, 38, 41, 48, 51, 54, 57, 64, 67, 70, 73, 80, 83, 86, 89, 96, 99, 102, 105, 112, 115, 118, 121, 128, 131, 134, 137, 144, 147, 150, 153, 160, 163, 166, 169, 176, 179, 182, 185, 192, 195, 198, 201, 208, 211, 214, 217, 224, 227, 230, 233, 240, 243, 246, 249, 256, 259, 262, 265, 272, 275, 278, 281, 288, 291, 294, 297, 300, 303, 310, 313, 316, 319, 326, 329, 332, 335, 342, 345, 348, 351, 358, 361, 364, 367, 374, 377, 380, 383, 390, 393, 396, 399, 406, 409,  414, 421, 423, 426, 429, 436, 439, 442, 445, 452, 456, 458, 461, 468, 471, 474, 477, 484, 487, 490, 493, 500, 503, 506, 509, 516, 519, 522, 525, 532, 535, 538, 541, 548, 551, 554, 557, 564, 567, 570, 573, 580, 583, 586, 589, 596, 599, 602, 605, 612, 615, 618, 621, 628, 631, 634, 637]
#Variables uses to count the four possible results in Signal Detection Theory: hit, false alarm, miss and rejection
#Variables for counting in the first block
rightcounterb=0
wrongcounterb=0
falsealarmB=0
hitB=0
rejectionB=0
missB=0
#Variables for counting in the second block
rightcounterT=0
wrongcounterT=0
falsealarmT=0
hitT=0
rejectionT=0
missT=0

#central=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
mybaseline.write("Ensayo, Estimulo, Respuesta, Correcto, Aciertos, Errores, Hits, ContadorH, Rechazos, ContadorR, Falsas alarmas, ContadorF, Omisiones, ContadorM, Confidence, RTime1, RTime1b, RTime2, Color, \n")
mywindow.update()

#
#BEGIN OF FIRST BLOCK
#
for j in range(1):
	central = list()
	for i in range (640):
		central.append (i+1)
	for i in range (640):
#		choice=779
		choice=central.pop(random.randint(0,len(central)-1))
		if choice==1:
			img1=visual.ImageStim(mywindow, image="cext_over2_an.png", pos=[-15,0])
			img2=visual.ImageStim(mywindow, image="cext_under2_an.png", pos=[11,0])
			img3=visual.ImageStim(mywindow, image="ccentral_an.png", pos=[-15,0],size=[3,3])
			img4=visual.ImageStim(mywindow, image="ccentral_an.png", pos=[11,0],size=[3,3])
		if choice==2:
			img1=visual.ImageStim(mywindow, image="cext_under2_ver.png", pos=[-15,0])
			img2=visual.ImageStim(mywindow, image="cext_over2_ver.png", pos=[11,0])
			img3=visual.ImageStim(mywindow, image="ccentral_ver.png", pos=[-15,0],size=[3,3])
			img4=visual.ImageStim(mywindow, image="ccentral_ver.png", pos=[11,0],size=[3,3])
		if choice==3:
			img1=visual.ImageStim(mywindow, image="cext_over2_pur.png", pos=[-15,0])
			img2=visual.ImageStim(mywindow, image="cext_under2_pur.png", pos=[11,0])
			img3=visual.ImageStim(mywindow, image="ccentral_pur.png", pos=[-15,0],size=[3,3])
			img4=visual.ImageStim(mywindow, image="ccentral_pur.png", pos=[11,0],size=[3,3])
		if choice==4:
			img1=visual.ImageStim(mywindow, image="cext_under2_az.png", pos=[-15,0])
			img2=visual.ImageStim(mywindow, image="cext_over2_az.png", pos=[11,0])
			img3=visual.ImageStim(mywindow, image="ccentral_az.png", pos=[-15,0],size=[3,3])
			img4=visual.ImageStim(mywindow, image="ccentral_az.png", pos=[11,0],size=[3,3])
		if choice==5:
			img1=visual.ImageStim(mywindow, image="cext_over2_ver.png", pos=[-15,0])
			img2=visual.ImageStim(mywindow, image="cext_under2_ver.png", pos=[11,0])
			img3=visual.ImageStim(mywindow, image="ccentral_ver.png", pos=[-15,0],size=[3,3])
			img4=visual.ImageStim(mywindow, image="ccentral_ver.png", pos=[11,0],size=[3,3])
		txtRecordat1=visual.ImageStim(mywindow, image="Recordatorio1.png", pos=[0,15])
		txtRecordat2=visual.TextStim(mywindow,text="S = Si",wrapWidth = 60,color=[0,0,0], pos=[-13,-15], colorSpace="rgb255")
		txtRecordat3=visual.TextStim(mywindow,text="N = No",wrapWidth = 60,color=[0,0,0], pos=[10,-15], colorSpace="rgb255")
		txtRecordat1.draw()
		txtRecordat2.draw()
		txtRecordat3.draw()
		img1.draw()
		img2.draw()
		img3.draw()
		img4.draw()
		mybaseline.close
		mywindow.update()
		presentation_time = time.time()
#Tiempo que duran los ensayos0
		core.wait(1.5)
		presentation2_time = time.time()
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		txtRecordat1=visual.ImageStim(mywindow, image="Recordatorio1.png", pos=[0,15])
		txtRecordat2=visual.TextStim(mywindow,text="S = Si",wrapWidth = 60,color=[0,0,0], pos=[-13,-15], colorSpace="rgb255")
		txtRecordat3=visual.TextStim(mywindow,text="N = No",wrapWidth = 60,color=[0,0,0], pos=[10,-15], colorSpace="rgb255")
		txtRecordat1.draw()
		txtRecordat2.draw()
		txtRecordat3.draw()
		mywindow.update()
		userSel=" "
		validkeys=['s', 'n']
		while userSel !='s' and userSel !='n':
			keys=event.getKeys()
			for k in keys:
				if k in validkeys:
					userSel=k
				elif k=='escape': 
					mytestline.Close()
					core.quit()
			core.wait(0.1)
			response_time = time.time() - presentation_time
			response_timeB = time.time() - presentation2_time
		isSignal=(choice in right and userSel =='s') or (choice not in right and userSel =='s')
		if isSignal: 
			Respuesta='s'
		isNoise=(choice in right and userSel =='n') or (choice not in right and userSel =='n')
		if isNoise: 
			Respuesta='n'
		isCorrect=(choice in right and userSel =='s') or (choice not in right and userSel =='n')
		if isCorrect: 
			rightcounterb=rightcounterb+1
		else:
			 wrongcounterb=wrongcounterb+1
		isHit=(choice in right and userSel =='s')
		if isHit: 
			hitB=hitB+1
		isFalseAlarm=(choice not in right and userSel =='s')
		if isFalseAlarm: 
			falsealarmB=falsealarmB+1
		isMiss=(choice in right and userSel =='n')
		if isMiss: 
			missB=missB+1
		isRejection=(choice not in right and userSel =='n')
		if isRejection: 
			rejectionB=rejectionB+1
		isPurple=(choice in Pur)
		if isPurple: 
			Colorx='Purpura'
		isOrange=(choice in Nar)
		if isOrange: 
			Colorx='Naranja'
		isGreen=(choice in Ver)
		if isGreen: 
			Colorx='Verde'
		isBlue=(choice in Az)
		if isBlue: 
			Colorx='Azul'
#		mybaseline.write(str(i)+","+str(choice)+","+str(userSel)+","+str(isCorrect)+","+str(rightcounterb)+","+str(wrongcounterb)+","+str(isHit)+","+str(hitB)+","+str(isRejection)+","+str(rejectionB)+","+str(isFalseAlarm)+","+str(falsealarmB)+","+str(isMiss)+","+str(missB)+","+"\n")
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
#		core.wait(0.5)
		reglita=visual.ImageStim(mywindow, image="Reglita.png", pos=[0,0])
		reglita.draw()
		txtRegla=visual.ImageStim(mywindow, image="Recordatorio2.png", pos=[0,8])
		txtRegla.draw()
		mywindow.update()
		reglita_time = time.time()
#Tiempo que dura la Reglita 
#		core.wait(.5)
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
		userSel=" "
		validkeys=['1', '2', '3']
		while userSel !='1' and userSel !='2' and userSel !='3':
			keys=event.getKeys()
			for k in keys:
				if k in validkeys:
					userSel=k
				elif k=='escape': 
					mytestline.Close()
					core.quit()
			core.wait(0.1)
			response_time2 = time.time() - reglita_time
		isHighOld=(isFalseAlarm and userSel =='3') or (isHit and userSel =='3')
		if isHighOld: 
			Confidence=6 
		isHighNew=(isRejection and userSel =='3') or (isMiss and userSel =='3')
		if isHighNew: 
			Confidence=1
		isMediumOld=(isFalseAlarm and userSel =='2') or (isHit and userSel =='2')
		if isMediumOld: 
			Confidence=5
		isMediumNew=(isRejection and userSel =='2') or (isMiss and userSel =='2')
		if isMediumNew: 
			Confidence=2
		isLowOld=(isFalseAlarm and userSel =='1') or (isHit and userSel =='1')
		if isLowOld: 
			Confidence=4
		isLowNew=(isRejection and userSel =='1') or (isMiss and userSel =='1')
		if isLowNew: 
			Confidence=3
		mybaseline.write(str(i)+","+str(choice)+","+str(Respuesta)+","+str(isCorrect)+","+str(rightcounterb)+","+str(wrongcounterb)+","+str(isHit)+","+str(hitB)+","+str(isRejection)+","+str(rejectionB)+","+str(isFalseAlarm)+","+str(falsealarmB)+","+str(isMiss)+","+str(missB)+","+str(Confidence)+","+str(response_time)+","+str(response_timeB)+","+str(response_time2)+","+str(Colorx)+"\n")
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		mywindow.update()
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		mywindow.update()
		txtResults=visual.TextStim(mywindow,text="\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para pasar al siguiente ensayo.",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255",alignHoriz="center")
		txtResults.draw()
		mywindow.update()
		while 'space' not in event.getKeys(): 
			core.wait(0.1)
#END OF ONE TEST
	#END OF A BLOCK OF 48 TESTS
#
#END OF THE FIRST BLOCK
#
TOTAL_time = time.time() - total_time
print str(TOTAL_time)
mybaseline.write(str(TOTAL_time))

mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()

mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="Resultado: "+"\n"+"\n"+"Respuesatas Correctas: "+str(rightcounterb)+"\n"+"Respuestas Incorrectas: "+str(wrongcounterb)+"\n"+"\n"+"Presiona la barra espaciadora para continuar",color=[0,0,0], colorSpace="rgb255")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)



mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="Listo! El experimento ha terminado!"+"\n"+"\n"+"Muchisimas gracias por tu tiempo y cooperacion."+"\n"+"\n"+"Gracias por contribuir al trabajo de una tesista"+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para salir",color=[0,0,0], colorSpace="rgb255")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)
