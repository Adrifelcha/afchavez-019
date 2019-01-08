from psychopy import visual, core, event, gui
import random, time


options={"Experimento":"Caras", "Participante":"Participante"}
myDlg=gui.DlgFromDict(dictionary=options, title="Gano")
if myDlg.OK:
    print "Ok"
else:
    core.quit()
#
#WINDOW CONFIGURATION: Color de pantalla, blabla 
#(Ignora esta parte)
#	
mywindow= visual.Window(monitor="testMonitor", units="cm", fullscr=True, color="white")
mymouse= event.Mouse(mywindow)
mymouse.setVisible(0)
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()

#
#
# MODIFICACION 1
#INSTRUCCIONES:
#
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="Instrucciones:"+"\n"+"\n"+"\n"+"\n"+"Describe aqui los ensayos y la tarea de los participantes" +"\n"+"\n"+"\n"+"\n"+"Presiona la tecla S cuando los caras SI se vean enojadas. "+"\n"+"\n"+"Presiona la tecla N si NO"+"\n"+"\n"+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para continuar",wrapWidth = 20,color=[0,0,0], colorSpace="rgb255")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)

#MODIFICACION 2
#Tu primer ejemplo
mywindow.update()
img2=visual.ImageStim(mywindow, image="Cara1.png", pos=[0,0])
txtResults=visual.TextStim(mywindow,text="Por ejemplo: "+"\n"+"\n"+"\n"+"\n"+"Describe brevemente lo que estan viendo"+"\n"+"\n"+"Describe cual seria la respuesta ideal",wrapWidth = 20,pos=[0,8], color=[0,0,0], colorSpace="rgb255")
txtRes=visual.TextStim(mywindow,text="Por ejemplo: Presiona X",wrapWidth = 30,pos=[0,-17], color=[0,0,0], colorSpace="rgb255")
img2.draw()
txtResults.draw()
txtRes.draw()
mywindow.update()
	
userSel=" "
validkeys=['x']
while userSel !='x':
	keys=event.getKeys()
	for k in keys:
		if k in validkeys:
			userSel=k
		elif k=='escape': 
			myfile.Close()
			core.quit()
	core.wait(0.1)
#
#END OF FIRST EXAMPLE
#

#MODIFICACION 3
#Si necesitas mas instrucciones, quita los # de la seccion de abajo y escribe lo que necesites
#mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#mywindow.update()
#mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#mywindow.update()
#txtResults=visual.TextStim(mywindow,text="Responde TAN RAPIDO como puedas, cada pareja a comparar se te mostrara SOLO POR 1 segundo"+"\n"+"\n"+"\n"+"\n"+"NO avanzaras al siguiente ensayo hasta que registres tu respuesta."+"\n"+"\n"+"\n"+"\n"+"Los ensayos estan separados por una breve pantalla blanca, espera a que aparezca una nueva pareja para volver a responder."+"\n"+"\n"+"\n"+"\n"+"Los estimulos se te presentaran en varios colores para facilitar la distincion entre ensayos. Los colores NO ESTAN CORRELACIONADOS de ninguna forma con nada."+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para continuar",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255")
#txtResults.draw()
#mywindow.update()
#while 'space' not in event.getKeys(): 
#	core.wait(0.1)

#Pantalla de descanso para que preguntes si tienen dudas
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
#
#Pides al programa que abra un archivo para registrar los datos
mybaseline=open("Exp_Caras"+"_"+options["Participante"]+"_"+time.strftime("%Y-%m-%d"), 'w')
#La lista 'signal' debe contener el NUMERO en que aparecen tus senales 
signal=[1, 2, 480]
#Variables for counting in the first block. DEJA ESTO ASI
rightcounterb=0
wrongcounterb=0
falsealarmB=0
hitB=0
rejectionB=0
missB=0
#Variables for counting in the second block. DEJA ESTO ASI.
rightcounterT=0
wrongcounterT=0
falsealarmT=0
hitT=0
rejectionT=0
missT=0

#Le pides al programa que en el archivo de datos, genere las siguientes columnas
mybaseline.write("Ensayo, Estimulo, Respuesta, Correcto, Aciertos, Errores, Hits, ContadorH, Rechazos, ContadorR, Falsas alarmas, ContadorF, Omisiones, ContadorM, Latencia, \n")
mywindow.update()

#
#BEGIN OF FIRST BLOCK
#
for j in range(1):
	central = list()
#DONDE DICE in range () 
#El numero en el parentesis, debe ser tu total de estimulos.
	for i in range (5):
		central.append (i+1)
	for i in range (5):
		choice=central.pop(random.randint(0,len(central)-1))
#RECURDA: Copia y pega tantas 'choices' como estimulos tengas; y luego, pon en la lista signal el numero de 'choice' que tienen tus estimulos con senal
		if choice==1:
			img2=visual.ImageStim(mywindow, image="Cara1.png", pos=[0,0])
		if choice==2:
			img2=visual.ImageStim(mywindow, image="Cara2.png", pos=[0,0])
		if choice==3:
			img2=visual.ImageStim(mywindow, image="Cara1.png", pos=[0,0])
		if choice==4:
			img2=visual.ImageStim(mywindow, image="Cara2.png", pos=[0,0])
		if choice==5:
			img2=visual.ImageStim(mywindow, image="Cara1.png", pos=[0,0])
		txtRecordat1=visual.TextStim(mywindow, text="Las caras estan enojadas??", pos=[0,20])
		txtRecordat2=visual.TextStim(mywindow,text="S = Si",wrapWidth = 60,color=[0,0,0], pos=[-9,-14], colorSpace="rgb255")
		txtRecordat3=visual.TextStim(mywindow,text="N = No",wrapWidth = 60,color=[0,0,0], pos=[7,-14], colorSpace="rgb255")
		txtRecordat1.draw()
		txtRecordat2.draw()
		txtRecordat3.draw()
		img2.draw()
		mybaseline.close
		mywindow.update()
		presentation_time = time.time()
#OPCIONAL: El core.wait de abajo, te dice cuantos segundos debe aparecer la imagen (entre parentesis)
		core.wait(2)
#		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
#		mywindow.update()
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		txtRecordat1=visual.TextStim(mywindow, text="Las caras estan enojadas??", pos=[0,20])
		txtRecordat2=visual.TextStim(mywindow,text="S = Si",wrapWidth = 60,color=[0,0,0], pos=[-9,-14], colorSpace="rgb255")
		txtRecordat3=visual.TextStim(mywindow,text="N = No",wrapWidth = 60,color=[0,0,0], pos=[7,-14], colorSpace="rgb255")
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
		isSignal=(choice in signal and userSel =='s') or (choice not in signal and userSel =='s')
		if isSignal: 
			Respuesta='s'
		isNoise=(choice in signal and userSel =='n') or (choice not in signal and userSel =='n')
		if isNoise: 
			Respuesta='n'
		isCorrect=(choice in signal and userSel =='s') or (choice not in signal and userSel =='n')
		if isCorrect: 
			rightcounterb=rightcounterb+1
		else:
			 wrongcounterb=wrongcounterb+1
		isHit=(choice in signal and userSel =='s')
		if isHit: 
			hitB=hitB+1
		isFalseAlarm=(choice not in signal and userSel =='s')
		if isFalseAlarm: 
			falsealarmB=falsealarmB+1
		isMiss=(choice in signal and userSel =='n')
		if isMiss: 
			missB=missB+1
		isRejection=(choice not in signal and userSel =='n')
		if isRejection: 
			rejectionB=rejectionB+1
		mybaseline.write(str(i)+","+str(choice)+","+str(Respuesta)+","+str(isCorrect)+","+str(rightcounterb)+","+str(wrongcounterb)+","+str(isHit)+","+str(hitB)+","+str(isRejection)+","+str(rejectionB)+","+str(isFalseAlarm)+","+str(falsealarmB)+","+str(isMiss)+","+str(missB)+","+str(response_time)+"\n")
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		mywindow.update()
		mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
		mywindow.update()
		txtResults=visual.TextStim(mywindow,text="\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para avanzar al siguiente ensayo.",wrapWidth = 45,color=[0,0,0], colorSpace="rgb255",alignHoriz="center")
		txtResults.draw()
		mywindow.update()
		while 'space' not in event.getKeys(): 
			core.wait(0.1)
#END OF ONE TEST
	#END OF A BLOCK OF 48 TESTS
#
#END OF THE FIRST BLOCK
#
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()

#Al acabar el experimento, se les dice a los participantes que tan bien lo hicieron
#Sino quieres que lo vean, lo comentas con #
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="Resultado: "+"\n"+"\n"+"Respuesatas Correctas: "+str(rightcounterb)+"\n"+"Respuestas Incorrectas: "+str(wrongcounterb)+"\n"+"\n"+"Presiona la barra espaciadora para continuar",color=[0,0,0], colorSpace="rgb255")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)


#Estos son los agradecimientos finales 
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
mywindow.setColor(color=[255,255,255], colorSpace="rgb255")
mywindow.update()
txtResults=visual.TextStim(mywindow,text="Listo! El experimento ha terminado!"+"\n"+"\n"+"Muchisimas gracias por tu tiempo y cooperacion."+"\n"+"\n"+"Gracias por contribuir al trabajo de una tesista"+"\n"+"\n"+"\n"+"\n"+"Presiona la barra espaciadora para salir",color=[0,0,0], colorSpace="rgb255")
txtResults.draw()
mywindow.update()
while 'space' not in event.getKeys(): 
	core.wait(0.1)

