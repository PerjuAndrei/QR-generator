import QR
import MyMatrix
main::IO()
main = do
		putStrLn "Introduceti textul pentru a fi convertit in QR"
		text<-getLine
		if length text > 25 then do 
							putStrLn "Textul prea mare. Maxim 25 caractere." 
							main
		else	do
			putStrLn "Introduceti calea unde va fi salvata imaginea si numele acesteia" 
			cale<-getLine 
			let pixelSize = 10
			imageCreator cale pixelSize ( getFinalMatrix text)
