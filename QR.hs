module QR where
	import MyMatrix
	import Codec.Picture

	--Specifiacatii pentru 21x21-codare alfanumerica-error code 7% L
	_size = 21						--size
	_modeIndicator = "0010"			--specific alfanumeric 
	_caracterCountIndicator = 9		--specific alfanumeric+size
	_dataCodewords = 19				--specific size + tipul de errore correction
	_codewordsPerBlock = 7 			--specific size + tipul de errore correction
	_padBytes1 = "11101100"			--biti de umplutura
	_padBytes2 = "00010001"			--biti de umplutura
	_myPolynom = [0, 87, 229, 146, 149, 238, 102, 21] --Polinom pentru 7 codewordsperblock
	_formatInfoStrings = ["001000111110111", "110011110100111", "010101011011111", "101110010001111", "111101000110011", "000110001100011", "100000100011011", "011011101001011"]

	--Lungimea unui String
	getLength::String->Integer
	getLength [] = 0
	getLength (hd:tl) = 1 + getLength tl

	--Converteste un integer intr-un string ce reprezinta numarul in baza2
	convertToBinary'::Integer->String
	convertToBinary' 0 = []
	convertToBinary' x 	| x `mod` 2 == 1 = convertToBinary' (x `div` 2) ++ ['1']
						| otherwise = convertToBinary' (x `div` 2) ++ ['0']
	convertToBinary::Integer->String
	convertToBinary 0 = "0"
	convertToBinary x = convertToBinary' x

	--Adauga 0 in fata la string pana are size 9
	toXBits::String->Integer->String
	toXBits sir x		| getLength sir == x = sir
				 		| otherwise = toXBits ('0':sir) x

	--Returneaza CaracterCountIndicator pentru input
	getCaracterCountIndicator::String->Integer->String
	getCaracterCountIndicator input size = toXBits (convertToBinary (getLength input)) size

	--Fiecare caracter in valoarea sa
	getAlphanumericValues::Char->Integer	
	getAlphanumericValues '0' = 0
	getAlphanumericValues '1' = 1
	getAlphanumericValues '2' = 2
	getAlphanumericValues '3' = 3
	getAlphanumericValues '4' = 4
	getAlphanumericValues '5' = 5
	getAlphanumericValues '6' = 6
	getAlphanumericValues '7' = 7
	getAlphanumericValues '8' = 8
	getAlphanumericValues '9' = 9
	getAlphanumericValues 'A' = 10
	getAlphanumericValues 'B' = 11
	getAlphanumericValues 'C' = 12
	getAlphanumericValues 'D' = 13
	getAlphanumericValues 'E' = 14
	getAlphanumericValues 'F' = 15
	getAlphanumericValues 'G' = 16
	getAlphanumericValues 'H' = 17
	getAlphanumericValues 'I' = 18
	getAlphanumericValues 'J' = 19
	getAlphanumericValues 'K' = 20
	getAlphanumericValues 'L' =	21
 	getAlphanumericValues 'M' = 22
 	getAlphanumericValues 'N' = 23
 	getAlphanumericValues 'O' = 24
 	getAlphanumericValues 'P' = 25
	getAlphanumericValues 'Q' = 26				
	getAlphanumericValues 'R' = 27
	getAlphanumericValues 'S' = 28
	getAlphanumericValues 'T' = 29
	getAlphanumericValues 'U' = 30
	getAlphanumericValues 'V' = 31
	getAlphanumericValues 'W' = 32
	getAlphanumericValues 'X' = 33
	getAlphanumericValues 'Y' = 34
	getAlphanumericValues 'Z' = 35
	getAlphanumericValues 'a' = 10
	getAlphanumericValues 'b' = 11
	getAlphanumericValues 'c' = 12
	getAlphanumericValues 'd' = 13
	getAlphanumericValues 'e' = 14
	getAlphanumericValues 'f' = 15
	getAlphanumericValues 'g' = 16
	getAlphanumericValues 'h' = 17
	getAlphanumericValues 'i' = 18
	getAlphanumericValues 'j' = 19
	getAlphanumericValues 'k' = 20
	getAlphanumericValues 'l' =	21
 	getAlphanumericValues 'm' = 22
 	getAlphanumericValues 'n' = 23
 	getAlphanumericValues 'o' = 24
 	getAlphanumericValues 'p' = 25
	getAlphanumericValues 'q' = 26				
	getAlphanumericValues 'r' = 27
	getAlphanumericValues 's' = 28
	getAlphanumericValues 't' = 29
	getAlphanumericValues 'u' = 30
	getAlphanumericValues 'v' = 31
	getAlphanumericValues 'w' = 32
	getAlphanumericValues 'x' = 33
	getAlphanumericValues 'y' = 34
	getAlphanumericValues 'z' = 35
	getAlphanumericValues ' ' = 36 
	getAlphanumericValues '$' = 37
	getAlphanumericValues '%' = 38
	getAlphanumericValues '*' = 39
	getAlphanumericValues '+' = 40
	getAlphanumericValues '-' = 41
	getAlphanumericValues '.' = 42
	getAlphanumericValues '/' = 43
	getAlphanumericValues ':' = 44 

	--Returneaza stringul initial intr-un sir binar ce reprezinta textul
	getEncodedData::String->String
	getEncodedData [] = []
	getEncodedData (hd:[]) = toXBits (convertToBinary(getAlphanumericValues hd)) 6
	getEncodedData (hd:hd':tl) = toXBits (convertToBinary ((getAlphanumericValues hd)* 45 + (getAlphanumericValues hd'))) 11 ++ getEncodedData tl

	--Pune pana la 4 caractere de "0" in functie de lungime
	getTerminator::String->String
	getTerminator sir 	| dif >= 4 ="0000"
						| dif == 3 = "000"
						| dif == 2 = "00"
						| dif == 1 = "0"
						| dif == 0 = ""
						where dif = _dataCodewords*8 - (getLength sir)

	--Adaugam "0" la final pana e multiplu de 8
	addToDiv8::String->String
	addToDiv8 sir 	| (getLength sir) `mod` 8 == 0 = sir
					|	otherwise = addToDiv8 (sir++"0")

	--Adaugam PadBytes
	addPadBytes::String->String
	addPadBytes sir | needToAdd == 0 = sir
					| needToAdd == 1 = sir ++ _padBytes1
					| otherwise = addPadBytes (sir ++ _padBytes1 ++ _padBytes2)
					where needToAdd = (_dataCodewords*8 - (getLength sir)) `div` 8

	--Combina tot ce am facut pana acum
	combineResults::String->String
	combineResults input = addPadBytes (addToDiv8 (out ++ (getTerminator out)))
			where out = _modeIndicator ++ getCaracterCountIndicator input _caracterCountIndicator ++ getEncodedData input

	--Imparte un String (multiplu de 8) intr-o lista de stringuri de fix 8 caractere
	splitBy8::String->[String]
	splitBy8 (h1:h2:h3:h4:h5:h6:h7:h8:[]) = [(h1:h2:h3:h4:h5:h6:h7:h8:[])]
	splitBy8 (h1:h2:h3:h4:h5:h6:h7:h8:tl) = (h1:h2:h3:h4:h5:h6:h7:h8:[]) : splitBy8 tl

	--Converteste String de biti in Integer
	convertToDecimal::String->Integer
	convertToDecimal (hd:[]) | hd == '1' = 1
							 | otherwise = 0
	convertToDecimal (hd:tl) | hd == '1' = 2^getLength(tl) + (convertToDecimal tl)
							 | otherwise = convertToDecimal tl

	--Schimba un sir de biti in sir de Integer
	getDecimals::[String]->[Integer]
	getDecimals [] = []
	getDecimals (hd:[]) = (convertToDecimal hd) : []
	getDecimals (hd:tl) = (convertToDecimal hd) : (getDecimals tl)

	--Creeaza o lista in care un element apare de nr ori
	multiply::a->Integer->[a]
	multiply x 0 = []
	multiply x nr = x : multiply x (nr-1)

	--Pregateste polinomul (mareste gradul cu n [x^n])
	growGrade::[Integer]->[Integer]
	growGrade input = input ++ multiply 0 _codewordsPerBlock

	--Tablea Log-AntiLog
	logAntilogTable :: [(Integer, Integer)]
	logAntilogTable = zip [0..255] [1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38, 76, 152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157, 39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35, 70, 140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161, 95, 190, 97, 194, 153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240, 253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163, 91, 182, 113, 226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189, 103, 206, 129, 31, 62, 124, 248, 237, 199, 147, 59, 118, 236, 197, 151, 51, 102, 204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132, 21, 42, 84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228, 213, 183, 115, 230, 209, 191, 99, 198, 145, 63, 126, 252, 229, 215, 179, 123, 246, 241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174, 65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239, 195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27, 54, 108, 216, 173, 71, 142, 1]
	
	--Acces la tabela log-Antilog--Fct ajutatoare
	getExp :: [(Integer, Integer)]->Integer -> Integer
	getExp (hd:tl) x 	| snd hd == x = fst hd
						| otherwise = getExp tl x 
	getExp' :: [(Integer, Integer)]->Integer -> Integer
	getExp' (hd:tl) x 	| fst hd == x = snd hd
						| otherwise = getExp' tl x

	--Acces la tebela log-Antilog
	exponentToInteger::Integer->Integer
	exponentToInteger x = getExp' logAntilogTable x
	integerToExponenet::Integer->Integer
	integerToExponenet x = getExp logAntilogTable x

	--Codewords
	stepA::[Integer]->[Integer]->[Integer]
	stepA (hd:tl) myPolynom = map (exponentToInteger) (map (`mod` 255) (map (+ (integerToExponenet hd)) myPolynom))

	--xor pe stringuri de biti
	xor'::String->String->String
	xor' [] [] = []
	xor' [] x = x
	xor' x [] = x
	xor' (h1:t1) (h2:t2) | h1==h2 = '0': xor' t1 t2
						| otherwise ='1' : xor' t1 t2
	--xor pe integer
	xor::Integer->Integer->Integer
	xor x y =  convertToDecimal (reverse(xor' (reverse(convertToBinary x)) (reverse((convertToBinary y)))))


	stepB'::[Integer]->[Integer]->[Integer]
	stepB' [] [] = []
	stepB' [] (h2:t2) = (xor 0 h2) : stepB' [] t2
	stepB' (h1:t1) [] = (xor h1 0) : stepB' t1 []
	stepB' (h1:t1) (h2:t2) = (xor h1 h2) : stepB' t1 t2

	stepB::[Integer]->[Integer]->[Integer]
	stepB (h1:t1) (h2:t2) = stepB' t1 t2

	--StepA+StepB
	errorCorrectionStep::[Integer]->[Integer]->Integer->[Integer]
	errorCorrectionStep message myPolynom 1 = stepB message (stepA message myPolynom) 
	errorCorrectionStep (0:message) myPolynom i = errorCorrectionStep (stepB message (stepA message myPolynom)) myPolynom (i-2) 
	errorCorrectionStep message myPolynom i = errorCorrectionStep (stepB message (stepA message myPolynom)) myPolynom (i-1) 

	--Returneaza ErrorCorrectionCodewords
	getErrorCorrectionCodewords::[Integer] -> [Integer]
	getErrorCorrectionCodewords input = errorCorrectionStep input _myPolynom _dataCodewords

	--Transforma lista de numere in sir de 8biti/numar
	almostFinalMessage::[Integer]->String
	almostFinalMessage [] = []
	almostFinalMessage (hd:tl) = (toXBits (convertToBinary hd) 8) ++ (almostFinalMessage tl)

	--Mesajul pregatit pentru a fi pus in matrice
	getFinalMessage::String->[String]
	getFinalMessage input = splitBy8 (almostFinalMessage (dataCodeword ++ (getErrorCorrectionCodewords dataCodeword)))
									where dataCodeword = getDecimals(splitBy8(combineResults input)) 

    --PT MASK
	condition1'::[String]->Integer->Integer->Integer->Integer
	condition1' _ _ (-1) lafel | lafel > 4 = (lafel-2) 
								 	| otherwise = 0
	condition1' matrix i j lafel 	| getElement matrix i j == getElement matrix i (j+1) = condition1' matrix i (j-1) (lafel+1)
								 	| lafel > 4 = (lafel-2) + condition1' matrix i (j-1) 1
								 	| otherwise =  condition1' matrix i (j-1) 1

	condition1''::[String]->Integer->Integer->Integer->Integer
	condition1'' _ (-1) _ lafel | lafel > 4 = (lafel-2) 
								 	| otherwise = 0
	condition1'' matrix i j lafel 	| getElement matrix i j == getElement matrix (i+1) j = condition1'' matrix (i-1) j (lafel+1)
								 	| lafel > 4 = (lafel-2) + condition1'' matrix (i-1) j 1
								 	| otherwise =  condition1'' matrix (i-1) j 1

	cond1::[String]->Integer->Integer
	cond1 _ (-1) = 0
	cond1 matrix i = condition1' matrix i (_size-2) 1 + condition1'' matrix (_size-2) i 1 + cond1 matrix (i-1)

	condition1::[String]->Integer
	condition1 matrix =  cond1 matrix (_size-1)


	condition2'::[String]->Integer->Integer->Integer
	condition2' _ (-1) _ = 0
	condition2' matrix i (-1) = condition2' matrix (i-1) (_size-2)
	condition2' matrix i j 	| (getElement matrix i j == getElement matrix (i+1) j) && (getElement matrix i j == getElement matrix i (j+1)) && (getElement matrix i j == getElement matrix (i+1) (j+1)) = 3+ condition2' matrix i (j-1)
							| otherwise = condition2' matrix i (j-1)
	condition2::[String]->Integer
	condition2 matrix = condition2' matrix (_size-2) (_size-2)


	condition3'::[String]->Integer->Integer->Integer
	condition3' _ _ (-1) = 0
	condition3' matrix i j | ((getElement matrix i j == _black) &&
							(getElement matrix i (j+1) == _white) &&
							(getElement matrix i (j+2) == _black) &&
							(getElement matrix i (j+3) == _black) &&
							(getElement matrix i (j+4) == _black) &&
							(getElement matrix i (j+5) == _white) &&
							(getElement matrix i (j+6) == _black) &&
							(getElement matrix i (j+7) == _white) &&
							(getElement matrix i (j+8) == _white) &&
							(getElement matrix i (j+9) == _white) &&
							(getElement matrix i (j+10) == _white)) ||
							((getElement matrix i j == _white) &&
							(getElement matrix i (j+1) == _white) &&
							(getElement matrix i (j+2) == _white) &&
							(getElement matrix i (j+3) == _white) &&
							(getElement matrix i (j+4) == _black) &&
							(getElement matrix i (j+5) == _white) &&
							(getElement matrix i (j+6) == _black) &&
							(getElement matrix i (j+7) == _black) &&
							(getElement matrix i (j+8) == _black) &&
							(getElement matrix i (j+9) == _white) &&
							(getElement matrix i (j+10) == _black)) 
							= 40 + condition3' matrix i (j-1)
							| otherwise = condition3' matrix i (j-1)
	condition3''::[String]->Integer->Integer->Integer
	condition3'' _ (-1) _ = 0
	condition3'' matrix i j | ((getElement matrix i j == _black) &&
							(getElement matrix (i+1) j == _white) &&
							(getElement matrix (i+2) j == _black) &&
							(getElement matrix (i+3) j == _black) &&
							(getElement matrix (i+4) j == _black) &&
							(getElement matrix (i+5) j == _white) &&
							(getElement matrix (i+6) j == _black) &&
							(getElement matrix (i+7) j == _white) &&
							(getElement matrix (i+8) j == _white) &&
							(getElement matrix (i+9) j == _white) &&
							(getElement matrix (i+10) j == _white)) ||
							((getElement matrix i j == _white) &&
							(getElement matrix (i+1) j == _white) &&
							(getElement matrix (i+2) j == _white) &&
							(getElement matrix (i+3) j == _white) &&
							(getElement matrix (i+4) j == _black) &&
							(getElement matrix (i+5) j == _white) &&
							(getElement matrix (i+6) j == _black) &&
							(getElement matrix (i+7) j == _black) &&
							(getElement matrix (i+8) j == _black) &&
							(getElement matrix (i+9) j == _white) &&
							(getElement matrix (i+10) j == _black)) 
							= 40 + condition3'' matrix (i-1) j
							| otherwise = condition3'' matrix (i-1) j						

	cond3::[String]->Integer->Integer
	cond3 _ (-1) = 0
	cond3 matrix i = condition3' matrix i (_size-11) + condition3'' matrix (_size-11) i +cond3 matrix (i-1)

	condition3::[String]->Integer
	condition3 matrix = cond3 matrix (_size-1)


	blackNo::[String]->Integer->Integer->Integer
	blackNo _ (-1) _ = 0
	blackNo matrix i (-1) = blackNo matrix (i-1) (_size-1)
	blackNo matrix i j 	| getElement matrix i j == _black = 1 + blackNo matrix i (j-1)
						| otherwise = blackNo matrix i (j-1)

	condition4''::Integer->Integer
	condition4'' x = do
					let v1 = if (x-50)<0 then (-1)*(x-50) else (x-50)
					let v2 = if (x-45)<0 then (-1)*(x-45) else (x-45)
					let rez = if v1<v2 then v1 else v2
					(rez*2)


	condition4'::Integer->Integer
	condition4' cat 	| cat `mod` 5==0 = condition4'' cat
						| otherwise = condition4' (cat-1) 

	condition4::[String]->Integer
	condition4 matrix = do 
					let nrBlack = blackNo matrix (_size-1) (_size-1)
					let nrWhite = _size*_size - nrBlack
					let rez = condition4' ((nrBlack *100) `div` (nrBlack+nrWhite))
					rez

	--Luam pentalyScore
	getPenaltyScore::[String]->Integer
	getPenaltyScore matrix = (condition1 matrix') + (condition2 matrix') +(condition3 matrix' )+ (condition4 matrix')
							where matrix' = addStaticFields matrix

	--Mask formulas
	mask0::Integer->Integer->Bool
	mask0 i j = (i+j) `mod` 2 == 0

	mask1::Integer->Integer->Bool
	mask1 i _ = i `mod` 2 == 0

	mask2::Integer->Integer->Bool
	mask2 _ j = j `mod` 3 == 0

	mask3::Integer->Integer->Bool
	mask3 i j = (i+j) `mod` 3 == 0

	mask4::Integer->Integer->Bool
	mask4 i j = ((i `div` 2) + (j `div` 3)) `mod` 2 == 0

	mask5::Integer->Integer->Bool
	mask5 i j = ((i*j) `mod` 2)+ ((i*j) `mod` 3) == 0  

	mask6::Integer->Integer->Bool
	mask6 i j = (((i*j) `mod` 2) + ((i*j) `mod` 3)) `mod` 2 == 0

	mask7::Integer->Integer->Bool
	mask7 i j = (((i+j) `mod` 2) + ((i*j) `mod` 3)) `mod` 2 == 0


	applyMask'::[String]->(Integer->Integer->Bool)->Integer->Integer->[String]
	applyMask' matrix _ (-1) _ = matrix
	applyMask' matrix mask i (-1) = applyMask' matrix mask (i-1) (_size-1)
	applyMask' matrix mask i j 	| mask i j = applyMask' (switchElem matrix i j) mask i (j-1)
									| otherwise = applyMask' matrix mask i (j-1)


	applyMask::[String]->(Integer->Integer->Bool)->[String]
	applyMask matrix mask = applyMask' matrix mask (_size-1) (_size-1)

	compareMatrix::([String],Integer)->([String],Integer)->([String],Integer)
	compareMatrix m1 m2 = if (getPenaltyScore (fst m1)) < (getPenaltyScore (fst m2)) then m1 else m2

	getAlmostFinalMatrix::[String]->([String],Integer)
	getAlmostFinalMatrix matrix = do
								let m1 = compareMatrix (applyMask matrix mask0, 0) (applyMask matrix mask1, 1) 
								let m2 = compareMatrix (applyMask matrix mask2, 2) m1
								let m3 = compareMatrix (applyMask matrix mask3, 3) m2
								let m4 = compareMatrix (applyMask matrix mask4, 4) m3
								let m5 = compareMatrix (applyMask matrix mask5, 5) m4
								let m6 = compareMatrix (applyMask matrix mask7, 6) m5
								let rez = (addStaticFields(fst m6), snd m6)
								rez


	getFinalMatrix::String->[String]
	getFinalMatrix input = do
						let sad = getAlmostFinalMatrix (putData (initMatrix 21) (getFinalMessage input))
						setFormatVersion (fst sad) (getElementList' _formatInfoStrings (snd sad)) 

	imageCreator :: String->Int-> [String]-> IO()
 	imageCreator path size matrix = writePng path $ generateImage pixelRenderer (21 * size) (21 * size)
  			 	where pixelRenderer x y = if getElement'' matrix (x `div` size) (y `div` size) == _black then PixelRGB8 0 0 0 else PixelRGB8 255 255 255
					