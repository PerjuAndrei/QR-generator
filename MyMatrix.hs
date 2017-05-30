module MyMatrix where
	_black = 'X'
	_white = '.'

	--Ia element lista: (v, i) -> v[i]
	getElementList::String->Integer->Char
	getElementList [] _ = error "Error"
	getElementList (hd:tl) 0 = hd
	getElementList (hd:tl) i = getElementList tl (i-1)

	getElementList'::[String]->Integer->String
	getElementList' [] _ = error "Error"
	getElementList' (hd:tl) 0 = hd
	getElementList' (hd:tl) i = getElementList' tl (i-1)

	--Ia element matrice: (a, i, j) -> a[i,j]
	getElement::[String]->Integer->Integer->Char
	getElement [] _ _ = error "Error"
	getElement (hd:tl) 0 j = getElementList hd j
	getElement (hd:tl) i j = getElement tl (i-1) j

	getElementList''::String->Int->Char
	getElementList'' [] _ = error "Error"
	getElementList'' (hd:tl) 0 = hd
	getElementList'' (hd:tl) i = getElementList'' tl (i-1)
	getElement''::[String]->Int->Int->Char
	getElement'' [] _ _ = error "Error"
	getElement'' (hd:tl) 0 j = getElementList'' hd j
	getElement'' (hd:tl) i j = getElement'' tl (i-1) j

	--Seteaza element lista: (v,i,x) {v[i]=x} -> v
	setElemList::String->Integer->Char->String
	setElemList [] _ _ = error "Error"
	setElemList (hd:tl) 0 x = (x:tl)
	setElemList (hd:tl) i x = hd:setElemList tl (i-1) x

	--Seteaza element matrice: (a,i,j,x) {a[i,j]=x} -> a
	setElem::[String]->Integer->Integer->Char->[String]
	setElem [] _ _ _ = error "Error"
	setElem (hd:tl) 0 j x = (setElemList hd j x) : tl 
	setElem (hd:tl) i j x = hd:setElem tl (i-1) j x

	--Schimba elementul black->white si invers
	switchElem::[String]->Integer->Integer->[String]
	switchElem matrix i j 	| getElement matrix i j == _black = setElem matrix i j _white
							| otherwise = setElem matrix i j _black

	--Genereaza o lista cu i elemente de 0: (i) -> v
	initList::Integer->String
	initList 0 = [_white]
	initList i = _white:initList (i-1)

	--Ajuta la generearea matricii
	initMatrix'::Integer->Integer->[String]
	initMatrix' 0 j = initList j : []
	initMatrix' i j = initList j : initMatrix' (i-1) j

	--Genereaza matrice de x*x, plina cu 0: (x)->a
	initMatrix::Integer->[String]
	initMatrix size | size < 1 = error "Size invalid"
				 	| otherwise = initMatrix' (size-1) (size-1)

	--Setam casutele unde vor veni datele despre format cu alb
	resetFormatVersion::[String]->[String]
	resetFormatVersion matrix = do
								let m1 = setElem matrix 0 8 _white
								let m2 = setElem m1 1 8 _white
								let m3 = setElem m2 2 8 _white
								let m4 = setElem m3 3 8 _white
								let m5 = setElem m4 4 8 _white
								let m6 = setElem m5 5 8 _white
								let m7 = setElem m6 7 8 _white
								let m8 = setElem m7 8 8 _white
								let m9 = setElem m8 8 7 _white
								let m10 = setElem m9 8 5 _white
								let m11 = setElem m10 8 4 _white
								let m12 = setElem m11 8 3 _white
								let m13 = setElem m12 8 2 _white
								let m14 = setElem m13 8 1 _white
								let m15 = setElem m14 8 0 _white
								let a1 = setElem m15 20 8 _white
								let a2 = setElem a1 19 8 _white
								let a3 = setElem a2 18 8 _white
								let a4 = setElem a3 17 8 _white
								let a5 = setElem a4 16 8 _white
								let a6 = setElem a5 15 8 _white
								let a7 = setElem a6 14 8 _white
								let a8 = setElem a7 8 13 _white
								let a9 = setElem a8 8 14 _white
								let a10 = setElem a9 8 15 _white
								let a11 = setElem a10 8 16 _white
								let a12 = setElem a11 8 17 _white
								let a13 = setElem a12 8 18 _white
								let a14 = setElem a13 8 19 _white
								let a15 = setElem a14 8 20 _white
								a15


	--Adaugam elementele statice ale qr-codului
	addStaticFields::[String]->[String]
	addStaticFields matrix = resetFormatVersion(addTimingPatterns (setBigSquare (setBigSquare (setBigSquare matrix 0 0 ) 14 0) 0 14))

	addWhiteAroundBigSquares::[String]->[String]
	addWhiteAroundBigSquares matrix = do 
										let p0 = setElem matrix 0 7 _white
										let p1 = setElem p0 1 7 _white
										let p2 = setElem p1 2 7 _white
										let p3 = setElem p2 3 7 _white
										let p4 = setElem p3 4 7 _white
										let p5 = setElem p4 5 7 _white
										let p6 = setElem p5 6 7 _white
										let p7 = setElem p6 7 7 _white
										let p8 = setElem p7 7 6 _white
										let p9 = setElem p8 7 5 _white
										let p10 = setElem p9 7 4 _white
										let p11 = setElem p10 7 3 _white
										let p12 = setElem p11 7 2 _white
										let p13 = setElem p12 7 1 _white
										let p14 = setElem p13 7 0 _white

										let p0' = setElem p14 0 13 _white
										let p1' = setElem p0' 1 13 _white
										let p2' = setElem p1' 2 13 _white
										let p3' = setElem p2' 3 13 _white
										let p4' = setElem p3' 4 13 _white
										let p5' = setElem p4' 5 13 _white
										let p6' = setElem p5' 6 13 _white
										let p7' = setElem p6' 7 13 _white
										let p8' = setElem p7' 7 14 _white
										let p9' = setElem p8' 7 15 _white
										let p10' = setElem p9' 7 16 _white
										let p11' = setElem p10' 7 17 _white
										let p12' = setElem p11' 7 18 _white
										let p13' = setElem p12' 7 19 _white
										let p14' = setElem p13' 7 20 _white

										let p0'' = setElem p14' 13 7 _white
										let p1'' = setElem p0'' 14 7 _white
										let p2'' = setElem p1'' 15 7 _white
										let p3'' = setElem p2'' 16 7 _white
										let p4'' = setElem p3'' 17 7 _white
										let p5'' = setElem p4'' 18 7 _white
										let p6'' = setElem p5'' 19 7 _white
										let p7'' = setElem p6'' 20 7 _white
										let p8'' = setElem p7'' 13 6 _white
										let p9'' = setElem p8'' 13 5 _white
										let p10'' = setElem p9'' 13 4 _white
										let p11'' = setElem p10'' 13 3 _white
										let p12'' = setElem p11'' 13 2 _white
										let p13'' = setElem p12'' 13 1 _white
										let p14'' = setElem p13'' 13 0 _white
										p14''

	--Adauga timing-pattern
	addTimingPatterns::[String]->[String]
	addTimingPatterns matrix = do
								let p1 = setElem matrix 8 6 _black
								let p2 = setElem p1 10 6 _black
								let p3 = setElem p2 12 6 _black
								let p1' = setElem p3 6 8 _black
								let p2' = setElem p1' 6 10 _black
								let p3' = setElem p2' 6 12 _black
								let a1 = setElem p3' 9 6 _white
								let a2 = setElem a1 11 6 _white
								let a1' = setElem a2 6 9 _white
								let a2' = setElem a1' 6 11 _white
								let darkModule = setElem a2' 13 8 _black
								addWhiteAroundBigSquares darkModule


	--Patratul de 3x3 mic 
	setSmallSquare::[String]->Integer->Integer->[String]
	setSmallSquare matrix i j = do
						let coloana0 = setElem matrix (i+2) (j+2) _black
						let coloana1 = setElem coloana0 (i+2) (j+3) _black
						let coloana2 = setElem coloana1 (i+2) (j+4) _black
						let coloana0' = setElem coloana2 (i+3) (j+2) _black
						let coloana1' = setElem coloana0' (i+3) (j+3) _black
						let coloana2' = setElem coloana1' (i+3) (j+4) _black
						let coloana0'' = setElem coloana2' (i+4) (j+2) _black
						let coloana1'' = setElem coloana0'' (i+4) (j+3) _black
						let coloana2'' = setElem coloana1'' (i+4) (j+4) _black
						let a1 = setElem coloana2'' (i+1) (j+1) _white
						let a2 = setElem a1 (i+1) (j+2) _white
						let a3 = setElem a2 (i+1) (j+3) _white
						let a4 = setElem a3 (i+1) (j+4) _white
						let a5 = setElem a4 (i+1) (j+5) _white
						let a1' = setElem a5 (i+5) (j+1) _white
						let a2' = setElem a1' (i+5) (j+2) _white
						let a3' = setElem a2' (i+5) (j+3) _white
						let a4' = setElem a3' (i+5) (j+4) _white
						let a5' = setElem a4' (i+5) (j+5) _white
						let c1 = setElem a5' (i+2) (j+1) _white
						let c2 = setElem c1 (i+3) (j+1) _white
						let c3 = setElem c2 (i+4) (j+1) _white
						let c1' = setElem c3 (i+2) (j+5) _white
						let c2' = setElem c1' (i+3) (j+5) _white
						let c3' = setElem c2' (i+4) (j+5) _white
						c3'
						
	--Patratul de 7x7 mare
	setBigSquare::[String]->Integer->Integer->[String]
	setBigSquare matrix i j = do
						let coloana0 = setElem matrix i j _black
						let coloana1 = setElem coloana0 (i+1) j _black
						let coloana2 = setElem coloana1 (i+2) j _black
						let coloana3 = setElem coloana2 (i+3) j _black
						let coloana4 = setElem coloana3 (i+4) j _black
						let coloana5 = setElem coloana4 (i+5) j _black
						let coloana6 = setElem coloana5 (i+6) j _black
						let coloana0' = setElem coloana6 i (j+6) _black
						let coloana1' = setElem coloana0' (i+1) (j+6) _black
						let coloana2' = setElem coloana1' (i+2) (j+6) _black
						let coloana3' = setElem coloana2' (i+3) (j+6) _black
						let coloana4' = setElem coloana3' (i+4) (j+6) _black
						let coloana5' = setElem coloana4' (i+5) (j+6) _black
						let coloana6' = setElem coloana5' (i+6) (j+6) _black
						let linia1 = setElem coloana6' i (j+1) _black
						let linia2 = setElem linia1 i (j+2) _black
						let linia3 = setElem linia2 i (j+3) _black
						let linia4 = setElem linia3 i (j+4) _black
						let linia5 = setElem linia4 i (j+5) _black
						let linia1' = setElem linia5 (i+6) (j+1) _black
						let linia2' = setElem linia1' (i+6) (j+2) _black
						let linia3' = setElem linia2' (i+6) (j+3) _black
						let linia4' = setElem linia3' (i+6) (j+4) _black
						let linia5' = setElem linia4' (i+6) (j+5) _black
						setSmallSquare linia5' i j

			
	--Pune datele in casutele care merg in sus
	putDataUp::[String]->String->Integer->Integer->[String]						
	putDataUp matrix (h1:h2:h3:h4:h5:h6:h7:h8:[]) i j = do
									 	let p1 = setElem matrix (i+3) (j+1) (if h1=='1' then _black else _white)
									 	let p2 = setElem p1 (i+3) j (if h2=='1' then _black else _white)
									 	let p3 = setElem p2 (i+2) (j+1) (if h3=='1' then _black else _white)
									 	let p4 = setElem p3 (i+2) j (if h4=='1' then _black else _white)
										let p5 = setElem p4 (i+1) (j+1) (if h5=='1' then _black else _white)
										let p6 = setElem p5 (i+1) j (if h6=='1' then _black else _white)
										let p7 = setElem p6 i (j+1) (if h7=='1' then _black else _white)
										let p8 = setElem p7 i j (if h8=='1' then _black else _white)
										p8
	--Pune datele in casutele care coboara
	putDataDown::[String]->String->Integer->Integer->[String]	
	putDataDown matrix (h1:h2:h3:h4:h5:h6:h7:h8:[]) i j = do
										let p1 = setElem matrix (i+3) (j+1) (if h7=='1' then _black else _white)
									 	let p2 = setElem p1 (i+3) j (if h8=='1' then _black else _white)
									 	let p3 = setElem p2 (i+2) (j+1) (if h5=='1' then _black else _white)
									 	let p4 = setElem p3 (i+2) j (if h6=='1' then _black else _white)
										let p5 = setElem p4 (i+1) (j+1) (if h3=='1' then _black else _white)
										let p6 = setElem p5 (i+1) j (if h4=='1' then _black else _white)
										let p7 = setElem p6 i (j+1) (if h1=='1' then _black else _white)
										let p8 = setElem p7 i j (if h2=='1' then _black else _white)
										p8

	--Pt casuta 16								
	putDataUp'::[String]->String->Integer->Integer->[String]						
	putDataUp' matrix (h1:h2:h3:h4:h5:h6:h7:h8:[]) i j = do
									 	let p1 = setElem matrix (i+4) (j+1) (if h1=='1' then _black else _white)
									 	let p2 = setElem p1 (i+4) j (if h2=='1' then _black else _white)
									 	let p3 = setElem p2 (i+3) (j+1) (if h3=='1' then _black else _white)
									 	let p4 = setElem p3 (i+3) j (if h4=='1' then _black else _white)
										let p5 = setElem p4 (i+1) (j+1) (if h5=='1' then _black else _white)
										let p6 = setElem p5 (i+1) j (if h6=='1' then _black else _white)
										let p7 = setElem p6 i (j+1) (if h7=='1' then _black else _white)
										let p8 = setElem p7 i j (if h8=='1' then _black else _white)
										p8	
	--Pt Casuta 17										
	putDataDown'::[String]->String->Integer->Integer->[String]	
	putDataDown' matrix (h1:h2:h3:h4:h5:h6:h7:h8:[]) i j = do
										let p1 = setElem matrix (i+4) (j+1) (if h7=='1' then _black else _white)
									 	let p2 = setElem p1 (i+4) j (if h8=='1' then _black else _white)
									 	let p3 = setElem p2 (i+3) (j+1) (if h5=='1' then _black else _white)
									 	let p4 = setElem p3 (i+3) j (if h6=='1' then _black else _white)
										let p5 = setElem p4 (i+1) (j+1) (if h3=='1' then _black else _white)
										let p6 = setElem p5 (i+1) j (if h4=='1' then _black else _white)
										let p7 = setElem p6 i (j+1) (if h1=='1' then _black else _white)
										let p8 = setElem p7 i j (if h2=='1' then _black else _white)
										p8

	putData::[String]->[String]->[String]
	putData matrix (h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:[]) = do
										let m1 = putDataUp matrix h1 17 19
										let m2 = putDataUp m1 h2 13 19
										let m3 = putDataUp m2 h3 9 19
										let m4 = putDataDown m3 h4 9 17
										let m5 = putDataDown m4 h5 13 17
										let m6 = putDataDown m5 h6 17 17
										let m7 = putDataUp m6 h7 17 15
										let m8 = putDataUp m7 h8 13 15
										let m9 = putDataUp m8 h9 9 15
										let m10 = putDataDown m9 h10 9 13
										let m11 = putDataDown m10 h11 13 13
										let m12 = putDataDown m11 h12 17 13
										let m13 = putDataUp m12 h13 17 11
										let m14 = putDataUp m13 h14 13 11
										let m15 = putDataUp m14 h15 9 11
										let m16 = putDataUp' m15 h16 4 11
										let m17 = putDataUp m16 h17 0 11
										let m18 = putDataDown m17 h18 0 9
										let m19 = putDataDown' m18 h19 4 9
										let m20 = putDataDown m19 h20 9 9
										let m21 = putDataDown m20 h21 13 9
										let m22 = putDataDown m21 h22 17 9
										let m23 = putDataUp m22 h23 9 7
										let m24 = putDataDown m23 h24 9 4
										let m25 = putDataUp m24 h25 9 2
										let m26 = putDataDown m25 h26 9 0
										m26

	setFormatVersion::[String]->String->[String]
	setFormatVersion matrix (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:[]) = do
										let m1 = setElem matrix 0 8 (if h0 == '1' then _black else _white)
										let m1' = setElem m1 8 20 (if h0 == '1' then _black else _white)
										let m2 = setElem m1' 1 8 (if h1 == '1' then _black else _white)
										let m2' = setElem m2 8 19 (if h1 == '1' then _black else _white)
										let m3 = setElem m2' 2 8 (if h2 == '1' then _black else _white)
										let m3' = setElem m3 8 18 (if h2 == '1' then _black else _white)
										let m4 = setElem m3' 3 8 (if h3 == '1' then _black else _white)
										let m4' = setElem m4 8 17 (if h3 == '1' then _black else _white)
										let m5 = setElem m4' 4 8 (if h4 == '1' then _black else _white)
										let m5' = setElem m5 8 16 (if h4 == '1' then _black else _white)
										let m6 = setElem m5' 5 8 (if h5 == '1' then _black else _white)
										let m6' = setElem m6 8 15 (if h5 == '1' then _black else _white)
										let m7 = setElem m6' 7 8 (if h6 == '1' then _black else _white)
										let m7' = setElem m7 8 14 (if h6 == '1' then _black else _white)
										let m8 = setElem m7' 8 8 (if h7 == '1' then _black else _white)
										let m8' = setElem m8 8 13 (if h7 == '1' then _black else _white)
										let m9 = setElem m8' 8 7 (if h8 == '1' then _black else _white)
										let m9' = setElem m9 14 8 (if h8 == '1' then _black else _white)
										let m10 = setElem m9' 8 5 (if h9 == '1' then _black else _white)
										let m10' = setElem m10 15 8 (if h9 == '1' then _black else _white)
										let m11 = setElem m10' 8 4 (if h10 == '1' then _black else _white)
										let m11' = setElem m11 16 8 (if h10 == '1' then _black else _white)
										let m12 = setElem m11' 8 3 (if h11 == '1' then _black else _white)
										let m12' = setElem m12 17 8 (if h11 == '1' then _black else _white)
										let m13 = setElem m12' 8 2 (if h12 == '1' then _black else _white)
										let m13' = setElem m13 18 8 (if h12 == '1' then _black else _white)
										let m14 = setElem m13' 8 1 (if h13 == '1' then _black else _white)
										let m14' = setElem m14 19 8 (if h13 == '1' then _black else _white)
										let m15 = setElem m14' 8 0 (if h14 == '1' then _black else _white)
										let m15' = setElem m15 20 8 (if h14 == '1' then _black else _white)
										m15'
