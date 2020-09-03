$STORAGE:2
*
*     IMPORTGS.FOR
*     POUR IMPORTER DES DONNEES METEO CONTENUES DANS DES FICHIERS ASCII
*     DANS DES GRILLES DE SAISIE clicom
*     A PARTIR DES INDICATIONS DONNEES DANS IMPORTGS.INF
*     EXEMPLE DE FICHIER IMPORTGS.INF
*     CA COMMENCE LIGNE 10 IL FAUT JUSTE OTER LES COLONNE 1 2 ET 3 : *$*
*
*$*[debut de importgs.inf]
*$*rem  Paris 
*$*rem  le 18 octobre 1999
*$*rem  Le travail et les quelques r‚flexions ci-dessous font suite
*$*rem  … la prise de contact initi‚e d‚but juin 1999
*$*rem  avec Yann Sultan  (t‚l : 04 94 61 92 92)
*$*rem  des      
*$*rem  Etablissements
*$*rem  DEGREANE 
*$*rem  28 rue Font-Pr‚ 
*$*rem  B.P. 954 Toulon cedex
*$*rem 
*$*rem  Bonjour,
*$*rem  Voici une proposition pour r‚aliser
*$*rem  un fichier de configuration destin‚
*$*rem  au CATRA et … CLICOM (MISSION SOFREAVIA POUR LE LIBAN)
*$*rem  le nom de ce fichier (importgs.inf par d‚faut) est cependant
*$*rem  libre car il peut ˆtre contenu dans le fichier importgs.ini
*$*rem  qui est lu tout au d‚but du programme.
*$*rem   
*$*rem  . le CATRA y trouvera tous les ‚l‚ments de structure
*$*rem  afin de fabriquer des fichiers … importer
*$*rem  dans les grilles de saisie CLICOM
*$*rem  . CLICOM y trouvera toutes les informations
*$*rem  utiles au bon d‚roulement des importations de donn‚es m‚t‚orologiques
*$*rem  fournies r‚guliŠrement par le CATRA 
*$*rem  (… un rythme vraisemblablement quotidien)
*$*rem 
*$*rem  Le programme IMPORTGS.exe de CLICOM (ON EST DEDANS !)
*$*rem  trouvera dans ce fichier, entre autre :
*$*rem 
*$*rem  - Une lettre code (au lieu des 3 habituelles dans CLICOM)
*$*rem    correspondant … un PAS DE TEMPS donn‚ (H,Q ---> HLY,DLY)
*$*rem    (notion clef pour entrer dans une grille de saisie CLICOM) 
*$*rem    
*$*rem  - Pour chacunes des 30 stations (300 au maximum)
*$*rem    le code station CATRA, le code station CLICOM qui lui correspond
*$*rem    et un code relatif … la fr‚quence (et/ou … la nature) des observations 
*$*rem    (un produit de nombres premiers : 2 3 5 7 11 13 17)
*$*rem 
*$*rem  - les noms des fichiers cr‚s par le CATRA
*$*rem    
*$*rem  - le nombre et surtout les codes CLICOM des paramŠtres 
*$*rem    contenus dans chacuns de ces fichiers 
*$*rem    (40 paramŠtres maxi par fichier)
*$*rem
*$*rem  - le s‚parateur de champs (ou colonnes).
*$*rem    N.B. : Ce ne pourra pas ˆtre un espace blanc, ni un point. 
*$*rem    La ligne peut finir par un s‚parateur
*$*rem    Il y a autant de champs (ou colonnes)
*$*rem    que de paramŠtres (plus 5 colonnes pour identifier le code
*$*rem    station, l'ann‚e(en quatre chiffres), le mois, 
*$*rem    le jour et l'heure UTC. 
*$*rem    Pour les fichiers quotidiens le champ heure existe aussi mais
*$*rem    a une valeur conventionnelle : 99)
*$*rem  
*$*rem  - en cas de manque temporaire ou recurent la colonne peut 
*$*rem    soit rester vide,
*$*rem    soit ˆtre remplie par un code valable pour tous les paramŠtres
*$*rem    dans ce cas il est imp‚rativement inf‚rieur ou ‚gal … -999
*$*rem    (-999 suffit en m‚t‚o et ne gonfle pas trop la taille des fichiers)
*$*rem    X0000002,1999,6,29,99,,,,,,,,,,,,,,,
*$*rem    signifie que quatorze relev‚s quotidiens de la journ‚e du 29 juin 1999 
*$*rem    pour la station X0000002 sont manquants
*$*rem    
*$*rem  - Trois autres champs qui sont utiles au CATRA
*$*rem      
*$*rem  - Les paramŠtres CLICOM inconnus du CATRA 
*$*rem    (exemple : 191 Nature de la neige)
*$*rem    sont gard‚s pour m‚moire.
*$*rem    Ca donne par exemple :
*$*rem    inconnu2________=5,04,20,191,00,,,
*$*rem
*$*rem    Comme ce fichier est destin‚ … ˆtre modifi‚ … volont‚ 
*$*rem    (et ‚ventuellement … la main)
*$*rem    il a sembl‚ utile de "colonner" au maximum
*$*rem    exemple :
*$*rem  horaire6________=5,01,06,155,00,A0,17,2 
*$*rem  horaire12_______=5,01,12,164,00,D0,19,2
*$*rem
*$*rem  D'un coup d'oeil on lit que 
*$*rem  le paramŠtre clicom 155
*$*rem  est un paramŠtre horaire[5] qui se trouve en colonne 6[06] dans 
*$*rem  le fichier de type horaire [H.x01] … importer dans
*$*rem  la grille de saisie 001[01]
*$*rem  (le facteur d'echelle … appliquer est 1 (10**0)[00]
*$*rem  c'est … dire que les donn‚es sont des entiers
*$*rem  01 signfierait qu'il faut multiplier les donn‚es du fichier par 10
*$*rem  avant de faire l'importation dans la grille de saisie CLICOM
*$*rem  -1 qu'il faut diviser par 10)
*$*rem  et que ...
*$*rem  le paramŠtre clicom 164
*$*rem  est un paramŠtre horaire[5] qui se trouve en colonne 12[12] dans 
*$*rem  le fichier de type horaire [H.x01] … importer dans
*$*rem  la grille de saisie 001[01]
*$*rem  (le facteur d'echelle est 1 l… aussi)
*$*rem 
*$*rem  L'ordre des lignes dans ce fichier
*$*rem  n'a pas d'importance pour le programme IMPORTGS
*$*rem
*$*rem  Il vous sera certainement assez facile
*$*rem  s'il vous plait 
*$*rem  d'envoyer vous commentaires, remarques ou desirata … l'auteur :
*$*rem 
*$*rem  G‚rard Mayen‡on 
*$*rem  METEO-FRANCE
*$*rem  DIRIC/CLIM
*$*rem  26 boulevard Jourdan
*$*rem  75014 Paris
*$*rem  t‚l : 01 45 56 57 85
*$*rem  e-mail : gerard.mayencon@meteo.fr
*$*rem  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*$*rem [debut de clicom.ini (alias importgs.inf) ligne 1]
*$*rem Fichier de configuration pour Catra et Clicom
*$*rem Gerard Mayencon a Meteo-France 
*$*rem Version mise a jour a Paris le lundi 18 octobre 1999
*$*rem * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*$*rem rem Place en debut de ligne 
*$*rem sert a commenter (notamment le mnemonique x)
*$*rem !
*$*rem x
*$*rem Liste des mnemoniques :
*$*rem * * * * * * * * * * *
*$*rem [purement catra]
*$*rem chemin_transfert=
*$*rem nombre_parametres_horaires=
*$*rem nombre_parametres_inconnus=
*$*rem lecteur=
*$*rem nombre_fichiers=
*$*rem nombre_fichier_N=       (N varie de 1 a 7)
*$*rem nombre_parametres_Max=  (La valeur maxi est inferieure ou egale a 40 SVP) 
*$*rem code_invalide=
*$*rem separateur=
*$*rem
*$*rem [purement clicom]
*$*rem chemin_transfert_CLICOM=
*$*rem 
*$*rem lecteur_CLICOM=
*$*rem
*$*rem [catra et clicom]
*$*rem code_Tair_Horaire=101   (sic ?)
*$*rem code_Tairmin_Horaire=151
*$*rem code_UU_Horaire=105
*$*rem code_UUmin_Horaire=159
*$*rem code_pas_de_tempsN=     (N varie de 1 a 7)
*$*rem code_stationN=          (N varie de 1 a 300)
*$*rem complementaireN_=     (N varie de 1 a 40 au maximum)
*$*rem horaireN________=     (N varie de 1 a 40 au maximum)
*$*rem synopN__________=     (N varie de 1 a 40 au maximum)
*$*rem boueeN__________=     (N varie de 1 a 40 au maximum)
*$*rem quotidienN______=     (N varie de 1 a 40 au maximum)
*$*rem inconnuN________=     (N varie de 1 a 39 au maximum)
*$* 
*$*[general]
*$*  
*$*rem Dans CLICOM il y a 7 pas de temps qui sont prevus 
*$*rem Le code adopte conventionnellement est depuis longtemps le suivant :
*$*rem le pas de temps 1 est le pas de temps mensuel
*$*rem le pas de temps 2 est le pas de temps decadaire
*$*rem le pas de temps 3 est le pas de temps quotidien
*$*rem le pas de temps 4 est le pas de temps synoptique (8 observations par jour)
*$*rem le pas de temps 5 est le pas de temps horaire
*$*rem le pas de temps 6 est le pas de temps quinze minutes
*$*rem le pas de temps 7 est le pas de temps pour les donn‚es des Radio Sondes
*$*rem Des nombre premiers distincs sont affectes a chaque pas de temps pour
*$*rem un usage ulterieur
*$*rem !
*$*code_pas_de_temps1=M,2
*$*code_pas_de_temps2=D,3
*$*code_pas_de_temps3=Q,5
*$*code_pas_de_temps4=S,7
*$*code_pas_de_temps5=H,11
*$*code_pas_de_temps6=Z,13
*$*code_pas_de_temps7=A,17
*$* 
*$*[catra]
*$* 
*$*rem Il y a N fichiers generes par Catra
*$*rem a chaque fichier corespond une grille de saisie et une seule
*$*rem a chaque grille de saisie correspond une structure de fichier et 
*$*une seule
*$*rem Par exemple : 
*$*rem Les fichiers Hourly1.00 a Hourly1.99 alimentent tous une grille
*$*rem de saisie horaire definie dans Clicom sous le numero 001

*$*rem Lorsque le compteur Catra arrive a 99 il repasse a 00
*$*rem par exemple :
*$*rem apres Dayly10.07 catra fabriquera Dayly10.08
*$*rem ces deux fichier ont la meme structure compatible avec la grille
*$*rem CLICOM quotidienne 010 
*$*rem Les traitements d'alimentation des grilles Clicom finiront de temps
*$*rem en temps par une purge des fichiers traites
*$*rem (eventuellement precedee par un archivage de ces fichiers)
*$*rem !
*$*nombre_fichiers=8
*$* 
*$*rem Le detail par structure temporelle est donne ici
*$*rem !
*$*nombre_fichier_1=0
*$*nombre_fichier_2=0
*$*nombre_fichier_3=3
*$*nombre_fichier_4=0
*$*nombre_fichier_5=5
*$*nombre_fichier_6=0
*$*nombre_fichier_7=0
*$* 
*$*rem Il y a 40 parametres par fichier au maximum
*$*rem !
*$*nombre_parametres_Max=40
*$* 
*$*rem Nota Bene : ,, equivaut a ,-999,
*$*rem !
*$*code_invalide=-999
*$* 
*$*rem Ce que l'on veut est utilisable comme separateur
*$*rem je conseille la virgule (,)
*$*rem mais SVP exclure le point (.) et l'espace blanc ( )  
*$*rem !
*$*separateur=,
*$* 
*$*chemin_transfert=\\COMPAQ\TRF
*$*lecteur=o:
*$* 
*$*[clicom]
*$*rem Pour memoire le repertoire ou se trouvent les donnees a importer
*$*chemin_transfert_CLICOM=\tmp
*$*rem Pour memoire l'unite ou se trouvent les donnees a importer
*$*lecteur_CLICOM=c
*$* 
*$*[catra et clicom]
*$*code_Tair_Horaire=101
*$*code_Tairmin_Horaire=151
*$*code_UU_Horaire=105
*$*code_UUmin_Horaire=159
*$* 
*$*[stations]
*$*rem A mettre a jour en aout 1999
*$*rem Si besoin la colonne 1 pourrait servir a renseigner sur 
*$*rem le type de la station
*$*rem stations synoptiques + bouees
*$*rem !
*$*code_station1=30,00000001,IH100039
*$*code_station2=30,00000012,IL100017
*$*code_station3=30,00000020,LC100014
*$*code_station4=30,10000020,LN100031
*$*code_station5=30,20000020,MC100025
*$*code_station6=30,30000020,MN100007
*$*code_station6=30,40000020,MN100007
*$*code_station7=0,,
*$*code_station8=0,,
*$*code_station9=0,,
*$*code_station10=0,,
*$*code_station11=0,,
*$*code_station12=0,,
*$*code_station13=0,,
*$*code_station14=0,,
*$*code_station15=0,,
*$*code_station16=0,,
*$*code_station17=0,,
*$*code_station18=0,,
*$*code_station19=0,,
*$* 
*$*rem stations climatologiques
*$*rem !
*$*code_station20=0,,
*$*code_station21=0,,
*$*code_station22=0,,
*$*code_station23=0,,
*$*code_station24=0,,
*$*code_station25=0,,
*$*code_station26=0,,
*$*code_station27=0,,
*$*code_station28=0,,
*$*code_station29=0,,
*$*code_station30=0,,
*$*code_station31=0,,
*$*code_station32=0,,
*$*code_station33=0,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
*$* 
*$*[fichier1]
*$*rem pour catra
*$*typ_fichier=H
*$*nom_fichier=H001
*$*nombre_parametres_horaires=13
*$*nombre_parametres_inconnus=2
*$*rem Description des champs utiles pour catra et clicom
*$*rem Avec en regard la significations des diverses colonnes : 
*$*rem - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*$*rem                pas de temps (1,2,3,4,5,6 ou 7)
*$*rem                  numero grille (01 a 99)
*$*rem                     num. colonne (01 a 40)
*$*rem                        num. CLICOM (001 a 999)
*$*rem                            facteur d'‚chelle (Puissances de 10) ou conversions
*$*rem                               Champ CATRA 1
*$*rem                                 Champ CATRA 2
*$*rem                                    Champ CATRA 3
*$*horaire1________=5,001,01,101,00,A0,3,5
*$*horaire2________=5,001,02,151,00,A0,3,5
*$*horaire3________=5,001,03,152,00,A0,8,2
*$*horaire4________=5,001,04,153,00,A0,10,5
*$*horaire5________=5,001,05,154,00,A0,15,2
*$*horaire6________=5,001,06,155,00,A0,17,2
*$*horaire7________=5,001,07,105,00,D0,7,3
*$*horaire8________=5,001,08,159,00,D0,7,3
*$*horaire9________=5,001,09,119,00,D0,10,2
*$*horaire10_______=5,001,10,160,00,D0,12,3
*$*horaire11_______=5,001,11,120,00,D0,15,2
*$*horaire12_______=5,001,12,164,00,D0,19,2
*$*horaire13_______=5,001,13,161,00,D0,17,2
*$*inconnu1________=5,001,14,162,00,,,
*$*inconnu2________=5,001,15,163,00,,,
*$* 
*$*[fichier2]
*$*rem pour catra
*$*typ_fichier=H
*$*nom_fichier=H002
*$*nombre_parametres_horaires=4
*$*nombre_parametres_synoptiques=3
*$*nombre_parametres_complementaires=5
*$*nombre_parametres_inconnus=6
*$*rem description des champs utiles pour catra et clicom
*$*complementaire1_=5,002,01,156,00,A4,4,5
*$*complementaire2_=5,002,02,157,00,A5,4,5
*$*complementaire3_=5,002,03,167,00,A6,4,5
*$*complementaire4_=5,002,04,168,00,A7,4,5
*$*complementaire5_=5,002,05,158,00,A8,4,5
*$*horaire1________=5,002,06,104,00,B0,3,5
*$*horaire2________=5,002,07,132,00,F0,3,2 
*$*horaire3________=5,002,08,133,00,E1,3,6 
*$*horaire4________=5,002,09,166,00,D0,3,4
*$*synop1__________=5,002,10,114,00,S1,OMM,13,2
*$*synop2__________=5,002,11,115,00,S1,OMM,15,2
*$*synop3__________=5,002,12,195,00,S3,924,4,1
*$*inconnu1________=5,002,13,165,00,,,
*$*inconnu2________=5,002,14,121,00,,,
*$*inconnu3________=5,002,15,122,00,,,
*$*inconnu4________=5,002,16,150,00,,,
*$*inconnu5________=5,002,17,135,00,,,
*$*inconnu6________=5,002,18,134,00,,,
*$* 
*$*[fichier3]
*$*rem pour catra
*$*typ_fichier=H
*$*nom_fichier=H003
*$*nombre_parametres_horaires=6
*$*nombre_parametres_bouee=5
*$*nombre_parametres_inconnus=6
*$*rem description des champs utiles pour catra et clicom
*$*horaire1________=5,003,01,112,00,C0,3,3
*$*horaire2________=5,003,02,111,00,C0,6,4
*$*horaire3________=5,003,03,140,00,C0,10,2
*$*horaire4________=5,003,04,143,00,C0,21,3
*$*horaire5________=5,003,05,144,00,C0,24,4
*$*horaire6________=5,003,06,113,00,C0,28,3
*$*bouee1__________=5,003,07,196,00,DM,4,3
*$*bouee2__________=5,003,08,197,00,H,3,4
*$*bouee3__________=5,003,09,198,00,h,3,4
*$*bouee4__________=5,003,10,199,00,PM,4,3
*$*bouee5__________=5,003,11,200,00,T,3,4
*$*inconnu1________=5,003,12,141,00,,,
*$*inconnu2________=5,003,13,142,00,,,
*$*inconnu3________=5,003,14,147,00,,,
*$*inconnu4________=5,003,15,148,00,,,
*$*inconnu5________=5,003,16,146,00,,,
*$*inconnu6________=5,003,17,145,00,,,
*$* 
*$*[fichier4]
*$*rem pour catra
*$*typ_fichier=H
*$*nom_fichier=H099
*$*nombre_parametres_synoptiques=18
*$*nombre_parametres_inconnus=3
*$*rem description des champs utiles pour catra et clicom 
*$*synop1__________=5,099,01,110,00,S1,OMM,9,2
*$*synop2__________=5,099,02,170,00,S1,OMM,12,1
*$*synop3__________=5,099,03,171,00,S3,8,2,1
*$*synop4__________=5,099,04,173,00,S3,8,4,2
*$*synop5__________=5,099,05,174,00,S3,8,3,1
*$*synop6__________=5,099,06,175,00,S1,8,3,1
*$*synop7__________=5,099,07,176,00,S1,8,4,1
*$*synop8__________=5,099,08,177,00,S1,8,5,1
*$*synop9__________=5,099,09,178,00,S1,7,2,2
*$*synop10_________=5,099,10,179,00,S1,7,4,2
*$*synop11_________=5,099,11,180,00,S1,7,4,2
*$*synop12_________=5,099,12,181,00,S3,3,2,1
*$*synop13_________=5,099,13,182,00,S3,4,2,1
*$*synop14_________=5,099,14,193,00,S3,4,3,3
*$*synop15_________=5,099,15,107,00,S1,4,2,4
*$*synop16_________=5,099,16,108,00,S1,4a3,3,3
*$*synop17_________=5,099,17,106,00,S1,3,2,4
*$*synop18_________=5,099,18,103,00,S1,2,2,4
*$*inconnu1________=5,099,19,194,00,,,
*$*inconnu2________=5,099,20,191,00,,,
*$*inconnu3________=5,099,21,192,00,,,
*$*
*$*[fichier6]
*$*
*$*typ_fichier=Q
*$*nom_fichier=Q001
*$*nombre_parametres_quotidiens=14
*$*nombre_parametres_inconnus=4
*$*quotidien1______=3,001,01,005,01,B0,4,6
*$*quotidien2______=3,001,02,006,00,B0,10,5
*$*quotidien3______=3,001,03,007,01,B0,28,5
*$*quotidien4______=3,001,04,017,00,D0,4,3
*$*quotidien5______=3,001,05,016,01,D0,7,3
*$*quotidien6______=3,001,06,028,00,D0,10,4
*$*quotidien7______=3,001,07,015,00,D0,14,3
*$*quotidien8______=3,001,08,050,00,D0,17,4
*$*quotidien9______=3,001,09,095,00,D0,21,4
*$*quotidien10_____=3,001,10,096,00,D0,25,4
*$*quotidien11_____=3,001,11,097,00,D0,29,4
*$*quotidien12_____=3,001,12,098,51,D0,33,4
*$*quotidien13_____=3,001,13,099,-1,D0,37,4
*$*quotidien14_____=3,001,14,045,00,E1,4,4
*$*inconnu1________=3,001,15,094,00,,,
*$*inconnu2________=3,001,16,043,00,,,
*$*inconnu3________=3,001,17,044,00,,,
*$*inconnu4________=3,001,18,046,00,,,
*$* 
*$*[fichier7]
*$*
*$*typ_fichier=Q
*$*nom_fichier=Q010
*$*nombre_parametres_quotidiens=5
*$*nombre_parametres_inconnus=4
*$*quotidien1______=3,010,01,058,00,C1,13,4
*$*quotidien2______=3,010,02,059,00,C1,10,3
*$*quotidien3______=3,010,03,057,00,C1,4,6
*$*quotidien4______=3,010,04,060,00,C2,13,4
*$*quotidien5______=3,010,05,052,00,C2,4,6
*$*inconnu1________=3,010,06,055,00,,,
*$*inconnu2________=3,010,07,056,00,,,
*$*inconnu3________=3,010,08,053,00,,,
*$*inconnu4________=3,010,09,062,00,,,
*$* 
*$*[fichier8]
*$*
*$*typ_fichier=Q
*$*nom_fichier=Q003
*$*nombre_parametres_quotidiens=33
*$*nombre_parametres_inconnus=1
*$*quotidien1______=3,003,01,004,00,A0,4,5
*$*quotidien2______=3,003,02,002,00,A0,18,5
*$*quotidien3______=3,003,03,020,00,A0,23,4
*$*quotidien4______=3,003,04,003,00,A0,9,5
*$*quotidien5______=3,003,05,021,00,A0,14,4
*$*quotidien6______=3,003,06,012,00,TD,18,5
*$*quotidien7______=3,003,07,013,00,TD,9,5
*$*quotidien8______=3,003,08,014,00,TD,4,5
*$*quotidien9______=3,003,09,082,00,A4,4,5
*$*quotidien10_____=3,003,10,073,00,A5,4,5
*$*quotidien11_____=3,003,11,072,00,A6,4,5
*$*quotidien12_____=3,003,12,078,00,A7,4,5
*$*quotidien13_____=3,003,13,075,00,A8,4,5
*$*quotidien14_____=3,003,14,081,00,A4,9,5
*$*quotidien15_____=3,003,15,065,00,A5,9,5
*$*quotidien16_____=3,003,16,071,00,A6,9,5
*$*quotidien17_____=3,003,17,077,00,A7,9,5
*$*quotidien18_____=3,003,18,069,00,A8,9,5
*$*quotidien19_____=3,003,19,080,00,A4,18,5
*$*quotidien20_____=3,003,20,064,00,A5,18,5
*$*quotidien21_____=3,003,21,070,00,A6,18,5
*$*quotidien22_____=3,003,22,076,00,A7,18,5
*$*quotidien23_____=3,003,23,068,00,A8,18,5
*$*quotidien24_____=3,003,24,022,00,A4,14,4
*$*quotidien25_____=3,003,25,023,00,A5,14,4
*$*quotidien26_____=3,003,26,019,00,A6,14,4
*$*quotidien27_____=3,003,27,041,00,A7,14,4
*$*quotidien28_____=3,003,28,024,00,A8,14,4
*$*quotidien29_____=3,003,29,025,00,A4,23,4
*$*quotidien30_____=3,003,30,026,00,A5,23,4
*$*quotidien31_____=3,003,31,047,00,A6,23,4
*$*quotidien32_____=3,003,32,054,00,A7,23,4
*$*quotidien33_____=3,003,33,027,00,A8,23,4
*$*inconnu1________=3,003,34,086,00,,,
*$*rem [(sauf erreur ou ommission)fin de clicom.ini]


***********************************************************************
*      IMPORT23.FOR                                                 *
*      PROGRAMA REALIZADO POR  : LUIS CARRASCO CARE                 *
*      FECHA DE REALIZACION    : 30/Octubre/1996                    *
*                                                                   *
*      INSTITUCION             : AR III CLICOM AREA SUPPORT CENTRE  *
*                                DIRECCION METEOROLOGICA DE CHILE   *
*            +                                                      *
*      (MODIFICATIONS PAR                                           *
*      GERARD MAYENCON                                              * 
*      METEO-FRANCE            : JUIN-OCTOBRE 1999)                 *
*                                                                   *
* ---> IMPORTGS.FOR                                                 *               *
*********************************************************************
************************************************************************
*     THIS IS A  PROGRAM THAT LOADS DATA FROM AN EXISTING DIGITAL      *
*     DATA FILE INTO THE KEY-ENTRY/QC FILES.  THE DATA CAN THEN        *
*     BE RUN THROUGH NORMAL CLICOM QUALITY CONTROL.  THE "INC" FILES   *
*     BELOW SHOULD BE IN THE \SRC\MAIN DIRECTORY OF THE DISKETTE WHICH *
*     HOLDS THE SOURCE FILE FOR CLICOM PROGRAMS.                       *
*                                                                      *
*     THE PROGRAM ASSUMES THAT YOUR DATA ARE RECORDED BY OBSERVATION * *
*     THAT IS, MANY DIFFERENT ELEMENTS FOR A SINGLE TIME IN EACH RECORD*
*                                                                      *
************************************************************************
C

      INTERFACE TO FUNCTION SYSTEM[C] (STRING)
      INTEGER*2 SYSTEM
      CHARACTER*1 STRING[REFERENCE]
      END

      PROGRAM IMPORTGS

*     ECRIT PAR GERARD MAYENCON
*     (ADAPTATION DU PROGRAMME IMPORT23, DESTINEE AU LIBAN)

*     PROGRAM IMPORT23 
*     ECRIT PAR LUIS CARRASCO CARE

      PARAMETER (IPT=7,ING=12,INC=40,NSX=300)

*     IPT   NOMBRE MAXI DE PAS TEMPORELS
*     ING   NOMBRE MAXI DE GRILLES DE SAISIES (PAR PAS TEMPOREL)
*     INC   NOMBRE MAXI DE COLONNES (OU PARAMETRES PAR GRILLE DE SAISIE)
*     NSX   NOMBRE MAXI DE STATIONS

$INCLUDE:'VAL1.INC'
$INCLUDE:'INDEX.INC'


      INTEGER*2 SYSTEM
      INTEGER*2 STRUC(IPT,ING,INC,2)
      INTEGER*2 INP(0:IPT)
      INTEGER*2 NUMG(IPT,0:ING)
      INTEGER*2 YEAR,MONTH,DAY,HOUR
      INTEGER*2 TBLNUMLIN(7)
      INTEGER*2 DSETID,LOYR,HIYR,LOMO,HIMO
      INTEGER*2 INELEM(INC),ELMCOL(INC),IHRLBL(24)
      INTEGER*2 FE(INC)
      INTEGER*4 I4VALUE,RECCOUNT,NRECCOUNT
      INTEGER*2 IAC,IMC,IJC

      REAL*4  INVAL(INC),TRACE
      REAL*4  XAF,XBF,XAHG

      CHARACTER*1 RTNCODE,REPLY
      CHARACTER*2 AMONTH,ADAY,AHOUR,HOURLBL(24)
*      CHARACTER*2 RTNFLAG
      CHARACTER*3 RECTYPE,TBLRECTYP(7),ADSID
      CHARACTER*4 AYEAR
      CHARACTER*6 OUTCNT(2)
      CHARACTER*8 OUTSTN(2),LASTOLD
      CHARACTER*15 AFIELD(9)
      CHARACTER*8 ERRSRC
      CHARACTER*21 IDKEY,LASTID
*      CHARACTER*64 HELPFILE
*      CHARACTER*25 WORK
*      CHARACTER*71 WORDS
      CHARACTER*8 INSTN,OLDSTN(NSX),NEWSTN(NSX)
*      CHARACTER*8 HOLDSTN
*      CHARACTER*8 TWINSTN
      CHARACTER*1 OKWRITE
      CHARACTER FINI*30
      CHARACTER ZONA*80
      CHARACTER ZORO*288
      CHARACTER REP*1
      CHARACTER CPT(IPT)*1
      CHARACTER LEC*1
      CHARACTER IND(NSX,2)*8
      CHARACTER NOMFIC*20
      CHARACTER XFM*8
      CHARACTER CHEMIN*30
      CHARACTER XFICH*50
      CHARACTER CODEM*1

      LOGICAL OKELEM(INC),SORECR

      DATA CPT/'M','D','Q','S','H','Z','A'/
      DATA RECCOUNT/0/,NRECCOUNT/0/,OUTCNT/' ','\'/,LASTID/' '/
      DATA AFIELD /9*' '/,OUTSTN/' ','\'/
      DATA AMONTH/' '/
      DATA TBLRECTYP /'MLY','10D','DLY','SYN','HLY','15M','U-A'/
      DATA TBLNUMLIN /12,36,31,8,24,96,100/
*      DATA HELPFILE/'P:\HELP\IMPORTGS.HLP'/
      DATA OKELEM/INC*.FALSE./
      OUTCNT(2)=CHAR(0)
      OUTSTN(2)=CHAR(0)

*     CONSTANTES DESTINEES AUX TRANSFORMATIONS DEGRES C <---> DEGRES F

      XAF=5./9.
      XBF=-320.

*     CONSTANTE DESTINEE AUX TRANSFORMATIONS HECTOPASCALS <--->MILLIMETRES Hg
   
      XAHG=10135./760.
      
*     DETERMINATION DE LA DATE ET DE L'HEURE COURANTE

      CALL GETDAT(IAC,IMC,IJC)
      IF(IAC.GE.2000)THEN
        IAC=IAC-2000
      ELSE
        IAC=IAC-1900
      ENDIF

      KAC=INT(IAC)
      KMC=INT(IMC)
      KJC=INT(IJC)

*     LE MOIS EST CODEE EN UNE LETTRE A=JANVIER 2=FEVRIER ETC..

      CODEM=CHAR(ICHAR('A')+KMC-1)

      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      KHR=INT(IHR)
      KMIN=INT(IMIN)
      KSEC=INT(ISEC)
      

*     OUVERTURE DE FICHIERS DESTINES A LA SUITE DES OPERATION
*     (UNE FOIS IMPORTGS.EXE ACHEVE)

      OPEN(101,FILE='P:\DATA\IMPORTGS.LOG',ACCESS='APPEND',ERR=11111)

      WRITE(101,'(47HEXECUTABLE CLICOM  : IMPORTGS.EXE * * * * * * *,/,
     *33HAN MOIS JOUR HEURE MINUTE SECONDE,/,
     *I2.2,1X,I2.2,3X,I2.2,3X,I2.2,4X,I2.2,5X,I2.2)')
     *KAC,KMC,KJC,KHR,KMIN,KSEC

      OPEN(100,FILE='P:\BATCH\MAUVAIGS.BAT',ERR=11111)

      WRITE(100,'(16HREM MAUVAIGS.BAT,/,17HREM * * * * * * *,/,
     *56HREM AN MOIS CODEMOIS JOUR HEURE MINUTE SECONDE ARGUMENT1,/,
     *17HCALL ERRORGS.BAT ,
     *I2.2,1X,I2.2,1X,A1,1X,
     *I2.2,1X,I2.2,1X,I2.2,1X,I2.2,3H %1)')
     *KAC,KMC,CODEM,
     *KJC,KHR,KMIN,KSEC

      CLOSE(100)

      OPEN(200,FILE='P:\BATCH\BONGS.BAT',ERR=11115)
      WRITE(200,'(13HREM BONGS.BAT,/,
     *11HREM * * * *,/,
     *56HREM %1 : NOMBRE DE FICHIER(S) TRAITE(S) PAR IMPORTGS.EXE,/,
     *39HREM %2 : ARGUMENT 1 DONNE A MAUVAIS.BAT,/,
     *39HREM %3 : ARGUMENT 2 DONNE A MAUVAIS.BAT)')

*     RECHERCHE DU PARAMETRE EVENTUEL FOURNI LORS DE L'APPEL DU PROGRAMME

      NUMARG=NARGS()
      NUMARG=NUMARG-1
      CALL GETARG(0,CHEMIN,STATUS)
*      PRINT*,'--->',CHEMIN
      IF(NUMARG.EQ.0)THEN
*        PRINT*,'ON PEUT DONNER EN ARGUMENT LE NOM DU FICHIER'
*        PRINT*,'OU SONT STOCKEES EN SEQUENCE DE n LIGNES'
*        PRINT*,'LES NOMS DE n FICHIERS A IMPORTER'
*        PRINT*,'SINON P:\DATA\IMPORTGS.TXT DOIT CONTENIR CES NOMS'
         CHEMIN='P:\DATA\IMPORTGS.TXT'
      ELSE
        CALL GETARG(1,CHEMIN,STATUS)
      ENDIF

      OPEN(7,FILE=CHEMIN,ERR=1199)

      CHEMIN=' '
      FINI='P:\DATA\IMPORTGS.INI'
      ZONA=' '
      OPEN(11,FILE=FINI,ERR=1200)

*     LECTURE DU FICHIER IMPORTGS.INI
*     POUR RECUPERER LE NOM DU FICHIER QUI CONTIENT
*     LES INFORMATIONS (PAR DEFAUT P:\DATA\IMPORTGS.INF) 

      READ(11,'(A80)',ERR=1701)ZONA
1701  CONTINUE
      CLOSE(11)
      IF(ZONA.EQ.' ')THEN
        FINI='P:\DATA\IMPORTGS.INF'
      ELSE
        FINI=ZONA(1:30)
      ENDIF

*     TEST DU PREMIER CARACTERE
*     SI C'EST UNE MINUSCULE ON N'A PAS DE SORTIES ECRAN
*     
      IF(ICHAR(ZONA(1:1)).GE.ICHAR('a'))THEN
*       PAS DE SORTIE ECRAN
        SORECR=.FALSE.
        PRINT*,'PAS DE SORTIES ECRAN !'
        CALL PAUSE(2)
      ELSE
*       SORTIES ECRAN
        SORECR=.TRUE.
      ENDIF

      IF(SORECR)THEN
        PRINT*,'<--- ICI D‚BUT DU PROGRAMME IMPORTGS'
        PRINT*,'     IMPORTATION DE FICHIERS ASCII'
        PRINT*,'     DANS DES GRILLES DE SAISIE CLICOM'
        PRINT*,'     METEO-FRANCE'
        PRINT*,'     DIRIC/CLIM'
        PRINT*,'     VERSION 1 : LUNDI 22 NOVEMBRE 1999'     
        PRINT*
        PRINT*,'DATE  : ',KAC,KMC,' (',CODEM,')',KJC
        PRINT*,'HEURE : ',KHR,KMIN,KSEC
        PRINT*
        PRINT*,'LA LISTE DES FICHIERS A IMPORTER EST DANS : ',CHEMIN
        PRINT*,
     *'LE TRAVAIL … EFFECTUER EST COD‚ DANS LE FICHIER CI-DESSOUS'
        PRINT*,'--->',FINI
      ENDIF

      OPEN(11,FILE=FINI,ERR=1200)

      NBL=0
      NBS=0

101   CONTINUE

      READ(11,'(A80)',ERR=199)ZONA
      NBL=NBL+1
*      PRINT*,NBL,'>',ZONA(1:50)
*      CALL PAUSE(1)
      IF(ZONA(1:3).EQ.'rem'.OR.ZONA(1:3).EQ.'REM')THEN
*       AFFICHAGE DES COMMENTAIRES A L'ECRAN
*        PRINT*,ZONA(4:80)
        IF(ZONA(1:1).EQ.'r')THEN
*         DEFILEMENT LENT
*          CALL PAUSE(5)
        ELSE
*         DEFILEMENT RAPIDE
*          CALL PAUSE(1)
        ENDIF
        GOTO 101
      ENDIF

      KE=17      

      IF(ZONA(KE:KE).EQ.'='.AND.ZONA(KE+2:KE+2).EQ.','.AND.
     *ZONA(KE+6:KE+6).EQ.','.AND.ZONA(KE+9:KE+9).EQ.','.AND.
     *ZONA(KE+13:KE+13).EQ.','.AND.ZONA(KE+16:KE+16).EQ.',')THEN

*       POSITIONNEMENT PAR RAPPORT AU SIGNE =

*       DESCRIPTION D'UNE STRUCTURE DE FICHIER AVEC SES PARAMETRES
        IF(ZONA(KE+16:KE+18).EQ.',,,')THEN

*exemple :
*inconnu2________=5,099,20,191,00,,,

*         LE PARAMETRE DECRIT EST INCONNU DU CATRA
          GOTO 101
        ELSE

*exemple :
*quotidien13_____=3,003,13,075,00,A8,4,5

          IF(ZONA(KE+2:KE+2).EQ.','.AND.ZONA(KE+6:KE+6).EQ.
     *','.AND.ZONA(KE+9:KE+9).EQ.','.AND.ZONA(KE+13:KE+13).EQ.
     *','.AND.ZONA(KE+16:KE+16).EQ.',')THEN
*           LE FORMAT A L'AIR CORRECTEMENT RESPECTE
            GOTO 1101
          ELSE
*           MAUVAIS FORMATAGE IL S'AGIT DESORMAIS DE RESPECTER LA SYNTAXE !
            PRINT*,'LIGNE ',NBL
            PRINT*,ZONA(1:60)
            GOTO 1201
          ENDIF
        ENDIF
      ENDIF

      IF(ZONA(1:12).EQ.'code_station')THEN
*       IDENTIFICATION D'UNE STATION 
        GOTO 2101
      ENDIF

      IF(ZONA(1:11).EQ.'separateur=')THEN
*       LE SEPARATEUR NE DOIT PAS ETRE UN ESPACE BLANC
        IF(ZONA(12:12).EQ.' ')GOTO 1204
*       LE SEPARATEUR NE PEUT QUAND MEME PAS ETRE UN POINT NI UN CHIFFRE
        IF(ZONA(12:12).EQ.'.')GOTO 1204
        I0=ICHAR(ZONA(12:12))
        IF(I0.GE.ICHAR('0').AND.I0.LE.ICHAR('9'))GOTO 1204
        GOTO 101
      ENDIF

      IF(ZONA(1:13).EQ.'code_invalide')THEN
*       ACQUISITION DU CODE UTILISE EN CAS DE MANQUE D'UNE DONNEE
        GOTO 3001
      ENDIF

      IF(ZONA(1:17).EQ.'code_pas_de_temps')THEN
*       ACQUISITION DE LA LETTRE CODE CORRESPONDANT A UN PAS DE TEMPS
*       (CETTE LETTRE SERT DANS LE NOM DES FICHIERS QUI CONTIENNENT 
*       LES DONNEES A IMPORTER)
        GOTO 4101
      ENDIF

      IF(ZONA(1:15).EQ.'lecteur_CLICOM=')THEN
*       LECTEUR OU SONT PLACEES LES DONNEES
        GOTO 5101
      ENDIF

      IF(ZONA(1:24).EQ.'chemin_transfert_CLICOM=')THEN
*       CHEMIN POUR ARRIVER AUX DONNEES
        GOTO 6101
      ENDIF

      GOTO 101

1101  CONTINUE

*     MISE EN MEMOIRE DANS STRUC(I1,I5,I3,1)
*                     DU PAS DE TEMPS (I1)
*                     DU NUMERO DE LA GRILLE DE SAISIE CORRESPONDANTE (I2)
*                     DU NUMERO DE LA COLONNE CORRESPONDANT (I3)
*                     AU NUMERO DU PARAMETRE CLICOM (I4)
*                     STRUC(I1,I5,I3,2) CONTIENT LE FACTEUR D'ECHELLE
*                                       (PUISSANCE DE 10)
*                     QUI PERMET L'IMPORTATION DE NOMBRES AVEC DES CHIFFRES
*                     APRES LA VIRGULE
*                     CAR IL N'Y A PAS DE POINT DECIMAL DANS LES GRILLES

*                     EXEMPLE : TEMPERATURE 10.5 øC ---> 105 
*                     OU ENCORE CA PERMET DE SIMPLIFIER PAR 10,100 ETC..  
*                     EXEMPLE : VISIBLITE 10000 M  ---> VISI EN DECAMETRES 100
*                     CA PERMET AUSSI DE PROCEDER A DE PETITS CALCULS.
*                     AINSI PAR CONVENTION PURE :
*                     15 TRANSFORME LES DIXIEMES DE DEGRES FARHENEIT 
*                        EN DIXIEMES DE DEGRES CELSIUS
*                     51 TRANSFORME LES DIXIEMES DE DEGRES C 
*                        EN DIXIEMES DEGRES F
*                     13 TRANSFORME LES DIXIEMES DE MILLIBARS (HECTOPASCALS)
*                        EN DIXIEMES DE MILLIMETRES DE MERCURE
*                     31 TRANSFORME LES DIXIEMES DE MM DE MERCURE
*                        EN HECTOPASCALS
*                     55 TRANSFORME LES DIXIEMES D'HECTOPASCALS (CODAGE SYNOP)
*                        EN HECTOPASCALS ET DIXIEMES (VRAIS) 
*                        EN AJOUTANT 1000. HPA
*                        SI LE CODE PRESSION EST INFERIEUR A 1000
*
*                        ETC.. (ON POURRA CREER D'AUTRES TRANSFORMATIONS
*                               PLUS OU MOINS COMPLEXES NOTAMENT LE CALCUL
*                               DE LA FRACTION D'INSOLATION A PARTIR DE 
*                               L'INSOLATION DE LA LATITUDE DE LA STATION 
*                               ET DE LA DATE)
*                      
*
*encore un exemple :
*
*horaire1________=5,001,01,101,00,A0,3,5
*inconnu2________=5,099,20,191,00,,,

      READ(ZONA(KE+1:KE+15),'(I1,1X,I3,1X,I2,1X,I3,1X,I2)',ERR=1201)
     *I1,I2,I3,I4,IFE
*      WRITE(*,*)I1,I2,I3,I4,IFE
*      CALL PAUSE(2)

*     TEST CAR I1 I2 I3 ET I4 SONT FORCEMENT TOUS NON NULS

      IF(I1.EQ.0)GOTO 1201
      IF(I2.EQ.0)GOTO 1201
      IF(I3.EQ.0)GOTO 1201
      IF(I4.EQ.0)GOTO 1201

*     ET COMPRIS ENTRE 1 ET CERTAINES LIMITES

      IF(I1.GT.IPT)GOTO 1201
      IF(I2.LT.0)GOTO 1201
      IF(I3.LT.0.OR.I3.GT.INC)GOTO 1201

*     LE NUMERO CLICOM EST UN ENTIER POSITIF ENTRE 001 ET 999
      IF(I4.LT.1.OR.I4.GT.999)GOTO 1201

      DO 1102 I5=1,NUMG(I1,0)
        IF(I2.EQ.NUMG(I1,I5))THEN
          GOTO 1103
        ENDIF
1102  CONTINUE

      IF(I5.GT.ING)GOTO 1201
      NUMG(I1,0)=I5
      NUMG(I1,I5)=I2
1103  CONTINUE

      STRUC(I1,I5,I3,1)=I4
      STRUC(I1,I5,I3,2)=IFE
*      PRINT*,STRUC(I1,I2,I3,1),STRUC(I1,I2,I3,2)
      GOTO 101

2101  CONTINUE
*     DETERMINATION DU COUPLE DES INDENTIFANTS STATION (CATRA,CLICOM)
*     ET DETERMINATION DU RANG DE LA STATION UNE FOIS CELLE-CI CONTROLEE

      DO 2102 I1=13,80
        IF(ZONA(I1:I1).EQ.'=')THEN
          I2=I1
        ENDIF
2102  CONTINUE

      DO 2103 I1=80,13,-1
        IF(ZONA(I1:I1).NE.' ')THEN
          I3=I1
          GOTO 2104
        ENDIF
2103  CONTINUE
      
2104  CONTINUE

      IF(ZONA(I3:I3).EQ.',')GOTO 101
      IF(ZONA(I3-9:I3-8).EQ.',,')GOTO 101
      NBS=NBS+1

*     NBX SATIONS MAXI (APRES ELLES SONT IGNOREES)

      IF(NBS.GT.NSX)THEN
        NBS=NBS-1
        GOTO 101
      ENDIF
      IND(NBS,1)=ZONA(I3-16:I3-9)
      IND(NBS,2)=ZONA(I3-7:I3)
      OLDSTN(NBS)=IND(NBS,1)
      NEWSTN(NBS)=IND(NBS,2)

*     LES DOUBLONS SONT IGNORES
*     SEULE LA PREMIERE DEFINITION FAIT FOI

      DO 2105 I1=1,NBS-1
        IF(IND(I1,1).EQ.IND(NBS,1))THEN
          NBS=NBS-1
          GOTO 101
        ENDIF
        IF(IND(I1,2).EQ.IND(NBS,2))THEN
          NBS=NBS-1
          GOTO 101
        ENDIF
2105  CONTINUE

      GOTO 101

3001  CONTINUE

*     AFFECTATION DE LA VARIABLE XMQ 
*     (VALEUR CONVENTIONNELLE SIGNANT UN MANQUE)

      DO 3101 I1=30,15,-1
        IF(ZONA(I1:I1).NE.' ')THEN
          I2=I1
          GOTO 3102
        ENDIF
3101  CONTINUE

*     FORCAGE EN CAS DE MANQUE DU MANQUE
      ZONA(15:19)='-999.'
      I2=19

3102  CONTINUE

      DO 3103 I4=15,I2
*       ON CHERCHE LA POSITION DU POINT DECIMAL EVENTUEL
        IF(ZONA(I4:I4).EQ.'.')THEN
          I3=I4
          GOTO 3104
        ENDIF
3103  CONTINUE

*     FORCAGE DU POINT (XMQ EST UN NOMBRE REEL)
      I2=I2+1
      I3=I2
      ZONA(I3:I3)='.'

3104  CONTINUE
      XFM='(Fxx.xx)'
      WRITE(XFM,'(2H(F,I2.2,1H.,I2.2,1H))')I2-14,I2-I3
      READ(ZONA(15:I2),FMT=XFM)XMQ
      IF(XMQ.GT.-999.)THEN
        XMQ=-999.
      ENDIF

      GOTO 101

4101  CONTINUE

*     255 PAS DE TEMPS SONT POSSIBLES EN THEORIE
*     C'EST LARGEMENT TROP 
*     MAIS DECODER AVEC LA FONCTION ICHAR()EST PLUS ELEGANT 
*     QUE DE LIRE : READ(ZONA(18:18),'(I1)',ERR=?)I1
*
*     CODE
*          1=MENSUEL 
*          2=DECADAIRE 
*          3=QUOTIDIEN
*          4=SYNOPTIQUE
*          5=HORAIRE
*          6=15 MINUTES 
*          7=DONNEES EN ALTITUDE (12 HEURES ?)
*     
*  AINSI LA SEQUENCE THEORIQUE EST :     
*        1 2 3 4 5 6 7 8 9 : ; <=> ? @ A B C etc..

      I1=ICHAR(ZONA(18:18))-ICHAR('0')
      IF(I1.LE.0.OR.I1.GT.IPT)THEN 
        GOTO 1205
      ENDIF
      CPT(I1)=ZONA(20:20)
      GOTO 101

5101  CONTINUE
*     LECTEUR OU SONT PLACEES LES DONNEES
      LEC=ZONA(16:16)
      IF(ICHAR(LEC).GE.ICHAR('A').AND.ICHAR(LEC).LT.ICHAR('Z'))
     *GOTO 5102
      IF(ICHAR(LEC).GE.ICHAR('a').AND.ICHAR(LEC).LT.ICHAR('z'))
     *GOTO 5102

*     IL Y A UNE ERREUR CAR LE LECTEUR EST FORCEMMENT 
*     UNE DES 26 LETTRES DE L'ALPHABET ROMAIN     
      PRINT*,'ATTENTION LECTEUR ',LEC,' EST UNE VALEUR INCORRECTE'
      LEC='c'
*      GOTO 1206

5102  CONTINUE

      GOTO 101

6101  CONTINUE
      GOTO 101

*     CHEMIN POUR ARRIVER AUX DONNEES
      CHEMIN=ZONA(18:47)
      IF(CHEMIN(1:1).NE.'\')GOTO 6103
      DO 6102 I1=30,2,-1
        IF(CHEMIN(I1:I1).NE.' ')THEN
          ICHEMI=I1
          EXIT
        ENDIF
6102  CONTINUE
      IF(CHEMIN(ICHEMI:ICHEMI).NE.'\')THEN
        PRINT*,CHEMIN(1:ICHEMI)
        PRINT*,'ATTENTION LE CHEMIN DES DONNEES SEMBLE INCORECT !'
        GOTO 6103
      ENDIF
      GOTO 101
6103  CONTINUE
      CHEMIN='\CLICOM\DATA\'
      ICHEMI=13

*     JE RETOURNE LIRE
      GOTO 101

*     JE RETOURNE ENCORE LIRE 
      GOTO 101

199   CONTINUE
      CLOSE(11)

      IF(ICHEMI.EQ.0)THEN
*       EN DESESPOIR DE CAUSE
*       FORCAGE DU CHEMIN DES DONNEES  (c:\CLICOM\DATA\)
        LEC='c'
        CHEMIN='\CLICOM\DATA\'
        ICHEMI=13
      ENDIF
      IF(SORECR)THEN
        PRINT*,NBL,' LIGNES LUES'
        PRINT*,' JE SUIS ARRIV‚ SANS ENCOMBRES … LA FIN DE : ',FINI
        PRINT*,' JE R‚CAPITULE : '
        PRINT*,'VALEUR EN CAS DE MANQUE : ',XMQ
      ENDIF

      DO 201 I1=1,IPT
        DO 202 I2=1,ING
          DO 203 I3=INC,1,-1
            IF(STRUC(I1,I2,I3,1).NE.0)THEN
              DO 204 I4=1,I3
                IF(STRUC(I1,I2,I3,1).EQ.0)THEN
*                 UNE COLONNE EST RESTEE INDEFINIE
*                 R‚DIBITOIRE !
                  GOTO 1202
                ENDIF 
204           CONTINUE
              GOTO 205
            ENDIF
203       CONTINUE
205       CONTINUE

          IF(I3.GE.1)THEN
            IF(SORECR)THEN
              PRINT*,'PAS TEMPOREL',I1,' GRILLE',I2,' NB PARAM :',I3
              WRITE(NOMFIC,'(A1,I3.3,5Hxxxx.)')CPT(I1),NUMG(I1,I2)
              PRINT*,'FICHIER TYPE : '
     *//LEC//':'//CHEMIN(1:ICHEMI)//NOMFIC 
            ENDIF
            INP(I1)=INP(I1)+I3
            INP(0)=INP(0)+I3
          ENDIF
202     CONTINUE            
201   CONTINUE

      IF(SORECR)THEN
        DO 206 I1=1,IPT
          IF(INP(I1).GE.1)THEN
            PRINT*,INP(I1),' PARAMŠTRES POUR LE PAS TEMPOREL',I1,' (',
     *CPT(I1),')'
          ENDIF
206     CONTINUE
        PRINT*,INP(0),' PARAMŠTRES AU TOTAL'
        IF(NBS.LE.1)THEN
          PRINT*,NBS,
     *' STATION A ‚T‚ D‚FINIE PAR UN COUPLE IDENTIFICATEUR'
        ENDIF
        IF(NBS.GT.1)THEN
          PRINT*,NBS,
     *' STATIONS ONT ‚T‚ D‚FINIES PAR UN COUPLE IDENTIFICATEUR'
        ENDIF
      ENDIF

      IF(NBS.EQ.0)GOTO 1203
     
      GOTO 77777

1199  CONTINUE

      WRITE(101,'(12H ERREUR 1199 ,
     *23H IMPOSSIBLE A OUVRIR : ,A50)')XFICH
      PRINT*,XFICH,' IMPOSSIBLE … OUVRIR !'
      PRINT*,'ERREUR IMPORTGS.EXE 1199'
      GOTO 11111

1200  CONTINUE
      WRITE(101,'(12H ERREUR 1200)')
      PRINT*,FINI,' IMPOSSIBLE … OUVRIR !'
      PRINT*,'ERREUR IMPORTGS.EXE 1200'
      GOTO 11111

1201  CONTINUE

      WRITE(101,'(12H ERREUR 1201)')
      WRITE(101,'(30H NUMERO DE LA LIGNE FAUTIVE : ,I12)')NBL
      WRITE(101,'(22H VALEURS RETOURNEES : ,4I9)')I1,I2,I3,I4

      PRINT*,'NUMERO DE LA LIGNE FAUTIVE : ',NBL
      PRINT*,'VALEURS RETOURNEES : ',I1,I2,I3,I4
      PRINT*,'ERREUR IMPORTGS.EXE 1201'

      GOTO 11111

1202  CONTINUE

      WRITE(101,'(12H ERREUR 1202)')
      WRITE(101,'(33H UNE COLONNE EST RESTEE INDEFINIE)')
      WRITE(101,'(22H VALEURS RETOURNEES : ,3I9)')I1,I2,I3

      PRINT*,'VALEURS RETOURNEES : ',I1,I2,I3
      PRINT*,'UNE COLONNE EST REST‚e IND‚FINIE'
      PRINT*,'ERREUR IMPORTGS.EXE 1202'
      GOTO 11111
      
1203  CONTINUE

      WRITE(101,'(12H ERREUR 1203)')

      PRINT*,'ERREUR IMPORTGS.EXE 1203'
      GOTO 11111

1204  CONTINUE
      WRITE(101,'(12H ERREUR 1204)')
      WRITE(101,'(34H S‚PARATEUR DE CHAMPS INCORRECT : ,A1)')
     *ZONA(12:12)

      PRINT*,ZONA(12:12),' S‚PARATEUR DE CHAMPS INCORRECT'
      PRINT*,'SVP RESTEZ SIMPLE ET UTILISEZ LA VIRGULE'
      PRINT*,'ERREUR IMPORTGS.EXE 1204'
      GOTO 11111
      
1205  CONTINUE
      WRITE(101,'(21H ERREUR 1205 LIGNE : ,I12,I12)')NBL,I1

      PRINT*,'NUMERO DE LA LIGNE FAUTIVE : ',NBL
      PRINT*,'VALEUR RETOURNEE : ',I1
      PRINT*,'ERREUR IMPORTGS.EXE 1205'
      GOTO 11111

1206  CONTINUE
      WRITE(101,'(21H ERREUR 1206 LIGNE : ,I12,1X,A1,2H !)')NBL,LEC

      PRINT*,'NUMERO DE LA LIGNE FAUTIVE : ',NBL
      PRINT*,'LECTEUR FAUTIF : ',LEC
      PRINT*,'ERREUR IMPORTGS.EXE 1206'
      GOTO 11111
    
1207  CONTINUE
      WRITE(101,'(12H ERREUR 1207,1X,A50)')XFICH
      PRINT*,'   --->',XFICH
      PRINT*,'LE FICHIER DES DONNEES EST INCORECT' 
      PRINT*,'ERREUR IMPORTGS.EXE 1207'
      GOTO 11111

1208  CONTINUE
      WRITE(101,'(12H ERREUR 1208,1X,A50)')XFICH
      PRINT*,'   --->',XFICH
      PRINT*,'LE FICHIER DES DONNEES EST BIZARRE OU VIDE' 
      PRINT*,'ERREUR IMPORTGS.EXE 1208'
      GOTO 11111

1209  CONTINUE
      WRITE(101,'(12H ERREUR 1209,1X,A50)')XFICH
      PRINT*,'   --->',XFICH
      PRINT*,'LE NOM DU FICHIER DES DONNEES NE ME RENSEIGNE PAS' 
      PRINT*,'SUR LE TYPE DE DONNEES A IMPORTER'
      PRINT*,'ERREUR IMPORTGS.EXE 1209'
      GOTO 11111

1210  CONTINUE
      WRITE(101,'(12H ERREUR 1210,1X,A50)')XFICH
      PRINT*,'   --->',XFICH
      PRINT*,'LE NOM DU FICHIER DES DONNEES NE ME RENSEIGNE PAS' 
      PRINT*,'SUR LE NUMERO DE LA GRILLE DE SAISIE A UTILISER'
      PRINT*,'ERREUR IMPORTGS.EXE 1210'
      GOTO 11111

1211  CONTINUE
      WRITE(101,'(12H ERREUR 1211,1X,A50)')XFICH
      PRINT*,'   --->',XFICH
      PRINT*,'LE FICHIER QUI CONTIENT LE NOM DES FICHIERS DE DONNEES' 
      PRINT*,'EST VIDE OU INACCESSIBLE'
      PRINT*,'ERREUR IMPORTGS.EXE 1211'
      GOTO 11111

1212  CONTINUE
      WRITE(101,'(12H ERREUR 1212,1X,A3,1X,I3.3)')ITYPE,DSETID

      PRINT*,'   --->',ITYPE,DSETID,' NON DECRIT !'
      PRINT*,'POUR LE PAS TEMPOREL               : ',ITYPE
      PRINT*,'IL MANQUE LA DESCRIPTION LA GRILLE : ',DSETID
      PRINT*,'ERREUR IMPORTGS.EXE 1212'
      GOTO 11111

1213  CONTINUE
      PRINT*,'   --->'
      PRINT*,'ERREUR IMPORTGS.EXE 1213'
      GOTO 11111

1214  CONTINUE
      PRINT*,'   --->'
      PRINT*,'ERREUR IMPORTGS.EXE 1214'
      GOTO 11111

1215  CONTINUE
      PRINT*,'   --->',ZORO(1:80)
*      READ(*,'(A1)')REP
      PRINT*,'ERREUR IMPORTGS.EXE 1215'
      GOTO 11111

77777 CONTINUE
*     PETITE PAUSE AVANT LE TRAITEMENT
*     1 SECONDE
*      CALL PAUSE(10)
*      READ(*,'(A1)')REP
*     ALORS ON Y VA

*     INSERTION DU CORPS DU PROGRAMME IMPORT23 (LEGEREMENT ADAPTE)

************************************************************************
*     RETRIEVE THE DATA TYPE TO BE IMPORTED
************************************************************************
*     LES CODES EN USAGE DANS CLICOM :
*
*     0 : RIEN (SORTIE IMMEDIATE)
*     1 : MENSUEL
*     2 : DECADAIRE
*     3 : QUOTIDIEN
*     4 : SYNOPTIQUE
*     5 : HORAIRE
*     6 : QUINZE MINUTES
*     7 : SONDAGE ALTITUDE (12 HEURES ?)
*     8 : RIEN (PROSCRIT)
*     9 : RIEN (PROSCRIT)

      LOYR=0
      LOMO=1
      HIYR=9999
      HIMO=12
      DSETID=001 
      OKWRITE='N'
*     EDITEUR DE TEXTE EVENTUEL
      AFIELD(7)='EDIT'
      AFIELD(8)='N'

*     CODE POUR LES TRACES DE PRECIPITATIONS (TRACE)
*     EST-CE BIEN UTILE ?

      AFIELD(9)='9999.9'
      READ(AFIELD(9)(1:6),'(F6.1)')TRACE

      NBF=0
*     IL Y A PEUT-ETRE PLUSIEURS FICHIERS A IMPORTER
17777 CONTINUE

      IF(NBF.NE.0)THEN
        RECCOUNT=0
        NRECCOUNT=0
        DO 37777 I1=1,INC
          OKELEM(I1)=.FALSE.
37777   CONTINUE
        CLOSE(19)
        CLOSE(20)
        CLOSE(75)
      ENDIF
      
9977  CONTINUE

      READ(7,'(A50)',END=27777,ERR=1210)XFICH

      IF(XFICH(1:1).EQ.'*')THEN
*       * EST UN CARACTERE QUI DIT QUE LE NOM DU FICHIER A TRAITE
*         EST PLACE SUR LA LIGNE D'APRES
*         IL FAUT DONC SIMPLEMENT CONTINUER A LIRE L'UNITE 7
        GOTO 9977
      ENDIF

      IF(XFICH.EQ.' ')THEN
*       UNE LIGNE BLANCHE NE FAIT AUCUN MAL ELLE EST DONC PERMISE
*       ICI IL FAUT SIMPLEMENT CONTINUER A LIRE L'UNITE 7
        GOTO 9977
      ENDIF
      
*      CALL IQ(NIU)

*      PRINT*,'ON VA OUVRIR SUR LE PORT 33'
      OPEN(33,FILE=XFICH,ERR=1207)

*      CALL IQ(NIU)

9988  CONTINUE
      IF(SORECR)THEN
        PRINT*,'PRELECTURE : LIGNE SUIVANTE'
      ENDIF
      READ(33,'(A8,A288)',ERR=4001,IOSTAT=IERR)
     *INSTN,ZORO
     
      IF(INSTN.EQ.' ')THEN
*       UNE LIGNE BLANCHE N'A PAS A ARRETER LE PROGRAMME
*       IL SUFFIT DE LA SAUTER
        GOTO 9988
      ENDIF

*     CONTROLE VISUEL DE LA PREMIERE LIGNE DU FICHIER
      IF(SORECR)THEN
        PRINT*,'FICHIER : ',XFICH
        PRINT*,INSTN,ZORO(1:70)
      ENDIF

*     DECODAGE DE LA LIGNE COURANTE ZORO ---> DATE+INVAL

      CALL DIEGO(ZORO,YEAR,MONTH,DAY,HOUR,INVAL)
      IF(SORECR)THEN
        WRITE(*,'(20H*DEBUT DU FICHIER : ,A8,1X,I4,1X,I2,1X,I2,1X,I2)')
     *INSTN,YEAR,MONTH,DAY,HOUR
      ENDIF
    
      WRITE(101,'(1X,A50,/,9H DEBUT : ,A8,1X,I4,1X,I2,1X,I2,1X,I2)')
     *XFICH,INSTN,YEAR,MONTH,DAY,HOUR

      IF(YEAR.EQ.0)THEN
        PRINT*,INSTN,' ! ',ZORO(1:70)
        PRINT*,'POSE UN PROBLEME'

*       AU CHOIX 
*       IL Y A DEUX SOLUTIONS :

*       SOLUTION 1
        PRINT*,'MAIS ON CONTINUE A LA LIGNE SUIVANTE'
        GOTO 9988

*       SOLUTION 2
*        PRINT*,'AUSSI ON ARRETE TOUT'
*        GOTO 1208
      ENDIF

      CLOSE(33)

      NBF=NBF+1
      IF(SORECR)THEN
        PRINT*,'NOM DU FICHIER QUI VA ETRE TRAITE'
        PRINT*,'IL CONTENT LES DONNEES METEO A IMPORTER'
        PRINT*,'--->',XFICH

        IF(HOUR.EQ.99)THEN
          PRINT*,'PAS DE TEMPS QUOTIDIEN'
        ENDIF

        IF(HOUR.GE.0.AND.HOUR.LE.23)THEN
          PRINT*,'PAS DE TEMPS HORAIRE OU SYNOPTIQUE'
        ENDIF
      ENDIF

*     LE PREMIER CARACTERE DU NOM DU FICHIER CONTIENT SOUS FORME CODEE
*     LE NOM DE LA GRILLE DE SAISIE A CREER OU A METTRE A JOUR
      ITYPE=0
      
      DO 12 I1=50,1,-1
        IF(XFICH(I1:I1).EQ.'\')THEN
          EXIT
        ENDIF
12    CONTINUE

      DO 10 I2=1,IPT
        IF(XFICH(I1+1:I1+1).EQ.CPT(I2))THEN
          ITYPE=I2
          EXIT
        ENDIF
10    CONTINUE

      IF(ITYPE.EQ.0.OR.ITYPE.GT.IPT)THEN
        PRINT*,ITYPE
        GOTO 1209
      ELSE
        RECTYPE=TBLRECTYP(ITYPE)
        NUMLINE=TBLNUMLIN(ITYPE)
      ENDIF

*     LES TROIS CARACTERES QUI SUIVENT LA LETTRE CODE
*     DONNENT LE NUMERO DE LA GRILLE DE SAISIE CLICOM
 
      READ(XFICH(I1+2:I1+4),'(I3)',ERR=1210)DSETID
      IF(DSETID.LE.0)THEN
        WRITE(*  ,'(19H GRILLE DEMANDEE : ,A3,I3.3,3H ! )')
     *RECTYPE,DSETID
        WRITE(101,'(19H GRILLE DEMANDEE : ,A3,I3.3,3H ! )')
     *RECTYPE,DSETID
        GOTO 1210
      ELSE
        WRITE(*,'(19H GRILLE DEMANDEE : ,A3,1X,I3.3)')RECTYPE,DSETID
*      READ(*,'(A1)')REP
      ENDIF
      
*     RECHERCHE DE LA STRUCTURE CODEE QUELQUE PART DANS STRUC(ITYPE, , ,1)

      DO 501 I2=NUMG(ITYPE,0),1,-1
        IF(NUMG(ITYPE,I2).EQ.DSETID)THEN
          EXIT
        ENDIF
501   CONTINUE 

      IF(I2.GT.0)THEN
        DO 502 I3=1,INC
          IF(STRUC(ITYPE,I2,I3,1).EQ.0)THEN
            EXIT
          ELSE
            INELEM(I3)=STRUC(ITYPE,I2,I3,1)
            FE(I3)=STRUC(ITYPE,I2,I3,2)
            IF(SORECR)THEN
              IF(STRUC(ITYPE,I2,I3,2).EQ.0)THEN
                  PRINT*,'‚L‚MENT CLICOM : ',I3,STRUC(ITYPE,I2,I3,1)
              ELSE
                IF(STRUC(ITYPE,I2,I3,2).GE.-4
     *.AND.STRUC(ITYPE,I2,I3,2).LE.4)THEN
                  PRINT*,'‚L‚MENT CLICOM : ',I3,STRUC(ITYPE,I2,I3,1)
     *,10.**STRUC(ITYPE,I2,I3,2)
                ELSE
                  PRINT*,'‚L‚MENT CLICOM : ',I3,STRUC(ITYPE,I2,I3,1)
     *,' ! --->',STRUC(ITYPE,I2,I3,2)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
502     CONTINUE
      ELSE

*       ERREUR LA DESCRITION DU FICHIER CORRESPONDANT A LA GRILLE
*       N'EST PAS DANS P:\DATA\IMPORTGS.INI
        GOTO 1212
      ENDIF

      INELEM(I3)=0
      NBE=I3-1

      JSTN=NBS

      DO 570 I1=1,JSTN
*        PRINT*,'                 STATION               : ',
*     *I1,' ',oldstn(i1),'  ',newstn(i1)
570   CONTINUE 
*      READ(*,'(A1)')REP

************************************************************************
*     OPEN THE SETUP FILE (THE DEFINITION OF KEY-ENTRY FORMS)         *
*     AND LOAD THE ELEMENT CODES AND COLUMN HEADERS.  THEN OPEN THE    *
*     KEY-ENTRY/QC FILES.                                              *
************************************************************************

      CALL GETSET(DSETID,ITYPE,RECTYPE,HOURLBL,RTNCODE)
      ERRSRC='GETSET'
      IF(RTNCODE.NE.'0')THEN
        GOTO 9035
      ENDIF
      LASTID=' '
      CALL OPENFILES(2)

************************************************************************
*     ** FOR HOURLY AND SYNOPTIC DATA CONVERT HOUR LABELS FROM
*        CHARACTER TO INTEGER FORMAT
*     NOTE:  IT IS EXPECTED THAT THE HOUR VALUES FOR THE INPUT DATA
*            AGREE WITH THE HOUR VALUES SPECIFIED IN THE DATAQC.PRM
*            FILE.  IF THIS IS NOT TRUE, THEN CODE TO MAKE THE CONVERSION
*            MUST BE ADDED TO THIS PROGRAM
************************************************************************

      IF(ITYPE.EQ.4.OR.ITYPE.EQ.5)THEN
        DO 60 I=1,NUMLINE
          READ(HOURLBL(I),'(I2)')IHRLBL(I)
60      CONTINUE
      ENDIF

      DO 120 J=1,INC
        IF(INELEM(J).EQ.0)THEN
*         ON A FINI DE DECRIRE LES POSITIONS DES ELEMENTS
          NBE=J-1
          GOTO 125
        ENDIF
        DO 110 I=1,INC
          IF(INELEM(J).EQ.TBLELEM(I).AND..NOT.OKELEM(I))THEN
            OKELEM(I)=.TRUE.
            GOTO 115
          ENDIF
110     CONTINUE
        GOTO 9030
115     CONTINUE
*       LE RANG DE LA COLONNE A ETE TROUVE !
        ELMCOL(J)=I
120   CONTINUE

125   CONTINUE

      IF(SORECR)THEN
        PRINT*,' FICHIER : ',XFICH
        PRINT*,' NOMBRE ELEMENT(S) : ',NBE
      ENDIF

      DO 900 J=1,NUMLINE
        DO 900 K=1,NUMELEM
        VALARRAY(K,J)=' '
        FLAGARRAY(K,J,1)=' '
        FLAGARRAY(K,J,2)=' '
900   CONTINUE

*     FLAG IS SET FOR AUTOMATIC QUALITY CONTROL FOR ALL DATA TYPES EXCEPT
*     UPPER-AIR.  FOR UPPER-AIR DATA YOU MUST PRESS F10 AFTER YOU
*     SELECT VALIDATION.  FOR OTHER DATA TYPES QUALITY CONTROL OCCURS
*     AUTOMATICALLY AFTER YOU SELECT VALIDATION.

      IF(ITYPE.NE.7)FLAGARRAY(1,1,1)='?'

      INLC=0

      OPEN(75,FILE=XFICH,ERR=9005,IOSTAT=IERR)

      CALL CLRMSG(1)
      CALL LOCATE(24,0,IERR)

      LINENUM=0
1000  CONTINUE

***********************************************************************
*           --- THIS IS THE MAIN LOOP IN THE PROGRAM                   *
************************************************************************


9989  CONTINUE

*     INITIALIZE ALL VALUES TO MISSING
      DO 1050 I2=1,NBE
        INVAL(I2)=XMQ
1050  CONTINUE

      INLC=INLC+1

      READ(75,'(A8,A288)',END=4000,ERR=9008,IOSTAT=IERR)
     *INSTN,ZORO

*      PRINT*,INLC

      IF(INSTN.EQ.' ')THEN
*       LIGNE BLANCHE A SAUTER
        GOTO 9989
      ENDIF

*     DECODAGE DE LA LIGNE COURANTE ZORO ---> DATE+INVAL

      CALL DIEGO(ZORO,YEAR,MONTH,DAY,HOUR,INVAL)
 
      IF(YEAR.EQ.0)THEN
*       ERREUR DANS LA LECTURE (FORMAT NON RESPECTE)

*       AU CHOIX 
*       IL Y A DEUX SOLUTIONS :

*       SOLUTION 1
        WRITE(101,'(29H FORMAT NON RESPECTE LIGNE : ,I12)')INLC

        DO 4441 IN=288,2,-1
          IF(ZORO(IN:IN).NE.' ')THEN
            EXIT
          ENDIF
4441    CONTINUE
        ZONA='(A8,AXXX)'
        WRITE(ZONA(6:8),'(I3.3)')IN
        WRITE(101,FMT=ZONA)INSTN,ZORO(1:IN)
        IF(SORECR)THEN
          WRITE(*,'(29H FORMAT NON RESPECTE LIGNE : ,I12)')INLC
          PRINT*,'MAIS ON CONTINUE A LA LIGNE SUIVANTE'
        ENDIF

        GOTO 9989

*       SOLUTION 2
*        PRINT*,'AUSSI ON ARRETE TOUT'
*        GOTO 1215

      ELSE
*        PRINT*,YEAR,MONTH,DAY,HOUR
*        PRINT*,(INVAL(I),I=1,NBE)
*        READ(*,'(A1)')E     
      ENDIF
      NRECCOUNT=NRECCOUNT+1
      CALL LOCATE(24,15,IERR)
      WRITE(OUTCNT,'(I6)')NRECCOUNT
      CALL CWRITE(OUTCNT,12,IERR)

************************************************************************
*     CHECK IF THE RECORD READ IS WITHIN THE USER SPECIFIED RANGE      *
************************************************************************

      IF(INSTN.LT.OLDSTN(1))GOTO 1000
      IF(YEAR.LT.LOYR)GOTO 1000

      IF(INSTN.GT.OLDSTN(JSTN))GOTO 4000
      IF(YEAR.GT.HIYR.AND.INSTN.EQ.OLDSTN(JSTN))GOTO 4000

      DO 1500 L=1,JSTN
        IF(INSTN.EQ.OLDSTN(L))GOTO 1600
1500  CONTINUE
      GOTO 1000

1600  IF(RECTYPE.NE.'MLY'.AND.RECTYPE.NE.'10D')THEN
        IF(YEAR.EQ.LOYR.AND.MONTH.LT.LOMO)THEN
          GOTO 1000
        ELSE
          IF(YEAR.EQ.HIYR.AND.MONTH.GT.HIMO)THEN
            GOTO 4000
          ENDIF
        ENDIF
      ENDIF

************************************************************************
*  FILL IN THE IDKEY * THE FIELD WHICH WILL IDENTIFY THIS RECORD IN THE
*  KEY-ENTRY/QC FILE.
************************************************************************

      WRITE(ADSID,'(I3.3)')DSETID

      WRITE(AYEAR,'(I4.4)')YEAR

      IF(RECTYPE.EQ.'MLY'.OR.RECTYPE.EQ.'10D')THEN
*       MONTHLY AND 10-DAY DATA
        AMONTH=' '
        ADAY=' '
        AHOUR=' '
      ENDIF

      IF(RECTYPE.EQ.'DLY')THEN
*       DAILY DATA
        WRITE(AMONTH,'(I2.2)')MONTH
        ADAY=' '
        AHOUR=' '
      ENDIF      

      IF(RECTYPE.EQ.'HLY'.OR.RECTYPE.EQ.'SYN'.OR.RECTYPE.EQ.'15M')THEN
*       HOURLY, SYNOPTIC AND 15-MINUTE DATA
        WRITE(AMONTH,'(I2.2)')MONTH
        WRITE(ADAY,'(I2.2)')DAY
        AHOUR=' '
      ENDIF

      IF(RECTYPE.EQ.'U-A')THEN
*       UPPER AIR DATA
        WRITE(AMONTH,'(I2.2)')MONTH
        WRITE(ADAY,'(I2.2)')DAY
        WRITE(AHOUR,'(I2.2)')HOUR
      ENDIF

      WRITE(IDKEY,'(A8,A3,A4,3A2,)')
     *NEWSTN(L),ADSID,AYEAR,AMONTH,ADAY,AHOUR

*************************************************************************
*   IF IDKEY OF THIS RECORD IS DIFFERENT FROM THE PREVIOUS RECORD THEN  *
*   IT GOES INTO A NEW KEY-ENTRY/QC FORM.  WRITE OUT THE DATA THAT WE   *
*   HAVE BEEN ACCUMULATING INTO THE LAST FORM AND START A NEW ONE.      *
************************************************************************

*     WRITE OUT THE DATA WE HAVE BEEN ACCUMULATING

      IF(IDKEY.NE.LASTID.AND.LASTID.NE.' ')THEN
        CALL LOCATE(24,56,IERR)
        WRITE(OUTSTN,'(A8)')LASTOLD
        CALL CWRITE(OUTSTN,12,IERR)
        CALL LOCATE(24,71,IERR)
        WRITE(OUTSTN,'(A8)')LASTID(1:8)
        CALL CWRITE(OUTSTN,12,IERR)
        CALL BINDATA(LASTID,RTNCODE)
        IF(OKWRITE.EQ.'Y'.OR.OKWRITE.EQ.'y')THEN
*         IF RTNCODE=2 THEN ID NOT FOUND SO IT'S NEW (OK).
*         IF RTNCODE=0 THEN THE PROGRAM HAS ENCOUNTERED A DUPLICATE ID
*         ASK THE USER TO CONTINUE OR QUIT.  ANY OTHER RTNCODE INDICATES
*         AN ERROR.
          IF(RTNCODE.NE.'2')THEN
            IF(RTNCODE.EQ.'0')THEN
              CALL WRTMSG(4,305,12,1,0,' ',0)
              CALL LOCATE(22,5,IERR)
              CALL OKREPLY(REPLY,RTNCODE)
              IF(REPLY.EQ.'N')THEN
                CALL CLRMSG(4)
                CALL CLRMSG(3)
                GOTO 1100
              ELSE
                CALL CLRMSG(4)
                CALL CLRMSG(3)
              ENDIF
            ELSE
              CALL WRTMSG(4,51,12,1,1,RTNCODE,1)
              CALL WRTMSG(2,365,14,1,1,' ',0)
              CALL LOCATE(23,0,IERR)
              GOTO 11111
            ENDIF
          ENDIF
        ENDIF
        CALL PUTDATA(LASTID,2,RTNCODE)
1100    CONTINUE

*       RE-INITIALIZE THE WORK ARRAYS TO HOLD THE NEW DATA. NOTE THAT
*       NUMLINE AND NUMELEM ARE LISTED IN COMMON VAL1 (VAL1.INC)

        DO 7201 J=1,NUMLINE
          DO 7202 K=1,NUMELEM
            VALARRAY(K,J)=' '
7202      CONTINUE
7201    CONTINUE
        LINENUM=0
      ENDIF

* ....NEW CODE STATION

      LASTID=IDKEY

* ....OLD CODE STATION

      LASTOLD=OLDSTN(L)

************************************************************************
*     LOAD DATA VALUES FROM THE CURRENT RECORD INTO VALARRAY.
************************************************************************

1300  CONTINUE
      IF(ITYPE.LE.2)THEN
*       MONTHLY AND 10-DAY DATA
        IROW=MONTH
      ENDIF
      IF(ITYPE.EQ.3)THEN
*       DAILY DATA 
        IROW=DAY
      ENDIF
      IF(ITYPE.EQ.4.OR.ITYPE.EQ.5)THEN
*       HOURLY AND SYNOPTIC 
        DO 1320 I=1,NUMLINE
          IF(HOUR.EQ.IHRLBL(I))GOTO 1321
1320    CONTINUE
        GOTO 9040
1321    CONTINUE
        IROW=I
      ENDIF
      IF(ITYPE.EQ.6)THEN
*       15-MINUTE DATA
        IROW=HOUR
      ENDIF
      IF(ITYPE.EQ.7)THEN
*       UPPER AIR DATA
        LINENUM=LINENUM+1
        IROW=LINENUM
      ENDIF

      DO 3000 J1=1,NBE
        IF(INELEM(J1).GT.0.AND.INELEM(J1).LE.999)THEN
          JCOL=ELMCOL(J1)
          TBLCONV(JCOL)=1.
          IF(INVAL(J1).NE.XMQ)THEN
*           LA DONNEE N'EST PAS MANQUANTE
            IF(FE(J1).NE.0)THEN
*             IL FAUT TRANSFORMER LA DONNEE BRUTE DU FICHIER

              IF(FE(J1).GE.-4.AND.FE(J1).LE.4)THEN            
*               MULTIPLICATION PAR UN FACTEUR D'ECHELLE EVENTUEL
*               C'EST UNE FORCEMMENT UNE PUISSANCE DE 10 (DE .001 A 1000)
                INVAL(J1)=INVAL(J1)*(10.**FE(J1))
                GOTO 3003
              ENDIF

              IF(FE(J1).EQ.15)THEN
*               TRANSFORMATION DE DIXIEMES DE FARENHEIT EN DIXIEMES DE DEGRES C
*               C=5/9(F-32)

                INVAL(J1)=XAF*(INVAL(J1)+XBF)
                GOTO 3003
              ENDIF

              IF(FE(J1).EQ.51)THEN
*               TRANSFORMATION DE DIXIEMES DE DEGRES C EN DIXIEMES DE FARENHEIT
*                F=(C*(9/5))+32
                INVAL(J1)=(INVAL(J1)/XAF)-XBF
                GOTO 3003
              ENDIF

              IF(FE(J1).EQ.13)THEN
*               LES DIXIEMES D'HECTOPASCALS (HPa) VONT ETRE TRANSFORMES 
*               EN DIXIEMES DE MILLIMETRES DE MERCURE (Hg)
                INVAL(J1)=INVAL(J1)/XAHG
                GOTO 3003
              ENDIF

              IF(FE(J1).EQ.31)THEN
*               LES DIXIEMES DE Hg VONT ETRE TRANSFORMES EN DIXIEMES DE HPa
                INVAL(J1)=INVAL(J1)*XAHG
                GOTO 3003
              ENDIF

              IF(FE(J1).EQ.55)THEN
*               LES DIXIEMES D'HECTOPASCALS (HPa) (CODE SYNOP) 
*               VONT ETRE TRANSFORMES 
*               EN DIXIEMES D'HECTOPASCALS (VRAIS) 
                IF(INVAL(J1).LT.5000.)THEN
                  INVAL(J1)=INVAL(J1)+10000.
                ENDIF
                GOTO 3003
              ENDIF

              PRINT*,'ATTENTION AU FACTEUR ECHELLE POUR LA COLONNE',J1
              PRINT*,'LE CODAGE ',FE(J1),
     *' NE CORRESPOND … RIEN DE PR‚VU!'
            ENDIF

3003        CONTINUE

            IF(INVAL(J1).GE.-9999..AND.INVAL(J1).LT.99999.)THEN
               CALL IROUND4((INVAL(J1)/TBLCONV(JCOL)),I4VALUE)
               WRITE(VALARRAY(JCOL,IROW),'(I5,1X)')I4VALUE
            ELSE
              VALARRAY(JCOL,IROW)=' '
            ENDIF
          ELSE
            VALARRAY(JCOL,IROW)=' '
          ENDIF
        ENDIF

*         THE NEXT LINES ARE COMMENTED BUT GIVE AN EXAMPLE OF HOW
*         YOU MIGHT CHECK FOR SPECIAL VALUES WHICH INDICATE TRACE
*         OF PRECIP
*
*         DLY OBSERVATION
*         IF((INELEM(J1).EQ.005.OR.INELEM(J1).EQ.006.OR.
*     +   INELEM(J1).EQ.007.OR.INELEM(J1).EQ.008.OR.
*     +   INELEM(J1).EQ.009.OR.INELEM(J1).EQ.010.OR.
*     +   INELEM(J1).EQ.011.OR.
*         HLY, SYN, AND 15 MINUTES OBSERVATION
*     +   INELEM(J1).EQ.104.OR.INELEM(J1).EQ.173.OR.
*     +   INELEM(J1).EQ.174.OR.
*         MLY OBSERVATION
*     +   INELEM(J1).EQ.208.OR.INELEM(J1).EQ.210.OR.
*         10 DAY OBSERVATION
*     +   INELEM(J1).EQ.408.OR.INELEM(J1).EQ.410)
*     +   .AND.INVAL(J1).EQ.TRACE)THEN
*           VALARRAY(JCOL,IROW)='    0T'
*         ENDIF

3000  CONTINUE

3050  CONTINUE
      RECCOUNT=RECCOUNT+1
      CALL LOCATE(24,42,IERR)
      WRITE(OUTCNT,'(I6)')RECCOUNT
      CALL CWRITE(OUTCNT,12,IERR)

*     NOW GO BACK AND READ THE NEXT RECORD
*     LA LECTURE DU FICHIER DES DONNEES CONTINUE

      GOTO 1000

4000  CONTINUE
      
*      PRINT*,'ON EST ARRIVE A LA FIN DU FICHIER DES DONNEES'
*      READ(*,'(A1)')REP
*      ON EST ARRIVE A LA FIN DU FICHIER DES DONNEES

************************************************************************
*     BEFORE STOPPING, WRITE OUT THE DATA THAT HAS BEEN ACCUMULATING FOR
*     THE LAST KEY-ENTRY/QC FORM.
************************************************************************
      CALL LOCATE(24,56,IERR)
      WRITE(OUTSTN,'(A8)')LASTOLD
      CALL CWRITE(OUTSTN,12,IERR)

      CALL LOCATE(24,71,IERR)
      WRITE(OUTSTN,'(A8)')LASTID(1:8)
      CALL CWRITE(OUTSTN,12,IERR)

      IF(RECCOUNT.GT.0)THEN
        CALL BINDATA(LASTID,RTNCODE)
        IF(OKWRITE.EQ.'Y'.OR.OKWRITE.EQ.'y')THEN
          IF(RTNCODE .NE.'2')THEN
            IF(RTNCODE .EQ. '0')THEN
              CALL WRTMSG(4,305,12,1,0,' ',0)
              CALL LOCATE(22,4,IERR)
              CALL OKREPLY(REPLY,RTNCODE)
              IF(REPLY .EQ. 'N')THEN
                CALL CLRMSG(4)
                CALL CLRMSG(2)
                NMSG=365
                GOTO 5000
              ELSE
                CALL CLRMSG(4)
                CALL CLRMSG(3)
                NMSG=365
              ENDIF
            ELSE
              CALL WRTMSG(4,51,12,1,1,RTNCODE,1)
              CALL WRTMSG(2,365,14,1,1,' ',0)
              CALL LOCATE(23,0,IERR)
              GOTO 11111
            ENDIF
          ENDIF
        ENDIF
        CALL PUTDATA(LASTID,2,RTNCODE)
        NMSG=365
      ELSE
        NMSG=306
      ENDIF

 5000 CONTINUE
      CALL CLRMSG(1)
      CALL CLRMSG(2)
      CALL LOCATE(16,0,IERR)
      WRITE(*,5010)RECCOUNT
*,NRECCOUNT-RECCOUNT

      WRITE(101,5010)RECCOUNT

      CALL WRTMSG(3,NMSG,14,1,1,' ',0)

*      READ(*,'(A1)')REP

      WRITE(200,'(4HREM ,/,
     *76HREM NB_ENREGISTREMENTS  AN MOIS JOUR HEURE MINUTE SECONDE FICHI
     *ER  ARGUMENT3,/,
     *18HCALL CONTROGS.BAT ,I10,2X,
     *I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,
     *A50,
     *3H %3)')
     *RECCOUNT,
     *KAC,KMC,KJC,KHR,KMIN,KSEC,
     *XFICH

      WRITE(200,'(4HREM ,/,
     *44HREM FICHIER  AN MOIS CODEMOIS JOUR ARGUMENT2,/,
     *18HCALL CATRARCH.BAT ,
     *A50,1X,I2.2,1X,I2.2,1X,A1,1X,I2.2,3H %2)')
     *XFICH,KAC,KMC,CODEM,KJC

      GOTO 17777

4001  CONTINUE
      CLOSE(33)
      GOTO 17777
27777 CONTINUE
      PRINT*,'NOMBRE DE FICHIERS TRAITES : ',NBF
************************************************************************
*     PROCESSING IS COMPLETE.                                          *
************************************************************************

      OPEN(100,FILE='P:\BATCH\MAUVAIGS.BAT',ERR=11115)
      WRITE(100,'(16HREM MAUVAIGS.BAT,/,15HREM * * * * * *,/,
     *4HREM ,/,48HREM NOMBRE_FICHIERS_TRAITES ARGUMENT1 ARGUMENT 2,/,
     *15HCALL BONGS.BAT ,I10,6H %1 %2)')NBF
      CLOSE(100)

      
11115 CONTINUE
      GOTO 11113

5010  FORMAT(30X,I5,' ENREGISTREMENT(S) TRAITE(S)')

      GOTO 11111

*     ERREUR FATALES DANS LE PROCESSUS

9005  CONTINUE
      CALL LOCATE(21,0,IERROR)
      PRINT*,'ERREUR DANS OUVERTURE DE --->',XFICH
      PRINT*,'ERREUR FORTRAN =',IERR
      GOTO 9999

9007  CONTINUE
      CALL IQ(NIU)
      PRINT*,'FICHIER FAUTIF --->',XFICH
      PRINT*,INSTN,'!'
      PRINT*,'ERREUR FORTRAN DANS LA PRELECTURE',IERR
      GOTO 11111

9008  CONTINUE

      CALL IQ(NIU)
      PRINT*,'FICHIER FAUTIF --->',XFICH
      PRINT*,INSTN,' !!'
      PRINT*,'ERREUR FORTRAN DANS LA LECTURE',IERR
      GOTO 11111

9030  CONTINUE
      CALL LOCATE(21,0,IERROR)
      PRINT*,'ELEMENT= ',
     *INELEM(J),
     *'NOT DEFINED FOR THIS DATASET =',
     *DSETID
      GOTO 9999

9035  CONTINUE
      WRITE(101,'(14H ERROR 9035 : ,I10,14H FROM ROUTINE ,A8)')
     *IERROR,ERRSRC
      PRINT*,'ERROR CONDITION RETURNED FROM ROUTINE ',ERRSRC
      CALL LOCATE(21,0,IERROR)
      GOTO 9999

9037  CONTINUE
      CALL LOCATE(21,0,IERROR)
      PRINT*,'VALUES  ALLOWED  Y or y or N or n '
      GOTO 9999

9040  CONTINUE
      WRITE(101,'(14H ERROR 9040 : ,I10,17H HEURE FAUTIVE : ,I2.2)')
     *IERROR,HOUR
      CALL LOCATE(21,0,IERROR)
      PRINT*,
     *'INPUT HOUR DOES NOT AGREE WITH VALUES IN DATAQC.PRM: ',HOUR
      CALL IQ(NIU)
9999  CONTINUE
*      PRINT*,'ENTER ANY KEY TO CONTINUE '
*      CALL GETCHAR(INCHAR,0)
    
11111 CONTINUE

11112 CONTINUE

11113 CONTINUE
      PRINT*
      PRINT*,'EN CAS DE PROBLEME OU BIEN SI VOUS'
      PRINT*,'VOUS POSEZ DES QUESTIONS'
      PRINT*,'REPORTEZ VOUS A LA DOCUMENTATION'
      PRINT*,'OU ENCORE AU PROGRAMME SOURCE'
      PRINT*,'OU BIEN ECRIVEZ  A : gerard.mayencon@meteo.fr'
      PRINT*,'SI LA REPONSE TARDE A VENIR'
      PRINT*,'TELEPHONEZ A 14 UTC AU : (33) 01 45 56 57 85'
      PRINT*,'G‚rard Mayen‡on VOUS REPONDRA'
      PRINT*,'TAPEZ UNE TOUCHE ET C''EST'
      PRINT*,'LA FIN DU PROGRAMME IMPORTGS --->'
      READ(*,'(A1)')REP
*     PARIS MERCREDI 10 NOVEMBRE 1999 SOIR
*     CA DEMANDERAIT JUSTE A ETRE TESTE
*     SALUTATIONS A DENIS STUBER
      END
* * * * * * * * * * * * * *
      SUBROUTINE DIEGO(ZORO,YEAR,MONTH,DAY,HOUR,INVAL)

      PARAMETER (INC=40)

      INTEGER*2 YEAR,MONTH,DAY,HOUR
      INTEGER*2 V(INC+4)

      CHARACTER E*1,P*1,FX*8
      CHARACTER ZORO*288
      
      REAL*4  INVAL(INC)

      FX='(F06.01)'
      P='.'
      IVX=INC+4
      YEAR=0
      DAY=0
      HOUR=0
      K=0
      E=ZORO(1:1)
*      PRINT*,ZORO(1:70)     
    
*     RECHERCHE DE LA LONGEUR DE LA CHAINE
      DO 10 I=287,2,-1
        IF(ZORO(I:I).NE.' ')THEN
          GOTO 11
        ENDIF
10    CONTINUE
11    CONTINUE
*      PRINT*,'LONGEUR CHAINE : ',I
*     LA LIGNE DOIT FINIR PAR UN SEPARATEUR
      IF(ZORO(I:I).NE.E)THEN
*       FORCAGE DE LA FIN DE LA LIGNE AVEC LE SEPARATEUR
        ZORO(I+1:I+1)=E
      ENDIF

*     RECHERCHE DES POSITIONS DES SEPARATEURS
      DO 1 I=2,288
        IF(ZORO(I:I).EQ.E)THEN
          K=K+1
          IF(K.GT.IVX)THEN
            K=IVX
            GOTO 2
          ELSE
            V(K)=I
          ENDIF
        ENDIF
1     CONTINUE
2     CONTINUE
 
      NBV=K
*      PRINT*,NBV,' SEPARATEURS'
      I1=1
      DO 3 I=1,NBV
*        PRINT*,'ON TRAITE LE SEPARATEUR : ',I
*        READ(*,'(A1)')REP
        I2=V(I)
*       DISTANCE ENTRE LES DEUX SEPARATEURS
        I3=(I2-I1)-1
        IF(I3.GT.0)THEN
*         RECHERCHE DU POINT DECIMAL EVENTUEL
          I4=0
          DO 31 J=I1+1,I2-1
            IF(ZORO(J:J).EQ.P)THEN
              I4=J
              GOTO 32
            ENDIF
31        CONTINUE
32        CONTINUE
          IF(I4.NE.0)THEN
            IF(I2-I1.GE.8)THEN                           
*             ATTENTION NOMBRE TROP GROS
              XK=-999.
            ELSE
*              PRINT*,'REEL ',I4
*             IL FAUT LIRE UN NOMBRE REEL
              FX='(F  .  )'
              WRITE(FX(3:4),'(I2.2)')I3
              WRITE(FX(6:7),'(I2.2)')(I2-I4)-1
*              PRINT*,FX,I1+1,I2-1
*              READ(*,'(A1)')REP
*              PRINT*,ZORO(I1+1:I2-1)
*              READ(*,'(A1)')REP
              READ(ZORO(I1+1:I2-1),FMT=FX,ERR=333)XK
            ENDIF
            K=INT(XK)
          ELSE
*           IL FAUT LIRE UN ENTIER              
*            PRINT*,'ENTIER '
*            READ(*,'(A1)')REP
            FX='(I  )'
            WRITE(FX(3:4),'(I2)')I3
*            PRINT*,FX
*            READ(*,'(A1)')REP
            READ(ZORO(I1+1:I2-1),FMT=FX,ERR=333)K
            XK=FLOAT(K)
            K2=INT2(K)
          ENDIF
          IF(I.GE.5)THEN
            INVAL(I-4)=XK
          ELSE
*           ON DECODE UN CHAMP DATE
            IF(I.EQ.1)THEN
*             ANNEE
              IF(K2.GE.100)THEN
                YEAR=K2
              ELSE
                IF(K2.GE.80)THEN
                  YEAR=K2+1900
                ELSE
                  YEAR=K2+2000
                ENDIF
              ENDIF
            ENDIF
            IF(I.EQ.2)THEN
*             MOIS
              MONTH=K2
            ENDIF
            IF(I.EQ.3)THEN
*             JOUR
              DAY=K2
            ENDIF
            IF(I.EQ.4)THEN
*             HEURE
              HOUR=K2
            ENDIF
          ENDIF
        ELSE
*         DEUX VIRGULES SE SUIVENT 
*         CA VEUT JUSTE DIRE QUE LE CHAMP CORRESPONDANT EST MANQUANT
*          PRINT*,I,ZORO(I1:I2)
        ENDIF
*        PRINT*,I,' ',ZORO(I1+1:I2-1),K,XK,FX 
        I1=I2
3     CONTINUE
      GOTO 444
333   CONTINUE
*      PRINT*,'ERREUR IMPORTGS.EXE DANS LA LECTURE DE LA LIGNE'
*      PRINT*,ZORO
      YEAR=0
444   CONTINUE
      END
* * * * * * * * * * *
      SUBROUTINE IQ(N)
      CHARACTER NOMX*50
      N=0
      DO 8642 IU=1,200
        INQUIRE(IU,NAME=NOMX)
        IF(NOMX.NE.'UNKNOWN')THEN
          PRINT*,NOMX,' CONNECTE AU PORT : ',IU
          N=N+1
        ENDIF
8642  CONTINUE
      PRINT*,N,' FICHIERS OUVERTS'
      END