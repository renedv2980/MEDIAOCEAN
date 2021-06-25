*          DATA SET DESVICAMP  AT LEVEL 037 AS OF 08/23/00                      
*PHASE SVICAMPA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
         PRINT NOGEN                                                            
*                                                                               
SVICAMP  TITLE 'CREATE SEPTEMBER INDICES FOR CAMPBELL SOUP'                     
SVICAMP  CSECT                                                                  
         NBASE 600,SVICAMP,=V(REGSAVE)                                          
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
         LA    R4,NSIMKTS          R4 - NSIMKTS TABLE                           
*                                                                               
MAIN     LH    RE,0(R4)                                                         
         C     RE,=F'0'            END OF NSIMKTS TABLE ?                       
         BZ    ENDJOB                                                           
         LA    R5,DYTMS            R5 - DYTMS TABLE                             
         MVI   CARDD,C' '                                                       
         MVC   CARDD+1(79),CARDD                                                
*                                                                               
         MVC   CMED(3),=C'TNG'     MEDIA/SERVICE/SOURCE                         
*                                                                               
M300     LH    RE,0(R4)            MKT# FROM NSIMKTS TABLE                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CMKT,DUB                                                         
*                                                                               
         MVC   CDAY,0(R5)          DAY...FROM DYTMS                             
*                                                                               
         LA    R6,4(R5)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),CSTQH                                    
         SR    RE,RE                                                            
         IC    RE,CSTQH                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CSTQH,DUB           START QH                                     
*                                                                               
         LA    R6,6(R5)                                                         
         GOTO1 =V(HRTOQH),DUB,(0,(R6)),CENDQH                                   
         SR    RE,RE                                                            
         IC    RE,CENDQH                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CENDQH,DUB          END QH                                       
*                                                                               
         MVC   CAUD,=C'01'         AUDIENCE TYPE                                
         MVC   CACT,=C'A'          ACTION CODE                                  
*                                                                               
         MVC   CMONS(4),=C' 100'   MONTHS (JAN)                                 
         MVC   CMONS+4(44),CMONS   (FEB-DEC)                                    
         MVC   CMONS+32(4),8(R5)   SEPT INDEX FROM DYTMS TABLE                  
*                                                                               
         L     R3,=A(OUT)          OUTPUT CARDD RECORD                          
         PUT   (R3),CARDD                                                       
*                                                                               
         LA    RE,NSIMKTS          R4 - NSIMKTS TABLE                           
         CR    RE,R4               1ST MKT ?                                    
         BNE   NOPRINT               IF NOT - BYPASS PRINT                      
         XC    P,P                                                              
         MVC   P(80),CARDD         PRINT OUTPUT OF 1ST MARKET ONLY              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
NOPRINT  LA    R5,12(R5)           DYTMS TABLE - POINT TO NEXT ENTRY            
         LH    RE,0(R5)                                                         
         C     RE,=F'0'            END OF DYTMS TABLE ?                         
         BNZ   M300                  IF NOT - LOOP BACK                         
         LA    R4,32(R4)           NSIMKTS TABLE - POINT TO NEXT ENTRY          
         B     MAIN                PROCESS NEXT MARKET                          
*                                                                               
ENDJOB   CLOSE (OUT)                                                            
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
DUB      DS    D                   DOUBLE WORD                                  
WORD     DS    F                   FULL WORD                                    
HALF     DS    H                   HALF WORD                                    
CHAR     DS    C                   CHARACTER                                    
*                                                                               
CARDD    DS    0H                                                               
CMED     DS    CL1                 MEDIA = T                                    
CRAT     DS    CL1                 SERVICE = N                                  
CSRC     DS    CL1                 SOURCE = E/F                                 
         DS    CL1                                                              
CMKT     DS    CL4                 MARKET                                       
         DS    CL1                                                              
CDAY     DS    CL3                 DAY M-F,SAT,SUN                              
         DS    CL1                                                              
CSTQH    DS    CL2                 START QH                                     
         DS    CL1                                                              
CENDQH   DS    CL2                 END QH                                       
         DS    CL1                                                              
CAUD     DS    CL2                 AUDIENCE TYPE = 01                           
         DS    CL1                                                              
CACT     DS    CL1                 ACTION CODE = A                              
         DS    CL1                                                              
CMONS    DS    CL48                MONTHS                                       
         DS    CL8                 SPARE                                        
*                                                                               
         LTORG                                                                  
*                                                                               
DYTMS    DS    0H                            SEPT                               
*                DAY       STRT      END      SVI                               
         DC    C'M-F ',AL2(0700),AL2(1600),C' 100' M-F/7A-4P                    
         DC    C'M-F ',AL2(1600),AL2(1800),C' 103' M-F/4P-6P                    
         DC    C'SAT ',AL2(1600),AL2(1800),C' 103' SAT/4P-6P                    
         DC    C'M-F ',AL2(1800),AL2(1930),C' 107' M-F/6P-730P                  
         DC    C'SAT ',AL2(1800),AL2(1930),C' 107' SAT/6P-730P                  
         DC    C'M-F ',AL2(1930),AL2(2000),C' 115' M-F/730P-8P                  
         DC    C'SAT ',AL2(1930),AL2(2000),C' 115' SAT/730P-8P                  
         DC    C'M-F ',AL2(2000),AL2(2300),C' 116' M-SA/8P-11P                  
         DC    C'SAT ',AL2(2000),AL2(2300),C' 116'     "                        
         DC    C'SUN ',AL2(1900),AL2(2300),C' 116' SUN/7P-11P                   
         DC    C'M-F ',AL2(2300),AL2(0200),C' 100' M-SU/11P-2A                  
         DC    C'SAT ',AL2(2300),AL2(0200),C' 100'     "                        
         DC    C'SUN ',AL2(2300),AL2(0200),C' 100'     "                        
DYTMSX   DC    F'0'                                                             
*                                                                               
*NSIMKTS DC    C'NT',AL3(NSIMKTSX+2)                                            
NSIMKTS  DS    0CL32                                                            
         DC    AL2(0001),CL30'NETWORK'                                          
         DC    AL2(0100),CL30'PORTLAND-POLAND SPRING'                           
         DC    AL2(0101),CL30'NEW YORK'                                         
         DC    AL2(0102),CL30'BINGHAMTON'                                       
         DC    AL2(0103),CL30'MACON'                                            
         DC    AL2(0104),CL30'PHILADELPHIA'                                     
         DC    AL2(0105),CL30'DETROIT'                                          
         DC    AL2(0106),CL30'BOSTON'                                           
         DC    AL2(0107),CL30'SAVANNAH'                                         
         DC    AL2(0108),CL30'PITTSBURGH'                                       
         DC    AL2(0109),CL30'FT. WAYNE'                                        
         DC    AL2(0110),CL30'CLEVELAND'                                        
         DC    AL2(0111),CL30'WASHINGTON,DC'                                    
         DC    AL2(0112),CL30'BALTIMORE'                                        
         DC    AL2(0113),CL30'FLINT-SAGINAW-BAY CITY'                           
         DC    AL2(0114),CL30'BUFFALO'                                          
         DC    AL2(0115),CL30'CINCINNATI'                                       
         DC    AL2(0116),CL30'ERIE'                                             
         DC    AL2(0117),CL30'CHARLOTTE'                                        
         DC    AL2(0118),CL30'GRNSBORO-H.POINT-W.SALEM'                         
         DC    AL2(0119),CL30'CHARLESTON,SC'                                    
         DC    AL2(0120),CL30'AUGUSTA,GA'                                       
         DC    AL2(0121),CL30'PROVIDENCE'                                       
         DC    AL2(0122),CL30'COLUMBUS,GA'                                      
         DC    AL2(0123),CL30'BURLINGTON-PLATTSBURGH'                           
         DC    AL2(0124),CL30'ATLANTA'                                          
         DC    AL2(0125),CL30'ALBANY,GA'                                        
         DC    AL2(0127),CL30'INDIANAPOLIS'                                     
         DC    AL2(0128),CL30'MIAMI-FT. LAUDERDALE'                             
         DC    AL2(0129),CL30'LOUISVILLE'                                       
         DC    AL2(0130),CL30'TALLAHASSEE-THOMASVILLE'                          
         DC    AL2(0131),CL30'TRI-CITIES, TN-VA'                                
         DC    AL2(0132),CL30'ALBANY-SCHNCTADY-TROY'                            
         DC    AL2(0133),CL30'HARTFORD-NEW HAVEN'                               
         DC    AL2(0134),CL30'ORLANDO-DAYTON BEACH'                             
         DC    AL2(0135),CL30'COLUMBUS,OH'                                      
         DC    AL2(0136),CL30'YOUNGSTOWN'                                       
         DC    AL2(0137),CL30'BANGOR'                                           
         DC    AL2(0138),CL30'ROCHESTER'                                        
         DC    AL2(0139),CL30'TAMPA-ST PETERSBURG'                              
         DC    AL2(0140),CL30'TRAVERSE CITY-CADILLAC'                           
         DC    AL2(0141),CL30'LEXINGTON'                                        
         DC    AL2(0142),CL30'DAYTON'                                           
         DC    AL2(0143),CL30'SPRINFIELD-HOLYOKE'                               
         DC    AL2(0144),CL30'NORFOLK-PORTSMTH-NEWPT NWS'                       
         DC    AL2(0145),CL30'GRNVLLE-N.BERN-WASHNGTN'                          
         DC    AL2(0146),CL30'COLUMBIA,SC'                                      
         DC    AL2(0147),CL30'TOLEDO'                                           
         DC    AL2(0148),CL30'WEST PALM BEACH'                                  
         DC    AL2(0149),CL30'WATERTOWN'                                        
         DC    AL2(0150),CL30'WILMINGTON'                                       
         DC    AL2(0151),CL30'LANSING'                                          
         DC    AL2(0152),CL30'PRESQUE ISLE'                                     
         DC    AL2(0153),CL30'MARQUETTE'                                        
         DC    AL2(0154),CL30'WHEELING-STEUBENVILLE'                            
         DC    AL2(0155),CL30'SYRACUSE'                                         
         DC    AL2(0156),CL30'RICHMOND-PETERSBURG'                              
         DC    AL2(0157),CL30'KNOXVILLE'                                        
         DC    AL2(0158),CL30'LIMA'                                             
         DC    AL2(0159),CL30'BKLY-BLUEFIELD-OAK HILL'                          
         DC    AL2(0160),CL30'RALEIGH-DURHAM'                                   
         DC    AL2(0161),CL30'JACKSONVILLE'                                     
         DC    AL2(0162),CL30'SARASOTA'                                         
         DC    AL2(0163),CL30'GRAND RAPIDS-KALAMAZOO'                           
         DC    AL2(0164),CL30'CHARLESTON-HUNTINGTON'                            
         DC    AL2(0165),CL30'ELMIRA'                                           
         DC    AL2(0166),CL30'HRRSBRG-LANCSTR-LEB-YRK'                          
         DC    AL2(0167),CL30'GRNVLL-SPART-ASHEVILLE'                           
         DC    AL2(0168),CL30'ATLANTA'                                          
         DC    AL2(0169),CL30'HARRISONBURG'                                     
         DC    AL2(0170),CL30'FLORENCE,SC'                                      
         DC    AL2(0171),CL30'FT. MYERS'                                        
         DC    AL2(0172),CL30'MANCHESTER'                                       
         DC    AL2(0173),CL30'ROANOKE-LYNCHBURG'                                
         DC    AL2(0174),CL30'JOHNSTOWN-ALTOONA'                                
         DC    AL2(0175),CL30'CHATTANOOGA'                                      
         DC    AL2(0176),CL30'SALISBURY'                                        
         DC    AL2(0177),CL30'WILKES BARRE-SCRANTON'                            
         DC    AL2(0178),CL30'NEW HAVEN(METRO)'                                 
         DC    AL2(0179),CL30'HAGERSTOWN'                                       
         DC    AL2(0181),CL30'TERRE HAUTE'                                      
         DC    AL2(0182),CL30'LAFAYETTE,IN'                                     
         DC    AL2(0183),CL30'ALPENA'                                           
         DC    AL2(0184),CL30'CHARLOTTESVILLE'                                  
         DC    AL2(0185),CL30'AKRON'                                            
         DC    AL2(0188),CL30'SOUTH BEND-ELKHART'                               
         DC    AL2(0192),CL30'GAINESVILLE'                                      
         DC    AL2(0193),CL30'WORCESTER'                                        
         DC    AL2(0194),CL30'HAGERSTOWN'                                       
         DC    AL2(0195),CL30'AKRON'                                            
         DC    AL2(0196),CL30'ZANESVILLE'                                       
         DC    AL2(0197),CL30'PARKERSBURG'                                      
         DC    AL2(0198),CL30'CLARKSBURG-WESTON'                                
         DC    AL2(0199),CL30'SARASOTA'                                         
         DC    AL2(0200),CL30'CORPUS CHRISTI'                                   
         DC    AL2(0202),CL30'CHICAGO'                                          
         DC    AL2(0203),CL30'JOPLIN-PITTSBURG'                                 
         DC    AL2(0204),CL30'COLUMBIA-JEFFERSON CITY'                          
         DC    AL2(0205),CL30'TOPEKA'                                           
         DC    AL2(0206),CL30'DOTHAN'                                           
         DC    AL2(0209),CL30'ST. LOUIS'                                        
         DC    AL2(0210),CL30'ROCKFORD'                                         
         DC    AL2(0211),CL30'MASON CITY-AUSTIN-ROCHESTER'                      
         DC    AL2(0212),CL30'SHREVEPORT'                                       
         DC    AL2(0213),CL30'MINNEAPOLIS-ST. PAUL'                             
         DC    AL2(0216),CL30'KANSAS CITY'                                      
         DC    AL2(0217),CL30'MILWAUKEE'                                        
         DC    AL2(0218),CL30'HOUSTON'                                          
         DC    AL2(0219),CL30'SPRINGFIELD,MO'                                   
         DC    AL2(0222),CL30'NEW ORLEANS'                                      
         DC    AL2(0223),CL30'DALLAS-FT. WORTH'                                 
         DC    AL2(0224),CL30'SIOUX CITY'                                       
         DC    AL2(0225),CL30'WACO-TEMPLE'                                      
         DC    AL2(0226),CL30'VICTORIA'                                         
         DC    AL2(0227),CL30'WICHITA FALLS-LAWTON'                             
         DC    AL2(0228),CL30'MONROE-EL DORADO'                                 
         DC    AL2(0229),CL30'LAWTON(METRO)'                                    
         DC    AL2(0230),CL30'BIRMINGHAM'                                       
         DC    AL2(0231),CL30'OTTUMWA-KIRKSVILLE'                               
         DC    AL2(0232),CL30'PADUCAH-C.GIRARDEAU-HRRBRG'                       
         DC    AL2(0233),CL30'ODESSA-MIDLAND-MONAHANS'                          
         DC    AL2(0234),CL30'AMARILLO'                                         
         DC    AL2(0235),CL30'AUSTIN,TX'                                        
         DC    AL2(0236),CL30'HARLINGEN-WESLACO'                                
         DC    AL2(0237),CL30'CEDAR RAPIDS-WATERLOO'                            
         DC    AL2(0238),CL30'ST. JOSEPH'                                       
         DC    AL2(0239),CL30'JACKSON,TN'                                       
         DC    AL2(0240),CL30'MEMPHIS'                                          
         DC    AL2(0241),CL30'SAN ANTONIO'                                      
         DC    AL2(0242),CL30'LAFAYETTE,LA'                                     
         DC    AL2(0243),CL30'LAKE CHARLES'                                     
         DC    AL2(0244),CL30'ALEXANDRIA,LA'                                    
         DC    AL2(0246),CL30'ANNISTON'                                         
         DC    AL2(0247),CL30'GREENWOOD'                                        
         DC    AL2(0248),CL30'CHMPAGN-SPRNGFLD-DECATUR'                         
         DC    AL2(0249),CL30'EVANSVILLE'                                       
         DC    AL2(0250),CL30'OKLAHOMA CITY'                                    
         DC    AL2(0251),CL30'LUBBOCK'                                          
         DC    AL2(0252),CL30'OMAHA'                                            
         DC    AL2(0253),CL30'SPRNFLD-DECATUR(METRO)'                           
         DC    AL2(0254),CL30'HASTINGS-KEARNEY(METRO)'                          
         DC    AL2(0255),CL30'DUBUQUE'                                          
         DC    AL2(0256),CL30'PANAMA CITY'                                      
         DC    AL2(0257),CL30'ADA-ARDMORE'                                      
         DC    AL2(0258),CL30'GREEN BAY'                                        
         DC    AL2(0259),CL30'NASHVILLE'                                        
         DC    AL2(0260),CL30'ANNISTON'                                         
         DC    AL2(0261),CL30'SAN ANGELO'                                       
         DC    AL2(0262),CL30'ABILENE-SWEETWATER'                               
         DC    AL2(0269),CL30'MADISON'                                          
         DC    AL2(0270),CL30'FT. SMITH'                                        
         DC    AL2(0271),CL30'TULSA'                                            
         DC    AL2(0273),CL30'COLUMBUS-TUPELO'                                  
         DC    AL2(0275),CL30'PEORIA'                                           
         DC    AL2(0276),CL30'DULUTH-SUPERIOR'                                  
         DC    AL2(0278),CL30'WICHITA-HUTCHINSON'                               
         DC    AL2(0279),CL30'DES MOINES AMES'                                  
         DC    AL2(0282),CL30'DAVENPORT-R.ISLAND-MOLINE'                        
         DC    AL2(0286),CL30'MOBILE-PENSACOLA'                                 
         DC    AL2(0287),CL30'MINOT-BSMRK-DICKINSON'                            
         DC    AL2(0288),CL30'MINOT-BISMARK'                                    
         DC    AL2(0290),CL30'GREAT BEND'                                       
         DC    AL2(0291),CL30'HUNTSVILLE-DECATUR'                               
         DC    AL2(0292),CL30'BEAUMONT-PORT ARTHUR'                             
         DC    AL2(0293),CL30'LITTLE ROCK-PINE BLUFF'                           
         DC    AL2(0297),CL30'ALEXANDRIA,MN'                                    
         DC    AL2(0298),CL30'MONTGOMERY'                                       
         DC    AL2(0299),CL30'WICHITA'                                          
         DC    AL2(0302),CL30'LA CROSSE-EAU CLAIR'                              
         DC    AL2(0303),CL30'EAU CLAIRE'                                       
         DC    AL2(0304),CL30'LA CROSSE'                                        
         DC    AL2(0305),CL30'WAUSAU'                                           
         DC    AL2(0306),CL30'RHINELANDER'                                      
         DC    AL2(0309),CL30'TYLER'                                            
         DC    AL2(0310),CL30'HATTIESBURG-LAUREL'                               
         DC    AL2(0311),CL30'MERIDIAN'                                         
         DC    AL2(0316),CL30'BATON ROUGE'                                      
         DC    AL2(0317),CL30'QUINCY-HANNIBAL-KEOKUK'                           
         DC    AL2(0318),CL30'JACKSON, MS'                                      
         DC    AL2(0322),CL30'LINCOLN-HSTINGS-KEARNEY'                          
         DC    AL2(0324),CL30'FARGO-VALLEY CITY'                                
         DC    AL2(0325),CL30'SIOUX FALLS'                                      
         DC    AL2(0333),CL30'FLORENCE,AL'                                      
         DC    AL2(0334),CL30'JONESBORO'                                        
         DC    AL2(0336),CL30'BOWLING GREEN'                                    
         DC    AL2(0337),CL30'MANKATO'                                          
         DC    AL2(0338),CL30'BOWLING GREEN'                                    
         DC    AL2(0339),CL30'FT. DODGE'                                        
         DC    AL2(0340),CL30'N.PLATTE-HAYES-MC COOK'                           
         DC    AL2(0341),CL30'HAYS-GOODLAND'                                    
         DC    AL2(0342),CL30'ENSIGN-GARDEN CITY'                               
         DC    AL2(0343),CL30'ANCHORAGE'                                        
         DC    AL2(0344),CL30'HONOLULU'                                         
         DC    AL2(0345),CL30'FAIRBANKS'                                        
         DC    AL2(0346),CL30'BILOXI'                                           
         DC    AL2(0349),CL30'LAREDO'                                           
         DC    AL2(0350),CL30'FLAGSTAFF'                                        
         DC    AL2(0351),CL30'DENVER'                                           
         DC    AL2(0352),CL30'COLORADO SPRINGS-PUEBLO'                          
         DC    AL2(0353),CL30'PHOENIX'                                          
         DC    AL2(0354),CL30'BUTTE'                                            
         DC    AL2(0355),CL30'GREAT FALLS'                                      
         DC    AL2(0356),CL30'BILLINGS'                                         
         DC    AL2(0357),CL30'BOISE'                                            
         DC    AL2(0358),CL30'IDAHO FALLS-POCATELLO'                            
         DC    AL2(0359),CL30'CHYENN-SCOTTSBLUF-STERLNG'                        
         DC    AL2(0360),CL30'TWIN FALLS'                                       
         DC    AL2(0361),CL30'ROSWELL'                                          
         DC    AL2(0362),CL30'MISSOULA'                                         
         DC    AL2(0363),CL30'FLAGSTAFF'                                        
         DC    AL2(0364),CL30'RAPID CITY'                                       
         DC    AL2(0365),CL30'EL PASO'                                          
         DC    AL2(0367),CL30'CASPER-RIVERTON'                                  
         DC    AL2(0370),CL30'SALT LAKE CITY'                                   
         DC    AL2(0371),CL30'YUMA-EL CENTRO'                                   
         DC    AL2(0373),CL30'GRAND JUNCTION-MONTROSE'                          
         DC    AL2(0387),CL30'DICKINSON'                                        
         DC    AL2(0389),CL30'TUCSON'                                           
         DC    AL2(0390),CL30'ALBUQUERQUE'                                      
         DC    AL2(0391),CL30'FARMINGTON'                                       
         DC    AL2(0392),CL30'FARMINGTON'                                       
         DC    AL2(0398),CL30'GLENDIVE'                                         
         DC    AL2(0400),CL30'BAKERSFIELD'                                      
         DC    AL2(0401),CL30'EUGENE'                                           
         DC    AL2(0402),CL30'EUREKA'                                           
         DC    AL2(0403),CL30'LOS ANGELES'                                      
         DC    AL2(0404),CL30'PALM SPRINGS'                                     
         DC    AL2(0405),CL30'PALM SPRINGS'                                     
         DC    AL2(0407),CL30'SAN FRANCISCO-OAKLAND'                            
         DC    AL2(0410),CL30'YAKIMA'                                           
         DC    AL2(0411),CL30'RENO'                                             
         DC    AL2(0413),CL30'MEDFORD-KLAMATH-FALLS'                            
         DC    AL2(0419),CL30'SEATTLE-TACOMA'                                   
         DC    AL2(0420),CL30'PORTLAND,OR'                                      
         DC    AL2(0421),CL30'BEND,OR'                                          
         DC    AL2(0425),CL30'SAN DIEGO'                                        
         DC    AL2(0428),CL30'MONTEREY-SALINAS'                                 
         DC    AL2(0439),CL30'LAS VEGAS'                                        
         DC    AL2(0455),CL30'SANTA BARBARA-SANTA MARIA'                        
         DC    AL2(0462),CL30'SACRAMENTO-STOCKTON'                              
         DC    AL2(0466),CL30'FRESNO(VISALIA)'                                  
         DC    AL2(0468),CL30'CHICO-REDDING'                                    
         DC    AL2(0481),CL30'SPOKANE'                                          
         DC    AL2(0498),CL30'BELLINGHAM'                                       
NSIMKTSX DC    AL2(0)                                                           
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=08000,                                          X        
               MACRF=PM                                                         
*                                                                               
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037DESVICAMP 08/23/00'                                      
         END                                                                    
