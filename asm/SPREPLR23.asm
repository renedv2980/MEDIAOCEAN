*          DATA SET SPREPLR23  AT LEVEL 007 AS OF 05/01/02                      
*PHASE SPLR02C,+0,NOAUTO                                                        
         TITLE 'SPREPLR23 STRIP NETWORK RECORDS FROM PURFIL'                    
         PRINT NOGEN                                                            
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
*                                                                               
         LA    R4,2048(RB)         R4 IS SECOND BASE REGISTER                   
         LA    R4,2048(R4)                                                      
         USING SPLR02,RB,R4                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (OUT,(OUTPUT))                                                   
*                                                                               
OPEN3    L     R2,ADAGY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',FILELIST,(R2)                     
         B     OPEN4                                                            
         EJECT                                                                  
FILELIST DS    0C                                                               
         DC    CL8'NPAVDIR '                                                    
         DC    CL8'NPAVFIL '                                                    
         DC    C'X '                                                            
         SPACE 2                                                                
OPEN4    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING PNKEY,R6                                                         
         XC    PRVKEY,PRVKEY                                                    
         XC    CCOUNT,CCOUNT                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PAVDIR ',SVSPLKEY,SPLKEY              
         B     SPLS6                                                            
         SPACE 2                                                                
SPLS2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PAVDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0             READ OK?                                     
         BNE   EXIT1               EOF                                          
SPLS6    L     R7,RCOUNT                                                        
         LA    R7,1(R7)                                                         
         ST    R7,RCOUNT                                                        
         CLC   PNCODE(3),=C'QTN'                                                
         BE    *+8                                                              
         CLI   PNCODE+1,C'N'                                                    
         BE    SPLS8                                                            
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SPLKEY                                                 
         BAS   RE,PUTOUT                                                        
         LA    R8,OPTAB                                                         
         BAS   RE,SUMIT                                                         
         DROP  R7                                                               
         B     SPLS2                                                            
*                                                                               
SPLS8    L     R7,DCOUNT                                                        
         LA    R7,1(R7)                                                         
         ST    R7,DCOUNT                                                        
         LA    R8,DETAB                                                         
         BAS   RE,SUMIT                                                         
         C     R7,=F'30'                                                        
         BH    SPLS2                                                            
         MVC   P+1(7),=C'PURGED '                                               
         MVC   P+10(24),SPLKEY                                                  
         GOTO1 REPORT                                                           
         B     SPLS2                                                            
*                                                                               
SUMIT    CLI   0(R8),0                                                          
         BE    SUMIT2                                                           
         CLC   SPLKEY(3),0(R8)                                                  
         BE    *+12                                                             
         LA    R8,7(R8)                                                         
         B     SUMIT                                                            
SUMIT2   MVC   0(3,R8),SPLKEY                                                   
         ICM   RF,15,3(R8)                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,15,3(R8)                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
         DROP  R6,R7                                                            
SPLFEX   XIT1                                                                   
         EJECT                                                                  
PUTOUT   NTR1                                                                   
         LA    R5,RECIOLN                                                       
         L     R7,=A(OUT)                                                       
         PUT   (R7),(R5)                                                        
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         L     RE,CCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,CCOUNT                                                        
         C     RE,=F'100'                                                       
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,30                                         
         MVC   P+62(30),RECIOLN+4                                               
         GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         EJECT                                                                  
         PRINT GEN                                                              
EXIT1    CLOSE (OUT)                                                            
*        L     R3,ADAGY                                                         
*        GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT',,(R3)                           
         MVC   P+00(06),=C'OUTPUT'                                              
         MVC   P+10(07),=C'DELETED'                                             
         MVC   P+20(06),=C'INPUT '                                              
         GOTO1 REPORT                                                           
         MVC   P+00(06),=C'------'                                              
         MVC   P+10(07),=C'-------'                                             
         MVC   P+20(06),=C'----- '                                              
         GOTO1 REPORT                                                           
         EDIT  (B4,OCOUNT),(8,P)                                                
         EDIT  (B4,DCOUNT),(8,P+10)                                             
         EDIT  (B4,RCOUNT),(8,P+20)                                             
         GOTO1 REPORT                                                           
         LA    R8,OPTAB                                                         
         MVC   P(11),=C'OUTPUT KEYS'                                            
         BAS   RE,PRINTAB                                                       
         LA    R8,DETAB                                                         
         MVC   P(11),=C'DELETE KEYS'                                            
         BAS   RE,PRINTAB                                                       
EXIT     XMOD1 1                                                                
         EJECT                                                                  
PRINTAB  NTR1                                                                   
PRINTAB2 MVC   P+15(3),0(R8)                                                    
         MVC   OCOUNT,3(R8)                                                     
         EDIT  (B4,OCOUNT),(8,P+20)                                             
         GOTO1 REPORT                                                           
         LA    R8,7(R8)                                                         
         CLI   0(R8),0                                                          
         BNE   PRINTAB2                                                         
         XIT1                                                                   
         PRINT NOGEN                                                            
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
OCOUNT DC      F'0'                                                             
CCOUNT DC      F'0'                                                             
RCOUNT DC      F'0'                                                             
DCOUNT DC      F'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
OPTAB    DC    255X'00'                                                         
DETAB    DC    255X'00'                                                         
PRVSTAT  DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PVSMS    DS    CL7                                                              
PRVKEY   DS    CL24                                                             
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
SVDA     DS    CL4                                                              
HLDSTA   DS    CL5                                                              
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
BKTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
         LTORG                                                                  
PROGBUF  DS    6000C                                                            
         EJECT                                                                  
*     BOOK TYPES ARE ASSIGNED AS FOLLW                                          
*         H=HISPANIC                                                            
*         B=BLACK                                                               
*         T=TRADING AREA                                                        
*         M=METRO                                                               
*         N=NETWORK AFFILIATES ONLY                                             
*         S=SCANAMERICA / TMG SCANNER (ARB ONLY)                                
*         E=EXTENDED AREA                                                       
*                                                                               
* TABLE OF MARKETS                                                              
*                                                                               
MARKETS  DS    0XL33                                                            
NSIMKTS  DC    AL2(0001),C' ',CL30'NETWORK'                                     
         DC    AL2(0100),C' ',CL30'PORTLAND-AUBURN, ME.'                        
         DC    AL2(0101),C' ',CL30'NEW YORK'                                    
         DC    AL2(0102),C' ',CL30'BINGHAMTON'                                  
         DC    AL2(0103),C' ',CL30'MACON'                                       
         DC    AL2(0104),C' ',CL30'PHILADELPHIA'                                
         DC    AL2(0105),C' ',CL30'DETROIT'                                     
         DC    AL2(0106),C' ',CL30'BOSTON'                                      
         DC    AL2(0107),C' ',CL30'SAVANNAH'                                    
         DC    AL2(0108),C' ',CL30'PITTSBURGH'                                  
         DC    AL2(0109),C' ',CL30'FT. WAYNE'                                   
         DC    AL2(0110),C' ',CL30'CLEVELAND'                                   
         DC    AL2(0111),C' ',CL30'WASHINGTON,DC'                               
         DC    AL2(0112),C' ',CL30'BALTIMORE'                                   
         DC    AL2(0113),C' ',CL30'FLINT-SAGINAW-BAY CITY'                      
         DC    AL2(0114),C' ',CL30'BUFFALO'                                     
         DC    AL2(0115),C' ',CL30'CINCINNATI'                                  
         DC    AL2(0116),C' ',CL30'ERIE'                                        
         DC    AL2(0117),C' ',CL30'CHARLOTTE'                                   
         DC    AL2(0118),C' ',CL30'GRNSBORO-H.POINT-W.SALEM'                    
         DC    AL2(0119),C' ',CL30'CHARLESTON,SC'                               
         DC    AL2(0120),C' ',CL30'AUGUSTA,GA'                                  
         DC    AL2(0121),C' ',CL30'PROVIDENCE'                                  
         DC    AL2(0122),C' ',CL30'COLUMBUS,GA'                                 
         DC    AL2(0123),C' ',CL30'BURLINGTON-PLATTSBURGH'                      
         DC    AL2(0124),C' ',CL30'ATLANTA'                                     
         DC    AL2(0125),C' ',CL30'ALBANY,GA'                                   
         DC    AL2(0126),C' ',CL30'UTICA'                                       
         DC    AL2(0127),C' ',CL30'INDIANAPOLIS'                                
         DC    AL2(0128),C' ',CL30'MIAMI-FT. LAUDERDALE'                        
         DC    AL2(0129),C' ',CL30'LOUISVILLE'                                  
         DC    AL2(0130),C' ',CL30'TALLAHASSEE-THOMASVILLE'                     
         DC    AL2(0131),C' ',CL30'TRI-CITIES, TN-VA'                           
         DC    AL2(0132),C' ',CL30'ALBANY-SCHNCTADY-TROY'                       
         DC    AL2(0133),C' ',CL30'HARTFORD-NEW HAVEN'                          
         DC    AL2(0134),C' ',CL30'ORLANDO-DAYTON BEACH'                        
         DC    AL2(0135),C' ',CL30'COLUMBUS,OH'                                 
         DC    AL2(0136),C' ',CL30'YOUNGSTOWN'                                  
         DC    AL2(0137),C' ',CL30'BANGOR'                                      
         DC    AL2(0138),C' ',CL30'ROCHESTER'                                   
         DC    AL2(0139),C' ',CL30'TAMPA-ST PETERSBURG'                         
         DC    AL2(0140),C' ',CL30'TRAVERSE CITY-CADILLAC'                      
         DC    AL2(0141),C' ',CL30'LEXINGTON'                                   
         DC    AL2(0142),C' ',CL30'DAYTON'                                      
         DC    AL2(0143),C' ',CL30'SPRINFIELD-HOLYOKE'                          
         DC    AL2(0144),C' ',CL30'NORFOLK-PORTSMTH-NEWPT NWS'                  
         DC    AL2(0145),C' ',CL30'GRNVLLE-N.BERN-WASHNGTN'                     
         DC    AL2(0146),C' ',CL30'COLUMBIA,SC'                                 
         DC    AL2(0147),C' ',CL30'TOLEDO'                                      
         DC    AL2(0148),C' ',CL30'WEST PALM BEACH'                             
         DC    AL2(0149),C' ',CL30'WATERTOWN'                                   
         DC    AL2(0150),C' ',CL30'WILMINGTON'                                  
         DC    AL2(0151),C' ',CL30'LANSING'                                     
         DC    AL2(0152),C' ',CL30'PRESQUE ISLE'                                
         DC    AL2(0153),C' ',CL30'MARQUETTE'                                   
         DC    AL2(0154),C' ',CL30'WHEELING-STEUBENVILLE'                       
         DC    AL2(0155),C' ',CL30'SYRACUSE'                                    
         DC    AL2(0156),C' ',CL30'RICHMOND-PETERSBURG'                         
         DC    AL2(0157),C' ',CL30'KNOXVILLE'                                   
         DC    AL2(0158),C' ',CL30'LIMA'                                        
         DC    AL2(0159),C' ',CL30'BKLY-BLUEFIELD-OAK HILL'                     
         DC    AL2(0160),C' ',CL30'RALEIGH-DURHAM'                              
         DC    AL2(0161),C' ',CL30'JACKSONVILLE'                                
         DC    AL2(0162),C' ',CL30'SARASOTA'                                    
         DC    AL2(0163),C' ',CL30'GRAND RAPIDS-KALAMAZOO'                      
         DC    AL2(0164),C' ',CL30'CHARLESTON-HUNTINGTON'                       
         DC    AL2(0165),C' ',CL30'ELMIRA'                                      
         DC    AL2(0166),C' ',CL30'HRRSBRG-LANCSTR-LEB-YRK'                     
         DC    AL2(0167),C' ',CL30'GRNVLL-SPART-ASHEVILLE'                      
         DC    AL2(0168),C' ',CL30'ATLANTA'                                     
         DC    AL2(0169),C' ',CL30'HARRISONBURG'                                
         DC    AL2(0170),C' ',CL30'FLORENCE,SC'                                 
         DC    AL2(0171),C' ',CL30'FT. MYERS'                                   
         DC    AL2(0172),C' ',CL30'MANCHESTER'                                  
         DC    AL2(0173),C' ',CL30'ROANOKE-LYNCHBURG'                           
         DC    AL2(0174),C' ',CL30'JOHNSTOWN-ALTOONA'                           
         DC    AL2(0175),C' ',CL30'CHATTANOOGA'                                 
         DC    AL2(0176),C' ',CL30'SALISBURY'                                   
         DC    AL2(0177),C' ',CL30'WILKES BARRE-SCRANTON'                       
         DC    AL2(0178),C'M',CL30'NEW HAVEN(METRO)'                            
         DC    AL2(0179),C' ',CL30'HAGERSTOWN'                                  
         DC    AL2(0181),C' ',CL30'TERRE HAUTE'                                 
         DC    AL2(0182),C' ',CL30'LAFAYETTE,IN'                                
         DC    AL2(0183),C' ',CL30'ALPENA'                                      
         DC    AL2(0184),C' ',CL30'CHARLOTTESVILLE'                             
         DC    AL2(0185),C' ',CL30'AKRON'                                       
         DC    AL2(0188),C' ',CL30'SOUTH BEND-ELKHART'                          
         DC    AL2(0192),C' ',CL30'GAINESVILLE'                                 
         DC    AL2(0193),C' ',CL30'WORCESTER'                                   
         DC    AL2(0194),C' ',CL30'HAGERSTOWN'                                  
         DC    AL2(0195),C' ',CL30'AKRON'                                       
         DC    AL2(0196),C' ',CL30'ZANESVILLE'                                  
         DC    AL2(0197),C' ',CL30'PARKERSBURG'                                 
         DC    AL2(0198),C' ',CL30'CLARKSBURG-WESTON'                           
         DC    AL2(0199),C' ',CL30'SARASOTA'                                    
         DC    AL2(0200),C' ',CL30'CORPUS CHRISTI'                              
         DC    AL2(0201),C'M',CL30'WASHINGTON D.C. METRO'                       
         DC    AL2(0202),C' ',CL30'CHICAGO'                                     
         DC    AL2(0203),C' ',CL30'JOPLIN-PITTSBURG'                            
         DC    AL2(0204),C' ',CL30'COLUMBIA-JEFFERSON CITY'                     
         DC    AL2(0205),C' ',CL30'TOPEKA'                                      
         DC    AL2(0206),C' ',CL30'DOTHAN'                                      
         DC    AL2(0208),C'T',CL30'HARTFORD, VT. TA'                            
         DC    AL2(0209),C' ',CL30'ST. LOUIS'                                   
         DC    AL2(0210),C' ',CL30'ROCKFORD'                                    
         DC    AL2(0211),C' ',CL30'MASON CITY-AUSTIN-ROCHESTER'                 
         DC    AL2(0212),C' ',CL30'SHREVEPORT'                                  
         DC    AL2(0213),C' ',CL30'MINNEAPOLIS-ST. PAUL'                        
         DC    AL2(0215),C' ',CL30'LAFAYETTE, IN'                               
         DC    AL2(0216),C' ',CL30'KANSAS CITY'                                 
         DC    AL2(0217),C' ',CL30'MILWAUKEE'                                   
         DC    AL2(0218),C' ',CL30'HOUSTON'                                     
         DC    AL2(0219),C' ',CL30'SPRINGFIELD,MO'                              
         DC    AL2(0220),C' ',CL30'TUSCALOOSA, AL.'                             
         DC    AL2(0222),C' ',CL30'NEW ORLEANS'                                 
         DC    AL2(0223),C' ',CL30'DALLAS-FT. WORTH'                            
         DC    AL2(0224),C' ',CL30'SIOUX CITY'                                  
         DC    AL2(0225),C' ',CL30'WACO-TEMPLE'                                 
         DC    AL2(0226),C' ',CL30'VICTORIA'                                    
         DC    AL2(0227),C' ',CL30'WICHITA FALLS-LAWTON'                        
         DC    AL2(0228),C' ',CL30'MONROE-EL DORADO'                            
         DC    AL2(0229),C' ',CL30'LAWTON(METRO)'                               
         DC    AL2(0230),C' ',CL30'BIRMINGHAM'                                  
         DC    AL2(0231),C' ',CL30'OTTUMWA-KIRKSVILLE'                          
         DC    AL2(0232),C' ',CL30'PADUCAH-C.GIRARDEAU-HRRBRG'                  
         DC    AL2(0233),C' ',CL30'ODESSA-MIDLAND-MONAHANS'                     
         DC    AL2(0234),C' ',CL30'AMARILLO'                                    
         DC    AL2(0235),C' ',CL30'AUSTIN,TX'                                   
         DC    AL2(0236),C' ',CL30'HARLINGEN-WESLACO'                           
         DC    AL2(0237),C' ',CL30'CEDAR RAPIDS-WATERLOO'                       
         DC    AL2(0238),C' ',CL30'ST. JOSEPH'                                  
         DC    AL2(0239),C' ',CL30'JACKSON,TN'                                  
         DC    AL2(0240),C' ',CL30'MEMPHIS'                                     
         DC    AL2(0241),C' ',CL30'SAN ANTONIO'                                 
         DC    AL2(0242),C' ',CL30'LAFAYETTE,LA'                                
         DC    AL2(0243),C' ',CL30'LAKE CHARLES'                                
         DC    AL2(0244),C' ',CL30'ALEXANDRIA,LA'                               
         DC    AL2(0246),C' ',CL30'ANNISTON'                                    
         DC    AL2(0247),C' ',CL30'GREENWOOD'                                   
         DC    AL2(0248),C' ',CL30'CHMPAGN-SPRNGFLD-DECATUR'                    
         DC    AL2(0249),C' ',CL30'EVANSVILLE'                                  
         DC    AL2(0250),C' ',CL30'OKLAHOMA CITY'                               
         DC    AL2(0251),C' ',CL30'LUBBOCK'                                     
         DC    AL2(0252),C' ',CL30'OMAHA'                                       
         DC    AL2(0253),C' ',CL30'SPRNFLD-DECATUR(METRO)'                      
         DC    AL2(0254),C' ',CL30'HASTINGS-KEARNEY(METRO)'                     
         DC    AL2(0255),C' ',CL30'DUBUQUE'                                     
         DC    AL2(0256),C' ',CL30'PANAMA CITY'                                 
         DC    AL2(0257),C' ',CL30'ADA-ARDMORE'                                 
         DC    AL2(0258),C' ',CL30'GREEN BAY'                                   
         DC    AL2(0259),C' ',CL30'NASHVILLE'                                   
         DC    AL2(0260),C' ',CL30'ANNISTON'                                    
         DC    AL2(0261),C' ',CL30'SAN ANGELO'                                  
         DC    AL2(0262),C' ',CL30'ABILENE-SWEETWATER'                          
         DC    AL2(0269),C' ',CL30'MADISON'                                     
         DC    AL2(0270),C' ',CL30'FT. SMITH'                                   
         DC    AL2(0271),C' ',CL30'TULSA'                                       
         DC    AL2(0273),C' ',CL30'COLUMBUS-TUPELO'                             
         DC    AL2(0275),C' ',CL30'PEORIA'                                      
         DC    AL2(0276),C' ',CL30'DULUTH-SUPERIOR'                             
         DC    AL2(0278),C' ',CL30'WICHITA-HUTCHINSON'                          
         DC    AL2(0279),C' ',CL30'DES MOINES AMES'                             
         DC    AL2(0282),C' ',CL30'DAVENPORT-R.ISLAND-MOLINE'                   
         DC    AL2(0286),C' ',CL30'MOBILE-PENSACOLA'                            
         DC    AL2(0287),C' ',CL30'MINOT-BSMRK-DICKINSON'                       
         DC    AL2(0288),C' ',CL30'MINOT-BISMARK'                               
         DC    AL2(0290),C' ',CL30'GREAT BEND'                                  
         DC    AL2(0291),C' ',CL30'HUNTSVILLE-DECATUR'                          
         DC    AL2(0292),C' ',CL30'BEAUMONT-PORT ARTHUR'                        
         DC    AL2(0293),C' ',CL30'LITTLE ROCK-PINE BLUFF'                      
         DC    AL2(0297),C' ',CL30'ALEXANDRIA,MN'                               
         DC    AL2(0298),C' ',CL30'MONTGOMERY'                                  
         DC    AL2(0299),C' ',CL30'WICHITA'                                     
         DC    AL2(0302),C' ',CL30'LA CROSSE-EAU CLAIR'                         
         DC    AL2(0303),C' ',CL30'EAU CLAIRE'                                  
         DC    AL2(0304),C' ',CL30'LA CROSSE'                                   
         DC    AL2(0305),C' ',CL30'WAUSAU'                                      
         DC    AL2(0306),C' ',CL30'RHINELANDER'                                 
         DC    AL2(0309),C' ',CL30'TYLER'                                       
         DC    AL2(0310),C' ',CL30'HATTIESBURG-LAUREL'                          
         DC    AL2(0311),C' ',CL30'MERIDIAN'                                    
         DC    AL2(0316),C' ',CL30'BATON ROUGE'                                 
         DC    AL2(0317),C' ',CL30'QUINCY-HANNIBAL-KEOKUK'                      
         DC    AL2(0318),C' ',CL30'JACKSON, MS'                                 
         DC    AL2(0322),C' ',CL30'LINCOLN-HSTINGS-KEARNEY'                     
         DC    AL2(0324),C' ',CL30'FARGO-VALLEY CITY'                           
         DC    AL2(0325),C' ',CL30'SIOUX FALLS'                                 
         DC    AL2(0333),C' ',CL30'FLORENCE,AL'                                 
         DC    AL2(0334),C' ',CL30'JONESBORO'                                   
         DC    AL2(0336),C' ',CL30'BOWLING GREEN'                               
         DC    AL2(0337),C' ',CL30'MANKATO'                                     
         DC    AL2(0338),C' ',CL30'BOWLING GREEN'                               
         DC    AL2(0339),C' ',CL30'FT. DODGE'                                   
         DC    AL2(0340),C' ',CL30'N.PLATTE-HAYES-MC COOK'                      
         DC    AL2(0341),C' ',CL30'HAYS-GOODLAND'                               
         DC    AL2(0342),C' ',CL30'ENSIGN-GARDEN CITY'                          
         DC    AL2(0343),C' ',CL30'ANCHORAGE'                                   
         DC    AL2(0344),C' ',CL30'HONOLULU'                                    
         DC    AL2(0345),C' ',CL30'FAIRBANKS'                                   
         DC    AL2(0346),C' ',CL30'BILOXI'                                      
         DC    AL2(0349),C' ',CL30'LAREDO'                                      
         DC    AL2(0350),C' ',CL30'FLAGSTAFF'                                   
         DC    AL2(0351),C' ',CL30'DENVER'                                      
         DC    AL2(0352),C' ',CL30'COLORADO SPRINGS-PUEBLO'                     
         DC    AL2(0353),C' ',CL30'PHOENIX'                                     
         DC    AL2(0354),C' ',CL30'BUTTE'                                       
         DC    AL2(0355),C' ',CL30'GREAT FALLS'                                 
         DC    AL2(0356),C' ',CL30'BILLINGS'                                    
         DC    AL2(0357),C' ',CL30'BOISE'                                       
         DC    AL2(0358),C' ',CL30'IDAHO FALLS-POCATELLO'                       
         DC    AL2(0359),C' ',CL30'CHYENN-SCOTTSBLUF-STERLNG'                   
         DC    AL2(0360),C' ',CL30'TWIN FALLS'                                  
         DC    AL2(0361),C' ',CL30'ROSWELL'                                     
         DC    AL2(0362),C' ',CL30'MISSOULA'                                    
         DC    AL2(0363),C' ',CL30'FLAGSTAFF'                                   
         DC    AL2(0364),C' ',CL30'RAPID CITY'                                  
         DC    AL2(0365),C' ',CL30'EL PASO'                                     
         DC    AL2(0366),C' ',CL30'HELENA'                                      
         DC    AL2(0367),C' ',CL30'CASPER-RIVERTON'                             
         DC    AL2(0370),C' ',CL30'SALT LAKE CITY'                              
         DC    AL2(0371),C' ',CL30'YUMA-EL CENTRO'                              
         DC    AL2(0373),C' ',CL30'GRAND JUNCTION-MONTROSE'                     
         DC    AL2(0387),C' ',CL30'DICKINSON'                                   
         DC    AL2(0389),C' ',CL30'TUCSON'                                      
         DC    AL2(0390),C' ',CL30'ALBUQUERQUE'                                 
         DC    AL2(0391),C' ',CL30'FARMINGTON'                                  
         DC    AL2(0392),C' ',CL30'FARMINGTON'                                  
         DC    AL2(0398),C' ',CL30'GLENDIVE'                                    
         DC    AL2(0400),C' ',CL30'BAKERSFIELD'                                 
         DC    AL2(0401),C' ',CL30'EUGENE'                                      
         DC    AL2(0402),C' ',CL30'EUREKA'                                      
         DC    AL2(0403),C' ',CL30'LOS ANGELES'                                 
         DC    AL2(0404),C' ',CL30'PALM SPRINGS'                                
         DC    AL2(0405),C' ',CL30'PALM SPRINGS'                                
         DC    AL2(0406),C' ',CL30'SANTA ROSA, CA.'                             
         DC    AL2(0407),C' ',CL30'SAN FRANCISCO-OAKLAND'                       
         DC    AL2(0410),C' ',CL30'YAKIMA'                                      
         DC    AL2(0411),C' ',CL30'RENO'                                        
         DC    AL2(0413),C' ',CL30'MEDFORD-KLAMATH-FALLS'                       
         DC    AL2(0419),C' ',CL30'SEATTLE-TACOMA'                              
         DC    AL2(0420),C' ',CL30'PORTLAND,OR'                                 
         DC    AL2(0421),C' ',CL30'BEND,OR'                                     
         DC    AL2(0425),C' ',CL30'SAN DIEGO'                                   
         DC    AL2(0428),C' ',CL30'MONTEREY-SALINAS'                            
         DC    AL2(0439),C' ',CL30'LAS VEGAS'                                   
         DC    AL2(0455),C' ',CL30'SANTA BARBARA-SANTA MARIA'                   
         DC    AL2(0462),C' ',CL30'SACRAMENTO-STOCKTON'                         
         DC    AL2(0466),C' ',CL30'FRESNO(VISALIA)'                             
         DC    AL2(0468),C' ',CL30'CHICO-REDDING'                               
         DC    AL2(0481),C' ',CL30'SPOKANE'                                     
         DC    AL2(0498),C' ',CL30'BELLINGHAM'                                  
NSIMKTSX DC    AL2(0)                                                           
         EJECT                                                                  
ARBMKTS  DC    AL2(0003),C' ',CL30'BOSTON'                                      
         DC    AL2(0009),C' ',CL30'NEW YORK'                                    
         DC    AL2(0011),C' ',CL30'PHILADELPHIA'                                
         DC    AL2(0013),C' ',CL30'LOS ANGELES'                                 
         DC    AL2(0015),C' ',CL30'SAN DIEGO'                                   
         DC    AL2(0017),C' ',CL30'SANTA BARBARA-SANTA MARIA'                   
         DC    AL2(0019),C' ',CL30'WASHINGTON,DC'                               
         DC    AL2(0021),C' ',CL30'BALTIMORE'                                   
         DC    AL2(0023),C' ',CL30'SALISBURY'                                   
         DC    AL2(0025),C' ',CL30'HARTFORD-NEW HAVEN'                          
         DC    AL2(0029),C' ',CL30'PITTSBURGH'                                  
         DC    AL2(0031),C' ',CL30'YOUNGSTOWN'                                  
         DC    AL2(0033),C' ',CL30'JOHNSTOWN-ALTOONA'                           
         DC    AL2(0035),C' ',CL30'CLEVELAND'                                   
         DC    AL2(0037),C' ',CL30'AKRON'                                       
         DC    AL2(0039),C' ',CL30'EL CENTRO-YUMA'                              
         DC    AL2(0041),C' ',CL30'MANCHESTER'                                  
         DC    AL2(0043),C' ',CL30'HRRSBRG-YORK-LANCSTR-LEBANON'                
         DC    AL2(0045),C' ',CL30'SPRINGFIELD,MA'                              
         DC    AL2(0047),C' ',CL30'PROVIDENCE-NEW BEDFORD'                      
         DC    AL2(0051),C' ',CL30'CHICAGO'                                     
         DC    AL2(0053),C' ',CL30'SOUTH BEND-ELKHART'                          
         DC    AL2(0055),C' ',CL30'TOLEDO'                                      
         DC    AL2(0057),C' ',CL30'DETROIT'                                     
         DC    AL2(0059),C' ',CL30'GRAND RAPIDS-KALAMAZOO'                      
         DC    AL2(0061),C' ',CL30'LANSING'                                     
         DC    AL2(0063),C' ',CL30'FLINT-SAGINAW-BAY CITY'                      
         DC    AL2(0065),C' ',CL30'SAN FRANCISCO'                               
         DC    AL2(0067),C' ',CL30'SACRAMENTO-STOCKTON'                         
         DC    AL2(0069),C' ',CL30'SALINAS-MONTEREY'                            
         DC    AL2(0071),C' ',CL30'FRESNO - VISALIA'                            
         DC    AL2(0073),C' ',CL30'BAKERSFIELD'                                 
         DC    AL2(0075),C' ',CL30'ST. LOUIS'                                   
         DC    AL2(0077),C' ',CL30'SPRNGFLD-DECATUR-CHMPAIGN'                   
         DC    AL2(0083),C' ',CL30'INDIANAPOLIS'                                
         DC    AL2(0085),C' ',CL30'LAFAYETTE,IN'                                
         DC    AL2(0087),C' ',CL30'TERRE HAUTE'                                 
         DC    AL2(0089),C' ',CL30'CHICO-REDDING'                               
         DC    AL2(0091),C' ',CL30'FT. WAYNE'                                   
         DC    AL2(0093),C' ',CL30'CINCINNATI'                                  
         DC    AL2(0095),C' ',CL30'DAYTON'                                      
         DC    AL2(0101),C' ',CL30'LIMA'                                        
         DC    AL2(0103),C' ',CL30'WHEELING-STEUBENVILLE'                       
         DC    AL2(0105),C' ',CL30'SEATTLE-TACOMA'                              
         DC    AL2(0107),C' ',CL30'MINNEAPOLIS-ST PAUL'                         
         DC    AL2(0109),C' ',CL30'DALLAS-FT WORTH'                             
         DC    AL2(0111),C' ',CL30'MILWAUKEE'                                   
         DC    AL2(0113),C' ',CL30'MADISON'                                     
         DC    AL2(0115),C' ',CL30'WAUSAU-RHINELANDER'                          
         DC    AL2(0117),C' ',CL30'LA CROSS-EAU CLAIRE'                         
         DC    AL2(0119),C' ',CL30'ROCKFORD'                                    
         DC    AL2(0121),C' ',CL30'COLUMBUS,OH'                                 
         DC    AL2(0123),C' ',CL30'PORTLAND -POLAND SPRING'                     
         DC    AL2(0125),C' ',CL30'ZANESVILLE'                                  
         DC    AL2(0126),C' ',CL30'UTICA'                                       
         DC    AL2(0127),C' ',CL30'MIAMI'                                       
         DC    AL2(0129),C' ',CL30'WEST PALM BEACH'                             
         DC    AL2(0131),C' ',CL30'TAMPA-ST. PETERSBURG'                        
         DC    AL2(0133),C' ',CL30'FT. MYERS'                                   
         DC    AL2(0135),C' ',CL30'BUFFALO'                                     
         DC    AL2(0139),C' ',CL30'ROCHESTER,NY'                                
         DC    AL2(0140),C' ',CL30'ELMIRA'                                      
         DC    AL2(0141),C' ',CL30'SYRACUSE'                                    
         DC    AL2(0142),C' ',CL30'SYRACUSE-ELMIRA'                             
         DC    AL2(0143),C' ',CL30'WILKES BARRE-SCRANTON'                       
         DC    AL2(0145),C' ',CL30'BINGHAMTON'                                  
         DC    AL2(0147),C' ',CL30'ERIE'                                        
         DC    AL2(0149),C' ',CL30'ALBANY-SCHNCTDY-TROY'                        
         DC    AL2(0151),C' ',CL30'BURLINGTON-PLATTSBURG'                       
         DC    AL2(0153),C' ',CL30'WATERTOWN-CARTHAGE'                          
         DC    AL2(0155),C' ',CL30'UTICA'                                       
         DC    AL2(0157),C' ',CL30'KANSAS CITY'                                 
         DC    AL2(0159),C' ',CL30'ST. JOSEPH'                                  
         DC    AL2(0161),C' ',CL30'PRESQUE ISLE'                                
         DC    AL2(0163),C' ',CL30'BILLINGHAM'                                  
         DC    AL2(0165),C' ',CL30'RCHSTR-MASON CITY-AUSTIN'                    
         DC    AL2(0173),C' ',CL30'CEDAR RAPIDS-WATERLOO'                       
         DC    AL2(0175),C' ',CL30'PEORIA'                                      
         DC    AL2(0177),C' ',CL30'DAVENPORT-ROCK IS-MOLINE'                    
         DC    AL2(0179),C' ',CL30'MEMPHIS'                                     
         DC    AL2(0181),C' ',CL30'NASHVILLE'                                   
         DC    AL2(0183),C' ',CL30'JACKSON,TN'                                  
         DC    AL2(0185),C' ',CL30'HNTSVL-DECATUR-FLRENCE'                      
         DC    AL2(0187),C' ',CL30'PDUCAH-CP GIRARDEAU-HRRSBRG'                 
         DC    AL2(0195),C' ',CL30'BOWLING GREEN'                               
         DC    AL2(0197),C' ',CL30'ATLANTA'                                     
         DC    AL2(0199),C' ',CL30'CHATTANOOGA'                                 
         DC    AL2(0201),C' ',CL30'HOUSTON'                                     
         DC    AL2(0203),C' ',CL30'AUSTIN,TX'                                   
         DC    AL2(0205),C' ',CL30'WACO-TEMPLE'                                 
         DC    AL2(0207),C' ',CL30'EVANSVILLE'                                  
         DC    AL2(0209),C' ',CL30'LOUISVILLE'                                  
         DC    AL2(0211),C' ',CL30'LEXINGTON'                                   
         DC    AL2(0213),C' ',CL30'GRNVL-SPARTNBRG-ASHEVILLE'                   
         DC    AL2(0215),C' ',CL30'KNOXVILLE'                                   
         DC    AL2(0217),C' ',CL30'BRISTOL-KNGSPRT-JOHNSON CITY'                
         DC    AL2(0219),C' ',CL30'MACON'                                       
         DC    AL2(0221),C' ',CL30'BIRMINGHAM'                                  
         DC    AL2(0225),C' ',CL30'SELMA'                                       
         DC    AL2(0227),C' ',CL30'QUINCY-HANNIBAL'                             
         DC    AL2(0229),C' ',CL30'COLUMBIA-JEFFERSON CITY'                     
         DC    AL2(0231),C' ',CL30'TUSCALUSA'                                   
         DC    AL2(0233),C' ',CL30'PORTLAND,OR'                                 
         DC    AL2(0235),C' ',CL30'EUGENE'                                      
         DC    AL2(0237),C' ',CL30'MEDFORD'                                     
         DC    AL2(0241),C' ',CL30'DENVER'                                      
         DC    AL2(0243),C' ',CL30'COLORADO SPRNGS-PUEBLO'                      
         DC    AL2(0245),C' ',CL30'NEW ORLEANS'                                 
         DC    AL2(0247),C' ',CL30'BEAUMONT-PORT ARTHUR'                        
         DC    AL2(0249),C' ',CL30'BATON ROUGE'                                 
         DC    AL2(0251),C' ',CL30'LAKE CHARLES'                                
         DC    AL2(0253),C' ',CL30'LAFAYETTE,LA'                                
         DC    AL2(0255),C' ',CL30'ALEXANDRIA,LA'                               
         DC    AL2(0257),C' ',CL30'CHARLESTON-HUNTINGTON'                       
         DC    AL2(0259),C' ',CL30'PARKERSBURG'                                 
         DC    AL2(0261),C' ',CL30'CLARKSBURG-WESTON'                           
         DC    AL2(0263),C' ',CL30'OKLAHOMA CITY'                               
         DC    AL2(0265),C' ',CL30'ARDMORE-ADA'                                 
         DC    AL2(0269),C' ',CL30'TULSA'                                       
         DC    AL2(0271),C' ',CL30'SAN ANTONIO'                                 
         DC    AL2(0273),C' ',CL30'LAREDO'                                      
         DC    AL2(0275),C' ',CL30'PHOENIX'                                     
         DC    AL2(0277),C' ',CL30'TUCSON'                                      
         DC    AL2(0279),C' ',CL30'CHARLOTTE'                                   
         DC    AL2(0281),C' ',CL30'GRNSBRO-WINST SALEM-HI PNT'                  
         DC    AL2(0283),C' ',CL30'NRFLK-PRTSMTH-NWPRT-HAMPTN'                  
         DC    AL2(0285),C' ',CL30'RICHMOND'                                    
         DC    AL2(0287),C' ',CL30'HARRISONBURG'                                
         DC    AL2(0291),C' ',CL30'SALT LAKE CITY'                              
         DC    AL2(0293),C' ',CL30'TWIN FALLS'                                  
         DC    AL2(0295),C' ',CL30'IDAHO FALLS-POCATELLO'                       
         DC    AL2(0297),C' ',CL30'HELENA'                                      
         DC    AL2(0299),C' ',CL30'GREAT FALLS'                                 
         DC    AL2(0301),C' ',CL30'OMAHA'                                       
         DC    AL2(0303),C' ',CL30'DES MOINES'                                  
         DC    AL2(0305),C' ',CL30'OTTUMWA-KIRKSVILLE'                          
         DC    AL2(0307),C' ',CL30'WICHITA-HUTCHINSON'                          
         DC    AL2(0313),C' ',CL30'TOPEKA'                                      
         DC    AL2(0315),C' ',CL30'GREEN BAY'                                   
         DC    AL2(0317),C' ',CL30'MARQUETTE'                                   
         DC    AL2(0319),C' ',CL30'LITTLE ROCK'                                 
         DC    AL2(0321),C' ',CL30'SHRVPRT-TEXARKANA'                           
         DC    AL2(0323),C' ',CL30'TYLER'                                       
         DC    AL2(0325),C' ',CL30'FT. SMITH'                                   
         DC    AL2(0327),C' ',CL30'MONROE-EL DORADO'                            
         DC    AL2(0329),C' ',CL30'ORLANDO-DAYTONA BEACH'                       
         DC    AL2(0331),C' ',CL30'LINCOLN-HASTINGS KEARNEY'                    
         DC    AL2(0335),C' ',CL30'JACKSONVILLE'                                
         DC    AL2(0337),C' ',CL30'SPOKANE'                                     
         DC    AL2(0339),C' ',CL30'YAKIMA'                                      
         DC    AL2(0342),C' ',CL30'MISSOULA'                                    
         DC    AL2(0345),C' ',CL30'ROANOKE-LYNCHBURG'                           
         DC    AL2(0347),C' ',CL30'BLUEFLD-BECKLEY-OAK HILL'                    
         DC    AL2(0351),C' ',CL30'RALEIGH-DURHAM'                              
         DC    AL2(0353),C' ',CL30'GRNVL-NEW BERN-WASHINGTON'                   
         DC    AL2(0355),C' ',CL30'WILMINGTON'                                  
         DC    AL2(0357),C' ',CL30'BANGOR'                                      
         DC    AL2(0359),C' ',CL30'FLORENCE,SC'                                 
         DC    AL2(0361),C' ',CL30'COLUMBIA,SC'                                 
         DC    AL2(0363),C' ',CL30'BILOXI-GLFPRT-PASCAGOULA'                    
         DC    AL2(0367),C' ',CL30'ALBUQUERQUE'                                 
         DC    AL2(0369),C' ',CL30'ROSWELL'                                     
         DC    AL2(0371),C' ',CL30'EL PASO'                                     
         DC    AL2(0373),C' ',CL30'JACKSON,MS'                                  
         DC    AL2(0375),C' ',CL30'GREENWOOD-GREENVILLE'                        
         DC    AL2(0377),C' ',CL30'MERIDIAN'                                    
         DC    AL2(0379),C' ',CL30'LAUREL-HATTIESBURG'                          
         DC    AL2(0381),C' ',CL30'DULUTH-SUPERIOR'                             
         DC    AL2(0383),C' ',CL30'MOBILE-PENSACOLA'                            
         DC    AL2(0385),C' ',CL30'NORTH PLATTE'                                
         DC    AL2(0389),C' ',CL30'SIOUX FALLS-MITCHELL'                        
         DC    AL2(0391),C' ',CL30'SIOUX CITY'                                  
         DC    AL2(0393),C' ',CL30'FARGO'                                       
         DC    AL2(0395),C' ',CL30'ALEXANDRIA,MN'                               
         DC    AL2(0397),C' ',CL30'FT. DODGE'                                   
         DC    AL2(0403),C' ',CL30'AMARILLO'                                    
         DC    AL2(0405),C' ',CL30'WICHITA FALLS-LAWTON'                        
         DC    AL2(0409),C' ',CL30'COLUMBUS,GA'                                 
         DC    AL2(0411),C' ',CL30'MONTGOMERY'                                  
         DC    AL2(0412),C' ',CL30'MONTGOMERY-SELMA'                            
         DC    AL2(0413),C' ',CL30'TALLAHASEE'                                  
         DC    AL2(0415),C' ',CL30'DOTHAN'                                      
         DC    AL2(0417),C' ',CL30'PANAMA CITY'                                 
         DC    AL2(0419),C' ',CL30'ALBANY, GA'                                  
         DC    AL2(0421),C' ',CL30'AUGUSTA'                                     
         DC    AL2(0423),C' ',CL30'CHARLESTON,SC'                               
         DC    AL2(0425),C' ',CL30'SAVANNAH'                                    
         DC    AL2(0427),C' ',CL30'SPRINFIELD,MO'                               
         DC    AL2(0429),C' ',CL30'JOPLIN-PITTSBURG'                            
         DC    AL2(0431),C' ',CL30'JONESBORO'                                   
         DC    AL2(0433),C' ',CL30'CORPUS CHRISTI'                              
         DC    AL2(0435),C' ',CL30'MCALLEN-BROWNSVILLE'                         
         DC    AL2(0437),C' ',CL30'LUBBOCK'                                     
         DC    AL2(0439),C' ',CL30'ODESSA-MIDLAND'                              
         DC    AL2(0441),C' ',CL30'ABILENE-SWEETWATER'                          
         DC    AL2(0443),C' ',CL30'SAN ANGELO'                                  
         DC    AL2(0445),C' ',CL30'BOISE'                                       
         DC    AL2(0448),C' ',CL30'COLUMBUS-TUPELO'                             
         DC    AL2(0449),C' ',CL30'MANKATO'                                     
         DC    AL2(0451),C' ',CL30'TRAVERSE CITY CADILLAC'                      
         DC    AL2(0452),C' ',CL30'BIRMINGHAM'                                  
         DC    AL2(0455),C' ',CL30'LAS VEGAS'                                   
         DC    AL2(0457),C' ',CL30'BILLINGS'                                    
         DC    AL2(0459),C' ',CL30'RENO'                                        
         DC    AL2(0460),C' ',CL30'GLENDIVE'                                    
         DC    AL2(0462),C' ',CL30'MINOT-BSMRK-DICKINSON'                       
         DC    AL2(0465),C' ',CL30'CHEYENNE'                                    
         DC    AL2(0467),C' ',CL30'EUREKA'                                      
         DC    AL2(0469),C' ',CL30'RAPID CITY'                                  
         DC    AL2(0471),C' ',CL30'CASPER-RIVERTON'                             
         DC    AL2(0473),C' ',CL30'GRAND JUNCTION'                              
         DC    AL2(0481),C' ',CL30'FT PIERCE-VERO BEACH'                        
         DC    AL2(0505),C' ',CL30'ANDERSON'                                    
         DC    AL2(0509),C' ',CL30'CANTON'                                      
         DC    AL2(0511),C' ',CL30'MODESTO'                                     
         DC    AL2(0513),C' ',CL30'VICTORIA'                                    
         DC    AL2(0515),C' ',CL30'WILDWOOD'                                    
         DC    AL2(0517),C' ',CL30'WORCESTER'                                   
         DC    AL2(0569),C' ',CL30'FT LAUDERDALE'                               
         DC    AL2(0577),C' ',CL30'PALM SPRINGS'                                
         DC    AL2(0589),C' ',CL30'HICKORY'                                     
         DC    AL2(0591),C' ',CL30'BEND'                                        
         DC    AL2(0593),C' ',CL30'HANOVER'                                     
         DC    AL2(0601),C' ',CL30'HAGERSTOWN'                                  
         DC    AL2(0603),C' ',CL30'ANNISTON'                                    
         DC    AL2(0611),C' ',CL30'HAZARD'                                      
         DC    AL2(0613),C' ',CL30'BUTTE'                                       
         DC    AL2(0615),C' ',CL30'ALLENTOWN'                                   
         DC    AL2(0617),C' ',CL30'DUBUQUE'                                     
         DC    AL2(0619),C' ',CL30'DUBUQUE'                                     
         DC    AL2(0621),C' ',CL30'GAINESVILLE'                                 
         DC    AL2(0624),C' ',CL30'MILES CITY-GLENDIVE'                         
         DC    AL2(0625),C' ',CL30'FLAGSTAFF'                                   
         DC    AL2(0627),C' ',CL30'ALPENA'                                      
         DC    AL2(0631),C' ',CL30'HANFORD'                                     
         DC    AL2(0639),C' ',CL30'TULARE'                                      
         DC    AL2(0643),C' ',CL30'BATTLE CREEK'                                
         DC    AL2(0645),C' ',CL30'SARASOTA'                                    
         DC    AL2(0649),C' ',CL30'FARMINGTON'                                  
         DC    AL2(0651),C' ',CL30'CHARLOTTESVILLE'                             
         DC    AL2(0653),C' ',CL30'FREDERICKSBURG'                              
         DC    AL2(0655),C' ',CL30'ANCHORAGE'                                   
         DC    AL2(0659),C' ',CL30'CHARLTVLLE-HARRSNBRG'                        
         DC    AL2(0701),C'B',CL30'ALBANY, GA. BLACK'                           
         DC    AL2(0702),C'H',CL30'ALBUQUERQUE HISPANIC'                        
         DC    AL2(0703),C'B',CL30'ATLANTA BLACK'                               
         DC    AL2(0704),C'B',CL30'AUGUSTA BLACK'                               
         DC    AL2(0705),C'B',CL30'BALTIMORE BLACK'                             
         DC    AL2(0706),C'B',CL30'BATON ROUGE BLACK'                           
         DC    AL2(0708),C'B',CL30'BIRMINGHAM BLACK'                            
         DC    AL2(0709),C'B',CL30'CHARLESTON, SC BLACK'                        
         DC    AL2(0710),C'B',CL30'CHARLOTTE BLACK'                             
         DC    AL2(0711),C'B',CL30'CHICAGO BLACK'                               
         DC    AL2(0712),C'B',CL30'CINCINNATI BLACK'                            
         DC    AL2(0713),C'B',CL30'CLEVELAND BLACK'                             
         DC    AL2(0714),C'B',CL30'COLUMBIA, SC. BLACK'                         
         DC    AL2(0716),C'B',CL30'COLUMBUS, GA. BLACK'                         
         DC    AL2(0717),C'H',CL30'CORPUS CHRISTI HISPANIC'                     
         DC    AL2(0718),C'B',CL30'DALLAS-FT. WORTH BLACK'                      
         DC    AL2(0719),C'B',CL30'DETROIT BLACK'                               
         DC    AL2(0720),C'B',CL30'EL CENTRO-YUMA BLACK'                        
         DC    AL2(0721),C'H',CL30'EL CENTRO-YUMA HISPANIC'                     
         DC    AL2(0722),C'H',CL30'EL PASO HISPANIC'                            
         DC    AL2(0723),C'B',CL30'FLORENCE, SC BLACK'                          
         DC    AL2(0724),C'H',CL30'FRESNO-VISALIA HISPANIC'                     
         DC    AL2(0725),C'B',CL30'GRNWOOD-GRNVILLE BLACK'                      
         DC    AL2(0726),C'B',CL30'GRNVLLE-N.BERN-WASH. BLACK'                  
         DC    AL2(0727),C'B',CL30'HOUSTON BLACK'                               
         DC    AL2(0728),C'B',CL30'JACKSON, MS BLACK'                           
         DC    AL2(0729),C'B',CL30'JACKSONVILLE BLACK'                          
         DC    AL2(0730),C'B',CL30'LAFAYETTE, LA. BLACK'                        
         DC    AL2(0731),C'H',CL30'LAREDO HISPANIC'                             
         DC    AL2(0732),C'H',CL30'COLORADO SPR.-PUEBLO HISPANIC'               
         DC    AL2(0733),C'B',CL30'LITTLE ROCK BLACK'                           
         DC    AL2(0734),C'B',CL30'LOS ANGELES BLACK'                           
         DC    AL2(0735),C'H',CL30'LOS ANGELES HISPANIC'                        
         DC    AL2(0736),C'B',CL30'MACON BLACK'                                 
         DC    AL2(0737),C'H',CL30'MCALLEN-BRNSVLLE LRGV HISPANIC'              
         DC    AL2(0738),C'B',CL30'MEMPHIS BLACK'                               
         DC    AL2(0739),C'B',CL30'MERIDIAN BLACK'                              
         DC    AL2(0740),C'B',CL30'MIAMI BLACK'                                 
         DC    AL2(0741),C'H',CL30'MIAMI HISPANIC'                              
         DC    AL2(0742),C'B',CL30'MOBILE-PENSACOLA BLACK'                      
         DC    AL2(0743),C'B',CL30'MONROE-EL DORADO BLACK'                      
         DC    AL2(0744),C'B',CL30'MONTGOMERY BLACK'                            
         DC    AL2(0745),C'B',CL30'NEW ORLEANS BLACK'                           
         DC    AL2(0746),C'B',CL30'NEW YORK BLACK'                              
         DC    AL2(0747),C'H',CL30'NEW YORK HISPANIC'                           
         DC    AL2(0748),C'B',CL30'NFLK-PRTSMTH-N.NWS-HAMP BLACK '              
         DC    AL2(0749),C'B',CL30'PHILADELPHIA BLACK'                          
         DC    AL2(0750),C'B',CL30'PITTSBURG BLACK'                             
         DC    AL2(0751),C'B',CL30'RALEIGH-DURHAM BLACK'                        
         DC    AL2(0752),C'B',CL30'RICHMOND BLACK'                              
         DC    AL2(0753),C'B',CL30'ST. LOIUS BLACK'                             
         DC    AL2(0754),C'H',CL30'SALINAS-MONTEREY HISPANIC'                   
         DC    AL2(0755),C'B',CL30'SAN FRANCISCO BLACK'                         
         DC    AL2(0756),C'H',CL30'SAN ANTONIO HISPANIC'                        
         DC    AL2(0761),C'H',CL30'CHICAGO HISPANIC'                            
         DC    AL2(0762),C'H',CL30'DALLAS-FT. WORTH HISPANIC'                   
         DC    AL2(0763),C'B',CL30'WASHINGTON, DC. BLACK'                       
         DC    AL2(0765),C'B',CL30'GRNSBORO-WINSTON SALEM BLACK'                
         DC    AL2(0766),C'B',CL30'NASHVILLE BLACK'                             
         DC    AL2(0768),C'H',CL30'HOUSTON HISPANIC'                            
         DC    AL2(0769),C'H',CL30'AUSTIN, TX HISPANIC'                         
         DC    AL2(0770),C'H',CL30'BAKERSFIELD HISPANIC'                        
         DC    AL2(0772),C'B',CL30'LAUREL-HATTIESBURG BLACK'                    
         DC    AL2(0775),C'B',CL30'DAYTON BLACK'                                
         DC    AL2(0782),C'B',CL30'INDIANAPOLIS BLACK'                          
         DC    AL2(0785),C'B',CL30'LOUISVILLE BLACK'                            
         DC    AL2(0786),C'H',CL30'LUBBOCK HISPANIC'                            
         DC    AL2(0791),C'H',CL30'PHOENIX HISPANIC'                            
         DC    AL2(0793),C'H',CL30'SACRAMENTO-STOCKTON HISPANIC'                
         DC    AL2(0794),C'H',CL30'SAN DIEGO HISPANIC'                          
         DC    AL2(0801),C'M',CL30'EVANSVILLE METRO'                            
         DC    AL2(0802),C'T',CL30'SALISBURY DBM TA'                            
         DC    AL2(0803),C'T',CL30'HARTFORD-HANOVER TA'                         
         DC    AL2(0804),C'T',CL30'SANTA ROSA TA'                               
         DC    AL2(0805),C'T',CL30'ALLENTOWN TA'                                
         DC    AL2(0806),C'T',CL30'OKLAHOMA CITY TRADING AREA'                  
         DC    AL2(0807),C'T',CL30'COLORADO SPRINGS TRADING AREA'               
         DC    AL2(0808),C'T',CL30'FAYETTVLLE-SPRGDALE-ROGERS TA'               
         DC    AL2(0809),C'T',CL30'MEMPHIS TA'                                  
         DC    AL2(0810),C'T',CL30'MEDFORD/EUGENE TRADING AREA'                 
         DC    AL2(0811),C'T',CL30'AUGUSTA TA'                                  
         DC    AL2(0812),C'T',CL30'FAYETTEVILLE, NC TA'                         
         DC    AL2(0813),C'M',CL30'BRIDGEFORD METRO'                            
         DC    AL2(0814),C'T',CL30'BATON ROUGE TA'                              
         DC    AL2(0815),C'T',CL30'LONGVIEW-TYLER TA'                           
         DC    AL2(0816),C'T',CL30'PRESCOTT, AZ TA'                             
         DC    AL2(0817),C'T',CL30'SAN JOSE TA'                                 
         DC    AL2(0818),C'M',CL30'WITCHITA-HUTCHINSON METRO'                   
         DC    AL2(0819),C'T',CL30'BIRM-ANNIS-TUSCA TA'                         
         DC    AL2(0820),C'M',CL30'PORTLAND,OR METRO'                           
         DC    AL2(0821),C'T',CL30'BIRMINGHAM TA'                               
         DC    AL2(0822),C'T',CL30'SAN JOSE TRADING AREA'                       
         DC    AL2(0823),C'S',CL30'TMG SCANNER - EVANSVILLE'                    
         DC    AL2(0824),C'S',CL30'TMG SCANNER - PORTLAND, ME'                  
         DC    AL2(0825),C'S',CL30'TMG SCANNER - ORLANDO'                       
         DC    AL2(0829),C'T',CL30'TAMPA-ST. PETE TA'                           
         DC    AL2(0830),C'T',CL30'ALBUQUERQUE EXPANDED TA'                     
         DC    AL2(0831),C'M',CL30'ALBUQUERQUE METRO'                           
         DC    AL2(0832),C'M',CL30'BIRMINGHAM METRO'                            
         DC    AL2(0833),C'T',CL30'MOUNT VERNON TA'                             
         DC    AL2(0834),C'T',CL30'WENATCHEE TA'                                
         DC    AL2(0835),C'T',CL30'VIRGIN ISLAND TA'                            
         DC    AL2(0836),C'T',CL30'WORCESTER,MA TA'                             
         DC    AL2(0837),C'M',CL30'LINCOLN-HAST-KEARNEY METRO 2 '               
         DC    AL2(0838),C'T',CL30'VENTURA COUNTY TA'                           
         DC    AL2(0840),C'T',CL30'MONTGOMERY TA'                               
         DC    AL2(0841),C'T',CL30'MANCHESTER, NH TA'                           
         DC    AL2(0842),C'T',CL30'MONTGOMERY METRO TA'                         
         DC    AL2(0843),C'T',CL30'EASTERN-SE KENTUCKY TAR'                     
         DC    AL2(0846),C'T',CL30'ATLANTA TA'                                  
         DC    AL2(0847),C'T',CL30'WITCHITA-HUTCH,ETC. TA'                      
         DC    AL2(0853),C' ',CL30'PUERTO RICO'                                 
         DC    AL2(0854),C'T',CL30'BINGHAMTON TA'                               
         DC    AL2(0855),C'T',CL30'CONCORD, NH TA'                              
         DC    AL2(0856),C'N',CL30'KANSAS NTWK AFFILIATE REPORT'                
         DC    AL2(0857),C'T',CL30'LUFKIN-NOCAGDOCHES TA'                       
         DC    AL2(0858),C'T',CL30'MISSOULA-BUTTE TA'                           
         DC    AL2(0859),C'T',CL30'MONTGOMERY-SELMA TA'                         
         DC    AL2(0860),C'T',CL30'LONG ISLAND TA'                              
         DC    AL2(0861),C'T',CL30'N.W. WASHINGTON TA'                          
         DC    AL2(0862),C'S',CL30'TMG SCANNER - BOISE'                         
         DC    AL2(0863),C'T',CL30'BIRMINGHAM-TUSCALOOSA TAR'                   
         DC    AL2(0865),C'T',CL30'CHEYENNE-SCOTTS-STER. TAR'                   
         DC    AL2(0866),C'T',CL30'HUDSON VALLEY,NY TA'                         
         DC    AL2(0867),C'T',CL30'SALEM, OR TA'                                
         DC    AL2(0868),C'T',CL30'FAYTTVLL-CUMBERLND-ROBESON TA'               
         DC    AL2(0869),C'T',CL30'ANNISTON TA'                                 
         DC    AL2(0870),C'T',CL30'NEW LONDON TA'                               
         DC    AL2(0871),C'M',CL30'EUGENE-SPRINGFIELD METRO'                    
         DC    AL2(0872),C'T',CL30'SOUTHERN NH TA'                              
         DC    AL2(0877),C'E',CL30'BIRMINGHAM TA+TSA'                           
         DC    AL2(0881),C'T',CL30'MOBILE TA'                                   
ARBMKTSX DC    AL2(0)                                                           
*                                                                               
MARKETSX DC    AL2(0)                                                           
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=32760,MACRF=PM                                           
         LTORG                                                                  
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    100000C                                                          
*                                                                               
RANKTABD DSECT                                                                  
RMKT     DS    CL2                                                              
RNKUNV   DS    CL4                                                              
RNKPCT   DS    CL4                                                              
RANK     DS    CL2                                                              
SMARANK  DS    CL2                                                              
RMKTNAM  DS    CL30                                                             
RNKTABEN DS    0C                                                               
RANKLEN  EQU   RNKTABEN-RMKT                                                    
         SPACE 2                                                                
SPILTABD DSECT                                                                  
SMS      DS    0CL7                                                             
SMKT     DS    CL2                                                              
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SPILEND  DS    0C                                                               
SPILLEN  EQU   SPILEND-SMKT                                                     
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPLR23 05/01/02'                                      
         END                                                                    
