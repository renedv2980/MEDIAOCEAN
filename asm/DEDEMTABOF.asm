*          DATA SET DEDEMTABOF AT LEVEL 041 AS OF 12/04/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00AD2E                                                                  
         TITLE 'OFFLINE LOADABLE TABLES FOR INTERNAL USE BY DEMOS'              
***********************************************************************         
*                                                                               
* THIS ROUTINE CONTAINS VARIOUS TABLES USED BY THE DEMOGRAPHICS                 
* MODULES.  THIS PHASE CAN BE USED ONLY OFFLINE.                                
*                                                                               
* ON INPUT:  P1 = EQUATED VALUE FOR DESIRED TABLE (SEE DEDEMTABD)               
* ON OUTPUT: P1 = A(TABLE), OR NULLS IF P1 IS INVALID                           
*            P2 = L'TABLE ENTRY (FOR TABLES WITH FIXED-LENGTH ENTRIES,          
*                 ELSE NULLS)                                                   
*            P3 = TOTAL TABLE LENGTH                                            
*                                                                               
* NOTE: FOR TABLES WITH FIXED-LENGTH ENTRIES, IT IS ABSOLUTELY                  
*       MANDATORY TO USE THE RETURNED TABLE ENTRY LENGTH IN P2, SO THAT         
*       ANY TABLE CAN BE MADE WIDER IF NECESSARY IN THE FUTURE!!!               
*                                                                               
***********************************************************************         
                                                                                
*----------------------------------------------------------------------         
* MACRO DEFINITIONS                                                             
*----------------------------------------------------------------------         
                                                                                
       ++INCLUDE DFTIDMCN                                                       
                                                                                
                                                                                
         MACRO                                                                  
         SMTBL &KEY                                                             
.*                                                                              
         GBLC  &END                                                             
.*                                                                              
         AIF   (T'&KEY EQ 'O').MISSING                                          
         AIF   (K'&KEY NE 2).BADKEY                                             
         AGO   .OKAY                                                            
.*                                                                              
.MISSING MNOTE 8,'MISSING REQUIRED PARAMETER'                                   
         MEXIT                                                                  
.BADKEY  MNOTE 8,'SOURCE/MEDIA MUST BE OF LENGTH 2'                             
         MEXIT                                                                  
.*                                                                              
.OKAY    ANOP                                                                   
&END     SETC  'X&KEY'                                                          
.*                                                                              
         DC    CL2'&KEY',AL3(&END+2)                                            
.*                                                                              
         MEXIT                                                                  
         MEND                                                                   
                                                                                
                                                                                
         MACRO                                                                  
         MRKT  &MKTNUM,&MKTNM,&BOOKTYPE=                                        
.*                                                                              
         LCLA  &BTYPNUM                                                         
         LCLC  &BTYPE                                                           
.*                                                                              
         AIF   (T'&MKTNUM EQ 'O').MISSING                                       
         AIF   (T'&MKTNM EQ 'O').MISSING                                        
         AIF   (T'&MKTNUM NE 'N').BADMKT#                                       
         AIF   (&MKTNUM EQ 999).NO#999                                          
         AIF   (&MKTNUM GT 9999).TOOHIGH                                        
         AGO   .MKTNUM                                                          
.*                                                                              
.MISSING MNOTE 8,'MISSING REQUIRED PARAMETER'                                   
         MEXIT                                                                  
.BADMKT# MNOTE 8,'MARKET NUMBER MUST BE NUMERIC'                                
         MEXIT                                                                  
.NO#999  MNOTE 8,'MARKET NUMBER CANNOT BE 999 (SEE CTSFM34)'                    
         MEXIT                                                                  
.TOOHIGH MNOTE 8,'MARKET NUMBER CANNOT EXCEED 9999 (SEE CTSFM34)'               
         MEXIT                                                                  
.*                                                                              
.MKTNUM  DC    AL2(&MKTNUM)                                                     
.*                                                                              
.* ASSUME ONE-CHARACTER BOOKTYPE                                                
&BTYPE   SETC  ' '                                                              
         AIF   (T'&BOOKTYPE EQ 'O').BTYPECH                                     
         AIF   (K'&BOOKTYPE EQ 1).SETBTYP                                       
         AIF   (K'&BOOKTYPE GT 2).BADBKTP                                       
.*                                                                              
.* IT'S A TWO-CHARACTER BOOKTYPE. AS NEW TWO-CHARACTER BOOKTYPES ARE            
.* ADDED TO DEDEMTABS (TABLE TBL_SPBOOKTB), THEY NEED TO BE ADDED HERE          
.* AS WELL IF THEY ARE EVER TO BE USED AS BOOKTYPE PARAMETER VALUES             
.* IN THIS MACRO.                                                               
.*                                                                              
&BTYPNUM SETA  178                                                              
         AIF   ('&BOOKTYPE' EQ 'Z1').SETBTYN                                    
.*                                                                              
.BADBKTP MNOTE 8,'INVALID BOOKTYPE'                                             
         MEXIT                                                                  
.*                                                                              
.SETBTYN DC    AL1(&BTYPNUM)                                                    
         AGO   .MRKTNAM                                                         
.*                                                                              
.SETBTYP ANOP                                                                   
&BTYPE   SETC  '&BOOKTYPE'                                                      
.*                                                                              
.BTYPECH DC    CL1'&BTYPE'                                                      
.*                                                                              
.MRKTNAM DC    CL30&MKTNM                                                       
.*                                                                              
         MEXIT                                                                  
         MEND                                                                   
                                                                                
                                                                                
         MACRO                                                                  
         EMTBL                                                                  
         GBLC  &END                                                             
&END     DC    AL2(0)                                                           
         MEXIT                                                                  
         MEND                                                                   
                                                                                
                                                                                
*----- END OF MACRO DEFINITIONS --------------------------------------*         
         EJECT                                                                  
                                                                                
DEMTABOF CSECT                                                                  
         PRINT GEN                                                              
         NMOD1 0,*DEMTABO                                                       
         LR    R9,R1               A(PARAMETER LIST)                            
*                                                                               
         LA    R3,TBLADDRS         TABLE OF TABLE ADDRESSES                     
         USING TBLD,R3                                                          
         CLC   TBLID,0(R9)         MATCH ON TABLEID?                            
         BE    FOUNDTBL                                                         
         AHI   R3,TBLLENQ          BUMP TO NEXT ENTRY                           
         CLI   0(R3),X'FF'                                                      
         BNE   *-18                                                             
         XC    0(4,R9),0(R9)       CLEAR RETURN ADDRESS                         
         B     XIT                 CALLER PASSED BAD TABLEID                    
*                                                                               
FOUNDTBL DS    0H                                                               
         L     R1,TBLADDR          A(TABLE)                                     
*                                                                               
         CLC   TBLEYEC,0(R1)       DOES EYE-CATCHER MATCH?                      
         BE    *+6                 YES                                          
         DC    H'0'                TABLE IS CORRUPTED                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,14(R1)         L'TABLE ENTRY                                
         ST    R0,4(R9)            RETURN IN P2                                 
         SR    R0,R0                                                            
         ICM   R0,15,16(R1)        TOTAL LENGTH OF THE TABLE                    
         ST    R0,8(R9)            RETURN IN P3                                 
         AHI   R1,20               L'EYECATCHER + L'SPARE + L'LENGTH            
         ST    R1,0(R9)            RETURN TABLE ADDRESS                         
         DROP  R3                                                               
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
* TABLE OF TABLES                                                               
*                                                                               
         DS    0D                                                               
TBLADDRS DS    0XL16                                                            
         DC    A(MRKTNAMT),CL8'MRKTNAMT',A(TBL_MRKTNAMT)                        
         DC    A(NFORMTAB),CL8'NFORMTAB',A(TBL_NFORMTAB)                        
         DC    X'FF'                                                            
         SPACE 3                                                                
TBLD     DSECT                                                                  
TBLID    DS    A                   TABLEID (FROM DEDEMTABD)                     
TBLEYEC  DS    CL8                 EYE-CATCHER (MUST MATCH EYE-CATCHER          
*                                   IN THE ACTUAL TABLE!!!)                     
TBLADDR  DS    A                   A(TABLE)                                     
TBLLENQ  EQU   *-TBLD                                                           
*                                                                               
DEMTABOF CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
* THE STRUCTURE OF EACH TABLE MUST BE:                                          
* 1:   DS 0D (FORCE DOUBLEWORD ALIGNMENT)                                       
* 2:   8-BYTE EYE-CATCHER (MUST DUPLICATE THIS IN TBLADDRS ABOVE!)              
* 3:   6-BYTE SPARE                                                             
* 4:   2-BYTE L'TABLE ENTRY (IF ENTRIES ARE FIXED LENGTH, ELSE NULLS)           
* 5:   4-BYTE TOTAL TABLE LENGTH                                                
* 6:   TABLE ENTRIES                                                            
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
                                                                                
* TABLE OF MARKET NAMES                                                         
TBL_MRKTNAMT DS 0D                                                              
* (FROM PAN BOOK DELDCREC)                                                      
         DC    CL8'MRKTNAMT'                                                    
         DS    XL6                                                              
*                                                                               
         DC    AL2(MKNTABL)                L'ENTRY                              
         DC    AL4(TBL_MRKTNAMTX-TBL_MRKTNAMTS)                                 
*                                                                               
TBL_MRKTNAMTS DS 0X                                                             
* BBMMKTS                                                                       
         SMTBL AC                          BBM CANADIAN TV                      
         MRKT  0001,'FC AUDIENCE'                                               
         MRKT  0002,'NATIONAL'     TOTAL CANADA                                 
         MRKT  0003,'ONTARIO'                                                   
         MRKT  0004,'QUEBEC'                                                    
         MRKT  0009,'ST. JOHNS'                                                 
         MRKT  0041,'CORNER BROOK'                                              
         MRKT  0060,'CD6 GRD FLS'                                               
         MRKT  1021,'CHARLOTTETOWN'                                             
         MRKT  1031,'ATLANTIC'                                                  
         MRKT  2009,'SYDNEY-GLACE BAY'                                          
*961030  MRKT  2010,'CAPE BRETON'                  BECAME SYDNEY CA             
         MRKT  2010,'SYDNEY'                                                    
         MRKT  2079,'HALIFAX'                                                   
         MRKT  2080,'HALIFAX'                                                   
         MRKT  3011,'SAINT JOHN-MONCTON'                                        
         MRKT  3111,'CARLETON'                                                  
         MRKT  4041,'MATANE-SEPT.ISLES'                                         
         MRKT  4061,'RIMOUSKI-MATANE-SEPT.ISLES'                                
         MRKT  4063,'EST DU QUEBEC'                                             
         MRKT  4071,'RIMOUSKI'                                                  
         MRKT  4091,'SEPT ILES EM'                                              
         MRKT  4101,'RIVIERE DU LOUP'                                           
         MRKT  4119,'CHICOUTIMI-JONQUIERE'                                      
         MRKT  4120,'CHICOUTIMI'                                                
         MRKT  4199,'QUEBEC CITY'                                               
         MRKT  4350,'SHERBROOKE CM'                                             
         MRKT  4351,'SHERBROOKE'                                                
         MRKT  4479,'MONTREAL'                                                  
         MRKT  4480,'MTL ANGLO'                                                 
         MRKT  4481,'MTL FRANCO'                                                
         MRKT  4661,'TROIS RIVIERES'                                            
         MRKT  4667,'SHER-TR RIV EM'                                            
         MRKT  4723,'ROUYN-NORANDA'                                             
         MRKT  5069,'OTTAWA-HULL'                                               
         MRKT  5071,'OTT ANGLO'                                                 
         MRKT  5072,'OTT FRANCO'                                                
         MRKT  5073,'MARKET 5073'                                               
         MRKT  5075,'CKNC SPECIAL'                                              
*960104  MRKT  5100,'RENFREW'                                                   
         MRKT  5100,'PEMBROKE'                                                  
         MRKT  5109,'KINGSTON'                                                  
         MRKT  5120,'MARKET 5120'                                               
         MRKT  5145,'EAST CENTRAL ONT.'                                         
         MRKT  5159,'PETERBOROUGH'                                              
         MRKT  5199,'TORONTO'                                                   
         MRKT  5243,'BARRIE'                                                    
         MRKT  5269,'HAMILTON'                                                  
         MRKT  5334,'KITCH/LONDON EXTENED'                                      
         MRKT  5335,'SOUTHWESTERN ONT.'                                         
         MRKT  5336,'KITCH-LONDON'                                              
         MRKT  5339,'KITCHENER'                                                 
         MRKT  5369,'LONDON'                                                    
         MRKT  5409,'WINDSOR'                                                   
         MRKT  5441,'WINGHAM'                                                   
         MRKT  5461,'SUDBURY-TIMMINS-NORTH BAY'                                 
         MRKT  5469,'NORTH BAY'                                                 
         MRKT  5479,'SUDBURY'                                                   
         MRKT  5499,'TIMMINS'                                                   
         MRKT  5510,'SUD/TIM/NB/SS MARIE'                                       
         MRKT  5511,'SUDBURY/SSM'               MAR96                           
         MRKT  5531,'ALGOMA WEST'                                               
         MRKT  5539,'THUNDER BAY'                                               
         MRKT  5562,'KENORA WEST'                                               
         MRKT  5565,'KENORA'                                                    
         MRKT  6061,'BRANDON'                                                   
         MRKT  6119,'WINNIPEG'                                                  
         MRKT  7011,'YORKTON'                                                   
         MRKT  7045,'SWIFT CURRENT'                                             
         MRKT  7071,'REGINA-MOOSE JAW'                                          
         MRKT  7109,'SASKATOON'                                                 
         MRKT  7153,'PRINCE ALBERT'                                             
         MRKT  8010,'MEDICINE HAT'                                              
         MRKT  8019,'LETHBRIDGE CA'                                             
         MRKT  8020,'CD2'                                                       
         MRKT  8069,'CALGARY'                                                   
         MRKT  8078,'RED DEER'                                                  
         MRKT  8080,'RED DEER'                                                  
         MRKT  8091,'LLOYDMINSTER'                                              
         MRKT  8119,'EDMONTON'                                                  
         MRKT  8991,'OLD EDMONTON EM'                                           
         MRKT  9071,'OKANAGAN-KAMLOOPS'                                         
         MRKT  9109,'VANCOUVER'                                                 
         MRKT  9119,'VICTORIA'                                                  
         MRKT  9231,'KELOWNA'                                                   
         MRKT  9301,'TERRACE-KITIMAT'                                           
         MRKT  9331,'PRINCE GEORGE-KAMLOOPS'                                    
         MRKT  9341,'PRINCE GEORGE-TERRACE-KIT.'                                
         MRKT  9350,'PRINCE GEORGE'                                             
         MRKT  9351,'PRINCE GEORGE'                                             
         MRKT  9363,'DAWSON CREEK'                                              
         MRKT  9500,'NEW ENGLAND-1'                                             
         MRKT  9510,'NORTH EAST-1'                                              
         MRKT  9540,'PACIFIC COAST-1'                                           
         MRKT  9575,'CFTM CANCOM'                                               
         MRKT  9580,'CHCH CANCOM'                                               
         MRKT  9585,'CITV CANCOM'                                               
         MRKT  9590,'CHAN CANCOM'                                               
         MRKT  9598,'RADIO CANADA'                                              
         MRKT  9600,'VANCOUVER FC'                                              
         MRKT  9602,'SASK TV NETWORK'                                           
         MRKT  9605,'ONT TV NETWORK'                                            
         MRKT  9610,'NORTH ONT REG TV'                                          
         MRKT  9615,'SUDBURY'                                                   
         MRKT  9710,'BBS SASK. NETWRK RPT'                                      
         MRKT  9751,'E.C. ONT. USA FC'                                          
         MRKT  9760,'MONTREAL USA FC'                                           
         MRKT  9762,'TORONTO USA FC'                                            
         MRKT  9763,'CALGARY USA FC'                                            
         MRKT  9764,'KINGSTON USA FC'                                           
         MRKT  9765,'PETERBORO USA FC'                                          
         MRKT  9802,'NOVA SCOTIA'                                               
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* ARBRMKT                                                                       
         SMTBL AR                         ARB US RADIO                          
         MRKT  0001,'NEW YORK'                                                  
         MRKT  0003,'LOS ANGELES'                                               
         MRKT  0005,'CHICAGO'                                                   
         MRKT  0007,'PHILADELPHIA'                                              
         MRKT  0009,'SAN FRANCISCO'                                             
         MRKT  0011,'DETROIT'                                                   
         MRKT  0013,'BOSTON'                                                    
         MRKT  0015,'WASHINGTON D. C.'                                          
         MRKT  0017,'ST.LOUIS'                                                  
         MRKT  0019,'CLEVELAND'                                                 
         MRKT  0021,'BALTIMORE'                                                 
         MRKT  0023,'PITTSBURGH'                                                
         MRKT  0024,'DALLAS-FT. WORTH'                                          
         MRKT  0027,'MINNEAPOLIS-ST. PAUL'                                      
         MRKT  0029,'MIAMI CSA'                                                 
         MRKT  0031,'CINCINNATI'                                                
         MRKT  0033,'HOUSTON-GALVETON'                                          
         MRKT  0035,'DENVER-BOULDER'                                            
         MRKT  0037,'BUFFALO-NIAGARA FALLS'                                     
         MRKT  0039,'SEATTLE-TACOMA'                                            
         MRKT  0041,'KANSAS CITY'                                               
         MRKT  0043,'MILWAUKEE-RACINE'                                          
         MRKT  0045,'COLUMBUS OH'                                               
         MRKT  0047,'ATLANTA'                                                   
         MRKT  0049,'INDIANAPOLIS'                                              
         MRKT  0051,'PORTLAND, OR'                                              
         MRKT  0053,'NEW ORLEANS'                                               
         MRKT  0055,'LOUISVILLE'                                                
         MRKT  0057,'PHOENIX'                                                   
         MRKT  0059,'SAN ANTONIO'                                               
         MRKT  0061,'HARTFORD-N.BRITAIN-MDLTN'                                  
         MRKT  0062,'NEW HAVEN-MERIDEN'                                         
         MRKT  0063,'SAN DIEGO'                                                 
         MRKT  0065,'SACRAMENTO'                                                
         MRKT  0067,'DAYTON'                                                    
         MRKT  0069,'ALBANY-SCNCTADY-TROY'                                      
         MRKT  0071,'DES MOINES'                                                
         MRKT  0073,'NASHVILLE'                                                 
         MRKT  0075,'MEMPHIS'                                                   
         MRKT  0077,'PRVIDENCE-WARWICK-PAWTUCKET'                               
         MRKT  0079,'ROCHESTER, NY'                                             
         MRKT  0081,'AKRON'                                                     
         MRKT  0082,'CANTON'                                                    
         MRKT  0083,'OKLAHOMA CITY'                                             
         MRKT  0085,'OMAHA-COUNCIL BLUFFS'                                      
         MRKT  0087,'TAMPA-ST. PETE-CLEARWATER'                                 
         MRKT  0089,'FRESNO'                                                    
         MRKT  0091,'SYRACUSE'                                                  
         MRKT  0093,'CHRLT-GASTONIA-ROCK HILL'                                  
         MRKT  0095,'BIRMINGHAM'                                                
         MRKT  0097,'TOLEDO'                                                    
         MRKT  0099,'HONOLULU'                                                  
         MRKT  0101,'SALT LAKE-OGDEN-PROVO'                                     
         MRKT  0103,'TULSA'                                                     
         MRKT  0105,'RICHMOND'                                                  
         MRKT  0107,'JACKSONVILLE'                                              
         MRKT  0109,'NORFOLK-V.BEACH-NWPT NWS'                                  
         MRKT  0111,'SHREVEPORT'                                                
         MRKT  0113,'WORCESTER'                                                 
         MRKT  0115,'RALEIGH-DURHAM'                                            
         MRKT  0117,'SPRINFIELD, MA'                                            
         MRKT  0119,'HARSBRG-LEBANON-CALISLE'                                   
         MRKT  0121,'KNOXVILLE'                                                 
         MRKT  0123,'LITTLE ROCK'                                               
         MRKT  0125,'WICHITA'                                                   
         MRKT  0127,'GRAND RAPIDS'                                              
         MRKT  0129,'YOUNGSTOWN-WARREN'                                         
         MRKT  0131,'ORLANDO'                                                   
         MRKT  0133,'MOBILE'                                                    
         MRKT  0135,'AUSTIN'                                                    
         MRKT  0137,'PEORIA'                                                    
         MRKT  0139,'WILMINGTON, DE.'                                           
         MRKT  0141,'ALBUQUERQUE'                                               
         MRKT  0143,'BAKERSFIELD'                                               
         MRKT  0145,'ALLENTOWN-BETHLEHEM'                                       
         MRKT  0147,'AMARILLO'                                                  
         MRKT  0149,'BEAUMONT-PORT ARTHUR'                                      
         MRKT  0151,'CEDAR RAPIDS'                                              
         MRKT  0153,'CHARLESTON, WV'                                            
         MRKT  0155,'CORPUS CHRISTI'                                            
         MRKT  0157,'QUAD CITIES'                                               
         MRKT  0159,'DULUTH-SUPERIOR'                                           
         MRKT  0161,'EL PASO'                                                   
         MRKT  0163,'FLINT'                                                     
         MRKT  0165,'FT. WAYNE'                                                 
         MRKT  0166,'GREENBORO-W.SLM-HIGH PT'                                   
         MRKT  0169,'JACKSON'                                                   
         MRKT  0171,'MADISON'                                                   
         MRKT  0173,'MONTGOMERY'                                                
         MRKT  0175,'WILKES BARRE-SCANTON'                                      
         MRKT  0177,'SPOKANE'                                                   
         MRKT  0179,'WHEELING'                                                  
         MRKT  0181,'CHATTANOOGA'                                               
         MRKT  0183,'COLUMBIA, SC'                                              
         MRKT  0185,'EVANSVILLE'                                                
         MRKT  0187,'FARGO-MOORHEAD'                                            
         MRKT  0189,'METRO FAIRFIELD COUNTY'                                    
         MRKT  0191,'GREENVL-SPARTANBURG, SC'                                   
         MRKT  0193,'HUNTINGTON-ASHLAND'                                        
         MRKT  0195,'LANSING-E. LANSING'                                        
         MRKT  0197,'PORTLAND, ME'                                              
         MRKT  0203,'SPRINGFIELD, MO'                                           
         MRKT  0205,'TOPEKA'                                                    
         MRKT  0207,'TUCSON'                                                    
         MRKT  0209,'WICHITA FALLS, TX'                                         
         MRKT  0215,'SAN JOSE'                                                  
         MRKT  0219,'ALTOONA'                                                   
         MRKT  0221,'ASHEVILLE'                                                 
         MRKT  0223,'BATON ROUGE'                                               
         MRKT  0225,'BILLINGS'                                                  
         MRKT  0227,'BINGHAMTON'                                                
         MRKT  0229,'BOISE'                                                     
         MRKT  0231,'CHARLESTON, SC'                                            
         MRKT  0233,'COLORADO SPRINGS'                                          
         MRKT  0235,'COLUMBUS, GA'                                              
         MRKT  0239,'ERIE'                                                      
         MRKT  0241,'EUGENE-SPRINGFIELD'                                        
         MRKT  0243,'GREAT FALLS, MT'                                           
         MRKT  0245,'GREEN BAY'                                                 
         MRKT  0247,'PORTSMTH-DOVER-ROCHESTER'                                  
         MRKT  0251,'KALAMAZOO'                                                 
         MRKT  0253,'LAFAYETTE, LA'                                             
         MRKT  0255,'LANCASTER'                                                 
         MRKT  0257,'LAS VEGAS'                                                 
         MRKT  0259,'LEXINGTON-FAYETTE'                                         
         MRKT  0261,'LINCOLN'                                                   
         MRKT  0263,'LUBBOCK'                                                   
         MRKT  0265,'MACON'                                                     
         MRKT  0267,'MANCHESTER'                                                
         MRKT  0269,'MCALLEN-BROWNSVILLE'                                       
         MRKT  0271,'SALISBURY-OCEAN CITY'                                      
         MRKT  0273,'READING PA.'                                               
         MRKT  0275,'RENO'                                                      
         MRKT  0277,'ROANOKE-LYNCHBURG'                                         
         MRKT  0279,'ROCKFORD'                                                  
         MRKT  0281,'SAGINAW-BAY CITY-MDLND'                                    
         MRKT  0283,'SALINAS-SEASIDE-MONTERY'                                   
         MRKT  0285,'SAVANNAH'                                                  
         MRKT  0287,'SOUTH BEND'                                                
         MRKT  0289,'JACKSON, TN'                                               
         MRKT  0291,'STOCKTON'                                                  
         MRKT  0293,'VISALIA, CA'                                               
         MRKT  0295,'UTICA-ROME'                                                
         MRKT  0297,'WATERLOO-CEDAR FALLS'                                      
         MRKT  0299,'WEST PALM BCH-BOCA ROTAN'                                  
         MRKT  0301,'YORK'                                                      
         MRKT  0303,'APPLETON-OSHKOSH'                                          
         MRKT  0305,'AUGUSTA, GA'                                               
         MRKT  0307,'TERRE HAUTE'                                               
         MRKT  0309,'WACO'                                                      
         MRKT  0311,'LAKELAND-WINTER HAVEN'                                     
         MRKT  0313,'SAN DIEGO N. COUNTY'                                       
         MRKT  0315,'ANCHORAGE'                                                 
         MRKT  0317,'PENSACOLA'                                                 
         MRKT  0319,'SIOUX FALLS'                                               
         MRKT  0321,'NASSAU-SUFFOLK(LI, NY)'                                    
         MRKT  0323,'BLOOMINGTON'                                               
         MRKT  0325,'YAKIMA'                                                    
         MRKT  0327,'HUNTSVILLE'                                                
         MRKT  0331,'MELBRN-TITUSVILLE-COCOA'                                   
         MRKT  0333,'TALLAHASSEE'                                               
         MRKT  0335,'TRAVERSE CITY'                                             
         MRKT  0337,'BRIDGEPORT'                                                
         MRKT  0339,'MEDFORD-ASHLAND, OR'                                       
         MRKT  0341,'DAYTONA BEACH'                                             
         MRKT  0343,'MODESTO'                                                   
         MRKT  0345,'JOHNSON CTY-KNGSPRT-BRSTL'                                 
         MRKT  0351,'ANAHEIM-SANTA ANA'                                         
         MRKT  0353,'PUEBLO'                                                    
         MRKT  0359,'FAYETTEVILLE, NC'                                          
         MRKT  0361,'GREENVL-N.BERN-WASH'                                       
         MRKT  0365,'NEW BDFRD-FALL RVR, MA'                                    
         MRKT  0367,'ATLANTIC CITY'                                             
         MRKT  0369,'CASPER'                                                    
         MRKT  0371,'TRI-CITIES'                                                
         MRKT  0373,'SARASOTA-BRADENTON'                                        
         MRKT  0375,'STUEBENVL-WEIRTON'                                         
         MRKT  0377,'WILMINGTON, NC'                                            
         MRKT  0379,'RIVERSIDE-S.BERNADINO'                                     
         MRKT  0381,'GRAND ISLAND-KEARNEY, NE'                                  
         MRKT  0383,'HOT SPRINGS, AR'                                           
         MRKT  0385,'WENATCHEE, WA'                                             
         MRKT  0387,'SALINA-MANHATTAN, KS'                                      
         MRKT  0391,'FT. WALTON-DESTINE'                                        
         MRKT  0393,'HUDSON VALLEY'                                             
         MRKT  0395,'WENATCHEE-MOSES LAKE, WA'                                  
         MRKT  0402,'MANKATO-NEW ULM-ST. PETER, MN'                             
         MRKT  0403,'MASON CITY, IA'                                            
         MRKT  0404,'FLORENCE-MUSCLE SHOALS, AL'                                
         MRKT  0405,'COLUMBUS-STARKVLLE-W.POINT, MS'                            
         MRKT  0407,'BOWLING GREEN'                                             
         MRKT  0408,'ELIZABETH CITY-NAGS HEAD'                                  
         MRKT  0409,'SEBRING, FL'                                               
         MRKT  0410,'ST. GEORGE-CEDAR CITY, UT'                                 
         MRKT  0411,'WESTCHESTER, NY'                                           
         MRKT  0412,'FLAGSTAFF-PRESCOTT, AZ'                                    
         MRKT  0413,'MIDDLESEX-SOMERSET-UNION, NJ'                              
         MRKT  0414,'CLARKSVILLE-HOPKINSVILLE TN-KY'                            
         MRKT  0415,'EUREKA CA'                                                 
         MRKT  0416,'FREDRICKSBURG VA'                                          
         MRKT  0417,'MEADVILLE-FRANKLIN PA'                                     
         MRKT  0418,'SHEBOYGAN, WI' 02/20/04                                    
         MRKT  0419,'VICTOR VALLEY, CA'                                         
         MRKT  0420,'PITTSBURG, KS'                                             
         MRKT  0421,'OLEAN, NY'                                                 
         MRKT  0422,'MUNCIE-MARION, IN'                                         
         MRKT  0423,'HILTON HEAD, SC'                                           
         MRKT  0424,'HAMPTONS-RIVERHEAD'                                        
         MRKT  0425,'LASALLE-PERU'                                              
         MRKT  0426,'CONCORD (LAKE REGIONS)'                                    
         MRKT  0429,'MIAMI-FT.LAUD-HOLLYWOOD'                                   
         MRKT  0430,'LEBANON-RUTLAND-WHITE RIVER'                               
         MRKT  0431,'LEBANON-HANOVER-WHITE RIVER'                               
         MRKT  0432,'MONTPELIER-BARRE-WATERBURY'                                
         MRKT  0433,'VALDOSTA, GA'                                              
         MRKT  0434,'SUNBURY-SELINSGROVE-LEWISBURG, PA'                         
         MRKT  0435,'ELKINS-BUCKHANNON-WESTON, WVA'                             
         MRKT  0436,'BEND, OR'                                                  
         MRKT  0437,'THE FLORIDA KEYS, FL'                                      
         MRKT  0438,'ROCKY MOUNT-WILSON, NC'                                    
         MRKT  0439,'LUFKIN-NACOGDOCHES'                                        
         MRKT  0440,'KALISPELL-FLATHEAD VALLEY,MT'                              
         MRKT  0441,'LAS CRUCES,NM'                                             
         MRKT  0442,'TWIN FALLS (SUN VALLEY), ID'                               
         MRKT  0443,'ASPEN, CO'                                                 
         MRKT  0461,'LAS CRUCES-DEMING, NM'                                     
         MRKT  0462,'TWIN FALLS-SUN VALLEY'                                     
*        MRKT  0485,'LA CONSOLIDATED TAR'            SPEC-52027                 
         MRKT  0485,'RAPID CITY-SPEARFISH, SD'       SPEC-52027                 
         MRKT  0497,'JONESBORO, AR'                                             
         MRKT  0499,'DECATUR, IL'                                               
         MRKT  0501,'DOTHAN, AL'                                                
         MRKT  0502,'TYLER, TX'                                                 
*960808  MRKT  0503,'FAYETTVL-SPRNGDALE, AR'                                    
         MRKT  0503,'FAYETTVL-NW ARKANSAS'                                      
         MRKT  0504,'DANVILLE'                                                  
         MRKT  0505,'BURLINGTON, VT'                                            
*        MRKT  0506,'LONGVIEW-MARSHALL,TX'          -->OUT SPR/91               
         MRKT  0506,'LAREDO, TX'                                                
         MRKT  0507,'CHAMPAIGN, IL'                                             
         MRKT  0508,'CHICO, CA'                                                 
         MRKT  0509,'REDDING, CA'                                               
*        MRKT  0510,'BIG SPRING, TX'                   OUT 5/90                 
         MRKT  0510,'MYRTLE BEACH, SC'                                          
         MRKT  0511,'GRAND JUNCTION, CO'                                        
         MRKT  0512,'TEXARKANA, TX'                                             
         MRKT  0513,'BRUNSWICK, GA'                                             
         MRKT  0514,'WATERBURY, CT'                                             
         MRKT  0515,'FT. MYERS-CAPE CORAL, FL'                                  
         MRKT  0516,'MONMOUTH-OCEAN, NJ'                                        
         MRKT  0517,'FT. PIERCE, FL'                                            
         MRKT  0518,'POUGHKEEPSIE, NY'                                          
*        MRKT  0519,'ABERDEEN, WA'                     OUT 5/90                 
         MRKT  0519,'HARRISONBURG, VA'                                          
         MRKT  0520,'BLUEFIELD, WV'                                             
         MRKT  0521,'SPRINGFIELD, IL'                                           
         MRKT  0522,'LAUREL-HATTIESBURG, MS'                                    
         MRKT  0523,'SIOUX CITY, IA'                                            
         MRKT  0524,'HAGRSTWN-CHMBRSBURG-WANSEBORO'                             
         MRKT  0525,'OWENSBORO, KY'                                             
         MRKT  0526,'BANGOR, ME'                                                
         MRKT  0527,'AUGUSTA-WATERVILLE, ME'                                    
         MRKT  0528,'FREDERICK, MD'                                             
*        MRKT  0529,'PASCAGOULA-MOSS PT, MS'        <-F/97                      
         MRKT  0529,'LEWISTON, ME'                                              
         MRKT  0530,'CAPE COD, MA'                                              
         MRKT  0531,'BATTLE CREEK, MI'                                          
         MRKT  0532,'ROCHESTER, MN'                                             
         MRKT  0533,'BILOXI-GULFPORT, MS'                                       
         MRKT  0534,'JOPLIN, MO'                                                
*02/20/04MRKT  0535),C' ','NW MICHIGAN'                                         
         MRKT  0535,'TRAVERSE CITY,-PETOSKEY, MI'         2/20/04               
*        MRKT  0536,'BOZEMAN, MT'                 --->OUT SPR/91                
         MRKT  0536,'MERCED, CA'                                                
         MRKT  0537,'TRENTON, NJ'                                               
         MRKT  0538,'SANTA FE, NM'                                              
         MRKT  0539,'PARKERSBURG-MARIETTA WV/OH'                                
*        MRKT  0540,'MINOT, ND'                **OUT 10/30/98                   
         MRKT  0540,'PUERTO RICO'                                               
         MRKT  0541,'STATE COLLEGE, PA'                                         
         MRKT  0542,'NEWBURGH-MIDDLETOWN, NY'                                   
         MRKT  0543,'TUPELO, MS'                                                
         MRKT  0544,'JOHNSTOWN'                                                 
         MRKT  0545,'RAPID CITY, SD'                                            
         MRKT  0546,'ABILENE, TX'                                               
         MRKT  0547,'STAMFORD-NORWALK, CT'                                      
         MRKT  0548,'SAN ANGELO, TX'                                            
         MRKT  0549,'BRYAN-COLLEGE STA., TX'                                    
         MRKT  0550,'GAINESVILLE, FL'                                           
*        MRKT  0551,'VICTORIA, TX'               SPEC-50171                     
         MRKT  0551,'LIMA-VAN-WERT, OH'          SPEC-50171                     
         MRKT  0552,'SOUTHERN ILLINOIS'                                         
         MRKT  0553,'CHARLOTTESVILLE, VA'                                       
         MRKT  0554,'FLORENCE, SC'                                              
*        MRKT  0555,'STAUNTON-WAYNESBORO, VA'           OUT SPR/94              
         MRKT  0555,'MORGNTWN-CLKSBRG-FRMT WVA'         SPR/94                  
         MRKT  0556,'SAN LUIS OBISPO'                   SPR/93                  
         MRKT  0557,'ELMIRA-CORNING, NY'                                        
         MRKT  0558,'BECKLEY, WV'                                               
         MRKT  0559,'LA CROSSE, WI'                                             
         MRKT  0560,'CHEYENNE, WY'                                              
         MRKT  0561,'ODESSA-MIDLAND, TX'                                        
         MRKT  0562,'KILLEN-TEMPLE, TX'                                         
         MRKT  0563,'NEW LONDON, CT'                                            
         MRKT  0564,'COLUMBIA, MO'                                              
         MRKT  0565,'MORRISTOWN, NJ'                                            
*040220  MRKT  0566,'LUFKIN-NACOGDOCHES, TX'                                    
         MRKT  0566,'MUSKEGON, MI'                                              
*980506  MRKT  0567,'BOULDER, CO'                                               
         MRKT  0567,'SANTA MARIA/LOMPOC, CA.'                                   
         MRKT  0568,'NEW RIVER VALLEY, VA'                                      
         MRKT  0569,'WINCHESTER, VA'                                            
         MRKT  0570,'DUBUQUE, IA'                                               
         MRKT  0571,'FT. WALTON BCH, FL'                                        
         MRKT  0572,'GRAND FORKS ND-MN'                                         
         MRKT  0573,'PANAMA CITY, FL'                                           
         MRKT  0574,'SANTA ROSA, CA'                                            
         MRKT  0575,'ST. CLOUD, MN'                                             
         MRKT  0576,'LIMA'                       ->SP/92                        
         MRKT  0577,'WATERTOWN, NY'                                             
         MRKT  0578,'WILLIAMSPORT, PA'                                          
         MRKT  0579,'SUSSEX, NJ'                 ->SP/97                        
         MRKT  0580,'ALBANY, GA'                                                
         MRKT  0581,'ANN ARBOR, MI'                                             
         MRKT  0582,'BISMARK, ND'                                               
         MRKT  0583,'ITHACA, NY'                                                
         MRKT  0584,'LAKE CHARLES, LA'                                          
         MRKT  0585,'MONROE, LA'                                                
         MRKT  0586,'EAU CLAIRE, WI'                                            
         MRKT  0587,'ALEXANDRIA, LA'                                            
         MRKT  0588,'MERIDIAN, MS'                                              
         MRKT  0589,'FT. SMITH, AR'                                             
*        MRKT  0590,'ALBANY-CRVLLIS-LEBANON, OR'            ->SP/91             
         MRKT  0590,'FT. COLLINS, GREELEY'                  ->FAL/97            
         MRKT  0591,'SANTA BARBARA, CA'                                         
         MRKT  0592,'PALM SPRINGS, CA'                                          
         MRKT  0593,'DANBURY, CT'                                               
         MRKT  0594,'OXNARD-VENTURA'                                            
         MRKT  0595,'LAFAYETTE, IN'                                             
         MRKT  0596,'TUSCALOOSA, AL'                                            
         MRKT  0597,'WAUSAU/STEVENS POINT'                   ->SP/92            
*        MRKT  0598,'CAPE MAY, NJ'                           <-F/97             
         MRKT  0598,'COOKVILLE, TN'                                             
         MRKT  0599,'LAWTON,OK'                                                 
         MRKT  0600,'PUERTO RICO WEST',BOOKTYPE=C                               
         MRKT  0601,'PUERTO RICO NORTH',BOOKTYPE=C                              
         MRKT  0602,'PUERTO RICO SOUTH',BOOKTYPE=C                              
         MRKT  0604,'PUERTO RICO EAST',BOOKTYPE=C                               
         MRKT  0607,'NEW YORK/PHI DMA CSAR',BOOKTYPE=C                          
         MRKT  0608,'NORTHEAST RADIO NETWORK',BOOKTYPE=C                        
         MRKT  0610,'JKSN-BATRGE-ALXDRIA-MNROE CMBO',BOOKTYPE=C                 
         MRKT  0612,'DOVER CSAR',BOOKTYPE=C                                     
         MRKT  0616,'HUDSON VALLEY CSA',BOOKTYPE=C                              
         MRKT  0619,'PUERTO RICO NORTHEAST',BOOKTYPE=C                          
         MRKT  0625,'PUERTO RICO SAN JUAN',BOOKTYPE=C                           
         MRKT  0644,'WASHINGTON DC-FREDERICK,MD MET'                            
         MRKT  0647,'HAGERSTOWN-WINCHESTER COMBO'                               
         MRKT  0648,'WMEV REGION MAX',BOOKTYPE=C                                
         MRKT  0659,'ROANOKE-LYNCHBURG CSAR',BOOKTYPE=C                         
         MRKT  0671,'PADUCAH-CAPE GIR-HARRISBURG'                               
         MRKT  0674,'SPRINFLD-DECATUR-CHMPGN',BOOKTYPE=C                        
         MRKT  0736,'LA CROSSE/EAU CLAIRE DMA',BOOKTYPE=C                       
         MRKT  0737,'ATLANTA, GA',BOOKTYPE=H                                    
         MRKT  0738,'BOSTON, MA',BOOKTYPE=H                                     
         MRKT  0740,'LAS CRUCES DEMING HISPANIC',BOOKTYPE=H                     
         MRKT  0742,'TAMPA, FL',BOOKTYPE=H                                      
         MRKT  0743,'NEW JERSEY CSA',BOOKTYPE=C                                 
         MRKT  0745,'WASHINGTON, DC',BOOKTYPE=H                                 
         MRKT  0747,'CORPUS CHRISTI, TX',BOOKTYPE=H                             
         MRKT  0749,'LAREDO, TX',BOOKTYPE=H                                     
         MRKT  0750,'NASSAU-SUFFOLK HISPANIC',BOOKTYPE=H                        
         MRKT  0762,'ST COL-SNBRY-WB',BOOKTYPE=C                                
         MRKT  0773,'VISALIA-TULARE-HANFORD, CA',BOOKTYPE=H                     
         MRKT  0775,'OXNARD-VENTURA, CA',BOOKTYPE=H                             
         MRKT  0780,'NASSAU RADIO NETWK-NEW ENGLAND',BOOKTYPE=C                 
         MRKT  0781,'ALBUQUERQUE HISPANIC',BOOKTYPE=H                           
         MRKT  0782,'ANAHEIM-SANTA ANA HISPANIC',BOOKTYPE=H                     
         MRKT  0783,'BAKERSFIELD HISPANIC',BOOKTYPE=H                           
         MRKT  0784,'DENVER-BOULDER HISPANIC',BOOKTYPE=H                        
         MRKT  0786,'NEW ENGLAND NORTH CSA',BOOKTYPE=C                          
         MRKT  0791,'SUNBRY-WILK BAR',BOOKTYPE=C                                
         MRKT  0800,'EUREKA-ARCATA, CA CSA',BOOKTYPE=C                          
         MRKT  0801,'EDAR RAPIDS-IOWA CITY (III)',BOOKTYPE=C                    
         MRKT  0803,'IDAHO FALLS ID. CSAR',BOOKTYPE=C                           
         MRKT  0805,'FLORIDA KEYS CSAR',BOOKTYPE=C                              
         MRKT  0806,'NORTHERN NEE ENGLAND CSAR',BOOKTYPE=C                      
         MRKT  0807,'POCATELLO-ID.FALLS CSA (III)',BOOKTYPE=C                   
         MRKT  0809,'RIVERSIDE-S.BERNARDINO HISPANIC',BOOKTYPE=H                
         MRKT  0810,'AUGUSTA VA. CSAR',BOOKTYPE=C                               
         MRKT  0811,'HUDSON VALLEY CSAR (III)',BOOKTYPE=C                       
         MRKT  0814,'SAN ANTONIO HISPANIC',BOOKTYPE=H                           
         MRKT  0815,'MASON CITY IA. CSAR',BOOKTYPE=C                            
         MRKT  0816,'MERIDIAN CSAR',BOOKTYPE=C                                  
         MRKT  0818,'CAPE GIRARDEAU, MO CSAR (II)',BOOKTYPE=C                   
         MRKT  0819,'NEW YORK HISPANIC',BOOKTYPE=H                              
         MRKT  0820,'POCATELLO ID. CSAR',BOOKTYPE=C                             
         MRKT  0821,'SUNBURY-SELINSGROVE-LEWISBURG',BOOKTYPE=C                  
         MRKT  0823,'DECATUR IL. CSAR',BOOKTYPE=C                               
         MRKT  0824,'COLUMBUS-STRKVLE CSA III',BOOKTYPE=C                       
         MRKT  0825,'THE SHOLES TA CSAR',BOOKTYPE=C                             
         MRKT  0827,'MIAMI-FT LAUDERDALE HISPANIC',BOOKTYPE=H                   
         MRKT  0831,'LAS VEGAS HISPANIC',BOOKTYPE=H                             
         MRKT  0832,'HILTON HEAD/BEAUFORD CSAR',BOOKTYPE=C                      
         MRKT  0834,'COLUMBIA CSAR',BOOKTYPE=C                                  
         MRKT  0835,'AUSTIN HISPANIC',BOOKTYPE=H                                
         MRKT  0836,'FAIRFIELD COUNTY CSAR',BOOKTYPE=C                          
         MRKT  0837,'LUFKIN-NACOGDOCHES CSAR',BOOKTYPE=C                        
         MRKT  0838,'JONESBORO,ARKANSAS CSAR',BOOKTYPE=C                        
         MRKT  0841,'VICTORIA TX CSAR',BOOKTYPE=C                               
         MRKT  0842,'LOS ANGELES HISPANIC',BOOKTYPE=H                           
         MRKT  0845,'ORLANDO HISPANIC',BOOKTYPE=H                               
         MRKT  0846,'DALLAS-FT WORTH HISPANIC',BOOKTYPE=H                       
         MRKT  0848,'HOUSTON-GALVESTON HISPANIC',BOOKTYPE=H                     
         MRKT  0849,'MCALLEN-BROWNSVILLE HISPANIC',BOOKTYPE=H                   
         MRKT  0850,'EL PASO HISPANIC',BOOKTYPE=H                               
         MRKT  0851,'ROCHESTER, MN CSA (III)',BOOKTYPE=C                        
         MRKT  0852,'CHICAGO HISPANIC',BOOKTYPE=H                               
         MRKT  0854,'BEND OR. CSAR',BOOKTYPE=C                                  
         MRKT  0855,'KQS BOSTON SUB NETWORK CSAR',BOOKTYPE=C                    
         MRKT  0856,'KQS BOSTON SUBURB NETWORK CSAR',BOOKTYPE=C                 
         MRKT  0857,'CARBONDALE-MARION, IL CSA (II)',BOOKTYPE=C                 
         MRKT  0858,'WASHINGTON DC-BALT METRO CSAR',BOOKTYPE=C                  
         MRKT  0859,'SAN DIEGO HISPANIC',BOOKTYPE=H                             
         MRKT  0860,'SAN FRANCISCO HISPANIC',BOOKTYPE=H                         
         MRKT  0861,'FRESNO HISPANIC',BOOKTYPE=H                                
         MRKT  0862,'MONTERY-SALINAS-SANTA CRUZ HISP',BOOKTYPE=H                
         MRKT  0864,'PHOENIX HISPANIC',BOOKTYPE=H                               
         MRKT  0865,'TUCSON HISPANIC',BOOKTYPE=H                                
         MRKT  0866,'STUBENVLLE-WEIRTON OH/WV CSAR',BOOKTYPE=C                  
         MRKT  0870,'ARKANSAS RIVER VALLEY CSA',BOOKTYPE=C                      
         MRKT  0876,'ISLAND OF HAWAII CSAR',BOOKTYPE=C                          
         MRKT  0878,'SACRAMENTO HISPANIC',BOOKTYPE=H                            
         MRKT  0879,'SAN JOSE HISPANIC',BOOKTYPE=H                              
         MRKT  0880,'POPLAR BLUFF, MO (CSAR II)',BOOKTYPE=C                     
         MRKT  0881,'COLUMBIA-JEFFERSON CTY, MO CSAR',BOOKTYPE=C                
         MRKT  0883,'CENTRAL NEW JERSY CSAR',BOOKTYPE=C                         
         MRKT  0884,'FOND DU LAC, WI CSA',BOOKTYPE=C                            
         MRKT  0885,'STATE COLLEGE PA CSA (III)',BOOKTYPE=C                     
         MRKT  0886,'MINOT ND. CSAR',BOOKTYPE=C                                 
         MRKT  0887,'CLEARFIELD CSA (III)',BOOKTYPE=C                           
         MRKT  0888,'HAGERSTOWN-FREDERICK COMBO',BOOKTYPE=C                     
         MRKT  0891,'QUINCY-HANNIBAL DMA',BOOKTYPE=C                            
         MRKT  0893,'BLUEFIELD WV. CSAR',BOOKTYPE=C                             
         MRKT  0895,'MANKATO, MN CSA (III)',BOOKTYPE=C                          
         MRKT  0896,'BELLINGHAM,WA CSA (III)',BOOKTYPE=C                        
         MRKT  0901,'NEW YORK BLACK',BOOKTYPE=B                                 
         MRKT  0902,'LOS ANGELES BLACK',BOOKTYPE=B                              
         MRKT  0903,'CHICAGO BLACK',BOOKTYPE=B                                  
         MRKT  0904,'PHILADELPHIA BLACK',BOOKTYPE=B                             
         MRKT  0905,'SAN FRANCISCO BLACK',BOOKTYPE=B                            
         MRKT  0906,'DETROIT BLACK',BOOKTYPE=B                                  
         MRKT  0907,'ST. LOUIS BLACK',BOOKTYPE=B                                
         MRKT  0908,'WASHINGTON DC BLACK',BOOKTYPE=B                            
         MRKT  0909,'BALTIMORE BLACK',BOOKTYPE=B                                
         MRKT  0910,'CINCINNATI BLACK',BOOKTYPE=B                               
         MRKT  0911,'CLEVELAND BLACK',BOOKTYPE=B                                
         MRKT  0912,'DALLAS-FT WORTH BLACK',BOOKTYPE=B                          
         MRKT  0913,'HOUSTON-GLVSTN BLACK',BOOKTYPE=B                           
         MRKT  0914,'COLUMBUS, OH BLACK',BOOKTYPE=B                             
         MRKT  0915,'KANSAS CITY BLACK',BOOKTYPE=B                              
         MRKT  0916,'MIAMI-FT LAUDERDALE BLACK',BOOKTYPE=B                      
         MRKT  0917,'ATLANTA BLACK',BOOKTYPE=B                                  
         MRKT  0918,'DAYTON BLACK',BOOKTYPE=B                                   
         MRKT  0919,'INDIANAPOLIS BLACK',BOOKTYPE=B                             
         MRKT  0920,'LOUISVILLE BLACK',BOOKTYPE=B                               
         MRKT  0921,'NASHVILLE BLACK',BOOKTYPE=B                                
         MRKT  0922,'NEW ORLEANS BLACK',BOOKTYPE=B                              
         MRKT  0923,'BIRMINGHAM BLACK',BOOKTYPE=B                               
         MRKT  0924,'CHARLOTTE BLACK',BOOKTYPE=B                                
         MRKT  0925,'MEMPHIS BLACK',BOOKTYPE=B                                  
         MRKT  0926,'RICHMOND BLACK',BOOKTYPE=B                                 
         MRKT  0927,'JACKSONVILLE BLACK',BOOKTYPE=B                             
         MRKT  0928,'MIDDLESEX-SOMERSET-HISPANIC ',BOOKTYPE=H                   
         MRKT  0929,'NORFOLK-PRTSMOUTH BLACK',BOOKTYPE=B                        
         MRKT  0930,'RALEIGH-DURHAM BLACK',BOOKTYPE=B                           
         MRKT  0931,'BATON ROUGE BLACK',BOOKTYPE=B                              
         MRKT  0932,'GREENSBORO BLACK',BOOKTYPE=B                               
         MRKT  0933,'CHARLESTON, SC BLACK',BOOKTYPE=B                           
         MRKT  0934,'BOSTON BLACK',BOOKTYPE=B                                   
         MRKT  0935,'BUFFALO BLACK',BOOKTYPE=B                                  
         MRKT  0936,'CHATTANOOGA BLACK',BOOKTYPE=B                              
         MRKT  0937,'GREENVILLE BLACK',BOOKTYPE=B                               
         MRKT  0938,'JACKSON, MS BLACK',BOOKTYPE=B                              
         MRKT  0939,'MILWAUKEE BLACK',BOOKTYPE=B                                
         MRKT  0940,'ORLANDO BLACK',BOOKTYPE=B                                  
         MRKT  0941,'PITTSBURG BLACK',BOOKTYPE=B                                
         MRKT  0942,'TAMPA-ST. PETE BLACK',BOOKTYPE=B                           
         MRKT  0943,'TOLEDO BLACK',BOOKTYPE=B                                   
         MRKT  0944,'COLUMBIA, SC. BLACK',BOOKTYPE=B                            
         MRKT  0945,'GREENVL-N.BERN-JACK BLACK',BOOKTYPE=B                      
         MRKT  0946,'HUNTSVILLE BLACK',BOOKTYPE=B                               
         MRKT  0947,'LITTLE ROCK BLACK',BOOKTYPE=B                              
         MRKT  0948,'MOBILE BLACK',BOOKTYPE=B                                   
         MRKT  0949,'NASSAU-SUFFOLK BLACK',BOOKTYPE=B                           
         MRKT  0950,'WEST PALM BCH-BOCA BLACK',BOOKTYPE=B                       
         MRKT  0951,'SHREVEPORT BLACK',BOOKTYPE=B                               
         MRKT  0952,'AUGUSTA, GA BLACK',BOOKTYPE=B                              
         MRKT  0953,'MONTGOMERY BLACK',BOOKTYPE=B                               
         MRKT  0954,'FAYETTEVILLE, NC BLACK',BOOKTYPE=B                         
         MRKT  0955,'LAFAYETTE, LA BLACK',BOOKTYPE=B                            
         MRKT  0956,'OKLAHOMA CITY BLACK',BOOKTYPE=B                            
         MRKT  0957,'MACON BLACK',BOOKTYPE=B                                    
         MRKT  0958,'ROCHESTER, NY BLACK',BOOKTYPE=B                            
         MRKT  0959,'SAVANNAH BLACK',BOOKTYPE=B                                 
         MRKT  0960,'WEST PALM BCH-BOCA HISPANIC',BOOKTYPE=H                    
         MRKT  0961,'COLUMBUS, GA BLACK',BOOKTYPE=B                             
         MRKT  0962,'FLINT BLACK',BOOKTYPE=B                                    
         MRKT  0963,'ALBANY, GA BLACK',BOOKTYPE=B                               
         MRKT  0964,'STOCKTON HISPANIC',BOOKTYPE=H                              
         MRKT  0965,'PALM SPRINGS HISPANIC',BOOKTYPE=H                          
         MRKT  0966,'MODESTO HISPANIC',BOOKTYPE=H                               
         MRKT  0967,'VICTOR VALLEY HISPANIC',BOOKTYPE=H                         
         MRKT  0968,'OKLAHOMA CITY HISPANIC',BOOKTYPE=H                         
         MRKT  0969,'MERCED, CA HISPANIC',BOOKTYPE=H                            
         MRKT  0970,'LAS CRUCES, NM HISPANIC',BOOKTYPE=H                        
         MRKT  0971,'ODESSA-MIDLAND, TX HISPANIC',BOOKTYPE=H                    
         MRKT  0972,'SANTA MARIA-LOMPOC, CA HISPANIC',BOOKTYPE=H                
         MRKT  0973,'PUEBLO HISPANIC',BOOKTYPE=H                                
         MRKT  0974,'AKRON BLACK',BOOKTYPE=B                                    
         MRKT  0976,'BILOXI-GULFPORT, MS BLACK',BOOKTYPE=B                      
         MRKT  0977,'HUDSON VALLEY BLACK',BOOKTYPE=B                            
         MRKT  0978,'HUDSON VALLEY HISPANIC',BOOKTYPE=H                         
         MRKT  0979,'WILMINGTON, DE BLACK',BOOKTYPE=B                           
         MRKT  0980,'LUBBOCK HISPANIC',BOOKTYPE=H                               
         MRKT  0981,'RENO HISPANIC',BOOKTYPE=H                                  
         MRKT  0982,'SANTA BARBARA HISPANIC',BOOKTYPE=H                         
         MRKT  0983,'YAKIMA HISPANIC',BOOKTYPE=H                                
         MRKT  0984,'ALLENTOWN HISPANIC',BOOKTYPE=H                             
         MRKT  0985,'COLORADO SPRINGS HISPANIC',BOOKTYPE=H                      
         MRKT  0986,'FT. MYERS HISPANIC',BOOKTYPE=H                             
         MRKT  0987,'NEW ORLEANS HISPANIC',BOOKTYPE=H                           
         MRKT  0989,'SPRINGFIELD HISPANIC',BOOKTYPE=H                           
         MRKT  0988,'OMAHA-COUNCIL BLUFFS HISPANIC',BOOKTYPE=H                  
         MRKT  0990,'WICHITA HISPANIC',BOOKTYPE=H                               
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* ARBMKTS                                                                       
         SMTBL AT                        ARB US TV                              
         MRKT  0003,'BOSTON'                                                    
         MRKT  0009,'NEW YORK'                                                  
         MRKT  0011,'PHILADELPHIA'                                              
         MRKT  0013,'LOS ANGELES'                                               
         MRKT  0015,'SAN DIEGO'                                                 
         MRKT  0017,'SANTA BARBARA-SANTA MARIA'                                 
         MRKT  0019,'WASHINGTON,DC'                                             
         MRKT  0021,'BALTIMORE'                                                 
         MRKT  0023,'SALISBURY'                                                 
         MRKT  0025,'HARTFORD-NEW HAVEN'                                        
         MRKT  0029,'PITTSBURGH'                                                
         MRKT  0031,'YOUNGSTOWN'                                                
         MRKT  0033,'JOHNSTOWN-ALTOONA'                                         
         MRKT  0035,'CLEVELAND'                                                 
         MRKT  0037,'AKRON'                                                     
         MRKT  0039,'EL CENTRO-YUMA'                                            
         MRKT  0041,'MANCHESTER'                                                
         MRKT  0043,'HRRSBRG-YORK-LANCSTR-LEBANON'                              
         MRKT  0045,'SPRINGFIELD,MA'                                            
         MRKT  0047,'PROVIDENCE-NEW BEDFORD'                                    
         MRKT  0051,'CHICAGO'                                                   
         MRKT  0053,'SOUTH BEND-ELKHART'                                        
         MRKT  0055,'TOLEDO'                                                    
         MRKT  0057,'DETROIT'                                                   
         MRKT  0059,'GRAND RAPIDS-KALAMAZOO'                                    
         MRKT  0061,'LANSING'                                                   
         MRKT  0063,'FLINT-SAGINAW-BAY CITY'                                    
         MRKT  0065,'SAN FRANCISCO'                                             
         MRKT  0067,'SACRAMENTO-STOCKTON'                                       
         MRKT  0069,'SALINAS-MONTEREY'                                          
         MRKT  0071,'FRESNO - VISALIA'                                          
         MRKT  0073,'BAKERSFIELD'                                               
         MRKT  0075,'ST. LOUIS'                                                 
         MRKT  0077,'SPRNGFLD-DECATUR-CHMPAIGN'                                 
         MRKT  0083,'INDIANAPOLIS'                                              
         MRKT  0085,'LAFAYETTE,IN'                                              
         MRKT  0087,'TERRE HAUTE'                                               
         MRKT  0089,'CHICO-REDDING'                                             
         MRKT  0091,'FT. WAYNE'                                                 
         MRKT  0093,'CINCINNATI'                                                
         MRKT  0095,'DAYTON'                                                    
         MRKT  0101,'LIMA'                                                      
         MRKT  0103,'WHEELING-STEUBENVILLE'                                     
         MRKT  0104,'PHILADELPHIA PPM'                                          
         MRKT  0105,'SEATTLE-TACOMA'                                            
         MRKT  0107,'MINNEAPOLIS-ST PAUL'                                       
         MRKT  0109,'DALLAS-FT WORTH'                                           
         MRKT  0111,'MILWAUKEE'                                                 
         MRKT  0113,'MADISON'                                                   
         MRKT  0115,'WAUSAU-RHINELANDER'                                        
         MRKT  0117,'LA CROSS-EAU CLAIRE'                                       
         MRKT  0119,'ROCKFORD'                                                  
         MRKT  0121,'COLUMBUS,OH'                                               
         MRKT  0123,'PORTLAND -POLAND SPRING'                                   
         MRKT  0125,'ZANESVILLE'                                                
         MRKT  0126,'UTICA'                                                     
         MRKT  0127,'MIAMI'                                                     
         MRKT  0129,'WEST PALM BEACH'                                           
         MRKT  0131,'TAMPA-ST. PETERSBURG'                                      
         MRKT  0133,'FT. MYERS'                                                 
         MRKT  0135,'BUFFALO'                                                   
         MRKT  0139,'ROCHESTER,NY'                                              
         MRKT  0140,'ELMIRA'                                                    
         MRKT  0141,'SYRACUSE'                                                  
         MRKT  0142,'SYRACUSE-ELMIRA'                                           
         MRKT  0143,'WILKES BARRE-SCRANTON'                                     
         MRKT  0145,'BINGHAMTON'                                                
         MRKT  0147,'ERIE'                                                      
         MRKT  0149,'ALBANY-SCHNCTDY-TROY'                                      
         MRKT  0151,'BURLINGTON-PLATTSBURG'                                     
         MRKT  0153,'WATERTOWN-CARTHAGE'                                        
         MRKT  0155,'UTICA'                                                     
         MRKT  0157,'KANSAS CITY'                                               
         MRKT  0159,'ST. JOSEPH'                                                
         MRKT  0161,'PRESQUE ISLE'                                              
         MRKT  0163,'BILLINGHAM'                                                
         MRKT  0165,'RCHSTR-MASON CITY-AUSTIN'                                  
         MRKT  0173,'CEDAR RAPIDS-WATERLOO'                                     
         MRKT  0175,'PEORIA'                                                    
         MRKT  0177,'DAVENPORT-ROCK IS-MOLINE'                                  
         MRKT  0179,'MEMPHIS'                                                   
         MRKT  0181,'NASHVILLE'                                                 
         MRKT  0183,'JACKSON,TN'                                                
         MRKT  0185,'HNTSVL-DECATUR-FLRENCE'                                    
         MRKT  0187,'PDUCAH-CP GIRARDEAU-HRRSBRG'                               
         MRKT  0195,'BOWLING GREEN'                                             
         MRKT  0197,'ATLANTA'                                                   
         MRKT  0199,'CHATTANOOGA'                                               
         MRKT  0201,'HOUSTON'                                                   
         MRKT  0203,'AUSTIN,TX'                                                 
         MRKT  0205,'WACO-TEMPLE'                                               
         MRKT  0207,'EVANSVILLE'                                                
         MRKT  0209,'LOUISVILLE'                                                
         MRKT  0211,'LEXINGTON'                                                 
         MRKT  0213,'GRNVL-SPARTNBRG-ASHEVILLE'                                 
         MRKT  0215,'KNOXVILLE'                                                 
         MRKT  0217,'BRISTOL-KNGSPRT-JOHNSON CITY'                              
         MRKT  0219,'MACON'                                                     
         MRKT  0221,'BIRMINGHAM'                                                
         MRKT  0225,'SELMA'                                                     
         MRKT  0227,'QUINCY-HANNIBAL'                                           
         MRKT  0229,'COLUMBIA-JEFFERSON CITY'                                   
         MRKT  0231,'TUSCALUSA'                                                 
         MRKT  0233,'PORTLAND,OR'                                               
         MRKT  0235,'EUGENE'                                                    
         MRKT  0237,'MEDFORD'                                                   
         MRKT  0241,'DENVER'                                                    
         MRKT  0243,'COLORADO SPRNGS-PUEBLO'                                    
         MRKT  0245,'NEW ORLEANS'                                               
         MRKT  0247,'BEAUMONT-PORT ARTHUR'                                      
         MRKT  0249,'BATON ROUGE'                                               
         MRKT  0251,'LAKE CHARLES'                                              
         MRKT  0253,'LAFAYETTE,LA'                                              
         MRKT  0255,'ALEXANDRIA,LA'                                             
         MRKT  0257,'CHARLESTON-HUNTINGTON'                                     
         MRKT  0259,'PARKERSBURG'                                               
         MRKT  0261,'CLARKSBURG-WESTON'                                         
         MRKT  0263,'OKLAHOMA CITY'                                             
         MRKT  0265,'ARDMORE-ADA'                                               
         MRKT  0269,'TULSA'                                                     
         MRKT  0271,'SAN ANTONIO'                                               
         MRKT  0273,'LAREDO'                                                    
         MRKT  0275,'PHOENIX'                                                   
         MRKT  0277,'TUCSON'                                                    
         MRKT  0279,'CHARLOTTE'                                                 
         MRKT  0281,'GRNSBRO-WINST SALEM-HI PNT'                                
         MRKT  0283,'NRFLK-PRTSMTH-NWPRT-HAMPTN'                                
         MRKT  0285,'RICHMOND'                                                  
         MRKT  0287,'HARRISONBURG'                                              
         MRKT  0291,'SALT LAKE CITY'                                            
         MRKT  0293,'TWIN FALLS'                                                
         MRKT  0295,'IDAHO FALLS-POCATELLO'                                     
         MRKT  0297,'HELENA'                                                    
         MRKT  0299,'GREAT FALLS'                                               
         MRKT  0301,'OMAHA'                                                     
         MRKT  0303,'DES MOINES'                                                
         MRKT  0305,'OTTUMWA-KIRKSVILLE'                                        
         MRKT  0307,'WICHITA-HUTCHINSON'                                        
         MRKT  0313,'TOPEKA'                                                    
         MRKT  0315,'GREEN BAY'                                                 
         MRKT  0317,'MARQUETTE'                                                 
         MRKT  0319,'LITTLE ROCK'                                               
         MRKT  0321,'SHRVPRT-TEXARKANA'                                         
         MRKT  0323,'TYLER'                                                     
         MRKT  0325,'FT. SMITH'                                                 
         MRKT  0327,'MONROE-EL DORADO'                                          
         MRKT  0329,'ORLANDO-DAYTONA BEACH'                                     
         MRKT  0331,'LINCOLN-HASTINGS KEARNEY'                                  
         MRKT  0335,'JACKSONVILLE'                                              
         MRKT  0337,'SPOKANE'                                                   
         MRKT  0339,'YAKIMA'                                                    
         MRKT  0342,'MISSOULA'                                                  
         MRKT  0345,'ROANOKE-LYNCHBURG'                                         
         MRKT  0347,'BLUEFLD-BECKLEY-OAK HILL'                                  
         MRKT  0351,'RALEIGH-DURHAM'                                            
         MRKT  0353,'GRNVL-NEW BERN-WASHINGTON'                                 
         MRKT  0355,'WILMINGTON'                                                
         MRKT  0357,'BANGOR'                                                    
         MRKT  0359,'FLORENCE,SC'                                               
         MRKT  0361,'COLUMBIA,SC'                                               
         MRKT  0363,'BILOXI-GLFPRT-PASCAGOULA'                                  
         MRKT  0367,'ALBUQUERQUE'                                               
         MRKT  0369,'ROSWELL'                                                   
         MRKT  0371,'EL PASO'                                                   
         MRKT  0373,'JACKSON,MS'                                                
         MRKT  0375,'GREENWOOD-GREENVILLE'                                      
         MRKT  0377,'MERIDIAN'                                                  
         MRKT  0379,'LAUREL-HATTIESBURG'                                        
         MRKT  0381,'DULUTH-SUPERIOR'                                           
         MRKT  0383,'MOBILE-PENSACOLA'                                          
         MRKT  0385,'NORTH PLATTE'                                              
         MRKT  0389,'SIOUX FALLS-MITCHELL'                                      
         MRKT  0391,'SIOUX CITY'                                                
         MRKT  0393,'FARGO'                                                     
         MRKT  0395,'ALEXANDRIA,MN'                                             
         MRKT  0397,'FT. DODGE'                                                 
         MRKT  0403,'AMARILLO'                                                  
         MRKT  0405,'WICHITA FALLS-LAWTON'                                      
         MRKT  0409,'COLUMBUS,GA'                                               
         MRKT  0411,'MONTGOMERY'                                                
         MRKT  0412,'MONTGOMERY-SELMA'                                          
         MRKT  0413,'TALLAHASEE'                                                
         MRKT  0415,'DOTHAN'                                                    
         MRKT  0417,'PANAMA CITY'                                               
         MRKT  0419,'ALBANY, GA'                                                
         MRKT  0421,'AUGUSTA'                                                   
         MRKT  0423,'CHARLESTON,SC'                                             
         MRKT  0425,'SAVANNAH'                                                  
         MRKT  0427,'SPRINFIELD,MO'                                             
         MRKT  0429,'JOPLIN-PITTSBURG'                                          
         MRKT  0431,'JONESBORO'                                                 
         MRKT  0433,'CORPUS CHRISTI'                                            
         MRKT  0435,'MCALLEN-BROWNSVILLE'                                       
         MRKT  0437,'LUBBOCK'                                                   
         MRKT  0439,'ODESSA-MIDLAND'                                            
         MRKT  0441,'ABILENE-SWEETWATER'                                        
         MRKT  0443,'SAN ANGELO'                                                
         MRKT  0445,'BOISE'                                                     
         MRKT  0448,'COLUMBUS-TUPELO'                                           
         MRKT  0449,'MANKATO'                                                   
         MRKT  0451,'TRAVERSE CITY CADILLAC'                                    
         MRKT  0452,'BIRMINGHAM'                                                
         MRKT  0455,'LAS VEGAS'                                                 
         MRKT  0457,'BILLINGS'                                                  
         MRKT  0459,'RENO'                                                      
         MRKT  0460,'GLENDIVE'                                                  
         MRKT  0462,'MINOT-BSMRK-DICKINSON'                                     
         MRKT  0465,'CHEYENNE'                                                  
         MRKT  0467,'EUREKA'                                                    
         MRKT  0469,'RAPID CITY'                                                
         MRKT  0471,'CASPER-RIVERTON'                                           
         MRKT  0473,'GRAND JUNCTION'                                            
         MRKT  0481,'FT PIERCE-VERO BEACH'                                      
         MRKT  0505,'ANDERSON'                                                  
         MRKT  0509,'CANTON'                                                    
         MRKT  0511,'MODESTO'                                                   
         MRKT  0513,'VICTORIA'                                                  
         MRKT  0515,'WILDWOOD'                                                  
         MRKT  0517,'WORCESTER'                                                 
         MRKT  0569,'FT LAUDERDALE'                                             
         MRKT  0577,'PALM SPRINGS'                                              
         MRKT  0589,'HICKORY'                                                   
         MRKT  0591,'BEND'                                                      
         MRKT  0593,'HANOVER'                                                   
         MRKT  0601,'HAGERSTOWN'                                                
         MRKT  0603,'ANNISTON'                                                  
         MRKT  0611,'HAZARD'                                                    
         MRKT  0613,'BUTTE'                                                     
         MRKT  0615,'ALLENTOWN'                                                 
         MRKT  0617,'DUBUQUE'                                                   
         MRKT  0619,'DUBUQUE'                                                   
         MRKT  0621,'GAINESVILLE'                                               
         MRKT  0624,'MILES CITY-GLENDIVE'                                       
         MRKT  0625,'FLAGSTAFF'                                                 
         MRKT  0627,'ALPENA'                                                    
         MRKT  0631,'HANFORD'                                                   
         MRKT  0639,'TULARE'                                                    
         MRKT  0643,'BATTLE CREEK'                                              
         MRKT  0645,'SARASOTA'                                                  
         MRKT  0649,'FARMINGTON'                                                
         MRKT  0651,'CHARLOTTESVILLE'                                           
         MRKT  0653,'FREDERICKSBURG'                                            
         MRKT  0655,'ANCHORAGE'                                                 
         MRKT  0659,'CHARLTVLLE-HARRSNBRG'                                      
         MRKT  0701,'ALBANY, GA. BLACK',BOOKTYPE=B                              
         MRKT  0702,'ALBUQUERQUE HISPANIC',BOOKTYPE=H                           
         MRKT  0703,'ATLANTA BLACK',BOOKTYPE=B                                  
         MRKT  0704,'AUGUSTA BLACK',BOOKTYPE=B                                  
         MRKT  0705,'BALTIMORE BLACK',BOOKTYPE=B                                
         MRKT  0706,'BATON ROUGE BLACK',BOOKTYPE=B                              
         MRKT  0708,'BIRMINGHAM BLACK',BOOKTYPE=B                               
         MRKT  0709,'CHARLESTON, SC BLACK',BOOKTYPE=B                           
         MRKT  0710,'CHARLOTTE BLACK',BOOKTYPE=B                                
         MRKT  0711,'CHICAGO BLACK',BOOKTYPE=B                                  
         MRKT  0712,'CINCINNATI BLACK',BOOKTYPE=B                               
         MRKT  0713,'CLEVELAND BLACK',BOOKTYPE=B                                
         MRKT  0714,'COLUMBIA, SC. BLACK',BOOKTYPE=B                            
         MRKT  0716,'COLUMBUS, GA. BLACK',BOOKTYPE=B                            
         MRKT  0717,'CORPUS CHRISTI HISPANIC',BOOKTYPE=H                        
         MRKT  0718,'DALLAS-FT. WORTH BLACK',BOOKTYPE=B                         
         MRKT  0719,'DETROIT BLACK',BOOKTYPE=B                                  
         MRKT  0720,'EL CENTRO-YUMA BLACK',BOOKTYPE=B                           
         MRKT  0721,'EL CENTRO-YUMA HISPANIC',BOOKTYPE=H                        
         MRKT  0722,'EL PASO HISPANIC',BOOKTYPE=H                               
         MRKT  0723,'FLORENCE, SC BLACK',BOOKTYPE=B                             
         MRKT  0724,'FRESNO-VISALIA HISPANIC',BOOKTYPE=H                        
         MRKT  0725,'GRNWOOD-GRNVILLE BLACK',BOOKTYPE=B                         
         MRKT  0726,'GRNVLLE-N.BERN-WASH. BLACK',BOOKTYPE=B                     
         MRKT  0727,'HOUSTON BLACK',BOOKTYPE=B                                  
         MRKT  0728,'JACKSON, MS BLACK',BOOKTYPE=B                              
         MRKT  0729,'JACKSONVILLE BLACK',BOOKTYPE=B                             
         MRKT  0730,'LAFAYETTE, LA. BLACK',BOOKTYPE=B                           
         MRKT  0731,'LAREDO HISPANIC',BOOKTYPE=H                                
         MRKT  0732,'COLORADO SPR.-PUEBLO HISPANIC',BOOKTYPE=H                  
         MRKT  0733,'LITTLE ROCK BLACK',BOOKTYPE=B                              
         MRKT  0734,'LOS ANGELES BLACK',BOOKTYPE=B                              
         MRKT  0735,'LOS ANGELES HISPANIC',BOOKTYPE=H                           
         MRKT  0736,'MACON BLACK',BOOKTYPE=B                                    
         MRKT  0737,'MCALLEN-BRNSVLLE LRGV HISPANIC',BOOKTYPE=H                 
         MRKT  0738,'MEMPHIS BLACK',BOOKTYPE=B                                  
         MRKT  0739,'MERIDIAN BLACK',BOOKTYPE=B                                 
         MRKT  0740,'MIAMI BLACK',BOOKTYPE=B                                    
         MRKT  0741,'MIAMI HISPANIC',BOOKTYPE=H                                 
         MRKT  0742,'MOBILE-PENSACOLA BLACK',BOOKTYPE=B                         
         MRKT  0743,'MONROE-EL DORADO BLACK',BOOKTYPE=B                         
         MRKT  0744,'MONTGOMERY BLACK',BOOKTYPE=B                               
         MRKT  0745,'NEW ORLEANS BLACK',BOOKTYPE=B                              
         MRKT  0746,'NEW YORK BLACK',BOOKTYPE=B                                 
         MRKT  0747,'NEW YORK HISPANIC',BOOKTYPE=H                              
         MRKT  0748,'NFLK-PRTSMTH-N.NWS-HAMP BLACK',BOOKTYPE=B                  
         MRKT  0749,'PHILADELPHIA BLACK',BOOKTYPE=B                             
         MRKT  0750,'PITTSBURG BLACK',BOOKTYPE=B                                
         MRKT  0751,'RALEIGH-DURHAM BLACK',BOOKTYPE=B                           
         MRKT  0752,'RICHMOND BLACK',BOOKTYPE=B                                 
         MRKT  0753,'ST. LOIUS BLACK',BOOKTYPE=B                                
         MRKT  0754,'SALINAS-MONTEREY HISPANIC',BOOKTYPE=H                      
         MRKT  0755,'SAN FRANCISCO BLACK',BOOKTYPE=B                            
         MRKT  0756,'SAN ANTONIO HISPANIC',BOOKTYPE=H                           
         MRKT  0760,'SAN DIEGO HISPANIC',BOOKTYPE=H                             
         MRKT  0761,'CHICAGO HISPANIC',BOOKTYPE=H                               
         MRKT  0762,'DALLAS-FT. WORTH HISPANIC',BOOKTYPE=H                      
         MRKT  0763,'WASHINGTON, DC. BLACK',BOOKTYPE=B                          
         MRKT  0765,'GRNSBORO-WINSTON SALEM BLACK',BOOKTYPE=B                   
         MRKT  0766,'NASHVILLE BLACK',BOOKTYPE=B                                
         MRKT  0768,'HOUSTON HISPANIC',BOOKTYPE=H                               
         MRKT  0769,'AUSTIN, TX HISPANIC',BOOKTYPE=H                            
         MRKT  0770,'BAKERSFIELD HISPANIC',BOOKTYPE=H                           
         MRKT  0772,'LAUREL-HATTIESBURG BLACK',BOOKTYPE=B                       
         MRKT  0775,'DAYTON BLACK',BOOKTYPE=B                                   
         MRKT  0782,'INDIANAPOLIS BLACK',BOOKTYPE=B                             
         MRKT  0785,'LOUISVILLE BLACK',BOOKTYPE=B                               
         MRKT  0786,'LUBBOCK HISPANIC',BOOKTYPE=H                               
         MRKT  0791,'PHOENIX HISPANIC',BOOKTYPE=H                               
         MRKT  0793,'SACRAMENTO-STOCKTON HISPANIC',BOOKTYPE=H                   
         MRKT  0794,'SAN DIEGO HISPANIC',BOOKTYPE=H                             
         MRKT  0801,'EVANSVILLE METRO',BOOKTYPE=M                               
         MRKT  0802,'SAN FRANCISCO HISPANIC',BOOKTYPE=H                         
         MRKT  0803,'HARTFORD-HANOVER TA',BOOKTYPE=T                            
         MRKT  0804,'SANTA ROSA TA',BOOKTYPE=T                                  
         MRKT  0805,'ALLENTOWN TA',BOOKTYPE=T                                   
         MRKT  0806,'OKLAHOMA CITY TRADING AREA',BOOKTYPE=T                     
         MRKT  0807,'COLORADO SPRINGS TRADING AREA',BOOKTYPE=T                  
         MRKT  0808,'FAYETTVLLE-SPRGDALE-ROGERS TA',BOOKTYPE=T                  
         MRKT  0809,'MEMPHIS TA',BOOKTYPE=T                                     
         MRKT  0810,'MEDFORD/EUGENE TRADING AREA',BOOKTYPE=T                    
         MRKT  0811,'AUGUSTA TA',BOOKTYPE=T                                     
         MRKT  0812,'FAYETTEVILLE, NC TA',BOOKTYPE=T                            
         MRKT  0813,'BRIDGEFORD METRO',BOOKTYPE=M                               
         MRKT  0814,'BATON ROUGE TA',BOOKTYPE=T                                 
         MRKT  0815,'LONGVIEW-TYLER TA',BOOKTYPE=T                              
         MRKT  0816,'TAMPA-ST.PETE-SARAS TA',BOOKTYPE=T                         
         MRKT  0817,'SAN JOSE TA',BOOKTYPE=T                                    
         MRKT  0818,'WITCHITA-HUTCHINSON METRO',BOOKTYPE=M                      
         MRKT  0819,'BIRM-ANNIS-TUSCA TA',BOOKTYPE=T                            
         MRKT  0820,'PORTLAND,OR METRO',BOOKTYPE=M                              
         MRKT  0821,'BIRMINGHAM TA',BOOKTYPE=T                                  
         MRKT  0822,'SAN JOSE TRADING AREA',BOOKTYPE=T                          
         MRKT  0823,'TMG SCANNER - EVANSVILLE',BOOKTYPE=S                       
         MRKT  0824,'TMG SCANNER - PORTLAND, ME',BOOKTYPE=S                     
         MRKT  0825,'TMG SCANNER - ORLANDO',BOOKTYPE=S                          
         MRKT  0829,'TAMPA-ST. PETE TA',BOOKTYPE=T                              
         MRKT  0830,'ALBUQUERQUE EXPANDED TA',BOOKTYPE=T                        
         MRKT  0831,'ALBUQUERQUE METRO',BOOKTYPE=M                              
         MRKT  0832,'BIRMINGHAM METRO',BOOKTYPE=M                               
         MRKT  0833,'MOUNT VERNON TA',BOOKTYPE=T                                
         MRKT  0834,'WENATCHEE TA',BOOKTYPE=T                                   
         MRKT  0835,'VIRGIN ISLAND TA',BOOKTYPE=T                               
         MRKT  0836,'WORCESTER,MA TA',BOOKTYPE=T                                
         MRKT  0837,'LINCOLN-HAST-KEARNEY METRO 2',BOOKTYPE=M                   
         MRKT  0838,'VENTURA COUNTY TA',BOOKTYPE=T                              
         MRKT  0840,'MONTGOMERY TA',BOOKTYPE=T                                  
         MRKT  0841,'MANCHESTER, NH TA',BOOKTYPE=T                              
         MRKT  0842,'MONTGOMERY METRO TA',BOOKTYPE=T                            
         MRKT  0843,'EASTERN-SE KENTUCKY TAR',BOOKTYPE=T                        
         MRKT  0846,'ATLANTA TA',BOOKTYPE=T                                     
         MRKT  0847,'WITCHITA-HUTCH,ETC. TA',BOOKTYPE=T                         
         MRKT  0851,'GR.RAPIDS/KALAM/BC TAR',BOOKTYPE=T                         
         MRKT  0853,'PUERTO RICO'                                               
         MRKT  0854,'BINGHAMTON TA',BOOKTYPE=T                                  
         MRKT  0855,'CONCORD, NH TA',BOOKTYPE=T                                 
         MRKT  0856,'KANSAS NTWK AFFILIATE REPORT',BOOKTYPE=N                   
         MRKT  0857,'LUFKIN-NOCAGDOCHES TA',BOOKTYPE=T                          
         MRKT  0858,'MISSOULA-BUTTE TA',BOOKTYPE=T                              
         MRKT  0859,'MONTGOMERY-SELMA TA',BOOKTYPE=T                            
         MRKT  0860,'LONG ISLAND TA',BOOKTYPE=T                                 
         MRKT  0861,'N.W. WASHINGTON TA',BOOKTYPE=T                             
         MRKT  0862,'TMG SCANNER - BOISE',BOOKTYPE=S                            
         MRKT  0863,'BIRMINGHAM-TUSCALOOSA TAR',BOOKTYPE=T                      
         MRKT  0865,'CHEYENNE-SCOTTS-STER. TAR',BOOKTYPE=T                      
         MRKT  0866,'HUDSON VALLEY,NY TA',BOOKTYPE=T                            
         MRKT  0867,'SALEM, OR TA',BOOKTYPE=T                                   
         MRKT  0868,'FAYTTVLL-CUMBERLND-ROBESON TA',BOOKTYPE=T                  
         MRKT  0869,'ANNISTON TA',BOOKTYPE=T                                    
         MRKT  0870,'NEW LONDON TA',BOOKTYPE=T                                  
         MRKT  0871,'EUGENE-SPRINGFIELD METRO',BOOKTYPE=M                       
         MRKT  0872,'SOUTHERN NH TA',BOOKTYPE=T                                 
         MRKT  0873,'SARASOTA TA',BOOKTYPE=T                                    
         MRKT  0877,'BIRMINGHAM TA+TSA',BOOKTYPE=E                              
         MRKT  0881,'MOBILE TA',BOOKTYPE=T                                      
         MRKT  0882,'CHICAGO SUPERSTA (Y+R)',BOOKTYPE=T                         
         MRKT  0883,'NEW YORK SUPERSTA (WPIX)',BOOKTYPE=T                       
         MRKT  0884,'NEW YORK SUPERSTA (WWOR)',BOOKTYPE=E                       
         MRKT  0886,'CHICAGO SUPERSTATION',BOOKTYPE=E                           
         MRKT  0887,'LOS ANGELES SUPERSTA TA',BOOKTYPE=T                        
         MRKT  0888,'BOSTON SUPERSTA TA',BOOKTYPE=T                             
         MRKT  0889,'DALLAS SUPERSTA TA',BOOKTYPE=T                             
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* BBMRMKTS                                                                      
         SMTBL MR                          BBM CANADIAN RADIO                   
         MRKT  0009,'ST. JOHNS'                                                 
         MRKT  0011,'CARBONEAR'                                                 
         MRKT  0049,'OLD CORNER BROOK'                                          
         MRKT  0050,'CORNER BROOK'                                              
         MRKT  0060,'GAN/GR FA'                                                 
         MRKT  1019,'CHARLOTTETOWN'                                             
         MRKT  2010,'SYDNEY-GLACE BAY'                                          
         MRKT  2035,'ANTIGONISH'                                                
         MRKT  2045,'PORT HAWKSBURY'                                            
         MRKT  2069,'NEW GLASSOW'                                               
         MRKT  2080,'HALIFAX'                                                   
         MRKT  2089,'BRIDGEWATER'                                               
         MRKT  2119,'YARMOUTH'                                                  
         MRKT  2160,'KENTVILLE'                                                 
         MRKT  2170,'TRURO'                                                     
         MRKT  2180,'AMHERST CMA'                                               
         MRKT  3008,'SHEDIAC'                                                   
         MRKT  3009,'MONCTON'                                                   
         MRKT  3029,'SAINT JOHN'                                                
         MRKT  3039,'ST STEPHEN'                                                
         MRKT  3079,'FREDERICTON'                                               
         MRKT  3119,'BALMORAL'                                                  
         MRKT  3130,'WOODSTOCK NB'                                              
         MRKT  3140,'VICTORIA COUNTY'                                           
         MRKT  3150,'EDMUNSTON'                                                 
         MRKT  4010,'BONAVENTURE'                                               
         MRKT  4029,'GASPE'                                                     
         MRKT  4040,'MATANE'                                                    
         MRKT  4050,'MAT/AMQUI'                                                 
         MRKT  4070,'RIMOUSKI'                                                  
         MRKT  4081,'SEPT ILES'                                                 
         MRKT  4093,'BAIE COMEAU'                                               
         MRKT  4100,'RIVIERE DU LOUP'                                           
         MRKT  4120,'CHICOUTIMI-JONQUIERE'                                      
         MRKT  4130,'ALMA CMA'                                                  
         MRKT  4142,'ROBERVAL'                                                  
         MRKT  4148,'OLD ROBERVAL'                                              
         MRKT  4150,'CHARLEVIOX'                                                
         MRKT  4161,'LA POCATIERE'                                              
         MRKT  4179,'MONTMAGNY'                                                 
         MRKT  4199,'QUEBEC'                                                    
         MRKT  4201,'DONNACONA'                                                 
         MRKT  4258,'VICTORIAVILLE-THETFORD MINES'                              
         MRKT  4259,'THETFORD FALLS'                                            
         MRKT  4268,'STE. MARIE'                                                
         MRKT  4269,'ST. GEORGES'                                               
         MRKT  4270,'BEAUCE'                                                    
         MRKT  4279,'VICTORIAVILLE'                                             
         MRKT  4339,'SHERBROOK CMA'                                             
         MRKT  4359,'OLD GRANBY'                                                
         MRKT  4360,'GRANBY'                                                    
         MRKT  4369,'DRUMMONDVILLE'                                             
         MRKT  4408,'ST JEAN'                                                   
         MRKT  4449,'ST HYACIN'                                                 
         MRKT  4479,'MONTREAL'                                                  
         MRKT  4480,'MONT ANGLO'                                                
         MRKT  4481,'MONT FRANCO'                                               
         MRKT  4590,'ST JEROME'                                                 
         MRKT  4591,'STE ADELE 1'                                               
         MRKT  4595,'LACHUTE'                                                   
         MRKT  4609,'JOLIETTE'                                                  
         MRKT  4629,'ST-GABRIEL-DE-BRANDN'                                      
         MRKT  4659,'TROIS RIVIERES'                                            
         MRKT  4669,'SHAWINIGAN'                                                
         MRKT  4679,'MANIWAKI'                                                  
         MRKT  4700,'MONT-LAURIER'                                              
         MRKT  4710,'ROUYN'                                                     
         MRKT  4723,'VAL D OR'                                                  
         MRKT  5008,'CORNWALL'                                                  
         MRKT  5069,'OTTAWA-HULL'                                               
         MRKT  5071,'OTTAWA ANGLO'                                              
         MRKT  5072,'OTTAWA FRANCO'                                             
         MRKT  5079,'BROCKVILLE'                                                
         MRKT  5080,'OGDENSBURG'                                                
         MRKT  5089,'SMITHS FALLS'                                              
         MRKT  5099,'PEMBROOKE'                                                 
         MRKT  5100,'OLD PEMBROKE'                                              
         MRKT  5109,'KINGSTON'                                                  
         MRKT  5139,'BELLVL-TRENT'                                              
         MRKT  5150,'COBOURS'                                                   
         MRKT  5159,'PETERBOROUGH'                                              
         MRKT  5170,'LINDSAY'                                                   
         MRKT  5187,'OSH-WHIT CMA'                                              
         MRKT  5188,'AJAX'                                                      
         MRKT  5189,'OSHAWA'                                                    
         MRKT  5190,'AJAX-DURHAM'                                               
         MRKT  5199,'TORONTO'                                                   
         MRKT  5201,'NEWMARKET'                                                 
         MRKT  5229,'ORANGEVILLE 1'                                             
         MRKT  5230,'ORANGEVILLE'                                               
         MRKT  5236,'COLLINSWOOD'                                               
         MRKT  5237,'MIDLAND'                                                   
         MRKT  5238,'ORILLIA'                                                   
         MRKT  5239,'BARRIE/HURONIA'                                            
         MRKT  5242,'COLLINGWOOD'                                               
         MRKT  5249,'HUNTSVILLE'                                                
         MRKT  5250,'BRACEBRIDGE'                                               
         MRKT  5269,'HAMILTON'                                                  
         MRKT  5286,'ST CATHARINES'                                             
         MRKT  5299,'ST CATH-NIAG'                                              
         MRKT  5301,'FT ERIE/BUFFALO'                                           
         MRKT  5320,'BRANTFORD'                                                 
         MRKT  5329,'SIMCOE'                                                    
         MRKT  5337,'CAMBRIDGE/KITCH'                                           
         MRKT  5339,'KITCHENER'                                                 
         MRKT  5348,'TILLSONBURG'                                               
         MRKT  5349,'WOODSTOCK, ON'                                             
         MRKT  5350,'OLD WOODSTOCK'                                             
         MRKT  5358,'GUELPH'                                                    
         MRKT  5369,'LONDON'                                                    
         MRKT  5371,'ST THOMAS 1992'                                            
         MRKT  5380,'ST THOMAS'                                                 
         MRKT  5389,'OLD SARNIA'                                                
         MRKT  5390,'SARNIA'                                                    
         MRKT  5399,'CHATHAM WBURG'                                             
         MRKT  5400,'CHATHAM'                                                   
         MRKT  5407,'LEAMNGTN CMA'                                              
         MRKT  5409,'WINDSOR'                                                   
         MRKT  5419,'STRATFORD'                                                 
         MRKT  5429,'WINGHAM'                                                   
         MRKT  5449,'OWENSOUND'                                                 
         MRKT  5450,'GREY'                                                      
         MRKT  5459,'PARRY SOUND'                                               
         MRKT  5469,'NORTH BAY'                                                 
         MRKT  5479,'SUDBURY'                                                   
         MRKT  5480,'ESPANOLA'                                                  
         MRKT  5499,'TIMMINS'                                                   
         MRKT  5502,'KAPUSKASING'                                               
         MRKT  5515,'ELLIOT LAKE'                                               
         MRKT  5519,'S S MARIE'                                                 
         MRKT  5539,'THUNDER BAY'                                               
         MRKT  6020,'STEINBACH'                                                 
         MRKT  6030,'ALTONA-MORDEN'                                             
         MRKT  6050,'BOISSEVAIN'                                                
         MRKT  6069,'BRANDON'                                                   
         MRKT  6089,'PORTAGE LA PRAIRIE'                                        
         MRKT  6119,'WINNIPEG'                                                  
         MRKT  6129,'SELKIRK'                                                   
         MRKT  6809,'PORTAGE LA PRAIRIE'                                        
         MRKT  7020,'WEYBURN'                                                   
         MRKT  7049,'SWIFT CURRENT'                                             
         MRKT  7069,'REGINA'                                                    
         MRKT  7071,'ESTEVAN'                                                   
         MRKT  7079,'MOOSE JAW'                                                 
         MRKT  7090,'YORKTON'                                                   
         MRKT  7109,'SASKATOON'                                                 
         MRKT  7129,'ROSETOWN'                                                  
         MRKT  7154,'PRINCE ALBERT'                                             
         MRKT  7156,'NORTH BATTLEFORD'                                          
         MRKT  8010,'MEDICINE HAT'                                              
         MRKT  8015,'TABER'                                                     
         MRKT  8019,'LETHBRIDGE'                                                
         MRKT  8030,'BLAIRMORE'                                                 
         MRKT  8044,'DRUMHELLER'                                                
         MRKT  8045,'DRUMHELLER 1'                                              
         MRKT  8059,'WAINWRGHT CMA'                                             
         MRKT  8069,'CALGARY'                                                   
         MRKT  8079,'RED DEER'                                                  
         MRKT  8091,'LLOYDMINSTER'                                              
         MRKT  8111,'CAMROSE'                                                   
         MRKT  8119,'EDMONTON'                                                  
         MRKT  8150,'EDSON'                                                     
         MRKT  8159,'PEACE RIVER'                                               
         MRKT  8171,'GRANDE PRAIRIE'                                            
         MRKT  9070,'PENTICTON'                                                 
         MRKT  9079,'FR VALLEY'                                                 
         MRKT  9080,'CHILLIWACK'                                                
         MRKT  9081,'ABBOTSFORD'                                                
         MRKT  9109,'VANCOUVER'                                                 
         MRKT  9119,'VICTORIA'                                                  
         MRKT  9139,'NANAIMO'                                                   
         MRKT  9209,'KAMLOOPS'                                                  
         MRKT  9230,'KELOWNA'                                                   
         MRKT  9239,'VERNON'                                                    
         MRKT  9270,'CAR-QUE-W'                                                 
         MRKT  9309,'TER-PR RU'                                                 
         MRKT  9349,'PRINCE GEORGE'                                             
         MRKT  9371,'DAWSON CR'                                                 
         MRKT  9505,'Q RADIO SYSTEM'                                            
         MRKT  9510,'RADIO NFLD'                                                
         MRKT  9535,'E.BRAODCAST'                                               
         MRKT  9570,'ABITIBI EST'                                               
         MRKT  9587,'ALL SEASONS'                                               
         MRKT  9597,'EASTARIO RAD'                                              
         MRKT  9605,'KING-PETE RA'                                              
         MRKT  9607,'GREATER SUDBURY'                                           
         MRKT  9643,'GR.SAKS RADIO'                                             
         MRKT  9644,'REGINA/MOOSE JAW'                                          
         MRKT  9645,'LETH/TABER'                                                
         MRKT  9650,'S.ALB.RADIO'                                               
         MRKT  9652,'LETH+BLAIRMR'                                              
         MRKT  9705,'TELEMEDIA NORTH'                                           
         MRKT  9710,'ST. JOHNS EMA'                                             
         MRKT  9711,'MONTREAL EM'                                               
         MRKT  9712,'MONTREAL EM (1)'                                           
         MRKT  9713,'WINDSOR EM'                                                
         MRKT  9714,'BRANDON EM'                                                
         MRKT  9715,'WINNIPEG EM'                                               
*        MRKT  9716,'S. SASK(1)'                                                
         MRKT  9716,'BARRIE EM'                                                 
         MRKT  9717,'S.W. ONT RADIO'                                            
         MRKT  9718,'KITCHENER EM 1'                                            
         MRKT  9719,'BRANDON EM 1'                                              
         MRKT  9720,'S. SASK(1)'                                                
         MRKT  9721,'RED DEER EM'                                               
         MRKT  9722,'EDMONTON EM 1'                                             
         MRKT  9723,'KAMLOOPS EM'                                               
         MRKT  9724,'FRASER VALLEY PT1'                                         
         MRKT  9725,'FRASER VALLEY (2)'                                         
         MRKT  9726,'KINGS/ST JOHN/CHAR'                                        
         MRKT  9727,'HALIBURTON,MUSKOKA'                                        
         MRKT  9728,'ST CATHARINES UA'                                          
         MRKT  9729,'CHARLOTTETOWN EM'                                          
         MRKT  9730,'ROUYN EM'                                                  
         MRKT  9731,'YORK,SUND,QUEENS'                                          
         MRKT  9732,'OKANAGAN VALL'                                             
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* MDFMKTS                                                                       
         SMTBL MT                          MEDIA FAX                            
         MRKT  0980,'P.R. TOTAL ISLAND'                                         
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* CSIMKTS                                                                       
         SMTBL NC                    CANADIAN NIELSEN TV                        
         MRKT  0412,'EDMONTON METRO'                                            
         MRKT  0414,'MONTREAL ANGLO'                                            
         MRKT  0415,'MONTREAL FRANCO'                                           
         MRKT  0416,'OTTAWA ANGLO'                                              
         MRKT  0423,'CANCOM TORONTO'                                            
         MRKT  0424,'CANCOM EDMONTON'                                           
         MRKT  0426,'CANCOM VANCOUVER'                                          
         MRKT  0427,'RED DEER'                                                  
         MRKT  0429,'BRANDON'                                                   
         MRKT  0430,'HALIFAX'                                                   
         MRKT  0431,'MONCTON-ST. JOHN'                                          
         MRKT  0432,'SYDNEY/GLACE BAY'                                          
         MRKT  0433,'NEWFOUNLAND/ST. JOHNS'                                     
         MRKT  0434,'CHARLOTTETOWN'                                             
         MRKT  0435,'KENORA'                                                    
         MRKT  0440,'TORONTO'                                                   
         MRKT  0441,'MONTREAL'                                                  
         MRKT  0443,'QUEBEC'                                                    
         MRKT  0444,'OTTAWA'                                                    
         MRKT  0445,'LONDON'                                                    
         MRKT  0446,'SUDBURY/TIMMINS/N.BAY'                                     
         MRKT  0447,'KITCHENER'                                                 
         MRKT  0448,'RIMOUSKI/MATANE'                                           
         MRKT  0449,'CHICOUTIMI/JONQUIERE'                                      
         MRKT  0450,'TROIS RIVIERIES'                                           
         MRKT  0451,'SHERBROOKE'                                                
         MRKT  0452,'ROUYN-NORANDA'                                             
         MRKT  0453,'CARLETON'                                                  
         MRKT  0454,'KINGSTON'                                                  
         MRKT  0456,'PETERBOROUGH'                                              
         MRKT  0457,'BARRIE'                                                    
         MRKT  0458,'WINGHAM'                                                   
         MRKT  0459,'WINDSOR'                                                   
         MRKT  0460,'SAULT ST. MARIE'                                           
         MRKT  0461,'THUNDER BAY'                                               
         MRKT  0463,'RIVIERE DU LOUP'                                           
         MRKT  0464,'PEMBROKE'                                                  
         MRKT  0470,'WINIPEG/BRANDON'                                           
         MRKT  0471,'REGINA/MOOSE JAW'                                          
         MRKT  0472,'YORKTON'                                                   
         MRKT  0474,'SASKATOON'                                                 
         MRKT  0475,'PRINCE ALBERT'                                             
         MRKT  0476,'SWIFT CURRENT'                                             
         MRKT  0477,'MARITIMES'                                                 
         MRKT  0478,'OTTAWA FRANCO'                                             
         MRKT  0480,'LETHBRIDGE'                                                
         MRKT  0482,'EDMONTON'                                                  
         MRKT  0483,'DAWSON CREEK'                                              
         MRKT  0484,'CALGARY/LETHBRIDGE'                                        
         MRKT  0486,'LLOYDMINSTER'                                              
         MRKT  0488,'MEDICINE HAT'                                              
         MRKT  0490,'VANCOUVER'                                                 
         MRKT  0491,'KITCHENER/LONDON'                                          
         MRKT  0492,'KELOWNA/KAMLOOPS'                                          
         MRKT  0494,'PRINCE GEORGE'                                             
         MRKT  0495,'SUDBURY/TIMM/NB/S.S.MARIE'                                 
         MRKT  0496,'TERRACE-KITIMAT'                                           
         MRKT  0498,'SASKATCHEWAN'                                              
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* BIRRMKT                                                                       
         SMTBL NR                    BIRCH RADIO                                
         MRKT  0001,'NEW YORK NY-NJ-CT CMSA'                                    
         MRKT  0003,'LOS ANGELES CA BDM'                                        
         MRKT  0005,'CHICAGO IL CMSA'                                           
         MRKT  0007,'PHILADELPHIA PA-NJ PMSA'                                   
         MRKT  0009,'SAN FRANCISCO CA CMSA'                                     
         MRKT  0011,'DETROIT MI PMSA'                                           
         MRKT  0013,'BOSTON MA BDM'                                             
         MRKT  0015,'WASHINGTON D. C. MD-VA MSA'                                
         MRKT  0017,'ST.LOUIS MO-IL MSA'                                        
         MRKT  0019,'CLEVELAND OH BDM'                                          
         MRKT  0021,'BALTIMORE MD MSA'                                          
         MRKT  0023,'PITTSBURGH/BVR VALLEY PA CMSA'                             
         MRKT  0024,'DALLAS/FT. WORTH TX BDM'                                   
         MRKT  0027,'MINNEAPOLIS/ST. PAUL MN-WI MSA'                            
         MRKT  0029,'MIAMI/HIALEAH FL PMSA'                                     
         MRKT  0031,'CINCINNATI OH-KY-IN PMSA'                                  
         MRKT  0033,'HOUSTON/GALV/BRAZORIA TX CMSA'                             
         MRKT  0035,'DENVER/BOULDER CO MSA'                                     
         MRKT  0037,'BUFFALO/NIAGARA FALLS NY CMSA'                             
         MRKT  0039,'SEATTLE/TACOMA WA BDM'                                     
         MRKT  0041,'KANSAS CITY MO-KS MSA'                                     
         MRKT  0043,'MILWAUKEE/RACINE WI CMSA'                                  
         MRKT  0045,'COLUMBUS OH MSA'                                           
         MRKT  0047,'ATLANTA GA MSA'                                            
         MRKT  0049,'INDIANAPOLIS IN MSA'                                       
         MRKT  0051,'PORTLAND/VNCVER OR-WA BDM'                                 
         MRKT  0053,'NEW ORLEANS LA MSA'                                        
         MRKT  0055,'LOUISVILLE KY-IN MSA'                                      
         MRKT  0057,'PHOENIX AZ MSA'                                            
         MRKT  0059,'SAN ANTONIO TX MSA'                                        
         MRKT  0061,'HARTFORD/N.BRIT/MDLTN CT BDM'                              
         MRKT  0062,'NEW HAVEN-MERIDEN CT MSA'                                  
         MRKT  0063,'SAN DIEGO CA MSA'                                          
         MRKT  0065,'SACRAMENTO CA MSA'                                         
         MRKT  0067,'DAYTON/SPRNGFLD OH MSA'                                    
         MRKT  0069,'ALBANY-SCNCTADY-TROY NY MSA'                               
         MRKT  0071,'DES MOINES IA MSA'                                         
         MRKT  0073,'NASHVILLE TN MSA'                                          
         MRKT  0075,'MEMPHIS TN-AR-MS MSA'                                      
         MRKT  0077,'PROVIDENCE RI-MA BDM'                                      
         MRKT  0079,'ROCHESTER, NY MSA'                                         
         MRKT  0081,'AKRON OH PMSA'                                             
         MRKT  0082,'CANTON OH MSA'                                             
         MRKT  0083,'OKLAHOMA CITY OK MSA'                                      
         MRKT  0085,'OMAHA NE-IA MSA'                                           
         MRKT  0087,'TAMPA-ST PETE-CLRWTER FL MSA'                              
         MRKT  0089,'FRESNO CA MSA'                                             
         MRKT  0091,'SYRACUSE NY MSA'                                           
         MRKT  0093,'CHRLT-GSTNIA-ROCK HILL NC-SC MSA'                          
         MRKT  0095,'BIRMINGHAM AL MSA'                                         
         MRKT  0097,'TOLEDO OH MSA'                                             
         MRKT  0099,'HONOLULU HI MSA'                                           
         MRKT  0101,'SALT LAKE/PROVO/OGDON UT BDM'                              
         MRKT  0103,'TULSA OK BDM'                                              
         MRKT  0105,'RICHMOND/PETERSBURG VA MSA'                                
         MRKT  0107,'JACKSONVILLE FL BDM'                                       
         MRKT  0109,'NRFLK-V.BEACH-NWPT NWS VA MSA'                             
         MRKT  0111,'SHREVEPORT LA BDM'                                         
         MRKT  0113,'WORCESTER MA MSA'                                          
         MRKT  0115,'RALEIGH-DURHAM NC MSA'                                     
         MRKT  0117,'SPRINFIELD, MA'                                            
         MRKT  0119,'HARRISBURG PA BDM'                                         
         MRKT  0121,'KNOXVILLE TN MSA'                                          
         MRKT  0123,'LITTLE ROCK AR MSA'                                        
         MRKT  0125,'WITCHITA KS MSA'                                           
         MRKT  0127,'GRAND RAPIDS MI MSA'                                       
         MRKT  0129,'YOUNGSTOWN/WARREN OH MSA'                                  
         MRKT  0131,'ORLANDO FL MSA'                                            
         MRKT  0133,'MOBILE AL MSA'                                             
         MRKT  0135,'AUSTIN TX MSA'                                             
         MRKT  0137,'PEORIA,IL. MSA'                                            
         MRKT  0141,'ALBUQUERQUE NM BDM'                                        
         MRKT  0143,'BAKERSFIELD CA MSA'                                        
         MRKT  0145,'ALLENTOWN-BETHLEHEM PA-NJ MSA'                             
         MRKT  0147,'AMARILLO TX MSA'                                           
         MRKT  0149,'BEAUMONT-PORT ARTHUR TX MSA'                               
         MRKT  0151,'CEDAR RAPIDS IA MSA'                                       
         MRKT  0153,'CHARLESTON, WV MSA'                                        
         MRKT  0155,'CORPUS CHRISTI TX MSA'                                     
         MRKT  0157,'DVNPRT/RCK ISL/MOL IA-IL MSA'                              
         MRKT  0159,'DULUTH/SUPERIOR MN-WI BDM'                                 
         MRKT  0161,'EL PASO TX MSA'                                            
         MRKT  0163,'FLINT MI MSA'                                              
         MRKT  0165,'FT. WAYNE IN MSA'                                          
         MRKT  0166,'GRNSBRO/WIN-SLM/HI PT NC MSA'                              
         MRKT  0169,'JACKSON MS MSA'                                            
         MRKT  0171,'MADISON WI MSA'                                            
         MRKT  0172,'MADISON WI TSA'                                            
         MRKT  0173,'MONTGOMERY AL MSA'                                         
         MRKT  0175,'SCRANTON/WILKES BARRE PA MSA'                              
         MRKT  0177,'SPOKANE WA-ID BDM'                                         
         MRKT  0179,'WHEELING WV-OH MSA'                                        
         MRKT  0181,'CHATTANOOGA TN-GA MSA'                                     
         MRKT  0183,'COLUMBIA, SC MSA'                                          
         MRKT  0185,'EVANSVILLE IN-KY MSA'                                      
         MRKT  0187,'FARGO-MOORHEAD ND-MN MSA'                                  
         MRKT  0189,'FT LAUD/HLLYWD/POMP BCH FL PMSA'                           
         MRKT  0191,'GREENVL-SPARTANBURG, SC MSA'                               
         MRKT  0193,'HUNTINGTON-ASHLAND WV-KY-OH MSA'                           
         MRKT  0195,'LANSING-E. LANSING MI MSA'                                 
         MRKT  0197,'PRTLND/LWSTON/AUBURN ME BDM'                               
         MRKT  0203,'SPRINGFIELD, MO MSA'                                       
         MRKT  0205,'TOPEKA KS MSA'                                             
         MRKT  0207,'TUCSON AZ MSA'                                             
         MRKT  0209,'WICHITA FALLS TX MSA'                                      
         MRKT  0215,'SAN JOSE CA PMSA'                                          
         MRKT  0219,'ALTOONA PA MSA'                                            
         MRKT  0221,'ASHEVILLE NC BDM'                                          
         MRKT  0223,'BATON ROUGE LA MSA'                                        
         MRKT  0225,'BILLINGS MT MSA'                                           
         MRKT  0227,'BINGHAMTON NY MSA'                                         
         MRKT  0229,'BOISE ID BDM'                                              
         MRKT  0231,'CHARLESTON, SC MSA'                                        
         MRKT  0233,'COLORADO SPRINGS CO MSA'                                   
         MRKT  0235,'COLUMBUS, GA-AL MSA'                                       
         MRKT  0239,'ERIE, PA. MSA'                                             
         MRKT  0241,'EUGENE-SPRINGFIELD OR MSA'                                 
         MRKT  0245,'GREEN BAY WI MSA'                                          
         MRKT  0247,'PORTSMTH-DOVER-ROCH NH-ME BDM'                             
         MRKT  0251,'KALAMAZOO MI MSA'                                          
         MRKT  0253,'LAFAYETTE, LA'                                             
         MRKT  0255,'LANCASTER,PA MSA'                                          
         MRKT  0257,'LAS VAGAS NV MSA'                                          
         MRKT  0259,'LEXINGTON/FAYETTE KY MSA'                                  
         MRKT  0263,'LUBBOCK TX MSA'                                            
         MRKT  0265,'MARKET 265 ???'                                            
         MRKT  0267,'MANCHESTER, NH. MSA'                                       
         MRKT  0269,'MCALLEN-BROWNSVILLE'                                       
         MRKT  0273,'READING PA. MSA'                                           
         MRKT  0275,'RENO NV BDM'                                               
         MRKT  0277,'ROANOKE VA MSA'                                            
         MRKT  0279,'ROCKFORD, IL MSA'                                          
         MRKT  0281,'SAGINAW/BAY CITY/MDLND MI MSA'                             
         MRKT  0283,'SALINAS-SEASIDE-MONTERY'                                   
         MRKT  0285,'SAVANNAH/BEAFRT/HLT GA-SC BDM'                             
         MRKT  0287,'SOUTH BEND/MISHAWAKA IN MSA'                               
         MRKT  0291,'STOCKTON'                                                  
         MRKT  0295,'UTICA/ROME NY MSA'                                         
         MRKT  0297,'WATERLOO-CEDAR FALLS IA MSA'                               
         MRKT  0299,'W PALM BCH/BOCA/DELRAY FL MSA'                             
         MRKT  0301,'YORK,PA. MSA'                                              
         MRKT  0303,'APPLETON/OSHKOSH/NEENAH WI MSA'                            
         MRKT  0305,'AUGUSTA, GA-SC MSA'                                        
         MRKT  0307,'TERRE HAUTE IN MSA'                                        
         MRKT  0309,'WACO TX MSA'                                               
         MRKT  0311,'LAKELND/WNTER HAVEN FL MSA'                                
         MRKT  0315,'ANCHORAGE AK MSA'                                          
         MRKT  0317,'PENSACOLA FL MSA'                                          
         MRKT  0319,'SIOUX FALLS'                                               
         MRKT  0321,'NASSAU-SUFFOLK NY PMSA'                                    
         MRKT  0323,'BLOOMINGTON/NORMAL'                                        
         MRKT  0325,'YAKIMA'                                                    
         MRKT  0327,'HUNTSVILLE AL BDM'                                         
         MRKT  0331,'MELBRNE/TITUS PALM BAY FL'                                 
         MRKT  0333,'TALLAHASSEE FL MSA'                                        
         MRKT  0337,'BRIDGEPORT/MILFORD CT PMSA'                                
         MRKT  0339,'MEDFORD, OR MSA'                                           
         MRKT  0341,'DAYTONA BEACH FL MSA'                                      
         MRKT  0343,'MODESTO CA MSA'                                            
         MRKT  0345,'JOHNSON CTY-KNGSPRT-BRSTL TN-VA MSA'                       
         MRKT  0353,'PUEBLO'                                                    
         MRKT  0357,'BURLINGTON, VT. BDM'                                       
         MRKT  0363,'LYNCHBURG, VA. BDM'                                        
         MRKT  0365,'NEW BDFRD-FALL RVR, MA BDM'                                
         MRKT  0367,'ATLANTIC CITY NJ MSA'                                      
         MRKT  0369,'CASPER WY MSA'                                             
         MRKT  0371,'RICHLAND/KENNEWICK/PASCO WA MSA'                           
         MRKT  0373,'SARASOTA FL MSA'                                           
         MRKT  0379,'RIVERSIDE-S.BERNADINO CA BDM'                              
         MRKT  0429,'MIAMI-FT.LAUDERDALE FL CMSA'                               
         MRKT  0502,'TYLER, TX MSA'                                             
         MRKT  0503,'FAYETTEVILLE/SPRINGDALE AR'                                
         MRKT  0506,'LONGVIEW-MARSHALL,TX MSA'                                  
         MRKT  0508,'CHICO, CA MSA'                                             
         MRKT  0509,'REDDING, CA MSA'                                           
         MRKT  0512,'TEXARKANA TX-AR MSA'                                       
         MRKT  0515,'FT. MYERS/NAPLES/PORT CH'                                  
         MRKT  0516,'MONMOUTH-OCEAN, NJ PMSA'                                   
         MRKT  0517,'FT. PIERCE, FL BDM'                                        
         MRKT  0518,'POUGHKEEPSIE NY MSA'                                       
         MRKT  0521,'SPRINGFIELD IL MSA'                                        
         MRKT  0522,'LAUREL/HATTIESBURG'                                        
         MRKT  0524,'HAGERS/CHMBERS/WYNS MD-PA BDM'                             
         MRKT  0528,'FREDERICK, MD'                                             
         MRKT  0530,'CAPE COD, MA BDM'                                          
         MRKT  0532,'ROCHESTER, MN MSA'                                         
         MRKT  0534,'JOPLIN MO MSA'                                             
         MRKT  0539,'PARKERSBURG-MARIETTA WV/OH'                                
         MRKT  0544,'JOHNSTOWN PA MSA'                                          
         MRKT  0546,'ABILENE TX BDM'                                            
         MRKT  0547,'STAMFORD/NORWALK CT BDM'                                   
         MRKT  0548,'SAN ANGELO'                                                
         MRKT  0550,'GAINESVILLE, FL BDM'                                       
         MRKT  0551,'VICTORIA, TX MSA'                                          
         MRKT  0553,'CHARLOTTESVILLE, VA MSA'                                   
         MRKT  0555,'STAUNTON/WAYNESBORO'                                       
         MRKT  0559,'LA CROSSE WI MSA'                                          
         MRKT  0560,'CHEYENNE WY MSA'                                           
         MRKT  0561,'ODESSA-MIDLAND, TX BDM'                                    
         MRKT  0562,'KILLEEN-TEMPLE, TX MSA'                                    
         MRKT  0564,'COLUMBIA MO MSA'                                           
         MRKT  0570,'DUBUQUE, IA MSA'                                           
         MRKT  0571,'FT. WALTON BCH, FL MSA'                                    
         MRKT  0573,'PANAMA CITY, FL MSA'                                       
         MRKT  0574,'SANTA ROSA, CA'                                            
         MRKT  0578,'WILLIAMSPORT, PA MSA'                                      
         MRKT  0580,'ALBANY GA MSA'                                             
         MRKT  0585,'MONROE'                                                    
         MRKT  0586,'EAU CLAIRE WI MSA'                                         
         MRKT  0587,'ALEXANDRIA, LA MSA'                                        
         MRKT  0588,'MERIDIAN MS BDM'                                           
         MRKT  0589,'FT. SMITH'                                                 
         MRKT  0591,'SANTA BARBARA, CA BDM'                                     
         MRKT  0592,'PALM SPRINGS, CA'                                          
         MRKT  0593,'DANBURY, CT'                                               
         MRKT  0594,'OXNARD-VENTURA CA BDM'                                     
         MRKT  0596,'SAN JOSE CA HISPANIC',BOOKTYPE=H                           
         MRKT  0597,'OMAHA NE-IA-KS-MO TSA'                                     
         MRKT  0598,'BILOXI/GULFPORT MS BDM'                                    
         MRKT  0599,'WINNEBAGO COUNTY'                                          
         MRKT  0601,'LITCHFIELD COUNTY'                                         
         MRKT  0604,'CENTRAL UPPER MICH. BDM'                                   
         MRKT  0609,'NORTHERN LOWER MICH BDM'                                   
         MRKT  0611,'MONMOUTH-OCEAN, NJ PMSA'                                   
         MRKT  0617,'TRAVERSE CITY/CADILLAC MI BDM'                             
         MRKT  0618,'PADUCAH, KY. TA'                                           
         MRKT  0623,'FLORENCE AL MSA'                                           
         MRKT  0625,'MYRTLE BEACH SC BDM'                                       
         MRKT  0626,'SAN LOUIS OBISPO CA BDM'                                   
         MRKT  0627,'BANGOR, ME. BDM'                                           
         MRKT  0628,'ELKHART/GOSHEN IN MSA'                                     
         MRKT  0629,'HARRISONBURG VA BDM'                                       
         MRKT  0630,'FLORENCE SC MSA'                                           
         MRKT  0632,'WICHITA KS. TSA'                                           
         MRKT  0636,'EUREKA/ARCATA'                                             
         MRKT  0639,'CORNING/ELMIRA NY BDM'                                     
         MRKT  0643,'SANTA MARIA/LOMPOC CA BDM'                                 
         MRKT  0645,'VALDOSTA GA BDM'                                           
         MRKT  0650,'JEFFERSON CITY, MO. BDM'                                   
         MRKT  0654,'STOCKTON, CA. MSA'                                         
         MRKT  0659,'LIMA'                                                      
         MRKT  0660,'WAUSAU WI MSA'                                             
         MRKT  0662,'BLUEFIELD WV-VA BDM'                                       
         MRKT  0665,'SHREVEPORT, LA. TSA'                                       
         MRKT  0666,'BENTON HARBOR,MI. MSA'                                     
         MRKT  0668,'SPRINGFIELD, MO. TSA'                                      
         MRKT  0674,'MORGANTWN-CLARKS-FAIR WV BDM'                              
         MRKT  0676,'FRESNO, CA. TSA'                                           
*ALSO    MRKT  0676,'PLYMTH/BRISTOL CNTYS MA BDM'                               
         MRKT  0680,'RUSS/BWL GR/HPKNS/CLRKS KY/TN BDM'                         
         MRKT  0681,'FAYETTEVILLE, NC. BDM'                                     
         MRKT  0683,'SALISBURY/OCEAN CITY MD-DE BDM'                            
         MRKT  0685,'OCALA FL MSA'                                              
         MRKT  0688,'SPRINGFIELD, MA. BDM'                                      
         MRKT  0692,'FT. COLLINS/GREELEY/LVLND CO BDM'                          
         MRKT  0693,'BRIDGEPORT, CT. BDM'                                       
         MRKT  0694,'FAIRBANKS AK BDM'                                          
*WAS     MRKT  0694,'JACKSON, MS. MSA'                                          
         MRKT  0695,'EASTERN LONG IS. NY BDM'                                   
         MRKT  0703,'WAUSAU/RHINELANDER WI BDM'                                 
         MRKT  0704,'GREENVILLE/N BERN/JKVILL NV'                               
         MRKT  0705,'WINCHESTER, VA. BDM'                                       
         MRKT  0707,'YUBA CITY, CA. MSA'                                        
         MRKT  0708,'MONTEREY/SALINAS/S.JOSE CA CMSA'                           
         MRKT  0709,'SUMTER, SC BDM'                                            
         MRKT  0712,'BOSTON ADI'                                                
         MRKT  0713,'LAREDO, TX. MSA'                                           
         MRKT  0714,'BANNOCK COUNTY ID BDM'                                     
         MRKT  0715,'DOTHAN AL BDM'                                             
         MRKT  0716,'BRECKENRIDGE/VAIL CO BDM'                                  
         MRKT  0717,'NE MICHIGAN BDM'                                           
         MRKT  0718,'HARTFORD/NEW HAVEN ADI'                                    
         MRKT  0720,'GLENS FALLS NY BDM'                                        
         MRKT  0722,'AKRON OH TSA'                                              
         MRKT  0723,'WILMINGTON'                                                
         MRKT  0726,'FT.MYERS/NPLS/PRT CHRLTE FL BDM'                           
         MRKT  0729,'NEW LONDON CT BDM'                                         
         MRKT  0730,'PLYMTH/BARNSTBLE CNTY MA BDM'                              
         MRKT  0731,'PLYMTH/NORFLK/BARN/BRIST MA BDM'                           
         MRKT  0733,'PLYMOUTH/NORFOLK CNTY MA BDM'                              
         MRKT  0734,'JOPLIN MO MSA'                                             
         MRKT  0741,'POCATELLO ID BDM'                                          
         MRKT  0742,'IDAHO FALLS ID BDM'                                        
         MRKT  0743,'QUINCY/HANNIBAL IL-MO BDM'                                 
         MRKT  0745,'MIDDLETOWN NY BDM'                                         
         MRKT  0746,'CHAMPAIGN'                                                 
         MRKT  0749,'DECATUR IL MSA'                                            
         MRKT  0751,'COL. SPRINGS/PUEBLO BDM'                                   
         MRKT  0755,'ORANGE COUNTY CA BDM'                                      
         MRKT  0760,'HOUSTON HTMR',BOOKTYPE=H                                   
*        MRKT  0767,'CORPUS CHRISTI HTMR',BOOKTYPE=H                            
*        MRKT  0768,'SOUTH FL. HTMR',BOOKTYPE=H                                 
*        MRKT  0769,'LOS ANGELES HTMR',BOOKTYPE=H                               
         MRKT  0776,'TOPEKA KS. BDM'                     WAS 205                
         MRKT  0777,'NEWTON NJ BDM'                                             
*        MRKT  0783,'SAN ANTONIO HTMR',BOOKTYPE=H                               
         MRKT  0786,'EAST IDAHO BDM'                                            
         MRKT  0800,'TORONTO CMA'                                               
*        MRKT  0832,'FRESNO HTMR',BOOKTYPE=H                                    
*        MRKT  0833,'EL PASO HTMR',BOOKTYPE=H                                   
*        MRKT  0836,'NEW YORK HTMR',BOOKTYPE=H                                  
         MRKT  0845,'KANSAS CITY MO-KS MSA BLACK',BOOKTYPE=B                    
*        MRKT  0851,'SAN DIEGO HTMR',BOOKTYPE=H                                 
*        MRKT  0854,'DALLAS HTMR',BOOKTYPE=H                                    
         MRKT  0857,'FARGO/MOORHEAD ND-MN-SD TSA'                               
         MRKT  0858,'LINCOLN NE MSA'                                            
         MRKT  0861,'FLINT MI MSA BLACK',BOOKTYPE=B                             
         MRKT  0869,'MC ALLEN/BRWNSVL TX BDM HISP',BOOKTYPE=H                   
         MRKT  0876,'LAKE CHARLES LA MSA'                                       
*        MRKT  0883,'CHICAGO HTMR',BOOKTYPE=H                                   
         MRKT  0893,'SAN LUIS/S.MARIA/LOMPOC CA BDM'                            
         MRKT  0900,'ATLANTA BLACK',BOOKTYPE=B                                  
         MRKT  0901,'AUGUSTA GA-SC MSA BLACK',BOOKTYPE=B                        
         MRKT  0902,'BALTIMORE BLACK',BOOKTYPE=B                                
         MRKT  0903,'BATON ROUGE BLACK',BOOKTYPE=B                              
         MRKT  0904,'BEAUMONT/P. ARTHUR BLACK',BOOKTYPE=B                       
         MRKT  0905,'BIRMINGHAM BLACK',BOOKTYPE=B                               
         MRKT  0906,'CHARLESTON BLACK',BOOKTYPE=B                               
         MRKT  0907,'CHARLOTTE BLACK',BOOKTYPE=B                                
         MRKT  0908,'CHATTANOOGA TN-GA BLACK',BOOKTYPE=B                        
         MRKT  0909,'CHICAGO BLACK',BOOKTYPE=B                                  
         MRKT  0910,'CLEVELAND BLACK',BOOKTYPE=B                                
         MRKT  0911,'COLUMBIA BLACK',BOOKTYPE=B                                 
         MRKT  0912,'DALLAS BLACK',BOOKTYPE=B                                   
         MRKT  0913,'DETRIOT BLACK',BOOKTYPE=B                                  
*        MRKT  0914,'DALLAS/FT WORTH BLACK',BOOKTYPE=B                          
         MRKT  0915,'GREENSBORO BLACK',BOOKTYPE=B                               
         MRKT  0916,'HOUSTON BLACK',BOOKTYPE=B                                  
         MRKT  0917,'INDIANAPOLIS BLACK',BOOKTYPE=B                             
         MRKT  0918,'JACKSONVILLE FL BLACK',BOOKTYPE=B                          
         MRKT  0919,'LOS ANGELES BLACK',BOOKTYPE=B                              
         MRKT  0920,'MEMPHIS BLACK',BOOKTYPE=B                                  
         MRKT  0921,'MIAMI/FT LAUDERDALE BLACK',BOOKTYPE=B                      
         MRKT  0922,'MIAMI/HIALEAH BLACK',BOOKTYPE=B                            
         MRKT  0923,'MOBILE AL MSA BLACK',BOOKTYPE=B                            
         MRKT  0924,'MONTGOMERY BLACK',BOOKTYPE=B                               
         MRKT  0925,'NASHVILLE TN MSA BLACK',BOOKTYPE=B                         
         MRKT  0926,'NEW ORLEANS BLACK',BOOKTYPE=B                              
         MRKT  0927,'NEW YORK BLACK',BOOKTYPE=B                                 
         MRKT  0928,'NORFOLK/VA BCH/NWPRT NWS BLACK',BOOKTYPE=B                 
         MRKT  0929,'ORLANDO FL MSA BLACK',BOOKTYPE=B                           
         MRKT  0930,'PHILADELPHIA BLACK',BOOKTYPE=B                             
         MRKT  0931,'RALEIGH/DURHAM NC MSA BLACK',BOOKTYPE=B                    
         MRKT  0932,'RICHMOND BLACK',BOOKTYPE=B                                 
         MRKT  0933,'ST. LOUIS BLACK',BOOKTYPE=B                                
         MRKT  0934,'SHREVEPORT LA BDM BLACK',BOOKTYPE=B                        
         MRKT  0935,'WASHINGTON BLACK',BOOKTYPE=B                               
         MRKT  0936,'W. PALM BCH/BOCA/DLR BLACK',BOOKTYPE=B                     
*WAS     MRKT  0937,'ALBUQUERQUE BLACK',BOOKTYPE=B                              
         MRKT  0937,'ALBUQUERQUE NM BDM HISPANIC',BOOKTYPE=H                    
         MRKT  0938,'AUSTIN HISPANIC',BOOKTYPE=H                                
         MRKT  0939,'EL PASO TX MSA HISPANIC',BOOKTYPE=H                        
         MRKT  0940,'FRESNO CA MSA HISPANIC',BOOKTYPE=H                         
         MRKT  0941,'HOUSTON TX CMSA HISPANIC',BOOKTYPE=H                       
         MRKT  0942,'NEW YORK NY-NJ-CT CMSA HISP',BOOKTYPE=H                    
         MRKT  0943,'CHICAGO HISPANIC',BOOKTYPE=H                               
         MRKT  0944,'LOS ANGELES CA BDM HISPANIC',BOOKTYPE=H                    
*        MRKT  0945,'SOUTH FLORIDA HISPANIC',BOOKTYPE=H                         
         MRKT  0945,'MIAMI/HIALEAH FL PMSA HISP',BOOKTYPE=H                     
         MRKT  0946,'MIAMI/HIALEAH FL PMSA HISP',BOOKTYPE=H                     
*        MRKT  0947,'MODESTO HISPANIC',BOOKTYPE=H                               
*        MRKT  0948,'RIVERSIDE HISPANIC',BOOKTYPE=H                             
         MRKT  0949,'PHOENIX AZ MSA HISPANIC',BOOKTYPE=H                        
         MRKT  0950,'SAN ANTONIO TX MSA HISPANIC',BOOKTYPE=H                    
         MRKT  0951,'SAN DIEGO HISPANIC',BOOKTYPE=H                             
         MRKT  0952,'SAN FRANCISCO HISPANIC',BOOKTYPE=H                         
         MRKT  0953,'SAN JOSE HISPANIC',BOOKTYPE=H                              
         MRKT  0954,'STOCKTON HISPANIC',BOOKTYPE=H                              
         MRKT  0955,'DALLAS/FT WORTH BDM HISP',BOOKTYPE=H                       
*        MRKT  0956,'SALINAS HISPANIC',BOOKTYPE=H                               
         MRKT  0957,'CORPUS CHRISTI TX MSA HISPANIC',BOOKTYPE=H                 
*        MRKT  0958,'LITTLE ROCK BLACK',BOOKTYPE=B                              
*        MRKT  0959,'FLINT MI BLACK',BOOKTYPE=B                                 
         MRKT  0961,'MILWAUKEE/RACINE CMSA BLACK',BOOKTYPE=B                    
         MRKT  0962,'GREENVILLE/SPRTNBURG BLACK',BOOKTYPE=B                     
         MRKT  0963,'HARTFORD/NEW HAVEN ADI BLACK',BOOKTYPE=B                   
         MRKT  0964,'LITTLE ROCK BLACK',BOOKTYPE=B                              
         MRKT  0965,'SAN FRANCISCO BLACK',BOOKTYPE=B                            
         MRKT  0983,'MARQUETTE'                                                 
*        MRKT  0986,'SAN FRANCISCO HTMR',BOOKTYPE=H                             
*        MRKT  0987,'PHOENIX HTMR',BOOKTYPE=H                                   
         MRKT  0990,'GRAND JUNCTION CO BDM'                                     
         MRKT  0992,'JACKSON, TN'                                               
         MRKT  0995,'SACRA/STOCKTON CA BDM HISP',BOOKTYPE=H                     
         MRKT  0996,'SAN JOSE CA PMSA HISP',BOOKTYPE=H                          
         MRKT  0998,'MODESTO CA TSA'                                            
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
*IRO     SMTBL NO                    BIRCH RADIO                                
*        MRKT  0001,'NEW YORK'                                                  
*        MRKT  0003,'LOS ANGELES'                                               
*        MRKT  0005,'CHICAGO'                                                   
*        MRKT  0007,'PHILADELPHIA'                                              
*        MRKT  0009,'SAN FRANCISCO'                                             
*        MRKT  0011,'DETROIT'                                                   
*        MRKT  0013,'BOSTON'                                                    
*        MRKT  0015,'WASHINGTON D. C.'                                          
*        MRKT  0017,'ST.LOUIS'                                                  
*        MRKT  0019,'CLEVELAND'                                                 
*        MRKT  0021,'BALTIMORE'                                                 
*        MRKT  0023,'PITTSBURGH'                                                
*        MRKT  0024,'DALLAS-FT. WORTH'                                          
*        MRKT  0027,'MINNEAPOLIS-ST. PAUL'                                      
*        MRKT  0029,'MIAMI TAR'                                                 
*        MRKT  0031,'CINCINNATI'                                                
*        MRKT  0033,'HOUSTON-GALVETON'                                          
*        MRKT  0035,'DENVER-BOULDER'                                            
*        MRKT  0037,'BUFFALO-NIAGARA FALLS'                                     
*        MRKT  0039,'SEATTLE-TACOMA'                                            
*        MRKT  0041,'KANSAS CITY'                                               
*        MRKT  0043,'MILWAUKEE-RACINE'                                          
*        MRKT  0045,'COLUMBUS OH'                                               
*        MRKT  0047,'ATLANTA'                                                   
*        MRKT  0049,'INDIANAPOLIS'                                              
*        MRKT  0051,'PORTLAND, OR'                                              
*        MRKT  0053,'NEW ORLEANS'                                               
*        MRKT  0055,'LOUISVILLE'                                                
*        MRKT  0057,'PHOENIX'                                                   
*        MRKT  0059,'SAN ANTONIO'                                               
*        MRKT  0061,'HARTFORD-N.BRITAIN-MDLTN'                                  
*        MRKT  0062,'NEW HAVEN-MERIDEN'                                         
*        MRKT  0063,'SAN DIEGO'                                                 
*        MRKT  0065,'SACRAMENTO'                                                
*        MRKT  0067,'DAYTON'                                                    
*        MRKT  0069,'ALBANY-SCNCTADY-TROY'                                      
*        MRKT  0071,'DES MOINES'                                                
*        MRKT  0073,'NASHVILLE'                                                 
*        MRKT  0075,'MEMPHIS'                                                   
*        MRKT  0077,'PRVIDENCE-WARWICK-PAWTUCKET'                               
*        MRKT  0079,'ROCHESTER, NY'                                             
*        MRKT  0081,'AKRON'                                                     
*        MRKT  0082,'CANTON'                                                    
*        MRKT  0083,'OKLAHOMA CITY'                                             
*        MRKT  0085,'OMAHA-COUNCIL BLUFFS'                                      
*        MRKT  0087,'TAMPA-ST. PETE-CLEARWATER'                                 
*        MRKT  0089,'FRESNO'                                                    
*        MRKT  0091,'SYRACUSE'                                                  
*        MRKT  0093,'CHRLT-GASTONIA-ROCK HILL'                                  
*        MRKT  0095,'BIRMINGHAM'                                                
*        MRKT  0097,'TOLEDO'                                                    
*        MRKT  0099,'HONOLULU'                                                  
*        MRKT  0101,'SALT LAKE-OGDEN'                                           
*        MRKT  0103,'TULSA'                                                     
*        MRKT  0105,'RICHMOND'                                                  
*        MRKT  0107,'JACKSONVILLE'                                              
*        MRKT  0109,'NORFOLK-V.BEACH-NWPT NWS'                                  
*        MRKT  0111,'SHREVEPORT'                                                
*        MRKT  0113,'WORCESTER'                                                 
*        MRKT  0115,'RALEIGH-DURHAM'                                            
*        MRKT  0117,'SPRINFIELD, MO'                                            
*        MRKT  0119,'HARSBRG-LEBANON-CALISLE'                                   
*        MRKT  0121,'KNOXVILLE'                                                 
*        MRKT  0123,'LITTLE ROCK'                                               
*        MRKT  0125,'WITCHITA'                                                  
*        MRKT  0127,'GRAND RAPIDS'                                              
*        MRKT  0129,'YOUNGSTOWN-WARREN'                                         
*        MRKT  0131,'ORLANDO'                                                   
*        MRKT  0133,'MOBILE'                                                    
*        MRKT  0135,'AUSTIN'                                                    
*        MRKT  0137,'PEORIA'                                                    
*        MRKT  0139,'WILMINGTON, DE.'                                           
*        MRKT  0141,'ALBUQUERQUE'                                               
*        MRKT  0143,'BAKERSFIELD'                                               
*        MRKT  0145,'ALLENTOWN-BETHLEHEM'                                       
*        MRKT  0147,'AMARILLO'                                                  
*        MRKT  0149,'BEAUMONT-PORT ARTHUR'                                      
*        MRKT  0151,'CEDAR RAPIDS'                                              
*        MRKT  0153,'CHARLESTON, WV'                                            
*        MRKT  0155,'CORPUS CHRISTI'                                            
*        MRKT  0157,'QUAD CITIES'                                               
*        MRKT  0159,'DULUTH-SUPERIOR'                                           
*        MRKT  0161,'EL PASO'                                                   
*        MRKT  0163,'FLINT'                                                     
*        MRKT  0165,'FT. WAYNE'                                                 
*        MRKT  0166,'GREENBORO-W.SLM-HIGH PT'                                   
*        MRKT  0169,'JACKSON'                                                   
*        MRKT  0171,'MADISON'                                                   
*        MRKT  0172,'MADISON WI TSA'                                            
*        MRKT  0173,'MONTGOMERY'                                                
*        MRKT  0175,'WILKES BARRE-SCANTON'                                      
*        MRKT  0177,'SPOKANE'                                                   
*        MRKT  0179,'WHEELING'                                                  
*        MRKT  0181,'CHATTANOOGA'                                               
*        MRKT  0183,'COLUMBIA, SC'                                              
*        MRKT  0185,'EVANSVILLE'                                                
*        MRKT  0187,'FARGO-MOORHEAD'                                            
*        MRKT  0189,'FT. LAUDERDALE TAR'                                        
*        MRKT  0191,'GREENVL-SPARTANBURG, SC'                                   
*        MRKT  0193,'HUNTINGTON-ASHLAND'                                        
*        MRKT  0195,'LANSING-E. LANSING'                                        
*        MRKT  0197,'PORTLAND, ME'                                              
*        MRKT  0203,'SPRINGFIELD, MO'                                           
*        MRKT  0205,'TOPEKA'                                                    
*        MRKT  0207,'TUCSON'                                                    
*        MRKT  0209,'WICHITA FALLS, TX'                                         
*        MRKT  0215,'SAN JOSE'                                                  
*        MRKT  0219,'ALTOONA'                                                   
*        MRKT  0221,'ASHEVILLE'                                                 
*        MRKT  0223,'BATON ROUGE'                                               
*        MRKT  0225,'BILLINGS'                                                  
*        MRKT  0227,'BINGHAMTON'                                                
*        MRKT  0229,'BOISE'                                                     
*        MRKT  0231,'CHARLESTON, SC'                                            
*        MRKT  0233,'COLORADO SPRINGS'                                          
*        MRKT  0235,'COLUMBUS, GA'                                              
*        MRKT  0239,'ERIE'                                                      
*        MRKT  0241,'EUGENE-SPRINGFIELD'                                        
*        MRKT  0243,'GREAT FALLS, MT'                                           
*        MRKT  0245,'GREEN BAY'                                                 
*        MRKT  0247,'PORTSMTH-DOVER-ROCHESTER'                                  
*        MRKT  0251,'KALAMAZOO'                                                 
*        MRKT  0253,'LAFAYETTE, LA'                                             
*        MRKT  0255,'LANCASTER,PA MSA'                                          
*        MRKT  0257,'LAS VAGAS'                                                 
*        MRKT  0259,'LEXINGTON/FAYETTE,KY'                                      
*        MRKT  0261,'LINCOLN'                                                   
*        MRKT  0263,'LUBBOCK'                                                   
*        MRKT  0265,'MACON'                                                     
*        MRKT  0267,'MANCHESTER'                                                
*        MRKT  0269,'MCALLEN-BROWNSVILLE'                                       
*        MRKT  0271,'SALISBURY-OCEAN CITY'                                      
*        MRKT  0273,'READING PA.'                                               
*        MRKT  0275,'RENO'                                                      
*        MRKT  0277,'ROANOKE-LYNCHBURG'                                         
*        MRKT  0279,'ROCKFORD'                                                  
*        MRKT  0281,'SAGINAW-BAY CITY-MDLND'                                    
*        MRKT  0283,'SALINAS-SEASIDE-MONTERY'                                   
*        MRKT  0285,'SAVANNAH'                                                  
*        MRKT  0287,'SOUTH BEND'                                                
*        MRKT  0291,'STOCKTON'                                                  
*        MRKT  0295,'UTICA-ROME'                                                
*        MRKT  0297,'WATERLOO-CEDAR FALLS'                                      
*        MRKT  0299,'WEST PALM BCH-BOCA ROTAN'                                  
*        MRKT  0301,'YORK'                                                      
*        MRKT  0303,'APPLETON-OSHKOSH'                                          
*        MRKT  0305,'AUGUSTA, GA'                                               
*        MRKT  0307,'TERRE HAUTE'                                               
*        MRKT  0309,'WACO'                                                      
*        MRKT  0311,'LAKELAND-WINTER HAVEN'                                     
*        MRKT  0313,'SAN DIEGO N. COUNTY'                                       
*        MRKT  0315,'ANCHORAGE'                                                 
*        MRKT  0317,'PENSACOLA'                                                 
*        MRKT  0319,'SIOUX FALLS'                                               
*        MRKT  0321,'NASSAU-SUFFOLK(LI, NY)'                                    
*        MRKT  0323,'BLOOMINGTON'                                               
*        MRKT  0325,'YAKIMA'                                                    
*        MRKT  0327,'HUNTSVILLE'                                                
*        MRKT  0331,'MELBRN-TITUSVILLE-COCOA'                                   
*        MRKT  0333,'TALLAHASSEE'                                               
*        MRKT  0337,'BRIDGEPORT'                                                
*        MRKT  0339,'MEDFORD-ASHLAND, OR'                                       
*        MRKT  0341,'DAYTONA BEACH'                                             
*        MRKT  0343,'MODESTO'                                                   
*        MRKT  0345,'JOHNSON CTY-KNGSPRT-BRSTL'                                 
*        MRKT  0351,'ANAHEIM-SANTA ANA'                                         
*        MRKT  0353,'PUEBLO'                                                    
*        MRKT  0357,'BURLINGTON, VT. MA'                                        
*        MRKT  0359,'FAYETTEVILLE, NC'                                          
*        MRKT  0361,'GREENVL-N.BERN-WASH'                                       
*        MRKT  0363,'LYNCHBURG, VA. TA'                                         
*        MRKT  0365,'NEW BDFRD-FALL RVR, MA'                                    
*        MRKT  0367,'ATLANTIC CITY'                                             
*        MRKT  0369,'CASPER'                                                    
*        MRKT  0371,'TRI-CITIES'                                                
*        MRKT  0373,'SARASOTA-BRADENTON'                                        
*        MRKT  0375,'STUEBENVL-WEIRTON'                                         
*        MRKT  0377,'WILMINGTON, NC'                                            
*        MRKT  0379,'RIVERSIDE-S.BERNADINO'                                     
*        MRKT  0429,'MIAMI-FT.LAUD-HOLLYWOOD'                                   
*        MRKT  0485,'LA CONSOLIDATED TAR'                                       
*        MRKT  0501,'DOTHAN, AL'                                                
*        MRKT  0502,'TYLER, TX'                                                 
*        MRKT  0503,'FAYETTVL-SPRNGDALE, AR'                                    
*        MRKT  0505,'BURLINGTON, VT'                                            
*        MRKT  0506,'LONGVIEW-MARSHALL,TX'                                      
*        MRKT  0508,'CHICO, CA'                                                 
*        MRKT  0509,'REDDING, CA'                                               
*        MRKT  0510,'BIG SPRING, TX'                                            
*        MRKT  0511,'GRAND JUNCTION, CO'                                        
*        MRKT  0512,'TEXARKANA, TX'                                             
*        MRKT  0513,'MUSKEGON, MI'                                              
*        MRKT  0514,'WATERBURY, CT'                                             
*        MRKT  0515,'FT. MYERS-CAPE CORAL, FL'                                  
*        MRKT  0516,'MONMOUTH-OCEAN, NJ'                                        
*        MRKT  0517,'FT. PIERCE, FL'                                            
*        MRKT  0518,'POUGHKEEPSIE, NY'                                          
*        MRKT  0519,'ABERDEEN, WA'                                              
*        MRKT  0520,'NAPLES-MARCO ISL, FL'                                      
*        MRKT  0521,'SPRINGFIELD, IL'                                           
*        MRKT  0522,'LAUREL-HATTIESBURG, MS'                                    
*        MRKT  0523,'SIOUX CITY, IA'                                            
*        MRKT  0524,'HAGRSTWN-CHMBRSBURG-WANSEBORO'                             
*        MRKT  0525,'OWENSBORO, KY'                                             
*        MRKT  0526,'BANGOR, ME'                                                
*        MRKT  0527,'AUGUSTA-WATERVILLE, ME'                                    
*        MRKT  0528,'FREDERICK, MD'                                             
*        MRKT  0529,'PASCAGOULA-MOSS PT, MS'                                    
*        MRKT  0530,'CAPE COD, MA'                                              
*        MRKT  0531,'BATTLE CREEK, MI'                                          
*        MRKT  0532,'ROCHESTER, MN'                                             
*        MRKT  0533,'BILOXI-GULFPORT, MS'                                       
*        MRKT  0534,'JOPLIN, MO'                                                
*        MRKT  0535,'NW MICHIGAN'                                               
*        MRKT  0536,'BOZEMAN, MT'                                               
*        MRKT  0537,'TRENTON, NJ'                                               
*        MRKT  0538,'SANTA FE, NM'                                              
*        MRKT  0539,'PARKERSBURG-MARIETTA WV/OH'                                
*        MRKT  0540,'MINOT, ND'                                                 
*        MRKT  0544,'JOHNSTOWN'                                                 
*        MRKT  0545,'RAPID CITY, SD'                                            
*        MRKT  0546,'ABILENE, TX'                                               
*        MRKT  0547,'STAMFORD-NORWALK, CT'                                      
*        MRKT  0548,'SAN ANGELO, TX'                                            
*        MRKT  0550,'GAINESVILLE, FL'                                           
*        MRKT  0551,'VICTORIA, TX'                                              
*        MRKT  0552,' MARKET 552'                                               
*        MRKT  0553,'CHARLOTTESVILLE, VA'                                       
*        MRKT  0555,'STAUNTON-WAYNESBORO, VA'                                   
*        MRKT  0558,'BECKLEY, WV'                                               
*        MRKT  0559,'LA CROSSE, WI'                                             
*        MRKT  0560,'CHEYENNE, WY'                                              
*        MRKT  0561,'ODESSA-MIDLAND, TX'                                        
*        MRKT  0562,'KILLEN-TEMPLE, TX'                                         
*        MRKT  0564,'COLUMBIA, MO'                                              
*        MRKT  0565,'MORRISTOWN, NJ'                                            
*        MRKT  0566,'LUFKIN-NACOGDOCHES, TX'                                    
*        MRKT  0567,'BOULDER, CO'                                               
*        MRKT  0570,'DUBUQUE, IA'                                               
*        MRKT  0571,'FT. WALTON BCH, FL'                                        
*        MRKT  0572,'GRAND FORKS ND-MN'                                         
*        MRKT  0573,'PANAMA CITY, FL'                                           
*        MRKT  0574,'SANTA ROSA, CA'                                            
*        MRKT  0575,'ST. CLOUD, MN'                                             
*        MRKT  0577,'WATERTOWN, NY'                                             
*        MRKT  0578,'WILLIAMSPORT, PA'                                          
*        MRKT  0579,'YUMA, AZ.'                                                 
*        MRKT  0580,'ALBANY, GA'                                                
*        MRKT  0581,'ANN ARBOR, MI'                                             
*        MRKT  0582,'BISMARK, ND'                                               
*        MRKT  0583,'ITHACA, NY'                                                
*        MRKT  0584,'LAKE CHARLES, LA'                                          
*        MRKT  0585,'MONROE, LA'                                                
*        MRKT  0586,'EAU CLAIRE, WI'                                            
*        MRKT  0587,'ALEXANDRIA, LA'                                            
*        MRKT  0588,'MERIDIAN, MS'                                              
*        MRKT  0589,'FT. SMITH, AR'                                             
*        MRKT  0590,'ALBANY-CRVLLIS-LEBANON, OR'                                
*        MRKT  0591,'SANTA BARBARA, CA'                                         
*        MRKT  0592,'PALM SPRINGS, CA'                                          
*        MRKT  0593,'DANBURY, CT'                                               
*        MRKT  0594,'OXNARD-VENTURA'                                            
*        MRKT  0595,'LAFAYETTE, IN'                                             
*        MRKT  0596,'SAN JOSE CA HISPANIC',BOOKTYPE=H                           
*        MRKT  0598,'CAPE MAY, NJ'                                              
*        MRKT  0599,'LAWTON,OK'                                                 
*        MRKT  0603,'N.W. INDIANA TA'                                           
*        MRKT  0604,'CENTRAL UPPER MICH.'                                       
*        MRKT  0609,'NORTHERN LOWER MICH'                                       
*        MRKT  0610,' MARKET 610'                                               
*        MRKT  0611,'MONMOUTH-OCEAN, NJ. PMSA'                                  
*        MRKT  0617,'TRAVERSE CITY'                                             
*        MRKT  0618,'PADUCAH, KY. TA'                                           
*        MRKT  0619,'SALEM, OR. MSA'                                            
*        MRKT  0623,'FLORENCE AL'                                               
*        MRKT  0626,'SAN LOUIS OBISPO'                                          
*        MRKT  0627,'BANGOR, ME.'                                               
*        MRKT  0628,'ELKHART IN.'                                               
*        MRKT  0629,'FLORENCE SC'                                               
*        MRKT  0630,' FLORENCE SC MSA'                                          
*        MRKT  0631,' MARKET 631'                                               
*        MRKT  0632,'WICHITA KS. TSA'                                           
*        MRKT  0633,' MARKET 633'                                               
*        MRKT  0635,' MARKET 635'                                               
*        MRKT  0639,'CORNING-ELMIRA TA'                                         
*        MRKT  0643,'SANTA MARIA'                                               
*        MRKT  0649,'FOND DU LAC, WI.'                                          
*        MRKT  0650,'JEFFERSON CITY, MO.'                                       
*        MRKT  0652,' MARKET 652'                                               
*        MRKT  0654,'STOCKTON, CA. MSA'                                         
*        MRKT  0655,'KALAMAZOO, MI. MSA'                                        
*        MRKT  0659,'LIMA, IOWA'                                                
*        MRKT  0660,'WAUSAU'                                                    
*        MRKT  0662,'BLUEFIELD WV-VA BDM'                                       
*        MRKT  0665,'SHREVEPORT, LA. TSA'                                       
*        MRKT  0666,'BENTON HARBOR,MI.'                                         
*        MRKT  0668,'SPRINGFIELD, MO. TSA'                                      
*        MRKT  0671,'PADUCAH-CAPE GIR-HARRISBURG'                               
*        MRKT  0672,' MARKET 672'                                               
*        MRKT  0674,'SPRINFLD-DECATUR-CHMPGN'                                   
*        MRKT  0675,' MARKET 675'                                               
*        MRKT  0676,'FRESNO, CA. TSA'                                           
*        MRKT  0680,'RUSS/BWL GR/HPKNS/CLRKS KY/TN'                             
*        MRKT  0681,'FAYETTEVILLE, NC. TA'                                      
*        MRKT  0683,'SALISBURY/OCEAN CITY MD'                                   
*        MRKT  0685,'OCALA FL MSA'                                              
*        MRKT  0688,'SPRINGFIELD, MA.'                                          
*        MRKT  0690,'KALAMAZOO, MI TA'                                          
*        MRKT  0691,'SAN JUAN METRO TA'                                         
*        MRKT  0692,'FT. COLLINS, CO.'                                          
*        MRKT  0693,'BRIDGEPORT, CT. TSA'                                       
*        MRKT  0694,'JACKSON, MS. MSA'                                          
*        MRKT  0698,'WORCESTER, MA. MSA'                                        
*        MRKT  0703,'WAUSAU/RHINELANDER'                                        
*        MRKT  0705,'WINCHESTER, VA.'                                           
*        MRKT  0707,'YUBA CITY, CA.'                                            
*        MRKT  0708,'SALINAS, CA.'                                              
*        MRKT  0709,'SUMTER, SC'                                                
*        MRKT  0710,'ST. JOSEPH, MO.'                                           
*        MRKT  0712,'BOSTON ADI'                                                
*        MRKT  0713,'LAREDO, TX.'                                               
*        MRKT  0717,'NE MICHIGAN BDM'                                           
*        MRKT  0720,'GLENS FALLS NY'                                            
*        MRKT  0729,'NEW LONDON CT BDM'                                         
*        MRKT  0755,'ORANGE COUNTY CA BDM'                                      
*        MRKT  0760,'HOUSTON HTMR',BOOKTYPE=H                                   
*        MRKT  0767,'CORPUS CHRISTI HTMR',BOOKTYPE=H                            
*        MRKT  0768,'SOUTH FL. HTMR',BOOKTYPE=H                                 
*        MRKT  0769,'LOS ANGELES HTMR',BOOKTYPE=H                               
*        MRKT  0783,'SAN ANTONIO HTMR',BOOKTYPE=H                               
*        MRKT  0786,'EAST IDAHO BDM'                                            
*        MRKT  0800,'TORONTO CMA'                                               
*        MRKT  0806,'HAMILTON, ONTARIO CMS'                                     
*        MRKT  0811,'OWEN SOUND EXTENDED MARKET'                                
*        MRKT  0832,'FRESNO HTMR',BOOKTYPE=H                                    
*        MRKT  0833,'EL PASO HTMR',BOOKTYPE=H                                   
*        MRKT  0836,'NEW YORK HTMR',BOOKTYPE=H                                  
*        MRKT  0851,'SAN DIEGO HTMR',BOOKTYPE=H                                 
*        MRKT  0854,'DALLAS HTMR',BOOKTYPE=H                                    
*        MRKT  0869,'MC ALLEN/BRWNSVL HTMR',BOOKTYPE=H                          
*        MRKT  0883,'CHICAGO HTMR',BOOKTYPE=H                                   
*        MRKT  0900,'ATLANTA BLACK',BOOKTYPE=B                                  
*        MRKT  0901,'AUGUSTA BLACK',BOOKTYPE=B                                  
*        MRKT  0902,'BALTIMORE BLACK',BOOKTYPE=B                                
*        MRKT  0903,'BATON ROUGE BLACK',BOOKTYPE=B                              
*        MRKT  0904,'BEAUMONT BLACK',BOOKTYPE=B                                 
*        MRKT  0905,'BIRMINGHAM BLACK',BOOKTYPE=B                               
*        MRKT  0906,'CHARLESTON BLACK',BOOKTYPE=B                               
*        MRKT  0907,'CHARLOTTE BLACK',BOOKTYPE=B                                
*        MRKT  0909,'CHICAGO BLACK',BOOKTYPE=B                                  
*        MRKT  0910,'CLEVELAND BLACK',BOOKTYPE=B                                
*        MRKT  0911,'COLUMBIA BLACK',BOOKTYPE=B                                 
*        MRKT  0912,'DALLAS BLACK',BOOKTYPE=B                                   
*        MRKT  0913,'DETRIOT BLACK',BOOKTYPE=B                                  
*        MRKT  0914,'FLINT BLACK',BOOKTYPE=B                                    
*        MRKT  0915,'GREENSBORO BLACK',BOOKTYPE=B                               
*        MRKT  0916,'HOUSTON BLACK',BOOKTYPE=B                                  
*        MRKT  0918,'JACKSONVILLE BLACK',BOOKTYPE=B                             
*        MRKT  0919,'LOS ANGELES BLACK',BOOKTYPE=B                              
*        MRKT  0920,'MEMPHIS BLACK',BOOKTYPE=B                                  
*        MRKT  0921,'SOUTH FLORIDA BLACK',BOOKTYPE=B                            
*        MRKT  0922,'DADE BLACK',BOOKTYPE=B                                     
*        MRKT  0923,'MOBILE BLACK',BOOKTYPE=B                                   
*        MRKT  0924,'MONTGOMERY BLACK',BOOKTYPE=B                               
*        MRKT  0925,'NASHVILLE BLACK',BOOKTYPE=B                                
*        MRKT  0926,'NEW ORLEANS BLACK',BOOKTYPE=B                              
*        MRKT  0927,'NEW YORK BLACK',BOOKTYPE=B                                 
*        MRKT  0928,'NORFOLK BLACK',BOOKTYPE=B                                  
*        MRKT  0930,'PHILADELPHIA BLACK',BOOKTYPE=B                             
*        MRKT  0931,'RALEIGH BLACK',BOOKTYPE=B                                  
*        MRKT  0932,'RICHMOND BLACK',BOOKTYPE=B                                 
*        MRKT  0933,'ST. LOUIS BLACK',BOOKTYPE=B                                
*        MRKT  0934,'SHREVEPORT BLACK',BOOKTYPE=B                               
*        MRKT  0935,'WASHINGTON BLACK',BOOKTYPE=B                               
*        MRKT  0937,'ALBUQUERQUE BLACK',BOOKTYPE=B                              
*        MRKT  0938,'AUSTIN HISPANIC',BOOKTYPE=H                                
*        MRKT  0939,'EL PASO HISPANIC',BOOKTYPE=H                               
*        MRKT  0940,'FRESNO HISPANIC',BOOKTYPE=H                                
*        MRKT  0941,'HOUSTON HISPANIC',BOOKTYPE=H                               
*        MRKT  0942,'NEW YORK HISPANIC',BOOKTYPE=H                              
*        MRKT  0943,'CHICAGO HISPANIC',BOOKTYPE=H                               
*        MRKT  0944,'LOS ANGELES HISPANIC',BOOKTYPE=H                           
*        MRKT  0945,'SOUTH FLORIDA HISPANIC',BOOKTYPE=H                         
*        MRKT  0946,'MIAMII/HIALEAH HISPANIC',BOOKTYPE=H                        
*        MRKT  0947,'MODESTO HISPANIC',BOOKTYPE=H                               
*        MRKT  0948,'RIVERSIDE HISPANIC',BOOKTYPE=H                             
*        MRKT  0949,'PHOENIX HISPANIC',BOOKTYPE=H                               
*        MRKT  0950,'SAN ANTONIO HISPANIC',BOOKTYPE=H                           
*        MRKT  0951,'SAN DIEGO HISPANIC',BOOKTYPE=H                             
*        MRKT  0952,'SAN FRANCISCO HISPANIC',BOOKTYPE=H                         
*        MRKT  0953,'SAN HOSE HISPANIC',BOOKTYPE=H                              
*        MRKT  0954,'STOCKTON HISPANIC',BOOKTYPE=H                              
*        MRKT  0955,'DALLAS HISPANIC',BOOKTYPE=H                                
*        MRKT  0956,'SALINAS HISPANIC',BOOKTYPE=H                               
*        MRKT  0957,'CORPUS CHRISTI HISPANIC',BOOKTYPE=H                        
*        MRKT  0958,'LITTLE ROCK BLACK',BOOKTYPE=B                              
*        MRKT  0959,'FLINT MI BLACK',BOOKTYPE=B                                 
*        MRKT  0983,'ALBUQUERQUE HTMR',BOOKTYPE=H                               
*        MRKT  0986,'SAN FRANCISCO HTMR',BOOKTYPE=H                             
*        MRKT  0987,'PHOENIX HTMR',BOOKTYPE=H                                   
*        MRKT  0995,'SACRAMENTO HTMR',BOOKTYPE=H                                
*        MRKT  0998,'MODESTO CA TSA'                                            
*        EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* NSIMKTS                                                                       
         SMTBL NT                      USA NIELSEN TV                           
         MRKT  0001,'NETWORK'                                                   
         MRKT  0002,'BIRMINGHAM MERTO',BOOKTYPE=M                               
         MRKT  0004,'MANCHESTER, NH TSA',BOOKTYPE=T                             
         MRKT  0010,'NHSI SANTA CLARA SP RPT',BOOKTYPE=T                        
         MRKT  0012,'NHSI FRESNO KTFF SP RPT',BOOKTYPE=T                        
         MRKT  0018,'BROWARD CTY SPCL',BOOKTYPE=M                               
         MRKT  0019,'LONGVIEW-TYLER TAR',BOOKTYPE=E                             
         MRKT  0020,'TN/KY TAR',BOOKTYPE=T                                      
         MRKT  0021,'E+SE KY TRADE REPORT',BOOKTYPE=T                           
         MRKT  0022,'KFYR SATELLITE RPT',BOOKTYPE=T                             
         MRKT  0024,'LUFKIN/NACOGDOCHES TAR',BOOKTYPE=T    DUP STA 19           
         MRKT  0025,'FLORIDA TAR',BOOKTYPE=T                                    
         MRKT  0037,'SPECIAL FOXNET',BOOKTYPE=N                                 
         MRKT  0040,'FORT SMITH TAR',BOOKTYPE=T                                 
         MRKT  0041,'N.W. ARKANSAS TAR',BOOKTYPE=M                              
         MRKT  0042,'FLAGSTAFF,AZ TAR',BOOKTYPE=T                               
         MRKT  0043,'SANTA CLARA SPCL/METRO',BOOKTYPE=M                         
         MRKT  0052,'YAKIMA SPECIAL/TA',BOOKTYPE=M                              
         MRKT  0053,'TRI-CITIES SPECIAL/TA',BOOKTYPE=T                          
         MRKT  0058,'SOUTHERN NEW HAMPSHIRE TAR',BOOKTYPE=E                     
         MRKT  0061,'DADE COUNTY SPCL',BOOKTYPE=T                               
         MRKT  0071,'SPECIAL WVII REPORT FEB/95'                                
         MRKT  0072,'NHI LA CABLE DMA(LPM)',BOOKTYPE=P                          
         MRKT  0073,'NHI LA CABLE WIRED(LPM)',BOOKTYPE=P                        
         MRKT  0078,'LAKERS EXCLUSION REPORT',BOOKTYPE=X                        
         MRKT  0100,'PORTLAND-AUBURN'                                           
         MRKT  0101,'NEW YORK'                                                  
         MRKT  0102,'BINGHAMTON'                                                
         MRKT  0103,'MACON'                                                     
         MRKT  0104,'PHILADELPHIA'                                              
         MRKT  0105,'DETROIT'                                                   
         MRKT  0106,'BOSTON (MANCHESTER)'                                       
         MRKT  0107,'SAVANNAH'                                                  
         MRKT  0108,'PITTSBURGH'                                                
         MRKT  0109,'FT. WAYNE'                                                 
         MRKT  0110,'CLEVELAND-AKRON (CANTON)'                                  
         MRKT  0111,'WASHINGTON, DC (HAGRSTWN)'                                 
         MRKT  0112,'BALTIMORE'                                                 
         MRKT  0113,'FLINT-SAGINAW-BAY CITY'                                    
         MRKT  0114,'BUFFALO'                                                   
         MRKT  0115,'CINCINNATI'                                                
         MRKT  0116,'ERIE'                                                      
         MRKT  0117,'CHARLOTTE'                                                 
         MRKT  0118,'GREENSBORO-H.POINT-W.SALEM'                                
         MRKT  0119,'CHARLESTON, SC'                                            
         MRKT  0120,'AUGUSTA-AIKEN'                                             
         MRKT  0121,'PROVIDENCE-NEW BEDFORD'                                    
         MRKT  0122,'COLUMBUS, GA'                                              
         MRKT  0123,'BURLINGTON-PLATTSBURGH'                                    
         MRKT  0124,'ATLANTA -EXTENDED',BOOKTYPE=E                              
         MRKT  0125,'ALBANY, GA'                                                
         MRKT  0126,'UTICA'                                                     
         MRKT  0127,'INDIANAPOLIS'                                              
         MRKT  0128,'MIAMI-FT. LAUDERDALE'                                      
         MRKT  0129,'LOUISVILLE'                                                
         MRKT  0130,'TALLAHASSEE-THOMASVILLE'                                   
         MRKT  0131,'TRI-CITIES, TN-VA'                                         
         MRKT  0132,'ALBANY-SCHENECTADY-TROY'                                   
         MRKT  0133,'HARTFORD && NEW HAVEN'                                     
         MRKT  0134,'ORLANDO-DAYTONA BCH-MELBRN'                                
         MRKT  0135,'COLUMBUS, OH'                                              
         MRKT  0136,'YOUNGSTOWN'                                                
         MRKT  0137,'BANGOR'                                                    
         MRKT  0138,'ROCHESTER, NY'                                             
         MRKT  0139,'TAMPA-ST. PETE (SARASOTA)'                                 
         MRKT  0140,'TRAVERSE CITY-CADILLAC'                                    
         MRKT  0141,'LEXINGTON'                                                 
         MRKT  0142,'DAYTON'                                                    
         MRKT  0143,'SPRINGFIELD-HOLYOKE'                                       
         MRKT  0144,'NORFOLK-PORTSMTH-NEWPT NWS'                                
         MRKT  0145,'GREENVILLE-N.BERN-WASHNGTN'                                
         MRKT  0146,'COLUMBIA, SC'                                              
         MRKT  0147,'TOLEDO'                                                    
         MRKT  0148,'WEST PALM BEACH-FT. PIERCE'                                
         MRKT  0149,'WATERTOWN'                                                 
         MRKT  0150,'WILMINGTON'                                                
         MRKT  0151,'LANSING'                                                   
         MRKT  0152,'PRESQUE ISLE'                                              
         MRKT  0153,'MARQUETTE'                                                 
         MRKT  0154,'WHEELING-STEUBENVILLE'                                     
         MRKT  0155,'SYRACUSE'                                                  
         MRKT  0156,'RICHMOND-PETERSBURG'                                       
         MRKT  0157,'KNOXVILLE'                                                 
         MRKT  0158,'LIMA'                                                      
         MRKT  0159,'BLUEFIELD-BECKLEY-OAK HILL'                                
         MRKT  0160,'RALEIGH-DURHAM (FAYETVLLE)'                                
         MRKT  0161,'JACKSONVILLE'                                              
         MRKT  0162,'SARASOTA'                                                  
         MRKT  0163,'GRAND RAPIDS-KALMZOO-B.CRK'                                
         MRKT  0164,'CHARLESTON-HUNTINGTON'                                     
         MRKT  0165,'ELMIRA (CORNING)'                                          
         MRKT  0166,'HARRISBURG-LNCSTR-LEB-YORK'                                
         MRKT  0167,'GREENVLL-SPART-ASHEVLL-AND'                                
         MRKT  0168,'ATLANTA'                                                   
         MRKT  0169,'HARRISONBURG'                                              
         MRKT  0170,'MYRTLE BEACH-FLORENCE'                                     
         MRKT  0171,'FT. MYERS-NAPLES'                                          
         MRKT  0172,'MANCHESTER'                                                
         MRKT  0173,'ROANOKE-LYNCHBURG'                                         
         MRKT  0174,'JOHNSTOWN-ALTOONA'                                         
         MRKT  0175,'CHATTANOOGA'                                               
         MRKT  0176,'SALISBURY'                                                 
         MRKT  0177,'WILKES BARRE-SCRANTON'                                     
         MRKT  0178,'NEW HAVEN(METRO)',BOOKTYPE=E                               
         MRKT  0179,'BRUNSWICK, GA SMA',BOOKTYPE=T                              
         MRKT  0181,'TERRE HAUTE'                                               
         MRKT  0182,'LAFAYETTE, IN'                                             
         MRKT  0183,'ALPENA'                                                    
         MRKT  0184,'CHARLOTTESVILLE'                                           
         MRKT  0185,'AKRON'                                                     
         MRKT  0188,'SOUTH BEND-ELKHART'                                        
         MRKT  0189,'HARTFORD METRO',BOOKTYPE=T                                 
         MRKT  0191,'BURLINGTON/PLATTSBRUG METRO',BOOKTYPE=M                    
         MRKT  0192,'GAINESVILLE'                                               
         MRKT  0193,'WORCESTER'                                                 
         MRKT  0194,'HAGERSTOWN'                                                
         MRKT  0195,'AKRON'                                                     
         MRKT  0196,'ZANESVILLE'                                                
         MRKT  0197,'PARKERSBURG'                                               
         MRKT  0198,'CLARKSBURG-WESTON'                                         
         MRKT  0199,'SARASOTA SMA',BOOKTYPE=T                                   
         MRKT  0200,'CORPUS CHRISTI'                                            
         MRKT  0201,'WASHINGTON D.C. METRO',BOOKTYPE=M                          
         MRKT  0202,'CHICAGO'                                                   
         MRKT  0203,'JOPLIN-PITTSBURG'                                          
         MRKT  0204,'COLUMBIA-JEFFERSON CITY'                                   
         MRKT  0205,'TOPEKA'                                                    
         MRKT  0206,'DOTHAN'                                                    
         MRKT  0207,'TRAVERSE CITY/CAD METRO',BOOKTYPE=M                        
         MRKT  0208,'HARTFORD, VT. TA',BOOKTYPE=M                               
         MRKT  0209,'ST. LOUIS'                                                 
         MRKT  0210,'ROCKFORD'                                                  
         MRKT  0211,'ROCHESTR-MASON CITY-AUSTIN'                                
         MRKT  0212,'SHREVEPORT'                                                
         MRKT  0213,'MINNEAPOLIS-ST. PAUL'                                      
         MRKT  0215,'LAFAYETTE, IN'                                             
         MRKT  0216,'KANSAS CITY'                                               
         MRKT  0217,'MILWAUKEE'                                                 
         MRKT  0218,'HOUSTON'                                                   
         MRKT  0219,'SPRINGFIELD, MO'                                           
         MRKT  0220,'TUSCALOOSA, AL.'                                           
         MRKT  0222,'NEW ORLEANS'                                               
         MRKT  0223,'DALLAS-FT. WORTH'                                          
         MRKT  0224,'SIOUX CITY'                                                
         MRKT  0225,'WACO-TEMPLE-BRYAN'                                         
         MRKT  0226,'VICTORIA'                                                  
         MRKT  0227,'WICHITA FALLS && LAWTON'                                   
         MRKT  0228,'MONROE-EL DORADO'                                          
         MRKT  0229,'LAWTON(METRO)'                                             
         MRKT  0230,'BIRMINGHAM (ANN, TUSC)'                                    
         MRKT  0231,'OTTUMWA-KIRKSVILLE'                                        
         MRKT  0232,'PADUCAH-CAPE GIRARD-HARSBG'                                
         MRKT  0233,'ODESSA-MIDLAND'                                            
         MRKT  0234,'AMARILLO'                                                  
         MRKT  0235,'AUSTIN'                                                    
         MRKT  0236,'HARLINGEN-WSLCO-BRNSVL-MCA'                                
         MRKT  0237,'CEDAR RAPIDS-WTRLO-IWC&&DUB'                               
         MRKT  0238,'ST. JOSEPH'                                                
         MRKT  0239,'JACKSON, TN'                                               
         MRKT  0240,'MEMPHIS'                                                   
         MRKT  0241,'SAN ANTONIO'                                               
         MRKT  0242,'LAFAYETTE, LA'                                             
         MRKT  0243,'LAKE CHARLES'                                              
         MRKT  0244,'ALEXANDRIA, LA'                                            
         MRKT  0246,'ANNISTON'                                                  
         MRKT  0247,'GREENWOOD-GREENVILLE'                                      
         MRKT  0248,'CHAMPAIGN&&SPRNGFLD-DECATUR'                               
         MRKT  0249,'EVANSVILLE'                                                
         MRKT  0250,'OKLAHOMA CITY'                                             
         MRKT  0251,'LUBBOCK'                                                   
         MRKT  0252,'OMAHA'                                                     
         MRKT  0253,'SPRNFLD-DECATUR(METRO)'                                    
         MRKT  0254,'HASTINGS-KEARNEY(METRO)'                                   
         MRKT  0255,'DUBUQUE'                                                   
         MRKT  0256,'PANAMA CITY'                                               
         MRKT  0257,'SHERMAN-ADA'                                               
         MRKT  0258,'GREEN BAY-APPLETON'                                        
         MRKT  0259,'NASHVILLE'                                                 
         MRKT  0260,'ANNISTON PRIOR NOV89'                                      
         MRKT  0261,'SAN ANGELO'                                                
         MRKT  0262,'ABILENE-SWEETWATER'                                        
         MRKT  0269,'MADISON'                                                   
         MRKT  0270,'FT. SMITH-FAY-SPRNGDL-RGRS'                                
         MRKT  0271,'TULSA'                                                     
         MRKT  0273,'COLUMBUS-TUPELO-W PNT-HSTN'                                
         MRKT  0275,'PEORIA-BLOOMINGTON'                                        
         MRKT  0276,'DULUTH-SUPERIOR'                                           
         MRKT  0278,'WICHITA-HUTCHINSON PLUS'                                   
         MRKT  0279,'DES MOINES-AMES'                                           
         MRKT  0280,'FLORENCE, AL SMA',BOOKTYPE=T                               
         MRKT  0282,'DAVENPORT-R.ISLAND-MOLINE'                                 
         MRKT  0286,'MOBILE-PENSACOLA (FT WALT)'                                
         MRKT  0287,'MINOT-BISMARCK-DICKINSON'                                  
         MRKT  0288,'NEW HAVEN METRO',BOOKTYPE=M                                
         MRKT  0290,'GREAT BEND'                                                
         MRKT  0291,'HUNTSVILLE-DECATUR (FLOR)'                                 
         MRKT  0292,'BEAUMONT-PORT ARTHUR'                                      
         MRKT  0293,'LITTLE ROCK-PINE BLUFF'                                    
         MRKT  0297,'ALEXANDRIA,MN'                                             
         MRKT  0298,'MONTGOMERY-SELMA'                                          
         MRKT  0299,'COLORADO SPRINGS TA',BOOKTYPE=T                            
         MRKT  0302,'LA CROSSE-EAU CLAIRE'                                      
         MRKT  0303,'EAU CLAIRE'                                                
         MRKT  0304,'LA CROSSE'                                                 
         MRKT  0305,'WAUSAU-RHINELANDER'                                        
         MRKT  0306,'RHINELANDER'                                               
         MRKT  0309,'TYLER-LONGVIEW(LFKN&&NCGD)'                                
         MRKT  0310,'HATTIESBURG-LAUREL'                                        
         MRKT  0311,'MERIDIAN'                                                  
         MRKT  0316,'BATON ROUGE'                                               
         MRKT  0317,'QUINCY-HANNIBAL-KEOKUK'                                    
         MRKT  0318,'JACKSON, MS'                                               
         MRKT  0322,'LINCOLN && HASTINGS-KRNY'                                  
         MRKT  0324,'FARGO-VALLEY CITY'                                         
         MRKT  0325,'SIOUX FALLS(MITCHELL)'                                     
         MRKT  0333,'FLORENCE,AL'                                               
         MRKT  0334,'JONESBORO'                                                 
         MRKT  0335,'HAGERSTOWN SMA',BOOKTYPE=T                                 
         MRKT  0336,'BOWLING GREEN'                                             
         MRKT  0337,'MANKATO'                                                   
         MRKT  0338,'BOWLING GREEN'                                             
         MRKT  0339,'FT. DODGE'                                                 
         MRKT  0340,'NORTH PLATTE'                                              
         MRKT  0341,'HAYS-GOODLAND'                                             
         MRKT  0342,'ENSIGN-GARDEN CITY'                                        
         MRKT  0343,'ANCHORAGE'                                                 
         MRKT  0344,'HONOLULU'                                                  
         MRKT  0345,'FAIRBANKS'                                                 
         MRKT  0346,'BILOXI-GULFPORT'                                           
         MRKT  0347,'JUNEAU'                                                    
         MRKT  0349,'LAREDO'                                                    
         MRKT  0350,'FLAGSTAFF'                                                 
         MRKT  0351,'DENVER'                                                    
         MRKT  0352,'COLORADO SPRINGS-PUEBLO'                                   
         MRKT  0353,'PHOENIX (PRESCOTT)'                                        
         MRKT  0354,'BUTTE-BOZEMAN'                                             
         MRKT  0355,'GREAT FALLS'                                               
         MRKT  0356,'BILLINGS'                                                  
         MRKT  0357,'BOISE'                                                     
         MRKT  0358,'IDAHO FALLS-POCATELLO'                                     
         MRKT  0359,'CHEYENNE-SCOTTSBLUF'                                       
         MRKT  0360,'TWIN FALLS'                                                
         MRKT  0361,'ROSWELL'                                                   
         MRKT  0362,'MISSOULA'                                                  
         MRKT  0363,'FLAGSTAFF'                                                 
         MRKT  0364,'RAPID CITY'                                                
         MRKT  0365,'EL PASO (LAS CRUCES)'                                      
         MRKT  0366,'HELENA'                                                    
         MRKT  0367,'CASPER-RIVERTON'                                           
         MRKT  0370,'SALT LAKE CITY'                                            
         MRKT  0371,'YUMA-EL CENTRO'                                            
         MRKT  0373,'GRAND JUNCTION-MONTROSE'                                   
         MRKT  0374,'THE ALABAMA REPORT',BOOKTYPE=M                             
         MRKT  0375,'GRAND RAPIDS (METRO)',BOOKTYPE=M                           
         MRKT  0376,'OKLAHOMA CITY(METRO)',BOOKTYPE=M                           
         MRKT  0377,'NORTHWEST ARKANSAS TAR',BOOKTYPE=M                         
         MRKT  0379,'ALBUQUERQUE (METRO)',BOOKTYPE=M                            
         MRKT  0382,'HARTFORD/N.HAVEN N-METRO',BOOKTYPE=N                       
         MRKT  0384,'EUGENE METRO',BOOKTYPE=M                                   
         MRKT  0385,'CHICAGO EXTENDED',BOOKTYPE=E                               
         MRKT  0387,'DICKINSON'                                                 
         MRKT  0389,'TUCSON (SIERRA VISTA)'                                     
         MRKT  0390,'ALBUQUERQUE-SANTA FE'                                      
         MRKT  0391,'FARMINGTON'                                                
         MRKT  0392,'FARMINGTON'                                                
         MRKT  0398,'GLENDIVE'                                                  
         MRKT  0400,'BAKERSFIELD'                                               
         MRKT  0401,'EUGENE'                                                    
         MRKT  0402,'EUREKA'                                                    
         MRKT  0403,'LOS ANGELES'                                               
         MRKT  0404,'PALM SPRINGS'                                              
         MRKT  0405,'PALM SPRINGS'                                              
         MRKT  0406,'SANTA ROSA, CA. TAR',BOOKTYPE=T                            
         MRKT  0407,'SAN FRANCISCO-OAK-SAN JOSE'                                
         MRKT  0410,'YAKIMA-PASCO-RCHLND-KNNWCK'                                
         MRKT  0411,'RENO'                                                      
         MRKT  0413,'MEDFORD-KLAMATH FALLS'                                     
         MRKT  0419,'SEATTLE-TACOMA'                                            
         MRKT  0420,'PORTLAND, OR'                                              
         MRKT  0421,'BEND, OR'                                                  
         MRKT  0425,'SAN DIEGO'                                                 
         MRKT  0428,'MONTEREY-SALINAS'                                          
         MRKT  0439,'LAS VEGAS'                                                 
         MRKT  0455,'SANTABARBRA-SANMAR-SANLUOB'                                
         MRKT  0462,'SACRAMNTO-STKTON-MODESTO'                                  
         MRKT  0466,'FRESNO-VISALIA'                                            
         MRKT  0468,'CHICO-REDDING'                                             
         MRKT  0469,'BRYAN/COLLEGE (TA)',BOOKTYPE=T                             
         MRKT  0480,'SANTA CLARA (METRO)',BOOKTYPE=M                            
         MRKT  0481,'SPOKANE'                                                   
         MRKT  0498,'BELLINGHAM'                                                
         MRKT  0499,'WICHITA (METRO)',BOOKTYPE=M                                
         MRKT  0514,'NHI BOSTON DMA',BOOKTYPE=D                                 
         MRKT  0515,'NHI BOSTON CBL',BOOKTYPE=C                                 
         MRKT  0565,'DMA FOR WPTZ/WNNE',BOOKTYPE=D                              
         MRKT  0588,'HOUSTON WS EXCLUSION',BOOKTYPE=X                           
         MRKT  0589,'NORTH BAY SPECIAL REPORT',BOOKTYPE=T                       
         MRKT  0615,'PHOENIX SUN EXCLUSION',BOOKTYPE=X                          
         MRKT  0653,'JUNEAU SPECIAL',BOOKTYPE=T                                 
         MRKT  0669,'SPCL RPT WDSU',BOOKTYPE=E                                  
         MRKT  0670,'SPCL RPT KGWN',BOOKTYPE=E                                  
         MRKT  0901,'GREENVILLE-NB-WASHNC 3W',BOOKTYPE=T                        
         MRKT  0906,'WFMZ SPECIAL REPORT',BOOKTYPE=T                            
         MRKT  0909,'SP RPT WRNN',BOOKTYPE=T                                    
         MRKT  0911,'WPMI-TV MOBILE/PENSA. SPC'                                 
         MRKT  0912,'FL TRADING AREA SUPP RPT',BOOKTYPE=T                       
         MRKT  0923,'SEATTLE METRO SPCL X RPT',BOOKTYPE=M                       
         MRKT  0926,'WNEG SPECIAL REPORT',BOOKTYPE=T                            
         MRKT  0943,'SF DMA ASIAN REPORT',BOOKTYPE=K                            
         MRKT  0949,'CHICAGO WS EXCLUSION',BOOKTYPE=X                           
         MRKT  0961,'BINGHAMTON SPCL RPT',BOOKTYPE=T                            
         MRKT  0966,'SIOUX FALLS SPCL RPT',BOOKTYPE=T                           
         MRKT  0967,'KOAA SPCL RPT',BOOKTYPE=M                                  
         MRKT  0969,'WBMG SPCL RPT (METRO)',BOOKTYPE=M                          
         MRKT  0970,'WNEG SPCL RPT (METRO)',BOOKTYPE=M                          
         MRKT  0979,'WPDE SPCL RPT',BOOKTYPE=T                                  
         MRKT  0991,'WESTERN NC SPCL',BOOKTYPE=T                                
         MRKT  0992,'SE SONICS/MARINERS EXCL',BOOKTYPE=E                        
         MRKT  0993,'SLC JAZZ EXCLUSION',BOOKTYPE=E                             
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* NSWMKTS                                                                       
         SMTBL NW                      USA NIELSEN TV                           
         MRKT  0001,'NETWORK'                                                   
         MRKT  0100,'PORTLAND-POLAND SPRING'                                    
         MRKT  0101,'NEW YORK'                                                  
         MRKT  0102,'BINGHAMTON'                                                
         MRKT  0103,'MACON'                                                     
         MRKT  0104,'PHILADELPHIA'                                              
         MRKT  0105,'DETROIT'                                                   
         MRKT  0106,'BOSTON'                                                    
         MRKT  0107,'SAVANNAH'                                                  
         MRKT  0108,'PITTSBURGH'                                                
         MRKT  0109,'FT. WAYNE'                                                 
         MRKT  0110,'CLEVELAND'                                                 
         MRKT  0111,'WASHINGTON,DC'                                             
         MRKT  0112,'BALTIMORE'                                                 
         MRKT  0113,'FLINT-SAGINAW-BAY CITY'                                    
         MRKT  0114,'BUFFALO'                                                   
         MRKT  0115,'CINCINNATI'                                                
         MRKT  0116,'ERIE'                                                      
         MRKT  0117,'CHARLOTTE'                                                 
         MRKT  0118,'GRNSBORO-H.POINT-W.SALEM'                                  
         MRKT  0119,'CHARLESTON,SC'                                             
         MRKT  0120,'AUGUSTA,GA'                                                
         MRKT  0121,'PROVIDENCE'                                                
         MRKT  0122,'COLUMBUS,GA'                                               
         MRKT  0123,'BURLINGTON-PLATTSBURGH'                                    
         MRKT  0124,'ATLANTA'                                                   
         MRKT  0125,'ALBANY,GA'                                                 
         MRKT  0126,'UTICA'                                                     
         MRKT  0127,'INDIANAPOLIS'                                              
         MRKT  0128,'MIAMI-FT. LAUDERDALE'                                      
         MRKT  0129,'LOUISVILLE'                                                
         MRKT  0130,'TALLAHASSEE-THOMASVILLE'                                   
         MRKT  0131,'TRI-CITIES, TN-VA'                                         
         MRKT  0132,'ALBANY-SCHNCTADY-TROY'                                     
         MRKT  0133,'HARTFORD-NEW HAVEN'                                        
         MRKT  0134,'ORLANDO-DAYTON BEACH'                                      
         MRKT  0135,'COLUMBUS,OH'                                               
         MRKT  0136,'YOUNGSTOWN'                                                
         MRKT  0137,'BANGOR'                                                    
         MRKT  0138,'ROCHESTER'                                                 
         MRKT  0139,'TAMPA-ST PETERSBURG'                                       
         MRKT  0140,'TRAVERSE CITY-CADILLAC'                                    
         MRKT  0141,'LEXINGTON'                                                 
         MRKT  0142,'DAYTON'                                                    
         MRKT  0143,'SPRINFIELD-HOLYOKE'                                        
         MRKT  0144,'NORFOLK-PORTSMTH-NEWPT NWS'                                
         MRKT  0145,'GRNVLLE-N.BERN-WASHNGTN'                                   
         MRKT  0146,'COLUMBIA,SC'                                               
         MRKT  0147,'TOLEDO'                                                    
         MRKT  0148,'WEST PALM BEACH'                                           
         MRKT  0149,'WATERTOWN'                                                 
         MRKT  0150,'WILMINGTON'                                                
         MRKT  0151,'LANSING'                                                   
         MRKT  0152,'PRESQUE ISLE'                                              
         MRKT  0153,'MARQUETTE'                                                 
         MRKT  0154,'WHEELING-STEUBENVILLE'                                     
         MRKT  0155,'SYRACUSE'                                                  
         MRKT  0156,'RICHMOND-PETERSBURG'                                       
         MRKT  0157,'KNOXVILLE'                                                 
         MRKT  0158,'LIMA'                                                      
         MRKT  0159,'BKLY-BLUEFIELD-OAK HILL'                                   
         MRKT  0160,'RALEIGH-DURHAM'                                            
         MRKT  0161,'JACKSONVILLE'                                              
         MRKT  0162,'SARASOTA'                                                  
         MRKT  0163,'GRAND RAPIDS-KALAMAZOO'                                    
         MRKT  0164,'CHARLESTON-HUNTINGTON'                                     
         MRKT  0165,'ELMIRA'                                                    
         MRKT  0166,'HRRSBRG-LANCSTR-LEB-YRK'                                   
         MRKT  0167,'GRNVLL-SPART-ASHEVILLE'                                    
         MRKT  0168,'ATLANTA'                                                   
         MRKT  0169,'HARRISONBURG'                                              
         MRKT  0170,'FLORENCE,SC'                                               
         MRKT  0171,'FT. MYERS'                                                 
         MRKT  0172,'MANCHESTER'                                                
         MRKT  0173,'ROANOKE-LYNCHBURG'                                         
         MRKT  0174,'JOHNSTOWN-ALTOONA'                                         
         MRKT  0175,'CHATTANOOGA'                                               
         MRKT  0176,'SALISBURY'                                                 
         MRKT  0177,'WILKES BARRE-SCRANTON'                                     
         MRKT  0178,'NEW HAVEN(METRO)'                                          
         MRKT  0179,'HAGERSTOWN'                                                
         MRKT  0181,'TERRE HAUTE'                                               
         MRKT  0182,'LAFAYETTE,IN'                                              
         MRKT  0183,'ALPENA'                                                    
         MRKT  0184,'CHARLOTTESVILLE'                                           
         MRKT  0185,'AKRON'                                                     
         MRKT  0188,'SOUTH BEND-ELKHART'                                        
         MRKT  0192,'GAINESVILLE'                                               
         MRKT  0193,'WORCESTER'                                                 
         MRKT  0194,'HAGERSTOWN'                                                
         MRKT  0195,'AKRON'                                                     
         MRKT  0196,'ZANESVILLE'                                                
         MRKT  0197,'PARKERSBURG'                                               
         MRKT  0198,'CLARKSBURG-WESTON'                                         
         MRKT  0199,'SARASOTA'                                                  
         MRKT  0200,'CORPUS CHRISTI'                                            
         MRKT  0201,'WASHINGTON D.C. METRO'                                     
         MRKT  0202,'CHICAGO'                                                   
         MRKT  0203,'JOPLIN-PITTSBURG'                                          
         MRKT  0204,'COLUMBIA-JEFFERSON CITY'                                   
         MRKT  0205,'TOPEKA'                                                    
         MRKT  0206,'DOTHAN'                                                    
         MRKT  0208,'HARTFORD, VT. TA'                                          
         MRKT  0209,'ST. LOUIS'                                                 
         MRKT  0210,'ROCKFORD'                                                  
         MRKT  0211,'MASON CITY-AUSTIN-ROCHESTER'                               
         MRKT  0212,'SHREVEPORT'                                                
         MRKT  0213,'MINNEAPOLIS-ST. PAUL'                                      
         MRKT  0215,'LAFAYETTE, IN'                                             
         MRKT  0216,'KANSAS CITY'                                               
         MRKT  0217,'MILWAUKEE'                                                 
         MRKT  0218,'HOUSTON'                                                   
         MRKT  0219,'SPRINGFIELD,MO'                                            
         MRKT  0222,'NEW ORLEANS'                                               
         MRKT  0223,'DALLAS-FT. WORTH'                                          
         MRKT  0224,'SIOUX CITY'                                                
         MRKT  0225,'WACO-TEMPLE'                                               
         MRKT  0226,'VICTORIA'                                                  
         MRKT  0227,'WICHITA FALLS-LAWTON'                                      
         MRKT  0228,'MONROE-EL DORADO'                                          
         MRKT  0229,'LAWTON(METRO)'                                             
         MRKT  0230,'BIRMINGHAM'                                                
         MRKT  0231,'OTTUMWA-KIRKSVILLE'                                        
         MRKT  0232,'PADUCAH-C.GIRARDEAU-HRRBRG'                                
         MRKT  0233,'ODESSA-MIDLAND-MONAHANS'                                   
         MRKT  0234,'AMARILLO'                                                  
         MRKT  0235,'AUSTIN,TX'                                                 
         MRKT  0236,'HARLINGEN-WESLACO'                                         
         MRKT  0237,'CEDAR RAPIDS-WATERLOO'                                     
         MRKT  0238,'ST. JOSEPH'                                                
         MRKT  0239,'JACKSON,TN'                                                
         MRKT  0240,'MEMPHIS'                                                   
         MRKT  0241,'SAN ANTONIO'                                               
         MRKT  0242,'LAFAYETTE,LA'                                              
         MRKT  0243,'LAKE CHARLES'                                              
         MRKT  0244,'ALEXANDRIA,LA'                                             
         MRKT  0246,'ANNISTON'                                                  
         MRKT  0247,'GREENWOOD'                                                 
         MRKT  0248,'CHMPAGN-SPRNGFLD-DECATUR'                                  
         MRKT  0249,'EVANSVILLE'                                                
         MRKT  0250,'OKLAHOMA CITY'                                             
         MRKT  0251,'LUBBOCK'                                                   
         MRKT  0252,'OMAHA'                                                     
         MRKT  0253,'SPRNFLD-DECATUR(METRO)'                                    
         MRKT  0254,'HASTINGS-KEARNEY(METRO)'                                   
         MRKT  0255,'DUBUQUE'                                                   
         MRKT  0256,'PANAMA CITY'                                               
         MRKT  0257,'ADA-ARDMORE'                                               
         MRKT  0258,'GREEN BAY'                                                 
         MRKT  0259,'NASHVILLE'                                                 
         MRKT  0260,'ANNISTON'                                                  
         MRKT  0261,'SAN ANGELO'                                                
         MRKT  0262,'ABILENE-SWEETWATER'                                        
         MRKT  0269,'MADISON'                                                   
         MRKT  0270,'FT. SMITH'                                                 
         MRKT  0271,'TULSA'                                                     
         MRKT  0273,'COLUMBUS-TUPELO'                                           
         MRKT  0275,'PEORIA'                                                    
         MRKT  0276,'DULUTH-SUPERIOR'                                           
         MRKT  0278,'WICHITA-HUTCHINSON'                                        
         MRKT  0279,'DES MOINES AMES'                                           
         MRKT  0282,'DAVENPORT-R.ISLAND-MOLINE'                                 
         MRKT  0286,'MOBILE-PENSACOLA'                                          
         MRKT  0287,'MINOT-BSMRK-DICKINSON'                                     
         MRKT  0288,'MINOT-BISMARK'                                             
         MRKT  0290,'GREAT BEND'                                                
         MRKT  0291,'HUNTSVILLE-DECATUR'                                        
         MRKT  0292,'BEAUMONT-PORT ARTHUR'                                      
         MRKT  0293,'LITTLE ROCK-PINE BLUFF'                                    
         MRKT  0297,'ALEXANDRIA,MN'                                             
         MRKT  0298,'MONTGOMERY'                                                
         MRKT  0299,'WICHITA'                                                   
         MRKT  0302,'LA CROSSE-EAU CLAIR'                                       
         MRKT  0303,'EAU CLAIRE'                                                
         MRKT  0304,'LA CROSSE'                                                 
         MRKT  0305,'WAUSAU'                                                    
         MRKT  0306,'RHINELANDER'                                               
         MRKT  0309,'TYLER'                                                     
         MRKT  0310,'HATTIESBURG-LAUREL'                                        
         MRKT  0311,'MERIDIAN'                                                  
         MRKT  0316,'BATON ROUGE'                                               
         MRKT  0317,'QUINCY-HANNIBAL-KEOKUK'                                    
         MRKT  0318,'JACKSON, MS'                                               
         MRKT  0322,'LINCOLN-HSTINGS-KEARNEY'                                   
         MRKT  0324,'FARGO-VALLEY CITY'                                         
         MRKT  0325,'SIOUX FALLS'                                               
         MRKT  0333,'FLORENCE,AL'                                               
         MRKT  0334,'JONESBORO'                                                 
         MRKT  0336,'BOWLING GREEN'                                             
         MRKT  0337,'MANKATO'                                                   
         MRKT  0338,'BOWLING GREEN'                                             
         MRKT  0339,'FT. DODGE'                                                 
         MRKT  0340,'N.PLATTE-HAYES-MC COOK'                                    
         MRKT  0341,'HAYS-GOODLAND'                                             
         MRKT  0342,'ENSIGN-GARDEN CITY'                                        
         MRKT  0343,'ANCHORAGE'                                                 
         MRKT  0344,'HONOLULU'                                                  
         MRKT  0345,'FAIRBANKS'                                                 
         MRKT  0346,'BILOXI'                                                    
         MRKT  0349,'LAREDO'                                                    
         MRKT  0350,'FLAGSTAFF'                                                 
         MRKT  0351,'DENVER'                                                    
         MRKT  0352,'COLORADO SPRINGS-PUEBLO'                                   
         MRKT  0353,'PHOENIX'                                                   
         MRKT  0354,'BUTTE'                                                     
         MRKT  0355,'GREAT FALLS'                                               
         MRKT  0356,'BILLINGS'                                                  
         MRKT  0357,'BOISE'                                                     
         MRKT  0358,'IDAHO FALLS-POCATELLO'                                     
         MRKT  0359,'CHYENN-SCOTTSBLUF-STERLNG'                                 
         MRKT  0360,'TWIN FALLS'                                                
         MRKT  0361,'ROSWELL'                                                   
         MRKT  0362,'MISSOULA'                                                  
         MRKT  0363,'FLAGSTAFF'                                                 
         MRKT  0364,'RAPID CITY'                                                
         MRKT  0365,'EL PASO'                                                   
         MRKT  0366,'HELENA'                                                    
         MRKT  0367,'CASPER-RIVERTON'                                           
         MRKT  0370,'SALT LAKE CITY'                                            
         MRKT  0371,'YUMA-EL CENTRO'                                            
         MRKT  0373,'GRAND JUNCTION-MONTROSE'                                   
         MRKT  0376,'OKLAHOMA CITY(METRO)',BOOKTYPE=M                           
         MRKT  0387,'DICKINSON'                                                 
         MRKT  0389,'TUCSON'                                                    
         MRKT  0390,'ALBUQUERQUE'                                               
         MRKT  0391,'FARMINGTON'                                                
         MRKT  0392,'FARMINGTON'                                                
         MRKT  0398,'GLENDIVE'                                                  
         MRKT  0400,'BAKERSFIELD'                                               
         MRKT  0401,'EUGENE'                                                    
         MRKT  0402,'EUREKA'                                                    
         MRKT  0403,'LOS ANGELES'                                               
         MRKT  0404,'PALM SPRINGS'                                              
         MRKT  0405,'PALM SPRINGS'                                              
         MRKT  0407,'SAN FRANCISCO-OAKLAND'                                     
         MRKT  0410,'YAKIMA'                                                    
         MRKT  0411,'RENO'                                                      
         MRKT  0413,'MEDFORD-KLAMATH-FALLS'                                     
         MRKT  0419,'SEATTLE-TACOMA'                                            
         MRKT  0420,'PORTLAND,OR'                                               
         MRKT  0421,'BEND,OR'                                                   
         MRKT  0425,'SAN DIEGO'                                                 
         MRKT  0428,'MONTEREY-SALINAS'                                          
         MRKT  0439,'LAS VEGAS'                                                 
         MRKT  0455,'SANTA BARBARA-SANTA MARIA'                                 
         MRKT  0462,'SACRAMENTO-STOCKTON'                                       
         MRKT  0466,'FRESNO(VISALIA)'                                           
         MRKT  0468,'CHICO-REDDING'                                             
         MRKT  0481,'SPOKANE'                                                   
         MRKT  0498,'BELLINGHAM'                                                
         MRKT  0499,'WICHITA (METRO)',BOOKTYPE=M                                
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* RADRMKT                                                                       
         SMTBL RR                         RADAR US RADIO                        
         MRKT  0001,'NEW YORK'                                                  
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* SQDMKTS                                                                       
         SMTBL SR                   SQAD RADIO MARKETS                          
         MRKT  0004,'ABILENE, TX'                                               
         MRKT  0007,'AKRON'                                                     
         MRKT  0016,'ALBANY, GA'                                                
         MRKT  0013,'ALBANY-SCHENECTADY-TROY'                                   
         MRKT  0019,'ALBUQUERQUE'                                               
         MRKT  0022,'ALEXANDRIA, LA'                                            
         MRKT  0025,'ALLENTOWN-BETHLEHEM'                                       
         MRKT  0028,'ALTOONA'                                                   
         MRKT  0031,'AMARILLO'                                                  
         MRKT  0037,'ANCHORAGE'                                                 
         MRKT  0040,'ANN ARBOR'                                                 
         MRKT  0043,'APPLETON-OSHKOSH'                                          
         MRKT  0046,'ASHEVILLE'                                                 
         MRKT  0049,'ATLANTA'                                                   
         MRKT  0052,'ATLANTIC CITY'                                             
         MRKT  0058,'AUGUSTA, GA'                                               
         MRKT  0055,'AUGUSTA-WATERVILLE, ME'                                    
         MRKT  0061,'AUSTIN'                                                    
         MRKT  0064,'BAKERSFIELD'                                               
         MRKT  0067,'BALTIMORE'                                                 
         MRKT  0070,'BANGOR, ME'                                                
         MRKT  0073,'BATON ROUGE'                                               
         MRKT  0076,'BATTLE CREEK, MI'                                          
         MRKT  0079,'BEAUMONT-PORT ARTHUR'                                      
         MRKT  0082,'BECKLEY, WV'                                               
         MRKT  0085,'BILLINGS'                                                  
         MRKT  0088,'BILOXI-GULFPORT, MS'                                       
         MRKT  0091,'BINGHAMTON'                                                
         MRKT  0094,'BIRMINGHAM'                                                
         MRKT  0097,'BISMARCK, ND'                                              
         MRKT  0099,'BLACKSBURG-CHRISTN-RAD-PU'                                 
         MRKT  0100,'BLOOMINGTON'                                               
         MRKT  0101,'BLUEFIELD, WV'                                             
         MRKT  0103,'BOISE'                                                     
         MRKT  0106,'BOSTON'                                                    
         MRKT  0109,'BOWLING GREEN'                                             
         MRKT  0112,'BRIDGEPORT'                                                
         MRKT  0114,'BRUNSWICK, GA'                                             
         MRKT  0115,'BRYAN-COLLEGE STATION, TX'                                 
         MRKT  0118,'BUFFALO-NIAGARA FALLS'                                     
         MRKT  0121,'BURLINGTON, VT'                                            
         MRKT  0124,'CANTON'                                                    
         MRKT  0127,'CAPE COD, MA'                                              
         MRKT  0133,'CASPER'                                                    
         MRKT  0136,'CEDAR RAPIDS'                                              
         MRKT  0138,'CHAMPAIGN, IL'                                             
         MRKT  0139,'CHARLESTON, SC'                                            
         MRKT  0142,'CHARLESTON, WV'                                            
         MRKT  0145,'CHARLOTTE-GASTONIA-ROCK H'                                 
         MRKT  0148,'CHARLOTTESVILLE, VA'                                       
         MRKT  0151,'CHATTANOOGA'                                               
         MRKT  0154,'CHEYENNE, WY'                                              
         MRKT  0157,'CHICAGO'                                                   
         MRKT  0160,'CHICO, CA'                                                 
         MRKT  0163,'CINCINNATI'                                                
         MRKT  0164,'CLARKSVILLE-HOPKIN, TN-KY'                                 
         MRKT  0166,'CLEVELAND'                                                 
         MRKT  0172,'COLORADO SPRINGS'                                          
         MRKT  0175,'COLUMBIA, MO'                                              
         MRKT  0178,'COLUMBIA, SC'                                              
         MRKT  0181,'COLUMBUS, GA'                                              
         MRKT  0184,'COLUMBUS, OH'                                              
         MRKT  0185,'COLUMBUS-STRK-W POINT'                                     
         MRKT  0186,'COOKEVILLE, TN'                                            
         MRKT  0187,'CORPUS CHRISTI'                                            
         MRKT  0189,'METRO FAIRFIELD COUNTY'                                    
         MRKT  0190,'DALLAS-FT. WORTH'                                          
         MRKT  0193,'DANBURY, CT'                                               
         MRKT  0199,'DAYTON'                                                    
         MRKT  0202,'DAYTONA BEACH'                                             
         MRKT  0204,'DECATUR, IL'                                               
         MRKT  0205,'DENVER-BOULDER'                                            
         MRKT  0208,'DES MOINES'                                                
         MRKT  0211,'DETROIT'                                                   
         MRKT  0214,'DOTHAN, AL'                                                
         MRKT  0217,'DUBUQUE, IA'                                               
         MRKT  0220,'DULUTH-SUPERIOR'                                           
         MRKT  0223,'EAU CLAIRE, WI'                                            
         MRKT  0226,'EL PASO'                                                   
         MRKT  0225,'ELIZABETH CITY-NAGS HEAD'                                  
         MRKT  0227,'ELMIRA-CORNING, NY'                                        
         MRKT  0229,'ERIE'                                                      
         MRKT  0232,'EUGENE-SPRINGFIELD'                                        
         MRKT  0233,'EUREKA, CA'                                                
         MRKT  0235,'EVANSVILLE'                                                
         MRKT  0238,'FARGO-MOORHEAD'                                            
         MRKT  0244,'FAYETTEVILLE, NC'                                          
         MRKT  0241,'FAYETTEVILLE-SPRINGDALE,'                                  
         MRKT  0245,'FLAGSTAFF-PRESCOTT, AZ'                                    
         MRKT  0247,'FLINT'                                                     
         MRKT  0250,'FLORENCE, SC'                                              
         MRKT  0251,'FLORENCE-MUSCLE SHOAL,AL'                                  
         MRKT  0253,'FREDERICK, MD'                                             
         MRKT  0254,'FREDERICKSBURG, VA'                                        
         MRKT  0256,'FRESNO'                                                    
         MRKT  0257,'FT. COLLINS-GREELEY, CO'                                   
         MRKT  0259,'FT. MYERS, FL'                                             
         MRKT  0262,'FT. PIERCE, FL'                                            
         MRKT  0265,'FT. SMITH, AR'                                             
         MRKT  0268,'FT. WALTON BEACH, FL'                                      
         MRKT  0271,'FT. WAYNE'                                                 
         MRKT  0274,'GAINESVILLE-OCALA, FL'                                     
         MRKT  0277,'GRAND FORKS, ND-MN'                                        
         MRKT  0280,'GRAND JUNCTION, CO'                                        
         MRKT  0283,'GRAND RAPIDS'                                              
         MRKT  0286,'GREAT FALLS, MT'                                           
         MRKT  0289,'GREEN BAY'                                                 
         MRKT  0292,'GREENSBORO-WS-HIGH POINT'                                  
         MRKT  0295,'GREENVILLE-SPARTANBURG'                                    
         MRKT  0294,'GREENVILLE/NEW BERN, NC'                                   
         MRKT  0298,'HAGERSTN-CHAMBRSG-WAYNSB,'                                 
         MRKT  0301,'HARRISBURG-LEBANON-CARLIS'                                 
         MRKT  0302,'HARRISONBURG, VA'                                          
         MRKT  0304,'HARTFORD-NEW BRITAIN-MIDD'                                 
         MRKT  0307,'HONOLULU'                                                  
         MRKT  0310,'HOUSTON-GALVESTON'                                         
         MRKT  0313,'HUNTINGTON-ASHLAND'                                        
         MRKT  0316,'HUNTSVILLE'                                                
         MRKT  0319,'INDIANAPOLIS'                                              
         MRKT  0322,'ITHACA, NY'                                                
         MRKT  0325,'JACKSON, MS'                                               
         MRKT  0327,'JACKSON, TN'                                               
         MRKT  0328,'JACKSONVILLE'                                              
         MRKT  0331,'JOHNSON CITY-KINGSPORT-BR'                                 
         MRKT  0334,'JOHNSTOWN'                                                 
         MRKT  0335,'JONESBORO, AR'                                             
         MRKT  0337,'JOPLIN, MO'                                                
         MRKT  0340,'KALAMAZOO'                                                 
         MRKT  0343,'KANSAS CITY'                                               
         MRKT  0346,'KILLEEN-TEMPLE, TX'                                        
         MRKT  0349,'KNOXVILLE'                                                 
         MRKT  0355,'LAFAYETTE, IN'                                             
         MRKT  0358,'LAFAYETTE, LA'                                             
         MRKT  0361,'LAKE CHARLES, LA'                                          
         MRKT  0364,'LAKELAND-WINTER HAVEN'                                     
         MRKT  0367,'LANCASTER, PA'                                             
         MRKT  0370,'LANSING-EAST LANSING'                                      
         MRKT  0371,'LAREDO, TX'                                                
         MRKT  0373,'LAS VEGAS'                                                 
         MRKT  0376,'LAUREL-HATTIESBURG, MS'                                    
         MRKT  0379,'LAWTON, OK'                                                
         MRKT  0381,'LEWISTON-AUBURN, ME'                                       
         MRKT  0382,'LEXINGTON-FAYETTE, KY'                                     
         MRKT  0384,'LIMA, OH'                                                  
         MRKT  0385,'LINCOLN'                                                   
         MRKT  0388,'LITTLE ROCK'                                               
         MRKT  0394,'LOS ANGELES'                                               
         MRKT  0397,'LOUISVILLE'                                                
         MRKT  0400,'LUBBOCK'                                                   
         MRKT  0406,'MACON'                                                     
         MRKT  0409,'MADISON'                                                   
         MRKT  0412,'MANCHESTER'                                                
         MRKT  0413,'MANKATO-NEW ULM-ST. P, MN'                                 
         MRKT  0414,'MASON CITY, IA'                                            
         MRKT  0415,'MCALLEN-BROWNSVILLE'                                       
         MRKT  0417,'MEADVILLE-FRANKLIN, PA'                                    
         MRKT  0418,'MEDFORD-ASHLAND, OR'                                       
         MRKT  0421,'MELBOURNE-TITUSVILLE-COCO'                                 
         MRKT  0424,'MEMPHIS'                                                   
         MRKT  0426,'MERCED, CA'                                                
         MRKT  0427,'MERIDIAN, MS'                                              
         MRKT  0430,'MIAMI-FT. LAUDERDALE-HOLL'                                 
         MRKT  0431,'MIDDLESEX-SOMERSET-UNION'                                  
         MRKT  0433,'MILWAUKEE-RACINE'                                          
         MRKT  0436,'MINNEAPOLIS-ST. PAUL'                                      
         MRKT  0442,'MOBILE'                                                    
         MRKT  0445,'MODESTO'                                                   
         MRKT  0448,'MONMOUTH-OCEAN, NJ'                                        
         MRKT  0451,'MONROE, LA'                                                
         MRKT  0454,'MONTEREY-SALINAS-SANTA CR'                                 
         MRKT  0457,'MONTGOMERY, AL'                                            
         MRKT  0459,'MORGANTOWN-CLARKSBRG-FAIR'                                 
         MRKT  0460,'MORRISTOWN, NJ'                                            
         MRKT  0461,'MUNCIE-MARION, IN'                                         
         MRKT  0462,'MUSKEGON, MI'                                              
         MRKT  0464,'MYRTLE BEACH, SC'                                          
         MRKT  0469,'NASHVILLE'                                                 
         MRKT  0472,'NASSAU-SUFFOLK (LONG ISLA'                                 
         MRKT  0475,'NEW BEDFORD-FALL RIVER, M'                                 
         MRKT  0478,'NEW HAVEN'                                                 
         MRKT  0481,'NEW LONDON, CT'                                            
         MRKT  0484,'NEW ORLEANS'                                               
         MRKT  0485,'RAPID CITY-SPEARFISH, SD'        SPEC-52027                
         MRKT  0487,'NEW YORK'                                                  
         MRKT  0490,'NEWBURGH-MIDDLETOWN, NY'                                   
         MRKT  0493,'NORFOLK-VIRGINIA BEACH-NW'                                 
*        MRKT  0496,'NORTHWEST MICHIGAN, MI'          03/17/04                  
         MRKT  0496,'TRAVERSE CITY-PETOSKEY'                                    
         MRKT  0499,'ODESSA-MIDLAND, TX'                                        
         MRKT  0502,'OKLAHOMA CITY'                                             
         MRKT  0503,'OLEAN, NY'                                                 
         MRKT  0505,'OMAHA-COUNCIL BLUFFS'                                      
         MRKT  0508,'ORLANDO'                                                   
         MRKT  0511,'OWENSBORO, KY'                                             
         MRKT  0514,'OXNARD-VENTURA'                                            
         MRKT  0517,'PALM SPRINGS, CA'                                          
         MRKT  0520,'PANAMA CITY, FL'                                           
         MRKT  0523,'PARKERSBURG-MARIETTA, WV-'                                 
         MRKT  0529,'PENSACOLA'                                                 
         MRKT  0532,'PEORIA'                                                    
         MRKT  0535,'PHILADELPHIA'                                              
         MRKT  0538,'PHOENIX'                                                   
         MRKT  0540,'PITTSBURG, KS (SE KANSAS)'                                 
         MRKT  0541,'PITTSBURGH'                                                
         MRKT  0544,'PORTLAND, ME'                                              
         MRKT  0547,'PORTLAND, OR'                                              
         MRKT  0550,'PORTSMOUTH-DOVER-ROCHESTE'                                 
         MRKT  0553,'POUGHKEEPSIE, NY'                                          
         MRKT  0556,'PROVIDENCE-WARWICK-PAWTUC'                                 
         MRKT  0559,'PUEBLO'                                                    
         MRKT  0560,'PUERTO RICO'                                               
         MRKT  0562,'QUAD CITIES (DAVNPRT-RI-M'                                 
         MRKT  0565,'RALEIGH-DURHAM'                                            
         MRKT  0568,'RAPID CITY, SD'                                            
         MRKT  0571,'READING, PA'                                               
         MRKT  0574,'REDDING, CA'                                               
         MRKT  0577,'RENO'                                                      
         MRKT  0580,'RICHMOND, VA'                                              
         MRKT  0583,'RIVERSIDE-SAN BERNARDINO'                                  
         MRKT  0586,'ROANOKE-LYNCHBURG'                                         
         MRKT  0589,'ROCHESTER, MN'                                             
         MRKT  0592,'ROCHESTER, NY'                                             
         MRKT  0595,'ROCKFORD, IL'                                              
         MRKT  0598,'SACRAMENTO'                                                
         MRKT  0601,'SAGINAW-BAY CITY-MIDLAND'                                  
         MRKT  0604,'SALISBURY-OCEAN CITY'                                      
         MRKT  0607,'SALT LAKE CITY-OGDEN-PROV'                                 
         MRKT  0610,'SAN ANGELO, TX'                                            
         MRKT  0613,'SAN ANTONIO'                                               
         MRKT  0616,'SAN DIEGO'                                                 
         MRKT  0622,'SAN FRANCISCO'                                             
         MRKT  0625,'SAN JOSE'                                                  
         MRKT  0626,'SAN LUIS OBISPO, CA'                                       
         MRKT  0628,'SANTA BARBARA, CA'                                         
         MRKT  0631,'SANTA FE, NM'                                              
         MRKT  0633,'SANTA MARIA-LOMPOC, CA'                                    
         MRKT  0634,'SANTA ROSA, CA'                                            
         MRKT  0637,'SARASOTA-BRADENTON'                                        
         MRKT  0640,'SAVANNAH'                                                  
         MRKT  0643,'SEATTLE-TACOMA'                                            
         MRKT  0644,'SEBRING, FL'                                               
         MRKT  0645,'SHEBOYGAN, WI'                                             
         MRKT  0646,'SHREVEPORT'                                                
         MRKT  0649,'SIOUX CITY, IA'                                            
         MRKT  0652,'SIOUX FALLS'                                               
         MRKT  0655,'SOUTH BEND'                                                
         MRKT  0656,'SOUTHERN IL, (MARION,CARB'                                 
         MRKT  0658,'SPOKANE'                                                   
         MRKT  0661,'SPRINGFIELD, IL'                                           
         MRKT  0664,'SPRINGFIELD, MA'                                           
         MRKT  0667,'SPRINGFIELD, MO'                                           
         MRKT  0670,'ST. CLOUD, MN'                                             
         MRKT  0671,'ST. GEORGE-CEDAR CITY, UT'                                 
         MRKT  0673,'ST. LOUIS'                                                 
         MRKT  0676,'STAMFORD-NORWALK, CT'                                      
         MRKT  0678,'STATE COLLEGE, PA'                                         
         MRKT  0685,'STOCKTON'                                                  
         MRKT  0686,'SUSSEX, NJ'                                                
         MRKT  0688,'SYRACUSE'                                                  
         MRKT  0691,'TALLAHASSEE'                                               
         MRKT  0694,'TAMPA-ST. PETERSBURG-CLEA'                                 
         MRKT  0697,'TERRE HAUTE'                                               
         MRKT  0700,'TEXARKANA, TX'                                             
         MRKT  0703,'TOLEDO'                                                    
         MRKT  0706,'TOPEKA'                                                    
         MRKT  0709,'TRENTON, NJ'                                               
         MRKT  0712,'TRI-CITIES, WA (RCHLND-KN'                                 
         MRKT  0715,'TUCSON'                                                    
         MRKT  0718,'TULSA'                                                     
         MRKT  0719,'TUPELO, MS'                                                
         MRKT  0721,'TUSCALOOSA, AL'                                            
         MRKT  0724,'TYLER, TX'                                                 
         MRKT  0727,'UTICA-ROME'                                                
         MRKT  0729,'VICTOR VALLEY'                                             
         MRKT  0731,'VISALIA-TULARE-HANFORD, C'                                 
         MRKT  0733,'WACO'                                                      
         MRKT  0736,'WASHINGTON, DC'                                            
         MRKT  0742,'WATERLOO-CEDAR FALLS'                                      
         MRKT  0745,'WATERTOWN, NY'                                             
         MRKT  0746,'WAUSAU-STEVENS POINT, WI'                                  
         MRKT  0748,'WEST PALM BEACH-BOCA RATO'                                 
         MRKT  0749,'WESTCHESTER'                                               
         MRKT  0751,'WHEELING'                                                  
         MRKT  0754,'WICHITA'                                                   
         MRKT  0757,'WICHITA FALLS, TX'                                         
         MRKT  0760,'WILKES BARRE-SCRANTON'                                     
         MRKT  0763,'WILLIAMSPORT, PA'                                          
         MRKT  0766,'WILMINGTON, DE'                                            
         MRKT  0769,'WILMINGTON, NC'                                            
         MRKT  0770,'WINCHESTER, VA'                                            
         MRKT  0772,'WORCESTER'                                                 
         MRKT  0775,'YAKIMA, WA'                                                
         MRKT  0778,'YORK'                                                      
         MRKT  0781,'YOUNGSTOWN-WARREN'                                         
         EMTBL                                                                  
*                                                                               
         EJECT                                                                  
* SRCMKTS                                                                       
         SMTBL ST                   STRATEGY RESEARCH TPT                       
         MRKT  0001,'LOS ANGELES'                                               
         MRKT  0002,'NEW YORK'                                                  
         MRKT  0003,'MIAMI'                                                     
         MRKT  0004,'SAN ANTONIO'                                               
         MRKT  0005,'SAN FRANCISCO'                                             
         MRKT  0006,'CHICAGO'                                                   
         MRKT  0007,'HOUSTON'                                                   
         MRKT  0008,'MC ALLEN/BROWNSVILLE(LRGV)'                                
         MRKT  0009,'EL PASO'                                                   
         MRKT  0010,'ALBUQUERQUE'                                               
         MRKT  0011,'FRESNO'                                                    
         MRKT  0012,'SAN DIEGO'                                                 
         MRKT  0013,'DALLAS/FT. WORTH'                                          
         MRKT  0014,'PHOENIX'                                                   
         MRKT  0015,'SACRAMENTO'                                                
         MRKT  0016,'CORPUS CHRISTI'                                            
         MRKT  0017,'DENVER'                                                    
         MRKT  0018,'PHILADELPHIA'                                              
         MRKT  0019,'TUCSON'                                                    
         MRKT  0020,'AUSTIN'                                                    
         MRKT  0021,'SALINAS/MONTEREY'                                          
         MRKT  0022,'TAMPA/ST. PETERSBURG'                                      
         MRKT  0023,'BOSTON'                                                    
         MRKT  0024,'BAKERSFIELD'                                               
         MRKT  0025,'WASHINGTON DC'                                             
         MRKT  0026,'RENO'                                                      
         MRKT  0027,'SANTA BARB/MARIA S.LUIS OBS'                               
         MRKT  0028,'DETROIT'                                                   
         MRKT  0029,'ST. LOUIS'                                                 
         MRKT  0030,'LAS VEGAS'                                                 
         MRKT  0031,'TIJUANA M91'                                               
         MRKT  0034,'TIJUANA N90'                                               
         MRKT  0035,'MATAMOROS'                                                 
         MRKT  0036,'MILWAUKEE'                                                 
         MRKT  0061,'ALAMEDA'                                                   
         MRKT  0063,'S.F. COUNTY'                                               
         MRKT  0064,'SAN MATEO COUNTY'                                          
         MRKT  0065,'MARKET 65'                                                 
         MRKT  0069,'SAN JOSE METRO'                                            
         MRKT  0095,'EASTERN REGION'                                            
         MRKT  0096,'CALIFORNIA STATE NETWORK'                                  
         MRKT  0097,'TEXAS STATE NETWORK'                                       
         MRKT  0098,'NETWORK PR'                                                
         MRKT  0099,'NETWORK TP'                                                
         EMTBL                                                                  
*                                                                               
         DC    AL2(0)              NO MORE MARKET TABLES                        
TBL_MRKTNAMTX DS 0X                                                             
         EJECT                                                                  
                                                                                
* TABLE OF NEW-STYLE DEMO FORMULAS (USED FOR AUDIENCE ESTIMATOR)                
* ALL DSECTS ARE IN DEDEMFORMD                                                  
                                                                                
TBL_NFORMTAB DS 0D                                                              
         DC    CL8'NFORMTAB'                                                    
         DS    XL6                                                              
*                                                                               
         DC    AL2(0)              TABLE ENTRIES HAVE VARIABLE LENGTHS          
         DC    AL4(TBL_NFORMTABX-TBL_NFORMTABS)                                 
                                                                                
TBL_NFORMTABS DS 0X                START OF FORMULA TABLES                      
                                                                                
* TABLE ID ANN0174: NET BROADCAST FORMULAS STARTING DEC31/1973                  
                                                                                
         FMSBK ANN,01/74,WEEKLY,ANN0174    FILE/MEDIA/SOURCE/BOOK/TYPE          
                                                                                
         SFTID ANN0174             START OF FORMULAS FOR THIS F/M/S/BK          
                                                                                
*---------------------------------------------------------------------*         
* GENERAL MACRO FORMULAS                                              *         
*---------------------------------------------------------------------*         
                                                                                
         GMACR Y,WY/WEIGHT         RAW IMPRESSIONS                              
         GMACR U,WU/WEIGHT         UNIVERSES                                    
         GMACR Z,WZ/WEIGHT         PROGRAM PUT                                  
         GMACR B,WB/WEIGHT         GAA RAW IMPRESSIONS                          
                                                                                
         GMACR I,ROUND(Y,0)/1000.0                   IMPRESSIONS                
         GMACR R,ROUND(Y,0)/ROUND(U,0)/100.0         RATING                     
         GMACR V,ROUND(Y,0)*1000.0/ROUND(Y#1,0)      VPH                        
         GMACR S,ROUND(Y,0)*100.0/ROUND(Z,0)         SHARE                      
         GMACR N,ROUND(B,0)/1000.0                   GAA IMPRESSIONS            
         GMACR L,ROUND(B,0)/ROUND(U,0)/100.0         GAA RATING                 
         GMACR M,ROUND(B,0)*1000.0/ROUND(B#1,0)      GAA VPH                    
         FFEND                                                                  
                                                                                
*---------------------------------------------------------------------*         
* SPECIFIC MACRO FORMULAS                                             *         
*---------------------------------------------------------------------*         
                                                                                
* NONE RIGHT NOW                                                                
                                                                                
*---------------------------------------------------------------------*         
* OVERRIDE RELATIONAL FORMULAS                                        *         
*---------------------------------------------------------------------*         
                                                                                
         OVREL R,I,I*10/U          GET R FROM I                                 
         OVREL R,V,V*I#1/U/10      GET R FROM V                                 
                                                                                
         OVREL I,R,R*U/10          GET I FROM R                                 
         OVREL I,V,V*I#1/1000      GET I FROM V                                 
                                                                                
         OVREL V,R,R*10*U/I#1      GET V FROM R                                 
         OVREL V,I,I*1000/I#1      GET V FROM I                                 
         FFEND                                                                  
                                                                                
*---------------------------------------------------------------------*         
* DEMO FORMULAS BY MODIFIER                                           *         
*                                                                               
* DFORM MACRO:                                                                  
* - THE FIRST NUMBER IS THE DEMO # FROM DENTIDEMS                               
* - THE SECOND GROUP OF NUMBERS IN PARENTHESIS ARE THE INDEX INTO               
*   FTYP3 OR FTYP4.  THESE NUMBERS ARE THE "BUILDING BLOCKS" FOR THE            
*   DEMO #.  THIS IS PASSED TO NATIONAL DEMO ESTIMATOR (AUDIENCE) FOR           
*   CALCULATIONS.                                                               
*                                                                               
* FOR EXAMPLE:                                                                  
*  DFORM  770,(1,2)                                                             
* 770 = WOMEN 2-8 FROM DENTIDEMS                                                
* (1,2): 1 = 197 IN FTYP3 OR 325 IN FTYP4                                       
*        2 = 198 IN FTYP3 OR 326 IN FTYP4                                       
*                                                                               
* WOMEN 2-8 = (WOMEN 2-5) + (WOMEN 6-8)                                         
*                                                                               
* ASSUMPTION IS THE DFORM MACRO IS USING ADDITION AS IT'S OPERATOR              
*---------------------------------------------------------------------*         
                                                                                
         FFTID                         START OF FORMULAS                        
                                                                                
* RAW IMPRESSIONS (Y)                                                           
         FMOD  WY                                                               
                                                                                
         FTYP3 (197,198,199,24,26,68,69,196,207,208,209,203,55,58,     *        
               60,217,218,219,74,76,112,113,227,228,229,195,213,       *        
               105,108,110,120,230,231,232,233,234,235,236,1)                   
                                                                                
         FTYP4 (325,326,327,24,26,68,69,324,335,336,337,331,55,58,     *        
               60,345,346,347,74,76,112,113,355,356,357,323,341,       *        
               105,108,110,120,358,359,360,361,362,363,364,1)                   
                                                                                
         DFORM 325,(1)                                                          
         DFORM 326,(2)                                                          
         DFORM 770,(1,2)                                                        
         DFORM 327,(3)                                                          
         DFORM 119,(1,2,3)                                                      
         DFORM 21,(2,3)                                                         
         DFORM 24,(4)                                                           
         DFORM 772,(1,2,3,4)                                                    
         DFORM 786,(2,3,4)                                                      
         DFORM 61,(3,4)                                                         
         DFORM 26,(5)                                                           
         DFORM 773,(1,2,3,4,5)                                                  
         DFORM 787,(2,3,4,5)                                                    
         DFORM 800,(3,4,5)                                                      
         DFORM 25,(4,5)                                                         
         DFORM 68,(6)                                                           
         DFORM 774,(1,2,3,4,5,6)                                                
         DFORM 788,(2,3,4,5,6)                                                  
         DFORM 801,(3,4,5,6)                                                    
         DFORM 23,(4,5,6)                                                       
         DFORM 824,(5,6)                                                        
         DFORM 69,(7)                                                           
         DFORM 775,(1,2,3,4,5,6,7)                                              
         DFORM 789,(2,3,4,5,6,7)                                                
         DFORM 802,(3,4,5,6,7)                                                  
         DFORM 28,(4,5,6,7)                                                     
         DFORM 34,(5,6,7)                                                       
         DFORM 40,(6,7)                                                         
         DFORM 324,(8)                                                          
         DFORM 776,(1,2,3,4,5,6,7,8)                                            
         DFORM 790,(2,3,4,5,6,7,8)                                              
         DFORM 803,(3,4,5,6,7,8)                                                
         DFORM 815,(4,5,6,7,8)                                                  
         DFORM 826,(5,6,7,8)                                                    
         DFORM 836,(6,7,8)                                                      
         DFORM 845,(7,8)                                                        
         DFORM 335,(9)                                                          
         DFORM 777,(1,2,3,4,5,6,7,8,9)                                          
         DFORM 791,(2,3,4,5,6,7,8,9)                                            
         DFORM 804,(3,4,5,6,7,8,9)                                              
         DFORM 29,(4,5,6,7,8,9)                                                 
         DFORM 35,(5,6,7,8,9)                                                   
         DFORM 41,(6,7,8,9)                                                     
         DFORM 70,(7,8,9)                                                       
         DFORM 46,(8,9)                                                         
         DFORM 336,(10)                                                         
         DFORM 778,(1,2,3,4,5,6,7,8,9,10)                                       
         DFORM 792,(2,3,4,5,6,7,8,9,10)                                         
         DFORM 805,(3,4,5,6,7,8,9,10)                                           
         DFORM 817,(4,5,6,7,8,9,10)                                             
         DFORM 828,(5,6,7,8,9,10)                                               
         DFORM 838,(6,7,8,9,10)                                                 
         DFORM 847,(7,8,9,10)                                                   
         DFORM 855,(8,9,10)                                                     
         DFORM 862,(9,10)                                                       
         DFORM 337,(11)                                                         
         DFORM 779,(1,2,3,4,5,6,7,8,9,10,11)                                    
         DFORM 793,(2,3,4,5,6,7,8,9,10,11)                                      
         DFORM 806,(3,4,5,6,7,8,9,10,11)                                        
         DFORM 818,(4,5,6,7,8,9,10,11)                                          
         DFORM 829,(5,6,7,8,9,10,11)                                            
         DFORM 328,(6,7,8,9,10,11)                                              
         DFORM 848,(7,8,9,10,11)                                                
         DFORM 329,(8,9,10,11)                                                  
         DFORM 863,(9,10,11)                                                    
         DFORM 330,(10,11)                                                      
         DFORM 331,(12)                                                         
         DFORM 780,(1,2,3,4,5,6,7,8,9,10,11,12)                                 
         DFORM 794,(2,3,4,5,6,7,8,9,10,11,12)                                   
         DFORM 807,(3,4,5,6,7,8,9,10,11,12)                                     
         DFORM 30,(4,5,6,7,8,9,10,11,12)                                        
         DFORM 36,(5,6,7,8,9,10,11,12)                                          
         DFORM 42,(6,7,8,9,10,11,12)                                            
         DFORM 71,(7,8,9,10,11,12)                                              
         DFORM 47,(8,9,10,11,12)                                                
         DFORM 864,(9,10,11,12)                                                 
         DFORM 51,(10,11,12)                                                    
         DFORM 875,(11,12)                                                      
         DFORM 55,(13)                                                          
         DFORM 781,(1,2,3,4,5,6,7,8,9,10,11,12,13)                              
         DFORM 795,(2,3,4,5,6,7,8,9,10,11,12,13)                                
         DFORM 808,(3,4,5,6,7,8,9,10,11,12,13)                                  
         DFORM 31,(4,5,6,7,8,9,10,11,12,13)                                     
         DFORM 37,(5,6,7,8,9,10,11,12,13)                                       
         DFORM 43,(6,7,8,9,10,11,12,13)                                         
         DFORM 72,(7,8,9,10,11,12,13)                                           
         DFORM 48,(8,9,10,11,12,13)                                             
         DFORM 865,(9,10,11,12,13)                                              
         DFORM 52,(10,11,12,13)                                                 
         DFORM 876,(11,12,13)                                                   
         DFORM 332,(12,13)                                                      
         DFORM 58,(14)                                                          
         DFORM 782,(1,2,3,4,5,6,7,8,9,10,11,12,13,14)                           
         DFORM 796,(2,3,4,5,6,7,8,9,10,11,12,13,14)                             
         DFORM 809,(3,4,5,6,7,8,9,10,11,12,13,14)                               
         DFORM 32,(4,5,6,7,8,9,10,11,12,13,14)                                  
         DFORM 38,(5,6,7,8,9,10,11,12,13,14)                                    
         DFORM 44,(6,7,8,9,10,11,12,13,14)                                      
         DFORM 73,(7,8,9,10,11,12,13,14)                                        
         DFORM 49,(8,9,10,11,12,13,14)                                          
         DFORM 866,(9,10,11,12,13,14)                                           
         DFORM 53,(10,11,12,13,14)                                              
         DFORM 877,(11,12,13,14)                                                
         DFORM 333,(12,13,14)                                                   
         DFORM 56,(13,14)                                                       
         DFORM 60,(15)                                                          
         DFORM 783,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)                        
         DFORM 797,(2,3,4,5,6,7,8,9,10,11,12,13,14,15)                          
         DFORM 810,(3,4,5,6,7,8,9,10,11,12,13,14,15)                            
         DFORM 33,(4,5,6,7,8,9,10,11,12,13,14,15)                               
         DFORM 39,(5,6,7,8,9,10,11,12,13,14,15)                                 
         DFORM 45,(6,7,8,9,10,11,12,13,14,15)                                   
         DFORM 67,(7,8,9,10,11,12,13,14,15)                                     
         DFORM 50,(8,9,10,11,12,13,14,15)                                       
         DFORM 867,(9,10,11,12,13,14,15)                                        
         DFORM 54,(10,11,12,13,14,15)                                           
         DFORM 878,(11,12,13,14,15)                                             
         DFORM 334,(12,13,14,15)                                                
         DFORM 57,(13,14,15)                                                    
         DFORM 59,(14,15)                                                       
         DFORM 345,(16)                                                         
         DFORM 346,(17)                                                         
         DFORM 1026,(16,17)                                                     
         DFORM 347,(18)                                                         
         DFORM 20,(16,17,18)                                                    
         DFORM 22,(17,18)                                                       
         DFORM 74,(19)                                                          
         DFORM 1028,(16,17,18,19)                                               
         DFORM 1042,(17,18,19)                                                  
         DFORM 62,(18,19)                                                       
         DFORM 76,(20)                                                          
         DFORM 1029,(16,17,18,19,20)                                            
         DFORM 314,(17,18,19,20)                                                
         DFORM 1056,(18,19,20)                                                  
         DFORM 75,(19,20)                                                       
         DFORM 112,(21)                                                         
         DFORM 1030,(16,17,18,19,20,21)                                         
         DFORM 1044,(17,18,19,20,21)                                            
         DFORM 1057,(18,19,20,21)                                               
         DFORM 111,(19,20,21)                                                   
         DFORM 1080,(20,21)                                                     
         DFORM 113,(22)                                                         
         DFORM 1031,(16,17,18,19,20,21,22)                                      
         DFORM 1045,(17,18,19,20,21,22)                                         
         DFORM 1058,(18,19,20,21,22)                                            
         DFORM 78,(19,20,21,22)                                                 
         DFORM 84,(20,21,22)                                                    
         DFORM 90,(21,22)                                                       
         DFORM 355,(23)                                                         
         DFORM 1032,(16,17,18,19,20,21,22,23)                                   
         DFORM 1046,(17,18,19,20,21,22,23)                                      
         DFORM 1059,(18,19,20,21,22,23)                                         
         DFORM 1071,(19,20,21,22,23)                                            
         DFORM 1082,(20,21,22,23)                                               
         DFORM 1092,(21,22,23)                                                  
         DFORM 1101,(22,23)                                                     
         DFORM 356,(24)                                                         
         DFORM 1033,(16,17,18,19,20,21,22,23,24)                                
         DFORM 1047,(17,18,19,20,21,22,23,24)                                   
         DFORM 1060,(18,19,20,21,22,23,24)                                      
         DFORM 79,(19,20,21,22,23,24)                                           
         DFORM 85,(20,21,22,23,24)                                              
         DFORM 91,(21,22,23,24)                                                 
         DFORM 114,(22,23,24)                                                   
         DFORM 96,(23,24)                                                       
         DFORM 357,(25)                                                         
         DFORM 1034,(16,17,18,19,20,21,22,23,24,25)                             
         DFORM 1048,(17,18,19,20,21,22,23,24,25)                                
         DFORM 1061,(18,19,20,21,22,23,24,25)                                   
         DFORM 1073,(19,20,21,22,23,24,25)                                      
         DFORM 1084,(20,21,22,23,24,25)                                         
         DFORM 1094,(21,22,23,24,25)                                            
         DFORM 1103,(22,23,24,25)                                               
         DFORM 1111,(23,24,25)                                                  
         DFORM 1118,(24,25)                                                     
         DFORM 323,(26)                                                         
         DFORM 1035,(16,17,18,19,20,21,22,23,24,25,26)                          
         DFORM 1049,(17,18,19,20,21,22,23,24,25,26)                             
         DFORM 1062,(18,19,20,21,22,23,24,25,26)                                
         DFORM 1074,(19,20,21,22,23,24,25,26)                                   
         DFORM 1085,(20,21,22,23,24,25,26)                                      
         DFORM 338,(21,22,23,24,25,26)                                          
         DFORM 1104,(22,23,24,25,26)                                            
         DFORM 339,(23,24,25,26)                                                
         DFORM 1119,(24,25,26)                                                  
         DFORM 340,(25,26)                                                      
         DFORM 341,(27)                                                         
         DFORM 1036,(16,17,18,19,20,21,22,23,24,25,26,27)                       
         DFORM 1050,(17,18,19,20,21,22,23,24,25,26,27)                          
         DFORM 1063,(18,19,20,21,22,23,24,25,26,27)                             
         DFORM 80,(19,20,21,22,23,24,25,26,27)                                  
         DFORM 86,(20,21,22,23,24,25,26,27)                                     
         DFORM 92,(21,22,23,24,25,26,27)                                        
         DFORM 115,(22,23,24,25,26,27)                                          
         DFORM 97,(23,24,25,26,27)                                              
         DFORM 1120,(24,25,26,27)                                               
         DFORM 101,(25,26,27)                                                   
         DFORM 1131,(26,27)                                                     
         DFORM 105,(28)                                                         
         DFORM 1037,(16,17,18,19,20,21,22,23,24,25,26,27,28)                    
         DFORM 1051,(17,18,19,20,21,22,23,24,25,26,27,28)                       
         DFORM 1064,(18,19,20,21,22,23,24,25,26,27,28)                          
         DFORM 81,(19,20,21,22,23,24,25,26,27,28)                               
         DFORM 87,(20,21,22,23,24,25,26,27,28)                                  
         DFORM 93,(21,22,23,24,25,26,27,28)                                     
         DFORM 116,(22,23,24,25,26,27,28)                                       
         DFORM 98,(23,24,25,26,27,28)                                           
         DFORM 1121,(24,25,26,27,28)                                            
         DFORM 102,(25,26,27,28)                                                
         DFORM 1132,(26,27,28)                                                  
         DFORM 342,(27,28)                                                      
         DFORM 108,(29)                                                         
         DFORM 1038,(16,17,18,19,20,21,22,23,24,25,26,27,28,29)                 
         DFORM 1052,(17,18,19,20,21,22,23,24,25,26,27,28,29)                    
         DFORM 1065,(18,19,20,21,22,23,24,25,26,27,28,29)                       
         DFORM 82,(19,20,21,22,23,24,25,26,27,28,29)                            
         DFORM 88,(20,21,22,23,24,25,26,27,28,29)                               
         DFORM 94,(21,22,23,24,25,26,27,28,29)                                  
         DFORM 117,(22,23,24,25,26,27,28,29)                                    
         DFORM 99,(23,24,25,26,27,28,29)                                        
         DFORM 1122,(24,25,26,27,28,29)                                         
         DFORM 103,(25,26,27,28,29)                                             
         DFORM 1133,(26,27,28,29)                                               
         DFORM 343,(27,28,29)                                                   
         DFORM 106,(28,29)                                                      
         DFORM 110,(30)                                                         
         DFORM 1039,(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)              
         DFORM 1053,(17,18,19,20,21,22,23,24,25,26,27,28,29,30)                 
         DFORM 1066,(18,19,20,21,22,23,24,25,26,27,28,29,30)                    
         DFORM 83,(19,20,21,22,23,24,25,26,27,28,29,30)                         
         DFORM 89,(20,21,22,23,24,25,26,27,28,29,30)                            
         DFORM 95,(21,22,23,24,25,26,27,28,29,30)                               
         DFORM 118,(22,23,24,25,26,27,28,29,30)                                 
         DFORM 100,(23,24,25,26,27,28,29,30)                                    
         DFORM 1123,(24,25,26,27,28,29,30)                                      
         DFORM 104,(25,26,27,28,29,30)                                          
         DFORM 1134,(26,27,28,29,30)                                            
         DFORM 344,(27,28,29,30)                                                
         DFORM 107,(28,29,30)                                                   
         DFORM 109,(29,30)                                                      
         DFORM 121,(1,16)                                                       
         DFORM 289,(2,17)                                                       
         DFORM 1282,(1,2,16,17)                                                 
         DFORM 290,(3,18)                                                       
         DFORM 122,(1,2,3,16,17,18)                                             
         DFORM 123,(2,3,17,18)                                                  
         DFORM 124,(4,19)                                                       
         DFORM 299,(1,2,3,4,16,17,18,19)                                        
         DFORM 306,(2,3,4,17,18,19)                                             
         DFORM 380,(3,4,18,19)                                                  
         DFORM 126,(5,20)                                                       
         DFORM 300,(1,2,3,4,5,16,17,18,19,20)                                   
         DFORM 307,(2,3,4,5,17,18,19,20)                                        
         DFORM 1312,(3,4,5,18,19,20)                                            
         DFORM 125,(4,5,19,20)                                                  
         DFORM 316,(6,21)                                                       
         DFORM 1286,(1,2,3,4,5,6,16,17,18,19,20,21)                             
         DFORM 1300,(2,3,4,5,6,17,18,19,20,21)                                  
         DFORM 1313,(3,4,5,6,18,19,20,21)                                       
         DFORM 315,(4,5,6,19,20,21)                                             
         DFORM 1336,(5,6,20,21)                                                 
         DFORM 317,(7,22)                                                       
         DFORM 301,(1,2,3,4,5,6,7,16,17,18,19,20,21,22)                         
         DFORM 308,(2,3,4,5,6,7,17,18,19,20,21,22)                              
         DFORM 1314,(3,4,5,6,7,18,19,20,21,22)                                  
         DFORM 256,(4,5,6,7,19,20,21,22)                                        
         DFORM 262,(5,6,7,20,21,22)                                             
         DFORM 268,(6,7,21,22)                                                  
         DFORM 291,(8,23)                                                       
         DFORM 1288,(1,2,3,4,5,6,7,8,16,17,18,19,20,21,22,23)                   
         DFORM 1302,(2,3,4,5,6,7,8,17,18,19,20,21,22,23)                        
         DFORM 1315,(3,4,5,6,7,8,18,19,20,21,22,23)                             
         DFORM 1327,(4,5,6,7,8,19,20,21,22,23)                                  
         DFORM 1338,(5,6,7,8,20,21,22,23)                                       
         DFORM 1348,(6,7,8,21,22,23)                                            
         DFORM 1357,(7,8,22,23)                                                 
         DFORM 292,(9,24)                                                       
         DFORM 302,(1,2,3,4,5,6,7,8,9,16,17,18,19,20,21,22,23,24)               
         DFORM 309,(2,3,4,5,6,7,8,9,17,18,19,20,21,22,23,24)                    
         DFORM 1316,(3,4,5,6,7,8,9,18,19,20,21,22,23,24)                        
         DFORM 257,(4,5,6,7,8,9,19,20,21,22,23,24)                              
         DFORM 263,(5,6,7,8,9,20,21,22,23,24)                                   
         DFORM 269,(6,7,8,9,21,22,23,24)                                        
         DFORM 318,(7,8,9,22,23,24)                                             
         DFORM 274,(8,9,23,24)                                                  
         DFORM 293,(10,25)                                                      
         DFORM 1290,(1,2,3,4,5,6,7,8,9,10,16,17,18,19,20,21,22,23,     *        
               24,25)                                                           
         DFORM 1304,(2,3,4,5,6,7,8,9,10,17,18,19,20,21,22,23,24,25)             
         DFORM 1317,(3,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25)                  
         DFORM 1329,(4,5,6,7,8,9,10,19,20,21,22,23,24,25)                       
         DFORM 1340,(5,6,7,8,9,10,20,21,22,23,24,25)                            
         DFORM 1350,(6,7,8,9,10,21,22,23,24,25)                                 
         DFORM 1359,(7,8,9,10,22,23,24,25)                                      
         DFORM 1367,(8,9,10,23,24,25)                                           
         DFORM 1374,(9,10,24,25)                                                
         DFORM 294,(11,26)                                                      
         DFORM 1291,(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,     *        
               23,24,25,26)                                                     
         DFORM 1305,(2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,       *        
               24,25,26)                                                        
         DFORM 1318,(3,4,5,6,7,8,9,10,11,18,19,20,21,22,23,24,25,      *        
               26)                                                              
         DFORM 1330,(4,5,6,7,8,9,10,11,19,20,21,22,23,24,25,26)                 
         DFORM 1341,(5,6,7,8,9,10,11,20,21,22,23,24,25,26)                      
         DFORM 348,(6,7,8,9,10,11,21,22,23,24,25,26)                            
         DFORM 1360,(7,8,9,10,11,22,23,24,25,26)                                
         DFORM 349,(8,9,10,11,23,24,25,26)                                      
         DFORM 1375,(9,10,11,24,25,26)                                          
         DFORM 350,(10,11,25,26)                                                
         DFORM 351,(12,27)                                                      
         DFORM 303,(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21,      *        
               22,23,24,25,26,27)                                               
         DFORM 310,(2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,        *        
               23,24,25,26,27)                                                  
         DFORM 1319,(3,4,5,6,7,8,9,10,11,12,18,19,20,21,22,23,24,      *        
               25,26,27)                                                        
         DFORM 258,(4,5,6,7,8,9,10,11,12,19,20,21,22,23,24,25,26,      *        
               27)                                                              
         DFORM 264,(5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27)                 
         DFORM 270,(6,7,8,9,10,11,12,21,22,23,24,25,26,27)                      
         DFORM 319,(7,8,9,10,11,12,22,23,24,25,26,27)                           
         DFORM 275,(8,9,10,11,12,23,24,25,26,27)                                
         DFORM 1376,(9,10,11,12,24,25,26,27)                                    
         DFORM 279,(10,11,12,25,26,27)                                          
         DFORM 1387,(11,12,26,27)                                               
         DFORM 283,(13,28)                                                      
         DFORM 304,(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,      *        
               21,22,23,24,25,26,27,28)                                         
         DFORM 311,(2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,20,21,        *        
               22,23,24,25,26,27,28)                                            
         DFORM 1320,(3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,      *        
               24,25,26,27,28)                                                  
         DFORM 259,(4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,      *        
               26,27,28)                                                        
         DFORM 265,(5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,        *        
               27,28)                                                           
         DFORM 271,(6,7,8,9,10,11,12,13,21,22,23,24,25,26,27,28)                
         DFORM 320,(7,8,9,10,11,12,13,22,23,24,25,26,27,28)                     
         DFORM 276,(8,9,10,11,12,13,23,24,25,26,27,28)                          
         DFORM 1377,(9,10,11,12,13,24,25,26,27,28)                              
         DFORM 280,(10,11,12,13,25,26,27,28)                                    
         DFORM 1388,(11,12,13,26,27,28)                                         
         DFORM 352,(12,13,27,28)                                                
         DFORM 286,(14,29)                                                      
         DFORM 305,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,      *        
               20,21,22,23,24,25,26,27,28,29)                                   
         DFORM 312,(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20,        *        
               21,22,23,24,25,26,27,28,29)                                      
         DFORM 1321,(3,4,5,6,7,8,9,10,11,12,13,14,18,19,20,21,22,      *        
               23,24,25,26,27,28,29)                                            
         DFORM 260,(4,5,6,7,8,9,10,11,12,13,14,19,20,21,22,23,24,      *        
               25,26,27,28,29)                                                  
         DFORM 266,(5,6,7,8,9,10,11,12,13,14,20,21,22,23,24,25,        *        
               26,27,28,29)                                                     
         DFORM 272,(6,7,8,9,10,11,12,13,14,21,22,23,24,25,26,27,       *        
               28,29)                                                           
         DFORM 321,(7,8,9,10,11,12,13,14,22,23,24,25,26,27,28,29)               
         DFORM 277,(8,9,10,11,12,13,14,23,24,25,26,27,28,29)                    
         DFORM 1378,(9,10,11,12,13,14,24,25,26,27,28,29)                        
         DFORM 281,(10,11,12,13,14,25,26,27,28,29)                              
         DFORM 1389,(11,12,13,14,26,27,28,29)                                   
         DFORM 353,(12,13,14,27,28,29)                                          
         DFORM 284,(13,14,28,29)                                                
         DFORM 288,(15,30)                                                      
         DFORM 127,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,      *        
               19,20,21,22,23,24,25,26,27,28,29,30)                             
         DFORM 313,(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,        *        
               20,21,22,23,24,25,26,27,28,29,30)                                
         DFORM 1322,(3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,      *        
               22,23,24,25,26,27,28,29,30)                                      
         DFORM 261,(4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,22,23,      *        
               24,25,26,27,28,29,30)                                            
         DFORM 267,(5,6,7,8,9,10,11,12,13,14,15,20,21,22,23,24,        *        
               25,26,27,28,29,30)                                               
         DFORM 273,(6,7,8,9,10,11,12,13,14,15,21,22,23,24,25,26,       *        
               27,28,29,30)                                                     
         DFORM 322,(7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,      *        
               29,30)                                                           
         DFORM 278,(8,9,10,11,12,13,14,15,23,24,25,26,27,28,29,30)              
         DFORM 1379,(9,10,11,12,13,14,15,24,25,26,27,28,29,30)                  
         DFORM 282,(10,11,12,13,14,15,25,26,27,28,29,30)                        
         DFORM 1390,(11,12,13,14,15,26,27,28,29,30)                             
         DFORM 354,(12,13,14,15,27,28,29,30)                                    
         DFORM 285,(13,14,15,28,29,30)                                          
         DFORM 287,(14,15,29,30)                                                
         DFORM 358,(32)                                                         
         DFORM 359,(33)                                                         
         DFORM 1859,(32,33)                                                     
         DFORM 360,(34)                                                         
         DFORM 1861,(32,33,34)                                                  
         DFORM 1870,(33,34)                                                     
         DFORM 361,(35)                                                         
         DFORM 1863,(32,33,34,35)                                               
         DFORM 1872,(33,34,35)                                                  
         DFORM 1880,(34,35)                                                     
         DFORM 362,(36)                                                         
         DFORM 365,(32,33,34,35,36)                                             
         DFORM 1873,(33,34,35,36)                                               
         DFORM 1881,(34,35,36)                                                  
         DFORM 1894,(35,36)                                                     
         DFORM 363,(37)                                                         
         DFORM 1865,(32,33,34,35,36,37)                                         
         DFORM 1874,(33,34,35,36,37)                                            
         DFORM 366,(34,35,36,37)                                                
         DFORM 1895,(35,36,37)                                                  
         DFORM 1904,(36,37)                                                     
         DFORM 364,(38)                                                         
         DFORM 65,(32,33,34,35,36,37,38)                                        
         DFORM 1876,(33,34,35,36,37,38)                                         
         DFORM 1884,(34,35,36,37,38)                                            
         DFORM 1897,(35,36,37,38)                                               
         DFORM 1906,(36,37,38)                                                  
         DFORM 367,(37,38)                                                      
         DFORM 120,(31)                                                         
         DFORM 1,(39)                                                           
         FFEND                                                                  
                                                                                
         EMOD                      END OF ALL TABLES FOR MODIFIER               
                                                                                
* UNIVERSES (U)                                                                 
         FMOD  WU                                                               
                                                                                
         FTYP3 (197,198,199,24,26,68,69,196,207,208,209,203,55,58,     *        
               60,217,218,219,74,76,112,113,227,228,229,195,213,       *        
               105,108,110,120,230,231,232,233,234,235,236,1)                   
                                                                                
         FTYP4 (325,326,327,24,26,68,69,324,335,336,337,331,55,58,     *        
               60,345,346,347,74,76,112,113,355,356,357,323,341,       *        
               105,108,110,120,358,359,360,361,362,363,364,1)                   
                                                                                
         DFORM 325,(1)                                                          
         DFORM 326,(2)                                                          
         DFORM 770,(1,2)                                                        
         DFORM 327,(3)                                                          
         DFORM 119,(1,2,3)                                                      
         DFORM 21,(2,3)                                                         
         DFORM 24,(4)                                                           
         DFORM 772,(1,2,3,4)                                                    
         DFORM 786,(2,3,4)                                                      
         DFORM 61,(3,4)                                                         
         DFORM 26,(5)                                                           
         DFORM 773,(1,2,3,4,5)                                                  
         DFORM 787,(2,3,4,5)                                                    
         DFORM 800,(3,4,5)                                                      
         DFORM 25,(4,5)                                                         
         DFORM 68,(6)                                                           
         DFORM 774,(1,2,3,4,5,6)                                                
         DFORM 788,(2,3,4,5,6)                                                  
         DFORM 801,(3,4,5,6)                                                    
         DFORM 23,(4,5,6)                                                       
         DFORM 824,(5,6)                                                        
         DFORM 69,(7)                                                           
         DFORM 775,(1,2,3,4,5,6,7)                                              
         DFORM 789,(2,3,4,5,6,7)                                                
         DFORM 802,(3,4,5,6,7)                                                  
         DFORM 28,(4,5,6,7)                                                     
         DFORM 34,(5,6,7)                                                       
         DFORM 40,(6,7)                                                         
         DFORM 324,(8)                                                          
         DFORM 776,(1,2,3,4,5,6,7,8)                                            
         DFORM 790,(2,3,4,5,6,7,8)                                              
         DFORM 803,(3,4,5,6,7,8)                                                
         DFORM 815,(4,5,6,7,8)                                                  
         DFORM 826,(5,6,7,8)                                                    
         DFORM 836,(6,7,8)                                                      
         DFORM 845,(7,8)                                                        
         DFORM 335,(9)                                                          
         DFORM 777,(1,2,3,4,5,6,7,8,9)                                          
         DFORM 791,(2,3,4,5,6,7,8,9)                                            
         DFORM 804,(3,4,5,6,7,8,9)                                              
         DFORM 29,(4,5,6,7,8,9)                                                 
         DFORM 35,(5,6,7,8,9)                                                   
         DFORM 41,(6,7,8,9)                                                     
         DFORM 70,(7,8,9)                                                       
         DFORM 46,(8,9)                                                         
         DFORM 336,(10)                                                         
         DFORM 778,(1,2,3,4,5,6,7,8,9,10)                                       
         DFORM 792,(2,3,4,5,6,7,8,9,10)                                         
         DFORM 805,(3,4,5,6,7,8,9,10)                                           
         DFORM 817,(4,5,6,7,8,9,10)                                             
         DFORM 828,(5,6,7,8,9,10)                                               
         DFORM 838,(6,7,8,9,10)                                                 
         DFORM 847,(7,8,9,10)                                                   
         DFORM 855,(8,9,10)                                                     
         DFORM 862,(9,10)                                                       
         DFORM 337,(11)                                                         
         DFORM 779,(1,2,3,4,5,6,7,8,9,10,11)                                    
         DFORM 793,(2,3,4,5,6,7,8,9,10,11)                                      
         DFORM 806,(3,4,5,6,7,8,9,10,11)                                        
         DFORM 818,(4,5,6,7,8,9,10,11)                                          
         DFORM 829,(5,6,7,8,9,10,11)                                            
         DFORM 328,(6,7,8,9,10,11)                                              
         DFORM 848,(7,8,9,10,11)                                                
         DFORM 329,(8,9,10,11)                                                  
         DFORM 863,(9,10,11)                                                    
         DFORM 330,(10,11)                                                      
         DFORM 331,(12)                                                         
         DFORM 780,(1,2,3,4,5,6,7,8,9,10,11,12)                                 
         DFORM 794,(2,3,4,5,6,7,8,9,10,11,12)                                   
         DFORM 807,(3,4,5,6,7,8,9,10,11,12)                                     
         DFORM 30,(4,5,6,7,8,9,10,11,12)                                        
         DFORM 36,(5,6,7,8,9,10,11,12)                                          
         DFORM 42,(6,7,8,9,10,11,12)                                            
         DFORM 71,(7,8,9,10,11,12)                                              
         DFORM 47,(8,9,10,11,12)                                                
         DFORM 864,(9,10,11,12)                                                 
         DFORM 51,(10,11,12)                                                    
         DFORM 875,(11,12)                                                      
         DFORM 55,(13)                                                          
         DFORM 781,(1,2,3,4,5,6,7,8,9,10,11,12,13)                              
         DFORM 795,(2,3,4,5,6,7,8,9,10,11,12,13)                                
         DFORM 808,(3,4,5,6,7,8,9,10,11,12,13)                                  
         DFORM 31,(4,5,6,7,8,9,10,11,12,13)                                     
         DFORM 37,(5,6,7,8,9,10,11,12,13)                                       
         DFORM 43,(6,7,8,9,10,11,12,13)                                         
         DFORM 72,(7,8,9,10,11,12,13)                                           
         DFORM 48,(8,9,10,11,12,13)                                             
         DFORM 865,(9,10,11,12,13)                                              
         DFORM 52,(10,11,12,13)                                                 
         DFORM 876,(11,12,13)                                                   
         DFORM 332,(12,13)                                                      
         DFORM 58,(14)                                                          
         DFORM 782,(1,2,3,4,5,6,7,8,9,10,11,12,13,14)                           
         DFORM 796,(2,3,4,5,6,7,8,9,10,11,12,13,14)                             
         DFORM 809,(3,4,5,6,7,8,9,10,11,12,13,14)                               
         DFORM 32,(4,5,6,7,8,9,10,11,12,13,14)                                  
         DFORM 38,(5,6,7,8,9,10,11,12,13,14)                                    
         DFORM 44,(6,7,8,9,10,11,12,13,14)                                      
         DFORM 73,(7,8,9,10,11,12,13,14)                                        
         DFORM 49,(8,9,10,11,12,13,14)                                          
         DFORM 866,(9,10,11,12,13,14)                                           
         DFORM 53,(10,11,12,13,14)                                              
         DFORM 877,(11,12,13,14)                                                
         DFORM 333,(12,13,14)                                                   
         DFORM 56,(13,14)                                                       
         DFORM 60,(15)                                                          
         DFORM 783,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)                        
         DFORM 797,(2,3,4,5,6,7,8,9,10,11,12,13,14,15)                          
         DFORM 810,(3,4,5,6,7,8,9,10,11,12,13,14,15)                            
         DFORM 33,(4,5,6,7,8,9,10,11,12,13,14,15)                               
         DFORM 39,(5,6,7,8,9,10,11,12,13,14,15)                                 
         DFORM 45,(6,7,8,9,10,11,12,13,14,15)                                   
         DFORM 67,(7,8,9,10,11,12,13,14,15)                                     
         DFORM 50,(8,9,10,11,12,13,14,15)                                       
         DFORM 867,(9,10,11,12,13,14,15)                                        
         DFORM 54,(10,11,12,13,14,15)                                           
         DFORM 878,(11,12,13,14,15)                                             
         DFORM 334,(12,13,14,15)                                                
         DFORM 57,(13,14,15)                                                    
         DFORM 59,(14,15)                                                       
         DFORM 345,(16)                                                         
         DFORM 346,(17)                                                         
         DFORM 1026,(16,17)                                                     
         DFORM 347,(18)                                                         
         DFORM 20,(16,17,18)                                                    
         DFORM 22,(17,18)                                                       
         DFORM 74,(19)                                                          
         DFORM 1028,(16,17,18,19)                                               
         DFORM 1042,(17,18,19)                                                  
         DFORM 62,(18,19)                                                       
         DFORM 76,(20)                                                          
         DFORM 1029,(16,17,18,19,20)                                            
         DFORM 314,(17,18,19,20)                                                
         DFORM 1056,(18,19,20)                                                  
         DFORM 75,(19,20)                                                       
         DFORM 112,(21)                                                         
         DFORM 1030,(16,17,18,19,20,21)                                         
         DFORM 1044,(17,18,19,20,21)                                            
         DFORM 1057,(18,19,20,21)                                               
         DFORM 111,(19,20,21)                                                   
         DFORM 1080,(20,21)                                                     
         DFORM 113,(22)                                                         
         DFORM 1031,(16,17,18,19,20,21,22)                                      
         DFORM 1045,(17,18,19,20,21,22)                                         
         DFORM 1058,(18,19,20,21,22)                                            
         DFORM 78,(19,20,21,22)                                                 
         DFORM 84,(20,21,22)                                                    
         DFORM 90,(21,22)                                                       
         DFORM 355,(23)                                                         
         DFORM 1032,(16,17,18,19,20,21,22,23)                                   
         DFORM 1046,(17,18,19,20,21,22,23)                                      
         DFORM 1059,(18,19,20,21,22,23)                                         
         DFORM 1071,(19,20,21,22,23)                                            
         DFORM 1082,(20,21,22,23)                                               
         DFORM 1092,(21,22,23)                                                  
         DFORM 1101,(22,23)                                                     
         DFORM 356,(24)                                                         
         DFORM 1033,(16,17,18,19,20,21,22,23,24)                                
         DFORM 1047,(17,18,19,20,21,22,23,24)                                   
         DFORM 1060,(18,19,20,21,22,23,24)                                      
         DFORM 79,(19,20,21,22,23,24)                                           
         DFORM 85,(20,21,22,23,24)                                              
         DFORM 91,(21,22,23,24)                                                 
         DFORM 114,(22,23,24)                                                   
         DFORM 96,(23,24)                                                       
         DFORM 357,(25)                                                         
         DFORM 1034,(16,17,18,19,20,21,22,23,24,25)                             
         DFORM 1048,(17,18,19,20,21,22,23,24,25)                                
         DFORM 1061,(18,19,20,21,22,23,24,25)                                   
         DFORM 1073,(19,20,21,22,23,24,25)                                      
         DFORM 1084,(20,21,22,23,24,25)                                         
         DFORM 1094,(21,22,23,24,25)                                            
         DFORM 1103,(22,23,24,25)                                               
         DFORM 1111,(23,24,25)                                                  
         DFORM 1118,(24,25)                                                     
         DFORM 323,(26)                                                         
         DFORM 1035,(16,17,18,19,20,21,22,23,24,25,26)                          
         DFORM 1049,(17,18,19,20,21,22,23,24,25,26)                             
         DFORM 1062,(18,19,20,21,22,23,24,25,26)                                
         DFORM 1074,(19,20,21,22,23,24,25,26)                                   
         DFORM 1085,(20,21,22,23,24,25,26)                                      
         DFORM 338,(21,22,23,24,25,26)                                          
         DFORM 1104,(22,23,24,25,26)                                            
         DFORM 339,(23,24,25,26)                                                
         DFORM 1119,(24,25,26)                                                  
         DFORM 340,(25,26)                                                      
         DFORM 341,(27)                                                         
         DFORM 1036,(16,17,18,19,20,21,22,23,24,25,26,27)                       
         DFORM 1050,(17,18,19,20,21,22,23,24,25,26,27)                          
         DFORM 1063,(18,19,20,21,22,23,24,25,26,27)                             
         DFORM 80,(19,20,21,22,23,24,25,26,27)                                  
         DFORM 86,(20,21,22,23,24,25,26,27)                                     
         DFORM 92,(21,22,23,24,25,26,27)                                        
         DFORM 115,(22,23,24,25,26,27)                                          
         DFORM 97,(23,24,25,26,27)                                              
         DFORM 1120,(24,25,26,27)                                               
         DFORM 101,(25,26,27)                                                   
         DFORM 1131,(26,27)                                                     
         DFORM 105,(28)                                                         
         DFORM 1037,(16,17,18,19,20,21,22,23,24,25,26,27,28)                    
         DFORM 1051,(17,18,19,20,21,22,23,24,25,26,27,28)                       
         DFORM 1064,(18,19,20,21,22,23,24,25,26,27,28)                          
         DFORM 81,(19,20,21,22,23,24,25,26,27,28)                               
         DFORM 87,(20,21,22,23,24,25,26,27,28)                                  
         DFORM 93,(21,22,23,24,25,26,27,28)                                     
         DFORM 116,(22,23,24,25,26,27,28)                                       
         DFORM 98,(23,24,25,26,27,28)                                           
         DFORM 1121,(24,25,26,27,28)                                            
         DFORM 102,(25,26,27,28)                                                
         DFORM 1132,(26,27,28)                                                  
         DFORM 342,(27,28)                                                      
         DFORM 108,(29)                                                         
         DFORM 1038,(16,17,18,19,20,21,22,23,24,25,26,27,28,29)                 
         DFORM 1052,(17,18,19,20,21,22,23,24,25,26,27,28,29)                    
         DFORM 1065,(18,19,20,21,22,23,24,25,26,27,28,29)                       
         DFORM 82,(19,20,21,22,23,24,25,26,27,28,29)                            
         DFORM 88,(20,21,22,23,24,25,26,27,28,29)                               
         DFORM 94,(21,22,23,24,25,26,27,28,29)                                  
         DFORM 117,(22,23,24,25,26,27,28,29)                                    
         DFORM 99,(23,24,25,26,27,28,29)                                        
         DFORM 1122,(24,25,26,27,28,29)                                         
         DFORM 103,(25,26,27,28,29)                                             
         DFORM 1133,(26,27,28,29)                                               
         DFORM 343,(27,28,29)                                                   
         DFORM 106,(28,29)                                                      
         DFORM 110,(30)                                                         
         DFORM 1039,(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)              
         DFORM 1053,(17,18,19,20,21,22,23,24,25,26,27,28,29,30)                 
         DFORM 1066,(18,19,20,21,22,23,24,25,26,27,28,29,30)                    
         DFORM 83,(19,20,21,22,23,24,25,26,27,28,29,30)                         
         DFORM 89,(20,21,22,23,24,25,26,27,28,29,30)                            
         DFORM 95,(21,22,23,24,25,26,27,28,29,30)                               
         DFORM 118,(22,23,24,25,26,27,28,29,30)                                 
         DFORM 100,(23,24,25,26,27,28,29,30)                                    
         DFORM 1123,(24,25,26,27,28,29,30)                                      
         DFORM 104,(25,26,27,28,29,30)                                          
         DFORM 1134,(26,27,28,29,30)                                            
         DFORM 344,(27,28,29,30)                                                
         DFORM 107,(28,29,30)                                                   
         DFORM 109,(29,30)                                                      
         DFORM 121,(1,16)                                                       
         DFORM 289,(2,17)                                                       
         DFORM 1282,(1,2,16,17)                                                 
         DFORM 290,(3,18)                                                       
         DFORM 122,(1,2,3,16,17,18)                                             
         DFORM 123,(2,3,17,18)                                                  
         DFORM 124,(4,19)                                                       
         DFORM 299,(1,2,3,4,16,17,18,19)                                        
         DFORM 306,(2,3,4,17,18,19)                                             
         DFORM 380,(3,4,18,19)                                                  
         DFORM 126,(5,20)                                                       
         DFORM 300,(1,2,3,4,5,16,17,18,19,20)                                   
         DFORM 307,(2,3,4,5,17,18,19,20)                                        
         DFORM 1312,(3,4,5,18,19,20)                                            
         DFORM 125,(4,5,19,20)                                                  
         DFORM 316,(6,21)                                                       
         DFORM 1286,(1,2,3,4,5,6,16,17,18,19,20,21)                             
         DFORM 1300,(2,3,4,5,6,17,18,19,20,21)                                  
         DFORM 1313,(3,4,5,6,18,19,20,21)                                       
         DFORM 315,(4,5,6,19,20,21)                                             
         DFORM 1336,(5,6,20,21)                                                 
         DFORM 317,(7,22)                                                       
         DFORM 301,(1,2,3,4,5,6,7,16,17,18,19,20,21,22)                         
         DFORM 308,(2,3,4,5,6,7,17,18,19,20,21,22)                              
         DFORM 1314,(3,4,5,6,7,18,19,20,21,22)                                  
         DFORM 256,(4,5,6,7,19,20,21,22)                                        
         DFORM 262,(5,6,7,20,21,22)                                             
         DFORM 268,(6,7,21,22)                                                  
         DFORM 291,(8,23)                                                       
         DFORM 1288,(1,2,3,4,5,6,7,8,16,17,18,19,20,21,22,23)                   
         DFORM 1302,(2,3,4,5,6,7,8,17,18,19,20,21,22,23)                        
         DFORM 1315,(3,4,5,6,7,8,18,19,20,21,22,23)                             
         DFORM 1327,(4,5,6,7,8,19,20,21,22,23)                                  
         DFORM 1338,(5,6,7,8,20,21,22,23)                                       
         DFORM 1348,(6,7,8,21,22,23)                                            
         DFORM 1357,(7,8,22,23)                                                 
         DFORM 292,(9,24)                                                       
         DFORM 302,(1,2,3,4,5,6,7,8,9,16,17,18,19,20,21,22,23,24)               
         DFORM 309,(2,3,4,5,6,7,8,9,17,18,19,20,21,22,23,24)                    
         DFORM 1316,(3,4,5,6,7,8,9,18,19,20,21,22,23,24)                        
         DFORM 257,(4,5,6,7,8,9,19,20,21,22,23,24)                              
         DFORM 263,(5,6,7,8,9,20,21,22,23,24)                                   
         DFORM 269,(6,7,8,9,21,22,23,24)                                        
         DFORM 318,(7,8,9,22,23,24)                                             
         DFORM 274,(8,9,23,24)                                                  
         DFORM 293,(10,25)                                                      
         DFORM 1290,(1,2,3,4,5,6,7,8,9,10,16,17,18,19,20,21,22,23,     *        
               24,25)                                                           
         DFORM 1304,(2,3,4,5,6,7,8,9,10,17,18,19,20,21,22,23,24,25)             
         DFORM 1317,(3,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25)                  
         DFORM 1329,(4,5,6,7,8,9,10,19,20,21,22,23,24,25)                       
         DFORM 1340,(5,6,7,8,9,10,20,21,22,23,24,25)                            
         DFORM 1350,(6,7,8,9,10,21,22,23,24,25)                                 
         DFORM 1359,(7,8,9,10,22,23,24,25)                                      
         DFORM 1367,(8,9,10,23,24,25)                                           
         DFORM 1374,(9,10,24,25)                                                
         DFORM 294,(11,26)                                                      
         DFORM 1291,(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,     *        
               23,24,25,26)                                                     
         DFORM 1305,(2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,       *        
               24,25,26)                                                        
         DFORM 1318,(3,4,5,6,7,8,9,10,11,18,19,20,21,22,23,24,25,      *        
               26)                                                              
         DFORM 1330,(4,5,6,7,8,9,10,11,19,20,21,22,23,24,25,26)                 
         DFORM 1341,(5,6,7,8,9,10,11,20,21,22,23,24,25,26)                      
         DFORM 348,(6,7,8,9,10,11,21,22,23,24,25,26)                            
         DFORM 1360,(7,8,9,10,11,22,23,24,25,26)                                
         DFORM 349,(8,9,10,11,23,24,25,26)                                      
         DFORM 1375,(9,10,11,24,25,26)                                          
         DFORM 350,(10,11,25,26)                                                
         DFORM 351,(12,27)                                                      
         DFORM 303,(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21,      *        
               22,23,24,25,26,27)                                               
         DFORM 310,(2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,        *        
               23,24,25,26,27)                                                  
         DFORM 1319,(3,4,5,6,7,8,9,10,11,12,18,19,20,21,22,23,24,      *        
               25,26,27)                                                        
         DFORM 258,(4,5,6,7,8,9,10,11,12,19,20,21,22,23,24,25,26,      *        
               27)                                                              
         DFORM 264,(5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27)                 
         DFORM 270,(6,7,8,9,10,11,12,21,22,23,24,25,26,27)                      
         DFORM 319,(7,8,9,10,11,12,22,23,24,25,26,27)                           
         DFORM 275,(8,9,10,11,12,23,24,25,26,27)                                
         DFORM 1376,(9,10,11,12,24,25,26,27)                                    
         DFORM 279,(10,11,12,25,26,27)                                          
         DFORM 1387,(11,12,26,27)                                               
         DFORM 283,(13,28)                                                      
         DFORM 304,(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,      *        
               21,22,23,24,25,26,27,28)                                         
         DFORM 311,(2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,20,21,        *        
               22,23,24,25,26,27,28)                                            
         DFORM 1320,(3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,      *        
               24,25,26,27,28)                                                  
         DFORM 259,(4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,      *        
               26,27,28)                                                        
         DFORM 265,(5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,        *        
               27,28)                                                           
         DFORM 271,(6,7,8,9,10,11,12,13,21,22,23,24,25,26,27,28)                
         DFORM 320,(7,8,9,10,11,12,13,22,23,24,25,26,27,28)                     
         DFORM 276,(8,9,10,11,12,13,23,24,25,26,27,28)                          
         DFORM 1377,(9,10,11,12,13,24,25,26,27,28)                              
         DFORM 280,(10,11,12,13,25,26,27,28)                                    
         DFORM 1388,(11,12,13,26,27,28)                                         
         DFORM 352,(12,13,27,28)                                                
         DFORM 286,(14,29)                                                      
         DFORM 305,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,      *        
               20,21,22,23,24,25,26,27,28,29)                                   
         DFORM 312,(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20,        *        
               21,22,23,24,25,26,27,28,29)                                      
         DFORM 1321,(3,4,5,6,7,8,9,10,11,12,13,14,18,19,20,21,22,      *        
               23,24,25,26,27,28,29)                                            
         DFORM 260,(4,5,6,7,8,9,10,11,12,13,14,19,20,21,22,23,24,      *        
               25,26,27,28,29)                                                  
         DFORM 266,(5,6,7,8,9,10,11,12,13,14,20,21,22,23,24,25,        *        
               26,27,28,29)                                                     
         DFORM 272,(6,7,8,9,10,11,12,13,14,21,22,23,24,25,26,27,       *        
               28,29)                                                           
         DFORM 321,(7,8,9,10,11,12,13,14,22,23,24,25,26,27,28,29)               
         DFORM 277,(8,9,10,11,12,13,14,23,24,25,26,27,28,29)                    
         DFORM 1378,(9,10,11,12,13,14,24,25,26,27,28,29)                        
         DFORM 281,(10,11,12,13,14,25,26,27,28,29)                              
         DFORM 1389,(11,12,13,14,26,27,28,29)                                   
         DFORM 353,(12,13,14,27,28,29)                                          
         DFORM 284,(13,14,28,29)                                                
         DFORM 288,(15,30)                                                      
         DFORM 127,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,      *        
               19,20,21,22,23,24,25,26,27,28,29,30)                             
         DFORM 313,(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,        *        
               20,21,22,23,24,25,26,27,28,29,30)                                
         DFORM 1322,(3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,      *        
               22,23,24,25,26,27,28,29,30)                                      
         DFORM 261,(4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,22,23,      *        
               24,25,26,27,28,29,30)                                            
         DFORM 267,(5,6,7,8,9,10,11,12,13,14,15,20,21,22,23,24,        *        
               25,26,27,28,29,30)                                               
         DFORM 273,(6,7,8,9,10,11,12,13,14,15,21,22,23,24,25,26,       *        
               27,28,29,30)                                                     
         DFORM 322,(7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,      *        
               29,30)                                                           
         DFORM 278,(8,9,10,11,12,13,14,15,23,24,25,26,27,28,29,30)              
         DFORM 1379,(9,10,11,12,13,14,15,24,25,26,27,28,29,30)                  
         DFORM 282,(10,11,12,13,14,15,25,26,27,28,29,30)                        
         DFORM 1390,(11,12,13,14,15,26,27,28,29,30)                             
         DFORM 354,(12,13,14,15,27,28,29,30)                                    
         DFORM 285,(13,14,15,28,29,30)                                          
         DFORM 287,(14,15,29,30)                                                
         DFORM 358,(32)                                                         
         DFORM 359,(33)                                                         
         DFORM 1859,(32,33)                                                     
         DFORM 360,(34)                                                         
         DFORM 1861,(32,33,34)                                                  
         DFORM 1870,(33,34)                                                     
         DFORM 361,(35)                                                         
         DFORM 1863,(32,33,34,35)                                               
         DFORM 1872,(33,34,35)                                                  
         DFORM 1880,(34,35)                                                     
         DFORM 362,(36)                                                         
         DFORM 365,(32,33,34,35,36)                                             
         DFORM 1873,(33,34,35,36)                                               
         DFORM 1881,(34,35,36)                                                  
         DFORM 1894,(35,36)                                                     
         DFORM 363,(37)                                                         
         DFORM 1865,(32,33,34,35,36,37)                                         
         DFORM 1874,(33,34,35,36,37)                                            
         DFORM 366,(34,35,36,37)                                                
         DFORM 1895,(35,36,37)                                                  
         DFORM 1904,(36,37)                                                     
         DFORM 364,(38)                                                         
         DFORM 65,(32,33,34,35,36,37,38)                                        
         DFORM 1876,(33,34,35,36,37,38)                                         
         DFORM 1884,(34,35,36,37,38)                                            
         DFORM 1897,(35,36,37,38)                                               
         DFORM 1906,(36,37,38)                                                  
         DFORM 367,(37,38)                                                      
         DFORM 120,(31)                                                         
         DFORM 1,(39)                                                           
         FFEND                                                                  
                                                                                
         EMOD                      END OF ALL TABLES FOR MODIFIER               
                                                                                
* PROGRAM PUTS (Z)                                                              
         FMOD  WZ                                                               
                                                                                
         FTYP3 (197,198,199,24,26,68,69,196,207,208,209,203,55,58,     *        
               60,217,218,219,74,76,112,113,227,228,229,195,213,       *        
               105,108,110,120,230,231,232,233,234,235,236,1)                   
                                                                                
         FTYP4 (325,326,327,24,26,68,69,324,335,336,337,331,55,58,     *        
               60,345,346,347,74,76,112,113,355,356,357,323,341,       *        
               105,108,110,120,358,359,360,361,362,363,364,1)                   
                                                                                
         DFORM 325,(1)                                                          
         DFORM 326,(2)                                                          
         DFORM 770,(1,2)                                                        
         DFORM 327,(3)                                                          
         DFORM 119,(1,2,3)                                                      
         DFORM 21,(2,3)                                                         
         DFORM 24,(4)                                                           
         DFORM 772,(1,2,3,4)                                                    
         DFORM 786,(2,3,4)                                                      
         DFORM 61,(3,4)                                                         
         DFORM 26,(5)                                                           
         DFORM 773,(1,2,3,4,5)                                                  
         DFORM 787,(2,3,4,5)                                                    
         DFORM 800,(3,4,5)                                                      
         DFORM 25,(4,5)                                                         
         DFORM 68,(6)                                                           
         DFORM 774,(1,2,3,4,5,6)                                                
         DFORM 788,(2,3,4,5,6)                                                  
         DFORM 801,(3,4,5,6)                                                    
         DFORM 23,(4,5,6)                                                       
         DFORM 824,(5,6)                                                        
         DFORM 69,(7)                                                           
         DFORM 775,(1,2,3,4,5,6,7)                                              
         DFORM 789,(2,3,4,5,6,7)                                                
         DFORM 802,(3,4,5,6,7)                                                  
         DFORM 28,(4,5,6,7)                                                     
         DFORM 34,(5,6,7)                                                       
         DFORM 40,(6,7)                                                         
         DFORM 324,(8)                                                          
         DFORM 776,(1,2,3,4,5,6,7,8)                                            
         DFORM 790,(2,3,4,5,6,7,8)                                              
         DFORM 803,(3,4,5,6,7,8)                                                
         DFORM 815,(4,5,6,7,8)                                                  
         DFORM 826,(5,6,7,8)                                                    
         DFORM 836,(6,7,8)                                                      
         DFORM 845,(7,8)                                                        
         DFORM 335,(9)                                                          
         DFORM 777,(1,2,3,4,5,6,7,8,9)                                          
         DFORM 791,(2,3,4,5,6,7,8,9)                                            
         DFORM 804,(3,4,5,6,7,8,9)                                              
         DFORM 29,(4,5,6,7,8,9)                                                 
         DFORM 35,(5,6,7,8,9)                                                   
         DFORM 41,(6,7,8,9)                                                     
         DFORM 70,(7,8,9)                                                       
         DFORM 46,(8,9)                                                         
         DFORM 336,(10)                                                         
         DFORM 778,(1,2,3,4,5,6,7,8,9,10)                                       
         DFORM 792,(2,3,4,5,6,7,8,9,10)                                         
         DFORM 805,(3,4,5,6,7,8,9,10)                                           
         DFORM 817,(4,5,6,7,8,9,10)                                             
         DFORM 828,(5,6,7,8,9,10)                                               
         DFORM 838,(6,7,8,9,10)                                                 
         DFORM 847,(7,8,9,10)                                                   
         DFORM 855,(8,9,10)                                                     
         DFORM 862,(9,10)                                                       
         DFORM 337,(11)                                                         
         DFORM 779,(1,2,3,4,5,6,7,8,9,10,11)                                    
         DFORM 793,(2,3,4,5,6,7,8,9,10,11)                                      
         DFORM 806,(3,4,5,6,7,8,9,10,11)                                        
         DFORM 818,(4,5,6,7,8,9,10,11)                                          
         DFORM 829,(5,6,7,8,9,10,11)                                            
         DFORM 328,(6,7,8,9,10,11)                                              
         DFORM 848,(7,8,9,10,11)                                                
         DFORM 329,(8,9,10,11)                                                  
         DFORM 863,(9,10,11)                                                    
         DFORM 330,(10,11)                                                      
         DFORM 331,(12)                                                         
         DFORM 780,(1,2,3,4,5,6,7,8,9,10,11,12)                                 
         DFORM 794,(2,3,4,5,6,7,8,9,10,11,12)                                   
         DFORM 807,(3,4,5,6,7,8,9,10,11,12)                                     
         DFORM 30,(4,5,6,7,8,9,10,11,12)                                        
         DFORM 36,(5,6,7,8,9,10,11,12)                                          
         DFORM 42,(6,7,8,9,10,11,12)                                            
         DFORM 71,(7,8,9,10,11,12)                                              
         DFORM 47,(8,9,10,11,12)                                                
         DFORM 864,(9,10,11,12)                                                 
         DFORM 51,(10,11,12)                                                    
         DFORM 875,(11,12)                                                      
         DFORM 55,(13)                                                          
         DFORM 781,(1,2,3,4,5,6,7,8,9,10,11,12,13)                              
         DFORM 795,(2,3,4,5,6,7,8,9,10,11,12,13)                                
         DFORM 808,(3,4,5,6,7,8,9,10,11,12,13)                                  
         DFORM 31,(4,5,6,7,8,9,10,11,12,13)                                     
         DFORM 37,(5,6,7,8,9,10,11,12,13)                                       
         DFORM 43,(6,7,8,9,10,11,12,13)                                         
         DFORM 72,(7,8,9,10,11,12,13)                                           
         DFORM 48,(8,9,10,11,12,13)                                             
         DFORM 865,(9,10,11,12,13)                                              
         DFORM 52,(10,11,12,13)                                                 
         DFORM 876,(11,12,13)                                                   
         DFORM 332,(12,13)                                                      
         DFORM 58,(14)                                                          
         DFORM 782,(1,2,3,4,5,6,7,8,9,10,11,12,13,14)                           
         DFORM 796,(2,3,4,5,6,7,8,9,10,11,12,13,14)                             
         DFORM 809,(3,4,5,6,7,8,9,10,11,12,13,14)                               
         DFORM 32,(4,5,6,7,8,9,10,11,12,13,14)                                  
         DFORM 38,(5,6,7,8,9,10,11,12,13,14)                                    
         DFORM 44,(6,7,8,9,10,11,12,13,14)                                      
         DFORM 73,(7,8,9,10,11,12,13,14)                                        
         DFORM 49,(8,9,10,11,12,13,14)                                          
         DFORM 866,(9,10,11,12,13,14)                                           
         DFORM 53,(10,11,12,13,14)                                              
         DFORM 877,(11,12,13,14)                                                
         DFORM 333,(12,13,14)                                                   
         DFORM 56,(13,14)                                                       
         DFORM 60,(15)                                                          
         DFORM 783,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)                        
         DFORM 797,(2,3,4,5,6,7,8,9,10,11,12,13,14,15)                          
         DFORM 810,(3,4,5,6,7,8,9,10,11,12,13,14,15)                            
         DFORM 33,(4,5,6,7,8,9,10,11,12,13,14,15)                               
         DFORM 39,(5,6,7,8,9,10,11,12,13,14,15)                                 
         DFORM 45,(6,7,8,9,10,11,12,13,14,15)                                   
         DFORM 67,(7,8,9,10,11,12,13,14,15)                                     
         DFORM 50,(8,9,10,11,12,13,14,15)                                       
         DFORM 867,(9,10,11,12,13,14,15)                                        
         DFORM 54,(10,11,12,13,14,15)                                           
         DFORM 878,(11,12,13,14,15)                                             
         DFORM 334,(12,13,14,15)                                                
         DFORM 57,(13,14,15)                                                    
         DFORM 59,(14,15)                                                       
         DFORM 345,(16)                                                         
         DFORM 346,(17)                                                         
         DFORM 1026,(16,17)                                                     
         DFORM 347,(18)                                                         
         DFORM 20,(16,17,18)                                                    
         DFORM 22,(17,18)                                                       
         DFORM 74,(19)                                                          
         DFORM 1028,(16,17,18,19)                                               
         DFORM 1042,(17,18,19)                                                  
         DFORM 62,(18,19)                                                       
         DFORM 76,(20)                                                          
         DFORM 1029,(16,17,18,19,20)                                            
         DFORM 314,(17,18,19,20)                                                
         DFORM 1056,(18,19,20)                                                  
         DFORM 75,(19,20)                                                       
         DFORM 112,(21)                                                         
         DFORM 1030,(16,17,18,19,20,21)                                         
         DFORM 1044,(17,18,19,20,21)                                            
         DFORM 1057,(18,19,20,21)                                               
         DFORM 111,(19,20,21)                                                   
         DFORM 1080,(20,21)                                                     
         DFORM 113,(22)                                                         
         DFORM 1031,(16,17,18,19,20,21,22)                                      
         DFORM 1045,(17,18,19,20,21,22)                                         
         DFORM 1058,(18,19,20,21,22)                                            
         DFORM 78,(19,20,21,22)                                                 
         DFORM 84,(20,21,22)                                                    
         DFORM 90,(21,22)                                                       
         DFORM 355,(23)                                                         
         DFORM 1032,(16,17,18,19,20,21,22,23)                                   
         DFORM 1046,(17,18,19,20,21,22,23)                                      
         DFORM 1059,(18,19,20,21,22,23)                                         
         DFORM 1071,(19,20,21,22,23)                                            
         DFORM 1082,(20,21,22,23)                                               
         DFORM 1092,(21,22,23)                                                  
         DFORM 1101,(22,23)                                                     
         DFORM 356,(24)                                                         
         DFORM 1033,(16,17,18,19,20,21,22,23,24)                                
         DFORM 1047,(17,18,19,20,21,22,23,24)                                   
         DFORM 1060,(18,19,20,21,22,23,24)                                      
         DFORM 79,(19,20,21,22,23,24)                                           
         DFORM 85,(20,21,22,23,24)                                              
         DFORM 91,(21,22,23,24)                                                 
         DFORM 114,(22,23,24)                                                   
         DFORM 96,(23,24)                                                       
         DFORM 357,(25)                                                         
         DFORM 1034,(16,17,18,19,20,21,22,23,24,25)                             
         DFORM 1048,(17,18,19,20,21,22,23,24,25)                                
         DFORM 1061,(18,19,20,21,22,23,24,25)                                   
         DFORM 1073,(19,20,21,22,23,24,25)                                      
         DFORM 1084,(20,21,22,23,24,25)                                         
         DFORM 1094,(21,22,23,24,25)                                            
         DFORM 1103,(22,23,24,25)                                               
         DFORM 1111,(23,24,25)                                                  
         DFORM 1118,(24,25)                                                     
         DFORM 323,(26)                                                         
         DFORM 1035,(16,17,18,19,20,21,22,23,24,25,26)                          
         DFORM 1049,(17,18,19,20,21,22,23,24,25,26)                             
         DFORM 1062,(18,19,20,21,22,23,24,25,26)                                
         DFORM 1074,(19,20,21,22,23,24,25,26)                                   
         DFORM 1085,(20,21,22,23,24,25,26)                                      
         DFORM 338,(21,22,23,24,25,26)                                          
         DFORM 1104,(22,23,24,25,26)                                            
         DFORM 339,(23,24,25,26)                                                
         DFORM 1119,(24,25,26)                                                  
         DFORM 340,(25,26)                                                      
         DFORM 341,(27)                                                         
         DFORM 1036,(16,17,18,19,20,21,22,23,24,25,26,27)                       
         DFORM 1050,(17,18,19,20,21,22,23,24,25,26,27)                          
         DFORM 1063,(18,19,20,21,22,23,24,25,26,27)                             
         DFORM 80,(19,20,21,22,23,24,25,26,27)                                  
         DFORM 86,(20,21,22,23,24,25,26,27)                                     
         DFORM 92,(21,22,23,24,25,26,27)                                        
         DFORM 115,(22,23,24,25,26,27)                                          
         DFORM 97,(23,24,25,26,27)                                              
         DFORM 1120,(24,25,26,27)                                               
         DFORM 101,(25,26,27)                                                   
         DFORM 1131,(26,27)                                                     
         DFORM 105,(28)                                                         
         DFORM 1037,(16,17,18,19,20,21,22,23,24,25,26,27,28)                    
         DFORM 1051,(17,18,19,20,21,22,23,24,25,26,27,28)                       
         DFORM 1064,(18,19,20,21,22,23,24,25,26,27,28)                          
         DFORM 81,(19,20,21,22,23,24,25,26,27,28)                               
         DFORM 87,(20,21,22,23,24,25,26,27,28)                                  
         DFORM 93,(21,22,23,24,25,26,27,28)                                     
         DFORM 116,(22,23,24,25,26,27,28)                                       
         DFORM 98,(23,24,25,26,27,28)                                           
         DFORM 1121,(24,25,26,27,28)                                            
         DFORM 102,(25,26,27,28)                                                
         DFORM 1132,(26,27,28)                                                  
         DFORM 342,(27,28)                                                      
         DFORM 108,(29)                                                         
         DFORM 1038,(16,17,18,19,20,21,22,23,24,25,26,27,28,29)                 
         DFORM 1052,(17,18,19,20,21,22,23,24,25,26,27,28,29)                    
         DFORM 1065,(18,19,20,21,22,23,24,25,26,27,28,29)                       
         DFORM 82,(19,20,21,22,23,24,25,26,27,28,29)                            
         DFORM 88,(20,21,22,23,24,25,26,27,28,29)                               
         DFORM 94,(21,22,23,24,25,26,27,28,29)                                  
         DFORM 117,(22,23,24,25,26,27,28,29)                                    
         DFORM 99,(23,24,25,26,27,28,29)                                        
         DFORM 1122,(24,25,26,27,28,29)                                         
         DFORM 103,(25,26,27,28,29)                                             
         DFORM 1133,(26,27,28,29)                                               
         DFORM 343,(27,28,29)                                                   
         DFORM 106,(28,29)                                                      
         DFORM 110,(30)                                                         
         DFORM 1039,(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)              
         DFORM 1053,(17,18,19,20,21,22,23,24,25,26,27,28,29,30)                 
         DFORM 1066,(18,19,20,21,22,23,24,25,26,27,28,29,30)                    
         DFORM 83,(19,20,21,22,23,24,25,26,27,28,29,30)                         
         DFORM 89,(20,21,22,23,24,25,26,27,28,29,30)                            
         DFORM 95,(21,22,23,24,25,26,27,28,29,30)                               
         DFORM 118,(22,23,24,25,26,27,28,29,30)                                 
         DFORM 100,(23,24,25,26,27,28,29,30)                                    
         DFORM 1123,(24,25,26,27,28,29,30)                                      
         DFORM 104,(25,26,27,28,29,30)                                          
         DFORM 1134,(26,27,28,29,30)                                            
         DFORM 344,(27,28,29,30)                                                
         DFORM 107,(28,29,30)                                                   
         DFORM 109,(29,30)                                                      
         DFORM 121,(1,16)                                                       
         DFORM 289,(2,17)                                                       
         DFORM 1282,(1,2,16,17)                                                 
         DFORM 290,(3,18)                                                       
         DFORM 122,(1,2,3,16,17,18)                                             
         DFORM 123,(2,3,17,18)                                                  
         DFORM 124,(4,19)                                                       
         DFORM 299,(1,2,3,4,16,17,18,19)                                        
         DFORM 306,(2,3,4,17,18,19)                                             
         DFORM 380,(3,4,18,19)                                                  
         DFORM 126,(5,20)                                                       
         DFORM 300,(1,2,3,4,5,16,17,18,19,20)                                   
         DFORM 307,(2,3,4,5,17,18,19,20)                                        
         DFORM 1312,(3,4,5,18,19,20)                                            
         DFORM 125,(4,5,19,20)                                                  
         DFORM 316,(6,21)                                                       
         DFORM 1286,(1,2,3,4,5,6,16,17,18,19,20,21)                             
         DFORM 1300,(2,3,4,5,6,17,18,19,20,21)                                  
         DFORM 1313,(3,4,5,6,18,19,20,21)                                       
         DFORM 315,(4,5,6,19,20,21)                                             
         DFORM 1336,(5,6,20,21)                                                 
         DFORM 317,(7,22)                                                       
         DFORM 301,(1,2,3,4,5,6,7,16,17,18,19,20,21,22)                         
         DFORM 308,(2,3,4,5,6,7,17,18,19,20,21,22)                              
         DFORM 1314,(3,4,5,6,7,18,19,20,21,22)                                  
         DFORM 256,(4,5,6,7,19,20,21,22)                                        
         DFORM 262,(5,6,7,20,21,22)                                             
         DFORM 268,(6,7,21,22)                                                  
         DFORM 291,(8,23)                                                       
         DFORM 1288,(1,2,3,4,5,6,7,8,16,17,18,19,20,21,22,23)                   
         DFORM 1302,(2,3,4,5,6,7,8,17,18,19,20,21,22,23)                        
         DFORM 1315,(3,4,5,6,7,8,18,19,20,21,22,23)                             
         DFORM 1327,(4,5,6,7,8,19,20,21,22,23)                                  
         DFORM 1338,(5,6,7,8,20,21,22,23)                                       
         DFORM 1348,(6,7,8,21,22,23)                                            
         DFORM 1357,(7,8,22,23)                                                 
         DFORM 292,(9,24)                                                       
         DFORM 302,(1,2,3,4,5,6,7,8,9,16,17,18,19,20,21,22,23,24)               
         DFORM 309,(2,3,4,5,6,7,8,9,17,18,19,20,21,22,23,24)                    
         DFORM 1316,(3,4,5,6,7,8,9,18,19,20,21,22,23,24)                        
         DFORM 257,(4,5,6,7,8,9,19,20,21,22,23,24)                              
         DFORM 263,(5,6,7,8,9,20,21,22,23,24)                                   
         DFORM 269,(6,7,8,9,21,22,23,24)                                        
         DFORM 318,(7,8,9,22,23,24)                                             
         DFORM 274,(8,9,23,24)                                                  
         DFORM 293,(10,25)                                                      
         DFORM 1290,(1,2,3,4,5,6,7,8,9,10,16,17,18,19,20,21,22,23,     *        
               24,25)                                                           
         DFORM 1304,(2,3,4,5,6,7,8,9,10,17,18,19,20,21,22,23,24,25)             
         DFORM 1317,(3,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25)                  
         DFORM 1329,(4,5,6,7,8,9,10,19,20,21,22,23,24,25)                       
         DFORM 1340,(5,6,7,8,9,10,20,21,22,23,24,25)                            
         DFORM 1350,(6,7,8,9,10,21,22,23,24,25)                                 
         DFORM 1359,(7,8,9,10,22,23,24,25)                                      
         DFORM 1367,(8,9,10,23,24,25)                                           
         DFORM 1374,(9,10,24,25)                                                
         DFORM 294,(11,26)                                                      
         DFORM 1291,(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,     *        
               23,24,25,26)                                                     
         DFORM 1305,(2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,       *        
               24,25,26)                                                        
         DFORM 1318,(3,4,5,6,7,8,9,10,11,18,19,20,21,22,23,24,25,      *        
               26)                                                              
         DFORM 1330,(4,5,6,7,8,9,10,11,19,20,21,22,23,24,25,26)                 
         DFORM 1341,(5,6,7,8,9,10,11,20,21,22,23,24,25,26)                      
         DFORM 348,(6,7,8,9,10,11,21,22,23,24,25,26)                            
         DFORM 1360,(7,8,9,10,11,22,23,24,25,26)                                
         DFORM 349,(8,9,10,11,23,24,25,26)                                      
         DFORM 1375,(9,10,11,24,25,26)                                          
         DFORM 350,(10,11,25,26)                                                
         DFORM 351,(12,27)                                                      
         DFORM 303,(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21,      *        
               22,23,24,25,26,27)                                               
         DFORM 310,(2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,        *        
               23,24,25,26,27)                                                  
         DFORM 1319,(3,4,5,6,7,8,9,10,11,12,18,19,20,21,22,23,24,      *        
               25,26,27)                                                        
         DFORM 258,(4,5,6,7,8,9,10,11,12,19,20,21,22,23,24,25,26,      *        
               27)                                                              
         DFORM 264,(5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27)                 
         DFORM 270,(6,7,8,9,10,11,12,21,22,23,24,25,26,27)                      
         DFORM 319,(7,8,9,10,11,12,22,23,24,25,26,27)                           
         DFORM 275,(8,9,10,11,12,23,24,25,26,27)                                
         DFORM 1376,(9,10,11,12,24,25,26,27)                                    
         DFORM 279,(10,11,12,25,26,27)                                          
         DFORM 1387,(11,12,26,27)                                               
         DFORM 283,(13,28)                                                      
         DFORM 304,(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,      *        
               21,22,23,24,25,26,27,28)                                         
         DFORM 311,(2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,20,21,        *        
               22,23,24,25,26,27,28)                                            
         DFORM 1320,(3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,      *        
               24,25,26,27,28)                                                  
         DFORM 259,(4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,      *        
               26,27,28)                                                        
         DFORM 265,(5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,        *        
               27,28)                                                           
         DFORM 271,(6,7,8,9,10,11,12,13,21,22,23,24,25,26,27,28)                
         DFORM 320,(7,8,9,10,11,12,13,22,23,24,25,26,27,28)                     
         DFORM 276,(8,9,10,11,12,13,23,24,25,26,27,28)                          
         DFORM 1377,(9,10,11,12,13,24,25,26,27,28)                              
         DFORM 280,(10,11,12,13,25,26,27,28)                                    
         DFORM 1388,(11,12,13,26,27,28)                                         
         DFORM 352,(12,13,27,28)                                                
         DFORM 286,(14,29)                                                      
         DFORM 305,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,      *        
               20,21,22,23,24,25,26,27,28,29)                                   
         DFORM 312,(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20,        *        
               21,22,23,24,25,26,27,28,29)                                      
         DFORM 1321,(3,4,5,6,7,8,9,10,11,12,13,14,18,19,20,21,22,      *        
               23,24,25,26,27,28,29)                                            
         DFORM 260,(4,5,6,7,8,9,10,11,12,13,14,19,20,21,22,23,24,      *        
               25,26,27,28,29)                                                  
         DFORM 266,(5,6,7,8,9,10,11,12,13,14,20,21,22,23,24,25,        *        
               26,27,28,29)                                                     
         DFORM 272,(6,7,8,9,10,11,12,13,14,21,22,23,24,25,26,27,       *        
               28,29)                                                           
         DFORM 321,(7,8,9,10,11,12,13,14,22,23,24,25,26,27,28,29)               
         DFORM 277,(8,9,10,11,12,13,14,23,24,25,26,27,28,29)                    
         DFORM 1378,(9,10,11,12,13,14,24,25,26,27,28,29)                        
         DFORM 281,(10,11,12,13,14,25,26,27,28,29)                              
         DFORM 1389,(11,12,13,14,26,27,28,29)                                   
         DFORM 353,(12,13,14,27,28,29)                                          
         DFORM 284,(13,14,28,29)                                                
         DFORM 288,(15,30)                                                      
         DFORM 127,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,      *        
               19,20,21,22,23,24,25,26,27,28,29,30)                             
         DFORM 313,(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,        *        
               20,21,22,23,24,25,26,27,28,29,30)                                
         DFORM 1322,(3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,      *        
               22,23,24,25,26,27,28,29,30)                                      
         DFORM 261,(4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,22,23,      *        
               24,25,26,27,28,29,30)                                            
         DFORM 267,(5,6,7,8,9,10,11,12,13,14,15,20,21,22,23,24,        *        
               25,26,27,28,29,30)                                               
         DFORM 273,(6,7,8,9,10,11,12,13,14,15,21,22,23,24,25,26,       *        
               27,28,29,30)                                                     
         DFORM 322,(7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,      *        
               29,30)                                                           
         DFORM 278,(8,9,10,11,12,13,14,15,23,24,25,26,27,28,29,30)              
         DFORM 1379,(9,10,11,12,13,14,15,24,25,26,27,28,29,30)                  
         DFORM 282,(10,11,12,13,14,15,25,26,27,28,29,30)                        
         DFORM 1390,(11,12,13,14,15,26,27,28,29,30)                             
         DFORM 354,(12,13,14,15,27,28,29,30)                                    
         DFORM 285,(13,14,15,28,29,30)                                          
         DFORM 287,(14,15,29,30)                                                
         DFORM 358,(32)                                                         
         DFORM 359,(33)                                                         
         DFORM 1859,(32,33)                                                     
         DFORM 360,(34)                                                         
         DFORM 1861,(32,33,34)                                                  
         DFORM 1870,(33,34)                                                     
         DFORM 361,(35)                                                         
         DFORM 1863,(32,33,34,35)                                               
         DFORM 1872,(33,34,35)                                                  
         DFORM 1880,(34,35)                                                     
         DFORM 362,(36)                                                         
         DFORM 365,(32,33,34,35,36)                                             
         DFORM 1873,(33,34,35,36)                                               
         DFORM 1881,(34,35,36)                                                  
         DFORM 1894,(35,36)                                                     
         DFORM 363,(37)                                                         
         DFORM 1865,(32,33,34,35,36,37)                                         
         DFORM 1874,(33,34,35,36,37)                                            
         DFORM 366,(34,35,36,37)                                                
         DFORM 1895,(35,36,37)                                                  
         DFORM 1904,(36,37)                                                     
         DFORM 364,(38)                                                         
         DFORM 65,(32,33,34,35,36,37,38)                                        
         DFORM 1876,(33,34,35,36,37,38)                                         
         DFORM 1884,(34,35,36,37,38)                                            
         DFORM 1897,(35,36,37,38)                                               
         DFORM 1906,(36,37,38)                                                  
         DFORM 367,(37,38)                                                      
         DFORM 120,(31)                                                         
         DFORM 1,(39)                                                           
         FFEND                                                                  
                                                                                
         EMOD                      END OF ALL TABLES FOR MODIFIER               
                                                                                
* GAA IMPRESSIONS (B)                                                           
         FMOD  WB                                                               
                                                                                
         FTYP3 (197,198,199,24,26,68,69,196,207,208,209,203,55,58,     *        
               60,217,218,219,74,76,112,113,227,228,229,195,213,       *        
               105,108,110,120,230,231,232,233,234,235,236,1)                   
                                                                                
         FTYP4 (325,326,327,24,26,68,69,324,335,336,337,331,55,58,     *        
               60,345,346,347,74,76,112,113,355,356,357,323,341,       *        
               105,108,110,120,358,359,360,361,362,363,364,1)                   
                                                                                
         DFORM 325,(1)                                                          
         DFORM 326,(2)                                                          
         DFORM 770,(1,2)                                                        
         DFORM 327,(3)                                                          
         DFORM 119,(1,2,3)                                                      
         DFORM 21,(2,3)                                                         
         DFORM 24,(4)                                                           
         DFORM 772,(1,2,3,4)                                                    
         DFORM 786,(2,3,4)                                                      
         DFORM 61,(3,4)                                                         
         DFORM 26,(5)                                                           
         DFORM 773,(1,2,3,4,5)                                                  
         DFORM 787,(2,3,4,5)                                                    
         DFORM 800,(3,4,5)                                                      
         DFORM 25,(4,5)                                                         
         DFORM 68,(6)                                                           
         DFORM 774,(1,2,3,4,5,6)                                                
         DFORM 788,(2,3,4,5,6)                                                  
         DFORM 801,(3,4,5,6)                                                    
         DFORM 23,(4,5,6)                                                       
         DFORM 824,(5,6)                                                        
         DFORM 69,(7)                                                           
         DFORM 775,(1,2,3,4,5,6,7)                                              
         DFORM 789,(2,3,4,5,6,7)                                                
         DFORM 802,(3,4,5,6,7)                                                  
         DFORM 28,(4,5,6,7)                                                     
         DFORM 34,(5,6,7)                                                       
         DFORM 40,(6,7)                                                         
         DFORM 324,(8)                                                          
         DFORM 776,(1,2,3,4,5,6,7,8)                                            
         DFORM 790,(2,3,4,5,6,7,8)                                              
         DFORM 803,(3,4,5,6,7,8)                                                
         DFORM 815,(4,5,6,7,8)                                                  
         DFORM 826,(5,6,7,8)                                                    
         DFORM 836,(6,7,8)                                                      
         DFORM 845,(7,8)                                                        
         DFORM 335,(9)                                                          
         DFORM 777,(1,2,3,4,5,6,7,8,9)                                          
         DFORM 791,(2,3,4,5,6,7,8,9)                                            
         DFORM 804,(3,4,5,6,7,8,9)                                              
         DFORM 29,(4,5,6,7,8,9)                                                 
         DFORM 35,(5,6,7,8,9)                                                   
         DFORM 41,(6,7,8,9)                                                     
         DFORM 70,(7,8,9)                                                       
         DFORM 46,(8,9)                                                         
         DFORM 336,(10)                                                         
         DFORM 778,(1,2,3,4,5,6,7,8,9,10)                                       
         DFORM 792,(2,3,4,5,6,7,8,9,10)                                         
         DFORM 805,(3,4,5,6,7,8,9,10)                                           
         DFORM 817,(4,5,6,7,8,9,10)                                             
         DFORM 828,(5,6,7,8,9,10)                                               
         DFORM 838,(6,7,8,9,10)                                                 
         DFORM 847,(7,8,9,10)                                                   
         DFORM 855,(8,9,10)                                                     
         DFORM 862,(9,10)                                                       
         DFORM 337,(11)                                                         
         DFORM 779,(1,2,3,4,5,6,7,8,9,10,11)                                    
         DFORM 793,(2,3,4,5,6,7,8,9,10,11)                                      
         DFORM 806,(3,4,5,6,7,8,9,10,11)                                        
         DFORM 818,(4,5,6,7,8,9,10,11)                                          
         DFORM 829,(5,6,7,8,9,10,11)                                            
         DFORM 328,(6,7,8,9,10,11)                                              
         DFORM 848,(7,8,9,10,11)                                                
         DFORM 329,(8,9,10,11)                                                  
         DFORM 863,(9,10,11)                                                    
         DFORM 330,(10,11)                                                      
         DFORM 331,(12)                                                         
         DFORM 780,(1,2,3,4,5,6,7,8,9,10,11,12)                                 
         DFORM 794,(2,3,4,5,6,7,8,9,10,11,12)                                   
         DFORM 807,(3,4,5,6,7,8,9,10,11,12)                                     
         DFORM 30,(4,5,6,7,8,9,10,11,12)                                        
         DFORM 36,(5,6,7,8,9,10,11,12)                                          
         DFORM 42,(6,7,8,9,10,11,12)                                            
         DFORM 71,(7,8,9,10,11,12)                                              
         DFORM 47,(8,9,10,11,12)                                                
         DFORM 864,(9,10,11,12)                                                 
         DFORM 51,(10,11,12)                                                    
         DFORM 875,(11,12)                                                      
         DFORM 55,(13)                                                          
         DFORM 781,(1,2,3,4,5,6,7,8,9,10,11,12,13)                              
         DFORM 795,(2,3,4,5,6,7,8,9,10,11,12,13)                                
         DFORM 808,(3,4,5,6,7,8,9,10,11,12,13)                                  
         DFORM 31,(4,5,6,7,8,9,10,11,12,13)                                     
         DFORM 37,(5,6,7,8,9,10,11,12,13)                                       
         DFORM 43,(6,7,8,9,10,11,12,13)                                         
         DFORM 72,(7,8,9,10,11,12,13)                                           
         DFORM 48,(8,9,10,11,12,13)                                             
         DFORM 865,(9,10,11,12,13)                                              
         DFORM 52,(10,11,12,13)                                                 
         DFORM 876,(11,12,13)                                                   
         DFORM 332,(12,13)                                                      
         DFORM 58,(14)                                                          
         DFORM 782,(1,2,3,4,5,6,7,8,9,10,11,12,13,14)                           
         DFORM 796,(2,3,4,5,6,7,8,9,10,11,12,13,14)                             
         DFORM 809,(3,4,5,6,7,8,9,10,11,12,13,14)                               
         DFORM 32,(4,5,6,7,8,9,10,11,12,13,14)                                  
         DFORM 38,(5,6,7,8,9,10,11,12,13,14)                                    
         DFORM 44,(6,7,8,9,10,11,12,13,14)                                      
         DFORM 73,(7,8,9,10,11,12,13,14)                                        
         DFORM 49,(8,9,10,11,12,13,14)                                          
         DFORM 866,(9,10,11,12,13,14)                                           
         DFORM 53,(10,11,12,13,14)                                              
         DFORM 877,(11,12,13,14)                                                
         DFORM 333,(12,13,14)                                                   
         DFORM 56,(13,14)                                                       
         DFORM 60,(15)                                                          
         DFORM 783,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)                        
         DFORM 797,(2,3,4,5,6,7,8,9,10,11,12,13,14,15)                          
         DFORM 810,(3,4,5,6,7,8,9,10,11,12,13,14,15)                            
         DFORM 33,(4,5,6,7,8,9,10,11,12,13,14,15)                               
         DFORM 39,(5,6,7,8,9,10,11,12,13,14,15)                                 
         DFORM 45,(6,7,8,9,10,11,12,13,14,15)                                   
         DFORM 67,(7,8,9,10,11,12,13,14,15)                                     
         DFORM 50,(8,9,10,11,12,13,14,15)                                       
         DFORM 867,(9,10,11,12,13,14,15)                                        
         DFORM 54,(10,11,12,13,14,15)                                           
         DFORM 878,(11,12,13,14,15)                                             
         DFORM 334,(12,13,14,15)                                                
         DFORM 57,(13,14,15)                                                    
         DFORM 59,(14,15)                                                       
         DFORM 345,(16)                                                         
         DFORM 346,(17)                                                         
         DFORM 1026,(16,17)                                                     
         DFORM 347,(18)                                                         
         DFORM 20,(16,17,18)                                                    
         DFORM 22,(17,18)                                                       
         DFORM 74,(19)                                                          
         DFORM 1028,(16,17,18,19)                                               
         DFORM 1042,(17,18,19)                                                  
         DFORM 62,(18,19)                                                       
         DFORM 76,(20)                                                          
         DFORM 1029,(16,17,18,19,20)                                            
         DFORM 314,(17,18,19,20)                                                
         DFORM 1056,(18,19,20)                                                  
         DFORM 75,(19,20)                                                       
         DFORM 112,(21)                                                         
         DFORM 1030,(16,17,18,19,20,21)                                         
         DFORM 1044,(17,18,19,20,21)                                            
         DFORM 1057,(18,19,20,21)                                               
         DFORM 111,(19,20,21)                                                   
         DFORM 1080,(20,21)                                                     
         DFORM 113,(22)                                                         
         DFORM 1031,(16,17,18,19,20,21,22)                                      
         DFORM 1045,(17,18,19,20,21,22)                                         
         DFORM 1058,(18,19,20,21,22)                                            
         DFORM 78,(19,20,21,22)                                                 
         DFORM 84,(20,21,22)                                                    
         DFORM 90,(21,22)                                                       
         DFORM 355,(23)                                                         
         DFORM 1032,(16,17,18,19,20,21,22,23)                                   
         DFORM 1046,(17,18,19,20,21,22,23)                                      
         DFORM 1059,(18,19,20,21,22,23)                                         
         DFORM 1071,(19,20,21,22,23)                                            
         DFORM 1082,(20,21,22,23)                                               
         DFORM 1092,(21,22,23)                                                  
         DFORM 1101,(22,23)                                                     
         DFORM 356,(24)                                                         
         DFORM 1033,(16,17,18,19,20,21,22,23,24)                                
         DFORM 1047,(17,18,19,20,21,22,23,24)                                   
         DFORM 1060,(18,19,20,21,22,23,24)                                      
         DFORM 79,(19,20,21,22,23,24)                                           
         DFORM 85,(20,21,22,23,24)                                              
         DFORM 91,(21,22,23,24)                                                 
         DFORM 114,(22,23,24)                                                   
         DFORM 96,(23,24)                                                       
         DFORM 357,(25)                                                         
         DFORM 1034,(16,17,18,19,20,21,22,23,24,25)                             
         DFORM 1048,(17,18,19,20,21,22,23,24,25)                                
         DFORM 1061,(18,19,20,21,22,23,24,25)                                   
         DFORM 1073,(19,20,21,22,23,24,25)                                      
         DFORM 1084,(20,21,22,23,24,25)                                         
         DFORM 1094,(21,22,23,24,25)                                            
         DFORM 1103,(22,23,24,25)                                               
         DFORM 1111,(23,24,25)                                                  
         DFORM 1118,(24,25)                                                     
         DFORM 323,(26)                                                         
         DFORM 1035,(16,17,18,19,20,21,22,23,24,25,26)                          
         DFORM 1049,(17,18,19,20,21,22,23,24,25,26)                             
         DFORM 1062,(18,19,20,21,22,23,24,25,26)                                
         DFORM 1074,(19,20,21,22,23,24,25,26)                                   
         DFORM 1085,(20,21,22,23,24,25,26)                                      
         DFORM 338,(21,22,23,24,25,26)                                          
         DFORM 1104,(22,23,24,25,26)                                            
         DFORM 339,(23,24,25,26)                                                
         DFORM 1119,(24,25,26)                                                  
         DFORM 340,(25,26)                                                      
         DFORM 341,(27)                                                         
         DFORM 1036,(16,17,18,19,20,21,22,23,24,25,26,27)                       
         DFORM 1050,(17,18,19,20,21,22,23,24,25,26,27)                          
         DFORM 1063,(18,19,20,21,22,23,24,25,26,27)                             
         DFORM 80,(19,20,21,22,23,24,25,26,27)                                  
         DFORM 86,(20,21,22,23,24,25,26,27)                                     
         DFORM 92,(21,22,23,24,25,26,27)                                        
         DFORM 115,(22,23,24,25,26,27)                                          
         DFORM 97,(23,24,25,26,27)                                              
         DFORM 1120,(24,25,26,27)                                               
         DFORM 101,(25,26,27)                                                   
         DFORM 1131,(26,27)                                                     
         DFORM 105,(28)                                                         
         DFORM 1037,(16,17,18,19,20,21,22,23,24,25,26,27,28)                    
         DFORM 1051,(17,18,19,20,21,22,23,24,25,26,27,28)                       
         DFORM 1064,(18,19,20,21,22,23,24,25,26,27,28)                          
         DFORM 81,(19,20,21,22,23,24,25,26,27,28)                               
         DFORM 87,(20,21,22,23,24,25,26,27,28)                                  
         DFORM 93,(21,22,23,24,25,26,27,28)                                     
         DFORM 116,(22,23,24,25,26,27,28)                                       
         DFORM 98,(23,24,25,26,27,28)                                           
         DFORM 1121,(24,25,26,27,28)                                            
         DFORM 102,(25,26,27,28)                                                
         DFORM 1132,(26,27,28)                                                  
         DFORM 342,(27,28)                                                      
         DFORM 108,(29)                                                         
         DFORM 1038,(16,17,18,19,20,21,22,23,24,25,26,27,28,29)                 
         DFORM 1052,(17,18,19,20,21,22,23,24,25,26,27,28,29)                    
         DFORM 1065,(18,19,20,21,22,23,24,25,26,27,28,29)                       
         DFORM 82,(19,20,21,22,23,24,25,26,27,28,29)                            
         DFORM 88,(20,21,22,23,24,25,26,27,28,29)                               
         DFORM 94,(21,22,23,24,25,26,27,28,29)                                  
         DFORM 117,(22,23,24,25,26,27,28,29)                                    
         DFORM 99,(23,24,25,26,27,28,29)                                        
         DFORM 1122,(24,25,26,27,28,29)                                         
         DFORM 103,(25,26,27,28,29)                                             
         DFORM 1133,(26,27,28,29)                                               
         DFORM 343,(27,28,29)                                                   
         DFORM 106,(28,29)                                                      
         DFORM 110,(30)                                                         
         DFORM 1039,(16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)              
         DFORM 1053,(17,18,19,20,21,22,23,24,25,26,27,28,29,30)                 
         DFORM 1066,(18,19,20,21,22,23,24,25,26,27,28,29,30)                    
         DFORM 83,(19,20,21,22,23,24,25,26,27,28,29,30)                         
         DFORM 89,(20,21,22,23,24,25,26,27,28,29,30)                            
         DFORM 95,(21,22,23,24,25,26,27,28,29,30)                               
         DFORM 118,(22,23,24,25,26,27,28,29,30)                                 
         DFORM 100,(23,24,25,26,27,28,29,30)                                    
         DFORM 1123,(24,25,26,27,28,29,30)                                      
         DFORM 104,(25,26,27,28,29,30)                                          
         DFORM 1134,(26,27,28,29,30)                                            
         DFORM 344,(27,28,29,30)                                                
         DFORM 107,(28,29,30)                                                   
         DFORM 109,(29,30)                                                      
         DFORM 121,(1,16)                                                       
         DFORM 289,(2,17)                                                       
         DFORM 1282,(1,2,16,17)                                                 
         DFORM 290,(3,18)                                                       
         DFORM 122,(1,2,3,16,17,18)                                             
         DFORM 123,(2,3,17,18)                                                  
         DFORM 124,(4,19)                                                       
         DFORM 299,(1,2,3,4,16,17,18,19)                                        
         DFORM 306,(2,3,4,17,18,19)                                             
         DFORM 380,(3,4,18,19)                                                  
         DFORM 126,(5,20)                                                       
         DFORM 300,(1,2,3,4,5,16,17,18,19,20)                                   
         DFORM 307,(2,3,4,5,17,18,19,20)                                        
         DFORM 1312,(3,4,5,18,19,20)                                            
         DFORM 125,(4,5,19,20)                                                  
         DFORM 316,(6,21)                                                       
         DFORM 1286,(1,2,3,4,5,6,16,17,18,19,20,21)                             
         DFORM 1300,(2,3,4,5,6,17,18,19,20,21)                                  
         DFORM 1313,(3,4,5,6,18,19,20,21)                                       
         DFORM 315,(4,5,6,19,20,21)                                             
         DFORM 1336,(5,6,20,21)                                                 
         DFORM 317,(7,22)                                                       
         DFORM 301,(1,2,3,4,5,6,7,16,17,18,19,20,21,22)                         
         DFORM 308,(2,3,4,5,6,7,17,18,19,20,21,22)                              
         DFORM 1314,(3,4,5,6,7,18,19,20,21,22)                                  
         DFORM 256,(4,5,6,7,19,20,21,22)                                        
         DFORM 262,(5,6,7,20,21,22)                                             
         DFORM 268,(6,7,21,22)                                                  
         DFORM 291,(8,23)                                                       
         DFORM 1288,(1,2,3,4,5,6,7,8,16,17,18,19,20,21,22,23)                   
         DFORM 1302,(2,3,4,5,6,7,8,17,18,19,20,21,22,23)                        
         DFORM 1315,(3,4,5,6,7,8,18,19,20,21,22,23)                             
         DFORM 1327,(4,5,6,7,8,19,20,21,22,23)                                  
         DFORM 1338,(5,6,7,8,20,21,22,23)                                       
         DFORM 1348,(6,7,8,21,22,23)                                            
         DFORM 1357,(7,8,22,23)                                                 
         DFORM 292,(9,24)                                                       
         DFORM 302,(1,2,3,4,5,6,7,8,9,16,17,18,19,20,21,22,23,24)               
         DFORM 309,(2,3,4,5,6,7,8,9,17,18,19,20,21,22,23,24)                    
         DFORM 1316,(3,4,5,6,7,8,9,18,19,20,21,22,23,24)                        
         DFORM 257,(4,5,6,7,8,9,19,20,21,22,23,24)                              
         DFORM 263,(5,6,7,8,9,20,21,22,23,24)                                   
         DFORM 269,(6,7,8,9,21,22,23,24)                                        
         DFORM 318,(7,8,9,22,23,24)                                             
         DFORM 274,(8,9,23,24)                                                  
         DFORM 293,(10,25)                                                      
         DFORM 1290,(1,2,3,4,5,6,7,8,9,10,16,17,18,19,20,21,22,23,     *        
               24,25)                                                           
         DFORM 1304,(2,3,4,5,6,7,8,9,10,17,18,19,20,21,22,23,24,25)             
         DFORM 1317,(3,4,5,6,7,8,9,10,18,19,20,21,22,23,24,25)                  
         DFORM 1329,(4,5,6,7,8,9,10,19,20,21,22,23,24,25)                       
         DFORM 1340,(5,6,7,8,9,10,20,21,22,23,24,25)                            
         DFORM 1350,(6,7,8,9,10,21,22,23,24,25)                                 
         DFORM 1359,(7,8,9,10,22,23,24,25)                                      
         DFORM 1367,(8,9,10,23,24,25)                                           
         DFORM 1374,(9,10,24,25)                                                
         DFORM 294,(11,26)                                                      
         DFORM 1291,(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,     *        
               23,24,25,26)                                                     
         DFORM 1305,(2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,       *        
               24,25,26)                                                        
         DFORM 1318,(3,4,5,6,7,8,9,10,11,18,19,20,21,22,23,24,25,      *        
               26)                                                              
         DFORM 1330,(4,5,6,7,8,9,10,11,19,20,21,22,23,24,25,26)                 
         DFORM 1341,(5,6,7,8,9,10,11,20,21,22,23,24,25,26)                      
         DFORM 348,(6,7,8,9,10,11,21,22,23,24,25,26)                            
         DFORM 1360,(7,8,9,10,11,22,23,24,25,26)                                
         DFORM 349,(8,9,10,11,23,24,25,26)                                      
         DFORM 1375,(9,10,11,24,25,26)                                          
         DFORM 350,(10,11,25,26)                                                
         DFORM 351,(12,27)                                                      
         DFORM 303,(1,2,3,4,5,6,7,8,9,10,11,12,16,17,18,19,20,21,      *        
               22,23,24,25,26,27)                                               
         DFORM 310,(2,3,4,5,6,7,8,9,10,11,12,17,18,19,20,21,22,        *        
               23,24,25,26,27)                                                  
         DFORM 1319,(3,4,5,6,7,8,9,10,11,12,18,19,20,21,22,23,24,      *        
               25,26,27)                                                        
         DFORM 258,(4,5,6,7,8,9,10,11,12,19,20,21,22,23,24,25,26,      *        
               27)                                                              
         DFORM 264,(5,6,7,8,9,10,11,12,20,21,22,23,24,25,26,27)                 
         DFORM 270,(6,7,8,9,10,11,12,21,22,23,24,25,26,27)                      
         DFORM 319,(7,8,9,10,11,12,22,23,24,25,26,27)                           
         DFORM 275,(8,9,10,11,12,23,24,25,26,27)                                
         DFORM 1376,(9,10,11,12,24,25,26,27)                                    
         DFORM 279,(10,11,12,25,26,27)                                          
         DFORM 1387,(11,12,26,27)                                               
         DFORM 283,(13,28)                                                      
         DFORM 304,(1,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,      *        
               21,22,23,24,25,26,27,28)                                         
         DFORM 311,(2,3,4,5,6,7,8,9,10,11,12,13,17,18,19,20,21,        *        
               22,23,24,25,26,27,28)                                            
         DFORM 1320,(3,4,5,6,7,8,9,10,11,12,13,18,19,20,21,22,23,      *        
               24,25,26,27,28)                                                  
         DFORM 259,(4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,      *        
               26,27,28)                                                        
         DFORM 265,(5,6,7,8,9,10,11,12,13,20,21,22,23,24,25,26,        *        
               27,28)                                                           
         DFORM 271,(6,7,8,9,10,11,12,13,21,22,23,24,25,26,27,28)                
         DFORM 320,(7,8,9,10,11,12,13,22,23,24,25,26,27,28)                     
         DFORM 276,(8,9,10,11,12,13,23,24,25,26,27,28)                          
         DFORM 1377,(9,10,11,12,13,24,25,26,27,28)                              
         DFORM 280,(10,11,12,13,25,26,27,28)                                    
         DFORM 1388,(11,12,13,26,27,28)                                         
         DFORM 352,(12,13,27,28)                                                
         DFORM 286,(14,29)                                                      
         DFORM 305,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,      *        
               20,21,22,23,24,25,26,27,28,29)                                   
         DFORM 312,(2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,19,20,        *        
               21,22,23,24,25,26,27,28,29)                                      
         DFORM 1321,(3,4,5,6,7,8,9,10,11,12,13,14,18,19,20,21,22,      *        
               23,24,25,26,27,28,29)                                            
         DFORM 260,(4,5,6,7,8,9,10,11,12,13,14,19,20,21,22,23,24,      *        
               25,26,27,28,29)                                                  
         DFORM 266,(5,6,7,8,9,10,11,12,13,14,20,21,22,23,24,25,        *        
               26,27,28,29)                                                     
         DFORM 272,(6,7,8,9,10,11,12,13,14,21,22,23,24,25,26,27,       *        
               28,29)                                                           
         DFORM 321,(7,8,9,10,11,12,13,14,22,23,24,25,26,27,28,29)               
         DFORM 277,(8,9,10,11,12,13,14,23,24,25,26,27,28,29)                    
         DFORM 1378,(9,10,11,12,13,14,24,25,26,27,28,29)                        
         DFORM 281,(10,11,12,13,14,25,26,27,28,29)                              
         DFORM 1389,(11,12,13,14,26,27,28,29)                                   
         DFORM 353,(12,13,14,27,28,29)                                          
         DFORM 284,(13,14,28,29)                                                
         DFORM 288,(15,30)                                                      
         DFORM 127,(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,      *        
               19,20,21,22,23,24,25,26,27,28,29,30)                             
         DFORM 313,(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,        *        
               20,21,22,23,24,25,26,27,28,29,30)                                
         DFORM 1322,(3,4,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,      *        
               22,23,24,25,26,27,28,29,30)                                      
         DFORM 261,(4,5,6,7,8,9,10,11,12,13,14,15,19,20,21,22,23,      *        
               24,25,26,27,28,29,30)                                            
         DFORM 267,(5,6,7,8,9,10,11,12,13,14,15,20,21,22,23,24,        *        
               25,26,27,28,29,30)                                               
         DFORM 273,(6,7,8,9,10,11,12,13,14,15,21,22,23,24,25,26,       *        
               27,28,29,30)                                                     
         DFORM 322,(7,8,9,10,11,12,13,14,15,22,23,24,25,26,27,28,      *        
               29,30)                                                           
         DFORM 278,(8,9,10,11,12,13,14,15,23,24,25,26,27,28,29,30)              
         DFORM 1379,(9,10,11,12,13,14,15,24,25,26,27,28,29,30)                  
         DFORM 282,(10,11,12,13,14,15,25,26,27,28,29,30)                        
         DFORM 1390,(11,12,13,14,15,26,27,28,29,30)                             
         DFORM 354,(12,13,14,15,27,28,29,30)                                    
         DFORM 285,(13,14,15,28,29,30)                                          
         DFORM 287,(14,15,29,30)                                                
         DFORM 358,(32)                                                         
         DFORM 359,(33)                                                         
         DFORM 1859,(32,33)                                                     
         DFORM 360,(34)                                                         
         DFORM 1861,(32,33,34)                                                  
         DFORM 1870,(33,34)                                                     
         DFORM 361,(35)                                                         
         DFORM 1863,(32,33,34,35)                                               
         DFORM 1872,(33,34,35)                                                  
         DFORM 1880,(34,35)                                                     
         DFORM 362,(36)                                                         
         DFORM 365,(32,33,34,35,36)                                             
         DFORM 1873,(33,34,35,36)                                               
         DFORM 1881,(34,35,36)                                                  
         DFORM 1894,(35,36)                                                     
         DFORM 363,(37)                                                         
         DFORM 1865,(32,33,34,35,36,37)                                         
         DFORM 1874,(33,34,35,36,37)                                            
         DFORM 366,(34,35,36,37)                                                
         DFORM 1895,(35,36,37)                                                  
         DFORM 1904,(36,37)                                                     
         DFORM 364,(38)                                                         
         DFORM 65,(32,33,34,35,36,37,38)                                        
         DFORM 1876,(33,34,35,36,37,38)                                         
         DFORM 1884,(34,35,36,37,38)                                            
         DFORM 1897,(35,36,37,38)                                               
         DFORM 1906,(36,37,38)                                                  
         DFORM 367,(37,38)                                                      
         DFORM 120,(31)                                                         
         DFORM 1,(39)                                                           
         FFEND                                                                  
                                                                                
         EMOD                      END OF ALL TABLES FOR MODIFIER               
                                                                                
         EFTID                     END OF ALL MODIFIERS                         
                                                                                
XANN0174 DC    X'FFFF'             END OF ALL F/M/S/BKS                         
                                                                                
TBL_NFORMTABX DS 0X                                                             
                                                                                
         EJECT                                                                  
                                                                                
         DC    XL64'00'            SPARE                                        
         LTORG                                                                  
         EJECT                                                                  
                                                                                
       ++INCLUDE DEDEMTABD                                                      
         SPACE 3                                                                
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041DEDEMTABOF12/04/20'                                      
         END                                                                    
