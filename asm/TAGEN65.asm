*          DATA SET TAGEN65    AT LEVEL 013 AS OF 04/03/09                      
*PHASE T70265A                                                                  
         TITLE 'T70265 - TABLES FOR 1982 CONTRACTS'                             
T70265   CSECT                                                                  
         DC    AL4(USETBLS-T70265)                                              
         DC    AL4(USELUT-T70265)                                               
         DC    AL4(MAJLUT-T70265)                                               
         DC    AL4(AFMCOLS-T70265)                                              
         DC    AL4(RADCOLS-T70265)                                              
         DC    AL4(OFFCOLS-T70265)                                              
         DC    AL4(ONCOLS-T70265)                                               
         DC    AL4(MSWEET-T70265)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL1(0,36,1,1)       CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    F'30000'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'22560'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'21960'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'19440'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'16080'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'12720'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'11040'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'9000'             'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(0,36,2,2)       CLASS A USE 2                                
         DC    F'11595'                                                         
         DC    F'9075'                                                          
         DC    F'10745'                                                         
         DC    F'9200'                                                          
         DC    F'7530'                                                          
         DC    F'5830'                                                          
         DC    F'5070'                                                          
         DC    F'4160'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,3,3)       CLASS A USE 3                                
         DC    F'9200'                                                          
         DC    F'7215'                                                          
         DC    F'8410'                                                          
         DC    F'7625'                                                          
         DC    F'6235'                                                          
         DC    F'5450'                                                          
         DC    F'4665'                                                          
         DC    F'3810'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,4,13)      CLASS A USES 4-13                            
         DC    F'9200'                                                          
         DC    F'7215'                                                          
         DC    F'7940'                                                          
         DC    F'7150'                                                          
         DC    F'5860'                                                          
         DC    F'4975'                                                          
         DC    F'4345'                                                          
         DC    F'3560'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,14,255)    CLASS A USES 14+                             
         DC    F'4410'                                                          
         DC    F'3275'                                                          
         DC    F'2740'                                                          
         DC    F'2330'                                                          
         DC    F'1890'                                                          
         DC    F'1985'                                                          
         DC    F'1860'                                                          
         DC    F'1545'                                                          
         SPACE 1                                                                
CLBTAB   DC    AL1(1,36,0,0)       CLASS B WITH NY                              
         DC    F'71025'                                                         
         DC    F'50790'                                                         
         DC    F'45235'                                                         
         DC    F'40000'                                                         
         DC    F'32700'                                                         
         DC    F'16665'                                                         
         DC    F'13890'                                                         
         DC    F'11355'                                                         
         SPACE 1                                                                
CBXTAB   DC    AL1(2,36,0,0)       CLASS B W/O NY                               
         DC    F'57930'                                                         
         DC    F'40235'                                                         
         DC    F'45235'                                                         
         DC    F'40000'                                                         
         DC    F'32700'                                                         
         DC    F'16665'                                                         
         DC    F'13890'                                                         
         DC    F'11355'                                                         
         SPACE 1                                                                
CLCTAB   DC    AL1(3,36,0,0)       CLASS C                                      
         DC    F'34520'                                                         
         DC    F'23015'                                                         
         DC    F'29915'                                                         
         DC    F'26585'                                                         
         DC    F'21745'                                                         
         DC    F'13255'                                                         
         DC    F'11030'                                                         
         DC    F'9050'                                                          
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
         SPACE 3                                                                
DANTAB   DC    AL1(4,36,0,0)       DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    F'140870'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'98120'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'105775'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'93235'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'72470'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'43240'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'37870'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'27035'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL1(5,36,0,0)       DEALER A W/O NY                              
         DC    F'124585'                                                        
         DC    F'89980'                                                         
         DC    F'105775'                                                        
         DC    F'93235'                                                         
         DC    F'72470'                                                         
         DC    F'43240'                                                         
         DC    F'37870'                                                         
         DC    F'27035'                                                         
         SPACE 1                                                                
DBNTAB   DC    AL1(6,36,0,0)       CLASS B INCL NY                              
         DC    F'216600'                                                        
         DC    F'147385'                                                        
         DC    F'160820'                                                        
         DC    F'141765'                                                        
         DC    F'110335'                                                        
         DC    F'65875'                                                         
         DC    F'57655'                                                         
         DC    F'41125'                                                         
         SPACE 1                                                                
DBXTAB   DC    AL1(7,36,0,0)       CLASS B W/O NY                               
         DC    F'186880'                                                        
         DC    F'134760'                                                        
         DC    F'160820'                                                        
         DC    F'141765'                                                        
         DC    F'110335'                                                        
         DC    F'65875'                                                         
         DC    F'57655'                                                         
         DC    F'41125'                                                         
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE & SIGNATURE                             
         SPACE 3                                                                
G13TAB   DC    AL1(8,36,1,1)       13 USE                                       
         DC    F'121375'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'94350'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'102435'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'91600'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'75180'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'62685'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'54590'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'44685'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(8,4,2,13)       (2-13)                                       
         SPACE 1                                                                
         DC    AL1(8,36,14,18)     14-18                                        
         DC    F'8694'                                                          
         DC    F'6605'                                                          
         DC    F'6356'                                                          
         DC    F'5563'                                                          
         DC    F'4543'                                                          
         DC    F'4198'                                                          
         DC    F'3787'                                                          
         DC    F'3122'                                                          
         SPACE 1                                                                
         DC    AL1(8,36,19,255)    19+                                          
         DC    F'4410'                                                          
         DC    F'3275'                                                          
         DC    F'2740'                                                          
         DC    F'2330'                                                          
         DC    F'1890'                                                          
         DC    F'1985'                                                          
         DC    F'1860'                                                          
         DC    F'1545'                                                          
         SPACE                                                                  
SIGTAB   DC    AL1(9,36,0,0)       USE SIG                                      
         DC    F'0'                                                             
         DC    F'63245'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    3F'0'                                                            
         DC    2F'49805'           'OFF' 1-4M3,0-4S3,1-4M6,0-4S6,D3,D6          
         DC    F'43085'            'OFF' 1-4M9,1-4S9                            
         EJECT                                                                  
*              WILD SPOT TABLES - NO MAJORS - TV                                
         SPACE 3                                                                
WSPTAB   DC    AL1(10,36,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    F'30000'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'22560'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'21960'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'19440'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'16080'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'9490'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'7715'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'6325'             'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(10,36,2,25)     UNITS 2-25                                   
         DC    F'1200'                                                          
         DC    F'820'                                                           
         DC    F'935'                                                           
         DC    F'810'                                                           
         DC    F'660'                                                           
         DC    F'330'                                                           
         DC    F'260'                                                           
         DC    F'215'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,26,60)    UNITS 26-60                                  
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'480'                                                           
         DC    F'410'                                                           
         DC    F'335'                                                           
         DC    F'140'                                                           
         DC    F'95'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,61,125)   UNITS 61-125                                 
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'350'                                                           
         DC    F'275'                                                           
         DC    F'230'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,126,255)  UNITS 126+                                   
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'170'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL1(11,36,0,0)      NY ALONE                                     
         DC    F'68945'                                                         
         DC    F'48705'                                                         
         DC    F'44150'                                                         
         DC    F'39215'                                                         
         DC    F'32130'                                                         
         DC    F'17710'                                                         
         DC    F'14675'                                                         
         DC    F'12020'                                                         
         SPACE 1                                                                
         DC    AL1(11,36,1,35)     UNITS 1-35                                   
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'480'                                                           
         DC    F'410'                                                           
         DC    F'335'                                                           
         DC    F'140'                                                           
         DC    F'95'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,36,100)   UNITS 36-100                                 
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'350'                                                           
         DC    F'275'                                                           
         DC    F'230'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,101,255)  UNITS 101+                                   
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'170'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL1(12,36,0,0)      ALONE                                        
         DC    F'60090'                                                         
         DC    F'42380'                                                         
         DC    F'44150'                                                         
         DC    F'39215'                                                         
         DC    F'32130'                                                         
         DC    F'17710'                                                         
         DC    F'14675'                                                         
         DC    F'12020'                                                         
         SPACE 1                                                                
         DC    AL1(12,36,1,35)     UNITS 1-35                                   
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'480'                                                           
         DC    F'410'                                                           
         DC    F'335'                                                           
         DC    F'140'                                                           
         DC    F'95'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,36,100)   UNITS 36-100                                 
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'350'                                                           
         DC    F'275'                                                           
         DC    F'230'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,101,255)  UNITS 101+                                   
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'170'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL1(13,36,0,0)      TWO OF NY LA CHI                             
         DC    F'94875'                                                         
         DC    F'63885'                                                         
         DC    F'67930'                                                         
         DC    F'56165'                                                         
         DC    F'45920'                                                         
         DC    F'23405'                                                         
         DC    F'18850'                                                         
         DC    F'15435'                                                         
         SPACE 1                                                                
         DC    AL1(13,36,1,255)    UNITS 1+                                     
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'170'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(14,36,0,0)      ALL THREE MAJORS                             
         DC    F'111320'                                                        
         DC    F'79065'                                                         
         DC    F'83365'                                                         
         DC    F'71345'                                                         
         DC    F'58315'                                                         
         DC    F'27450'                                                         
         DC    F'22140'                                                         
         DC    F'18090'                                                         
         SPACE 1                                                                
         DC    AL1(14,36,1,255)    UNITS 1+                                     
         DC    F'445'                                                           
         DC    F'350'                                                           
         DC    F'170'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL1(15,28,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    F'11000'            ANN ALONE                                    
         DC    F'11000'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8110'             1-4M3,1-4S3,D3,S3                            
         DC    F'7180'             1-4M6,1-4S6,D6,S6                            
         DC    F'6370'             1-4M9,1-4S9,D9,S9                            
         DC    F'4355'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL1(15,24,2,25)     UNITS 2-25                                   
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'100'                                                           
         DC    F'90'                                                            
         DC    F'80'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,26,60)    UNITS 26-60                                  
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'85'                                                            
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,61,255)   UNITS 61+                                    
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'50'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(16,28,0,0)      NEW YORK ALONE                               
         DC    F'19300'                                                         
         DC    F'19300'                                                         
         DC    F'10500'                                                         
         DC    F'8150'                                                          
         DC    F'5400'                                                          
         DC    F'4355'                                                          
         SPACE 1                                                                
         DC    AL1(16,24,1,35)     UNITS 1-35                                   
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'85'                                                            
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(16,24,36,255)   UNITS 36+                                    
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'50'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         SPACE 1                                                                
         DC    AL1(17,28,0,0)      CHICAGO OR LA ALONE                          
         DC    F'17500'                                                         
         DC    F'17500'                                                         
         DC    F'10500'                                                         
         DC    F'8150'                                                          
         DC    F'5400'                                                          
         DC    F'4355'                                                          
         SPACE 1                                                                
         DC    AL1(17,24,1,35)     UNITS 1-35                                   
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'85'                                                            
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(17,24,36,255)   UNITS 36+                                    
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'50'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(18,28,0,0)      ANY TWO ALONE                                
         DC    F'23000'                                                         
         DC    F'23000'                                                         
         DC    F'12250'                                                         
         DC    F'9400'                                                          
         DC    F'6400'                                                          
         DC    F'4355'                                                          
         SPACE 1                                                                
         DC    AL1(18,24,1,255)    UNITS 1+                                     
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'50'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         SPACE 1                                                                
         DC    AL1(19,28,0,0)      ALL THREE ALONE                              
         DC    F'27800'                                                         
         DC    F'27800'                                                         
         DC    F'13050'                                                         
         DC    F'10100'                                                         
         DC    F'7300'                                                          
         DC    F'4355'                                                          
         SPACE 1                                                                
         DC    AL1(19,24,1,255)    UNITS 1+                                     
         DC    F'140'                                                           
         DC    F'140'                                                           
         DC    F'50'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL1(20,24,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(11000) ANN ALONE                                    
         DC    AL1(100),AL3(11000) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(8110)  1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(7180)  1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(6370)  1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL1(20,24,2,25)                                                  
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(100)                                                 
         DC    AL1(95),AL3(90)                                                  
         DC    AL1(95),AL3(80)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,26,60)    UNITS 26-60                                  
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(85)                                                  
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(60)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,61,255)   UNITS 61+                                    
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(35)                                                  
         DC    AL1(95),AL3(35)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(21,24,0,0)      NEW YORK ALONE                               
         DC    AL1(80),AL3(19300)                                               
         DC    AL1(80),AL3(19300)                                               
         DC    AL1(95),AL3(10500)                                               
         DC    AL1(95),AL3(8150)                                                
         DC    AL1(95),AL3(5400)                                                
         SPACE 1                                                                
         DC    AL1(21,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(85)                                                  
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(60)                                                  
         SPACE 1                                                                
         DC    AL1(21,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(35)                                                  
         DC    AL1(95),AL3(35)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,0,0)      CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(17500)                                               
         DC    AL1(80),AL3(17500)                                               
         DC    AL1(95),AL3(10500)                                               
         DC    AL1(95),AL3(8150)                                                
         DC    AL1(95),AL3(5400)                                                
         SPACE 1                                                                
         DC    AL1(22,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(85)                                                  
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(60)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(35)                                                  
         DC    AL1(95),AL3(35)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(23,24,0,0)      ANY TWO ALONE                                
         DC    AL1(80),AL3(23000)                                               
         DC    AL1(80),AL3(23000)                                               
         DC    AL1(95),AL3(12250)                                               
         DC    AL1(95),AL3(9400)                                                
         DC    AL1(95),AL3(6400)                                                
         SPACE 1                                                                
         DC    AL1(23,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(35)                                                  
         DC    AL1(95),AL3(35)                                                  
         SPACE 1                                                                
         DC    AL1(24,24,0,0)      ALL THREE ALONE                              
         DC    AL1(80),AL3(27800)                                               
         DC    AL1(80),AL3(27800)                                               
         DC    AL1(95),AL3(13050)                                               
         DC    AL1(95),AL3(10100)                                               
         DC    AL1(95),AL3(7300)                                                
         SPACE 1                                                                
         DC    AL1(24,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(80),AL3(140)                                                 
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(35)                                                  
         DC    AL1(95),AL3(35)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TALES - RADIO                                 
         SPACE 3                                                                
DLRTAB   DC    AL1(25,28,0,0)      DEALER COMMERCIALS                           
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    F'43204'            AR,AS,ANN                                    
         DC    F'34269'            P,S,1-4MS,1-4SS                              
         DC    F'22345'            1-4M3,1-4S3,D3,S3                            
         DC    F'17875'            1-4M6,1-4S6,D6,S6                            
         DC    F'11173'            1-4M9,1-4S9,D9,S9                            
         DC    F'10318'            SE                                           
         SPACE 1                                                                
N01TAB   DC    AL1(26,28,0,0)      NETWORK 1 WEEK                               
         DC    F'26500'                                                         
         DC    F'26500'                                                         
         DC    F'19885'                                                         
         DC    F'19885'                                                         
         DC    F'19885'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
N04TAB   DC    AL1(27,28,0,0)      NETWORK 4 WEEK                               
         DC    F'43000'                                                         
         DC    F'43000'                                                         
         DC    F'33065'                                                         
         DC    F'29570'                                                         
         DC    F'27015'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
N08TAB   DC    AL1(28,28,0,0)      NETWORK 8 WEEK                               
         DC    F'68500'                                                         
         DC    F'68500'                                                         
         DC    F'52700'                                                         
         DC    F'47100'                                                         
         DC    F'42190'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
N13TAB   DC    AL1(29,28,0,0)      NETWORK 13 WEEK                              
         DC    F'85000'                                                         
         DC    F'85000'                                                         
         DC    F'65370'                                                         
         DC    F'58455'                                                         
         DC    F'53550'                                                         
         DC    F'5670'                                                          
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL1(30,28,0,0)      ACROSS-THE-BOARD                             
         DC    F'89000'                                                         
         DC    F'89000'                                                         
         DC    F'68440'                                                         
         DC    F'61200'                                                         
         DC    F'56070'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
U26TAB   DC    AL1(31,28,0,0)      26 USE LIMIT                                 
         DC    F'63500'                                                         
         DC    F'42500'                                                         
         DC    F'32680'                                                         
         DC    F'29225'                                                         
         DC    F'26705'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
U39TAB   DC    AL1(32,28,0,0)      39 USE LIMIT                                 
         DC    F'64000'                                                         
         DC    F'64000'                                                         
         DC    F'44815'                                                         
         DC    F'40000'                                                         
         DC    F'36345'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
R13TAB   DC    AL1(33,28,0,0)      REGIONAL - NO MAJORS                         
         DC    F'75160'                                                         
         DC    F'52285'                                                         
         DC    F'52285'                                                         
         DC    F'47055'                                                         
         DC    F'24510'                                                         
         DC    F'5670'                                                          
         SPACE 1                                                                
         DC    AL1(34,28,0,0)      REGIONAL - WITH ANY MAJORS                   
         DC    F'75160'                                                         
         DC    F'52285'                                                         
         DC    F'52285'                                                         
         DC    F'47055'                                                         
         DC    F'42320'                                                         
         DC    F'5670'                                                          
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'4830'             CAST=1                                       
         DC    F'5219'                  2-4                                     
         DC    F'4830'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'6440'             CAST=1                                       
         DC    F'6960'                  2-4                                     
         DC    F'6440'                  5+                                      
         SPACE 3                                                                
MS8TAB   DC    AL1(95,16,0,0)      8-WEEK REUSE                                 
         DC    AL1(80),AL3(4830)                                                
         DC    AL1(80),AL3(5219)                                                
         DC    AL1(80),AL3(4830)                                                
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 2                                                                
         DC    AL1(61,28,0,0)      AFT RADIO BASE SESSION RATES                 
         DC    F'11000'            ANN ALONE                                    
         DC    F'11000'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8110'             1-4M3,1-4S3,D3,S3                            
         DC    F'7180'             1-4M6,1-4S6,D6,S6                            
         DC    F'6370'             1-4M9,1-4S9,D9,S9                            
         DC    F'8500'             SE                                           
         SPACE 1                                                                
         DC    AL1(62,64,0,0)      NON-AFM TV/CABLE BASE SESSION RATES          
         DC    F'30000'                                                         
         DC    F'22560'                                                         
         DC    F'21960'                                                         
         DC    F'19440'                                                         
         DC    F'16080'                                                         
         DC    F'12720'                                                         
         DC    F'11040'                                                         
         DC    F'9000'                                                          
         DC    F'19514'            EXB                                          
         DC    F'27632'            HMB                                          
         DC    F'11329'            EX                                           
         DC    F'18379'            HM                                           
         DC    F'46205'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'16590'            SE                                           
         SPACE 1                                                                
         DC    AL1(63,52,0,0)      TV HOLDING RATES                             
         DC    F'30000'                                                         
         DC    F'22560'                                                         
         DC    F'21960'                                                         
         DC    F'19440'                                                         
         DC    F'16080'                                                         
         DC    5A(0)                                                            
         DC    F'14606'                                                         
         DC    F'19920'                                                         
         EJECT                                                                  
*              CABLE RATES                                                      
         SPACE 3                                                                
         DC    AL1(70,36,0,0)      CAB                                          
*              CATEGORIES SAME FOR ALL CABLE (ENTRIES 70-74)                    
         DC    F'38495'                                                         
         DC    F'28945'                                                         
         DC    F'28175'                                                         
         DC    F'24945'                                                         
         DC    F'20635'                                                         
         DC    F'16320'                                                         
         DC    F'14165'                                                         
         DC    F'11550'                                                         
         SPACE 1                                                                
         DC    AL1(71,36,0,0)      C04                                          
         DC    F'10000'                                                         
         DC    F'7520'                                                          
         DC    F'7320'                                                          
         DC    F'6480'                                                          
         DC    F'5360'                                                          
         DC    F'4240'                                                          
         DC    F'3680'                                                          
         DC    F'3000'                                                          
         SPACE 1                                                                
         DC    AL1(72,36,0,0)      C13                                          
         DC    F'30000'                                                         
         DC    F'22560'                                                         
         DC    F'21960'                                                         
         DC    F'19440'                                                         
         DC    F'16080'                                                         
         DC    F'12720'                                                         
         DC    F'11040'                                                         
         DC    F'9000'                                                          
         SPACE 1                                                                
         DC    AL1(73,36,0,0)      C52                                          
         DC    F'75000'                                                         
         DC    F'56400'                                                         
         DC    F'54900'                                                         
         DC    F'48600'                                                         
         DC    F'40200'                                                         
         DC    F'31800'                                                         
         DC    F'27600'                                                         
         DC    F'22500'                                                         
         SPACE 1                                                                
         DC    AL1(74,36,0,0)      13-52 UPGRADE                                
         DC    F'45000'                                                         
         DC    F'33840'                                                         
         DC    F'32940'                                                         
         DC    F'29160'                                                         
         DC    F'24120'                                                         
         DC    F'19080'                                                         
         DC    F'16560'                                                         
         DC    F'13500'                                                         
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USAGE                                  
         SPACE 3                                                                
         DC    AL1(80,40,0,0)      DEM (TV)                                     
         DC    F'22560'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'11280'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'16470'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'14580'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'12060'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'7560'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'7560'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'7560'             'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    F'12000'            'OFF' S                                      
         SPACE 1                                                                
         DC    AL1(85,28,0,0)      DEM (AFT RADIO)                              
         DC    F'7625'             ANN ALONE                                    
         DC    F'7625'             AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'7625'             1-4M3,1-4S3,D3,S3                            
         DC    F'7625'             1-4M6,1-4S6,D6,S6                            
         DC    F'7625'             1-4M9,1-4S9,D9,S9                            
         DC    F'12100'            S                                            
         SPACE 1                                                                
         DC    AL1(86,36,0,0)      FOREIGN USE - TV  (NOT UK)                   
         DC    F'30000'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'22560'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'21960'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'19440'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'16080'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'12720'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'11040'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'9000'             'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(87,36,0,0)      FOREIGN USE - TV  (UK)                       
         DC    F'90000'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'67680'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'65880'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'58320'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'53595'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'48240'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'38160'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'27000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(88,24,0,0)      FOREIGN USE - RADIO                          
         DC    F'30250'            ANN ALONE                                    
         DC    F'30250'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'17550'            1-4M3,1-4S3,D3,S3                            
         DC    F'12100'            1-4M6,1-4S6,D6,S6                            
         DC    F'9680'             1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(60,UBSM,ALL,AFM+NON,0,0,0,ALL)         SESSION               
         DC    AL1(61,UBSR,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UBSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UFGS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USFS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
*                                                                               
         DC    AL1(61,ULFT,ALL,AFT+NON,0,0,0,RADIO)   LIFT                      
         DC    AL1(62,ULFT,ALL,ALL-AFM,0,0,0,TV)                                
*                                                                               
         DC    AL1(80,UDEM,ALL,ALL,0,0,0,TV)          DEMO                      
         DC    AL1(85,UDEM,ALL,ALL,0,0,0,RADIO)                                 
         DC    AL1(80,USNA,ALL,ALL,0,0,0,TV)          SPANISH DEMO              
         DC    AL1(85,USNA,ALL,ALL,0,0,0,RADIO)                                 
*                                                                               
         DC    AL1(63,UHLD,ALL,ALL-AFM,0,0,0,TV)      HOLDING FEE               
*                                                                               
         DC    AL1(00,UCLA,UCLAREG,ALL,0,0,0,ALL)     CLA                       
         DC    AL1(08,UCLA,UCLAG13,ALL,0,0,0,ALL)     13 USE GUAR               
*                                                                               
         DC    AL1(01,ULOC,ULOCBNY,ALL,0,0,0,ALL)     CLASS B + NY              
         DC    AL1(02,ULOC,ULOCB,ALL,0,0,0,ALL)       CLASS B - NY              
         DC    AL1(03,ULOC,ULOCC,ALL,0,0,0,ALL)       CLASS C                   
*                                                                               
         DC    AL1(10,UWSP,UWSP13W,ALL,0,0,0,TV)      13 WK TV                  
         DC    AL1(15,UWSP,UWSP13W,ALL,0,0,0,RADIO)   13 WK RADIO               
         DC    AL1(20,UWSP,UWSP8W,ALL,0,0,0,RADIO)    8 WK                      
*                                                                               
         DC    AL1(04,UDLR,UDLRANY,ALL,0,0,0,ALL)     DLR A + NY                
         DC    AL1(05,UDLR,UDLRA,ALL,0,0,0,ALL)       DLR A - NY                
         DC    AL1(06,UDLR,UDLRBNY,ALL,0,0,0,ALL)     DLR B + NY                
         DC    AL1(07,UDLR,UDLRB,ALL,0,0,0,ALL)       DLR B - NY                
         DC    AL1(25,UDLR,UDLRRAD,ALL,0,0,0,ALL)     DLR RADIO                 
*                                                                               
         DC    AL1(70,UCAB,UCABTV,ALL,0,0,0,ALL)      BDCST CML USE             
         DC    AL1(71,UCAB,UCAB4W,ALL,0,0,0,ALL)      4 WK                      
         DC    AL1(72,UCAB,UCAB13W,ALL,0,0,0,ALL)     13 WK                     
         DC    AL1(73,UCAB,UCAB52W,ALL,0,0,0,ALL)     52 WK                     
         DC    AL1(72,UCAB,UCABU413,ALL,0,0,0,ALL)    4-13 WK                   
         DC    AL1(73,UCAB,UCABU452,ALL,0,0,0,ALL)    4-52 WK                   
         DC    AL1(74,UCAB,UCABU13,ALL,0,0,0,ALL)     13-52 WK                  
*                                                                               
         DC    AL1(86,UFGN,UFGNXUK,ALL,0,0,0,TV)      FOREIGN - UK              
         DC    AL1(87,UFGN,UFGNUK,ALL,0,0,0,TV)               + UK              
         DC    AL1(88,UFGN,UFGNXUK,ALL,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(26,URNT,URNT1W,ALL,0,0,0,ALL)     RADIO NTWK 1 WK            
         DC    AL1(27,URNT,URNT4W,ALL,0,0,0,ALL)                4 WK            
         DC    AL1(28,URNT,URNT8W,ALL,0,0,0,ALL)                8 WK            
         DC    AL1(29,URNT,URNT13W,ALL,0,0,0,ALL)               13 WK           
         DC    AL1(30,URNT,URNTAB,ALL,0,0,0,ALL)                A-B             
         DC    AL1(31,URNT,URNT26U,ALL,0,0,0,ALL)               26 USE          
         DC    AL1(32,URNT,URNT39U,ALL,0,0,0,ALL)               39 USE          
*                                                                               
         DC    AL1(33,URRN,ALL,ALL,0,0,0,RADIO)       RAD REG NWK               
*                                                                               
         DC    AL1(90,URLO,ALL,ALL,0,0,0,ALL)         RAD LCL 13WK              
*                                                                               
         DC    AL1(35,UMUS,UMUSDUB,ALL,0,0,0,ALL)     DUBBING                   
         DC    AL1(35,UMUS,UMUS13W,ALL,0,0,0,ALL)     REUSE                     
         DC    AL1(35,UMUS,UMUSNEW,ALL,0,0,0,ALL)     NEW                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              PERCENTAGE TABLES FOR MULTITRACKING & SWEETENING                 
         SPACE 3                                                                
MSWEET   DS    0CL3                                                             
         DC    AL1(CTSS1),AL2(100) 1979 CODE            SWEETENING              
         DC    AL1(CTSS2),AL2(200)                      1-2 SINGERS             
         DC    AL1(CTSS3),AL2(300)                                              
         DC    AL1(CTSS4),AL2(400)                                              
         DC    X'FF',AL2(50)                            ALL OTHERS              
         SPACE 3                                                                
         SPACE 3                                                                
*              MAJORS TABLE                                                     
         SPACE 3                                                                
MAJLUT   DS    0CL2                                                             
         DC    AL1(0,0)                                                         
         DC    AL1(1,NY)                                                        
         DC    AL1(2,CHI)                                                       
         DC    AL1(2,LA)                                                        
         DC    AL1(3,NY+CHI)                                                    
         DC    AL1(3,NY+LA)                                                     
         DC    AL1(3,CHI+LA)                                                    
         DC    AL1(4,NY+CHI+LA)                                                 
         EJECT                                                                  
*              COLUMN TABLES - TV - ON CAMERA                                   
         SPACE 1                                                                
ONCOLS   DS    0CL2                                                             
         DC    AL1(1,CTACR)                                                     
         DC    AL1(1,CTP)                                                       
         DC    AL1(1,CTANN)                                                     
         DC    AL1(1,CTS)                                                       
         DC    AL1(1,CTD)                                                       
         DC    AL1(1,CTSD)                                                      
         DC    AL1(1,CTSM)                                                      
         DC    AL1(1,CTSS1)                                                     
         DC    AL1(1,CTSS2)                                                     
         DC    AL1(1,CTSS3)                                                     
         DC    AL1(1,CTSS4)                                                     
         DC    AL1(1,CTSSM)                                                     
         DC    AL1(1,CTC)                                                       
         DC    AL1(1,CTPP)                                                      
         DC    AL1(1,CTPT)                                                      
         DC    AL1(1,CTSA)                                                      
         DC    AL1(1,CTSS)                                                      
         DC    AL1(1,CTST)                                                      
         DC    AL1(1,CTSOC)                                                     
*                                                                               
         DC    AL1(3,CTG3)                                                      
         DC    AL1(3,CTG3M)                                                     
         DC    AL1(3,CTGD3)                                                     
         DC    AL1(3,CTGD)                                                      
         DC    AL1(3,CTGS)                                                      
         DC    AL1(3,CTGSS)                                                     
         DC    AL1(3,CTGSM)                                                     
*                                                                               
         DC    AL1(4,CTG6)                                                      
         DC    AL1(4,CTG6M)                                                     
         DC    AL1(4,CTGD6)                                                     
*                                                                               
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTG9M)                                                     
         DC    AL1(5,CTGD9)                                                     
*                                                                               
         DC    AL1(9,CTEXB)                                                     
         DC    AL1(9,CTE)                                                       
         DC    AL1(9,CTSB)                                                      
         DC    AL1(9,CTSI)                                                      
         DC    AL1(9,CTUS)                                                      
         DC    AL1(9,CTGE)                                                      
*                                                                               
         DC    AL1(10,CTHMB)                                                    
         DC    AL1(10,CTDEM)                                                    
*                                                                               
         DC    AL1(11,CTEX)                                                     
*                                                                               
         DC    AL1(12,CTHM)                                                     
*                                                                               
         DC    AL1(13,CTPIL)                                                    
*                                                                               
         DC    AL1(18,CTZZZ)       OTHER CATEGORY-ROW SHOULDN'T EXIST           
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - TV - OFF CAMERA                                  
         SPACE 1                                                                
OFFCOLS  DS    0CL2                                                             
         DC    AL1(2,CTACR)                                                     
         DC    AL1(2,CTP)                                                       
         DC    AL1(2,CTPUP)                                                     
         DC    AL1(2,CTANN)                                                     
         DC    AL1(2,CTS)                                                       
         DC    AL1(2,CTD)                                                       
         DC    AL1(2,CTSM)                                                      
         DC    AL1(2,CTSS1)                                                     
         DC    AL1(2,CTSS2)                                                     
         DC    AL1(2,CTSS3)                                                     
         DC    AL1(2,CTSS4)                                                     
         DC    AL1(2,CTCV)                                                      
         DC    AL1(2,CTC)                                                       
         DC    AL1(2,CTSS)                                                      
         DC    AL1(2,CTVO)                                                      
         DC    AL1(2,CTSSM)                                                     
*                                                                               
         DC    AL1(6,CTG3)                                                      
         DC    AL1(6,CTG3M)                                                     
         DC    AL1(6,CTGD3)                                                     
         DC    AL1(6,CTGD)                                                      
         DC    AL1(6,CTGS)                                                      
         DC    AL1(6,CTGSS)                                                     
         DC    AL1(6,CTGSM)                                                     
*                                                                               
         DC    AL1(7,CTG6)                                                      
         DC    AL1(7,CTG6M)                                                     
         DC    AL1(7,CTGD6)                                                     
*                                                                               
         DC    AL1(8,CTG9)                                                      
         DC    AL1(8,CTG9M)                                                     
         DC    AL1(8,CTGD9)                                                     
*                                                                               
         DC    AL1(15,CTSE)                                                     
*                                                                               
         DC    AL1(16,CTC3)                                                     
         DC    AL1(16,CTC6)                                                     
*                                                                               
         DC    AL1(17,CTC9)                                                     
*                                                                               
         DC    AL1(18,CTZZZ)       OTHER CATEGORY-ROW SHOULDN'T EXIST           
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - RADIO                                            
         SPACE 1                                                                
RADCOLS  DS    0CL2                                                             
         DC    AL1(2,CTACR)                                                     
         DC    AL1(2,CTP)                                                       
         DC    AL1(2,CTANN)                                                     
         DC    AL1(2,CTS)                                                       
         DC    AL1(2,CTD)                                                       
         DC    AL1(2,CTSM)                                                      
         DC    AL1(2,CTSS1)                                                     
         DC    AL1(2,CTSS2)                                                     
         DC    AL1(2,CTSS3)                                                     
         DC    AL1(2,CTSS4)                                                     
*                                                                               
         DC    AL1(3,CTGD3)                                                     
         DC    AL1(3,CTG3)                                                      
         DC    AL1(3,CTG3M)                                                     
*                                                                               
         DC    AL1(4,CTGD6)                                                     
         DC    AL1(4,CTG6)                                                      
         DC    AL1(4,CTG6M)                                                     
*                                                                               
         DC    AL1(5,CTGD9)                                                     
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTG9M)                                                     
*                                                                               
         DC    AL1(6,CTSE)                                                      
*                                                                               
         DC    AL1(18,CTZZZ)       OTHER CATEGORY-ROW SHOULDN'T EXIST           
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - AFM                                              
         SPACE 3                                                                
AFMCOLS  DS    0CL2                                                             
         DC    AL1(1,CTA)          ARRANGER                                     
         DC    AL1(1,CTACP)        ARRANGER-COPYIST                             
         DC    AL1(1,CTAM)         ARRANGER-MUSICIAN                            
         DC    AL1(1,CTAMC)        ARRANGER-MUSICIAN-COPYIST                    
         DC    AL1(1,CTAO)         ARRANGER-ORCHESTRATOR                        
*                                                                               
         DC    AL1(1,CTC)          CONTRACTOR                                   
         DC    AL1(1,CTCO)         CONTRACTOR-ORCHESTRATOR                      
         DC    AL1(1,CTCCP)        CONTRACTOR-COPYIST                           
*                                                                               
         DC    AL1(1,CTCP)         COPYIST                                      
*                                                                               
         DC    AL1(1,CTL)          LEADER                                       
         DC    AL1(1,CTLA)         LEADER-ARRANGER                              
         DC    AL1(1,CTLAC)        LEADER-ARRANGER-COPYIST                      
         DC    AL1(1,CTLCP)        LEADER-COPYIST                               
         DC    AL1(1,CTLM)         LEADER-MUSICIAN                              
         DC    AL1(1,CTLO)         LEADER-ORCHESTRATOR                          
         DC    AL1(1,CTLOC)        LEADER-ORCHESTRATOR-COPYIST                  
*                                                                               
         DC    AL1(1,CTM)          MUSICIAN                                     
         DC    AL1(1,CTMCP)        MUSICIAN-COPYIST                             
*                                                                               
         DC    AL1(1,CTO)          ORCHESTRATOR                                 
         DC    AL1(1,CTOCP)        ORCHESTRATOR-COPYIST                         
         DC    AL1(1,CTOM)         ORCHESTRATOR-MUSICIAN                        
         DC    AL1(1,CTOMC)        ORCHESTRATOR-MUSICIAN-COPYIST                
*                                                                               
         DC    AL1(1,CTSYN)        SYNTHESIZER                                  
*                                                                               
         DC    AL1(1,CTZZZ)        GENERAL                                      
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGEN65   04/03/09'                                      
         END                                                                    
