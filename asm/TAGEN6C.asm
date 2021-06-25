*          DATA SET TAGEN6C    AT LEVEL 051 AS OF 10/05/11                      
*PHASE T7026CC,*                                                                
         TITLE 'T7026C - TABLES FOR 1993 CONTRACTS'                             
T7026C   CSECT                                                                  
         DC    AL4(USETBLS-T7026C)                                              
         DC    AL4(USELUT-T7026C)                                               
         DC    AL4(MAJLUT-T7026C)                                               
         DC    AL4(AFMCOLS-T7026C)                                              
         DC    AL4(RADCOLS-T7026C)                                              
         DC    AL4(OFFCOLS-T7026C)                                              
         DC    AL4(ONCOLS-T7026C)                                               
         DC    AL4(MSWEET-T7026C)                                               
         DC    AL4(TAGFEE-T7026C)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL1(0,36,1,1)       CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    F'41425'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'31150'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'30325'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'26845'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'22205'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'17565'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'15245'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'12430'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(0,36,2,2)       CLASS A USE 2                                
         DC    F'12270'                                                         
         DC    F'9600'                                                          
         DC    F'11370'                                                         
         DC    F'9735'                                                          
         DC    F'7965'                                                          
         DC    F'6170'                                                          
         DC    F'5365'                                                          
         DC    F'4400'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,3,3)       CLASS A USE 3                                
         DC    F'9735'                                                          
         DC    F'7635'                                                          
         DC    F'8900'                                                          
         DC    F'8065'                                                          
         DC    F'6595'                                                          
         DC    F'5765'                                                          
         DC    F'4935'                                                          
         DC    F'4030'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,4,13)      CLASS A USES 4-13                            
         DC    F'9735'                                                          
         DC    F'7635'                                                          
         DC    F'8400'                                                          
         DC    F'7565'                                                          
         DC    F'6200'                                                          
         DC    F'5265'                                                          
         DC    F'4595'                                                          
         DC    F'3765'                                                          
         SPACE 1                                                                
         DC    AL1(0,36,14,255)    CLASS A USES 14+                             
         DC    F'4665'                                                          
         DC    F'3465'                                                          
         DC    F'2900'                                                          
         DC    F'2465'                                                          
         DC    F'2000'                                                          
         DC    F'2100'                                                          
         DC    F'1970'                                                          
         DC    F'1635'                                                          
         SPACE 1                                                                
CLBTAB   DC    AL1(1,36,0,0)       CLASS B WITH NY                              
         DC    F'94600'                                                         
         DC    F'67655'                                                         
         DC    F'60250'                                                         
         DC    F'53280'                                                         
         DC    F'43555'                                                         
         DC    F'22200'                                                         
         DC    F'18505'                                                         
         DC    F'15125'                                                         
         SPACE 1                                                                
CBXTAB   DC    AL1(2,36,0,0)       CLASS B W/O NY                               
         DC    F'77160'                                                         
         DC    F'53590'                                                         
         DC    F'60250'                                                         
         DC    F'53280'                                                         
         DC    F'43555'                                                         
         DC    F'22200'                                                         
         DC    F'18505'                                                         
         DC    F'15125'                                                         
         SPACE 1                                                                
CLCTAB   DC    AL1(3,36,0,0)       CLASS C                                      
         DC    F'45980'                                                         
         DC    F'30655'                                                         
         DC    F'39850'                                                         
         DC    F'35415'                                                         
         DC    F'28960'                                                         
         DC    F'17660'                                                         
         DC    F'14695'                                                         
         DC    F'12055'                                                         
         SPACE 1                                                                
NWKTBL   DC    AL1(58),AL1(36,1,1) NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(22980)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(17280)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(16825)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(14895)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(12315)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(9740)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(8450)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(6895)           'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
         SPACE 3                                                                
DANTAB   DC    AL1(4,36,0,0)       DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    F'187635'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'130690'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'140890'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'124190'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'96525'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'57595'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'50440'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'36015'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL1(5,36,0,0)       DEALER A W/O NY                              
         DC    F'165940'                                                        
         DC    F'119850'                                                        
         DC    F'140890'                                                        
         DC    F'124190'                                                        
         DC    F'96525'                                                         
         DC    F'57595'                                                         
         DC    F'50440'                                                         
         DC    F'36015'                                                         
         SPACE 1                                                                
DBNTAB   DC    AL1(6,36,0,0)       CLASS B INCL NY                              
         DC    F'288500'                                                        
         DC    F'196315'                                                        
         DC    F'214205'                                                        
         DC    F'188825'                                                        
         DC    F'146960'                                                        
         DC    F'87745'                                                         
         DC    F'76795'                                                         
         DC    F'54780'                                                         
         SPACE 1                                                                
DBXTAB   DC    AL1(7,36,0,0)       CLASS B W/O NY                               
         DC    F'248920'                                                        
         DC    F'179495'                                                        
         DC    F'214205'                                                        
         DC    F'188825'                                                        
         DC    F'146960'                                                        
         DC    F'87745'                                                         
         DC    F'76795'                                                         
         DC    F'54780'                                                         
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
         SPACE 3                                                                
G13TAB   DC    AL1(8,36,1,1)       13 USE                                       
         DC    F'138105'          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS              
         DC    F'107120'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'115470'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'103190'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'84730'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'70435'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'61315'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'50175'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(8,4,2,13)       (2-13)                                       
         SPACE 1                                                                
         DC    AL1(8,36,14,18)     14-18                                        
         DC    F'9200'                                                          
         DC    F'6988'                                                          
         DC    F'6725'                                                          
         DC    F'5886'                                                          
         DC    F'4807'                                                          
         DC    F'4443'                                                          
         DC    F'4006'                                                          
         DC    F'3302'                                                          
         SPACE 1                                                                
         DC    AL1(8,36,19,255)    19+                                          
         DC    F'4665'                                                          
         DC    F'3465'                                                          
         DC    F'2900'                                                          
         DC    F'2465'                                                          
         DC    F'2000'                                                          
         DC    F'2100'                                                          
         DC    F'1970'                                                          
         DC    F'1635'                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
         SPACE 3                                                                
WSPTAB   DC    AL1(10,36,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    F'41425'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'31150'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'30325'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'26845'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'22205'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'17565'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'15245'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'12430'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(10,36,2,25)     UNITS 2-25                                   
         DC    F'1630'                                                          
         DC    F'1115'                                                          
         DC    F'1270'                                                          
         DC    F'1095'                                                          
         DC    F'895'                                                           
         DC    F'450'                                                           
         DC    F'355'                                                           
         DC    F'295'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,26,60)    UNITS 26-60                                  
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'655'                                                           
         DC    F'555'                                                           
         DC    F'460'                                                           
         DC    F'190'                                                           
         DC    F'130'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,61,125)   UNITS 61-125                                 
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,126,255)  UNITS 126+                                   
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'235'                                                           
         DC    F'190'                                                           
         DC    F'165'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL1(11,36,0,0)      NY ALONE                                     
         DC    F'93580'                                                         
         DC    F'66110'                                                         
         DC    F'59930'                                                         
         DC    F'53230'                                                         
         DC    F'43615'                                                         
         DC    F'24040'                                                         
         DC    F'19920'                                                         
         DC    F'16310'                                                         
         SPACE 1                                                                
         DC    AL1(11,36,1,35)     UNITS 1-35                                   
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'655'                                                           
         DC    F'555'                                                           
         DC    F'460'                                                           
         DC    F'190'                                                           
         DC    F'130'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(11,36,36,100)   UNITS 36-100                                 
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,101,255)  UNITS 101+                                   
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'235'                                                           
         DC    F'190'                                                           
         DC    F'165'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL1(12,36,0,0)      CHI OR LA ALONE                              
         DC    F'81565'                                                         
         DC    F'57525'                                                         
         DC    F'59930'                                                         
         DC    F'53230'                                                         
         DC    F'43615'                                                         
         DC    F'24040'                                                         
         DC    F'19920'                                                         
         DC    F'16310'                                                         
         SPACE 1                                                                
         DC    AL1(12,36,1,35)     UNITS 1-35                                   
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'655'                                                           
         DC    F'555'                                                           
         DC    F'460'                                                           
         DC    F'190'                                                           
         DC    F'130'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(12,36,36,100)   UNITS 36-100                                 
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,101,255)  UNITS 101+                                   
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'235'                                                           
         DC    F'190'                                                           
         DC    F'165'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL1(13,36,0,0)      TWO OF NY LA CHI                             
         DC    F'128780'                                                        
         DC    F'86710'                                                         
         DC    F'92205'                                                         
         DC    F'76240'                                                         
         DC    F'62330'                                                         
         DC    F'31770'                                                         
         DC    F'25590'                                                         
         DC    F'20955'                                                         
         SPACE 1                                                                
         DC    AL1(13,36,1,255)    UNITS 1+                                     
         DC    F'605'                                                           
         DC    F'475'                                                           
         DC    F'235'                                                           
         DC    F'190'                                                           
         DC    F'165'                                                           
         DC    F'115'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         SPACE 1                                                                
         DC    AL1(14,36,0,0)      ALL THREE MAJORS                             
         DC    F'155335'                                                        
         DC    F'110325'                                                        
         DC    F'116325'                                                        
         DC    F'99555'                                                         
         DC    F'81370'                                                         
         DC    F'38305'                                                         
         DC    F'30890'                                                         
         DC    F'25245'                                                         
         SPACE 1                                                                
         DC    AL1(14,36,1,255)    UNITS 1+                                     
         DC    F'620'                                                           
         DC    F'485'                                                           
         DC    F'240'                                                           
         DC    F'195'                                                           
         DC    F'170'                                                           
         DC    F'120'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL1(15,28,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    F'16330'            ANN ALONE                                    
         DC    F'16330'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'12040'            1-4M3,1-4S3,D3,S3                            
         DC    F'10655'            1-4M6,1-4S6,D6,S6                            
         DC    F'9460'             1-4M9,1-4S9,D9,S9                            
         DC    F'6155'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL1(15,24,2,25)     UNITS 2-25                                   
         DC    F'270'                                                           
         DC    F'270'                                                           
         DC    F'140'                                                           
         DC    F'120'                                                           
         DC    F'105'                                                           
         SPACE 1                                                                
         DC    AL1(15,24,26,60)    UNITS 26-60                                  
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'120'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,61,255)   UNITS 61+                                    
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'65'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(16,28,0,0)      NEW YORK ALONE                               
         DC    F'27405'                                                         
         DC    F'27405'                                                         
         DC    F'14905'                                                         
         DC    F'11570'                                                         
         DC    F'10300'                                                         
         DC    F'6155'                                                          
         SPACE 1                                                                
         DC    AL1(16,24,1,35)     UNITS 1-35                                   
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'85'                                                            
         SPACE 1                                                                
         DC    AL1(16,24,36,255)   UNITS 36+                                    
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'65'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(17,28,0,0)      CHICAGO OR LA ALONE                          
         DC    F'24850'                                                         
         DC    F'24850'                                                         
         DC    F'14905'                                                         
         DC    F'11570'                                                         
         DC    F'10300'                                                         
         DC    F'6155'                                                          
         SPACE 1                                                                
         DC    AL1(17,24,1,35)     UNITS 1-35                                   
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'120'                                                           
         DC    F'85'                                                            
         DC    F'85'                                                            
         SPACE 1                                                                
         DC    AL1(17,24,36,255)   UNITS 36+                                    
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'65'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(18,28,0,0)      ANY TWO ALONE                                
         DC    F'33425'                                                         
         DC    F'33425'                                                         
         DC    F'17800'                                                         
         DC    F'13655'                                                         
         DC    F'12155'                                                         
         DC    F'6155'                                                          
         SPACE 1                                                                
         DC    AL1(18,24,1,255)    UNITS 1+                                     
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'65'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(19,28,0,0)      ALL THREE ALONE                              
         DC    F'42230'                                                         
         DC    F'42230'                                                         
         DC    F'19825'                                                         
         DC    F'15345'                                                         
         DC    F'13655'                                                         
         DC    F'6155'                                                          
         SPACE 1                                                                
         DC    AL1(19,24,1,255)    UNITS 1+                                     
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'65'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL1(20,24,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(16330) ANN ALONE                                    
         DC    AL1(100),AL3(16330) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(12040) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(10655) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(9460)  1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL1(20,24,2,25)     UNITS 2-25                                   
         DC    AL1(80),AL3(270)                                                 
         DC    AL1(80),AL3(270)                                                 
         DC    AL1(95),AL3(140)                                                 
         DC    AL1(95),AL3(120)                                                 
         DC    AL1(95),AL3(105)                                                 
         SPACE 1                                                                
         DC    AL1(20,24,26,60)    UNITS 26-60                                  
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(120)                                                 
         DC    AL1(95),AL3(90)                                                  
         DC    AL1(95),AL3(90)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,61,255)   UNITS 61+                                    
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(21,24,0,0)      NEW YORK ALONE                               
         DC    AL1(80),AL3(27405)                                               
         DC    AL1(80),AL3(27405)                                               
         DC    AL1(95),AL3(14905)                                               
         DC    AL1(95),AL3(11570)                                               
         DC    AL1(95),AL3(10300)                                               
         SPACE 1                                                                
         DC    AL1(21,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(120)                                                 
         DC    AL1(95),AL3(85)                                                  
         DC    AL1(95),AL3(85)                                                  
         SPACE 1                                                                
         DC    AL1(21,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,0,0)      CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(24850)                                               
         DC    AL1(80),AL3(24850)                                               
         DC    AL1(95),AL3(14905)                                               
         DC    AL1(95),AL3(11570)                                               
         DC    AL1(95),AL3(10300)                                               
         SPACE 1                                                                
         DC    AL1(22,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(120)                                                 
         DC    AL1(95),AL3(85)                                                  
         DC    AL1(95),AL3(85)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(23,24,0,0)      ANY TWO ALONE                                
         DC    AL1(80),AL3(33425)                                               
         DC    AL1(80),AL3(33425)                                               
         DC    AL1(95),AL3(17800)                                               
         DC    AL1(95),AL3(13655)                                               
         DC    AL1(95),AL3(12155)                                               
         SPACE 1                                                                
         DC    AL1(23,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         SPACE 1                                                                
         DC    AL1(24,24,0,0)      ALL THREE ALONE                              
         DC    AL1(80),AL3(42230)                                               
         DC    AL1(80),AL3(42230)                                               
         DC    AL1(95),AL3(19825)                                               
         DC    AL1(95),AL3(15345)                                               
         DC    AL1(95),AL3(13655)                                               
         SPACE 1                                                                
         DC    AL1(24,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(80),AL3(200)                                                 
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
         SPACE 3                                                                
DLRTAB   DC    AL1(25,28,0,0)      DEALER COMMERCIALS                           
         DC    F'58290'            AR,AS,P,ANN                                  
         DC    F'46235'            S,1-4MS,1-4SS                                
         DC    F'30145'            1-4M3,1-4S3,D3,S3                            
         DC    F'24120'            1-4M6,1-4S6,D6,S6                            
         DC    F'15070'            1-4M9,1-4S9,D9,S9                            
         DC    F'15245'            SE                                           
         SPACE 1                                                                
N01TAB   DC    AL1(26,28,0,0)      NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    F'35755'            ANN ALONE                                    
         DC    F'35755'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'26825'            1-4M3,1-4S3,D3,S3                            
         DC    F'26825'            1-4M6,1-4S6,D6,S6                            
         DC    F'26825'            1-4M9,1-4S9,D9,S9                            
         DC    F'8015'             SE                                           
         SPACE 1                                                                
N04TAB   DC    AL1(27,28,0,0)      NETWORK 4 WEEK                               
         DC    F'83645'                                                         
         DC    F'58015'                                                         
         DC    F'44610'                                                         
         DC    F'39895'                                                         
         DC    F'36445'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
N08TAB   DC    AL1(28,28,0,0)      NETWORK 8 WEEK                               
         DC    F'92415'                                                         
         DC    F'92415'                                                         
         DC    F'71100'                                                         
         DC    F'63545'                                                         
         DC    F'56920'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
N13TAB   DC    AL1(29,28,0,0)      NETWORK 13 WEEK                              
         DC    F'114670'                                                        
         DC    F'114670'                                                        
         DC    F'88195'                                                         
         DC    F'78860'                                                         
         DC    F'72245'                                                         
         DC    F'8015'                                                          
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL1(30,28,0,0)      ACROSS-THE-BOARD                             
         DC    F'120070'                                                        
         DC    F'120070'                                                        
         DC    F'92335'                                                         
         DC    F'82570'                                                         
         DC    F'75650'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
U26TAB   DC    AL1(31,28,0,0)      26 USE LIMIT                                 
         DC    F'85670'                                                         
         DC    F'57340'                                                         
         DC    F'44090'                                                         
         DC    F'39430'                                                         
         DC    F'36025'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
U39TAB   DC    AL1(32,28,0,0)      39 USE LIMIT                                 
         DC    F'86350'                                                         
         DC    F'86350'                                                         
         DC    F'60460'                                                         
         DC    F'53965'                                                         
         DC    F'49035'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
R13TAB   DC    AL1(33,28,0,0)      REGIONAL - NO MAJORS                         
         DC    F'99465'                                                         
         DC    F'69195'                                                         
         DC    F'32435'                                                         
         DC    F'32435'                                                         
         DC    F'32435'                                                         
         DC    F'8015'                                                          
         SPACE 1                                                                
         DC    AL1(34,28,0,0)      REGIONAL - WITH ANY MAJORS                   
         DC    F'99465'                                                         
         DC    F'69195'                                                         
         DC    F'69195'                                                         
         DC    F'62280'                                                         
         DC    F'56010'                                                         
         DC    F'8015'                                                          
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'6450'             CAST=1                                       
         DC    F'6450'                  2-4                                     
         DC    F'6450'                  5+                                      
         SPACE 2                                                                
FMUTAB   DC    AL1(36,16,0,0)      FIRST REUSE                                  
         DC    F'4000'             CAST=1                                       
         DC    F'2000'                  2-4                                     
         DC    F'2000'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'8600'             CAST=1                                       
         DC    F'8600'                  2-4                                     
         DC    F'8600'                  5+                                      
         SPACE 3                                                                
MS8TAB   DC    AL1(09,16,0,0)      8-WEEK REUSE                                 
         DC    AL1(80),AL3(6450)                                                
         DC    AL1(80),AL3(6450)                                                
         DC    AL1(80),AL3(6450)                                                
         EJECT                                                                  
*              MUSIC FOREIGN USE                                                
         SPACE 2                                                                
FGMTAB   DC    AL1(45,16,0,0)      EUROPE OR OUTSIDE EUROPE-12M                 
         DC    F'5375'             CAST=1                                       
         DC    F'5375'                  2-4                                     
         DC    F'5375'                  5+                                      
         SPACE                                                                  
         DC    AL1(46,16,0,0)      WORLD - 12M                                  
         DC    F'8600'             CAST=1                                       
         DC    F'8600'                  2-4                                     
         DC    F'8600'                  5+                                      
         SPACE 3                                                                
         DC    AL1(47,16,0,0)      EUROPE OR OUTSIDE EUROPE-24M                 
         DC    F'8065'             CAST=1                                       
         DC    F'8065'                  2-4                                     
         DC    F'8065'                  5+                                      
         SPACE                                                                  
         DC    AL1(48,16,0,0)      WORLD - 24M                                  
         DC    F'12900'            CAST=1                                       
         DC    F'12900'                2-4                                      
         DC    F'12900'                5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 1                                                                
         DC    AL1(61,36,0,0)      AFT RADIO BASE SESSION RATES                 
         DC    F'16330'            ANN ALONE                                    
         DC    F'16330'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'12040'            1-4M3,1-4S3,D3,S3                            
         DC    F'10655'            1-4M6,1-4S6,D6,S6                            
         DC    F'9460'             1-4M9,1-4S9,D9,S9                            
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         SPACE 1                                                                
         DC    AL1(62,72,1,255)    NON-AFM TV BASE SESSION RATES                
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    F'17565'                                                         
         DC    F'15245'                                                         
         DC    F'12430'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    F'63805'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(64,72,1,255)    NON-AFM CABLE BASE SESSION RATES             
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    F'17565'                                                         
         DC    F'15245'                                                         
         DC    F'12430'                                                         
         DC    F'17835'                                                         
         DC    F'27065'                                                         
         DC    F'17835'                                                         
         DC    F'27065'                                                         
         DC    F'63850'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         EJECT                                                                  
         DC    AL1(63,52,0,0)      TV HOLDING RATES                             
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    5A(0)                                                            
         DC    F'13485'                                                         
         DC    F'25560'                                                         
         SPACE 1                                                                
         DC    AL1(37,52,0,0)      TV POSTPONEMENT FEE RATES                    
         DC    F'20713'                                                         
         DC    F'15575'                                                         
         DC    F'15163'                                                         
         DC    F'13423'                                                         
         DC    F'11103'                                                         
         DC    5A(0)                                                            
         DC    F'6743'                                                          
         DC    F'12780'                                                         
         SPACE 1                                                                
         DC    AL1(38,52,0,0)      REN - REINSTATEMENT                          
         DC    F'82850'                                                         
         DC    F'62300'                                                         
         DC    F'60650'                                                         
         DC    F'53690'                                                         
         DC    F'44410'                                                         
         DC    5A(0)                                                            
         DC    F'26970'                                                         
         DC    F'51120'                                                         
         EJECT                                                                  
*              CABLE RATES                                                      
         SPACE 3                                                                
         DC    AL1(41,36,1,1)      CBL & SCB - MINIMUM                          
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    F'17565'                                                         
         DC    F'15245'                                                         
         DC    F'12430'                                                         
         SPACE 1                                                                
         DC    AL1(41,4,2,120)     MINIMUM COVERS UPTO 120                      
         SPACE 1                                                                
         DC    AL1(41,36,121,121)                                               
         DC    F'76'                                                            
         DC    F'31'                                                            
         DC    F'51'                                                            
         DC    F'27'                                                            
         DC    F'16'                                                            
         DC    F'34'                                                            
         DC    F'18'                                                            
         DC    F'0'                                                             
         SPACE 1                                                                
         DC    AL1(41,36,122,122)                                               
         DC    F'281'                                                           
         DC    F'211'                                                           
         DC    F'206'                                                           
         DC    F'182'                                                           
         DC    F'151'                                                           
         DC    F'119'                                                           
         DC    F'103'                                                           
         DC    F'68'                                                            
         SPACE 1                                                                
         DC    AL1(41,36,123,150)                                               
         DC    F'281'                                                           
         DC    F'211'                                                           
         DC    F'206'                                                           
         DC    F'182'                                                           
         DC    F'151'                                                           
         DC    F'119'                                                           
         DC    F'103'                                                           
         DC    F'84'                                                            
         SPACE 1                                                                
         DC    AL1(41,36,151,172)  MAX IS 172 EFF 8/7/92                        
         DC    F'231'                                                           
         DC    F'174'                                                           
         DC    F'169'                                                           
         DC    F'150'                                                           
         DC    F'124'                                                           
         DC    F'98'                                                            
         DC    F'85'                                                            
         DC    F'69'                                                            
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
         SPACE 1                                                                
         DC    AL1(42,76,1,255)     DEM (TV)                                    
         DC    F'31150'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'15575'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'22740'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'20130'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'16650'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'8500'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'8500'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'8500'             'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2A(0)               N/D                                          
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    3A(0)               N/D                                          
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         DC    F'13000'            'OFF' S                                      
         SPACE 1                                                                
         DC    AL1(43,40,1,255)    DEM (AFT RADIO)                              
         DC    F'11265'            ANN ALONE                                    
         DC    F'11265'            AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'8500'             1-4M3,1-4S3,D3,S3                            
         DC    F'8500'             1-4M6,1-4S6,D6,S6                            
         DC    F'8500'             1-4M9,1-4S9,D9,S9                            
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         DC    F'13000'            SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL1(39,28,0,0)      LOCAL 13 WEEK - RADIO                        
         DC    F'21500'                                                         
         DC    F'21500'                                                         
         DC    F'21500'                                                         
         DC    F'21500'                                                         
         DC    F'21500'                                                         
         DC    F'8015'                                                          
         EJECT                                                                  
*              FOREIGN REUSE                                                    
         SPACE                                                                  
         DC    AL1(50,72,0,0)      UK                                           
         DC    F'124275'                                                        
         DC    F'93450'                                                         
         DC    F'90960'                                                         
         DC    F'80520'                                                         
         DC    F'66615'                                                         
         DC    F'52695'                                                         
         DC    F'45735'                                                         
         DC    F'37290'                                                         
         DC    F'69705'                                                         
         DC    F'106365'                                                        
         DC    F'40455'                                                         
         DC    F'70740'                                                         
         DC    F'191415'           PIL                                          
         DC    F'0'                N/D                                          
         DC    F'65760'            SE                                           
         DC    F'19920'            C3,C6                                        
         DC    F'39300'            C9                                           
         SPACE                                                                  
         DC    AL1(51,72,0,0)      EUROPE W/O UK                                
         DC    F'82850'                                                         
         DC    F'62300'                                                         
         DC    F'60650'                                                         
         DC    F'53690'                                                         
         DC    F'44410'                                                         
         DC    F'35130'                                                         
         DC    F'30490'                                                         
         DC    F'24860'                                                         
         DC    F'46470'                                                         
         DC    F'70910'                                                         
         DC    F'26970'                                                         
         DC    F'47160'                                                         
         DC    F'127610'           PIL                                          
         DC    F'0'                N/D                                          
         DC    F'43840'            SE                                           
         DC    F'13280'            C3,C6                                        
         DC    F'26200'            C9                                           
         SPACE                                                                  
         DC    AL1(49,24,0,0)      RADIO                                        
         DC    F'0'                N/D                                          
         DC    F'42760'            P,ANN,S,D,ACR                                
         DC    F'24805'            3-5 GROUP                                    
         DC    F'17105'            6-8 GROUP                                    
         DC    F'13685'            9+                                           
         EJECT                                                                  
         DC    AL1(52,24,0,0)      PBS RADIO                                    
         DC    F'48765'            N/D                                          
         DC    F'50635'            P,ANN,S,D,ACR                                
         DC    F'33015'            3-5 GROUP                                    
         DC    F'26410'            6-8 GROUP                                    
         DC    F'16515'            9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
         SPACE                                                                  
SNTTBL   DC    AL1(40,36,0,0)      NETWORK                                      
         DC    F'103695'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'77980'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'75910'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'67190'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'55575'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'44055'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'38160'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'31110'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
SNWTBL   DC    AL1(40,36,1,255)    NETWK/WILDSPT COMBINED (UNITS 1-255)         
         DC    F'275'                                                           
         DC    F'200'                                                           
         DC    F'195'                                                           
         DC    F'180'                                                           
         DC    F'140'                                                           
         DC    F'115'                                                           
         DC    F'105'                                                           
         DC    F'75'                                                            
         EJECT                                                                  
*              ADDENDUM USES                                                    
         SPACE 1                                                                
ADTTBL   DC    AL1(65,80,0,0)      TV SESSION RATES - 3 DAY - GA                
         DC    F'25200'            ON CAMERA                                    
         DC    F'19000'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'25200'                                                         
         DC    F'25200'                                                         
         DC    F'25200'                                                         
         DC    F'25200'                                                         
         DC    F'25200'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         DC    F'26900'            SOLO/DUO ON CAM                              
         DC    F'20200'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(70,80,0,0)      1 WEEK - GA                                  
         DC    F'27000'            ON CAMERA                                    
         DC    F'20300'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'27000'                                                         
         DC    F'27000'                                                         
         DC    F'27000'                                                         
         DC    F'27000'                                                         
         DC    F'27000'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'                                                         
         DC    F'6640'                                                          
         DC    F'13100'                                                         
         DC    F'26900'            SOLO/DUO ON CAM                              
         DC    F'20200'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(71,72,0,0)      TV SESSION RATES - 1 WEEK - KS               
         DC    F'21180'            ON CAMERA                                    
         DC    F'15905'            OFF                                          
         DC    F'16385'                                                         
         DC    F'14010'                                                         
         DC    F'11185'                                                         
         DC    F'7120'                                                          
         DC    F'5765'                                                          
         DC    F'4180'                                                          
         DC    F'13120'                                                         
         DC    F'18080'                                                         
         DC    F'7645'                                                          
         DC    F'11985'                                                         
         DC    F'21180'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(80,80,0,0)      4 WEEK - GA                                  
         DC    F'28800'            ON CAMERA                                    
         DC    F'21700'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'28800'                                                         
         DC    F'28800'                                                         
         DC    F'28800'                                                         
         DC    F'28800'                                                         
         DC    F'28800'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'                                                         
         DC    F'6640'                                                          
         DC    F'13100'                                                         
         DC    F'26900'            SOLO/DUO ON CAM                              
         DC    F'20200'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(81,72,0,0)      31 DAY - KS                                  
         DC    F'27150'            ON CAMERA                                    
         DC    F'20390'            OFF                                          
         DC    F'20225'                                                         
         DC    F'17180'                                                         
         DC    F'13675'                                                         
         DC    F'8930'                                                          
         DC    F'7010'                                                          
         DC    F'5310'                                                          
         DC    F'13120'                                                         
         DC    F'18080'                                                         
         DC    F'7645'                                                          
         DC    F'11985'                                                         
         DC    F'27150'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(90,80,0,0)      13 WEEK - GA                                 
         DC    F'36000'            ON CAMERA                                    
         DC    F'27100'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'36000'                                                         
         DC    F'36000'                                                         
         DC    F'36000'                                                         
         DC    F'36000'                                                         
         DC    F'36000'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'                                                         
         DC    F'6640'                                                          
         DC    F'13100'                                                         
         DC    F'26900'            SOLO/DUO ON CAM                              
         DC    F'20200'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(91,72,0,0)      13 WEEKS - KS                                
         DC    F'33110'            ON CAMERA                                    
         DC    F'24860'            OFF                                          
         DC    F'23960'                                                         
         DC    F'20455'                                                         
         DC    F'16385'                                                         
         DC    F'10510'                                                         
         DC    F'8475'                                                          
         DC    F'6330'                                                          
         DC    F'13120'                                                         
         DC    F'18080'                                                         
         DC    F'7645'                                                          
         DC    F'11985'                                                         
         DC    F'33110'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(92,72,0,0)      13 WEEKS - TX                                
         DC    F'31600'            ON CAMERA                                    
         DC    F'22500'            OFF                                          
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         DC    F'12200'                                                         
         DC    F'12200'                                                         
         DC    F'12200'                                                         
         DC    F'17072'                                                         
         DC    F'23682'                                                         
         DC    F'9913'                                                          
         DC    F'15751'                                                         
         DC    F'31600'                                                         
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 3                                                                
ADOTBL   DC    AL1(100,40,0,0)     RADIO SESSION RATES - 3 DAY - GA             
         DC    F'9900'                                                          
         DC    F'9900'                                                          
         DC    F'11700'                                                         
         DC    F'10350'                                                         
         DC    F'9150'                                                          
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         DC    F'15900'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(105,40,0,0)     1 WEEK - GA                                  
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'11700'                                                         
         DC    F'10350'                                                         
         DC    F'9150'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         DC    F'15900'                                                         
         SPACE 1                                                                
         DC    AL1(106,36,0,0)     1 WEEK - KS                                  
         DC    F'8095'                                                          
         DC    F'8095'                                                          
         DC    F'5060'                                                          
         DC    F'4255'                                                          
         DC    F'3795'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         SPACE 1                                                                
         DC    AL1(115,40,0,0)     4 WEEK - GA                                  
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'11700'                                                         
         DC    F'10350'                                                         
         DC    F'9150'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         DC    F'15900'                                                         
         SPACE 1                                                                
         DC    AL1(116,36,0,0)     31 DAY - KS                                  
         DC    F'10375'                                                         
         DC    F'10375'                                                         
         DC    F'6095'                                                          
         DC    F'5405'                                                          
         DC    F'4830'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         SPACE 1                                                                
         DC    AL1(125,40,0,0)     13 WEEK - GA                                 
         DC    F'14200'                                                         
         DC    F'14200'                                                         
         DC    F'11700'                                                         
         DC    F'10350'                                                         
         DC    F'9150'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         DC    F'15900'                                                         
         SPACE 1                                                                
         DC    AL1(126,36,0,0)     13 WEEK - KS                                 
         DC    F'12650'                                                         
         DC    F'12650'                                                         
         DC    F'7130'                                                          
         DC    F'6325'                                                          
         DC    F'5635'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         SPACE 1                                                                
         DC    AL1(127,36,0,0)     13 WEEK - TX                                 
         DC    F'12900'                                                         
         DC    F'12900'                                                         
         DC    F'8200'                                                          
         DC    F'8200'                                                          
         DC    F'8200'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         SPACE 3                                                                
ADHTAB   DC    AL1(225,76,0,0)      ADDENDUM HOLDING RATES - GA                 
         DC    F'36000'                                                         
         DC    F'27100'                                                         
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    12A(0)                                                           
         DC    F'26900'                                                         
         SPACE 1                                                                
         DC    AL1(226,24,0,0)      ADDENDUM HOLDING RATES - KS                 
         DC    F'33110'             ON CAMERA                                   
         DC    F'24860'             OFF                                         
         DC    F'23960'                                                         
         DC    F'20455'                                                         
         DC    F'16385'                                                         
         SPACE 1                                                                
         DC    AL1(227,24,0,0)      ADDENDUM HOLDING RATES - TX                 
         DC    F'31600'             ON CAMERA                                   
         DC    F'22500'             OFF                                         
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         SPACE 3                                                                
ADDTAB   DC    AL1(205,72,0,0)      ADDENDUM DEMO (TV) - GA                     
         DC    F'27100'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'13600'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'27100'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'27100'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'27100'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'13600'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'13600'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'13600'            'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7A(0)               N/D                                          
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(206,72,0,0)      ADDENDUM DEMO (TV) - KS                     
         DC    F'6670'             'ON'                                         
         DC    F'5765'             'OFF'                                        
         DC    F'6670'                                                          
         DC    F'6670'                                                          
         DC    F'6670'                                                          
         DC    F'5765'                                                          
         DC    F'5765'                                                          
         DC    F'5765'                                                          
         DC    7A(0)               N/D                                          
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE 1                                                                
         DC    AL1(207,80,0,0)      ADDENDUM DEMO (TV) - TX                     
         DC    F'20900'            'ON'                                         
         DC    F'10300'            'OFF'                                        
         DC    F'10000'                                                         
         DC    F'10000'                                                         
         DC    F'10000'                                                         
         DC    F'4600'                                                          
         DC    F'4600'                                                          
         DC    F'4600'                                                          
         DC    7A(0)               N/D                                          
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         DC    F'20900'            SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    F'7400'             SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(215,36,0,0)     ADDENDUM DEMO (AFT RADIO) - GA               
         DC    F'9800'             ANN ALONE                                    
         DC    F'9800'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'9800'             1-4M3,1-4S3,D3,S3                            
         DC    F'9800'             1-4M6,1-4S6,D6,S6                            
         DC    F'9800'             1-4M9,1-4S9,D9,S9                            
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         SPACE 1                                                                
         DC    AL1(216,36,0,0)     ADDENDUM DEMO (AFT RADIO) - KS               
         DC    F'4715'             ANN ALONE                                    
         DC    F'4715'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'4715'             1-4M3,1-4S3,D3,S3                            
         DC    F'4715'             1-4M6,1-4S6,D6,S6                            
         DC    F'4715'             1-4M9,1-4S9,D9,S9                            
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         SPACE 1                                                                
         DC    AL1(217,40,0,0)     ADDENDUM DEMO (AFT RADIO) - TX               
         DC    F'9100'             ANN ALONE                                    
         DC    F'9100'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'4600'             1-4M3,1-4S3,D3,S3                            
         DC    F'4600'             1-4M6,1-4S6,D6,S6                            
         DC    F'4600'             1-4M9,1-4S9,D9,S9                            
         DC    F'12565'            SE                                           
         DC    F'5610'             C3,C6                                        
         DC    F'8975'             C9                                           
         DC    F'7400'             SOLO/DUO                                     
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
         SPACE 3                                                                
ADWTAB   DC    AL1(135,44,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'25200'            ON CAMERA                                    
         DC    F'19000'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'26900'            SOLO/DUO ON CAMERA                           
         DC    F'20200'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(135,44,2,25)    UNITS 2-25                                   
         DC    F'970'                                                           
         DC    F'665'                                                           
         DC    F'825'                                                           
         DC    F'710'                                                           
         DC    F'580'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'1060'                                                          
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(135,44,26,60)    UNITS 26-60                                 
         DC    F'360'                                                           
         DC    F'285'                                                           
         DC    F'425'                                                           
         DC    F'360'                                                           
         DC    F'300'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'395'                                                           
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(135,44,61,255)   UNITS 61+                                   
         DC    F'360'                                                           
         DC    F'285'                                                           
         DC    F'310'                                                           
         DC    F'240'                                                           
         DC    F'200'                                                           
         DC    F'75'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         DC    F'395'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'27000'            ON CAMERA                                    
         DC    F'20300'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'26900'            SOLO/DUO ON CAMERA                           
         DC    F'20200'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(140,44,2,25)    UNITS 2-25                                   
         DC    F'1040'                                                          
         DC    F'715'                                                           
         DC    F'825'                                                           
         DC    F'710'                                                           
         DC    F'580'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'1060'                                                          
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,26,60)    UNITS 26-60                                 
         DC    F'385'                                                           
         DC    F'305'                                                           
         DC    F'425'                                                           
         DC    F'360'                                                           
         DC    F'300'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'395'                                                           
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,61,255)    UNITS 61+                                  
         DC    F'385'                                                           
         DC    F'305'                                                           
         DC    F'310'                                                           
         DC    F'240'                                                           
         DC    F'200'                                                           
         DC    F'75'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         DC    F'395'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(141,36,1,1)     1 WEEK - KS - UNIT 1                         
         DC    F'21180'            ON CAMERA                                    
         DC    F'15905'            OFF                                          
         DC    F'16385'                                                         
         DC    F'14010'                                                         
         DC    F'11185'                                                         
         DC    F'7120'                                                          
         DC    F'5765'                                                          
         DC    F'4180'                                                          
         SPACE 1                                                                
         DC    AL1(141,36,2,255)   UNITS 2+                                     
         DC    F'870'                                                           
         DC    F'870'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'28800'            ON CAMERA                                    
         DC    F'21700'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'26900'            SOLO/DUO ON CAMERA                           
         DC    F'20200'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(150,44,2,25)    UNITS 2-25                                   
         DC    F'1110'                                                          
         DC    F'760'                                                           
         DC    F'825'                                                           
         DC    F'710'                                                           
         DC    F'580'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'1060'                                                          
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,26,60)    UNITS 26-60                                 
         DC    F'410'                                                           
         DC    F'325'                                                           
         DC    F'425'                                                           
         DC    F'360'                                                           
         DC    F'300'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'395'                                                           
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,61,255)   UNITS 61+                                   
         DC    F'410'                                                           
         DC    F'325'                                                           
         DC    F'310'                                                           
         DC    F'240'                                                           
         DC    F'200'                                                           
         DC    F'75'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         DC    F'395'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(151,36,1,1)     31 DAY - KS - UNIT 1                         
         DC    F'27150'            ON CAMERA                                    
         DC    F'20390'            OFF                                          
         DC    F'20225'                                                         
         DC    F'17180'                                                         
         DC    F'13675'                                                         
         DC    F'8930'                                                          
         DC    F'7010'                                                          
         DC    F'5310'                                                          
         SPACE 1                                                                
         DC    AL1(151,36,2,255)   UNITS 2+                                     
         DC    F'870'                                                           
         DC    F'870'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'36000'            ON CAMERA                                    
         DC    F'27100'            OFF                                          
         DC    F'19700'                                                         
         DC    F'17400'                                                         
         DC    F'14400'                                                         
         DC    F'11400'                                                         
         DC    F'9900'                                                          
         DC    F'8100'                                                          
         DC    F'26900'            SOLO/DUO ON CAMERA                           
         DC    F'20200'            OFF                                          
         SPACE 1                                                                
         DC    AL1(160,44,2,25)    UNITS 2-25                                   
         DC    F'1385'                                                          
         DC    F'950'                                                           
         DC    F'825'                                                           
         DC    F'710'                                                           
         DC    F'580'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'1060'                                                          
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,26,60)    UNITS 26-60                                 
         DC    F'515'                                                           
         DC    F'405'                                                           
         DC    F'425'                                                           
         DC    F'360'                                                           
         DC    F'300'                                                           
         DC    F'295'                                                           
         DC    F'230'                                                           
         DC    F'190'                                                           
         DC    F'395'                                                           
         DC    F'725'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,61,255)    UNITS 61+                                  
         DC    F'515'                                                           
         DC    F'405'                                                           
         DC    F'310'                                                           
         DC    F'240'                                                           
         DC    F'200'                                                           
         DC    F'75'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         DC    F'395'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(161,36,1,1)      13 WEEKS - KS - UNIT 1                      
         DC    F'33110'             ON CAMERA                                   
         DC    F'24860'             OFF                                         
         DC    F'23960'                                                         
         DC    F'20455'                                                         
         DC    F'16385'                                                         
         DC    F'10510'                                                         
         DC    F'8475'                                                          
         DC    F'6330'                                                          
         SPACE 1                                                                
         DC    AL1(161,36,2,255)    UNITS 2+                                    
         DC    F'870'                                                           
         DC    F'870'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(162,36,1,1)      13 WEEKS - TX - UNIT 1                      
         DC    F'31600'             ON CAMERA                                   
         DC    F'22500'             OFF                                         
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         DC    F'21200'                                                         
         DC    F'12200'                                                         
         DC    F'12200'                                                         
         DC    F'12200'                                                         
         SPACE 1                                                                
         DC    AL1(162,36,2,11)     UNITS 2-11                                  
         DC    F'675'                                                           
         DC    F'495'                                                           
         DC    F'275'                                                           
         DC    F'275'                                                           
         DC    F'275'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(162,36,12,255)   UNITS 12+                                   
         DC    F'460'                                                           
         DC    F'330'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'220'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
         SPACE 3                                                                
         DC    AL1(170,32,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'9900'             ANN ALONE                                    
         DC    F'9900'             AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'11700'            1-4M3,1-4S3,D3,S3                            
         DC    F'10350'            1-4M6,1-4S6,D6,S6                            
         DC    F'9150'             1-4M9,1-4S9,D9,S9                            
         DC    F'9900'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'15900'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(170,32,2,25)    UNITS 2-25                                   
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'135'                                                           
         DC    F'120'                                                           
         DC    F'105'                                                           
         DC    F'0'                                                             
         DC    F'265'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,26,60)   UNITS 26-60                                  
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,61,255)  UNITS 61+                                    
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'60'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'10700'            ANN ALONE                                    
         DC    F'10700'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'11700'            1-4M3,1-4S3,D3,S3                            
         DC    F'10350'            1-4M6,1-4S6,D6,S6                            
         DC    F'9150'             1-4M9,1-4S9,D9,S9                            
         DC    F'10700'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'15900'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(175,32,2,25)    UNITS 2-25                                   
         DC    F'175'                                                           
         DC    F'175'                                                           
         DC    F'135'                                                           
         DC    F'120'                                                           
         DC    F'105'                                                           
         DC    F'0'                                                             
         DC    F'265'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,26,60)   UNITS 26-60                                  
         DC    F'130'                                                           
         DC    F'130'                                                           
         DC    F'120'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,61,255)  UNITS 61+                                    
         DC    F'130'                                                           
         DC    F'130'                                                           
         DC    F'60'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(176,28,0,0)     1 WEEK - KS - UNIT 1                         
         DC    F'8095'                                                          
         DC    F'8095'                                                          
         DC    F'5060'                                                          
         DC    F'4255'                                                          
         DC    F'3795'                                                          
         DC    F'8095'                                                          
         SPACE 1                                                                
         DC    AL1(176,28,2,255)   UNITS 2+                                     
         DC    F'270'                                                           
         DC    F'270'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'270'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'11400'            ANN ALONE                                    
         DC    F'11400'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'11700'            1-4M3,1-4S3,D3,S3                            
         DC    F'10350'            1-4M6,1-4S6,D6,S6                            
         DC    F'9150'             1-4M9,1-4S9,D9,S9                            
         DC    F'11400'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'15900'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(185,32,2,25)    UNITS 2-25                                   
         DC    F'185'                                                           
         DC    F'185'                                                           
         DC    F'135'                                                           
         DC    F'120'                                                           
         DC    F'105'                                                           
         DC    F'0'                                                             
         DC    F'265'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,26,60)   UNITS 26-60                                  
         DC    F'135'                                                           
         DC    F'135'                                                           
         DC    F'120'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,61,255)  UNITS 61+                                    
         DC    F'135'                                                           
         DC    F'135'                                                           
         DC    F'60'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(186,28,0,0)     31 DAY - KS - UNIT 1                         
         DC    F'10375'                                                         
         DC    F'10375'                                                         
         DC    F'6095'                                                          
         DC    F'5405'                                                          
         DC    F'4830'                                                          
         DC    F'10375'                                                         
         SPACE 1                                                                
         DC    AL1(186,28,2,255)   UNITS 2+                                     
         DC    F'270'                                                           
         DC    F'270'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'270'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'14200'            ANN ALONE                                    
         DC    F'14200'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'11700'            1-4M3,1-4S3,D3,S3                            
         DC    F'10350'            1-4M6,1-4S6,D6,S6                            
         DC    F'9150'             1-4M9,1-4S9,D9,S9                            
         DC    F'14200'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'15900'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(195,32,2,25)    UNITS 2-25                                   
         DC    F'230'                                                           
         DC    F'230'                                                           
         DC    F'135'                                                           
         DC    F'120'                                                           
         DC    F'105'                                                           
         DC    F'0'                                                             
         DC    F'265'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,26,60)   UNITS 26-60                                  
         DC    F'170'                                                           
         DC    F'170'                                                           
         DC    F'120'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,61,255)  UNITS 61+                                    
         DC    F'170'                                                           
         DC    F'170'                                                           
         DC    F'60'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         DC    F'0'                                                             
         DC    F'195'                                                           
         SPACE 1                                                                
         DC    AL1(196,28,0,0)     13 WEEK - KS - UNIT 1                        
         DC    F'12650'                                                         
         DC    F'12650'                                                         
         DC    F'7130'                                                          
         DC    F'6325'                                                          
         DC    F'5635'                                                          
         DC    F'12650'                                                         
         SPACE 1                                                                
         DC    AL1(196,28,2,255)   UNITS 2+                                     
         DC    F'270'                                                           
         DC    F'270'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'270'                                                           
         SPACE 1                                                                
         DC    AL1(197,28,0,0)     13 WEEK - TX - UNIT 1                        
         DC    F'12900'                                                         
         DC    F'12900'                                                         
         DC    F'8200'                                                          
         DC    F'8200'                                                          
         DC    F'8200'                                                          
         DC    F'12900'                                                         
         SPACE 1                                                                
         DC    AL1(197,28,2,255)   UNITS 2+                                     
         DC    F'110'                                                           
         DC    F'110'                                                           
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'110'                                                           
         EJECT                                                                  
*&&DO                                                                           
*              INSERTS FOR BOOKENDS                                             
         SPACE 3                                                                
IFBTAB   DC    AL1(44,36,1,1)      UNIT 1                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         SPACE                                                                  
         DC    AL1(44,36,2,3)      UNITS 2,3                                    
         DC    AL1(50),AL3(12225)                                               
         DC    AL1(50),AL3(9260)                                                
         DC    AL1(50),AL3(12225)                                               
         DC    AL1(50),AL3(12225)                                               
         DC    AL1(50),AL3(12225)                                               
         DC    AL1(50),AL3(9260)                                                
         DC    AL1(50),AL3(9260)                                                
         DC    AL1(50),AL3(9260)                                                
         SPACE                                                                  
         DC    AL1(44,36,4,13)     UNITS 4-13                                   
         DC    AL1(25),AL3(12225)                                               
         DC    AL1(25),AL3(9260)                                                
         DC    AL1(25),AL3(12225)                                               
         DC    AL1(25),AL3(12225)                                               
         DC    AL1(25),AL3(12225)                                               
         DC    AL1(25),AL3(9260)                                                
         DC    AL1(25),AL3(9260)                                                
         DC    AL1(25),AL3(9260)                                                
         SPACE                                                                  
         DC    AL1(44,36,14,255)   UNITS 14+                                    
         DC    AL1(15),AL3(12225)                                               
         DC    AL1(15),AL3(9260)                                                
         DC    AL1(15),AL3(12225)                                               
         DC    AL1(15),AL3(12225)                                               
         DC    AL1(15),AL3(12225)                                               
         DC    AL1(15),AL3(9260)                                                
         DC    AL1(15),AL3(9260)                                                
         DC    AL1(15),AL3(9260)                                                
*&&                                                                             
         EJECT                                                                  
         DC    AL1(53,72,1,255)    TV TAGS - REGULAR                            
         DC    F'12225'                                                         
         DC    F'9260'                                                          
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'0'                N/D                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         SPACE 1                                                                
         DC    AL1(54,72,1,1)      TV TAGS - W/1 SESS FEE                       
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    F'17565'                                                         
         DC    F'15245'                                                         
         DC    F'12430'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    F'63805'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'24090'            SE                                           
         DC    F'6640'             C3,C6                                        
         DC    F'13100'            C9                                           
         SPACE                                                                  
         DC    AL1(54,72,2,255)                                                 
         DC    F'12225'                                                         
         DC    F'9260'                                                          
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'12225'                                                         
         DC    F'0'                N/D                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         DC    F'9260'                                                          
         SPACE 1                                                                
         DC    AL1(55,36,1,255)    AFT RADIO TAGS - REGULAR                     
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         SPACE 1                                                                
         DC    AL1(56,36,1,1)      AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    F'16330'                                                         
         DC    F'16330'                                                         
         DC    F'12040'                                                         
         DC    F'10655'                                                         
         DC    F'9460'                                                          
         DC    F'12565'                                                         
         DC    F'5610'                                                          
         DC    F'8975'                                                          
         SPACE                                                                  
         DC    AL1(56,36,2,255)                                                 
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         DC    F'6760'                                                          
         EJECT                                                                  
INRUNLTB DC    AL1(236,36,0,0)     THEAT/INDUST REUSE-TV UNLIMITED USE          
         DC    AL1(160),AL3(41425)                                              
         DC    AL1(160),AL3(31150)                                              
         DC    AL1(160),AL3(30325)                                              
         DC    AL1(160),AL3(26845)                                              
         DC    AL1(160),AL3(22205)                                              
         DC    AL1(160),AL3(17565)                                              
         DC    AL1(160),AL3(15245)                                              
         DC    AL1(160),AL3(12430)                                              
         SPACE 3                                                                
         DC    AL1(235,24,0,0)     THEAT/INDUST REUSE-RAD UNLIMITED USE         
         DC    AL1(160),AL3(16330)                                              
         DC    AL1(160),AL3(16330)                                              
         DC    AL1(160),AL3(12040)                                              
         DC    AL1(160),AL3(10655)                                              
         DC    AL1(160),AL3(9460)                                               
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
         SPACE 3                                                                
LCBTAB   DC    AL1(238,36,0,0)      1-50,000 SUBSCRIBERS                        
         DC    F'1710'                                                          
         DC    F'1170'                                                          
         DC    F'1335'                                                          
         DC    F'1150'                                                          
         DC    F'935'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(239,36,0,0)      50,001-100,000 SUBSCRIBERS                  
         DC    F'3425'                                                          
         DC    F'2345'                                                          
         DC    F'2670'                                                          
         DC    F'2300'                                                          
         DC    F'1875'                                                          
         DC    F'950'                                                           
         DC    F'740'                                                           
         DC    F'620'                                                           
         SPACE 1                                                                
         DC    AL1(240,36,0,0)      100,001-150,000 SUBSCRIBERS                 
         DC    F'5135'                                                          
         DC    F'3515'                                                          
         DC    F'4005'                                                          
         DC    F'3450'                                                          
         DC    F'2810'                                                          
         DC    F'1420'                                                          
         DC    F'1110'                                                          
         DC    F'930'                                                           
         SPACE 1                                                                
         DC    AL1(241,36,0,0)      150,001-200,000 SUBSCRIBERS                 
         DC    F'6845'                                                          
         DC    F'4685'                                                          
         DC    F'5340'                                                          
         DC    F'4600'                                                          
         DC    F'3750'                                                          
         DC    F'1900'                                                          
         DC    F'1480'                                                          
         DC    F'1245'                                                          
         SPACE 1                                                                
         DC    AL1(242,36,0,0)      200,001-250,000 SUBSCRIBERS                 
         DC    F'8555'                                                          
         DC    F'5860'                                                          
         DC    F'6675'                                                          
         DC    F'5750'                                                          
         DC    F'4685'                                                          
         DC    F'2370'                                                          
         DC    F'1855'                                                          
         DC    F'1555'                                                          
         SPACE 1                                                                
         DC    AL1(243,36,0,0)      250,001-500,000 SUBSCRIBERS                 
         DC    F'17115'                                                         
         DC    F'11720'                                                         
         DC    F'13355'                                                         
         DC    F'11500'                                                         
         DC    F'9375'                                                          
         DC    F'4740'                                                          
         DC    F'3705'                                                          
         DC    F'3105'                                                          
         SPACE 1                                                                
         DC    AL1(244,36,0,0)      500,001-750,000 SUBSCRIBERS                 
         DC    F'25670'                                                         
         DC    F'17575'                                                         
         DC    F'20030'                                                         
         DC    F'17250'                                                         
         DC    F'14060'                                                         
         DC    F'7110'                                                          
         DC    F'5560'                                                          
         DC    F'4660'                                                          
         SPACE 1                                                                
         DC    AL1(245,36,0,0)      750,001-1 MILLION SUBSCRIBERS               
         DC    F'34225'                                                         
         DC    F'23435'                                                         
         DC    F'26705'                                                         
         DC    F'13000'                                                         
         DC    F'18750'                                                         
         DC    F'9485'                                                          
         DC    F'7410'                                                          
         DC    F'6215'                                                          
         SPACE 1                                                                
         DC    AL1(246,36,0,0)      OVER 1 MILLION SUBSCRIBERS                  
         DC    F'41425'                                                         
         DC    F'31150'                                                         
         DC    F'30325'                                                         
         DC    F'26845'                                                         
         DC    F'22205'                                                         
         DC    F'17565'                                                         
         DC    F'15245'                                                         
         DC    F'12430'                                                         
         SPACE 1                                                                
         DC    AL1(237,36,1,2)      LCB MAJORS - LA, CHI, LA+CHI                
         DC    F'1710'                                                          
         DC    F'1170'                                                          
         DC    F'1335'                                                          
         DC    F'1150'                                                          
         DC    F'935'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         SPACE 1                                                                
         DC    AL1(237,36,3,3)      LCB MAJORS - NY                             
         DC    F'3425'                                                          
         DC    F'2345'                                                          
         DC    F'2670'                                                          
         DC    F'2300'                                                          
         DC    F'1880'                                                          
         DC    F'945'                                                           
         DC    F'740'                                                           
         DC    F'625'                                                           
         SPACE 1                                                                
         DC    AL1(237,36,4,5)      LCB MAJORS - NY+CHI, NY+LA, ALL 3           
         DC    F'1710'                                                          
         DC    F'1170'                                                          
         DC    F'1335'                                                          
         DC    F'1150'                                                          
         DC    F'935'                                                           
         DC    F'475'                                                           
         DC    F'370'                                                           
         DC    F'310'                                                           
         SPACE 3                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(60,UBSM,ALL,AFM+NON,0,0,0,ALL)         SESSION               
         DC    AL1(61,UBSR,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(61,URRR,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UBSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,URRS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USRS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USFS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UDWN,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UFGS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(61,UCNL,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UCNL,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(64,UCNL,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(64,UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(64,URRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(64,USRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(37,UPPF,ALL,ALL-AFM,0,0,0,TV)                                
*                                                                               
         DC    AL1(61,ULFT,ALL,AFT+NON,0,0,0,RADIO)   LIFT                      
         DC    AL1(62,ULFT,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(64,ULFT,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
*                                                                               
         DC    AL1(61,USLF,ALL,AFT+NON,0,0,0,RADIO)   SPAN LIFT                 
         DC    AL1(62,USLF,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(64,USLF,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
*                                                                               
         DC    AL1(42,UDEM,ALL,ALL,0,0,0,TV)          DEMO                      
         DC    AL1(43,UDEM,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(42,USNA,ALL,ALL,0,0,0,TV)          SPANISH DEMO              
         DC    AL1(43,USNA,ALL,AFT+NON,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(63,UHLD,ALL,ALL-AFM,0,0,0,TV)      HLD FEE                   
         DC    AL1(63,USHL,ALL,ALL-AFM,0,0,0,TV)      SPAN HLD FEE              
         DC    AL1(38,UREN,ALL,ALL-AFM,0,0,0,TV)      REINSTATEMENT             
         DC    AL1(38,USRE,ALL,ALL-AFM,0,0,0,TV)      REINSTATEMENT             
         DC    AL1(225,UADH,ALL,ALL-AFM,0,0,0,TV)     AD HLD FEE                
*                                                                               
         DC    AL1(00,UCLA,UCLAREG,ALL,0,0,0,ALL)     CLA                       
         DC    AL1(08,UCLA,UCLAG13,ALL,0,0,0,ALL)     13 USE GUAR               
         DC    AL1(58),AL1(ULNA,ALL,ALL,0,0,0,ALL)    LATE NIGHT ABC            
         DC    AL1(58),AL1(ULNN,ALL,ALL,0,0,0,ALL)               NBC            
         DC    AL1(58),AL1(ULNC,ALL,ALL,0,0,0,ALL)               CBS            
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
         DC    AL1(41,UCBL,ALL,ALL,0,0,0,ALL)         CABLE                     
         DC    AL1(41,USCB,ALL,ALL,0,0,0,ALL)         SPANISH CABLE             
*                                                                               
         DC    AL1(26,URNT,URNT1W,ALL,0,0,0,ALL)      RAD NWK 1 WK              
         DC    AL1(27,URNT,URNT4W,ALL,0,0,0,ALL)             4 WK               
         DC    AL1(28,URNT,URNT8W,ALL,0,0,0,ALL)             8 WK               
         DC    AL1(29,URNT,URNT13W,ALL,0,0,0,ALL)            13 WK              
         DC    AL1(30,URNT,URNTAB,ALL,0,0,0,ALL)             A-B                
         DC    AL1(31,URNT,URNT26U,ALL,0,0,0,ALL)            26 USE             
         DC    AL1(32,URNT,URNT39U,ALL,0,0,0,ALL)            39 USE             
*                                                                               
         DC    AL1(33,URRN,ALL,ALL,0,0,0,RADIO)       RAD REG NWK               
*                                                                               
         DC    AL1(39,URLO,ALL,ALL,0,0,0,ALL)         RAD LCL 13 WK             
*                                                                               
         DC    AL1(35,UMUS,UMUSDUB,ALL,0,0,0,ALL)     DUBBING                   
         DC    AL1(35,UMUS,UMUS13W,ALL,0,0,0,ALL)     REUSE                     
         DC    AL1(35,UMUS,UMUSNEW,ALL,0,0,0,ALL)     NEW                       
         DC    AL1(35,UMUS,UMUSDSH,ALL,0,0,0,ALL)     DUB TO SHORTER            
         DC    AL1(35,UNBM,ALL,ALL,0,0,0,ALL)         NON-BRD MUSIC             
         DC    AL1(09,UMUS,UMUSDUB8,ALL,0,0,0,ALL)    DUB - 8 WK                
         DC    AL1(09,UMUS,UMUS8W,ALL,0,0,0,ALL)      REUSE - 8 WK              
         DC    AL1(09,UMUS,UMUSNEW8,ALL,0,0,0,ALL)    NEW - 8 WK                
         DC    AL1(09,UMUS,UMUSDSH8,ALL,0,0,0,ALL)    DUB TO SHRT 8W            
         DC    AL1(36,UFMU,ALL,ALL,0,0,0,ALL)         FIRST REUSE               
*                                                                               
         DC    AL1(45,UFGM,UFGMEU12,ALL,0,0,0,ALL)    AFM FGN-EUR 12M           
         DC    AL1(45,UFGM,UFGMNE12,ALL,0,0,0,ALL)    NOT EUR 12M               
         DC    AL1(47,UFGM,UFGMEU24,ALL,0,0,0,ALL)    EUR 24M                   
         DC    AL1(47,UFGM,UFGMNE24,ALL,0,0,0,ALL)    NOT EUR 24M               
         DC    AL1(46,UFGM,UFGMWO12,ALL,0,0,0,ALL)    WORLD 12M                 
         DC    AL1(48,UFGM,UFGMWO24,ALL,0,0,0,ALL)    WORLD 24M                 
*                                                                               
         DC    AL1(50,UFGR,UFGRUK,ALL,0,0,0,TV)       FGN REUSE-UK              
         DC    AL1(51,UFGR,UFGREUR,ALL,0,0,0,TV)      EUROPE W/O UK             
         DC    AL1(62,UFGR,UFGRWOR,ALL,0,0,0,TV)      WLD W/O UK&EUR            
         DC    AL1(62,UFGR,UFGRAP,ALL,0,0,0,TV)       ASIAN PAC                 
         DC    AL1(62,UFGR,UFGRMAJ,ALL,0,0,0,TV)      NEW TYPE-W/MAJ            
         DC    AL1(49,UFGR,UFGRRAD,ALL,0,0,0,RADIO)   RADIO                     
*                                                                               
         DC    AL1(62,UPBS,ALL,ALL-AFM,0,0,0,TV)      PUB SERV REUSE            
         DC    AL1(52,UPBS,ALL,ALL-AFM,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(40,USNT,ALL,ALL,0,0,0,TV)          SPAN NWK USE              
         DC    AL1(40,USNW,ALL,ALL,0,0,0,TV)          SPAN N/W COMB.            
         DC    AL1(10,USWS,ALL,ALL,0,0,0,TV)          SPAN WSP                  
*                                                                               
         DC    AL1(65,UADT,UADT3D,ALL,0,0,0,ALL)      AD SES-TV-3DAY            
         DC    AL1(70,UADT,UADT1W,ALL,0,0,0,ALL)      1 WEEK                    
         DC    AL1(80,UADT,UADT4W,ALL,0,0,0,ALL)      4 WEEK                    
         DC    AL1(80,UADT,UADT31D,ALL,0,0,0,ALL)     31 DAY(KS ONLY)           
         DC    AL1(90,UADT,UADT13W,ALL,0,0,0,ALL)     13 WEEK                   
*                                                                               
         DC    AL1(100,UADO,UADO3D,ALL,0,0,0,ALL)  AD SES-RAD-3DY               
         DC    AL1(105,UADO,UADO1W,ALL,0,0,0,ALL)     1 WEEK                    
         DC    AL1(115,UADO,UADO4W,ALL,0,0,0,ALL)     4 WEEK                    
         DC    AL1(115,UADO,UADO31D,ALL,0,0,0,ALL)    31 DAY(KS ONLY)           
         DC    AL1(125,UADO,UADO13W,ALL,0,0,0,ALL)    13 WEEK                   
*                                                                               
         DC    AL1(205,UADD,ALL,ALL,0,0,0,TV)         AD DEMO                   
         DC    AL1(215,UADD,ALL,AFT+NON,0,0,0,RADIO)                            
*                                                                               
         DC    AL1(135,UADW,UADW3D,ALL,0,0,0,TV)     AD WSP-TV-3DAY             
         DC    AL1(140,UADW,UADW1W,ALL,0,0,0,TV)     1 WEEK                     
         DC    AL1(150,UADW,UADW4W,ALL,0,0,0,TV)     4 WEEK                     
         DC    AL1(150,UADW,UADW31D,ALL,0,0,0,TV)    31 DAY(KS ONLY)            
         DC    AL1(160,UADW,UADW13W,ALL,0,0,0,TV)    13 WEEK                    
*                                                                               
         DC    AL1(170,UADW,UADW3D,ALL,0,0,0,RADIO)   AD WSP-RAD-3DY            
         DC    AL1(175,UADW,UADW1W,ALL,0,0,0,RADIO)   1 WEEK                    
         DC    AL1(185,UADW,UADW4W,ALL,0,0,0,RADIO)   4 WEEK                    
         DC    AL1(185,UADW,UADW4W,ALL,0,0,0,RADIO)   31 DAY(KS ONLY)           
         DC    AL1(195,UADW,UADW13W,ALL,0,0,0,RADIO)  13 WEEK                   
*                                                                               
         DC    AL1(135,UADC,UADC3D,ALL,0,0,0,TV)     CMB S/W-TV-3 DAY           
         DC    AL1(140,UADC,UADC1W,ALL,0,0,0,TV)     1 WEEK                     
         DC    AL1(150,UADC,UADC4W,ALL,0,0,0,TV)     4 WEEK                     
         DC    AL1(150,UADC,UADC31D,ALL,0,0,0,TV)    31 DAY(KS ONLY)            
         DC    AL1(160,UADC,UADC13W,ALL,0,0,0,TV)    13 WEEK                    
*                                                                               
         DC    AL1(170,UADC,UADC3D,ALL,0,0,0,RADIO)   CMB S/W-RAD-3D            
         DC    AL1(175,UADC,UADC1W,ALL,0,0,0,RADIO)   1 WEEK                    
         DC    AL1(185,UADC,UADC4W,ALL,0,0,0,RADIO)   4 WEEK                    
         DC    AL1(185,UADC,UADC31D,ALL,0,0,0,RADIO)  31 DAY(KS ONLY)           
         DC    AL1(195,UADC,UADC13W,ALL,0,0,0,RADIO)  13 WEEK                   
*                                                                               
*        DC    AL1(44,UIFB,ALL,ALL,0,0,0,TV)          INS FOR BOOKS             
*                                                                               
         DC    AL1(53,UTAG,UTAGREG,ALL,0,0,0,TV)      TAGS FOR TV               
         DC    AL1(54,UTAG,UTAGSESS,ALL,0,0,0,TV)     TAGS FOR TV               
         DC    AL1(55,UTAG,UTAGREG,ALL,0,0,0,RADIO)   TAGS FOR RAD              
         DC    AL1(56,UTAG,UTAGSESS,ALL,0,0,0,RADIO)  TAGS FOR RAD              
*                                                                               
         DC    AL1(62,UINR,UINR30D,ALL,0,0,0,TV)      TH/IN RE-30D              
         DC    AL1(236,UINR,UINRUNL,ALL,0,0,0,TV)     UNLIM USE                 
         DC    AL1(61,UINR,UINR30D,ALL,0,0,0,RADIO)   30 DAYS-RAD               
         DC    AL1(235,UINR,UINRUNL,ALL,0,0,0,RADIO)  UNLIM USE-RAD             
*                                                                               
         DC    AL1(238,ULCB,ULCB50,ALL,0,0,0,ALL)     LCL CBL                   
         DC    AL1(239,ULCB,ULCB100,ALL,0,0,0,ALL)                              
         DC    AL1(240,ULCB,ULCB150,ALL,0,0,0,ALL)                              
         DC    AL1(241,ULCB,ULCB200,ALL,0,0,0,ALL)                              
         DC    AL1(242,ULCB,ULCB250,ALL,0,0,0,ALL)                              
         DC    AL1(243,ULCB,ULCB500,ALL,0,0,0,ALL)                              
         DC    AL1(244,ULCB,ULCB750,ALL,0,0,0,ALL)                              
         DC    AL1(245,ULCB,ULCB1M,ALL,0,0,0,ALL)                               
         DC    AL1(246,ULCB,ULCBMAX,ALL,0,0,0,ALL)                              
*                                                                               
******  USE NUMBERS 34 USED FOR RRN WITH MAJORS                                 
******  USE NUMBERS 237 USED FOR LCB WITH MAJORS                                
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
         SPACE 3                                                                
*              TAG FEES                                                         
         SPACE 3                                                                
TAGFEE   DS    0F                                                               
         DC    F'12225'            TV SESSION TAG FEE - ON CAMERA               
         DC    F'9260'             TV SESSION TAG FEE - OFF                     
         DC    F'6760'             RADIO SESSION TAG FEE                        
         DC    F'10600'            GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'8100'             GA ADDEN TV SESSION TAG FEE - OFF            
         DC    F'5900'             GA ADDENDUM RADIO SESSION TAG FEE            
         DC    F'6330'             KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'4180'             KS ADDEN TV SESSION TAG FEE - OFF            
         DC    F'2300'             KS ADDENDUM RADIO SESSION TAG FEE            
         DC    F'10000'            TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'7100'             TX ADDEN TV SESSION TAG FEE - OFF            
         DC    F'5500'             TX ADDENDUM RADIO SESSION TAG FEE            
         EJECT                                                                  
*              COLUMN TABLES - TV - ON CAMERA                                   
         SPACE 1                                                                
ONCOLS   DS    0CL2                                                             
         DC    AL1(1,CTACR)                                                     
         DC    AL1(1,CTP)                                                       
         DC    AL1(1,CTPUP)                                                     
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
         DC    AL1(1,CTEXD)                                                     
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
*                                  IF DEFINE ROW >= 18, CHANGE SYSCALC          
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTCAR)                                                    
         DC    AL1(60,CTGEN)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - TV - OFF CAMERA                                  
         SPACE 1                                                                
OFFCOLS  DS    0CL2                                                             
         DC    AL1(1,CTEXD)        EXD ALWAYS GETS ON CAMERA RATES              
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
*                                 IF DEFINE ROW >= 18, CHANGE TASYSCALC         
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTAD)                                                     
         DC    AL1(60,CTAPR)                                                    
         DC    AL1(60,CTCAR)                                                    
         DC    AL1(60,CTGEN)                                                    
         DC    AL1(60,CTMV)                                                     
         DC    AL1(60,CTPR)                                                     
         DC    AL1(60,CTSA)                                                     
         DC    AL1(60,CTSV)                                                     
         DC    AL1(60,CTW)                                                      
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
         DC    AL1(7,CTC3)                                                      
         DC    AL1(7,CTC6)                                                      
         DC    AL1(8,CTC9)                                                      
*                                  IF DEFINE ROW >= 9, CHANGE TASYSCALC         
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTAD)                                                     
         DC    AL1(60,CTAPR)                                                    
         DC    AL1(60,CTCV)                                                     
         DC    AL1(60,CTGEN)                                                    
         DC    AL1(60,CTGSM)                                                    
         DC    AL1(60,CTGSS)                                                    
         DC    AL1(60,CTGS)                                                     
         DC    AL1(60,CTPR)                                                     
         DC    AL1(60,CTSA)                                                     
         DC    AL1(60,CTW)                                                      
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
         DC    AL1(1,CTCA)         CONTRACTOR-ARRANGERR                         
         DC    AL1(1,CTCO)         CONTRACTOR-ORCHESTRATOR                      
         DC    AL1(1,CTCCP)        CONTRACTOR-COPYIST                           
*                                                                               
         DC    AL1(1,CTCP)         COPYIST                                      
*                                                                               
         DC    AL1(1,CTL)          LEADER                                       
         DC    AL1(1,CTLA)         LEADER-ARRANGER                              
         DC    AL1(1,CTLAC)        LEADER-ARRANGER-COPYIST                      
         DC    AL1(1,CTLAO)        LEADER-ARRANGER-ORCHESTRATOR                 
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
**PAN#1  DC    CL21'051TAGEN6C   10/05/11'                                      
         END                                                                    
