*          DATA SET TAGEN6D    AT LEVEL 019 AS OF 10/05/11                      
*PHASE T7026DC,*                                                                
         TITLE 'T7026D - TABLES FOR 1994 CONTRACTS'                             
T7026D   CSECT                                                                  
         DC    AL4(USETBLS-T7026D)                                              
         DC    AL4(USELUT-T7026D)                                               
         DC    AL4(MAJLUT-T7026D)                                               
         DC    AL4(AFMCOLS-T7026D)                                              
         DC    AL4(RADCOLS-T7026D)                                              
         DC    AL4(OFFCOLS-T7026D)                                              
         DC    AL4(ONCOLS-T7026D)                                               
         DC    AL4(MSWEET-T7026D)                                               
         DC    AL4(TAGFEE-T7026D)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL1(0,36,1,1)       CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    F'44325'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'33330'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'32450'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'28725'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'23760'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'18795'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'16310'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'13300'            'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    F'101220'                                                        
         DC    F'72390'                                                         
         DC    F'64470'                                                         
         DC    F'57010'                                                         
         DC    F'46605'                                                         
         DC    F'23755'                                                         
         DC    F'19800'                                                         
         DC    F'16185'                                                         
         SPACE 1                                                                
CBXTAB   DC    AL1(2,36,0,0)       CLASS B W/O NY                               
         DC    F'82560'                                                         
         DC    F'57340'                                                         
         DC    F'64470'                                                         
         DC    F'57010'                                                         
         DC    F'46605'                                                         
         DC    F'23755'                                                         
         DC    F'19800'                                                         
         DC    F'16185'                                                         
         SPACE 1                                                                
CLCTAB   DC    AL1(3,36,0,0)       CLASS C                                      
         DC    F'49200'                                                         
         DC    F'32800'                                                         
         DC    F'42640'                                                         
         DC    F'37895'                                                         
         DC    F'30985'                                                         
         DC    F'18895'                                                         
         DC    F'15725'                                                         
         DC    F'12900'                                                         
         SPACE 1                                                                
NWKTBL   DC    AL1(58),AL1(36,1,1) NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(27575)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(20735)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(20190)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(17875)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(14780)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(11690)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(10140)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(8275)           'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
         SPACE 3                                                                
DANTAB   DC    AL1(4,36,0,0)       DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    F'200770'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'139840'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'150750'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'132885'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'103280'           'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'61625'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'53970'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'38535'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL1(5,36,0,0)       DEALER A W/O NY                              
         DC    F'177555'                                                        
         DC    F'128240'                                                        
         DC    F'150750'                                                        
         DC    F'132885'                                                        
         DC    F'103280'                                                        
         DC    F'61625'                                                         
         DC    F'53970'                                                         
         DC    F'38535'                                                         
         SPACE 1                                                                
DBNTAB   DC    AL1(6,36,0,0)       CLASS B INCL NY                              
         DC    F'308695'                                                        
         DC    F'210055'                                                        
         DC    F'229200'                                                        
         DC    F'202045'                                                        
         DC    F'157245'                                                        
         DC    F'93885'                                                         
         DC    F'82170'                                                         
         DC    F'58615'                                                         
         SPACE 1                                                                
DBXTAB   DC    AL1(7,36,0,0)       CLASS B W/O NY                               
         DC    F'266345'                                                        
         DC    F'192060'                                                        
         DC    F'229200'                                                        
         DC    F'202045'                                                        
         DC    F'157245'                                                        
         DC    F'93885'                                                         
         DC    F'82170'                                                         
         DC    F'58615'                                                         
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
         SPACE 3                                                                
G13TAB   DC    AL1(8,36,1,1)       13 USE                                       
         DC    F'141005'          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS              
         DC    F'109300'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'117595'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'105070'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'86285'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'71665'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'62380'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'51045'            'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    F'44325'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'33330'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'32450'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'28725'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'23760'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'18795'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'16310'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'13300'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(10,36,2,25)     UNITS 2-25                                   
         DC    F'1744'                                                          
         DC    F'1193'                                                          
         DC    F'1359'                                                          
         DC    F'1172'                                                          
         DC    F'958'                                                           
         DC    F'482'                                                           
         DC    F'380'                                                           
         DC    F'316'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,26,60)    UNITS 26-60                                  
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'701'                                                           
         DC    F'594'                                                           
         DC    F'492'                                                           
         DC    F'203'                                                           
         DC    F'139'                                                           
         DC    F'128'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,61,125)   UNITS 61-125                                 
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'508'                                                           
         DC    F'396'                                                           
         DC    F'332'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,126,255)  UNITS 126+                                   
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'251'                                                           
         DC    F'203'                                                           
         DC    F'177'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL1(11,36,0,0)      NY ALONE                                     
         DC    F'100130'                                                        
         DC    F'70740'                                                         
         DC    F'64125'                                                         
         DC    F'56955'                                                         
         DC    F'46670'                                                         
         DC    F'25725'                                                         
         DC    F'21315'                                                         
         DC    F'17450'                                                         
         SPACE 1                                                                
         DC    AL1(11,36,1,35)     UNITS 1-35                                   
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'701'                                                           
         DC    F'594'                                                           
         DC    F'492'                                                           
         DC    F'203'                                                           
         DC    F'139'                                                           
         DC    F'128'                                                           
         SPACE 1                                                                
         DC    AL1(11,36,36,100)   UNITS 36-100                                 
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'508'                                                           
         DC    F'396'                                                           
         DC    F'332'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,101,255)  UNITS 101+                                   
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'251'                                                           
         DC    F'203'                                                           
         DC    F'177'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL1(12,36,0,0)      CHI OR LA ALONE                              
         DC    F'87275'                                                         
         DC    F'61550'                                                         
         DC    F'64125'                                                         
         DC    F'56955'                                                         
         DC    F'46670'                                                         
         DC    F'25725'                                                         
         DC    F'21315'                                                         
         DC    F'17450'                                                         
         SPACE 1                                                                
         DC    AL1(12,36,1,35)     UNITS 1-35                                   
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'701'                                                           
         DC    F'594'                                                           
         DC    F'492'                                                           
         DC    F'203'                                                           
         DC    F'139'                                                           
         DC    F'128'                                                           
         SPACE 1                                                                
         DC    AL1(12,36,36,100)   UNITS 36-100                                 
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'508'                                                           
         DC    F'396'                                                           
         DC    F'332'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,101,255)  UNITS 101+                                   
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'251'                                                           
         DC    F'203'                                                           
         DC    F'177'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL1(13,36,0,0)      TWO OF NY LA CHI                             
         DC    F'137795'                                                        
         DC    F'92780'                                                         
         DC    F'98660'                                                         
         DC    F'81575'                                                         
         DC    F'66695'                                                         
         DC    F'33995'                                                         
         DC    F'27380'                                                         
         DC    F'22420'                                                         
         SPACE 1                                                                
         DC    AL1(13,36,1,255)    UNITS 1+                                     
         DC    F'647'                                                           
         DC    F'508'                                                           
         DC    F'251'                                                           
         DC    F'203'                                                           
         DC    F'177'                                                           
         DC    F'123'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(14,36,0,0)      ALL THREE MAJORS                             
         DC    F'166210'                                                        
         DC    F'118050'                                                        
         DC    F'124470'                                                        
         DC    F'106525'                                                        
         DC    F'87065'                                                         
         DC    F'40985'                                                         
         DC    F'33050'                                                         
         DC    F'27010'                                                         
         SPACE 1                                                                
         DC    AL1(14,36,1,255)    UNITS 1+                                     
         DC    F'663'                                                           
         DC    F'519'                                                           
         DC    F'257'                                                           
         DC    F'209'                                                           
         DC    F'182'                                                           
         DC    F'128'                                                           
         DC    F'75'                                                            
         DC    F'75'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL1(15,28,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    F'18500'            ANN ALONE                                    
         DC    F'18500'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'13640'            1-4M3,1-4S3,D3,S3                            
         DC    F'12070'            1-4M6,1-4S6,D6,S6                            
         DC    F'10715'            1-4M9,1-4S9,D9,S9                            
         DC    F'6400'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL1(15,24,2,25)     UNITS 2-25                                   
         DC    F'280'                                                           
         DC    F'280'                                                           
         DC    F'145'                                                           
         DC    F'125'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(15,24,26,60)    UNITS 26-60                                  
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'125'                                                           
         DC    F'95'                                                            
         DC    F'95'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,61,255)   UNITS 61+                                    
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'70'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(16,28,0,0)      NEW YORK ALONE                               
         DC    F'28500'                                                         
         DC    F'28500'                                                         
         DC    F'15500'                                                         
         DC    F'12035'                                                         
         DC    F'10710'                                                         
         DC    F'6400'                                                          
         SPACE 1                                                                
         DC    AL1(16,24,1,35)     UNITS 1-35                                   
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'125'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(16,24,36,255)   UNITS 36+                                    
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'70'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(17,28,0,0)      CHICAGO OR LA ALONE                          
         DC    F'25845'                                                         
         DC    F'25845'                                                         
         DC    F'15500'                                                         
         DC    F'12035'                                                         
         DC    F'10710'                                                         
         DC    F'6400'                                                          
         SPACE 1                                                                
         DC    AL1(17,24,1,35)     UNITS 1-35                                   
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'125'                                                           
         DC    F'90'                                                            
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(17,24,36,255)   UNITS 36+                                    
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'70'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(18,28,0,0)      ANY TWO ALONE                                
         DC    F'34760'                                                         
         DC    F'34760'                                                         
         DC    F'18510'                                                         
         DC    F'14200'                                                         
         DC    F'12640'                                                         
         DC    F'6400'                                                          
         SPACE 1                                                                
         DC    AL1(18,24,1,255)    UNITS 1+                                     
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'70'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         SPACE 1                                                                
         DC    AL1(19,28,0,0)      ALL THREE ALONE                              
         DC    F'43920'                                                         
         DC    F'43920'                                                         
         DC    F'20620'                                                         
         DC    F'15960'                                                         
         DC    F'14200'                                                         
         DC    F'6400'                                                          
         SPACE 1                                                                
         DC    AL1(19,24,1,255)    UNITS 1+                                     
         DC    F'210'                                                           
         DC    F'210'                                                           
         DC    F'70'                                                            
         DC    F'50'                                                            
         DC    F'50'                                                            
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL1(20,24,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(18500) ANN ALONE                                    
         DC    AL1(100),AL3(18500) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(13640) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(12070) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(10715) 1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL1(20,24,2,25)     UNITS 2-25                                   
         DC    AL1(80),AL3(280)                                                 
         DC    AL1(80),AL3(280)                                                 
         DC    AL1(95),AL3(145)                                                 
         DC    AL1(95),AL3(125)                                                 
         DC    AL1(95),AL3(110)                                                 
         SPACE 1                                                                
         DC    AL1(20,24,26,60)    UNITS 26-60                                  
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(125)                                                 
         DC    AL1(95),AL3(95)                                                  
         DC    AL1(95),AL3(95)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,61,255)   UNITS 61+                                    
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(21,24,0,0)      NEW YORK ALONE                               
         DC    AL1(80),AL3(28500)                                               
         DC    AL1(80),AL3(28500)                                               
         DC    AL1(95),AL3(15500)                                               
         DC    AL1(95),AL3(12035)                                               
         DC    AL1(95),AL3(10710)                                               
         SPACE 1                                                                
         DC    AL1(21,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(125)                                                 
         DC    AL1(95),AL3(90)                                                  
         DC    AL1(95),AL3(90)                                                  
         SPACE 1                                                                
         DC    AL1(21,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,0,0)      CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(25845)                                               
         DC    AL1(80),AL3(25845)                                               
         DC    AL1(95),AL3(15500)                                               
         DC    AL1(95),AL3(12035)                                               
         DC    AL1(95),AL3(10710)                                               
         SPACE 1                                                                
         DC    AL1(22,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(125)                                                 
         DC    AL1(95),AL3(90)                                                  
         DC    AL1(95),AL3(90)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(23,24,0,0)      ANY TWO ALONE                                
         DC    AL1(80),AL3(34760)                                               
         DC    AL1(80),AL3(34760)                                               
         DC    AL1(95),AL3(18510)                                               
         DC    AL1(95),AL3(14200)                                               
         DC    AL1(95),AL3(12640)                                               
         SPACE 1                                                                
         DC    AL1(23,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         SPACE 1                                                                
         DC    AL1(24,24,0,0)      ALL THREE ALONE                              
         DC    AL1(80),AL3(43920)                                               
         DC    AL1(80),AL3(43920)                                               
         DC    AL1(95),AL3(20620)                                               
         DC    AL1(95),AL3(15960)                                               
         DC    AL1(95),AL3(14200)                                               
         SPACE 1                                                                
         DC    AL1(24,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(80),AL3(210)                                                 
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(50)                                                  
         DC    AL1(95),AL3(50)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
         SPACE 3                                                                
DLRTAB   DC    AL1(25,28,0,0)      DEALER COMMERCIALS                           
         DC    F'60620'            AR,AS,P,ANN                                  
         DC    F'48085'            S,1-4MS,1-4SS                                
         DC    F'31350'            1-4M3,1-4S3,D3,S3                            
         DC    F'25085'            1-4M6,1-4S6,D6,S6                            
         DC    F'15675'            1-4M9,1-4S9,D9,S9                            
         DC    F'15855'            SE                                           
         SPACE 1                                                                
N01TAB   DC    AL1(26,28,0,0)      NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    F'37185'            ANN ALONE                                    
         DC    F'37185'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'27900'            1-4M3,1-4S3,D3,S3                            
         DC    F'27900'            1-4M6,1-4S6,D6,S6                            
         DC    F'27900'            1-4M9,1-4S9,D9,S9                            
         DC    F'8715'             SE                                           
         SPACE 1                                                                
N04TAB   DC    AL1(27,28,0,0)      NETWORK 4 WEEK                               
         DC    F'60335'                                                         
         DC    F'60335'                                                         
         DC    F'46395'                                                         
         DC    F'41490'                                                         
         DC    F'37900'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
N08TAB   DC    AL1(28,28,0,0)      NETWORK 8 WEEK                               
         DC    F'96110'                                                         
         DC    F'96110'                                                         
         DC    F'73945'                                                         
         DC    F'66085'                                                         
         DC    F'59195'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
N13TAB   DC    AL1(29,28,0,0)      NETWORK 13 WEEK                              
         DC    F'119255'                                                        
         DC    F'119255'                                                        
         DC    F'91725'                                                         
         DC    F'82015'                                                         
         DC    F'75135'                                                         
         DC    F'8715'                                                          
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL1(30,28,0,0)      ACROSS-THE-BOARD                             
         DC    F'124875'                                                        
         DC    F'124875'                                                        
         DC    F'96030'                                                         
         DC    F'85875'                                                         
         DC    F'78675'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
U26TAB   DC    AL1(31,28,0,0)      26 USE LIMIT                                 
         DC    F'59635'                                                         
         DC    F'59635'                                                         
         DC    F'45855'                                                         
         DC    F'41005'                                                         
         DC    F'37465'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
U39TAB   DC    AL1(32,28,0,0)      39 USE LIMIT                                 
         DC    F'89805'                                                         
         DC    F'89805'                                                         
         DC    F'62880'                                                         
         DC    F'56125'                                                         
         DC    F'50995'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
R13TAB   DC    AL1(33,28,0,0)      REGIONAL - NO MAJORS                         
         DC    F'71965'                                                         
         DC    F'71965'                                                         
         DC    F'33730'                                                         
         DC    F'33730'                                                         
         DC    F'33730'                                                         
         DC    F'8715'                                                          
         SPACE 1                                                                
         DC    AL1(34,28,0,0)      REGIONAL - WITH ANY MAJORS                   
         DC    F'71965'                                                         
         DC    F'71965'                                                         
         DC    F'71965'                                                         
         DC    F'64770'                                                         
         DC    F'58250'                                                         
         DC    F'8715'                                                          
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'6750'             CAST=1                                       
         DC    F'6750'                  2-4                                     
         DC    F'6750'                  5+                                      
         SPACE 2                                                                
FMUTAB   DC    AL1(36,16,0,0)      FIRST REUSE                                  
         DC    F'4400'             CAST=1                                       
         DC    F'2200'                  2-4                                     
         DC    F'2200'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'9000'             CAST=1                                       
         DC    F'9000'                  2-4                                     
         DC    F'9000'                  5+                                      
         SPACE 3                                                                
MS8TAB   DC    AL1(09,16,0,0)      8-WEEK REUSE                                 
         DC    AL1(80),AL3(6750)                                                
         DC    AL1(80),AL3(6750)                                                
         DC    AL1(80),AL3(6750)                                                
         EJECT                                                                  
*              MUSIC FOREIGN USE                                                
         SPACE 2                                                                
FGMTAB   DC    AL1(45,16,0,0)      EUROPE OR OUTSIDE EUROPE-12M                 
         DC    F'5625'             CAST=1                                       
         DC    F'5625'                  2-4                                     
         DC    F'5625'                  5+                                      
         SPACE                                                                  
         DC    AL1(46,16,0,0)      WORLD - 12M                                  
         DC    F'9000'             CAST=1                                       
         DC    F'9000'                  2-4                                     
         DC    F'9000'                  5+                                      
         SPACE 3                                                                
         DC    AL1(47,16,0,0)      EUROPE OR OUTSIDE EUROPE-24M                 
         DC    F'8440'             CAST=1                                       
         DC    F'8440'                  2-4                                     
         DC    F'8440'                  5+                                      
         SPACE                                                                  
         DC    AL1(48,16,0,0)      WORLD - 24M                                  
         DC    F'13500'            CAST=1                                       
         DC    F'13500'                2-4                                      
         DC    F'13500'                5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 1                                                                
         DC    AL1(61,36,0,0)      AFT RADIO BASE SESSION RATES                 
         DC    F'18500'            ANN ALONE                                    
         DC    F'18500'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'13640'            1-4M3,1-4S3,D3,S3                            
         DC    F'12070'            1-4M6,1-4S6,D6,S6                            
         DC    F'10715'            1-4M9,1-4S9,D9,S9                            
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(62,72,1,255)    NON-AFM TV BASE SESSION RATES                
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         DC    F'18795'                                                         
         DC    F'16310'                                                         
         DC    F'13300'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    F'68270'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(64,72,1,255)    NON-AFM CABLE BASE SESSION RATES             
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         DC    F'18795'                                                         
         DC    F'16310'                                                         
         DC    F'13300'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'68270'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         EJECT                                                                  
         DC    AL1(63,24,0,0)      TV HOLDING RATES - HLD                       
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         SPACE 1                                                                
         DC    AL1(37,72,0,0)      TV POSTPONEMENT FEE RATES                    
         DC    F'22163'                                                         
         DC    F'16665'                                                         
         DC    F'16225'                                                         
         DC    F'14363'                                                         
         DC    F'11880'                                                         
         DC    F'9398'                                                          
         DC    F'8155'                                                          
         DC    F'6650'                                                          
         DC    F'12000'                                                         
         DC    F'18313'                                                         
         DC    F'6965'                                                          
         DC    F'12180'                                                         
         DC    F'34135'                                                         
         DC    F'0'                                                             
         DC    F'13645'                                                         
         DC    F'3553'                                                          
         DC    F'7008'                                                          
         SPACE 1                                                                
         DC    AL1(38,52,0,0)      REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(44325)                                              
         DC    AL1(200),AL3(33330)                                              
         DC    AL1(200),AL3(32450)                                              
         DC    AL1(200),AL3(28725)                                              
         DC    AL1(200),AL3(23760)                                              
         DC    5A(0)                                                            
         DC    AL1(200),AL3(13930)                                              
         DC    AL1(200),AL3(24360)                                              
         EJECT                                                                  
*              CABLE RATES                                                      
         SPACE 3                                                                
         DC    AL1(41,36,1,1)      CBL & SCB - MINIMUM                          
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         DC    F'18795'                                                         
         DC    F'16310'                                                         
         DC    F'13300'                                                         
         SPACE 1                                                                
         DC    AL1(41,4,2,104)     MINIMUM COVERS UPTO 104                      
         SPACE 1                                                                
         DC    AL1(41,36,105,105)                                               
         DC    F'60'                                                            
         DC    F'35'                                                            
         DC    F'35'                                                            
         DC    F'15'                                                            
         DC    F'0'                                                             
         DC    F'20'                                                            
         DC    F'10'                                                            
         DC    F'05'                                                            
         SPACE 1                                                                
         DC    AL1(41,36,106,106)                                               
         DC    F'337'                                                           
         DC    F'253'                                                           
         DC    F'247'                                                           
         DC    F'218'                                                           
         DC    F'176'                                                           
         DC    F'143'                                                           
         DC    F'124'                                                           
         DC    F'101'                                                           
         SPACE 1                                                                
         DC    AL1(41,36,107,150)                                               
         DC    F'337'                                                           
         DC    F'253'                                                           
         DC    F'247'                                                           
         DC    F'218'                                                           
         DC    F'181'                                                           
         DC    F'143'                                                           
         DC    F'124'                                                           
         DC    F'101'                                                           
         SPACE 1                                                                
         DC    AL1(41,36,151,178)  MAX IS 178 EFF 2/7/94                        
         DC    F'277'                                                           
         DC    F'209'                                                           
         DC    F'203'                                                           
         DC    F'180'                                                           
         DC    F'149'                                                           
         DC    F'118'                                                           
         DC    F'102'                                                           
         DC    F'83'                                                            
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
         SPACE 1                                                                
         DC    AL1(42,76,1,255)     DEM (TV)                                    
         DC    F'33330'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'16665'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'24330'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'21540'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'17815'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'9095'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'9095'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'9095'             'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2A(0)               N/D                                          
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    3A(0)               N/D                                          
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         DC    F'13910'            'OFF' S                                      
         SPACE 1                                                                
         DC    AL1(43,40,1,255)    DEM (AFT RADIO)                              
         DC    F'12760'            ANN ALONE                                    
         DC    F'12760'            AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'9095'             1-4M3,1-4S3,D3,S3                            
         DC    F'9095'             1-4M6,1-4S6,D6,S6                            
         DC    F'9095'             1-4M9,1-4S9,D9,S9                            
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'13910'            SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL1(39,28,0,0)      LOCAL 13 WEEK - RADIO                        
         DC    F'24360'                                                         
         DC    F'24360'                                                         
         DC    F'24360'                                                         
         DC    F'24360'                                                         
         DC    F'24360'                                                         
         DC    F'8715'                                                          
         EJECT                                                                  
*              FOREIGN REUSE                                                    
         SPACE                                                                  
         DC    AL1(50,72,0,0)      UK - 3X SESSION RATE (CAN'T USE MULT         
         DC    F'132975'           FACTOR, WON'T FIT IN AL1)                    
         DC    F'99990'                                                         
         DC    F'97350'                                                         
         DC    F'86175'                                                         
         DC    F'71280'                                                         
         DC    F'56385'                                                         
         DC    F'48930'                                                         
         DC    F'39900'                                                         
         DC    F'72000'                                                         
         DC    F'109875'                                                        
         DC    F'41790'                                                         
         DC    F'73080'                                                         
         DC    F'204810'           PIL                                          
         DC    F'0'                N/D                                          
         DC    F'81870'            SE                                           
         DC    F'21315'            C3,C6                                        
         DC    F'42045'            C9                                           
         SPACE                                                                  
         DC    AL1(51,72,0,0)      EUROPE W/O UK - 2X SESSION RATE              
         DC    AL1(200),AL3(44325)                                              
         DC    AL1(200),AL3(33330)                                              
         DC    AL1(200),AL3(32450)                                              
         DC    AL1(200),AL3(28725)                                              
         DC    AL1(200),AL3(23760)                                              
         DC    AL1(200),AL3(18795)                                              
         DC    AL1(200),AL3(16310)                                              
         DC    AL1(200),AL3(13300)                                              
         DC    AL1(200),AL3(24000)                                              
         DC    AL1(200),AL3(36625)                                              
         DC    AL1(200),AL3(13930)                                              
         DC    AL1(200),AL3(24360)                                              
         DC    AL1(200),AL3(68270) PIL                                          
         DC    F'0'                N/D                                          
         DC    AL1(200),AL3(27290) SE                                           
         DC    AL1(200),AL3(7105)  C3,C6                                        
         DC    AL1(200),AL3(14015) C9                                           
         SPACE                                                                  
         DC    AL1(237,72,0,0)    WORLDWIDE - 8X SESSION RATE (CAN'T            
         DC    F'354600'          USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    F'266640'                                                        
         DC    F'259600'                                                        
         DC    F'229800'                                                        
         DC    F'190080'                                                        
         DC    F'150360'                                                        
         DC    F'130480'                                                        
         DC    F'106400'                                                        
         DC    F'192000'                                                        
         DC    F'293000'                                                        
         DC    F'111440'                                                        
         DC    F'194880'                                                        
         DC    F'546160'           PIL                                          
         DC    F'0'                N/D                                          
         DC    F'218320'           SE                                           
         DC    F'56840'            C3,C6                                        
         DC    F'112120'           C9                                           
         SPACE                                                                  
         DC    AL1(49,24,0,0)      RADIO                                        
         DC    F'44470'            N/D                                          
         DC    F'44470'            P,ANN,S,D,ACR                                
         DC    F'25795'            3-5 GROUP                                    
         DC    F'17790'            6-8 GROUP                                    
         DC    F'14230'            9+                                           
         EJECT                                                                  
         DC    AL1(52,24,0,0)      PBS RADIO                                    
         DC    F'50715'            P,ANN,ACR                                    
         DC    F'52660'            S,D                                          
         DC    F'34335'            3-5 GROUP                                    
         DC    F'27465'            6-8 GROUP                                    
         DC    F'17175'            9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
         SPACE                                                                  
SNTTBL   DC    AL1(40,36,0,0)      NETWORK                                      
         DC    F'116655'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'87730'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'85400'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'75590'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'62520'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'49560'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'42930'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'35000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
SNWTBL   DC    AL1(40,36,1,255)    NETWK/WILDSPT COMBINED (UNITS 1-255)         
         DC    F'294'                                                           
         DC    F'214'                                                           
         DC    F'209'                                                           
         DC    F'193'                                                           
         DC    F'150'                                                           
         DC    F'123'                                                           
         DC    F'112'                                                           
         DC    F'80'                                                            
         EJECT                                                                  
*              ADDENDUM USES                                                    
         SPACE 1                                                                
ADTTBL   DC    AL1(65,80,0,0)      TV SESSION RATES - 3 DAY - GA                
         DC    F'27000'            ON CAMERA                                    
         DC    F'20300'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'27000'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         DC    F'28800'            SOLO/DUO ON CAM                              
         DC    F'21700'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(70,80,0,0)      1 WEEK - GA                                  
         DC    F'28900'            ON CAMERA                                    
         DC    F'21700'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'28900'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'                                                         
         DC    F'7105'                                                          
         DC    F'14015'                                                         
         DC    F'28800'            SOLO/DUO ON CAM                              
         DC    F'21700'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(71,72,0,0)      TV SESSION RATES - 1 WEEK - KS               
         DC    F'22665'            ON CAMERA                                    
         DC    F'17020'            OFF                                          
         DC    F'17530'                                                         
         DC    F'14990'                                                         
         DC    F'11970'                                                         
         DC    F'7620'                                                          
         DC    F'6170'                                                          
         DC    F'4475'                                                          
         DC    F'14440'                                                         
         DC    F'19345'                                                         
         DC    F'8180'                                                          
         DC    F'12825'                                                         
         DC    F'22665'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(80,80,0,0)      4 WEEK - GA                                  
         DC    F'30900'            ON CAMERA                                    
         DC    F'23200'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'30900'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'                                                         
         DC    F'7105'                                                          
         DC    F'14015'                                                         
         DC    F'28800'            SOLO/DUO ON CAM                              
         DC    F'21700'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(81,72,0,0)      31 DAY - KS                                  
         DC    F'29050'            ON CAMERA                                    
         DC    F'21815'            OFF                                          
         DC    F'21640'                                                         
         DC    F'18385'                                                         
         DC    F'14630'                                                         
         DC    F'9555'                                                          
         DC    F'7500'                                                          
         DC    F'5680'                                                          
         DC    F'14440'                                                         
         DC    F'19345'                                                         
         DC    F'8180'                                                          
         DC    F'12825'                                                         
         DC    F'29050'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(90,80,0,0)      13 WEEK - GA                                 
         DC    F'38600'            ON CAMERA                                    
         DC    F'29000'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'9500'                                                          
         DC    F'17500'                                                         
         DC    F'38600'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'                                                         
         DC    F'7105'                                                          
         DC    F'14015'                                                         
         DC    F'28800'            SOLO/DUO ON CAM                              
         DC    F'21700'            SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(91,72,0,0)      13 WEEKS - KS                                
         DC    F'35430'            ON CAMERA                                    
         DC    F'26600'            OFF                                          
         DC    F'25635'                                                         
         DC    F'21885'                                                         
         DC    F'17530'                                                         
         DC    F'11245'                                                         
         DC    F'9070'                                                          
         DC    F'6775'                                                          
         DC    F'14440'                                                         
         DC    F'19345'                                                         
         DC    F'8180'                                                          
         DC    F'12825'                                                         
         DC    F'35430'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(92,72,0,0)      13 WEEKS - TX                                
         DC    F'33800'            ON CAMERA                                    
         DC    F'24100'            OFF                                          
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         DC    F'13100'                                                         
         DC    F'13100'                                                         
         DC    F'13100'                                                         
         DC    F'18500'                                                         
         DC    F'25400'                                                         
         DC    F'10700'                                                         
         DC    F'16900'                                                         
         DC    F'33800'                                                         
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 3                                                                
ADOTBL   DC    AL1(100,40,0,0)     RADIO SESSION RATES - 3 DAY - GA             
         DC    F'11300'                                                         
         DC    F'11300'                                                         
         DC    F'8900'                                                          
         DC    F'7800'                                                          
         DC    F'7000'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'12000'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(105,40,0,0)     1 WEEK - GA                                  
         DC    F'12100'                                                         
         DC    F'12100'                                                         
         DC    F'8900'                                                          
         DC    F'7800'                                                          
         DC    F'7000'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'12000'                                                         
         SPACE 1                                                                
         DC    AL1(106,36,0,0)     1 WEEK - KS                                  
         DC    F'9170'                                                          
         DC    F'9170'                                                          
         DC    F'5730'                                                          
         DC    F'4820'                                                          
         DC    F'4300'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(115,40,0,0)     4 WEEK - GA                                  
         DC    F'12900'                                                         
         DC    F'12900'                                                         
         DC    F'8900'                                                          
         DC    F'7800'                                                          
         DC    F'7000'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'12000'                                                         
         SPACE 1                                                                
         DC    AL1(116,36,0,0)     31 DAY - KS                                  
         DC    F'11755'                                                         
         DC    F'11755'                                                         
         DC    F'6905'                                                          
         DC    F'6125'                                                          
         DC    F'5470'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(125,40,0,0)     13 WEEK - GA                                 
         DC    F'16100'                                                         
         DC    F'16100'                                                         
         DC    F'8900'                                                          
         DC    F'7800'                                                          
         DC    F'7000'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'12000'                                                         
         SPACE 1                                                                
         DC    AL1(126,36,0,0)     13 WEEK - KS                                 
         DC    F'14330'                                                         
         DC    F'14330'                                                         
         DC    F'8080'                                                          
         DC    F'7165'                                                          
         DC    F'6385'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(127,36,0,0)     13 WEEK - TX                                 
         DC    F'14600'                                                         
         DC    F'14600'                                                         
         DC    F'9300'                                                          
         DC    F'9300'                                                          
         DC    F'9300'                                                          
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 3                                                                
ADHTAB   DC    AL1(225,80,0,0)      ADDENDUM HOLDING RATES - GA                 
         DC    F'38600'                                                         
         DC    F'29000'                                                         
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    12A(0)                                                           
         DC    F'28800'                                                         
         DC    F'21700'                                                         
         SPACE 1                                                                
         DC    AL1(226,24,0,0)      ADDENDUM HOLDING RATES - KS                 
         DC    F'35430'             ON CAMERA                                   
         DC    F'26600'             OFF                                         
         DC    F'25635'                                                         
         DC    F'21885'                                                         
         DC    F'17530'                                                         
         SPACE 1                                                                
         DC    AL1(227,24,0,0)      ADDENDUM HOLDING RATES - TX                 
         DC    F'33800'             ON CAMERA                                   
         DC    F'24100'             OFF                                         
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         SPACE 3                                                                
ADDTAB   DC    AL1(205,72,0,0)      ADDENDUM DEMO (TV) - GA                     
         DC    F'29000'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'14500'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'29000'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'29000'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'29000'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'14500'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'14500'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'14500'            'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7A(0)               N/D                                          
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(206,72,0,0)      ADDENDUM DEMO (TV) - KS                     
         DC    F'7135'             'ON'                                         
         DC    F'6170'             'OFF'                                        
         DC    F'7135'                                                          
         DC    F'7135'                                                          
         DC    F'7135'                                                          
         DC    F'6170'                                                          
         DC    F'6170'                                                          
         DC    F'6170'                                                          
         DC    7A(0)               N/D                                          
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE 1                                                                
         DC    AL1(207,80,0,0)      ADDENDUM DEMO (TV) - TX                     
         DC    F'23000'            'ON'                                         
         DC    F'11300'            'OFF'                                        
         DC    F'11000'                                                         
         DC    F'11000'                                                         
         DC    F'11000'                                                         
         DC    F'5300'                                                          
         DC    F'5300'                                                          
         DC    F'5300'                                                          
         DC    7A(0)               N/D                                          
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         DC    F'23000'            SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    F'8500'             SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL1(215,36,0,0)     ADDENDUM DEMO (AFT RADIO) - GA               
         DC    F'11100'            ANN ALONE                                    
         DC    F'11100'            AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'11100'            1-4M3,1-4S3,D3,S3                            
         DC    F'11100'            1-4M6,1-4S6,D6,S6                            
         DC    F'11100'            1-4M9,1-4S9,D9,S9                            
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(216,36,0,0)     ADDENDUM DEMO (AFT RADIO) - KS               
         DC    F'5340'             ANN ALONE                                    
         DC    F'5340'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'5340'             1-4M3,1-4S3,D3,S3                            
         DC    F'5340'             1-4M6,1-4S6,D6,S6                            
         DC    F'5340'             1-4M9,1-4S9,D9,S9                            
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         SPACE 1                                                                
         DC    AL1(217,40,0,0)     ADDENDUM DEMO (AFT RADIO) - TX               
         DC    F'10300'            ANN ALONE                                    
         DC    F'10300'            AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'5300'             1-4M3,1-4S3,D3,S3                            
         DC    F'5300'             1-4M6,1-4S6,D6,S6                            
         DC    F'5300'             1-4M9,1-4S9,D9,S9                            
         DC    F'14235'            SE                                           
         DC    F'6355'             C3,C6                                        
         DC    F'10165'            C9                                           
         DC    F'8500'             SOLO/DUO                                     
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
         SPACE 3                                                                
ADWTAB   DC    AL1(135,44,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'27000'            ON CAMERA                                    
         DC    F'20300'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'28800'            SOLO/DUO ON CAMERA                           
         DC    F'21700'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(135,44,2,25)    UNITS 2-25                                   
         DC    F'1062'                                                          
         DC    F'727'                                                           
         DC    F'883'                                                           
         DC    F'762'                                                           
         DC    F'623'                                                           
         DC    F'313'                                                           
         DC    F'247'                                                           
         DC    F'205'                                                           
         DC    F'1134'                                                          
         DC    F'775'                                                           
         SPACE 1                                                                
         DC    AL1(135,44,26,60)    UNITS 26-60                                 
         DC    F'394'                                                           
         DC    F'309'                                                           
         DC    F'456'                                                           
         DC    F'386'                                                           
         DC    F'320'                                                           
         DC    F'132'                                                           
         DC    F'90'                                                            
         DC    F'83'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(135,44,61,255)   UNITS 61+                                   
         DC    F'394'                                                           
         DC    F'309'                                                           
         DC    F'330'                                                           
         DC    F'257'                                                           
         DC    F'216'                                                           
         DC    F'80'                                                            
         DC    F'46'                                                            
         DC    F'46'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'28900'            ON CAMERA                                    
         DC    F'21700'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'28800'                                                         
         DC    F'21700'                                                         
         SPACE 1                                                                
         DC    AL1(140,44,2,25)    UNITS 2-25                                   
         DC    F'1138'                                                          
         DC    F'779'                                                           
         DC    F'883'                                                           
         DC    F'762'                                                           
         DC    F'623'                                                           
         DC    F'313'                                                           
         DC    F'247'                                                           
         DC    F'205'                                                           
         DC    F'1134'                                                          
         DC    F'775'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,26,60)    UNITS 26-60                                 
         DC    F'422'                                                           
         DC    F'332'                                                           
         DC    F'456'                                                           
         DC    F'386'                                                           
         DC    F'320'                                                           
         DC    F'132'                                                           
         DC    F'90'                                                            
         DC    F'83'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,61,255)    UNITS 61+                                  
         DC    F'422'                                                           
         DC    F'332'                                                           
         DC    F'330'                                                           
         DC    F'257'                                                           
         DC    F'216'                                                           
         DC    F'80'                                                            
         DC    F'46'                                                            
         DC    F'46'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(141,36,1,1)     1 WEEK - KS - UNIT 1                         
         DC    F'22665'            ON CAMERA                                    
         DC    F'17020'            OFF                                          
         DC    F'17530'                                                         
         DC    F'14990'                                                         
         DC    F'11970'                                                         
         DC    F'7620'                                                          
         DC    F'6170'                                                          
         DC    F'4475'                                                          
         SPACE 1                                                                
         DC    AL1(141,36,2,255)   UNITS 2+                                     
         DC    F'930'                                                           
         DC    F'930'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'30900'            ON CAMERA                                    
         DC    F'23200'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'28800'            SOLO/DUO ON CAMERA                           
         DC    F'21700'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(150,44,2,25)    UNITS 2-25                                   
         DC    F'1214'                                                          
         DC    F'830'                                                           
         DC    F'883'                                                           
         DC    F'762'                                                           
         DC    F'623'                                                           
         DC    F'313'                                                           
         DC    F'247'                                                           
         DC    F'205'                                                           
         DC    F'1134'                                                          
         DC    F'775'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,26,60)    UNITS 26-60                                 
         DC    F'450'                                                           
         DC    F'354'                                                           
         DC    F'456'                                                           
         DC    F'386'                                                           
         DC    F'320'                                                           
         DC    F'132'                                                           
         DC    F'90'                                                            
         DC    F'83'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(150,44,61,255)   UNITS 61+                                   
         DC    F'410'                                                           
         DC    F'325'                                                           
         DC    F'330'                                                           
         DC    F'257'                                                           
         DC    F'216'                                                           
         DC    F'80'                                                            
         DC    F'46'                                                            
         DC    F'46'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(151,36,1,1)     31 DAY - KS - UNIT 1                         
         DC    F'29050'            ON CAMERA                                    
         DC    F'21815'            OFF                                          
         DC    F'21640'                                                         
         DC    F'18385'                                                         
         DC    F'14630'                                                         
         DC    F'9555'                                                          
         DC    F'7500'                                                          
         DC    F'5680'                                                          
         SPACE 1                                                                
         DC    AL1(151,36,2,255)   UNITS 2+                                     
         DC    F'930'                                                           
         DC    F'930'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'38600'            ON CAMERA                                    
         DC    F'29000'            OFF                                          
         DC    F'21100'                                                         
         DC    F'18700'                                                         
         DC    F'15400'                                                         
         DC    F'12200'                                                         
         DC    F'10600'                                                         
         DC    F'8600'                                                          
         DC    F'28800'            SOLO/DUO ON CAMERA                           
         DC    F'21700'            OFF                                          
         SPACE 1                                                                
         DC    AL1(160,44,2,25)    UNITS 2-25                                   
         DC    F'1517'                                                          
         DC    F'1038'                                                          
         DC    F'883'                                                           
         DC    F'762'                                                           
         DC    F'623'                                                           
         DC    F'313'                                                           
         DC    F'247'                                                           
         DC    F'205'                                                           
         DC    F'1134'                                                          
         DC    F'775'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,26,60)    UNITS 26-60                                 
         DC    F'563'                                                           
         DC    F'442'                                                           
         DC    F'456'                                                           
         DC    F'386'                                                           
         DC    F'320'                                                           
         DC    F'132'                                                           
         DC    F'90'                                                            
         DC    F'83'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(160,44,61,255)    UNITS 61+                                  
         DC    F'563'                                                           
         DC    F'442'                                                           
         DC    F'330'                                                           
         DC    F'257'                                                           
         DC    F'216'                                                           
         DC    F'80'                                                            
         DC    F'46'                                                            
         DC    F'46'                                                            
         DC    F'421'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(161,36,1,1)      13 WEEKS - KS - UNIT 1                      
         DC    F'35430'            ON CAMERA                                    
         DC    F'26600'            OFF                                          
         DC    F'25635'                                                         
         DC    F'21885'                                                         
         DC    F'17530'                                                         
         DC    F'11245'                                                         
         DC    F'9070'                                                          
         DC    F'6775'                                                          
         SPACE 1                                                                
         DC    AL1(161,36,2,255)    UNITS 2+                                    
         DC    F'930'                                                           
         DC    F'930'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(162,36,1,1)      13 WEEKS - TX - UNIT 1                      
         DC    F'33800'             ON CAMERA                                   
         DC    F'24100'             OFF                                         
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         DC    F'22700'                                                         
         DC    F'13100'                                                         
         DC    F'13100'                                                         
         DC    F'13100'                                                         
         SPACE 1                                                                
         DC    AL1(162,36,2,13)     UNITS 2-13                                  
         DC    F'720'                                                           
         DC    F'530'                                                           
         DC    F'300'                                                           
         DC    F'300'                                                           
         DC    F'300'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         SPACE 1                                                                
         DC    AL1(162,36,14,255)   UNITS 14+                                   
         DC    F'495'                                                           
         DC    F'355'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'235'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         DC    F'120'                                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
         SPACE 3                                                                
         DC    AL1(170,32,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'11300'            ANN ALONE                                    
         DC    F'11300'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8900'             1-4M3,1-4S3,D3,S3                            
         DC    F'7800'             1-4M6,1-4S6,D6,S6                            
         DC    F'7000'             1-4M9,1-4S9,D9,S9                            
         DC    F'11300'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'12000'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(170,32,2,25)    UNITS 2-25                                   
         DC    F'171'                                                           
         DC    F'171'                                                           
         DC    F'94'                                                            
         DC    F'81'                                                            
         DC    F'72'                                                            
         DC    F'0'                                                             
         DC    F'182'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,26,60)   UNITS 26-60                                  
         DC    F'128'                                                           
         DC    F'128'                                                           
         DC    F'81'                                                            
         DC    F'62'                                                            
         DC    F'62'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,61,255)  UNITS 61+                                    
         DC    F'128'                                                           
         DC    F'128'                                                           
         DC    F'46'                                                            
         DC    F'33'                                                            
         DC    F'33'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'12100'            ANN ALONE                                    
         DC    F'12100'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8900'             1-4M3,1-4S3,D3,S3                            
         DC    F'7800'             1-4M6,1-4S6,D6,S6                            
         DC    F'7000'             1-4M9,1-4S9,D9,S9                            
         DC    F'12100'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'12000'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(175,32,2,25)    UNITS 2-25                                   
         DC    F'183'                                                           
         DC    F'183'                                                           
         DC    F'94'                                                            
         DC    F'81'                                                            
         DC    F'72'                                                            
         DC    F'0'                                                             
         DC    F'182'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,26,60)   UNITS 26-60                                  
         DC    F'137'                                                           
         DC    F'137'                                                           
         DC    F'81'                                                            
         DC    F'62'                                                            
         DC    F'62'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(175,32,61,255)  UNITS 61+                                    
         DC    F'137'                                                           
         DC    F'137'                                                           
         DC    F'46'                                                            
         DC    F'33'                                                            
         DC    F'33'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(176,28,0,0)     1 WEEK - KS - UNIT 1                         
         DC    F'9170'                                                          
         DC    F'9170'                                                          
         DC    F'5730'                                                          
         DC    F'4820'                                                          
         DC    F'4300'                                                          
         DC    F'14235'            SE                                           
         SPACE 1                                                                
         DC    AL1(176,28,2,255)   UNITS 2+                                     
         DC    F'305'                                                           
         DC    F'305'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'305'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'12900'            ANN ALONE                                    
         DC    F'12900'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8900'             1-4M3,1-4S3,D3,S3                            
         DC    F'7800'             1-4M6,1-4S6,D6,S6                            
         DC    F'7000'             1-4M9,1-4S9,D9,S9                            
         DC    F'12900'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'12000'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(185,32,2,25)    UNITS 2-25                                   
         DC    F'195'                                                           
         DC    F'195'                                                           
         DC    F'94'                                                            
         DC    F'81'                                                            
         DC    F'72'                                                            
         DC    F'0'                                                             
         DC    F'182'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,26,60)   UNITS 26-60                                  
         DC    F'146'                                                           
         DC    F'146'                                                           
         DC    F'81'                                                            
         DC    F'62'                                                            
         DC    F'62'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(185,32,61,255)  UNITS 61+                                    
         DC    F'146'                                                           
         DC    F'146'                                                           
         DC    F'46'                                                            
         DC    F'33'                                                            
         DC    F'33'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(186,28,0,0)     31 DAY - KS - UNIT 1                         
         DC    F'11755'                                                         
         DC    F'11755'                                                         
         DC    F'6905'                                                          
         DC    F'6125'                                                          
         DC    F'5470'                                                          
         DC    F'14235'            SE                                           
         SPACE 1                                                                
         DC    AL1(186,28,2,255)   UNITS 2+                                     
         DC    F'305'                                                           
         DC    F'305'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'305'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'16100'            ANN ALONE                                    
         DC    F'16100'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'8900'             1-4M3,1-4S3,D3,S3                            
         DC    F'7800'             1-4M6,1-4S6,D6,S6                            
         DC    F'7000'             1-4M9,1-4S9,D9,S9                            
         DC    F'16100'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'12000'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(195,32,2,25)    UNITS 2-25                                   
         DC    F'244'                                                           
         DC    F'244'                                                           
         DC    F'94'                                                            
         DC    F'81'                                                            
         DC    F'72'                                                            
         DC    F'0'                                                             
         DC    F'182'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,26,60)   UNITS 26-60                                  
         DC    F'183'                                                           
         DC    F'183'                                                           
         DC    F'81'                                                            
         DC    F'62'                                                            
         DC    F'62'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(195,32,61,255)  UNITS 61+                                    
         DC    F'183'                                                           
         DC    F'183'                                                           
         DC    F'46'                                                            
         DC    F'33'                                                            
         DC    F'33'                                                            
         DC    F'0'                                                             
         DC    F'137'                                                           
         SPACE 1                                                                
         DC    AL1(196,28,0,0)     13 WEEK - KS - UNIT 1                        
         DC    F'14330'                                                         
         DC    F'14330'                                                         
         DC    F'8080'                                                          
         DC    F'7165'                                                          
         DC    F'6385'                                                          
         DC    F'14235'            SE                                           
         SPACE 1                                                                
         DC    AL1(196,28,2,255)   UNITS 2+                                     
         DC    F'305'                                                           
         DC    F'305'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'305'                                                           
         SPACE 1                                                                
         DC    AL1(197,28,0,0)     13 WEEK - TX - UNIT 1                        
         DC    F'14600'                                                         
         DC    F'14600'                                                         
         DC    F'9300'                                                          
         DC    F'9300'                                                          
         DC    F'9300'                                                          
         DC    F'14600'                                                         
         SPACE 1                                                                
         DC    AL1(197,28,2,255)   UNITS 2+                                     
         DC    F'125'                                                           
         DC    F'125'                                                           
         DC    F'95'                                                            
         DC    F'95'                                                            
         DC    F'95'                                                            
         DC    F'125'                                                           
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
         DC    AL1(50),AL3(13080)                                               
         DC    AL1(50),AL3(9910)                                                
         DC    AL1(50),AL3(13080)                                               
         DC    AL1(50),AL3(13080)                                               
         DC    AL1(50),AL3(13080)                                               
         DC    AL1(50),AL3(9910)                                                
         DC    AL1(50),AL3(9910)                                                
         DC    AL1(50),AL3(9910)                                                
         SPACE                                                                  
         DC    AL1(44,36,4,13)     UNITS 4-13                                   
         DC    AL1(25),AL3(13080)                                               
         DC    AL1(25),AL3(9910)                                                
         DC    AL1(25),AL3(13080)                                               
         DC    AL1(25),AL3(13080)                                               
         DC    AL1(25),AL3(13080)                                               
         DC    AL1(25),AL3(9910)                                                
         DC    AL1(25),AL3(9910)                                                
         DC    AL1(25),AL3(9910)                                                
         SPACE                                                                  
         DC    AL1(44,36,14,255)   UNITS 14+                                    
         DC    AL1(15),AL3(13080)                                               
         DC    AL1(15),AL3(9910)                                                
         DC    AL1(15),AL3(13080)                                               
         DC    AL1(15),AL3(13080)                                               
         DC    AL1(15),AL3(13080)                                               
         DC    AL1(15),AL3(9910)                                                
         DC    AL1(15),AL3(9910)                                                
         DC    AL1(15),AL3(9910)                                                
*&&                                                                             
         EJECT                                                                  
         DC    AL1(53,72,1,24)     TV TAGS - REGULAR, UNITS 1-24                
         DC    F'13080'                                                         
         DC    F'9910'                                                          
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'0'                N/D                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         SPACE                                                                  
         DC    AL1(53,72,25,49)    UNITS 25-49                                  
         DC    F'7300'                                                          
         DC    F'5500'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'0'                N/D                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         SPACE                                                                  
         DC    AL1(53,72,50,255)   UNITS 50+                                    
         DC    F'4000'                                                          
         DC    F'3000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'0'                N/D                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         SPACE 1                                                                
         DC    AL1(54,72,1,1)      TV TAGS - W/1 SESS FEE                       
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         DC    F'18795'                                                         
         DC    F'16310'                                                         
         DC    F'13300'                                                         
         DC    F'24000'                                                         
         DC    F'36625'                                                         
         DC    F'13930'                                                         
         DC    F'24360'                                                         
         DC    F'68270'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'27290'            SE                                           
         DC    F'7105'             C3,C6                                        
         DC    F'14015'            C9                                           
         SPACE                                                                  
         DC    AL1(54,72,2,25)     UNITS 2-25                                   
         DC    F'13080'                                                         
         DC    F'9910'                                                          
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'13080'                                                         
         DC    F'0'                N/D                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         DC    F'9910'                                                          
         SPACE                                                                  
         DC    AL1(54,72,26,50)    UNITS 26-50                                  
         DC    F'7300'                                                          
         DC    F'5500'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'7300'                                                          
         DC    F'0'                N/D                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         DC    F'5500'                                                          
         SPACE                                                                  
         DC    AL1(54,72,51,255)   UNITS 51+                                    
         DC    F'4000'                                                          
         DC    F'3000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'4000'                                                          
         DC    F'0'                N/D                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         DC    F'3000'                                                          
         SPACE 1                                                                
         DC    AL1(55,36,1,255)    AFT RADIO TAGS - REGULAR                     
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         SPACE 1                                                                
         DC    AL1(56,36,1,1)      AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    F'18500'                                                         
         DC    F'18500'                                                         
         DC    F'13640'                                                         
         DC    F'12070'                                                         
         DC    F'10715'                                                         
         DC    F'14235'                                                         
         DC    F'6355'                                                          
         DC    F'10165'                                                         
         SPACE                                                                  
         DC    AL1(56,36,2,255)                                                 
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         DC    F'7660'                                                          
         EJECT                                                                  
INRUNLTB DC    AL1(236,36,0,0)     THEAT/INDUST REUSE-TV UNLIMITED USE          
         DC    AL1(160),AL3(44325)                                              
         DC    AL4(53330)          (1.6 X 33330, BUT WANT NEAREST .05)          
         DC    AL1(160),AL3(32450)                                              
         DC    AL1(160),AL3(28725)                                              
         DC    AL4(38015)          (1.6 X 23760, BUT WANT NEAREST .05)          
         DC    AL4(30070)          (1.6 X 18795, BUT WANT NEAREST .05)          
         DC    AL4(26095)          (1.6 X 16310, BUT WANT NEAREST .05)          
         DC    AL1(160),AL3(13300)                                              
         SPACE 3                                                                
         DC    AL1(235,24,0,0)     THEAT/INDUST REUSE-RAD UNLIMITED USE         
         DC    AL1(160),AL3(18500)                                              
         DC    AL1(160),AL3(18500)                                              
         DC    AL4(21825)          (1.6 X 13640, BUT WANT NEAREST .05)          
         DC    AL4(19310)          (1.6 X 12070, BUT WANT NEAREST .05)          
         DC    AL4(17145)          (1.6 X 10715, BUT WANT NEAREST .05)          
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
         SPACE 3                                                                
LCBTAB   DC    AL1(238,36,0,0)      1-50,000 SUBSCRIBERS                        
         DC    F'1830'                                                          
         DC    F'1250'                                                          
         DC    F'1430'                                                          
         DC    F'1230'                                                          
         DC    F'1000'                                                          
         DC    F'510'                                                           
         DC    F'395'                                                           
         DC    F'330'                                                           
         SPACE 1                                                                
         DC    AL1(239,36,0,0)      50,001-100,000 SUBSCRIBERS                  
         DC    F'3665'                                                          
         DC    F'2510'                                                          
         DC    F'2855'                                                          
         DC    F'2460'                                                          
         DC    F'2005'                                                          
         DC    F'1015'                                                          
         DC    F'790'                                                           
         DC    F'665'                                                           
         SPACE 1                                                                
         DC    AL1(240,36,0,0)      100,001-150,000 SUBSCRIBERS                 
         DC    F'5495'                                                          
         DC    F'3760'                                                          
         DC    F'4285'                                                          
         DC    F'3690'                                                          
         DC    F'3005'                                                          
         DC    F'1520'                                                          
         DC    F'1190'                                                          
         DC    F'995'                                                           
         SPACE 1                                                                
         DC    AL1(241,36,0,0)      150,001-200,000 SUBSCRIBERS                 
         DC    F'7325'                                                          
         DC    F'5015'                                                          
         DC    F'5715'                                                          
         DC    F'4920'                                                          
         DC    F'4015'                                                          
         DC    F'2035'                                                          
         DC    F'1585'                                                          
         DC    F'1330'                                                          
         SPACE 1                                                                
         DC    AL1(242,36,0,0)      200,001-250,000 SUBSCRIBERS                 
         DC    F'9155'                                                          
         DC    F'6270'                                                          
         DC    F'7140'                                                          
         DC    F'6155'                                                          
         DC    F'5015'                                                          
         DC    F'2535'                                                          
         DC    F'1985'                                                          
         DC    F'1665'                                                          
         SPACE 1                                                                
         DC    AL1(243,36,0,0)      250,001-500,000 SUBSCRIBERS                 
         DC    F'18315'                                                         
         DC    F'12540'                                                         
         DC    F'14290'                                                         
         DC    F'12305'                                                         
         DC    F'10030'                                                         
         DC    F'5070'                                                          
         DC    F'3965'                                                          
         DC    F'3320'                                                          
         SPACE 1                                                                
         DC    AL1(244,36,0,0)      500,001-750,000 SUBSCRIBERS                 
         DC    F'27465'                                                         
         DC    F'18805'                                                         
         DC    F'21430'                                                         
         DC    F'18460'                                                         
         DC    F'15045'                                                         
         DC    F'7610'                                                          
         DC    F'5950'                                                          
         DC    F'4985'                                                          
         SPACE 1                                                                
         DC    AL1(245,36,0,0)      750,001-1 MILLION SUBSCRIBERS               
         DC    F'36620'                                                         
         DC    F'25075'                                                         
         DC    F'28575'                                                         
         DC    F'24610'                                                         
         DC    F'20065'                                                         
         DC    F'10150'                                                         
         DC    F'7930'                                                          
         DC    F'6650'                                                          
         SPACE 1                                                                
         DC    AL1(246,36,0,0)      OVER 1 MILLION SUBSCRIBERS                  
         DC    F'44325'                                                         
         DC    F'33330'                                                         
         DC    F'32450'                                                         
         DC    F'28725'                                                         
         DC    F'23760'                                                         
         DC    F'18795'                                                         
         DC    F'16310'                                                         
         DC    F'13300'                                                         
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
         SPACE 1                                                                
         DC    AL1(247,72,1,24)    TX - TV, REGULAR, UNITS 1-24                 
         DC    F'10700'            ON CAMERA                                    
         DC    F'8100'             OFF                                          
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'8100'                                                          
         DC    F'8100'                                                          
         DC    F'8100'                                                          
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'10700'                                                         
         DC    F'0'                N/D                                          
         DC    F'8100'             SE                                           
         DC    F'8100'             C3,C6                                        
         DC    F'8100'             C9                                           
         SPACE 1                                                                
         DC    AL1(247,72,25,49)   UNITS 25-49                                  
         DC    F'6000'             ON CAMERA                                    
         DC    F'4500'             OFF                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'4500'                                                          
         DC    F'4500'                                                          
         DC    F'4500'                                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'6000'                                                          
         DC    F'0'                N/D                                          
         DC    F'4500'             SE                                           
         DC    F'4500'             C3,C6                                        
         DC    F'4500'             C9                                           
         SPACE 1                                                                
         DC    AL1(247,72,50,255)  UNITS 50+                                    
         DC    F'3300'             ON CAMERA                                    
         DC    F'2500'             OFF                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'2500'                                                          
         DC    F'2500'                                                          
         DC    F'2500'                                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'3300'                                                          
         DC    F'0'                N/D                                          
         DC    F'2500'             SE                                           
         DC    F'2500'             C3,C6                                        
         DC    F'2500'             C9                                           
         SPACE 1                                                                
         DC    AL1(248,36,1,24)    RADIO TAGS - REGULAR, UNITS 1-24             
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         DC    F'6850'                                                          
         SPACE 1                                                                
         DC    AL1(248,36,25,49)  UNITS 25-49                                   
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         DC    F'3800'                                                          
         SPACE 1                                                                
         DC    AL1(248,36,50,255) UNITS 50+                                     
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         DC    F'2100'                                                          
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
         SPACE 1                                                                
         DC    AL1(249,80,1,4)     GA - TV, REGULAR, UNITS 1-4                  
         DC    F'11400'            ON CAMERA                                    
         DC    F'8600'             OFF                                          
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'8600'                                                          
         DC    F'8600'                                                          
         DC    F'8600'                                                          
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'11400'                                                         
         DC    F'0'                N/D                                          
         DC    F'8600'             SE                                           
         DC    F'8600'             C3,C6                                        
         DC    F'8600'             C9                                           
         DC    F'11400'            SOLO/DUO ON CAMERA                           
         DC    F'8600'                      OFF                                 
         SPACE 1                                                                
         DC    AL1(249,80,5,14)    UNITS 5-14                                   
         DC    F'6400'             ON CAMERA                                    
         DC    F'4800'             OFF                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'4800'                                                          
         DC    F'4800'                                                          
         DC    F'4800'                                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'6400'                                                          
         DC    F'0'                N/D                                          
         DC    F'4800'             SE                                           
         DC    F'4800'             C3,C6                                        
         DC    F'4800'             C9                                           
         DC    F'6400'             SOLO/DUO ON CAMERA                           
         DC    F'4800'             OFF                                          
         SPACE 1                                                                
         DC    AL1(249,80,15,255)  UNITS 15+                                    
         DC    F'3500'             ON CAMERA                                    
         DC    F'2600'             OFF                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'2600'                                                          
         DC    F'2600'                                                          
         DC    F'2600'                                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'3500'                                                          
         DC    F'0'                N/D                                          
         DC    F'2600'             SE                                           
         DC    F'2600'             C3,C6                                        
         DC    F'2600'             C9                                           
         DC    F'3500'             SOLO/DUO ON CAMERA                           
         DC    F'2600'             OFF                                          
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
         SPACE 1                                                                
         DC    AL1(250,76,0,0)     PRM FOR SAG AND AFT AND PRR FOR SAG          
         DC    F'30500'            ON CAMERA                                    
         DC    F'21500'            OFF                                          
         DC    F'30500'            ON CAMERA                                    
         DC    F'30500'            ON CAMERA                                    
         DC    F'30500'            ON CAMERA                                    
         DC    F'17500'            OFF                                          
         DC    F'17500'            OFF                                          
         DC    F'17500'            OFF                                          
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'30500'            ON                                           
         DC    F'0'                N/D                                          
         DC    F'21500'            OFF                                          
         DC    F'21500'            OFF                                          
         DC    F'21500'            OFF                                          
         DC    F'21275'            SOLO/DUO OFF CAM                             
         SPACE 3                                                                
         DC    AL1(251,76,0,0)     PRR FOR AFT                                  
         DC    F'30500'            ON CAMERA                                    
         DC    F'21500'            OFF                                          
         DC    F'30500'            ON CAMERA                                    
         DC    F'30500'            ON CAMERA                                    
         DC    F'30500'            ON CAMERA                                    
         DC    F'18500'            OFF                                          
         DC    F'18500'            OFF                                          
         DC    F'18500'            OFF                                          
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'7500'             EXTRA                                        
         DC    F'30500'            ON                                           
         DC    F'0'                N/D                                          
         DC    F'21500'            OFF                                          
         DC    F'21500'            OFF                                          
         DC    F'21500'            OFF                                          
         DC    F'21275'            SOLO/DUO OFF CAM                             
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
         DC    AL1(61,USLF,ALL,AFT+NON,0,0,0,RADIO)   SPANISH LIFT              
         DC    AL1(62,USLF,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(64,USLF,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
*                                                                               
         DC    AL1(42,UDEM,ALL,ALL,0,0,0,TV)          DEMO                      
         DC    AL1(43,UDEM,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(42,USNA,ALL,ALL,0,0,0,TV)          SPANISH DEMO              
         DC    AL1(43,USNA,ALL,AFT+NON,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(63,UHLD,ALL,ALL-AFM,0,0,0,TV)      HOLDING FEE               
         DC    AL1(63,USHL,ALL,ALL-AFM,0,0,0,TV)      SPAN HLD FEE              
         DC    AL1(38,UREN,ALL,ALL-AFM,0,0,0,TV)      REINST                    
         DC    AL1(38,USRE,ALL,ALL-AFM,0,0,0,TV)      REINST                    
         DC    AL1(225,UADH,ALL,ALL-AFM,0,0,0,TV)     ADDEN HLD FEE             
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
         DC    AL1(33,URRN,ALL,ALL,0,0,0,RADIO)       RAD REG NETWORK           
*                                                                               
         DC    AL1(39,URLO,ALL,ALL,0,0,0,ALL)         RAD LCL 13WK              
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
         DC    AL1(47,UFGM,UFGMEU24,ALL,0,0,0,ALL)    EUROPE 24M                
         DC    AL1(47,UFGM,UFGMNE24,ALL,0,0,0,ALL)    NOT EUR 24M               
         DC    AL1(46,UFGM,UFGMWO12,ALL,0,0,0,ALL)    WORLD 12M                 
         DC    AL1(48,UFGM,UFGMWO24,ALL,0,0,0,ALL)    WORLD 24M                 
*                                                                               
         DC    AL1(50,UFGR,UFGRUK,ALL,0,0,0,TV)       FGN-UK                    
         DC    AL1(51,UFGR,UFGREUR,ALL,0,0,0,TV)      EUR W/O UK                
         DC    AL1(62,UFGR,UFGRWOR,ALL,0,0,0,TV)      WRLD W/O UK&EUR           
         DC    AL1(62,UFGR,UFGRAP,ALL,0,0,0,TV)       ASIAN PAC                 
         DC    AL1(62,UFGR,UFGRJAP,ALL,0,0,0,TV)      JAPAN                     
         DC    AL1(237,UFGR,UFGRWIDE,ALL,0,0,0,TV)    WORLDWIDE                 
         DC    AL1(62,UFGR,UFGRMAJ,ALL,0,0,0,TV)      NEW-W/MAJORS              
         DC    AL1(49,UFGR,UFGRRAD,ALL,0,0,0,RADIO)   RADIO                     
*                                                                               
         DC    AL1(62,UPBS,ALL,ALL-AFM,0,0,0,TV)      PUB SERV                  
         DC    AL1(52,UPBS,ALL,ALL-AFM,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(40,USNT,ALL,ALL,0,0,0,TV)          SPAN NETWORK              
         DC    AL1(40,USNW,ALL,ALL,0,0,0,TV)          SP NWK/WSP COMB           
         DC    AL1(10,USWS,ALL,ALL,0,0,0,TV)          SPAN WSP                  
*                                                                               
         DC    AL1(65,UADT,UADT3D,ALL,0,0,0,ALL)      AD SES-TV-3D              
         DC    AL1(70,UADT,UADT1W,ALL,0,0,0,ALL)      1 WEEK                    
         DC    AL1(80,UADT,UADT4W,ALL,0,0,0,ALL)      4 WEEK                    
         DC    AL1(80,UADT,UADT31D,ALL,0,0,0,ALL)     31D(KS ONLY)              
         DC    AL1(90,UADT,UADT13W,ALL,0,0,0,ALL)     13 WEEK                   
*                                                                               
         DC    AL1(100,UADO,UADO3D,ALL,0,0,0,ALL)     AD SES-RAD-3DY            
         DC    AL1(105,UADO,UADO1W,ALL,0,0,0,ALL)     1 WEEK                    
         DC    AL1(115,UADO,UADO4W,ALL,0,0,0,ALL)     4 WEEK                    
         DC    AL1(115,UADO,UADO31D,ALL,0,0,0,ALL)    31D(KS ONLY)              
         DC    AL1(125,UADO,UADO13W,ALL,0,0,0,ALL)    13 WEEK                   
*                                                                               
         DC    AL1(205,UADD,ALL,ALL,0,0,0,TV)         ADDENDUM DEMO             
         DC    AL1(215,UADD,ALL,AFT+NON,0,0,0,RADIO)                            
*                                                                               
         DC    AL1(135,UADW,UADW3D,ALL,0,0,0,TV)     AD WSP-TV-3D               
         DC    AL1(140,UADW,UADW1W,ALL,0,0,0,TV)     1 WEEK                     
         DC    AL1(150,UADW,UADW4W,ALL,0,0,0,TV)     4 WEEK                     
         DC    AL1(150,UADW,UADW31D,ALL,0,0,0,TV)    31 DAY(KS ONLY)            
         DC    AL1(160,UADW,UADW13W,ALL,0,0,0,TV)    13 WEEK                    
*                                                                               
         DC    AL1(170,UADW,UADW3D,ALL,0,0,0,RADIO)   AD WSP-RAD-3D             
         DC    AL1(175,UADW,UADW1W,ALL,0,0,0,RADIO)   1 WEEK                    
         DC    AL1(185,UADW,UADW4W,ALL,0,0,0,RADIO)   4 WEEK                    
         DC    AL1(185,UADW,UADW31D,ALL,0,0,0,RADIO)  31 DAY(KS ONLY)           
         DC    AL1(195,UADW,UADW13W,ALL,0,0,0,RADIO)  13 WEEK                   
*                                                                               
         DC    AL1(135,UADC,UADC3D,ALL,0,0,0,TV)     CMB S/W-TV-3D              
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
         DC    AL1(62,UINR,UINR30D,ALL,0,0,0,TV)      TH/IND-30D                
         DC    AL1(236,UINR,UINRUNL,ALL,0,0,0,TV)     UNL USE                   
         DC    AL1(61,UINR,UINR30D,ALL,0,0,0,RADIO)   30 DAYS-RAD               
         DC    AL1(235,UINR,UINRUNL,ALL,0,0,0,RADIO)  UNL USE-RAD               
         DC    AL1(62,USIN,USIN30D,ALL,0,0,0,TV)      SP IND                    
         DC    AL1(236,USIN,USINUNL,ALL,0,0,0,TV)     UNLIMITED                 
         DC    AL1(61,USIN,USIN30D,ALL,0,0,0,RADIO)   30 DAYS-RAD               
         DC    AL1(235,USIN,USINUNL,ALL,0,0,0,RADIO)  UNL USE-RAD               
*                                                                               
         DC    AL1(238,ULCB,ULCB50,ALL,0,0,0,ALL)     LOCAL CABLE               
         DC    AL1(239,ULCB,ULCB100,ALL,0,0,0,ALL)                              
         DC    AL1(240,ULCB,ULCB150,ALL,0,0,0,ALL)                              
         DC    AL1(241,ULCB,ULCB200,ALL,0,0,0,ALL)                              
         DC    AL1(242,ULCB,ULCB250,ALL,0,0,0,ALL)                              
         DC    AL1(243,ULCB,ULCB500,ALL,0,0,0,ALL)                              
         DC    AL1(244,ULCB,ULCB750,ALL,0,0,0,ALL)                              
         DC    AL1(245,ULCB,ULCB1M,ALL,0,0,0,ALL)                               
         DC    AL1(246,ULCB,ULCBMAX,ALL,0,0,0,ALL)                              
*                                                                               
         DC    AL1(250,UPRM,ALL,ALL,0,0,0,TV)                                   
         DC    AL1(250,UPRR,ALL,SAG,0,0,0,TV)                                   
         DC    AL1(251,UPRR,ALL,AFT,0,0,0,TV)                                   
*                                                                               
******  USE NUMBERS 34 USED FOR RRN WITH MAJORS                                 
******  USE NUMBERS 247-249 USED FOR TAGS FOR TX AND GA ADDENDUM                
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
         DC    F'13080'            TV SESSION TAG FEE - ON CAMERA               
         DC    F'9910'             TV SESSION TAG FEE - OFF                     
         DC    F'7660'             RADIO SESSION TAG FEE                        
         DC    F'11400'            GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'8600'             GA ADDEN TV SESSION TAG FEE - OFF            
         DC    F'6700'             GA ADDENDUM RADIO SESSION TAG FEE            
         DC    F'6775'             KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'4475'             KS ADDEN TV SESSION TAG FEE - OFF            
         DC    F'2605'             KS ADDENDUM RADIO SESSION TAG FEE            
         DC    F'10700'            TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    F'8100'             TX ADDEN TV SESSION TAG FEE - OFF            
         DC    F'6850'             TX ADDENDUM RADIO SESSION TAG FEE            
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
         DC    AL1(3,CTGS3)                                                     
         DC    AL1(3,CTGSS)                                                     
         DC    AL1(3,CTGSM)                                                     
*                                                                               
         DC    AL1(4,CTG6)                                                      
         DC    AL1(4,CTGS6)                                                     
         DC    AL1(4,CTG6M)                                                     
         DC    AL1(4,CTGD6)                                                     
*                                                                               
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTGS9)                                                     
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
**PAN#1  DC    CL21'019TAGEN6D   10/05/11'                                      
         END                                                                    
