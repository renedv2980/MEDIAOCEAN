*          DATA SET TAGEN69    AT LEVEL 019 AS OF 04/03/09                      
*PHASE T70269A                                                                  
         TITLE 'T70269 - TABLES FOR 1987-88 CONTRACTS'                          
T70269   CSECT                                                                  
         DC    AL4(USETBLS-T70269)                                              
         DC    AL4(USELUT-T70269)                                               
         DC    AL4(MAJLUT-T70269)                                               
         DC    AL4(AFMCOLS-T70269)                                              
         DC    AL4(RADCOLS-T70269)                                              
         DC    AL4(OFFCOLS-T70269)                                              
         DC    AL4(ONCOLS-T70269)                                               
         DC    AL4(MSWEET-T70269)                                               
         DC    AL4(TAGFEE-T70269)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL1(0,36,1,1)       CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    F'36660'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'27565'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'26835'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'23755'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'19650'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'15545'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'13490'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'11000'            'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    F'86790'                                                         
         DC    F'62070'                                                         
         DC    F'55275'                                                         
         DC    F'48880'                                                         
         DC    F'39960'                                                         
         DC    F'20365'                                                         
         DC    F'16975'                                                         
         DC    F'13875'                                                         
         SPACE 1                                                                
CBXTAB   DC    AL1(2,36,0,0)       CLASS B W/O NY                               
         DC    F'70790'                                                         
         DC    F'49165'                                                         
         DC    F'55275'                                                         
         DC    F'48880'                                                         
         DC    F'39960'                                                         
         DC    F'20365'                                                         
         DC    F'16975'                                                         
         DC    F'13875'                                                         
         SPACE 1                                                                
CLCTAB   DC    AL1(3,36,0,0)       CLASS C                                      
         DC    F'42185'                                                         
         DC    F'28125'                                                         
         DC    F'36560'                                                         
         DC    F'32490'                                                         
         DC    F'26570'                                                         
         DC    F'16200'                                                         
         DC    F'13480'                                                         
         DC    F'11060'                                                         
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
         SPACE 3                                                                
DANTAB   DC    AL1(4,36,0,0)       DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    F'172140'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'119900'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'129255'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'113935'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'88555'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'52840'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'46275'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'33040'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL1(5,36,0,0)       DEALER A W/O NY                              
         DC    F'152240'                                                        
         DC    F'109955'                                                        
         DC    F'129255'                                                        
         DC    F'113955'                                                        
         DC    F'88555'                                                         
         DC    F'52840'                                                         
         DC    F'46275'                                                         
         DC    F'33040'                                                         
         SPACE 1                                                                
DBNTAB   DC    AL1(6,36,0,0)       CLASS B INCL NY                              
         DC    F'264680'                                                        
         DC    F'180105'                                                        
         DC    F'196520'                                                        
         DC    F'173235'                                                        
         DC    F'134825'                                                        
         DC    F'80500'                                                         
         DC    F'70455'                                                         
         DC    F'50255'                                                         
         SPACE 1                                                                
DBXTAB   DC    AL1(7,36,0,0)       CLASS B W/O NY                               
         DC    F'228365'                                                        
         DC    F'164675'                                                        
         DC    F'196520'                                                        
         DC    F'173235'                                                        
         DC    F'134825'                                                        
         DC    F'80500'                                                         
         DC    F'70455'                                                         
         DC    F'50255'                                                         
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
         SPACE 3                                                                
G13TAB   DC    AL1(8,36,1,1)       13 USE                                       
         DC    F'133340'          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS              
         DC    F'103535'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'112045'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'100100'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'82175'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'68415'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'59560'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'48745'            'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    F'4007'                                                          
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
         DC    F'36660'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'27565'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'26835'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'23755'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'19650'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'15545'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'13490'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'11000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(10,36,2,25)     UNITS 2-25                                   
         DC    F'1495'                                                          
         DC    F'1025'                                                          
         DC    F'1165'                                                          
         DC    F'1005'                                                          
         DC    F'820'                                                           
         DC    F'415'                                                           
         DC    F'325'                                                           
         DC    F'270'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,26,60)    UNITS 26-60                                  
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'600'                                                           
         DC    F'510'                                                           
         DC    F'420'                                                           
         DC    F'175'                                                           
         DC    F'120'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,61,125)   UNITS 61-125                                 
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'435'                                                           
         DC    F'340'                                                           
         DC    F'285'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,126,255)  UNITS 126+                                   
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'215'                                                           
         DC    F'175'                                                           
         DC    F'150'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL1(11,36,0,0)      NY ALONE                                     
         DC    F'85855'                                                         
         DC    F'60650'                                                         
         DC    F'54980'                                                         
         DC    F'48835'                                                         
         DC    F'40015'                                                         
         DC    F'22055'                                                         
         DC    F'18275'                                                         
         DC    F'14965'                                                         
         SPACE 1                                                                
         DC    AL1(11,36,1,35)     UNITS 1-35                                   
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'600'                                                           
         DC    F'510'                                                           
         DC    F'420'                                                           
         DC    F'175'                                                           
         DC    F'120'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(11,36,36,100)   UNITS 36-100                                 
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'435'                                                           
         DC    F'340'                                                           
         DC    F'285'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,101,255)  UNITS 101+                                   
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'215'                                                           
         DC    F'175'                                                           
         DC    F'150'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL1(12,36,0,0)      CHI OR LA ALONE                              
         DC    F'74830'                                                         
         DC    F'52775'                                                         
         DC    F'54980'                                                         
         DC    F'48835'                                                         
         DC    F'40015'                                                         
         DC    F'22055'                                                         
         DC    F'18275'                                                         
         DC    F'14965'                                                         
         SPACE 1                                                                
         DC    AL1(12,36,1,35)     UNITS 1-35                                   
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'600'                                                           
         DC    F'510'                                                           
         DC    F'420'                                                           
         DC    F'175'                                                           
         DC    F'120'                                                           
         DC    F'110'                                                           
         SPACE 1                                                                
         DC    AL1(12,36,36,100)   UNITS 36-100                                 
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'435'                                                           
         DC    F'340'                                                           
         DC    F'285'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,101,255)  UNITS 101+                                   
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'215'                                                           
         DC    F'175'                                                           
         DC    F'150'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL1(13,36,0,0)      TWO OF NY LA CHI                             
         DC    F'118145'                                                        
         DC    F'79550'                                                         
         DC    F'84590'                                                         
         DC    F'69945'                                                         
         DC    F'57185'                                                         
         DC    F'29145'                                                         
         DC    F'23475'                                                         
         DC    F'19225'                                                         
         SPACE 1                                                                
         DC    AL1(13,36,1,255)    UNITS 1+                                     
         DC    F'555'                                                           
         DC    F'435'                                                           
         DC    F'215'                                                           
         DC    F'175'                                                           
         DC    F'150'                                                           
         DC    F'105'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         SPACE 1                                                                
         DC    AL1(14,36,0,0)      ALL THREE MAJORS                             
         DC    F'142510'                                                        
         DC    F'101215'                                                        
         DC    F'106720'                                                        
         DC    F'91335'                                                         
         DC    F'74650'                                                         
         DC    F'35140'                                                         
         DC    F'28340'                                                         
         DC    F'23160'                                                         
         SPACE 1                                                                
         DC    AL1(14,36,1,255)    UNITS 1+                                     
         DC    F'570'                                                           
         DC    F'445'                                                           
         DC    F'220'                                                           
         DC    F'180'                                                           
         DC    F'155'                                                           
         DC    F'110'                                                           
         DC    F'65'                                                            
         DC    F'65'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL1(15,28,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    F'14200'            ANN ALONE                                    
         DC    F'14200'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10470'            1-4M3,1-4S3,D3,S3                            
         DC    F'9265'             1-4M6,1-4S6,D6,S6                            
         DC    F'8225'             1-4M9,1-4S9,D9,S9                            
         DC    F'5595'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL1(15,24,2,25)     UNITS 2-25                                   
         DC    F'255'                                                           
         DC    F'255'                                                           
         DC    F'130'                                                           
         DC    F'115'                                                           
         DC    F'100'                                                           
         SPACE 1                                                                
         DC    AL1(15,24,26,60)    UNITS 26-60                                  
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'110'                                                           
         DC    F'80'                                                            
         DC    F'80'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,61,255)   UNITS 61+                                    
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'60'                                                            
         DC    F'45'                                                            
         DC    F'45'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(16,28,0,0)      NEW YORK ALONE                               
         DC    F'24915'                                                         
         DC    F'24915'                                                         
         DC    F'13550'                                                         
         DC    F'10520'                                                         
         DC    F'9365'                                                          
         DC    F'5595'                                                          
         SPACE 1                                                                
         DC    AL1(16,24,1,35)     UNITS 1-35                                   
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'110'                                                           
         DC    F'80'                                                            
         DC    F'80'                                                            
         SPACE 1                                                                
         DC    AL1(16,24,36,255)   UNITS 36+                                    
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'60'                                                            
         DC    F'45'                                                            
         DC    F'45'                                                            
         SPACE 1                                                                
         DC    AL1(17,28,0,0)      CHICAGO OR LA ALONE                          
         DC    F'22590'                                                         
         DC    F'22590'                                                         
         DC    F'13550'                                                         
         DC    F'10520'                                                         
         DC    F'9365'                                                          
         DC    F'5595'                                                          
         SPACE 1                                                                
         DC    AL1(17,24,1,35)     UNITS 1-35                                   
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'110'                                                           
         DC    F'80'                                                            
         DC    F'80'                                                            
         SPACE 1                                                                
         DC    AL1(17,24,36,255)   UNITS 36+                                    
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'60'                                                            
         DC    F'45'                                                            
         DC    F'45'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(18,28,0,0)      ANY TWO ALONE                                
         DC    F'30385'                                                         
         DC    F'30385'                                                         
         DC    F'16180'                                                         
         DC    F'12415'                                                         
         DC    F'11050'                                                         
         DC    F'5595'                                                          
         SPACE 1                                                                
         DC    AL1(18,24,1,255)    UNITS 1+                                     
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'60'                                                            
         DC    F'45'                                                            
         DC    F'45'                                                            
         SPACE 1                                                                
         DC    AL1(19,28,0,0)      ALL THREE ALONE                              
         DC    F'38390'                                                         
         DC    F'38390'                                                         
         DC    F'18025'                                                         
         DC    F'13950'                                                         
         DC    F'12415'                                                         
         DC    F'5595'                                                          
         SPACE 1                                                                
         DC    AL1(19,24,1,255)    UNITS 1+                                     
         DC    F'180'                                                           
         DC    F'180'                                                           
         DC    F'60'                                                            
         DC    F'45'                                                            
         DC    F'45'                                                            
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL1(20,24,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(14200) ANN ALONE                                    
         DC    AL1(100),AL3(14200) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(10470) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(9265)  1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(8225)  1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL1(20,24,2,25)     UNITS 2-25                                   
         DC    AL1(80),AL3(255)                                                 
         DC    AL1(80),AL3(255)                                                 
         DC    AL1(95),AL3(130)                                                 
         DC    AL1(95),AL3(115)                                                 
         DC    AL1(95),AL3(100)                                                 
         SPACE 1                                                                
         DC    AL1(20,24,26,60)    UNITS 26-60                                  
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(110)                                                 
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,61,255)   UNITS 61+                                    
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(45)                                                  
         DC    AL1(95),AL3(45)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(21,24,0,0)      NEW YORK ALONE                               
         DC    AL1(80),AL3(24910)                                               
         DC    AL1(80),AL3(24910)                                               
         DC    AL1(95),AL3(13550)                                               
         DC    AL1(95),AL3(10520)                                               
         DC    AL1(95),AL3(9365)                                                
         SPACE 1                                                                
         DC    AL1(21,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(110)                                                 
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         SPACE 1                                                                
         DC    AL1(21,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(45)                                                  
         DC    AL1(95),AL3(45)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,0,0)      CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(22590)                                               
         DC    AL1(80),AL3(22590)                                               
         DC    AL1(95),AL3(13550)                                               
         DC    AL1(95),AL3(10520)                                               
         DC    AL1(95),AL3(9365)                                                
         SPACE 1                                                                
         DC    AL1(22,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(110)                                                 
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(45)                                                  
         DC    AL1(95),AL3(45)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(23,24,0,0)      ANY TWO ALONE                                
         DC    AL1(80),AL3(30385)                                               
         DC    AL1(80),AL3(30385)                                               
         DC    AL1(95),AL3(16180)                                               
         DC    AL1(95),AL3(12415)                                               
         DC    AL1(95),AL3(11050)                                               
         SPACE 1                                                                
         DC    AL1(23,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(45)                                                  
         DC    AL1(95),AL3(45)                                                  
         SPACE 1                                                                
         DC    AL1(24,24,0,0)      ALL THREE ALONE                              
         DC    AL1(80),AL3(38390)                                               
         DC    AL1(80),AL3(38390)                                               
         DC    AL1(95),AL3(18025)                                               
         DC    AL1(95),AL3(13950)                                               
         DC    AL1(95),AL3(12415)                                               
         SPACE 1                                                                
         DC    AL1(24,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(80),AL3(180)                                                 
         DC    AL1(95),AL3(60)                                                  
         DC    AL1(95),AL3(45)                                                  
         DC    AL1(95),AL3(45)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TALES - RADIO                                 
         SPACE 3                                                                
DLRTAB   DC    AL1(25,28,0,0)      DEALER COMMERCIALS                           
         DC    F'54475'            AR,AS,P,ANN                                  
         DC    F'43210'            S,1-4MS,1-4SS                                
         DC    F'28175'            1-4M3,1-4S3,D3,S3                            
         DC    F'22540'            1-4M6,1-4S6,D6,S6                            
         DC    F'14085'            1-4M9,1-4S9,D9,S9                            
         DC    F'13255'            SE                                           
         SPACE 1                                                                
N01TAB   DC    AL1(26,28,0,0)      NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    F'33415'            ANN ALONE                                    
         DC    F'33415'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'25070'            1-4M3,1-4S3,D3,S3                            
         DC    F'25070'            1-4M6,1-4S6,D6,S6                            
         DC    F'25070'            1-4M9,1-4S9,D9,S9                            
         DC    F'7285'             SE                                           
         SPACE 1                                                                
N04TAB   DC    AL1(27,28,0,0)      NETWORK 4 WEEK                               
         DC    F'78175'                                                         
         DC    F'54220'                                                         
         DC    F'41690'                                                         
         DC    F'37285'                                                         
         DC    F'34060'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
N08TAB   DC    AL1(28,28,0,0)      NETWORK 8 WEEK                               
         DC    F'86370'                                                         
         DC    F'86370'                                                         
         DC    F'66450'                                                         
         DC    F'59390'                                                         
         DC    F'53195'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
N13TAB   DC    AL1(29,28,0,0)      NETWORK 13 WEEK                              
         DC    F'107170'                                                        
         DC    F'107170'                                                        
         DC    F'82425'                                                         
         DC    F'73700'                                                         
         DC    F'67520'                                                         
         DC    F'7285'                                                          
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL1(30,28,0,0)      ACROSS-THE-BOARD                             
         DC    F'112215'                                                        
         DC    F'112215'                                                        
         DC    F'86295'                                                         
         DC    F'77170'                                                         
         DC    F'70700'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
U26TAB   DC    AL1(31,28,0,0)      26 USE LIMIT                                 
         DC    F'80065'                                                         
         DC    F'53590'                                                         
         DC    F'41205'                                                         
         DC    F'36850'                                                         
         DC    F'33670'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
U39TAB   DC    AL1(32,28,0,0)      39 USE LIMIT                                 
         DC    F'80700'                                                         
         DC    F'80700'                                                         
         DC    F'56505'                                                         
         DC    F'50435'                                                         
         DC    F'45825'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
R13TAB   DC    AL1(33,28,0,0)      REGIONAL - NO MAJORS                         
         DC    F'92960'                                                         
         DC    F'64670'                                                         
         DC    F'30315'                                                         
         DC    F'30315'                                                         
         DC    F'30315'                                                         
         DC    F'7285'                                                          
         SPACE 1                                                                
         DC    AL1(34,28,0,0)      REGIONAL - WITH ANY MAJORS                   
         DC    F'92960'                                                         
         DC    F'64670'                                                         
         DC    F'64670'                                                         
         DC    F'58205'                                                         
         DC    F'52345'                                                         
         DC    F'7285'                                                          
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'5850'             CAST=1                                       
         DC    F'6325'                  2-4                                     
         DC    F'5850'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'7800'             CAST=1                                       
         DC    F'8430'                  2-4                                     
         DC    F'7800'                  5+                                      
         SPACE 3                                                                
MS8TAB   DC    AL1(95,16,0,0)      8-WEEK REUSE                                 
         DC    AL1(80),AL3(5850)                                                
         DC    AL1(80),AL3(6325)                                                
         DC    AL1(80),AL3(5850)                                                
         EJECT                                                                  
*              MUSIC FOREIGN USE FOR 89 CONTRACT                                
         SPACE 2                                                                
FGMTAB   DC    AL1(96,16,0,0)      EUROPE OR OUTSIDE EUROPE-12M                 
         DC    F'5000'             CAST=1                                       
         DC    F'5000'                  2-4                                     
         DC    F'5000'                  5+                                      
         SPACE                                                                  
         DC    AL1(97,16,0,0)      WORLD - 12M                                  
         DC    F'8000'             CAST=1                                       
         DC    F'8000'                  2-4                                     
         DC    F'8000'                  5+                                      
         SPACE 3                                                                
         DC    AL1(98,16,0,0)      EUROPE OR OUTSIDE EUROPE-24M                 
         DC    F'7500'             CAST=1                                       
         DC    F'7500'                  2-4                                     
         DC    F'7500'                  5+                                      
         SPACE                                                                  
         DC    AL1(99,16,0,0)      WORLD - 24M                                  
         DC    F'12000'            CAST=1                                       
         DC    F'12000'                2-4                                      
         DC    F'12000'                5+                                       
         SPACE 3                                                                
*              MUSIC FOREIGN USE FOR 87 CONTRACT                                
         SPACE 2                                                                
         DC    AL1(100,16,0,0)     EUROPE OR OUTSIDE EUROPE-12M                 
         DC    F'4465'             CAST=1                                       
         DC    F'4465'                  2-4                                     
         DC    F'4465'                  5+                                      
         SPACE                                                                  
         DC    AL1(101,16,0,0)     WORLD - 12M                                  
         DC    F'7150'             CAST=1                                       
         DC    F'7150'                  2-4                                     
         DC    F'7150'                  5+                                      
         SPACE 3                                                                
         DC    AL1(102,16,0,0)     EUROPE OR OUTSIDE EUROPE-24M                 
         DC    F'6700'             CAST=1                                       
         DC    F'6700'                  2-4                                     
         DC    F'6700'                  5+                                      
         SPACE                                                                  
         DC    AL1(103,16,0,0)     WORLD - 24M                                  
         DC    F'10725'            CAST=1                                       
         DC    F'10725'                2-4                                      
         DC    F'10725'                5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 1                                                                
         DC    AL1(61,36,0,0)      AFT RADIO BASE SESSION RATES                 
         DC    F'14200'            ANN ALONE                                    
         DC    F'14200'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10470'            1-4M3,1-4S3,D3,S3                            
         DC    F'9265'             1-4M6,1-4S6,D6,S6                            
         DC    F'8225'             1-4M9,1-4S9,D9,S9                            
         DC    F'10925'            SE                                           
         DC    F'4880'             C3,C6                                        
         DC    F'7805'             C9                                           
         SPACE 1                                                                
         DC    AL1(62,72,1,255)    NON-AFM TV BASE SESSION RATES                
         DC    F'36660'                                                         
         DC    F'27565'                                                         
         DC    F'26835'                                                         
         DC    F'23755'                                                         
         DC    F'19650'                                                         
         DC    F'15545'                                                         
         DC    F'13490'                                                         
         DC    F'11000'                                                         
         DC    F'23235'                                                         
         DC    F'35455'                                                         
         DC    F'13485'                                                         
         DC    F'23580'                                                         
         DC    F'56465'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'21315'            SE                                           
         DC    F'5875'             C3,C6                                        
         DC    F'11595'            C9                                           
         SPACE 1                                                                
         DC    AL1(64,72,1,255)    NON-AFM CABLE BASE SESSION RATES             
         DC    F'38495'                                                         
         DC    F'28945'                                                         
         DC    F'28175'                                                         
         DC    F'24945'                                                         
         DC    F'20635'                                                         
         DC    F'16320'                                                         
         DC    F'14165'                                                         
         DC    F'11550'                                                         
         DC    F'17265'                                                         
         DC    F'26200'                                                         
         DC    F'17265'                                                         
         DC    F'26200'                                                         
         DC    F'59290'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'21315'            SE                                           
         DC    F'6170'             C3,C6                                        
         DC    F'12175'            C9                                           
         EJECT                                                                  
         DC    AL1(63,52,0,0)      TV HOLDING RATES                             
         DC    F'36660'                                                         
         DC    F'27565'                                                         
         DC    F'26835'                                                         
         DC    F'23755'                                                         
         DC    F'19650'                                                         
         DC    5A(0)                                                            
         DC    F'13485'                                                         
         DC    F'25560'                                                         
         SPACE 1                                                                
         DC    AL1(65,52,0,0)      TV POSTPONEMENT FEE RATES                    
         DC    F'18330'                                                         
         DC    F'13783'                                                         
         DC    F'13418'                                                         
         DC    F'11878'                                                         
         DC    F'9825'                                                          
         DC    5A(0)                                                            
         DC    F'6743'                                                          
         DC    F'12780'                                                         
         SPACE 1                                                                
         DC    AL1(66,52,0,0)      REN - REINSTATEMENT                          
         DC    F'73320'                                                         
         DC    F'55130'                                                         
         DC    F'53670'                                                         
         DC    F'47510'                                                         
         DC    F'39300'                                                         
         DC    5A(0)                                                            
         DC    F'26970'                                                         
         DC    F'51120'                                                         
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
         DC    F'12830'                                                         
         DC    F'9650'                                                          
         DC    F'9390'                                                          
         DC    F'8315'                                                          
         DC    F'6880'                                                          
         DC    F'5440'                                                          
         DC    F'4725'                                                          
         DC    F'3855'                                                          
         SPACE 1                                                                
         DC    AL1(72,36,0,0)      C13                                          
         DC    F'38495'                                                         
         DC    F'28945'                                                         
         DC    F'28175'                                                         
         DC    F'24945'                                                         
         DC    F'20635'                                                         
         DC    F'16320'                                                         
         DC    F'14165'                                                         
         DC    F'11550'                                                         
         SPACE 1                                                                
         DC    AL1(73,36,0,0)      C52                                          
         DC    F'96235'                                                         
         DC    F'72365'                                                         
         DC    F'70445'                                                         
         DC    F'62360'                                                         
         DC    F'51580'                                                         
         DC    F'40850'                                                         
         DC    F'35410'                                                         
         DC    F'28870'                                                         
         SPACE 1                                                                
         DC    AL1(74,36,0,0)      13-52 UPGRADE                                
         DC    F'57740'                                                         
         DC    F'43420'                                                         
         DC    F'42270'                                                         
         DC    F'37415'                                                         
         DC    F'30945'                                                         
         DC    F'24530'                                                         
         DC    F'21245'                                                         
         DC    F'17320'                                                         
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
         SPACE 1                                                                
         DC    AL1(80,76,1,255)     DEM (TV)                                    
         DC    F'27565'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'13785'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'20125'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'17815'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'14735'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'8500'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'8500'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'8500'             'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    F'23235'            'ON' EXTRAS                                  
         DC    8A(0)                N/D                                         
         DC    F'13000'            'OFF' S                                      
         SPACE 1                                                                
         DC    AL1(85,28,1,255)    DEM (AFT RADIO)                              
         DC    F'9795'             ANN ALONE                                    
         DC    F'9795'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'8500'             1-4M3,1-4S3,D3,S3                            
         DC    F'8500'             1-4M6,1-4S6,D6,S6                            
         DC    F'8500'             1-4M9,1-4S9,D9,S9                            
         DC    F'13000'            S                                            
         SPACE 2                                                                
         DC    AL1(86,36,0,0)      FOREIGN USE - TV  (NOT UK)                   
         DC    F'36660'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'27565'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'26835'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'23755'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'19650'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'15545'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'13490'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'11000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(87,36,0,0)      FOREIGN USE - TV  (UK)                       
         DC    F'109975'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'82700'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'80505'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'71265'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'58955'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'46630'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'40475'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'33000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(88,24,0,0)      FOREIGN USE - RADIO                          
         DC    F'38870'            ANN ALONE                                    
         DC    F'38870'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'22550'            1-4M3,1-4S3,D3,S3                            
         DC    F'15550'            1-4M6,1-4S6,D6,S6                            
         DC    F'12440'            1-4M9,1-4S9,D9,S9                            
         EJECT                                                                  
         SPACE 1                                                                
L13TAB   DC    AL1(90,28,0,0)      LOCAL 13 WEEK - RADIO                        
         DC    F'18695'                                                         
         DC    F'18695'                                                         
         DC    F'18695'                                                         
         DC    F'18695'                                                         
         DC    F'18695'                                                         
         DC    F'7285'                                                          
         EJECT                                                                  
*              FOREIGN REUSE                                                    
         SPACE                                                                  
         DC    AL1(50,72,0,0)      UK                                           
         DC    F'109980'                                                        
         DC    F'82695'                                                         
         DC    F'80505'                                                         
         DC    F'71265'                                                         
         DC    F'58950'                                                         
         DC    F'46635'                                                         
         DC    F'40470'                                                         
         DC    F'33000'                                                         
         DC    F'69705'                                                         
         DC    F'106365'                                                        
         DC    F'40455'                                                         
         DC    F'70740'                                                         
         DC    F'169395'           PIL                                          
         DC    F'0'                N/D                                          
         DC    F'63945'            SE                                           
         DC    F'17625'            C3,C6                                        
         DC    F'34785'            C9                                           
         SPACE                                                                  
         DC    AL1(51,72,0,0)      EUROPE W/O UK                                
         DC    F'36660'                                                         
         DC    F'27565'                                                         
         DC    F'26835'                                                         
         DC    F'23755'                                                         
         DC    F'19650'                                                         
         DC    F'15545'                                                         
         DC    F'13490'                                                         
         DC    F'11000'                                                         
         DC    F'23235'                                                         
         DC    F'35455'                                                         
         DC    F'13485'                                                         
         DC    F'23580'                                                         
         DC    F'56465'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'21315'            SE                                           
         DC    F'5875'             C3,C6                                        
         DC    F'11595'            C9                                           
         SPACE                                                                  
         DC    AL1(52,72,0,0)      WORLD W/O EUROPE W/O UK                      
         DC    F'36660'                                                         
         DC    F'27565'                                                         
         DC    F'26835'                                                         
         DC    F'23755'                                                         
         DC    F'19650'                                                         
         DC    F'15545'                                                         
         DC    F'13490'                                                         
         DC    F'11000'                                                         
         DC    F'23235'                                                         
         DC    F'35455'                                                         
         DC    F'13485'                                                         
         DC    F'23580'                                                         
         DC    F'56465'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'21315'            SE                                           
         DC    F'5875'             C3,C6                                        
         DC    F'11595'            C9                                           
         SPACE                                                                  
         DC    AL1(53,24,0,0)      RADIO                                        
         DC    F'0'                N/D                                          
         DC    F'38870'            P,ANN,S,D,ACR                                
         DC    F'22550'            3-5 GROUP                                    
         DC    F'15550'            6-8 GROUP                                    
         DC    F'12440'            9+                                           
         EJECT                                                                  
         DC    AL1(58,24,0,0)      PBS RADIO                                    
         DC    F'42405'                                                         
         DC    F'44030'            P,ANN,S,D,ACR                                
         DC    F'28710'            3-5 GROUP                                    
         DC    F'22965'            6-8 GROUP                                    
         DC    F'14360'            9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
         SPACE                                                                  
SNTTBL   DC    AL1(40,36,0,0)      NETWORK                                      
         DC    F'92175'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'69315'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'67475'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'59725'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'49400'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'39160'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'33920'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'27655'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
SNWTBL   DC    AL1(40,36,1,255)    NETWK/WILDSPT COMBINED (UNITS 1-255)         
         DC    F'250'                                                           
         DC    F'185'                                                           
         DC    F'180'                                                           
         DC    F'165'                                                           
         DC    F'130'                                                           
         DC    F'105'                                                           
         DC    F'95'                                                            
         DC    F'75'                                                            
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
         SPACE 3                                                                
ADWTAB   DC    AL1(110,44,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'21967'            ON CAMERA                                    
         DC    F'16517'            OFF                                          
         DC    F'28992'                                                         
         DC    F'25656'                                                         
         DC    F'21222'                                                         
         DC    F'16217'                                                         
         DC    F'14075'                                                         
         DC    F'11476'                                                         
         DC    F'39593'            SOLO/DUO ON CAMERA                           
         DC    F'28757'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(110,44,2,25)    UNITS 2-25                                   
         DC    F'896'                                                           
         DC    F'614'                                                           
         DC    F'1259'                                                          
         DC    F'1086'                                                          
         DC    F'885'                                                           
         DC    F'433'                                                           
         DC    F'339'                                                           
         DC    F'282'                                                           
         DC    F'1614'                                                          
         DC    F'1069'                                                          
         SPACE 1                                                                
         DC    AL1(110,44,26,60)    UNITS 26-60                                 
         DC    F'333'                                                           
         DC    F'261'                                                           
         DC    F'648'                                                           
         DC    F'551'                                                           
         DC    F'453'                                                           
         DC    F'183'                                                           
         DC    F'125'                                                           
         DC    F'116'                                                           
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(110,44,61,255)   UNITS 61+                                   
         DC    F'333'                                                           
         DC    F'261'                                                           
         DC    F'470'                                                           
         DC    F'368'                                                           
         DC    F'308'                                                           
         DC    F'109'                                                           
         DC    F'63'                                                            
         DC    F'63'                                                            
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(120,44,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'23536'            ON CAMERA                                    
         DC    F'17721'            OFF                                          
         DC    F'28992'                                                         
         DC    F'25656'                                                         
         DC    F'21222'                                                         
         DC    F'16217'                                                         
         DC    F'14075'                                                         
         DC    F'11476'                                                         
         DC    F'39593'            SOLO/DUO ON CAMERA                           
         DC    F'28757'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(120,44,2,25)    UNITS 2-25                                   
         DC    F'960'                                                           
         DC    F'658'                                                           
         DC    F'1259'                                                          
         DC    F'1086'                                                          
         DC    F'885'                                                           
         DC    F'433'                                                           
         DC    F'339'                                                           
         DC    F'282'                                                           
         DC    F'1614'                                                          
         DC    F'1069'                                                          
         SPACE 1                                                                
         DC    AL1(120,44,26,60)    UNITS 26-60                                 
         DC    F'356'                                                           
         DC    F'279'                                                           
         DC    F'648'                                                           
         DC    F'551'                                                           
         DC    F'453'                                                           
         DC    F'183'                                                           
         DC    F'125'                                                           
         DC    F'116'                                                           
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(120,44,61,255)    UNITS 61+                                  
         DC    F'356'                                                           
         DC    F'279'                                                           
         DC    F'470'                                                           
         DC    F'368'                                                           
         DC    F'308'                                                           
         DC    F'109'                                                           
         DC    F'63'                                                            
         DC    F'63'                                                            
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(130,44,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'26674'            ON CAMERA                                    
         DC    F'20056'            OFF                                          
         DC    F'28992'                                                         
         DC    F'25656'                                                         
         DC    F'21222'                                                         
         DC    F'16217'                                                         
         DC    F'14075'                                                         
         DC    F'11476'                                                         
         DC    F'39593'            SOLO/DUO ON CAMERA                           
         DC    F'28757'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(130,44,2,25)    UNITS 2-25                                   
         DC    F'1088'                                                          
         DC    F'746'                                                           
         DC    F'1259'                                                          
         DC    F'1086'                                                          
         DC    F'885'                                                           
         DC    F'433'                                                           
         DC    F'339'                                                           
         DC    F'282'                                                           
         DC    F'1614'                                                          
         DC    F'1069'                                                          
         SPACE 1                                                                
         DC    AL1(130,44,26,60)    UNITS 26-60                                 
         DC    F'403'                                                           
         DC    F'317'                                                           
         DC    F'648'                                                           
         DC    F'551'                                                           
         DC    F'453'                                                           
         DC    F'183'                                                           
         DC    F'125'                                                           
         DC    F'116'                                                           
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(130,44,61,255)   UNITS 61+                                   
         DC    F'403'                                                           
         DC    F'317'                                                           
         DC    F'470'                                                           
         DC    F'368'                                                           
         DC    F'308'                                                           
         DC    F'109'                                                           
         DC    F'63'                                                            
         DC    F'63'                                                            
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'31381'            ON CAMERA                                    
         DC    F'23596'            OFF                                          
         DC    F'28992'                                                         
         DC    F'25656'                                                         
         DC    F'21222'                                                         
         DC    F'16217'                                                         
         DC    F'14075'                                                         
         DC    F'11476'                                                         
         DC    F'39593'            SOLO/DUO ON CAMERA                           
         DC    F'28757'            OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL1(140,44,2,25)    UNITS 2-25                                   
         DC    F'1280'                                                          
         DC    F'877'                                                           
         DC    F'1259'                                                          
         DC    F'1086'                                                          
         DC    F'885'                                                           
         DC    F'433'                                                           
         DC    F'339'                                                           
         DC    F'282'                                                           
         DC    F'1614'                                                          
         DC    F'1069'                                                          
         SPACE 1                                                                
         DC    AL1(140,44,26,60)    UNITS 26-60                                 
         DC    F'475'                                                           
         DC    F'372'                                                           
         DC    F'648'                                                           
         DC    F'551'                                                           
         DC    F'453'                                                           
         DC    F'183'                                                           
         DC    F'125'                                                           
         DC    F'116'                                                           
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(140,44,61,255)    UNITS 61+                                  
         DC    F'475'                                                           
         DC    F'372'                                                           
         DC    F'470'                                                           
         DC    F'368'                                                           
         DC    F'308'                                                           
         DC    F'109'                                                           
         DC    F'63'                                                            
         DC    F'63'                                                            
         DC    F'600'                                                           
         DC    F'455'                                                           
         SPACE 1                                                                
         DC    AL1(142,36,1,1)      13 WEEKS - TX - UNIT 1                      
         DC    F'28800'             ON CAMERA                                   
         DC    F'20500'             OFF                                         
         DC    F'14000'                                                         
         DC    F'14000'                                                         
         DC    F'14000'                                                         
         DC    F'9000'                                                          
         DC    F'9000'                                                          
         DC    F'9000'                                                          
         SPACE 1                                                                
         DC    AL1(142,36,2,10)     UNITS 2-10                                  
         DC    F'625'                                                           
         DC    F'450'                                                           
         DC    F'250'                                                           
         DC    F'250'                                                           
         DC    F'250'                                                           
         DC    F'100'                                                           
         DC    F'100'                                                           
         DC    F'100'                                                           
         SPACE 1                                                                
         DC    AL1(142,36,11,255)   UNITS 11+                                   
         DC    F'425'                                                           
         DC    F'300'                                                           
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'200'                                                           
         DC    F'100'                                                           
         DC    F'100'                                                           
         DC    F'100'                                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
         SPACE 3                                                                
         DC    AL1(150,32,1,1)     3 DAY - GA - UNIT 1                          
         DC    F'8509'             ANN ALONE                                    
         DC    F'8509'             AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10924'            1-4M3,1-4S3,D3,S3                            
         DC    F'9672'             1-4M6,1-4S6,D6,S6                            
         DC    F'8580'             1-4M9,1-4S9,D9,S9                            
         DC    F'8509'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'14814'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(150,32,2,25)    UNITS 2-25                                   
         DC    F'153'                                                           
         DC    F'153'                                                           
         DC    F'137'                                                           
         DC    F'118'                                                           
         DC    F'106'                                                           
         DC    F'0'                                                             
         DC    F'266'                                                           
         SPACE 1                                                                
         DC    AL1(150,32,26,60)   UNITS 26-60                                  
         DC    F'109'                                                           
         DC    F'109'                                                           
         DC    F'112'                                                           
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(150,32,61,255)  UNITS 61+                                    
         DC    F'109'                                                           
         DC    F'109'                                                           
         DC    F'64'                                                            
         DC    F'47'                                                            
         DC    F'47'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(160,32,1,1)     1 WEEK - GA - UNIT 1                         
         DC    F'9116'             ANN ALONE                                    
         DC    F'9116'             AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10924'            1-4M3,1-4S3,D3,S3                            
         DC    F'9672'             1-4M6,1-4S6,D6,S6                            
         DC    F'8580'             1-4M9,1-4S9,D9,S9                            
         DC    F'9116'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'14814'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(160,32,2,25)    UNITS 2-25                                   
         DC    F'164'                                                           
         DC    F'164'                                                           
         DC    F'137'                                                           
         DC    F'118'                                                           
         DC    F'106'                                                           
         DC    F'0'                                                             
         DC    F'266'                                                           
         SPACE 1                                                                
         DC    AL1(160,32,26,60)   UNITS 26-60                                  
         DC    F'117'                                                           
         DC    F'117'                                                           
         DC    F'112'                                                           
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(160,32,61,255)  UNITS 61+                                    
         DC    F'117'                                                           
         DC    F'117'                                                           
         DC    F'64'                                                            
         DC    F'47'                                                            
         DC    F'47'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,1,1)     4 WEEK - GA - UNIT 1                         
         DC    F'10332'            ANN ALONE                                    
         DC    F'10332'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10924'            1-4M3,1-4S3,D3,S3                            
         DC    F'9672'             1-4M6,1-4S6,D6,S6                            
         DC    F'8580'             1-4M9,1-4S9,D9,S9                            
         DC    F'10332'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'14814'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(170,32,2,25)    UNITS 2-25                                   
         DC    F'185'                                                           
         DC    F'185'                                                           
         DC    F'137'                                                           
         DC    F'118'                                                           
         DC    F'106'                                                           
         DC    F'0'                                                             
         DC    F'266'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,26,60)   UNITS 26-60                                  
         DC    F'132'                                                           
         DC    F'132'                                                           
         DC    F'112'                                                           
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(170,32,61,255)  UNITS 61+                                    
         DC    F'132'                                                           
         DC    F'132'                                                           
         DC    F'64'                                                            
         DC    F'47'                                                            
         DC    F'47'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(180,32,1,1)     13 WEEK - GA - UNIT 1                        
         DC    F'12155'            ANN ALONE                                    
         DC    F'12155'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'10924'            1-4M3,1-4S3,D3,S3                            
         DC    F'9672'             1-4M6,1-4S6,D6,S6                            
         DC    F'8580'             1-4M9,1-4S9,D9,S9                            
         DC    F'12155'            SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    F'14814'            SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL1(180,32,2,25)    UNITS 2-25                                   
         DC    F'218'                                                           
         DC    F'218'                                                           
         DC    F'137'                                                           
         DC    F'118'                                                           
         DC    F'106'                                                           
         DC    F'0'                                                             
         DC    F'266'                                                           
         SPACE 1                                                                
         DC    AL1(180,32,26,60)   UNITS 26-60                                  
         DC    F'155'                                                           
         DC    F'155'                                                           
         DC    F'112'                                                           
         DC    F'82'                                                            
         DC    F'82'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(180,32,61,255)  UNITS 61+                                    
         DC    F'155'                                                           
         DC    F'155'                                                           
         DC    F'64'                                                            
         DC    F'47'                                                            
         DC    F'47'                                                            
         DC    F'0'                                                             
         DC    F'189'                                                           
         SPACE 1                                                                
         DC    AL1(182,28,0,0)     13 WEEK - TX - UNIT 1                        
         DC    F'11750'                                                         
         DC    F'11750'                                                         
         DC    F'7500'                                                          
         DC    F'7500'                                                          
         DC    F'7500'                                                          
         DC    F'11750'                                                         
         SPACE 1                                                                
         DC    AL1(182,28,2,255)   UNITS 2+                                     
         DC    F'100'                                                           
         DC    F'100'                                                           
         DC    F'70'                                                            
         DC    F'70'                                                            
         DC    F'70'                                                            
         DC    F'100'                                                           
         EJECT                                                                  
INRUNLTB DC    AL1(254,36,0,0)    THEAT/INDUS REUSE TV UNLIMITED USE            
         DC    AL1(160),AL3(36660)                                              
         DC    AL1(160),AL3(27565)                                              
         DC    AL1(160),AL3(26835)                                              
         DC    AL1(160),AL3(23755)                                              
         DC    AL1(160),AL3(19650)                                              
         DC    AL1(160),AL3(15545)                                              
         DC    AL1(160),AL3(13490)                                              
         DC    AL1(160),AL3(11000)                                              
         SPACE 1                                                                
         DC    AL1(253,24,0,0)    THEAT/INDUS REUSE RAD UNLIMITED USE           
         DC    AL1(160),AL3(14200)                                              
         DC    AL1(160),AL3(14200)                                              
         DC    AL1(160),AL3(10470)                                              
         DC    AL1(160),AL3(9265)                                               
         DC    AL1(160),AL3(8225)                                               
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(60,UBSM,ALL,AFM+NON,0,0,0,ALL)         SESSION               
         DC    AL1(61,UBSR,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UBSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,URRS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USFS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UDWN,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UFGS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(61,UCNL,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UCNL,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(64,UCNL,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(64,UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(64,URRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
         DC    AL1(65,UPPF,ALL,ALL-AFM,0,0,0,TV)                                
*                                                                               
         DC    AL1(61,ULFT,ALL,AFT+NON,0,0,0,RADIO)   LIFT                      
         DC    AL1(62,ULFT,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(61,USLF,ALL,AFT+NON,0,0,0,RADIO)   SPANISH LIFT              
         DC    AL1(62,USLF,ALL,ALL-AFM,0,0,0,TV)                                
*                                                                               
         DC    AL1(80,UDEM,ALL,ALL,0,0,0,TV)          DEMO                      
         DC    AL1(85,UDEM,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(80,USNA,ALL,ALL,0,0,0,TV)          SPANISH DEMO              
         DC    AL1(85,USNA,ALL,AFT+NON,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(63,UHLD,ALL,ALL-AFM,0,0,0,TV)      HOLDING FEE               
         DC    AL1(63,USHL,ALL,ALL-AFM,0,0,0,TV)      SPANISH HFEE              
         DC    AL1(66,UREN,ALL,ALL-AFM,0,0,0,TV)      REINSTATEMENT             
         DC    AL1(66,USRE,ALL,ALL-AFM,0,0,0,TV)      SPAN REINST               
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
         DC    AL1(70,UCAB,UCABTV,ALL,0,0,0,ALL)      BDCST COMML               
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
         DC    AL1(26,URNT,URNT1W,ALL,0,0,0,ALL)     RADIO NTWK 1W              
         DC    AL1(27,URNT,URNT4W,ALL,0,0,0,ALL)                4W              
         DC    AL1(28,URNT,URNT8W,ALL,0,0,0,ALL)                8W              
         DC    AL1(29,URNT,URNT13W,ALL,0,0,0,ALL)               13W             
         DC    AL1(30,URNT,URNTAB,ALL,0,0,0,ALL)                A-B             
         DC    AL1(31,URNT,URNT26U,ALL,0,0,0,ALL)            26USE              
         DC    AL1(32,URNT,URNT39U,ALL,0,0,0,ALL)            39USE              
*                                                                               
         DC    AL1(33,URRN,ALL,ALL,0,0,0,RADIO)       RAD REG NWK               
*                                                                               
         DC    AL1(90,URLO,ALL,ALL,0,0,0,ALL)         RAD LCL 13W               
*                                                                               
         DC    AL1(35,UMUS,UMUSDUB,ALL,0,0,0,ALL)     DUBBING                   
         DC    AL1(35,UMUS,UMUS13W,ALL,0,0,0,ALL)     REUSE                     
         DC    AL1(35,UMUS,UMUSNEW,ALL,0,0,0,ALL)     NEW                       
         DC    AL1(35,UMUS,UMUSDSH,ALL,0,0,0,ALL)     DUB TO SHRT               
         DC    AL1(35,UNBM,ALL,ALL,0,0,0,ALL)         NON-BRD MUS               
         DC    AL1(95,UMUS,UMUSDUB8,ALL,0,0,0,ALL)    DUBBING-8W                
         DC    AL1(95,UMUS,UMUS8W,ALL,0,0,0,ALL)      REUSE-8WK                 
         DC    AL1(95,UMUS,UMUSNEW8,ALL,0,0,0,ALL)    NEW-8WK                   
         DC    AL1(95,UMUS,UMUSDSH8,ALL,0,0,0,ALL)    DUB TO SHRT 8WK           
*                                                                               
         DC    AL1(96,UFGM,UFGMEU12,ALL,0,0,0,ALL)    AFM FGR-EUR 12M           
         DC    AL1(96,UFGM,UFGMNE12,ALL,0,0,0,ALL)    NOT EUR 12M               
         DC    AL1(98,UFGM,UFGMEU24,ALL,0,0,0,ALL)    EUROPE 24M                
         DC    AL1(98,UFGM,UFGMNE24,ALL,0,0,0,ALL)    NOT EUR 24M               
         DC    AL1(97,UFGM,UFGMWO12,ALL,0,0,0,ALL)    WORLD 12M                 
         DC    AL1(99,UFGM,UFGMWO24,ALL,0,0,0,ALL)    WORLD 24M                 
*                                                                               
         DC    AL1(50,UFGR,UFGRUK,ALL,0,0,0,TV)       FGR REUSE - UK            
         DC    AL1(51,UFGR,UFGREUR,ALL,0,0,0,TV)      EUROPE W/O UK             
         DC    AL1(52,UFGR,UFGRWOR,ALL,0,0,0,TV)      WRLD W/O UK&EUR           
         DC    AL1(52,UFGR,UFGRAP,ALL,0,0,0,TV)       ASIAN PACIFIC             
         DC    AL1(62,UFGR,UFGRMAJ,ALL,0,0,0,TV)      NEW TYPE W/MAJ            
         DC    AL1(53,UFGR,UFGRRAD,ALL,0,0,0,RADIO)   RADIO                     
*                                                                               
         DC    AL1(62,UPBS,ALL,ALL-AFM,0,0,0,TV)      PUB SERV REUSE            
         DC    AL1(58,UPBS,ALL,ALL-AFM,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(40,USNT,ALL,ALL,0,0,0,TV)          SPAN NETWORK USE          
         DC    AL1(40,USNW,ALL,ALL,0,0,0,TV)          SPAN NWK/WSP CMB          
         DC    AL1(10,USWS,ALL,ALL,0,0,0,TV)          SPAN WILDSPOT             
*                                                                               
         DC    AL1(110,UADW,UADW3D,ALL,0,0,0,TV)     AD WSP-TV-3D               
         DC    AL1(120,UADW,UADW1W,ALL,0,0,0,TV)     1 WEEK                     
         DC    AL1(130,UADW,UADW4W,ALL,0,0,0,TV)     4 WEEK                     
         DC    AL1(140,UADW,UADW13W,ALL,0,0,0,TV)    13 WEEK                    
*                                                                               
         DC    AL1(150,UADW,UADW3D,ALL,0,0,0,RADIO)   AD WSP-RAD-3D             
         DC    AL1(160,UADW,UADW1W,ALL,0,0,0,RADIO)   1 WEEK                    
         DC    AL1(170,UADW,UADW4W,ALL,0,0,0,RADIO)   4 WEEK                    
         DC    AL1(180,UADW,UADW13W,ALL,0,0,0,RADIO)  13 WEEK                   
*                                                                               
         DC    AL1(110,UADC,UADC3D,ALL,0,0,0,TV)     CM S/W-TV-3D               
         DC    AL1(120,UADC,UADC1W,ALL,0,0,0,TV)     1 WEEK                     
         DC    AL1(130,UADC,UADC4W,ALL,0,0,0,TV)     4 WEEK                     
         DC    AL1(140,UADC,UADC13W,ALL,0,0,0,TV)    13 WEEK                    
*                                                                               
         DC    AL1(150,UADC,UADC3D,ALL,0,0,0,RADIO)   CM S/W-RD-3D              
         DC    AL1(160,UADC,UADC1W,ALL,0,0,0,RADIO)   1 WEEK                    
         DC    AL1(170,UADC,UADC4W,ALL,0,0,0,RADIO)   4 WEEK                    
         DC    AL1(180,UADC,UADC13W,ALL,0,0,0,RADIO)  13 WEEK                   
*                                                                               
         DC    AL1(62,UINR,UINR30D,ALL,0,0,0,TV)      TH/IN -30D                
         DC    AL1(254,UINR,UINRUNL,ALL,0,0,0,TV)     UNLIMITED                 
         DC    AL1(61,UINR,UINR30D,ALL,0,0,0,RADIO)   30D RADIO                 
         DC    AL1(253,UINR,UINRUNL,ALL,0,0,0,RADIO)  UNLIMITED RAD             
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
         DC    F'5880'             RADIO SESSION TAG FEE                        
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
*                                                                               
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
*                                                                               
*                                  IF DEFINE ROW >= 18, CHANGE SYSCALC          
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
*                                                                               
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
**PAN#1  DC    CL21'019TAGEN69   04/03/09'                                      
         END                                                                    
