*          DATA SET TAGEN67    AT LEVEL 023 AS OF 04/03/09                      
*PHASE T70267A                                                                  
         TITLE 'T70267 - TABLES FOR 1985 CONTRACTS'                             
T70267   CSECT                                                                  
         DC    AL4(USETBLS-T70267)                                              
         DC    AL4(USELUT-T70267)                                               
         DC    AL4(MAJLUT-T70267)                                               
         DC    AL4(AFMCOLS-T70267)                                              
         DC    AL4(RADCOLS-T70267)                                              
         DC    AL4(OFFCOLS-T70267)                                              
         DC    AL4(ONCOLS-T70267)                                               
         DC    AL4(MSWEET-T70267)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL1(0,36,1,1)       CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    F'33325'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'25060'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'24395'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'21595'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'17865'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'14130'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'12265'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'10000'            'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    F'78900'                                                         
         DC    F'56425'                                                         
         DC    F'50250'                                                         
         DC    F'44435'                                                         
         DC    F'36325'                                                         
         DC    F'18515'                                                         
         DC    F'15430'                                                         
         DC    F'12615'                                                         
         SPACE 1                                                                
CBXTAB   DC    AL1(2,36,0,0)       CLASS B W/O NY                               
         DC    F'64355'                                                         
         DC    F'44695'                                                         
         DC    F'50250'                                                         
         DC    F'44435'                                                         
         DC    F'36325'                                                         
         DC    F'18515'                                                         
         DC    F'15430'                                                         
         DC    F'12615'                                                         
         SPACE 1                                                                
CLCTAB   DC    AL1(3,36,0,0)       CLASS C                                      
         DC    F'38350'                                                         
         DC    F'25570'                                                         
         DC    F'33235'                                                         
         DC    F'29535'                                                         
         DC    F'24155'                                                         
         DC    F'14725'                                                         
         DC    F'12255'                                                         
         DC    F'10055'                                                         
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
         SPACE 3                                                                
DANTAB   DC    AL1(4,36,0,0)       DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    F'156490'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'109000'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'117505'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'103575'           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'80505'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'48035'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'42070'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'30035'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL1(5,36,0,0)       DEALER A W/O NY                              
         DC    F'138400'                                                        
         DC    F'99960'                                                         
         DC    F'117505'                                                        
         DC    F'103575'                                                        
         DC    F'80505'                                                         
         DC    F'48035'                                                         
         DC    F'42070'                                                         
         DC    F'30035'                                                         
         SPACE 1                                                                
DBNTAB   DC    AL1(6,36,0,0)       CLASS B INCL NY                              
         DC    F'240620'                                                        
         DC    F'163730'                                                        
         DC    F'178655'                                                        
         DC    F'157485'                                                        
         DC    F'122570'                                                        
         DC    F'73180'                                                         
         DC    F'64050'                                                         
         DC    F'45685'                                                         
         SPACE 1                                                                
DBXTAB   DC    AL1(7,36,0,0)       CLASS B W/O NY                               
         DC    F'207605'                                                        
         DC    F'149705'                                                        
         DC    F'178655'                                                        
         DC    F'157485'                                                        
         DC    F'122570'                                                        
         DC    F'73180'                                                         
         DC    F'64050'                                                         
         DC    F'45685'                                                         
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
         SPACE 3                                                                
G13TAB   DC    AL1(8,36,1,1)       13 USE                                       
         DC    F'130005'           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'101030'           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'109540'           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'97940'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'80390'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'67000'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'58335'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'47745'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(8,4,2,13)       (2-13)                                       
         SPACE 1                                                                
         DC    AL1(8,36,14,18)     14-18                                        
         DC    F'9200'                                                          
         DC    F'6990'                                                          
         DC    F'6725'                                                          
         DC    F'5885'                                                          
         DC    F'4810'                                                          
         DC    F'4440'                                                          
         DC    F'4010'                                                          
         DC    F'3300'                                                          
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
         DC    F'33325'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'25060'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'24395'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'21595'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'17865'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'10745'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'8735'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'7160'             'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(10,36,2,25)     UNITS 2-25                                   
         DC    F'1360'                                                          
         DC    F'930'                                                           
         DC    F'1060'                                                          
         DC    F'915'                                                           
         DC    F'745'                                                           
         DC    F'375'                                                           
         DC    F'295'                                                           
         DC    F'245'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,26,60)    UNITS 26-60                                  
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'545'                                                           
         DC    F'465'                                                           
         DC    F'380'                                                           
         DC    F'160'                                                           
         DC    F'110'                                                           
         DC    F'100'                                                           
         SPACE 1                                                                
         DC    AL1(10,36,61,125)   UNITS 61-125                                 
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'395'                                                           
         DC    F'310'                                                           
         DC    F'260'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         SPACE 1                                                                
         DC    AL1(10,36,126,255)  UNITS 126+                                   
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'195'                                                           
         DC    F'160'                                                           
         DC    F'135'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL1(11,36,0,0)      NY ALONE                                     
         DC    F'78050'                                                         
         DC    F'55135'                                                         
         DC    F'49980'                                                         
         DC    F'44395'                                                         
         DC    F'36375'                                                         
         DC    F'20050'                                                         
         DC    F'16615'                                                         
         DC    F'13605'                                                         
         SPACE 1                                                                
         DC    AL1(11,36,1,35)     UNITS 1-35                                   
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'545'                                                           
         DC    F'465'                                                           
         DC    F'380'                                                           
         DC    F'160'                                                           
         DC    F'110'                                                           
         DC    F'100'                                                           
         SPACE 1                                                                
         DC    AL1(11,36,36,100)   UNITS 36-100                                 
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'395'                                                           
         DC    F'310'                                                           
         DC    F'260'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         SPACE 1                                                                
         DC    AL1(11,36,101,255)  UNITS 101+                                   
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'195'                                                           
         DC    F'160'                                                           
         DC    F'135'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL1(12,36,0,0)      CHI OR LA ALONE                              
         DC    F'68025'                                                         
         DC    F'47975'                                                         
         DC    F'49980'                                                         
         DC    F'44395'                                                         
         DC    F'36375'                                                         
         DC    F'20050'                                                         
         DC    F'16615'                                                         
         DC    F'13605'                                                         
         SPACE 1                                                                
         DC    AL1(12,36,1,35)     UNITS 1-35                                   
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'545'                                                           
         DC    F'465'                                                           
         DC    F'380'                                                           
         DC    F'160'                                                           
         DC    F'110'                                                           
         DC    F'100'                                                           
         SPACE 1                                                                
         DC    AL1(12,36,36,100)   UNITS 36-100                                 
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'395'                                                           
         DC    F'310'                                                           
         DC    F'260'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         SPACE 1                                                                
         DC    AL1(12,36,101,255)  UNITS 101+                                   
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'195'                                                           
         DC    F'160'                                                           
         DC    F'135'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL1(13,36,0,0)      TWO OF NY LA CHI                             
         DC    F'107405'                                                        
         DC    F'72320'                                                         
         DC    F'76900'                                                         
         DC    F'63585'                                                         
         DC    F'51985'                                                         
         DC    F'26495'                                                         
         DC    F'21340'                                                         
         DC    F'17475'                                                         
         SPACE 1                                                                
         DC    AL1(13,36,1,255)    UNITS 1+                                     
         DC    F'505'                                                           
         DC    F'395'                                                           
         DC    F'195'                                                           
         DC    F'160'                                                           
         DC    F'135'                                                           
         DC    F'95'                                                            
         DC    F'55'                                                            
         DC    F'55'                                                            
         SPACE 1                                                                
         DC    AL1(14,36,0,0)      ALL THREE MAJORS                             
         DC    F'129555'                                                        
         DC    F'92015'                                                         
         DC    F'97020'                                                         
         DC    F'83030'                                                         
         DC    F'67865'                                                         
         DC    F'31945'                                                         
         DC    F'25765'                                                         
         DC    F'21055'                                                         
         SPACE 1                                                                
         DC    AL1(14,36,1,255)    UNITS 1+                                     
         DC    F'520'                                                           
         DC    F'405'                                                           
         DC    F'200'                                                           
         DC    F'165'                                                           
         DC    F'140'                                                           
         DC    F'100'                                                           
         DC    F'60'                                                            
         DC    F'60'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL1(15,28,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    F'12510'            ANN ALONE                                    
         DC    F'12510'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'9225'             1-4M3,1-4S3,D3,S3                            
         DC    F'8165'             1-4M6,1-4S6,D6,S6                            
         DC    F'7245'             1-4M9,1-4S9,D9,S9                            
         DC    F'4930'             SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL1(15,24,2,25)     UNITS 2-25                                   
         DC    F'225'                                                           
         DC    F'225'                                                           
         DC    F'115'                                                           
         DC    F'100'                                                           
         DC    F'90'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,26,60)    UNITS 26-60                                  
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'95'                                                            
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(15,24,61,255)   UNITS 61+                                    
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'55'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(16,28,0,0)      NEW YORK ALONE                               
         DC    F'21950'                                                         
         DC    F'21950'                                                         
         DC    F'11940'                                                         
         DC    F'9270'                                                          
         DC    F'6140'                                                          
         DC    F'4930'                                                          
         SPACE 1                                                                
         DC    AL1(16,24,1,35)     UNITS 1-35                                   
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'95'                                                            
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(16,24,36,255)   UNITS 36+                                    
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'55'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         SPACE 1                                                                
         DC    AL1(17,28,0,0)      CHICAGO OR LA ALONE                          
         DC    F'19905'                                                         
         DC    F'19905'                                                         
         DC    F'11940'                                                         
         DC    F'9270'                                                          
         DC    F'6140'                                                          
         DC    F'4930'                                                          
         SPACE 1                                                                
         DC    AL1(17,24,1,35)     UNITS 1-35                                   
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'95'                                                            
         DC    F'70'                                                            
         DC    F'70'                                                            
         SPACE 1                                                                
         DC    AL1(17,24,36,255)   UNITS 36+                                    
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'55'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(18,28,0,0)      ANY TWO ALONE                                
         DC    F'26770'                                                         
         DC    F'26770'                                                         
         DC    F'14255'                                                         
         DC    F'10940'                                                         
         DC    F'7450'                                                          
         DC    F'4930'                                                          
         SPACE 1                                                                
         DC    AL1(18,24,1,255)    UNITS 1+                                     
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'55'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         SPACE 1                                                                
         DC    AL1(19,28,0,0)      ALL THREE ALONE                              
         DC    F'33825'                                                         
         DC    F'33825'                                                         
         DC    F'15880'                                                         
         DC    F'12290'                                                         
         DC    F'8880'                                                          
         DC    F'4930'                                                          
         SPACE 1                                                                
         DC    AL1(19,24,1,255)    UNITS 1+                                     
         DC    F'160'                                                           
         DC    F'160'                                                           
         DC    F'55'                                                            
         DC    F'40'                                                            
         DC    F'40'                                                            
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL1(20,24,1,1)      UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(12510) ANN ALONE                                    
         DC    AL1(100),AL3(12510) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(9225)  1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(8165)  1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(7245)  1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL1(20,24,2,25)     UNITS 2-25                                   
         DC    AL1(80),AL3(225)                                                 
         DC    AL1(80),AL3(225)                                                 
         DC    AL1(95),AL3(115)                                                 
         DC    AL1(95),AL3(100)                                                 
         DC    AL1(95),AL3(90)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,26,60)    UNITS 26-60                                  
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(95)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         SPACE 1                                                                
         DC    AL1(20,24,61,255)   UNITS 61+                                    
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(55)                                                  
         DC    AL1(95),AL3(40)                                                  
         DC    AL1(95),AL3(40)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL1(21,24,0,0)      NEW YORK ALONE                               
         DC    AL1(80),AL3(21950)                                               
         DC    AL1(80),AL3(21950)                                               
         DC    AL1(95),AL3(11940)                                               
         DC    AL1(95),AL3(9270)                                                
         DC    AL1(95),AL3(6140)                                                
         SPACE 1                                                                
         DC    AL1(21,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(95)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         SPACE 1                                                                
         DC    AL1(21,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(55)                                                  
         DC    AL1(95),AL3(40)                                                  
         DC    AL1(95),AL3(40)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,0,0)      CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(19905)                                               
         DC    AL1(80),AL3(19905)                                               
         DC    AL1(95),AL3(11940)                                               
         DC    AL1(95),AL3(9270)                                                
         DC    AL1(95),AL3(6140)                                                
         SPACE 1                                                                
         DC    AL1(22,24,1,35)     UNITS 1-35                                   
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(95)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         SPACE 1                                                                
         DC    AL1(22,24,36,255)   UNITS 36+                                    
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(55)                                                  
         DC    AL1(95),AL3(40)                                                  
         DC    AL1(95),AL3(40)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL1(23,24,0,0)      ANY TWO ALONE                                
         DC    AL1(80),AL3(26770)                                               
         DC    AL1(80),AL3(26770)                                               
         DC    AL1(95),AL3(14255)                                               
         DC    AL1(95),AL3(10940)                                               
         DC    AL1(95),AL3(7450)                                                
         SPACE 1                                                                
         DC    AL1(23,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(55)                                                  
         DC    AL1(95),AL3(40)                                                  
         DC    AL1(95),AL3(40)                                                  
         SPACE 1                                                                
         DC    AL1(24,24,0,0)      ALL THREE ALONE                              
         DC    AL1(80),AL3(33825)                                               
         DC    AL1(80),AL3(33825)                                               
         DC    AL1(95),AL3(15880)                                               
         DC    AL1(95),AL3(12290)                                               
         DC    AL1(95),AL3(8880)                                                
         SPACE 1                                                                
         DC    AL1(24,24,1,255)    UNITS 1+                                     
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(80),AL3(160)                                                 
         DC    AL1(95),AL3(55)                                                  
         DC    AL1(95),AL3(40)                                                  
         DC    AL1(95),AL3(40)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TALES - RADIO                                 
         SPACE 3                                                                
DLRTAB   DC    AL1(25,28,0,0)      DEALER COMMERCIALS                           
         DC    F'47995'            AR,AS,P,ANN                                  
         DC    F'38070'            S,1-4MS,1-4SS                                
         DC    F'24825'            1-4M3,1-4S3,D3,S3                            
         DC    F'19860'            1-4M6,1-4S6,D6,S6                            
         DC    F'12410'            1-4M9,1-4S9,D9,S9                            
         DC    F'11680'            SE                                           
         SPACE 1                                                                
N01TAB   DC    AL1(26,28,0,0)      NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    F'29440'            ANN ALONE                                    
         DC    F'29440'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'22090'            1-4M3,1-4S3,D3,S3                            
         DC    F'22090'            1-4M6,1-4S6,D6,S6                            
         DC    F'22090'            1-4M9,1-4S9,D9,S9                            
         DC    F'6420'             SE                                           
         SPACE 1                                                                
N04TAB   DC    AL1(27,28,0,0)      NETWORK 4 WEEK                               
         DC    F'68875'                                                         
         DC    F'47770'                                                         
         DC    F'36730'                                                         
         DC    F'32850'                                                         
         DC    F'30010'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
N08TAB   DC    AL1(28,28,0,0)      NETWORK 8 WEEK                               
         DC    F'76095'                                                         
         DC    F'76095'                                                         
         DC    F'58545'                                                         
         DC    F'52325'                                                         
         DC    F'46870'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
N13TAB   DC    AL1(29,28,0,0)      NETWORK 13 WEEK                              
         DC    F'94425'                                                         
         DC    F'94425'                                                         
         DC    F'72620'                                                         
         DC    F'64935'                                                         
         DC    F'59490'                                                         
         DC    F'6420'                                                          
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL1(30,28,0,0)      ACROSS-THE-BOARD                             
         DC    F'98870'                                                         
         DC    F'98870'                                                         
         DC    F'76030'                                                         
         DC    F'67990'                                                         
         DC    F'62290'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
U26TAB   DC    AL1(31,28,0,0)      26 USE LIMIT                                 
         DC    F'70540'                                                         
         DC    F'47215'                                                         
         DC    F'36305'                                                         
         DC    F'32465'                                                         
         DC    F'29665'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
U39TAB   DC    AL1(32,28,0,0)      39 USE LIMIT                                 
         DC    F'71100'                                                         
         DC    F'71100'                                                         
         DC    F'49785'                                                         
         DC    F'44435'                                                         
         DC    F'40375'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
R13TAB   DC    AL1(33,28,0,0)      REGIONAL - NO MAJORS                         
         DC    F'81905'                                                         
         DC    F'56980'                                                         
         DC    F'26710'                                                         
         DC    F'26710'                                                         
         DC    F'26710'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
         DC    AL1(34,28,0,0)      REGIONAL - WITH ANY MAJORS                   
         DC    F'81905'                                                         
         DC    F'56980'                                                         
         DC    F'56980'                                                         
         DC    F'51280'                                                         
         DC    F'46120'                                                         
         DC    F'6420'                                                          
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL1(35,16,0,0)      REUSE                                        
         DC    F'5565'             CAST=1                                       
         DC    F'6015'                  2-4                                     
         DC    F'5565'                  5+                                      
         SPACE 3                                                                
BSMTAB   DC    AL1(60,16,0,0)      SESSION                                      
         DC    F'7420'             CAST=1                                       
         DC    F'8020'                  2-4                                     
         DC    F'7420'                  5+                                      
         SPACE 3                                                                
FGMTAB   DC    AL1(96,16,0,0)      FOREIGN MUSIC                                
         DC    F'11130'            CAST=1                                       
         DC    F'12030'                 2-4                                     
         DC    F'11130'                 5+                                      
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 2                                                                
         DC    AL1(61,28,0,0)      AFT RADIO BASE SESSION RATES                 
         DC    F'12510'            ANN ALONE                                    
         DC    F'12510'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'9225'             1-4M3,1-4S3,D3,S3                            
         DC    F'8165'             1-4M6,1-4S6,D6,S6                            
         DC    F'7245'             1-4M9,1-4S9,D9,S9                            
         DC    F'9625'             SE                                           
         SPACE 1                                                                
         DC    AL1(62,64,1,255)    NON-AFM TV/CABLE BASE SESSION RATES          
         DC    F'33325'                                                         
         DC    F'25060'                                                         
         DC    F'24395'                                                         
         DC    F'21595'                                                         
         DC    F'17865'                                                         
         DC    F'14130'                                                         
         DC    F'12265'                                                         
         DC    F'10000'                                                         
         DC    F'22130'            EXB                                          
         DC    F'30695'            HMB                                          
         DC    F'12845'            EX                                           
         DC    F'20415'            HM                                           
         DC    F'51330'            PIL                                          
         DC    F'0'                N/D                                          
         DC    F'18280'            SE                                           
         SPACE 1                                                                
         DC    AL1(63,52,0,0)      TV HOLDING RATES                             
         DC    F'33325'                                                         
         DC    F'25060'                                                         
         DC    F'24395'                                                         
         DC    F'21595'                                                         
         DC    F'17865'                                                         
         DC    5A(0)                                                            
         DC    F'16565'                                                         
         DC    F'22130'                                                         
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
         DC    F'11110'                                                         
         DC    F'8355'                                                          
         DC    F'8130'                                                          
         DC    F'7200'                                                          
         DC    F'5955'                                                          
         DC    F'4710'                                                          
         DC    F'4090'                                                          
         DC    F'3325'                                                          
         SPACE 1                                                                
         DC    AL1(72,36,0,0)      C13                                          
         DC    F'33325'                                                         
         DC    F'25060'                                                         
         DC    F'24395'                                                         
         DC    F'21595'                                                         
         DC    F'17865'                                                         
         DC    F'14130'                                                         
         DC    F'12265'                                                         
         DC    F'10000'                                                         
         SPACE 1                                                                
         DC    AL1(73,36,0,0)      C52                                          
         DC    F'83320'                                                         
         DC    F'62655'                                                         
         DC    F'60990'                                                         
         DC    F'53990'                                                         
         DC    F'44660'                                                         
         DC    F'35325'                                                         
         DC    F'30660'                                                         
         DC    F'24995'                                                         
         SPACE 1                                                                
         DC    AL1(74,36,0,0)      13-52 UPGRADE                                
         DC    F'49995'                                                         
         DC    F'37595'                                                         
         DC    F'36595'                                                         
         DC    F'32395'                                                         
         DC    F'26795'                                                         
         DC    F'21195'                                                         
         DC    F'18395'                                                         
         DC    F'14995'                                                         
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
         SPACE 1                                                                
         DC    AL1(80,40,0,0)      DEM (TV)                                     
         DC    F'25060'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'12530'            'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    F'18295'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'16195'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'13395'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'8500'             'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'8500'             'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'8500'             'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    F'23235'            'OFF' S                                      
         SPACE 1                                                                
         DC    AL1(85,28,0,0)      DEM (AFT RADIO)                              
         DC    F'8630'             ANN ALONE                                    
         DC    F'8630'             AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    F'8500'             1-4M3,1-4S3,D3,S3                            
         DC    F'8500'             1-4M6,1-4S6,D6,S6                            
         DC    F'8500'             1-4M9,1-4S9,D9,S9                            
         DC    F'13000'            S                                            
         SPACE 2                                                                
         DC    AL1(86,36,0,0)      FOREIGN USE - TV  (NOT UK)                   
         DC    F'33325'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'25060'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'24395'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'21595'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'17865'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'14130'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'12265'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'10000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(87,36,0,0)      FOREIGN USE - TV  (UK)                       
         DC    F'99975'            'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    F'75180'            'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    F'73185'            'ON' 1-4M3,1-4S3,D3,S3                       
         DC    F'64785'            'ON' 1-4M6,1-4S6,D6,S6                       
         DC    F'53595'            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    F'42390'            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    F'36795'            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    F'30000'            'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL1(88,24,0,0)      FOREIGN USE - RADIO                          
         DC    F'34245'            ANN ALONE                                    
         DC    F'34245'            AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    F'19870'            1-4M3,1-4S3,D3,S3                            
         DC    F'13700'            1-4M6,1-4S6,D6,S6                            
         DC    F'10960'            1-4M9,1-4S9,D9,S9                            
         EJECT                                                                  
         SPACE 1                                                                
L13TAB   DC    AL1(90,28,0,0)      LOCAL 13 WEEK - RADIO                        
         DC    F'16470'                                                         
         DC    F'16470'                                                         
         DC    F'16470'                                                         
         DC    F'16470'                                                         
         DC    F'16470'                                                         
         DC    F'6420'                                                          
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(60,UBSM,ALL,AFM+NON,0,0,0,ALL)     SESSION                   
         DC    AL1(61,UBSR,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(62,UBSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USSS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UFGS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,USFS,ALL,ALL-AFM,0,0,0,TV)                                
         DC    AL1(62,UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                         
*                                                                               
         DC    AL1(61,ULFT,ALL,AFT+NON,0,0,0,RADIO)   LIFT                      
         DC    AL1(62,ULFT,ALL,ALL-AFM,0,0,0,TV)                                
*                                                                               
         DC    AL1(80,UDEM,ALL,ALL,0,0,0,TV)          DEMO                      
         DC    AL1(85,UDEM,ALL,AFT+NON,0,0,0,RADIO)                             
         DC    AL1(80,USNA,ALL,ALL,0,0,0,TV)          SPANISH DEMO              
         DC    AL1(85,USNA,ALL,AFT+NON,0,0,0,RADIO)                             
*                                                                               
         DC    AL1(63,UHLD,ALL,ALL-AFM,0,0,0,TV)      HOLDING FEE               
         DC    AL1(63,USHL,ALL,ALL-AFM,0,0,0,TV)      SPAN HLD FEE              
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
         DC    AL1(70,UCAB,UCABTV,ALL,0,0,0,ALL)      BDCST COMML USE           
         DC    AL1(71,UCAB,UCAB4W,ALL,0,0,0,ALL)      4 WK                      
         DC    AL1(72,UCAB,UCAB13W,ALL,0,0,0,ALL)     13 WK                     
         DC    AL1(73,UCAB,UCAB52W,ALL,0,0,0,ALL)     52 WK                     
         DC    AL1(72,UCAB,UCABU413,ALL,0,0,0,ALL)    4-13 WK                   
         DC    AL1(73,UCAB,UCABU452,ALL,0,0,0,ALL)    4-52 WK                   
         DC    AL1(74,UCAB,UCABU13,ALL,0,0,0,ALL)     13-52 WK                  
*                                                                               
         DC    AL1(87,UFGR,UFGRUK,ALL,0,0,0,TV)       FGN REUSE - UK            
         DC    AL1(86,UFGR,UFGREUR,ALL,0,0,0,TV)      EUR W/O UK                
         DC    AL1(86,UFGR,UFGRWOR,ALL,0,0,0,TV)      WD W/O UK&EUR             
         DC    AL1(86,UFGR,UFGRAP,ALL,0,0,0,TV)       ASIAN PAC                 
         DC    AL1(62,UFGR,UFGRMAJ,ALL,0,0,0,TV)      NEW TYPE-W/MAJ            
         DC    AL1(88,UFGR,UFGRRAD,ALL,0,0,0,RADIO)                             
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
         DC    AL1(90,URLO,ALL,ALL,0,0,0,ALL)         RAD LCL 13W               
*                                                                               
         DC    AL1(35,UMUS,UMUSDUB,ALL,0,0,0,ALL)     DUBBING                   
         DC    AL1(35,UMUS,UMUS13W,ALL,0,0,0,ALL)     REUSE                     
         DC    AL1(35,UMUS,UMUSNEW,ALL,0,0,0,ALL)     NEW                       
*                                                                               
* NO-OP  DC    AL1(96,UFGM,0,ALL,0,0,0,ALL)           FOREIGN MUSIC             
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
**PAN#1  DC    CL21'023TAGEN67   04/03/09'                                      
         END                                                                    
