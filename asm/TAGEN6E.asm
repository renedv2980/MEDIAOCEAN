*          DATA SET TAGEN6E    AT LEVEL 027 AS OF 10/05/11                      
*PHASE T7026EC,*                                                                
         TITLE 'T7026E - TABLES FOR 1997 && 1998 CONTRACTS'                     
T7026E   CSECT                                                                  
         DC    AL4(USETBLS-T7026E)                                              
         DC    AL4(USELUT-T7026E)                                               
         DC    AL4(MAJLUT-T7026E)                                               
         DC    AL4(AFMCOLS-T7026E)                                              
         DC    AL4(RADCOLS-T7026E)                                              
         DC    AL4(OFFCOLS-T7026E)                                              
         DC    AL4(ONCOLS-T7026E)                                               
         DC    AL4(MSWEET-T7026E)                                               
         DC    AL4(TAGFEE-T7026E)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
CLATBL   DC    AL2(0,44,1,1,0,0)   CLASS A USE 1                                
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(47870)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(35995)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(35045)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(31025)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(25660)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(20300)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(17615)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(14365)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(12270)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(11370)                                                       
         DC    AL4(9735)                                                        
         DC    AL4(7965)                                                        
         DC    AL4(6170)                                                        
         DC    AL4(5365)                                                        
         DC    AL4(4400)                                                        
         SPACE 1                                                                
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(9735)                                                        
         DC    AL4(7635)                                                        
         DC    AL4(8900)                                                        
         DC    AL4(8065)                                                        
         DC    AL4(6595)                                                        
         DC    AL4(5765)                                                        
         DC    AL4(4935)                                                        
         DC    AL4(4030)                                                        
         SPACE 1                                                                
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(9735)                                                        
         DC    AL4(7635)                                                        
         DC    AL4(8400)                                                        
         DC    AL4(7565)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(5265)                                                        
         DC    AL4(4595)                                                        
         DC    AL4(3765)                                                        
         SPACE 1                                                                
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(4665)                                                        
         DC    AL4(3465)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2100)                                                        
         DC    AL4(1970)                                                        
         DC    AL4(1635)                                                        
         SPACE 1                                                                
CLBTAB   DC    AL2(1,44,0,0,0,0)    CLASS B WITH NY                             
         DC    AL4(101220)                                                      
         DC    AL4(72390)                                                       
         DC    AL4(64470)                                                       
         DC    AL4(57010)                                                       
         DC    AL4(46605)                                                       
         DC    AL4(23755)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(16185)                                                       
         SPACE 1                                                                
CBXTAB   DC    AL2(2,44,0,0,0,0)    CLASS B W/O NY                              
         DC    AL4(82560)                                                       
         DC    AL4(57340)                                                       
         DC    AL4(64470)                                                       
         DC    AL4(57010)                                                       
         DC    AL4(46605)                                                       
         DC    AL4(23755)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(16185)                                                       
         SPACE 1                                                                
CLCTAB   DC    AL2(3,44,0,0,0,0)    CLASS C                                     
         DC    AL4(49200)                                                       
         DC    AL4(32800)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(37895)                                                       
         DC    AL4(30985)                                                       
         DC    AL4(18895)                                                       
         DC    AL4(15725)                                                       
         DC    AL4(12900)                                                       
         SPACE 1                                                                
NWKTBL   DC    AL2(58,44,1,1,0,0)   NWK (LNA,LNB,LNC) USE 1                     
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
DANTAB   DC    AL2(4,44,0,0,0,0)   DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(200770)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(139840)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(150750)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(132885)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(103280)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(61625)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(53970)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(38535)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
DAXTAB   DC    AL2(5,44,0,0,0,0)    DEALER A W/O NY                             
         DC    AL4(177555)                                                      
         DC    AL4(128240)                                                      
         DC    AL4(150750)                                                      
         DC    AL4(132885)                                                      
         DC    AL4(103280)                                                      
         DC    AL4(61625)                                                       
         DC    AL4(53970)                                                       
         DC    AL4(38535)                                                       
         SPACE 1                                                                
DBNTAB   DC    AL2(6,44,0,0,0,0)    CLASS B INCL NY                             
         DC    AL4(308695)                                                      
         DC    AL4(210055)                                                      
         DC    AL4(229200)                                                      
         DC    AL4(202045)                                                      
         DC    AL4(157245)                                                      
         DC    AL4(93885)                                                       
         DC    AL4(82170)                                                       
         DC    AL4(58615)                                                       
         SPACE 1                                                                
DBXTAB   DC    AL2(7,44,0,0,0,0)    CLASS B W/O NY                              
         DC    AL4(266345)                                                      
         DC    AL4(192060)                                                      
         DC    AL4(229200)                                                      
         DC    AL4(202045)                                                      
         DC    AL4(157245)                                                      
         DC    AL4(93885)                                                       
         DC    AL4(82170)                                                       
         DC    AL4(58615)                                                       
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
         SPACE 3                                                                
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(144550)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(111965)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(120190)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(107370)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(88185)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(73170)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(63685)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(52110)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
         SPACE 1                                                                
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(9200)                                                        
         DC    AL4(6988)                                                        
         DC    AL4(6725)                                                        
         DC    AL4(5886)                                                        
         DC    AL4(4807)                                                        
         DC    AL4(4443)                                                        
         DC    AL4(4006)                                                        
         DC    AL4(3302)                                                        
         SPACE 1                                                                
         DC    AL2(8,44,19,255,0,0)  19+                                        
         DC    AL4(4665)                                                        
         DC    AL4(3465)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2100)                                                        
         DC    AL4(1970)                                                        
         DC    AL4(1635)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
         SPACE 3                                                                
WSPTAB   DC    AL2(10,44,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(47870)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(35995)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(35045)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(31025)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(25660)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(20300)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(17615)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(14365)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1359)                                                        
         DC    AL4(1172)                                                        
         DC    AL4(958)                                                         
         DC    AL4(482)                                                         
         DC    AL4(380)                                                         
         DC    AL4(316)                                                         
         SPACE 1                                                                
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
         SPACE 1                                                                
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         SPACE 1                                                                
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(251)                                                         
         DC    AL4(203)                                                         
         DC    AL4(177)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
         SPACE 3                                                                
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(100130)                                                      
         DC    AL4(70740)                                                       
         DC    AL4(64125)                                                       
         DC    AL4(56955)                                                       
         DC    AL4(46670)                                                       
         DC    AL4(25725)                                                       
         DC    AL4(21315)                                                       
         DC    AL4(17450)                                                       
         SPACE 1                                                                
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
         SPACE 1                                                                
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         SPACE 1                                                                
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(251)                                                         
         DC    AL4(203)                                                         
         DC    AL4(177)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
         SPACE 3                                                                
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(87275)                                                       
         DC    AL4(61550)                                                       
         DC    AL4(64125)                                                       
         DC    AL4(56955)                                                       
         DC    AL4(46670)                                                       
         DC    AL4(25725)                                                       
         DC    AL4(21315)                                                       
         DC    AL4(17450)                                                       
         SPACE 1                                                                
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
         SPACE 1                                                                
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         SPACE 1                                                                
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(251)                                                         
         DC    AL4(203)                                                         
         DC    AL4(177)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
         SPACE 3                                                                
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(137795)                                                      
         DC    AL4(92780)                                                       
         DC    AL4(98660)                                                       
         DC    AL4(81575)                                                       
         DC    AL4(66695)                                                       
         DC    AL4(33995)                                                       
         DC    AL4(27380)                                                       
         DC    AL4(22420)                                                       
         SPACE 1                                                                
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(251)                                                         
         DC    AL4(203)                                                         
         DC    AL4(177)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         SPACE 1                                                                
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(166210)                                                      
         DC    AL4(118050)                                                      
         DC    AL4(124470)                                                      
         DC    AL4(106525)                                                      
         DC    AL4(87065)                                                       
         DC    AL4(40985)                                                       
         DC    AL4(33050)                                                       
         DC    AL4(27010)                                                       
         SPACE 1                                                                
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(663)                                                         
         DC    AL4(519)                                                         
         DC    AL4(257)                                                         
         DC    AL4(209)                                                         
         DC    AL4(182)                                                         
         DC    AL4(128)                                                         
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
         SPACE 3                                                                
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(20000)          ANN ALONE                                    
         DC    AL4(20000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(14730)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(13035)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11570)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(6910)           SE (ONLY GETS PAID FOR FIRST UNIT)           
         SPACE 1                                                                
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(302)                                                         
         DC    AL4(302)                                                         
         DC    AL4(157)                                                         
         DC    AL4(135)                                                         
         DC    AL4(119)                                                         
         SPACE 1                                                                
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(135)                                                         
         DC    AL4(103)                                                         
         DC    AL4(103)                                                         
         SPACE 1                                                                
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(76)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(30780)                                                       
         DC    AL4(30780)                                                       
         DC    AL4(16740)                                                       
         DC    AL4(14860)                                                       
         DC    AL4(13190)                                                       
         DC    AL4(6910)                                                        
         SPACE 1                                                                
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(135)                                                         
         DC    AL4(113)                                                         
         DC    AL4(108)                                                         
         SPACE 1                                                                
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(76)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         SPACE 1                                                                
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(27915)                                                       
         DC    AL4(27915)                                                       
         DC    AL4(16740)                                                       
         DC    AL4(14860)                                                       
         DC    AL4(13190)                                                       
         DC    AL4(6910)                                                        
         SPACE 1                                                                
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(135)                                                         
         DC    AL4(113)                                                         
         DC    AL4(108)                                                         
         SPACE 1                                                                
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(76)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(37540)                                                       
         DC    AL4(37540)                                                       
         DC    AL4(19990)                                                       
         DC    AL4(15335)                                                       
         DC    AL4(13650)                                                       
         DC    AL4(6910)                                                        
         SPACE 1                                                                
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(113)                                                         
         DC    AL4(113)                                                         
         DC    AL4(108)                                                         
         SPACE 1                                                                
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(76)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         SPACE 1                                                                
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(47435)                                                       
         DC    AL4(47435)                                                       
         DC    AL4(22270)                                                       
         DC    AL4(17235)                                                       
         DC    AL4(15335)                                                       
         DC    AL4(6910)                                                        
         SPACE 1                                                                
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(113)                                                         
         DC    AL4(113)                                                         
         DC    AL4(108)                                                         
         SPACE 1                                                                
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(227)                                                         
         DC    AL4(227)                                                         
         DC    AL4(76)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
         SPACE 3                                                                
         DC    AL2(20,32,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(20000) ANN ALONE                                    
         DC    AL1(100),AL3(20000) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(14730) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(13035) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(11570) 1-4M9,1-4S9,D9,S9                            
         SPACE 1                                                                
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL1(80),AL3(302)                                                 
         DC    AL1(80),AL3(302)                                                 
         DC    AL1(95),AL3(157)                                                 
         DC    AL1(95),AL3(135)                                                 
         DC    AL1(95),AL3(119)                                                 
         SPACE 1                                                                
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(135)                                                 
         DC    AL1(95),AL3(103)                                                 
         DC    AL1(95),AL3(103)                                                 
         SPACE 1                                                                
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(76)                                                  
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(65)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
         SPACE 3                                                                
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL1(80),AL3(30780)                                               
         DC    AL1(80),AL3(30780)                                               
         DC    AL1(95),AL3(16740)                                               
         DC    AL1(95),AL3(14860)                                               
         DC    AL1(95),AL3(13190)                                               
         SPACE 1                                                                
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(135)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(108)                                                 
         SPACE 1                                                                
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(76)                                                  
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(65)                                                  
         SPACE 1                                                                
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(27915)                                               
         DC    AL1(80),AL3(27915)                                               
         DC    AL1(95),AL3(16740)                                               
         DC    AL1(95),AL3(14860)                                               
         DC    AL1(95),AL3(13190)                                               
         SPACE 1                                                                
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(135)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(108)                                                 
         SPACE 1                                                                
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(76)                                                  
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(65)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
         SPACE 3                                                                
         DC    AL2(23,32,0,0,0,0)   ANY TWO ALONE                               
         DC    AL1(80),AL3(37540)                                               
         DC    AL1(80),AL3(37540)                                               
         DC    AL1(95),AL3(19990)                                               
         DC    AL1(95),AL3(15335)                                               
         DC    AL1(95),AL3(13650)                                               
         SPACE 1                                                                
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(108)                                                 
         SPACE 1                                                                
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(76)                                                  
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(65)                                                  
         SPACE 1                                                                
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL1(80),AL3(47435)                                               
         DC    AL1(80),AL3(47435)                                               
         DC    AL1(95),AL3(22270)                                               
         DC    AL1(95),AL3(17235)                                               
         DC    AL1(95),AL3(15335)                                               
         SPACE 1                                                                
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(113)                                                 
         DC    AL1(95),AL3(108)                                                 
         SPACE 1                                                                
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(80),AL3(227)                                                 
         DC    AL1(95),AL3(76)                                                  
         DC    AL1(95),AL3(65)                                                  
         DC    AL1(95),AL3(65)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
         SPACE 3                                                                
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(60620)          AR,AS,P,ANN                                  
         DC    AL4(48085)          S,1-4MS,1-4SS                                
         DC    AL4(31350)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(25085)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(15675)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(15855)          SE                                           
         SPACE 1                                                                
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(37930)          ANN ALONE                                    
         DC    AL4(37930)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(28460)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(28460)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(28460)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(8890)           SE                                           
         SPACE 1                                                                
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(61540)                                                       
         DC    AL4(61540)                                                       
         DC    AL4(47325)                                                       
         DC    AL4(42320)                                                       
         DC    AL4(38660)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
N08TAB   DC    AL2(28,36,0,0,0,0) NETWORK 8 WEEK                                
         DC    AL4(98030)                                                       
         DC    AL4(98030)                                                       
         DC    AL4(75425)                                                       
         DC    AL4(67405)                                                       
         DC    AL4(60380)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(121640)                                                      
         DC    AL4(121640)                                                      
         DC    AL4(93560)                                                       
         DC    AL4(83655)                                                       
         DC    AL4(76640)                                                       
         DC    AL4(8890)                                                        
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
         SPACE 3                                                                
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(127375)                                                      
         DC    AL4(127375)                                                      
         DC    AL4(97950)                                                       
         DC    AL4(87595)                                                       
         DC    AL4(80250)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(60830)                                                       
         DC    AL4(60830)                                                       
         DC    AL4(46770)                                                       
         DC    AL4(41825)                                                       
         DC    AL4(38215)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(91600)                                                       
         DC    AL4(91600)                                                       
         DC    AL4(64140)                                                       
         DC    AL4(57250)                                                       
         DC    AL4(52015)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(73405)                                                       
         DC    AL4(73405)                                                       
         DC    AL4(34405)                                                       
         DC    AL4(34405)                                                       
         DC    AL4(34405)                                                       
         DC    AL4(8890)                                                        
         SPACE 1                                                                
         DC    AL2(34,36,0,0,0,0) REGIONAL - WITH ANY MAJORS                    
         DC    AL4(73405)                                                       
         DC    AL4(73405)                                                       
         DC    AL4(73405)                                                       
         DC    AL4(66065)                                                       
         DC    AL4(59415)                                                       
         DC    AL4(8890)                                                        
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
         SPACE 2                                                                
MUSTAB   DC    AL2(35,24,0,0,0,0)  REUSE                                        
         DC    AL4(7050)           CAST=1                                       
         DC    AL4(7050)                2-4                                     
         DC    AL4(7050)                5+                                      
*                                                                               
         DC    AL2(79,24,0,0,0,0)  98 REUSE                                     
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
*                                                                               
         DC    AL2(359,24,0,0,0,0) 01 REUSE                                     
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
*                                                                               
FMUTAB   DC    AL2(36,24,0,0,0,0)  FIRST REUSE                                  
         DC    AL4(4800)           CAST=1                                       
         DC    AL4(2400)                2-4                                     
         DC    AL4(2400)                5+                                      
         SPACE 2                                                                
         DC    AL2(114,24,0,0,0,0)  98 FIRST REUSE                              
         DC    AL4(5600)           CAST=1                                       
         DC    AL4(2800)                2-4                                     
         DC    AL4(2800)                5+                                      
*                                                                               
         DC    AL2(361,24,0,0,0,0)  01 FIRST REUSE                              
         DC    AL4(6000)           CAST=1                                       
         DC    AL4(3000)                2-4                                     
         DC    AL4(3000)                5+                                      
*                                                                               
BSMTAB   DC    AL2(60,24,0,0,0,0)  SESSION                                      
         DC    AL4(9400)           CAST=1                                       
         DC    AL4(9400)                2-4                                     
         DC    AL4(9400)                5+                                      
         SPACE                                                                  
         DC    AL2(194,24,0,0,0,0)  98 SESSION                                  
         DC    AL4(10000)          CAST=1                                       
         DC    AL4(10000)               2-4                                     
         DC    AL4(10000)               5+                                      
*                                                                               
         DC    AL2(366,24,0,0,0,0)  01 SESSION                                  
         DC    AL4(10600)          CAST=1                                       
         DC    AL4(10600)               2-4                                     
         DC    AL4(10600)               5+                                      
         SPACE 3                                                                
IMSTAB   DC    AL2(59,20,0,0,0,0)  SESSION   OLD IMS RATES                      
         DC    AL4(16548)          ON CAM                                       
         DC    AL4(16363)          OFF CAM                                      
         SPACE 3                                                                
         DC    AL2(257,20,0,0,0,0)  SESSION  >= 2/13/00 IMS RATES               
         DC    AL4(17044)           ON CAM                                      
         DC    AL4(16854)           OFF CAM                                     
         SPACE 3                                                                
         DC    AL2(258,20,0,0,0,0)  SESSION  >= 1/18/01 IMS RATES               
         DC    AL4(17555)           ON CAM                                      
         DC    AL4(17360)           OFF CAM                                     
         SPACE 3                                                                
         DC    AL2(370,20,0,0,0,0)  SESSION  >= 2/16/02 IMS RATES               
         DC    AL4(18082)           ON CAM                                      
         DC    AL4(17881)           OFF CAM                                     
         SPACE 3                                                                
         DC    AL2(371,20,0,0,0,0)  SESSION  >= 2/16/03 IMS RATES               
         DC    AL4(18624)           ON CAM                                      
         DC    AL4(18417)           OFF CAM                                     
         SPACE 3                                                                
         DC    AL2(372,20,0,0,0,0)  SESSION  >= 2/16/04 IMS RATES               
         DC    AL4(19183)           ON CAM                                      
         DC    AL4(18970)           OFF CAM                                     
         SPACE 3                                                                
MS8TAB   DC    AL2(09,24,0,0,0,0)  8-WEEK REUSE                                 
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
         SPACE 3                                                                
         DC    AL2(89,24,0,0,0,0)  98 8-WEEK REUSE                              
         DC    AL1(80),AL3(7500)                                                
         DC    AL1(80),AL3(7500)                                                
         DC    AL1(80),AL3(7500)                                                
*                                                                               
         DC    AL2(360,24,0,0,0,0) 01 8-WEEK REUSE                              
         DC    AL1(80),AL3(7950)                                                
         DC    AL1(80),AL3(7950)                                                
         DC    AL1(80),AL3(7950)                                                
         EJECT                                                                  
*              MUSIC FOREIGN USE                                                
         SPACE 2                                                                
FGMTAB   DC    AL2(45,24,0,0,0,0)  EUROPE OR OUTSIDE EUROPE-12M                 
         DC    AL4(5875)           CAST=1                                       
         DC    AL4(5875)                2-4                                     
         DC    AL4(5875)                5+                                      
         SPACE                                                                  
         DC    AL2(124,24,0,0,0,0)  98 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(6250)           CAST=1                                       
         DC    AL4(6250)                2-4                                     
         DC    AL4(6250)                5+                                      
         SPACE                                                                  
         DC    AL2(362,24,0,0,0,0)  01 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(6625)           CAST=1                                       
         DC    AL4(6625)                2-4                                     
         DC    AL4(6625)                5+                                      
         SPACE                                                                  
         DC    AL2(46,24,0,0,0,0)  WORLD - 12M                                  
         DC    AL4(9400)           CAST=1                                       
         DC    AL4(9400)                2-4                                     
         DC    AL4(9400)                5+                                      
         SPACE                                                                  
         DC    AL2(159,24,0,0,0,0)  98 WORLD - 12M                              
         DC    AL4(10000)          CAST=1                                       
         DC    AL4(10000)               2-4                                     
         DC    AL4(10000)               5+                                      
         SPACE                                                                  
         DC    AL2(363,24,0,0,0,0)  01 WORLD - 12M                              
         DC    AL4(10600)          CAST=1                                       
         DC    AL4(10600)               2-4                                     
         DC    AL4(10600)               5+                                      
         SPACE                                                                  
         DC    AL2(47,24,0,0,0,0)  EUROPE OR OUTSIDE EUROPE-24M                 
         DC    AL4(8815)           CAST=1                                       
         DC    AL4(8815)                2-4                                     
         DC    AL4(8815)                5+                                      
         SPACE                                                                  
         DC    AL2(149,24,0,0,0,0)  98 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(9375)           CAST=1                                       
         DC    AL4(9375)                2-4                                     
         DC    AL4(9375)                5+                                      
         SPACE                                                                  
         DC    AL2(364,24,0,0,0,0)  01 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(9938)           CAST=1                                       
         DC    AL4(9938)                2-4                                     
         DC    AL4(9938)                5+                                      
         SPACE                                                                  
         DC    AL2(48,24,0,0,0,0)  WORLD - 24M                                  
         DC    AL4(14100)          CAST=1                                       
         DC    AL4(14100)              2-4                                      
         DC    AL4(14100)              5+                                       
         SPACE                                                                  
         DC    AL2(184,24,0,0,0,0)  98 WORLD - 24M                              
         DC    AL4(15000)          CAST=1                                       
         DC    AL4(15000)              2-4                                      
         DC    AL4(15000)              5+                                       
         SPACE                                                                  
         DC    AL2(365,24,0,0,0,0)  01 WORLD - 24M                              
         DC    AL4(15900)          CAST=1                                       
         DC    AL4(15900)              2-4                                      
         DC    AL4(15900)              5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
         SPACE 1                                                                
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(20000)          ANN ALONE                                    
         DC    AL4(20000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(14730)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(13035)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11570)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         DC    AL4(20300)                                                       
         DC    AL4(17615)                                                       
         DC    AL4(14365)                                                       
         DC    AL4(25920)                                                       
         DC    AL4(39555)                                                       
         DC    AL4(15045)                                                       
         DC    AL4(26310)                                                       
         DC    AL4(73730)          PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(47870)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(35995)                                               
         DC    AL1(50),AL3(35045)                                               
         DC    AL1(50),AL3(31025)                                               
         DC    AL1(50),AL3(25660)                                               
         DC    AL1(50),AL3(20300)                                               
         DC    AL1(50),AL3(17615)                                               
         DC    AL1(50),AL3(14365)                                               
         DC    AL1(50),AL3(25920)                                               
         DC    AL1(50),AL3(39555)                                               
         DC    AL1(50),AL3(15045)                                               
         DC    AL1(50),AL3(26310)                                               
         DC    AL1(50),AL3(73730)          PIL                                  
         DC    AL4(0)                      N/D                                  
         DC    AL1(50),AL3(29475)          SE                                   
         DC    AL1(50),AL3(7675)           C3,C6                                
         DC    AL1(50),AL3(15135)          C9                                   
         SPACE 1                                                                
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         DC    AL4(20300)                                                       
         DC    AL4(17615)                                                       
         DC    AL4(14365)                                                       
         DC    AL4(25920)                                                       
         DC    AL4(39555)                                                       
         DC    AL4(25920)                                                       
         DC    AL4(39555)                                                       
         DC    AL4(73730)          PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         EJECT                                                                  
         DC    AL2(63,32,0,0,0,0)  TV HOLDING RATES - HLD                       
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         SPACE 1                                                                
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(47870)                                               
         DC    AL1(50),AL3(35995)                                               
         DC    AL1(50),AL3(35045)                                               
         DC    AL1(50),AL3(31025)                                               
         DC    AL1(50),AL3(25660)                                               
         DC    AL1(50),AL3(20300)                                               
         DC    AL1(50),AL3(17615)                                               
         DC    AL1(50),AL3(14365)                                               
         DC    AL1(50),AL3(25920)                                               
         DC    AL1(50),AL3(39555)                                               
         DC    AL1(50),AL3(15045)                                               
         DC    AL1(50),AL3(26310)                                               
         DC    AL1(50),AL3(73730)  PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL1(50),AL3(29475)  SE                                           
         DC    AL1(50),AL3(7675)   C3,C6                                        
         DC    AL1(50),AL3(15135)  C9                                           
         SPACE 1                                                                
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(47870)                                              
         DC    AL1(200),AL3(35995)                                              
         DC    AL1(200),AL3(35045)                                              
         DC    AL1(200),AL3(31025)                                              
         DC    AL1(200),AL3(25660)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(15045)                                              
         DC    AL1(200),AL3(26310)                                              
         EJECT                                                                  
*              CABLE RATES                                                      
         SPACE 3                                                                
         DC    AL2(41,44,1,1,0,0)  CBL & SCB - MINIMUM                          
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         DC    AL4(20300)                                                       
         DC    AL4(17615)                                                       
         DC    AL4(14365)                                                       
         SPACE 1                                                                
         DC    AL2(41,12,2,115,0,0)  MINIMUM COVERS UPTO 115                    
         SPACE 1                                                                
         DC    AL2(41,44,116,116,0,0)                                           
         DC    AL4(222)                                                         
         DC    AL4(153)                                                         
         DC    AL4(157)                                                         
         DC    AL4(113)                                                         
         DC    AL4(86)                                                          
         DC    AL4(88)                                                          
         DC    AL4(69)                                                          
         DC    AL4(51)                                                          
         SPACE 1                                                                
         DC    AL2(41,44,117,150,0,0)                                           
         DC    AL4(337)                                                         
         DC    AL4(253)                                                         
         DC    AL4(247)                                                         
         DC    AL4(218)                                                         
         DC    AL4(181)                                                         
         DC    AL4(143)                                                         
         DC    AL4(124)                                                         
         DC    AL4(101)                                                         
         SPACE 1                                                                
         DC    AL2(41,44,151,200,0,0)                                           
         DC    AL4(277)                                                         
         DC    AL4(209)                                                         
         DC    AL4(203)                                                         
         DC    AL4(180)                                                         
         DC    AL4(149)                                                         
         DC    AL4(118)                                                         
         DC    AL4(102)                                                         
         DC    AL4(83)                                                          
         SPACE 1                                                                
         DC    AL2(41,44,201,1500,0,0)                                          
         DC    AL4(35)                                                          
         DC    AL4(26)                                                          
         DC    AL4(26)                                                          
         DC    AL4(23)                                                          
         DC    AL4(19)                                                          
         DC    AL4(15)                                                          
         DC    AL4(13)                                                          
         DC    AL4(10)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
         SPACE 1                                                                
         DC    AL2(42,84,1,255,0,0)  DEM (TV)                                   
         DC    AL4(35995)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(18000)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(26275)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(23265)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(19240)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(9095)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(9095)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(9095)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(15045)                                                       
         DC    AL4(26310)                                                       
         DC    3AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(13910)          'OFF' S                                      
         SPACE 1                                                                
         DC    AL2(43,48,1,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(13780)          ANN ALONE                                    
         DC    AL4(13780)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(9095)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(9095)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(9095)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(13910)          SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(24360)                                                       
         DC    AL4(24360)                                                       
         DC    AL4(24360)                                                       
         DC    AL4(24360)                                                       
         DC    AL4(24360)                                                       
         DC    AL4(8715)                                                        
         EJECT                                                                  
*              FOREIGN REUSE                                                    
         SPACE                                                                  
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(143610)         (3 X 47870)                                  
         DC    AL4(107985)                                                      
         DC    AL4(105135)                                                      
         DC    AL4(93075)                                                       
         DC    AL4(76980)                                                       
         DC    AL4(60900)                                                       
         DC    AL4(52845)                                                       
         DC    AL4(43095)                                                       
         DC    AL4(77760)                                                       
         DC    AL4(118665)                                                      
         DC    AL4(45135)                                                       
         DC    AL4(78930)                                                       
         DC    AL4(221190)         PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(88425)          SE                                           
         DC    AL4(23025)          C3,C6                                        
         DC    AL4(45405)          C9                                           
         SPACE                                                                  
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL1(200),AL3(47870)                                              
         DC    AL1(200),AL3(35995)                                              
         DC    AL1(200),AL3(35045)                                              
         DC    AL1(200),AL3(31025)                                              
         DC    AL1(200),AL3(25660)                                              
         DC    AL1(200),AL3(20300)                                              
         DC    AL1(200),AL3(17615)                                              
         DC    AL1(200),AL3(14365)                                              
         DC    AL1(200),AL3(25920)                                              
         DC    AL1(200),AL3(39555)                                              
         DC    AL1(200),AL3(15045)                                              
         DC    AL1(200),AL3(26310)                                              
         DC    AL1(200),AL3(73730) PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL1(200),AL3(29475) SE                                           
         DC    AL1(200),AL3(7675)  C3,C6                                        
         DC    AL1(200),AL3(15135) C9                                           
         SPACE                                                                  
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 8X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(382960)        (8 X 47870)                                   
         DC    AL4(287960)                                                      
         DC    AL4(280360)                                                      
         DC    AL4(248200)                                                      
         DC    AL4(205280)                                                      
         DC    AL4(162400)                                                      
         DC    AL4(140920)                                                      
         DC    AL4(114920)                                                      
         DC    AL4(207360)                                                      
         DC    AL4(316440)                                                      
         DC    AL4(120360)                                                      
         DC    AL4(210480)                                                      
         DC    AL4(589840)         PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(235800)         SE                                           
         DC    AL4(61400)          C3,C6                                        
         DC    AL4(121080)         C9                                           
         SPACE                                                                  
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(44470)          N/D                                          
         DC    AL4(44470)          P,ANN,S,D,ACR                                
         DC    AL4(25795)          3-5 GROUP                                    
         DC    AL4(17790)          6-8 GROUP                                    
         DC    AL4(14230)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(50715)          P,ANN,ACR                                    
         DC    AL4(52660)          S,D                                          
         DC    AL4(34335)          3-5 GROUP                                    
         DC    AL4(27465)          6-8 GROUP                                    
         DC    AL4(17175)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
         SPACE                                                                  
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK                                      
         DC    AL4(139985)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(105275)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(102480)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(90710)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(75025)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(59470)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(51515)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(42000)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE 1                                                                
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(353)                                                         
         DC    AL4(257)                                                         
         DC    AL4(251)                                                         
         DC    AL4(232)                                                         
         DC    AL4(180)                                                         
         DC    AL4(148)                                                         
         DC    AL4(134)                                                         
         DC    AL4(96)                                                          
         EJECT                                                                  
*              ADDENDUM USES                                                    
         SPACE 1                                                                
ADTTBL   DC    AL2(65,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA                
         DC    AL4(29100)          ON CAMERA                                    
         DC    AL4(21900)          OFF                                          
         DC    AL4(16000)                                                       
         DC    AL4(14100)                                                       
         DC    AL4(11700)                                                       
         DC    AL4(9200)                                                        
         DC    AL4(8000)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(29100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(21800)          SOLO/DUO ON CAM                              
         DC    AL4(16400)          SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL2(70,88,0,0,0,0)  1 WEEK - GA                                  
         DC    AL4(31200)          ON CAMERA                                    
         DC    AL4(23500)          OFF                                          
         DC    AL4(17100)                                                       
         DC    AL4(15150)                                                       
         DC    AL4(12525)                                                       
         DC    AL4(9900)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(31200)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(23325)          SOLO/DUO ON CAM                              
         DC    AL4(17600)          SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL2(71,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS               
         DC    AL4(24500)          ON CAMERA                                    
         DC    AL4(18400)          OFF                                          
         DC    AL4(18900)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(12900)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(6700)                                                        
         DC    AL4(4800)                                                        
         DC    AL4(15600)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(8800)                                                        
         DC    AL4(13900)                                                       
         DC    AL4(24500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(80,88,0,0,0,0)  4 WEEK - GA                                  
         DC    AL4(33300)          ON CAMERA                                    
         DC    AL4(25000)          OFF                                          
         DC    AL4(18200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(9100)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(30900)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(24900)          SOLO/DUO ON CAM                              
         DC    AL4(18700)          SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL2(81,80,0,0,0,0)  31 DAY - KS                                  
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(23600)          OFF                                          
         DC    AL4(23400)                                                       
         DC    AL4(19900)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(8100)                                                        
         DC    AL4(6100)                                                        
         DC    AL4(15600)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(8800)                                                        
         DC    AL4(13900)                                                       
         DC    AL4(31400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(90,88,0,0,0,0)  13 WEEK - GA                                 
         DC    AL4(41600)          ON CAMERA                                    
         DC    AL4(31300)          OFF                                          
         DC    AL4(22800)                                                       
         DC    AL4(20200)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(13200)                                                       
         DC    AL4(11400)                                                       
         DC    AL4(9300)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(38600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(31100)          SOLO/DUO ON CAM                              
         DC    AL4(23400)          SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL2(91,80,0,0,0,0)  13 WEEKS - KS                                
         DC    AL4(38300)          ON CAMERA                                    
         DC    AL4(28700)          OFF                                          
         DC    AL4(27700)                                                       
         DC    AL4(23600)                                                       
         DC    AL4(18900)                                                       
         DC    AL4(12100)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(15600)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(8800)                                                        
         DC    AL4(13900)                                                       
         DC    AL4(38300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(92,80,0,0,0,0)  13 WEEKS - TX                                
         DC    AL4(36500)          ON CAMERA                                    
         DC    AL4(26030)          OFF                                          
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         DC    AL4(14150)                                                       
         DC    AL4(14150)                                                       
         DC    AL4(14150)                                                       
         DC    AL4(19980)                                                       
         DC    AL4(27430)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(18250)                                                       
         DC    AL4(36500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 3                                                                
ADOTBL   DC    AL2(100,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA            
         DC    AL4(12200)                                                       
         DC    AL4(12200)                                                       
         DC    AL4(6900)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(9100)           SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL2(105,48,0,0,0,0)  1 WEEK - GA                                 
         DC    AL4(13100)                                                       
         DC    AL4(13100)                                                       
         DC    AL4(7400)                                                        
         DC    AL4(6400)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(9800)                                                        
         SPACE 1                                                                
         DC    AL2(106,44,0,0,0,0)  1 WEEK - KS                                 
         DC    AL4(9900)                                                        
         DC    AL4(9900)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(115,48,0,0,0,0)  4 WEEK - GA                                 
         DC    AL4(13900)                                                       
         DC    AL4(13900)                                                       
         DC    AL4(7800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(10400)                                                       
         SPACE 1                                                                
         DC    AL2(116,44,0,0,0,0)  31 DAY - KS                                 
         DC    AL4(12700)                                                       
         DC    AL4(12700)                                                       
         DC    AL4(7500)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(125,48,0,0,0,0)  13 WEEK - GA                                
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(7500)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(13000)                                                       
         SPACE 1                                                                
         DC    AL2(126,44,0,0,0,0)  13 WEEK - KS                                
         DC    AL4(15500)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(127,44,0,0,0,0)  13 WEEK - TX                                
         DC    AL4(15770)                                                       
         DC    AL4(15770)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 3                                                                
ADHTAB   DC    AL2(225,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                 
         DC    AL4(41600)                                                       
         DC    AL4(31300)                                                       
         DC    AL4(22800)                                                       
         DC    AL4(20200)                                                       
         DC    AL4(16700)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(31100)                                                       
         DC    AL4(23400)                                                       
         SPACE 1                                                                
         DC    AL2(226,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                 
         DC    AL4(38300)           ON CAMERA                                   
         DC    AL4(28700)           OFF                                         
         DC    AL4(27700)                                                       
         DC    AL4(23600)                                                       
         DC    AL4(18900)                                                       
         SPACE 1                                                                
         DC    AL2(227,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                 
         DC    AL4(36500)           ON CAMERA                                   
         DC    AL4(26030)           OFF                                         
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         SPACE 3                                                                
*                                   ADDENDUM REINSTSATEMENT-GA                  
ARNTAB   DC    AL2(230,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                 
         DC    AL1(200),AL3(41600)                                              
         DC    AL1(200),AL3(31300)                                              
         DC    AL1(200),AL3(22800)                                              
         DC    AL1(200),AL3(20200)                                              
         DC    AL1(200),AL3(16700)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(31100)                                              
         DC    AL1(200),AL3(23400)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(231,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(38300)             ON CAMERA                        
         DC    AL1(200),AL3(28700)             OFF                              
         DC    AL1(200),AL3(27700)                                              
         DC    AL1(200),AL3(23600)                                              
         DC    AL1(200),AL3(18900)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(232,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(36500)             ON CAMERA                        
         DC    AL1(200),AL3(26030)             OFF                              
         DC    AL1(200),AL3(24520)                                              
         DC    AL1(200),AL3(24520)                                              
         DC    AL1(200),AL3(24520)                                              
         SPACE 3                                                                
ADDTAB   DC    AL2(205,80,0,0,0,0)  ADDENDUM DEMO (TV) - GA                     
         DC    AL4(31300)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(15700)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(31300)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(31300)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(31300)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(15700)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(15700)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(15700)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(206,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                     
         DC    AL4(8600)           'ON'                                         
         DC    AL4(7600)           'OFF'                                        
         DC    AL4(8600)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE 1                                                                
         DC    AL2(207,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                     
         DC    AL4(24840)          'ON'                                         
         DC    AL4(12200)          'OFF'                                        
         DC    AL4(11880)                                                       
         DC    AL4(11880)                                                       
         DC    AL4(11880)                                                       
         DC    AL4(5720)                                                        
         DC    AL4(5720)                                                        
         DC    AL4(5720)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(24840)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(9180)           SOLO/DUO OFF CAM                             
         SPACE 1                                                                
         DC    AL2(215,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA              
         DC    AL4(12000)          ANN ALONE                                    
         DC    AL4(12000)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(12000)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(12000)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(12000)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(216,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS              
         DC    AL4(6600)           ANN ALONE                                    
         DC    AL4(6600)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(6600)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6600)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6600)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         SPACE 1                                                                
         DC    AL2(217,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX              
         DC    AL4(11120)          ANN ALONE                                    
         DC    AL4(11120)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(5720)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(5720)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5720)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(9180)           SOLO/DUO                                     
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
         SPACE 3                                                                
ADWTAB   DC    AL2(135,52,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(29100)          ON CAMERA                                    
         DC    AL4(21900)          OFF                                          
         DC    AL4(16000)                                                       
         DC    AL4(14100)                                                       
         DC    AL4(11700)                                                       
         DC    AL4(9200)                                                        
         DC    AL4(8000)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(21800)          SOLO/DUO ON CAMERA                           
         DC    AL4(16400)          OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL2(135,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1062)                                                        
         DC    AL4(727)                                                         
         DC    AL4(618)                                                         
         DC    AL4(533)                                                         
         DC    AL4(436)                                                         
         DC    AL4(219)                                                         
         DC    AL4(173)                                                         
         DC    AL4(144)                                                         
         DC    AL4(794)                                                         
         DC    AL4(543)                                                         
         SPACE 1                                                                
         DC    AL2(135,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(394)                                                         
         DC    AL4(309)                                                         
         DC    AL4(319)                                                         
         DC    AL4(270)                                                         
         DC    AL4(224)                                                         
         DC    AL4(92)                                                          
         DC    AL4(63)                                                          
         DC    AL4(58)                                                          
         DC    AL4(295)                                                         
         DC    AL4(231)                                                         
         SPACE 1                                                                
         DC    AL2(135,52,61,255,0,0) UNITS 61+                                 
         DC    AL4(394)                                                         
         DC    AL4(309)                                                         
         DC    AL4(231)                                                         
         DC    AL4(180)                                                         
         DC    AL4(151)                                                         
         DC    AL4(56)                                                          
         DC    AL4(32)                                                          
         DC    AL4(32)                                                          
         DC    AL4(295)                                                         
         DC    AL4(231)                                                         
         SPACE 1                                                                
         DC    AL2(140,52,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(31200)          ON CAMERA                                    
         DC    AL4(23500)          OFF                                          
         DC    AL4(17100)                                                       
         DC    AL4(15150)                                                       
         DC    AL4(12525)                                                       
         DC    AL4(9900)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(23325)                                                       
         DC    AL4(17600)                                                       
         SPACE 1                                                                
         DC    AL2(140,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1138)                                                        
         DC    AL4(779)                                                         
         DC    AL4(662)                                                         
         DC    AL4(572)                                                         
         DC    AL4(467)                                                         
         DC    AL4(235)                                                         
         DC    AL4(185)                                                         
         DC    AL4(154)                                                         
         DC    AL4(851)                                                         
         DC    AL4(581)                                                         
         SPACE 1                                                                
         DC    AL2(140,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(422)                                                         
         DC    AL4(332)                                                         
         DC    AL4(342)                                                         
         DC    AL4(290)                                                         
         DC    AL4(240)                                                         
         DC    AL4(99)                                                          
         DC    AL4(68)                                                          
         DC    AL4(62)                                                          
         DC    AL4(316)                                                         
         DC    AL4(248)                                                         
         SPACE 1                                                                
         DC    AL2(140,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(422)                                                         
         DC    AL4(332)                                                         
         DC    AL4(248)                                                         
         DC    AL4(193)                                                         
         DC    AL4(162)                                                         
         DC    AL4(60)                                                          
         DC    AL4(35)                                                          
         DC    AL4(35)                                                          
         DC    AL4(316)                                                         
         DC    AL4(248)                                                         
         SPACE 1                                                                
         DC    AL2(141,44,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(24500)          ON CAMERA                                    
         DC    AL4(18400)          OFF                                          
         DC    AL4(18900)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(12900)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(6700)                                                        
         DC    AL4(4800)                                                        
         SPACE 1                                                                
         DC    AL2(141,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(930)                                                         
         DC    AL4(930)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         SPACE 1                                                                
         DC    AL2(150,52,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(33300)          ON CAMERA                                    
         DC    AL4(25000)          OFF                                          
         DC    AL4(18200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(9100)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(24900)          SOLO/DUO ON CAMERA                           
         DC    AL4(18700)          OFF CAMERA                                   
         SPACE 1                                                                
         DC    AL2(150,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1214)                                                        
         DC    AL4(830)                                                         
         DC    AL4(706)                                                         
         DC    AL4(610)                                                         
         DC    AL4(498)                                                         
         DC    AL4(250)                                                         
         DC    AL4(198)                                                         
         DC    AL4(164)                                                         
         DC    AL4(907)                                                         
         DC    AL4(620)                                                         
         SPACE 1                                                                
         DC    AL2(150,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(450)                                                         
         DC    AL4(354)                                                         
         DC    AL4(365)                                                         
         DC    AL4(309)                                                         
         DC    AL4(256)                                                         
         DC    AL4(106)                                                         
         DC    AL4(72)                                                          
         DC    AL4(66)                                                          
         DC    AL4(337)                                                         
         DC    AL4(264)                                                         
         SPACE 1                                                                
         DC    AL2(150,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(410)                                                         
         DC    AL4(325)                                                         
         DC    AL4(264)                                                         
         DC    AL4(206)                                                         
         DC    AL4(173)                                                         
         DC    AL4(64)                                                          
         DC    AL4(37)                                                          
         DC    AL4(37)                                                          
         DC    AL4(337)                                                         
         DC    AL4(264)                                                         
         SPACE 1                                                                
         DC    AL2(151,44,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(23600)          OFF                                          
         DC    AL4(23400)                                                       
         DC    AL4(19900)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(8100)                                                        
         DC    AL4(6100)                                                        
         SPACE 1                                                                
         DC    AL2(151,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(930)                                                         
         DC    AL4(930)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         SPACE 1                                                                
         DC    AL2(160,52,1,1,0,0) 13 WEEK - GA - UNIT 1                        
         DC    AL4(41600)          ON CAMERA                                    
         DC    AL4(31300)          OFF                                          
         DC    AL4(22800)                                                       
         DC    AL4(20200)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(13200)                                                       
         DC    AL4(11400)                                                       
         DC    AL4(9300)                                                        
         DC    AL4(31100)          SOLO/DUO ON CAMERA                           
         DC    AL4(23400)          OFF                                          
         SPACE 1                                                                
         DC    AL2(160,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1517)                                                        
         DC    AL4(1038)                                                        
         DC    AL4(883)                                                         
         DC    AL4(762)                                                         
         DC    AL4(623)                                                         
         DC    AL4(313)                                                         
         DC    AL4(247)                                                         
         DC    AL4(205)                                                         
         DC    AL4(1134)                                                        
         DC    AL4(775)                                                         
         SPACE 1                                                                
         DC    AL2(160,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(563)                                                         
         DC    AL4(442)                                                         
         DC    AL4(456)                                                         
         DC    AL4(386)                                                         
         DC    AL4(320)                                                         
         DC    AL4(132)                                                         
         DC    AL4(90)                                                          
         DC    AL4(83)                                                          
         DC    AL4(421)                                                         
         DC    AL4(330)                                                         
         SPACE 1                                                                
         DC    AL2(160,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(563)                                                         
         DC    AL4(442)                                                         
         DC    AL4(330)                                                         
         DC    AL4(257)                                                         
         DC    AL4(216)                                                         
         DC    AL4(80)                                                          
         DC    AL4(46)                                                          
         DC    AL4(46)                                                          
         DC    AL4(421)                                                         
         DC    AL4(330)                                                         
         SPACE 1                                                                
         DC    AL2(161,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                      
         DC    AL4(38300)          ON CAMERA                                    
         DC    AL4(28700)          OFF                                          
         DC    AL4(27700)                                                       
         DC    AL4(23600)                                                       
         DC    AL4(18900)                                                       
         DC    AL4(12100)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(7300)                                                        
         SPACE 1                                                                
         DC    AL2(161,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(930)                                                         
         DC    AL4(930)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         SPACE 1                                                                
         DC    AL2(162,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                      
         DC    AL4(36500)           ON CAMERA                                   
         DC    AL4(26030)           OFF                                         
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         DC    AL4(24520)                                                       
         DC    AL4(14150)                                                       
         DC    AL4(14150)                                                       
         DC    AL4(14150)                                                       
         SPACE 1                                                                
         DC    AL2(162,44,2,13,0,0)  UNITS 2-13                                 
         DC    AL4(780)                                                         
         DC    AL4(570)                                                         
         DC    AL4(320)                                                         
         DC    AL4(320)                                                         
         DC    AL4(320)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         SPACE 1                                                                
         DC    AL2(162,44,14,255,0,0)  UNITS 14+                                
         DC    AL4(535)                                                         
         DC    AL4(385)                                                         
         DC    AL4(250)                                                         
         DC    AL4(250)                                                         
         DC    AL4(250)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
         SPACE 3                                                                
         DC    AL2(170,40,1,1,0,0) 3 DAY - GA - UNIT 1                          
         DC    AL4(12200)          ANN ALONE                                    
         DC    AL4(12200)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(6900)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6000)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5300)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(12200)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(9100)           SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL2(170,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(184)                                                         
         DC    AL4(184)                                                         
         DC    AL4(71)                                                          
         DC    AL4(62)                                                          
         DC    AL4(54)                                                          
         DC    AL4(0)                                                           
         DC    AL4(137)                                                         
         SPACE 1                                                                
         DC    AL2(170,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(139)                                                         
         DC    AL4(139)                                                         
         DC    AL4(62)                                                          
         DC    AL4(47)                                                          
         DC    AL4(47)                                                          
         DC    AL4(0)                                                           
         DC    AL4(104)                                                         
         SPACE 1                                                                
         DC    AL2(170,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(139)                                                         
         DC    AL4(139)                                                         
         DC    AL4(34)                                                          
         DC    AL4(29)                                                          
         DC    AL4(29)                                                          
         DC    AL4(0)                                                           
         DC    AL4(104)                                                         
         SPACE 1                                                                
         DC    AL2(175,40,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(13100)          ANN ALONE                                    
         DC    AL4(13100)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7400)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6400)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5600)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(13100)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(9800)           SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL2(175,40,2,25,0,0) UNITS 2-25                                  
         DC    AL4(197)                                                         
         DC    AL4(197)                                                         
         DC    AL4(77)                                                          
         DC    AL4(66)                                                          
         DC    AL4(58)                                                          
         DC    AL4(0)                                                           
         DC    AL4(147)                                                         
         SPACE 1                                                                
         DC    AL2(175,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(149)                                                         
         DC    AL4(149)                                                         
         DC    AL4(66)                                                          
         DC    AL4(50)                                                          
         DC    AL4(50)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
         SPACE 1                                                                
         DC    AL2(175,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(149)                                                         
         DC    AL4(149)                                                         
         DC    AL4(37)                                                          
         DC    AL4(32)                                                          
         DC    AL4(32)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
         SPACE 1                                                                
         DC    AL2(176,36,0,0,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(9900)                                                        
         DC    AL4(9900)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(15375)          SE                                           
         SPACE 1                                                                
         DC    AL2(176,36,2,255,0,0) UNITS 2+                                   
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(330)                                                         
         SPACE 1                                                                
         DC    AL2(185,40,1,1,0,0) 4 WEEK - GA - UNIT 1                         
         DC    AL4(13900)          ANN ALONE                                    
         DC    AL4(13900)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6800)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6000)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(13900)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(10400)          SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL2(185,40,2,25,0,0) UNITS 2-25                                  
         DC    AL4(210)                                                         
         DC    AL4(210)                                                         
         DC    AL4(82)                                                          
         DC    AL4(70)                                                          
         DC    AL4(62)                                                          
         DC    AL4(0)                                                           
         DC    AL4(157)                                                         
         SPACE 1                                                                
         DC    AL2(185,40,26,60,0,0) UNITS 26-60                                
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(70)                                                          
         DC    AL4(54)                                                          
         DC    AL4(54)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
         SPACE 1                                                                
         DC    AL2(185,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(39)                                                          
         DC    AL4(34)                                                          
         DC    AL4(34)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
         SPACE 1                                                                
         DC    AL2(186,36,0,0,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(12700)                                                       
         DC    AL4(12700)                                                       
         DC    AL4(7500)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(15375)          SE                                           
         SPACE 1                                                                
         DC    AL2(186,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(330)                                                         
         SPACE 1                                                                
         DC    AL2(195,40,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(17400)          ANN ALONE                                    
         DC    AL4(17400)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(9800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8500)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7500)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17400)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(13000)          SOLO/DUO                                     
         SPACE 1                                                                
         DC    AL2(195,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(263)                                                         
         DC    AL4(263)                                                         
         DC    AL4(102)                                                         
         DC    AL4(88)                                                          
         DC    AL4(77)                                                          
         DC    AL4(0)                                                           
         DC    AL4(196)                                                         
         SPACE 1                                                                
         DC    AL2(195,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(198)                                                         
         DC    AL4(198)                                                         
         DC    AL4(88)                                                          
         DC    AL4(67)                                                          
         DC    AL4(67)                                                          
         DC    AL4(0)                                                           
         DC    AL4(148)                                                         
         SPACE 1                                                                
         DC    AL2(195,40,61,255,0,0) UNITS 61+                                 
         DC    AL4(198)                                                         
         DC    AL4(198)                                                         
         DC    AL4(49)                                                          
         DC    AL4(42)                                                          
         DC    AL4(42)                                                          
         DC    AL4(0)                                                           
         DC    AL4(148)                                                         
         SPACE 1                                                                
         DC    AL2(196,36,0,0,0,0)  13 WEEK - KS - UNIT 1                       
         DC    AL4(15500)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(15375)          SE                                           
         SPACE 1                                                                
         DC    AL2(196,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(330)                                                         
         SPACE 1                                                                
         DC    AL2(197,36,0,0,0,0)  13 WEEK - TX - UNIT 1                       
         DC    AL4(15770)                                                       
         DC    AL4(15770)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(10040)                                                       
         DC    AL4(15770)                                                       
         SPACE 1                                                                
         DC    AL2(197,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(103)                                                         
         DC    AL4(103)                                                         
         DC    AL4(103)                                                         
         DC    AL4(135)                                                         
         EJECT                                                                  
*&&DO                                                                           
*              INSERTS FOR BOOKENDS                                             
         SPACE 3                                                                
IFBTAB   DC    AL2(44,44,1,1,0,0)  UNIT 1                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         SPACE                                                                  
         DC    AL2(44,44,2,3,0,0)  UNITS 2,3                                    
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(10700)                                               
         SPACE                                                                  
         DC    AL2(44,44,4,13,0,0)  UNITS 4-13                                  
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(10700)                                               
         SPACE                                                                  
         DC    AL2(44,44,14,255,0,0)  UNITS 14+                                 
         DC    AL1(15),AL3(14125)                                               
         DC    AL1(15),AL3(10700)                                               
         DC    AL1(15),AL3(14125)                                               
         DC    AL1(15),AL3(14125)                                               
         DC    AL1(15),AL3(14125)                                               
         DC    AL1(15),AL3(10700)                                               
         DC    AL1(15),AL3(10700)                                               
         DC    AL1(15),AL3(10700)                                               
*&&                                                                             
         EJECT                                                                  
         DC    AL2(53,80,1,24,0,0)  TV TAGS - REGULAR, UNITS 1-24               
         DC    AL4(14125)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         SPACE                                                                  
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(7885)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         SPACE                                                                  
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(4320)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         SPACE 1                                                                
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         DC    AL4(20300)                                                       
         DC    AL4(17615)                                                       
         DC    AL4(14365)                                                       
         DC    AL4(25920)                                                       
         DC    AL4(39555)                                                       
         DC    AL4(15045)                                                       
         DC    AL4(26310)                                                       
         DC    AL4(73730)          PIL                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(29475)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         SPACE                                                                  
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(14125)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(14125)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         DC    AL4(10705)                                                       
         SPACE                                                                  
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(7885)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(7885)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         DC    AL4(5940)                                                        
         SPACE                                                                  
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(4320)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(4320)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         DC    AL4(3240)                                                        
         SPACE 1                                                                
         DC    AL2(55,44,1,255,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         SPACE 1                                                                
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(20000)                                                       
         DC    AL4(20000)                                                       
         DC    AL4(14730)                                                       
         DC    AL4(13035)                                                       
         DC    AL4(11570)                                                       
         DC    AL4(15375)                                                       
         DC    AL4(6865)                                                        
         DC    AL4(10980)                                                       
         SPACE                                                                  
         DC    AL2(56,44,2,255,0,0)                                             
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(76590)          (1.6 X 47870, BUT WANT NEAREST .05)          
         DC    AL4(57590)          (1.6 X 35995, BUT WANT NEAREST .05)          
         DC    AL4(56070)          (1.6 X 35045, BUT WANT NEAREST .05)          
         DC    AL1(160),AL3(31025)                                              
         DC    AL4(41055)          (1.6 X 25660, BUT WANT NEAREST .05)          
         DC    AL1(160),AL3(20300)                                              
         DC    AL4(28185)          (1.6 X 17615, BUT WANT NEAREST .05)          
         DC    AL4(22985)          (1.6 X 14365, BUT WANT NEAREST .05)          
         SPACE 3                                                                
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL1(160),AL3(20000)                                              
         DC    AL1(160),AL3(20000)                                              
         DC    AL4(23570)          (1.6 X 14730, BUT WANT NEAREST .05)          
         DC    AL4(20855)          (1.6 X 13035, BUT WANT NEAREST .05)          
         DC    AL4(18510)          (1.6 X 11570, BUT WANT NEAREST .05)          
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
         SPACE 3                                                                
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(1975)                                                        
         DC    AL4(1350)                                                        
         DC    AL4(1545)                                                        
         DC    AL4(1330)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(550)                                                         
         DC    AL4(425)                                                         
         DC    AL4(355)                                                         
         SPACE 1                                                                
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(3960)                                                        
         DC    AL4(2710)                                                        
         DC    AL4(3085)                                                        
         DC    AL4(2655)                                                        
         DC    AL4(2165)                                                        
         DC    AL4(1095)                                                        
         DC    AL4(855)                                                         
         DC    AL4(720)                                                         
         SPACE 1                                                                
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(5935)                                                        
         DC    AL4(4060)                                                        
         DC    AL4(4630)                                                        
         DC    AL4(3985)                                                        
         DC    AL4(3245)                                                        
         DC    AL4(1640)                                                        
         DC    AL4(1285)                                                        
         DC    AL4(1075)                                                        
         SPACE 1                                                                
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(7910)                                                        
         DC    AL4(5415)                                                        
         DC    AL4(6170)                                                        
         DC    AL4(5315)                                                        
         DC    AL4(4335)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(1710)                                                        
         DC    AL4(1435)                                                        
         SPACE 1                                                                
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(9885)                                                        
         DC    AL4(6770)                                                        
         DC    AL4(7710)                                                        
         DC    AL4(6645)                                                        
         DC    AL4(5415)                                                        
         DC    AL4(2740)                                                        
         DC    AL4(2145)                                                        
         DC    AL4(1800)                                                        
         SPACE 1                                                                
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(19780)                                                       
         DC    AL4(13545)                                                       
         DC    AL4(15435)                                                       
         DC    AL4(13290)                                                       
         DC    AL4(10830)                                                       
         DC    AL4(5475)                                                        
         DC    AL4(4280)                                                        
         DC    AL4(3585)                                                        
         SPACE 1                                                                
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(29660)                                                       
         DC    AL4(20310)                                                       
         DC    AL4(23145)                                                       
         DC    AL4(19935)                                                       
         DC    AL4(16250)                                                       
         DC    AL4(8220)                                                        
         DC    AL4(6425)                                                        
         DC    AL4(5385)                                                        
         SPACE 1                                                                
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(39550)                                                       
         DC    AL4(27080)                                                       
         DC    AL4(30860)                                                       
         DC    AL4(26580)                                                       
         DC    AL4(21670)                                                       
         DC    AL4(10960)                                                       
         DC    AL4(8565)                                                        
         DC    AL4(7180)                                                        
         SPACE 1                                                                
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(47870)                                                       
         DC    AL4(35995)                                                       
         DC    AL4(35045)                                                       
         DC    AL4(31025)                                                       
         DC    AL4(25660)                                                       
         DC    AL4(20300)                                                       
         DC    AL4(17615)                                                       
         DC    AL4(14365)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
         SPACE 1                                                                
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(11560)          ON CAMERA                                    
         DC    AL4(8750)           OFF                                          
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(8750)                                                        
         DC    AL4(8750)                                                        
         DC    AL4(8750)                                                        
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(11560)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8750)           SE                                           
         DC    AL4(8750)           C3,C6                                        
         DC    AL4(8750)           C9                                           
         SPACE 1                                                                
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(6480)           ON CAMERA                                    
         DC    AL4(4860)           OFF                                          
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(4860)                                                        
         DC    AL4(4860)                                                        
         DC    AL4(4860)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(6480)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4860)           SE                                           
         DC    AL4(4860)           C3,C6                                        
         DC    AL4(4860)           C9                                           
         SPACE 1                                                                
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(3560)           ON CAMERA                                    
         DC    AL4(2700)           OFF                                          
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(3560)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2700)           SE                                           
         DC    AL4(2700)           C3,C6                                        
         DC    AL4(2700)           C9                                           
         SPACE 1                                                                
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         SPACE 1                                                                
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         SPACE 1                                                                
         DC    AL2(248,44,50,255,0,0)  UNITS 50+                                
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(2270)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
         SPACE 1                                                                
         DC    AL2(249,88,1,4,0,0)  GA - TV, REGULAR, UNITS 1-4                 
         DC    AL4(12300)          ON CAMERA                                    
         DC    AL4(9300)           OFF                                          
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(9300)                                                        
         DC    AL4(9300)                                                        
         DC    AL4(9300)                                                        
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9300)           SE                                           
         DC    AL4(9300)           C3,C6                                        
         DC    AL4(9300)           C9                                           
         DC    AL4(12300)          SOLO/DUO ON CAMERA                           
         DC    AL4(9300)                    OFF                                 
         SPACE 1                                                                
         DC    AL2(249,88,5,14,0,0)  UNITS 5-14                                 
         DC    AL4(6900)           ON CAMERA                                    
         DC    AL4(5200)           OFF                                          
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5200)           SE                                           
         DC    AL4(5200)           C3,C6                                        
         DC    AL4(5200)           C9                                           
         DC    AL4(6900)           SOLO/DUO ON CAMERA                           
         DC    AL4(5200)           OFF                                          
         SPACE 1                                                                
         DC    AL2(249,88,15,255,0,0)  UNITS 15+                                
         DC    AL4(3800)           ON CAMERA                                    
         DC    AL4(2800)           OFF                                          
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2800)           SE                                           
         DC    AL4(2800)           C3,C6                                        
         DC    AL4(2800)           C9                                           
         DC    AL4(3800)           SOLO/DUO ON CAMERA                           
         DC    AL4(2800)           OFF                                          
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
         SPACE 1                                                                
         DC    AL2(250,84,0,0,0,0)  PRM FOR SAG AND AFT                         
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(21500)          OFF                                          
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(17500)          OFF                                          
         DC    AL4(17500)          OFF                                          
         DC    AL4(17500)          OFF                                          
         DC    AL4(7500)           EXTRA                                        
         DC    AL4(7500)           EXTRA                                        
         DC    AL4(7500)           EXTRA                                        
         DC    AL4(7500)           EXTRA                                        
         DC    AL4(30500)          ON                                           
         DC    AL4(8500)           SAG ONLY EXTRA                               
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21275)          SOLO/DUO OFF CAM                             
         SPACE 3                                                                
         DC    AL2(251,84,0,0,0,0)  PRR FOR AFT AND SAG                         
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(21500)          OFF                                          
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(18500)          OFF                                          
         DC    AL4(18500)          OFF                                          
         DC    AL4(18500)          OFF                                          
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(30500)          ON                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21275)          SOLO/DUO OFF CAM                             
         EJECT                                                                  
*&&DO                                                                           
VNWTAB   DC    AL2(252,44,1,1,0,0)  VNW 1ST USE AT SESSION FEE                  
         DC    AL4(47870)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(35995)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(35045)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(31025)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(25660)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(20300)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(17615)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(14365)          'OFF' 1-4M9,1-4S9,D9,S9                      
         SPACE                                                                  
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(47870)                                               
         DC    AL1(50),AL3(35995)                                               
         DC    AL1(50),AL3(35045)                                               
         DC    AL1(50),AL3(31025)                                               
         DC    AL1(50),AL3(25660)                                               
         DC    AL1(50),AL3(20300)                                               
         DC    AL1(50),AL3(17615)                                               
         DC    AL1(50),AL3(14365)                                               
         SPACE                                                                  
         DC    AL2(253,44,1,255,0,0)  VNW WITH NO SESS RATE                     
         DC    AL1(50),AL3(47870)                                               
         DC    AL1(50),AL3(35995)                                               
         DC    AL1(50),AL3(35045)                                               
         DC    AL1(50),AL3(31025)                                               
         DC    AL1(50),AL3(25660)                                               
         DC    AL1(50),AL3(20300)                                               
         DC    AL1(50),AL3(17615)                                               
         DC    AL1(50),AL3(14365)                                               
         SPACE 3                                                                
         DC    AL2(254,32,1,1,0,0)  VNW RADIO 1ST USE AT SESSION FEE            
         DC    AL4(20000)                                                       
         DC    AL4(20000)                                                       
         DC    AL4(14730)                                                       
         DC    AL4(13035)                                                       
         DC    AL4(11570)                                                       
         SPACE                                                                  
         DC    AL2(254,32,2,255,0,0)                                            
         DC    AL1(50),AL3(20000)                                               
         DC    AL1(50),AL3(20000)                                               
         DC    AL1(50),AL3(14730)                                               
         DC    AL1(50),AL3(13035)                                               
         DC    AL1(50),AL3(11570)                                               
         SPACE                                                                  
         DC    AL2(57,32,1,255,0,0)  VNW RADIO WITH NO SESS FEE                 
         DC    AL1(50),AL3(20000)                                               
         DC    AL1(50),AL3(20000)                                               
         DC    AL1(50),AL3(14730)                                               
         DC    AL1(50),AL3(13035)                                               
         DC    AL1(50),AL3(11570)                                               
*&&                                                                             
         SPACE 2                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL2(59),AL1(UIMS,ALL,AFM+NON,0,0,0,ALL)    SESSION               
         DC    AL2(60),AL1(UBSM,ALL,AFM+NON,0,0,0,ALL)                          
         DC    AL2(60),AL1(UMRR,ALL,AFM+NON,0,0,0,ALL)                          
         DC    AL2(61),AL1(UBSR,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(61),AL1(URRR,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(62),AL1(UBSS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(URRS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(USRS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(USSS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(USFS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(UDWN,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(UFGS,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(62),AL1(UPUB,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(52),AL1(UPUB,ALL,ALL-AFM,0,0,0,RADIO)                        
         DC    AL2(61),AL1(UCNL,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(62),AL1(UCNL,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(64),AL1(UCNL,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(61),AL1(USCN,ALL,AFT+NON,0,0,0,RADIO)      SP CNCL           
         DC    AL2(62),AL1(USCN,ALL,ALL-AFM,0,0,0,TV)         SP CNCL           
         DC    AL2(64),AL1(USCN,ALL,SAG+NON+AFT,0,0,0,CABLE)  SP CNCL           
         DC    AL2(64),AL1(UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(64),AL1(URRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(64),AL1(USRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(37),AL1(UPPF,ALL,ALL-AFM,0,0,0,TV)                           
*                                                                               
         DC    AL2(61),AL1(ULFT,ALL,AFT+NON,0,0,0,RADIO)  LIFT                  
         DC    AL2(62),AL1(ULFT,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(64),AL1(ULFT,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(61),AL1(USLF,ALL,AFT+NON,0,0,0,RADIO)  SPAN LFT              
         DC    AL2(62),AL1(USLF,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(64),AL1(USLF,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(125),AL1(UALF,ALL,AFT+NON,0,0,0,RADIO) AD LFT                
         DC    AL2(90),AL1(UALF,ALL,ALL-AFM,0,0,0,TV+CABLE)                     
*                                                                               
         DC    AL2(42),AL1(UDEM,ALL,ALL,0,0,0,TV)         DEMO                  
         DC    AL2(43),AL1(UDEM,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(42),AL1(USNA,ALL,ALL,0,0,0,TV)         SPANISH DEMO          
         DC    AL2(43),AL1(USNA,ALL,AFT+NON,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(63),AL1(UHLD,ALL,ALL-AFM,0,0,0,TV)     HLD FEE               
         DC    AL2(63),AL1(USHL,ALL,ALL-AFM,0,0,0,TV)     SP HLD FEE            
         DC    AL2(38),AL1(UREN,ALL,ALL-AFM,0,0,0,TV)     REINST                
         DC    AL2(38),AL1(USRE,ALL,ALL-AFM,0,0,0,TV)     REINST                
         DC    AL2(225),AL1(UADH,ALL,ALL-AFM,0,0,0,TV)    AD HLD FEE            
         DC    AL2(230),AL1(UARN,ALL,ALL-AFM,0,0,0,TV)    AD REINST             
*                                                                               
         DC    AL2(00),AL1(UCLA,UCLAREG,ALL,0,0,0,ALL)    CLA                   
         DC    AL2(08),AL1(UCLA,UCLAG13,ALL,0,0,0,ALL)    13USE GUAR            
         DC    AL2(58),AL1(ULNA,ALL,ALL,0,0,0,ALL)        LT NGHT ABC           
         DC    AL2(58),AL1(ULNN,ALL,ALL,0,0,0,ALL)                NBC           
         DC    AL2(58),AL1(ULNC,ALL,ALL,0,0,0,ALL)                CBS           
*                                                                               
         DC    AL2(01),AL1(ULOC,ULOCBNY,ALL,0,0,0,ALL)    CLASS B+NY            
         DC    AL2(02),AL1(ULOC,ULOCB,ALL,0,0,0,ALL)      CLASS B-NY            
         DC    AL2(03),AL1(ULOC,ULOCC,ALL,0,0,0,ALL)      CLASS C               
*                                                                               
         DC    AL2(10),AL1(UWSP,UWSP13W,ALL,0,0,0,TV)     13 WK TV              
         DC    AL2(15),AL1(UWSP,UWSP13W,ALL,0,0,0,RADIO)  13 WK RAD             
         DC    AL2(20),AL1(UWSP,UWSP8W,ALL,0,0,0,RADIO)   8WK                   
*                                                                               
         DC    AL2(04),AL1(UDLR,UDLRANY,ALL,0,0,0,ALL)    DLR A+NY              
         DC    AL2(05),AL1(UDLR,UDLRA,ALL,0,0,0,ALL)      DLR A-NY              
         DC    AL2(06),AL1(UDLR,UDLRBNY,ALL,0,0,0,ALL)    DLR B+NY              
         DC    AL2(07),AL1(UDLR,UDLRB,ALL,0,0,0,ALL)      DLR B-NY              
         DC    AL2(25),AL1(UDLR,UDLRRAD,ALL,0,0,0,ALL)    DLR RAD               
*                                                                               
         DC    AL2(41),AL1(UCBL,ALL,ALL,0,0,0,ALL)        CABLE                 
         DC    AL2(41),AL1(USCB,ALL,ALL,0,0,0,ALL)        SPAN CBL              
*                                                                               
         DC    AL2(26),AL1(URNT,URNT1W,ALL,0,0,0,ALL)   RAD NWK 1WK             
         DC    AL2(27),AL1(URNT,URNT4W,ALL,0,0,0,ALL)           4WK             
         DC    AL2(28),AL1(URNT,URNT8W,ALL,0,0,0,ALL)           8WK             
         DC    AL2(29),AL1(URNT,URNT13W,ALL,0,0,0,ALL)          13WK            
         DC    AL2(30),AL1(URNT,URNTAB,ALL,0,0,0,ALL)           A-B             
         DC    AL2(31),AL1(URNT,URNT26U,ALL,0,0,0,ALL)          26USE           
         DC    AL2(32),AL1(URNT,URNT39U,ALL,0,0,0,ALL)          39USE           
*                                                                               
         DC    AL2(33),AL1(URRN,ALL,ALL,0,0,0,RADIO)      RAD REG NWK           
*                                                                               
         DC    AL2(39),AL1(URLO,ALL,ALL,0,0,0,ALL)        RAD LCL 13W           
*                                                                               
         DC    AL2(35),AL1(UMUS,UMUSDUB,ALL,0,0,0,ALL)    DUB                   
         DC    AL2(35),AL1(UMUS,UMUS13W,ALL,0,0,0,ALL)    REUSE                 
         DC    AL2(35),AL1(UMUS,UMUSNEW,ALL,0,0,0,ALL)    NEW                   
         DC    AL2(35),AL1(UMUS,UMUSDSH,ALL,0,0,0,ALL)    DUB TO SHRT           
         DC    AL2(35),AL1(UNBM,ALL,ALL,0,0,0,ALL)        NON-BRD MUS           
         DC    AL2(09),AL1(UMUS,UMUSDUB8,ALL,0,0,0,ALL)   DUB-8WK               
         DC    AL2(09),AL1(UMUS,UMUS8W,ALL,0,0,0,ALL)     REUSE-8WK             
         DC    AL2(09),AL1(UMUS,UMUSNEW8,ALL,0,0,0,ALL)   NEW 8WK               
         DC    AL2(09),AL1(UMUS,UMUSDSH8,ALL,0,0,0,ALL)   DUB 2 SHT 8W          
         DC    AL2(36),AL1(UFMU,ALL,ALL,0,0,0,ALL)        1ST                   
*                                                                               
         DC    AL2(45),AL1(UFGM,UFGMEU12,ALL,0,0,0,ALL)   AFM F-E 12M           
         DC    AL2(45),AL1(UFGM,UFGMNE12,ALL,0,0,0,ALL)   NOT EUR 12M           
         DC    AL2(47),AL1(UFGM,UFGMEU24,ALL,0,0,0,ALL)   EUR 24M               
         DC    AL2(47),AL1(UFGM,UFGMNE24,ALL,0,0,0,ALL)   NOT EUR 24M           
         DC    AL2(46),AL1(UFGM,UFGMWO12,ALL,0,0,0,ALL)   WLD 12M               
         DC    AL2(48),AL1(UFGM,UFGMWO24,ALL,0,0,0,ALL)   WLD 24M               
*                                                                               
         DC    AL2(50),AL1(UFGR,UFGRUK,ALL,0,0,0,TV)      FGN-UK                
         DC    AL2(51),AL1(UFGR,UFGREUR,ALL,0,0,0,TV)     EUR W/O UK            
         DC    AL2(62),AL1(UFGR,UFGRWOR,ALL,0,0,0,TV)     W W/O UK&EUR          
         DC    AL2(62),AL1(UFGR,UFGRAP,ALL,0,0,0,TV)      ASIAN PAC             
         DC    AL2(62),AL1(UFGR,UFGRJAP,ALL,0,0,0,TV)     JAPAN                 
         DC    AL2(237),AL1(UFGR,UFGRWIDE,ALL,0,0,0,TV)   WORLDWIDE             
         DC    AL2(62),AL1(UFGR,UFGRMAJ,ALL,0,0,0,TV)     NEW-W/MAJOR           
         DC    AL2(256),AL1(UFGR,UFGREXT,ALL,0,0,0,TV)    FGR EXT               
         DC    AL2(49),AL1(UFGR,UFGRRAD,ALL,0,0,0,RADIO)  RADIO                 
*                                                                               
         DC    AL2(62),AL1(UPBS,ALL,ALL-AFM,0,0,0,TV)     PUB SVC               
         DC    AL2(52),AL1(UPBS,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(40),AL1(USNT,ALL,ALL,0,0,0,TV)         SPAN NWK              
         DC    AL2(40),AL1(USNW,ALL,ALL,0,0,0,TV)        SPAN N/W CMB           
         DC    AL2(10),AL1(USWS,ALL,ALL,0,0,0,TV)         SPAN WSP              
*                                                                               
         DC    AL2(65),AL1(UADT,UADT3D,ALL,0,0,0,ALL)     ADSES-TV-3D           
         DC    AL2(70),AL1(UADT,UADT1W,ALL,0,0,0,ALL)     1WEEK                 
         DC    AL2(80),AL1(UADT,UADT4W,ALL,0,0,0,ALL)     4WEEK                 
         DC    AL2(80),AL1(UADT,UADT31D,ALL,0,0,0,ALL)    31D(KS ONLY)          
         DC    AL2(90),AL1(UADT,UADT13W,ALL,0,0,0,ALL)    13WEEK                
*                                                                               
         DC    AL2(65),AL1(UARS,UARS3D,ALL,0,0,0,ALL)     ADSES-TV-3DY          
         DC    AL2(70),AL1(UARS,UARS1W,ALL,0,0,0,ALL)     1WK                   
         DC    AL2(80),AL1(UARS,UARS4W,ALL,0,0,0,ALL)     4WK                   
         DC    AL2(80),AL1(UARS,UARS31D,ALL,0,0,0,ALL)    31D(KS ONLY)          
         DC    AL2(90),AL1(UARS,UARS13W,ALL,0,0,0,ALL)    13WK                  
*                                                                               
         DC    AL2(100),AL1(UADO,UADO3D,ALL,0,0,0,ALL)    ADSES-RAD-3D          
         DC    AL2(105),AL1(UADO,UADO1W,ALL,0,0,0,ALL)    1WK                   
         DC    AL2(115),AL1(UADO,UADO4W,ALL,0,0,0,ALL)    4WK                   
         DC    AL2(115),AL1(UADO,UADO31D,ALL,0,0,0,ALL)   31DY(KS ONLY)         
         DC    AL2(125),AL1(UADO,UADO13W,ALL,0,0,0,ALL)   13WK                  
*                                                                               
         DC    AL2(100),AL1(UARR,UARR3D,ALL,0,0,0,ALL)    ADSES-RAD-3D          
         DC    AL2(105),AL1(UARR,UARR1W,ALL,0,0,0,ALL)    1WK                   
         DC    AL2(115),AL1(UARR,UARR4W,ALL,0,0,0,ALL)    4WK                   
         DC    AL2(115),AL1(UARR,UARR31D,ALL,0,0,0,ALL)   31D(KS ONLY)          
         DC    AL2(125),AL1(UARR,UARR13W,ALL,0,0,0,ALL)   13WK                  
*                                                                               
         DC    AL2(205),AL1(UADD,ALL,ALL,0,0,0,TV)        AD DEMO               
         DC    AL2(215),AL1(UADD,ALL,AFT+NON,0,0,0,RADIO)                       
*                                                                               
         DC    AL2(135),AL1(UADW,UADW3D,ALL,0,0,0,TV)     AD WSP-TV-3D          
         DC    AL2(140),AL1(UADW,UADW1W,ALL,0,0,0,TV)     1WK                   
         DC    AL2(150),AL1(UADW,UADW4W,ALL,0,0,0,TV)     4WK                   
         DC    AL2(150),AL1(UADW,UADW31D,ALL,0,0,0,TV)    31D(KS ONLY)          
         DC    AL2(160),AL1(UADW,UADW13W,ALL,0,0,0,TV)    13WK                  
*                                                                               
         DC    AL2(170),AL1(UADW,UADW3D,ALL,0,0,0,RADIO)  ADWSP-RAD-3D          
         DC    AL2(175),AL1(UADW,UADW1W,ALL,0,0,0,RADIO)  1WK                   
         DC    AL2(185),AL1(UADW,UADW4W,ALL,0,0,0,RADIO)  4WK                   
         DC    AL2(185),AL1(UADW,UADW31D,ALL,0,0,0,RADIO) 31D(KS ONLY)          
         DC    AL2(195),AL1(UADW,UADW13W,ALL,0,0,0,RADIO) 13WK                  
*                                                                               
         DC    AL2(135),AL1(UADC,UADC3D,ALL,0,0,0,TV)    CMB S/W-TV-3D          
         DC    AL2(140),AL1(UADC,UADC1W,ALL,0,0,0,TV)    1WK                    
         DC    AL2(150),AL1(UADC,UADC4W,ALL,0,0,0,TV)    4WK                    
         DC    AL2(150),AL1(UADC,UADC31D,ALL,0,0,0,TV)   31D(KS ONLY)           
         DC    AL2(160),AL1(UADC,UADC13W,ALL,0,0,0,TV)   13W                    
*                                                                               
         DC    AL2(170),AL1(UADC,UADC3D,ALL,0,0,0,RADIO) CMB S/W-RD-3D          
         DC    AL2(175),AL1(UADC,UADC1W,ALL,0,0,0,RADIO) 1W                     
         DC    AL2(185),AL1(UADC,UADC4W,ALL,0,0,0,RADIO) 4W                     
         DC    AL2(185),AL1(UADC,UADC31D,ALL,0,0,0,RADIO) 31D(KS ONLY)          
         DC    AL2(195),AL1(UADC,UADC13W,ALL,0,0,0,RADIO) 13W                   
*                                                                               
*        DC    AL2(44),AL1(UIFB,ALL,ALL,0,0,0,TV)        INS FOR BOOKS          
*                                                                               
         DC    AL2(53),AL1(UTAG,UTAGREG,ALL,0,0,0,TV)      TAGS TV              
         DC    AL2(54),AL1(UTAG,UTAGSESS,ALL,0,0,0,TV)     TAGS TV              
         DC    AL2(55),AL1(UTAG,UTAGREG,ALL,0,0,0,RADIO)   TAGS RAD             
         DC    AL2(56),AL1(UTAG,UTAGSESS,ALL,0,0,0,RADIO)  TAGS RAD             
*                                                                               
         DC    AL2(62),AL1(UINR,UINR30D,ALL,0,0,0,TV)    TH/IND-30DY            
         DC    AL2(236),AL1(UINR,UINRUNL,ALL,0,0,0,TV)   UNLIM                  
         DC    AL2(61),AL1(UINR,UINR30D,ALL,0,0,0,RADIO) 30D-RADIO              
         DC    AL2(235),AL1(UINR,UINRUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
         DC    AL2(62),AL1(USIN,USIN30D,ALL,0,0,0,TV)   SP IND-30D              
         DC    AL2(236),AL1(USIN,USINUNL,ALL,0,0,0,TV)    UNLIM                 
         DC    AL2(61),AL1(USIN,USIN30D,ALL,0,0,0,RADIO)  30D-RAD               
         DC    AL2(235),AL1(USIN,USINUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
*                                                                               
         DC    AL2(238),AL1(ULCB,ULCB50,ALL,0,0,0,ALL)    LCL CBL               
         DC    AL2(239),AL1(ULCB,ULCB100,ALL,0,0,0,ALL)                         
         DC    AL2(240),AL1(ULCB,ULCB150,ALL,0,0,0,ALL)                         
         DC    AL2(241),AL1(ULCB,ULCB200,ALL,0,0,0,ALL)                         
         DC    AL2(242),AL1(ULCB,ULCB250,ALL,0,0,0,ALL)                         
         DC    AL2(243),AL1(ULCB,ULCB500,ALL,0,0,0,ALL)                         
         DC    AL2(244),AL1(ULCB,ULCB750,ALL,0,0,0,ALL)                         
         DC    AL2(245),AL1(ULCB,ULCB1M,ALL,0,0,0,ALL)                          
         DC    AL2(246),AL1(ULCB,ULCBMAX,ALL,0,0,0,ALL)                         
*                                                                               
         DC    AL2(250),AL1(UPRM,ALL,ALL,0,0,0,TV)                              
         DC    AL2(251),AL1(UPRR,ALL,SAG,0,0,0,TV)                              
         DC    AL2(251),AL1(UPRR,ALL,AFT,0,0,0,TV)                              
*                                                                               
*        DC    AL2(62),AL1(UVAR,ALL,ALL-AFM,0,0,0,TV)                           
*        DC    AL2(61),AL1(UVAR,ALL,AFT+NON,0,0,0,RADIO)                        
*        DC    AL2(252),AL1(UVNW,UVNWSESS,ALL-AFM,0,0,0,TV)                     
*        DC    AL2(253),AL1(UVNW,UVNWREG,ALL-AFM,0,0,0,TV)                      
*        DC    AL2(254),AL1(UVNW,UVNWSESS,AFT+NON,0,0,0,RADIO)                  
*        DC    AL2(57),AL1(UVNW,UVNWREG,AFT+NON,0,0,0,RADIO)                    
*                                                                               
******  NO USE NUMBERS ARE AVAILABLE.  IF NEEDED, THE FOLLOWING                 
******  USE NUMBERS ARE AVAILABLE FROM ADDENDUMS, BUT MAKE SURE NO MORE         
******  THAN 5 ADDENDUM STATES ARE VALID FOR TYPES 3 DAYS, 1 WEEK, AND          
******  4 WEEKS/31 DAYS: 75-78, 85-88, 110-113, 120-123, 145-148,               
******  155-158, 180-183, 190-193, 210-214, AND 220-224.                        
*                                                                               
******  USE NUMBERS 34 USED FOR RRN WITH MAJORS                                 
******  USE NUMBERS 247-249 USED FOR TAGS FOR TX AND GA ADDENDUM                
******  USE NUMBERS 79,89,114,124,149,159,184&194 USED FOR 98 AFM CTRCT         
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
         DC    AL4(14125)          TV SESSION TAG FEE - ON CAMERA               
         DC    AL4(10705)          TV SESSION TAG FEE - OFF                     
         DC    AL4(8275)           RADIO SESSION TAG FEE                        
         DC    AL4(12300)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(9300)           GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(7200)           GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(8200)           KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(5700)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(3700)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(11560)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(8750)           TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(7400)           TX ADDENDUM RADIO SESSION TAG FEE            
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
         DC    AL1(4,CTG6M)                                                     
         DC    AL1(4,CTGD6)                                                     
         DC    AL1(4,CTGS6)                                                     
*                                                                               
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTG9M)                                                     
         DC    AL1(5,CTGD9)                                                     
         DC    AL1(5,CTGS9)                                                     
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
         DC    AL1(13,CTPI)                                                     
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
         DC    AL1(6,CTGS3)                                                     
         DC    AL1(6,CTGSS)                                                     
         DC    AL1(6,CTGSM)                                                     
*                                                                               
         DC    AL1(7,CTG6)                                                      
         DC    AL1(7,CTG6M)                                                     
         DC    AL1(7,CTGD6)                                                     
         DC    AL1(7,CTGS6)                                                     
*                                                                               
         DC    AL1(8,CTG9)                                                      
         DC    AL1(8,CTG9M)                                                     
         DC    AL1(8,CTGD9)                                                     
         DC    AL1(8,CTGS9)                                                     
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
**PAN#1  DC    CL21'027TAGEN6E   10/05/11'                                      
         END                                                                    
