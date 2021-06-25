*          DATA SET TAGEN61    AT LEVEL 019 AS OF 10/05/11                      
*PHASE T70261C                                                                  
         TITLE 'T70261 - TABLES FOR 2000 CONTRACT'                              
TAPREV   CSECT                                                                  
         DC    AL4(USETBLS-TAPREV)                                              
         DC    AL4(USELUT-TAPREV)                                               
         DC    AL4(MAJLUT-TAPREV)                                               
         DC    AL4(AFMCOLS-TAPREV)                                              
         DC    AL4(RADCOLS-TAPREV)                                              
         DC    AL4(OFFCOLS-TAPREV)                                              
         DC    AL4(ONCOLS-TAPREV)                                               
         DC    AL4(MSWEET-TAPREV)                                               
         DC    AL4(TAGFEE-TAPREV)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
*                                                                               
USETBLS  DS    0F                                                               
CLATBL   DC    AL2(0,44,1,1,0,0)    CLASS A USE 1                               
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(50000)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(37595)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(36605)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32405)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(26800)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(21205)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(18400)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(15005)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(12270)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(11370)                                                       
         DC    AL4(9735)                                                        
         DC    AL4(7965)                                                        
         DC    AL4(6170)                                                        
         DC    AL4(5365)                                                        
         DC    AL4(4400)                                                        
*                                                                               
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(9735)                                                        
         DC    AL4(7635)                                                        
         DC    AL4(8900)                                                        
         DC    AL4(8065)                                                        
         DC    AL4(6595)                                                        
         DC    AL4(5765)                                                        
         DC    AL4(4935)                                                        
         DC    AL4(4030)                                                        
*                                                                               
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(9735)                                                        
         DC    AL4(7635)                                                        
         DC    AL4(8400)                                                        
         DC    AL4(7565)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(5265)                                                        
         DC    AL4(4595)                                                        
         DC    AL4(3765)                                                        
*                                                                               
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(4665)                                                        
         DC    AL4(3465)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2100)                                                        
         DC    AL4(1970)                                                        
         DC    AL4(1635)                                                        
*                                                                               
PAXTAB   DC    AL2(75,44,1,255,0,0)   PAX USE                                   
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(2000)           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(1500)           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(1245)           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(1060)           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(860)            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(905)            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(845)            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(705)            'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
CLBTAB   DC    AL2(1,44,0,0,0,0)   CLASS B WITH NY                              
         DC    AL4(101220)                                                      
         DC    AL4(72390)                                                       
         DC    AL4(64470)                                                       
         DC    AL4(57010)                                                       
         DC    AL4(46605)                                                       
         DC    AL4(23755)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(16185)                                                       
*                                                                               
CBXTAB   DC    AL2(2,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(82560)                                                       
         DC    AL4(57340)                                                       
         DC    AL4(64470)                                                       
         DC    AL4(57010)                                                       
         DC    AL4(46605)                                                       
         DC    AL4(23755)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(16185)                                                       
*                                                                               
CLCTAB   DC    AL2(3,44,0,0,0,0)   CLASS C                                      
         DC    AL4(49200)                                                       
         DC    AL4(32800)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(37895)                                                       
         DC    AL4(30985)                                                       
         DC    AL4(18895)                                                       
         DC    AL4(15725)                                                       
         DC    AL4(12900)                                                       
*                                                                               
NWKTBL   DC    AL2(58,44,1,1,0,0)  NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(27575)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(20735)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(20190)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(17875)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(14840)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(11695)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(10145)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(8275)           'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
*                                                                               
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
*                                                                               
DAXTAB   DC    AL2(5,44,0,0,0,0)   DEALER A W/O NY                              
         DC    AL4(177555)                                                      
         DC    AL4(128240)                                                      
         DC    AL4(150750)                                                      
         DC    AL4(132885)                                                      
         DC    AL4(103280)                                                      
         DC    AL4(61625)                                                       
         DC    AL4(53970)                                                       
         DC    AL4(38535)                                                       
*                                                                               
DBNTAB   DC    AL2(6,44,0,0,0,0)   CLASS B INCL NY                              
         DC    AL4(308695)                                                      
         DC    AL4(210055)                                                      
         DC    AL4(229200)                                                      
         DC    AL4(202045)                                                      
         DC    AL4(157245)                                                      
         DC    AL4(93885)                                                       
         DC    AL4(82170)                                                       
         DC    AL4(58615)                                                       
*                                                                               
DBXTAB   DC    AL2(7,44,0,0,0,0)   CLASS B W/O NY                               
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
*                                                                               
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(146680)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(113565)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(121750)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(108750)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(89325)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(74075)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(64470)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(52750)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
*                                                                               
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(9200)                                                        
         DC    AL4(6988)                                                        
         DC    AL4(6725)                                                        
         DC    AL4(5886)                                                        
         DC    AL4(4807)                                                        
         DC    AL4(4443)                                                        
         DC    AL4(4006)                                                        
         DC    AL4(3302)                                                        
*                                                                               
         DC    AL2(8,44,19,255,0,0)  19+                                        
         DC    AL4(4868)                                                        
         DC    AL4(3818)                                                        
         DC    AL4(4200)                                                        
         DC    AL4(3783)                                                        
         DC    AL4(3100)                                                        
         DC    AL4(2633)                                                        
         DC    AL4(2298)                                                        
         DC    AL4(1883)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(50000)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(37595)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(36605)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32405)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(26800)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(21205)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(18400)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(15005)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1359)                                                        
         DC    AL4(1172)                                                        
         DC    AL4(958)                                                         
         DC    AL4(482)                                                         
         DC    AL4(380)                                                         
         DC    AL4(316)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
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
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(100130)                                                      
         DC    AL4(70740)                                                       
         DC    AL4(64125)                                                       
         DC    AL4(56955)                                                       
         DC    AL4(46670)                                                       
         DC    AL4(25725)                                                       
         DC    AL4(21315)                                                       
         DC    AL4(17450)                                                       
*                                                                               
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
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
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(87275)                                                       
         DC    AL4(61550)                                                       
         DC    AL4(64125)                                                       
         DC    AL4(56955)                                                       
         DC    AL4(46670)                                                       
         DC    AL4(25725)                                                       
         DC    AL4(21315)                                                       
         DC    AL4(17450)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(701)                                                         
         DC    AL4(594)                                                         
         DC    AL4(492)                                                         
         DC    AL4(203)                                                         
         DC    AL4(139)                                                         
         DC    AL4(128)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(508)                                                         
         DC    AL4(396)                                                         
         DC    AL4(332)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
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
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(137795)                                                      
         DC    AL4(92780)                                                       
         DC    AL4(98660)                                                       
         DC    AL4(81575)                                                       
         DC    AL4(66695)                                                       
         DC    AL4(33995)                                                       
         DC    AL4(27380)                                                       
         DC    AL4(22420)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(647)                                                         
         DC    AL4(508)                                                         
         DC    AL4(251)                                                         
         DC    AL4(203)                                                         
         DC    AL4(177)                                                         
         DC    AL4(123)                                                         
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(166210)                                                      
         DC    AL4(118050)                                                      
         DC    AL4(124470)                                                      
         DC    AL4(106525)                                                      
         DC    AL4(87065)                                                       
         DC    AL4(40985)                                                       
         DC    AL4(33050)                                                       
         DC    AL4(27010)                                                       
*                                                                               
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
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(22000)          ANN ALONE                                    
         DC    AL4(22000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(16205)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(14340)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(12725)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(7395)           SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(168)                                                         
         DC    AL4(144)                                                         
         DC    AL4(127)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(144)                                                         
         DC    AL4(110)                                                         
         DC    AL4(110)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(81)                                                          
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(32935)                                                       
         DC    AL4(32935)                                                       
         DC    AL4(17910)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(14115)                                                       
         DC    AL4(7395)                                                        
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(144)                                                         
         DC    AL4(121)                                                         
         DC    AL4(116)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(81)                                                          
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(29870)                                                       
         DC    AL4(29870)                                                       
         DC    AL4(17910)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(14115)                                                       
         DC    AL4(7395)                                                        
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(144)                                                         
         DC    AL4(121)                                                         
         DC    AL4(116)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(81)                                                          
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(40170)                                                       
         DC    AL4(40170)                                                       
         DC    AL4(21390)                                                       
         DC    AL4(16410)                                                       
         DC    AL4(14605)                                                       
         DC    AL4(7395)                                                        
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(121)                                                         
         DC    AL4(121)                                                         
         DC    AL4(116)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(81)                                                          
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(50755)                                                       
         DC    AL4(50755)                                                       
         DC    AL4(23830)                                                       
         DC    AL4(18440)                                                       
         DC    AL4(16410)                                                       
         DC    AL4(7395)                                                        
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(121)                                                         
         DC    AL4(121)                                                         
         DC    AL4(116)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(243)                                                         
         DC    AL4(243)                                                         
         DC    AL4(81)                                                          
         DC    AL4(70)                                                          
         DC    AL4(70)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(22000) ANN ALONE                                    
         DC    AL1(100),AL3(22000) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(16205) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(14340) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(12725) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL1(80),AL3(323)                                                 
         DC    AL1(80),AL3(323)                                                 
         DC    AL1(95),AL3(168)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(127)                                                 
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(110)                                                 
         DC    AL1(95),AL3(110)                                                 
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(81)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL1(80),AL3(32935)                                               
         DC    AL1(80),AL3(32935)                                               
         DC    AL1(95),AL3(17910)                                               
         DC    AL1(95),AL3(15900)                                               
         DC    AL1(95),AL3(14115)                                               
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(121)                                                 
         DC    AL1(95),AL3(116)                                                 
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(81)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(29870)                                               
         DC    AL1(80),AL3(29870)                                               
         DC    AL1(95),AL3(17910)                                               
         DC    AL1(95),AL3(15900)                                               
         DC    AL1(95),AL3(14115)                                               
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(120)                                                 
         DC    AL1(95),AL3(116)                                                 
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(81)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL1(80),AL3(40170)                                               
         DC    AL1(80),AL3(40170)                                               
         DC    AL1(95),AL3(21390)                                               
         DC    AL1(95),AL3(16410)                                               
         DC    AL1(95),AL3(14605)                                               
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(121)                                                 
         DC    AL1(95),AL3(121)                                                 
         DC    AL1(95),AL3(116)                                                 
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(81)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL1(80),AL3(50755)                                               
         DC    AL1(80),AL3(50755)                                               
         DC    AL1(95),AL3(23830)                                               
         DC    AL1(95),AL3(18440)                                               
         DC    AL1(95),AL3(16410)                                               
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(121)                                                 
         DC    AL1(95),AL3(121)                                                 
         DC    AL1(95),AL3(116)                                                 
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(80),AL3(243)                                                 
         DC    AL1(95),AL3(81)                                                  
         DC    AL1(95),AL3(70)                                                  
         DC    AL1(95),AL3(70)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
*                                                                               
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(63650)          AR,AS,P,ANN                                  
         DC    AL4(50490)          S,1-4MS,1-4SS                                
         DC    AL4(32920)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(26340)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(16460)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(16650)          SE                                           
*                                                                               
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(39825)          ANN ALONE                                    
         DC    AL4(39825)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(29825)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(29825)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(29825)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(8890)           SE                                           
*                                                                               
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(64615)                                                       
         DC    AL4(64615)                                                       
         DC    AL4(49690)                                                       
         DC    AL4(44435)                                                       
         DC    AL4(40595)                                                       
         DC    AL4(8890)                                                        
*                                                                               
N08TAB   DC    AL2(28,36,0,0,0,0)  NETWORK 8 WEEK                               
         DC    AL4(102930)                                                      
         DC    AL4(102930)                                                      
         DC    AL4(79195)                                                       
         DC    AL4(70775)                                                       
         DC    AL4(63400)                                                       
         DC    AL4(8890)                                                        
*                                                                               
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(127720)                                                      
         DC    AL4(127720)                                                      
         DC    AL4(98240)                                                       
         DC    AL4(87840)                                                       
         DC    AL4(80470)                                                       
         DC    AL4(8890)                                                        
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
*                                                                               
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(133745)                                                      
         DC    AL4(133745)                                                      
         DC    AL4(102850)                                                      
         DC    AL4(91975)                                                       
         DC    AL4(84265)                                                       
         DC    AL4(8890)                                                        
*                                                                               
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(63870)                                                       
         DC    AL4(63870)                                                       
         DC    AL4(49110)                                                       
         DC    AL4(43915)                                                       
         DC    AL4(40125)                                                       
         DC    AL4(8890)                                                        
*                                                                               
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(96180)                                                       
         DC    AL4(96180)                                                       
         DC    AL4(67345)                                                       
         DC    AL4(60115)                                                       
         DC    AL4(54615)                                                       
         DC    AL4(8890)                                                        
*                                                                               
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(77075)                                                       
         DC    AL4(77075)                                                       
         DC    AL4(36125)                                                       
         DC    AL4(36125)                                                       
         DC    AL4(36125)                                                       
         DC    AL4(8890)                                                        
*                                                                               
         DC    AL2(34,36,0,0,0,0)  REGIONAL - WITH ANY MAJORS                   
         DC    AL4(77075)                                                       
         DC    AL4(77075)                                                       
         DC    AL4(77075)                                                       
         DC    AL4(69370)                                                       
         DC    AL4(62385)                                                       
         DC    AL4(8890)                                                        
         EJECT                                                                  
*              MUSIC SESSION AND REUSE TABLES                                   
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    AL2(194,24,0,0,0,0)  98 SESSION                                  
         DC    AL4(10000)          CAST=1                                       
         DC    AL4(10000)               2-4                                     
         DC    AL4(10000)               5+                                      
*                                                                               
         DC    AL2(366,24,0,0,0,0)  01 SESSION                                  
         DC    AL4(10600)          CAST=1                                       
         DC    AL4(10600)               2-4                                     
         DC    AL4(10600)               5+                                      
*                                                                               
IMSTAB   DC    AL2(59,20,0,0,0,0)  SESSION   OLD IMS RATES                      
         DC    AL4(16548)          ON CAM                                       
         DC    AL4(16363)          OFF CAM                                      
*                                                                               
         DC    AL2(257,20,0,0,0,0)  SESSION  >= 2/13/00 IMS RATES               
         DC    AL4(17044)           ON CAM                                      
         DC    AL4(16854)           OFF CAM                                     
*                                                                               
         DC    AL2(258,20,0,0,0,0)  SESSION  >= 1/18/01 IMS RATES               
         DC    AL4(17555)           ON CAM                                      
         DC    AL4(17360)           OFF CAM                                     
*                                                                               
         DC    AL2(370,20,0,0,0,0)  SESSION  >= 2/16/02 IMS RATES               
         DC    AL4(18082)           ON CAM                                      
         DC    AL4(17881)           OFF CAM                                     
*                                                                               
         DC    AL2(371,20,0,0,0,0)  SESSION  >= 2/16/03 IMS RATES               
         DC    AL4(18624)           ON CAM                                      
         DC    AL4(18417)           OFF CAM                                     
*                                                                               
         DC    AL2(372,20,0,0,0,0)  SESSION  >= 2/16/04 IMS RATES               
         DC    AL4(19183)           ON CAM                                      
         DC    AL4(18970)           OFF CAM                                     
*                                                                               
MS8TAB   DC    AL2(09,24,0,0,0,0)  8-WEEK REUSE                                 
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
*                                                                               
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
*                                                                               
FGMTAB   DC    AL2(45,24,0,0,0,0)  EUROPE OR OUTSIDE EUROPE-12M                 
         DC    AL4(5875)           CAST=1                                       
         DC    AL4(5875)                2-4                                     
         DC    AL4(5875)                5+                                      
*                                                                               
         DC    AL2(124,24,0,0,0,0)  98 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(6250)           CAST=1                                       
         DC    AL4(6250)                2-4                                     
         DC    AL4(6250)                5+                                      
*                                                                               
         DC    AL2(362,24,0,0,0,0)  01 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(6625)           CAST=1                                       
         DC    AL4(6625)                2-4                                     
         DC    AL4(6625)                5+                                      
*                                                                               
         DC    AL2(46,24,0,0,0,0)  WORLD - 12M                                  
         DC    AL4(9400)           CAST=1                                       
         DC    AL4(9400)                2-4                                     
         DC    AL4(9400)                5+                                      
*                                                                               
         DC    AL2(159,24,0,0,0,0)  98 WORLD - 12M                              
         DC    AL4(10000)          CAST=1                                       
         DC    AL4(10000)               2-4                                     
         DC    AL4(10000)               5+                                      
*                                                                               
         DC    AL2(363,24,0,0,0,0)  01 WORLD - 12M                              
         DC    AL4(10600)          CAST=1                                       
         DC    AL4(10600)               2-4                                     
         DC    AL4(10600)               5+                                      
*                                                                               
         DC    AL2(47,24,0,0,0,0)  EUROPE OR OUTSIDE EUROPE-24M                 
         DC    AL4(8815)           CAST=1                                       
         DC    AL4(8815)                2-4                                     
         DC    AL4(8815)                5+                                      
*                                                                               
         DC    AL2(149,24,0,0,0,0)  98 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(9375)           CAST=1                                       
         DC    AL4(9375)                2-4                                     
         DC    AL4(9375)                5+                                      
*                                                                               
         DC    AL2(364,24,0,0,0,0)  01 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(9938)           CAST=1                                       
         DC    AL4(9938)                2-4                                     
         DC    AL4(9938)                5+                                      
*                                                                               
         DC    AL2(48,24,0,0,0,0)  WORLD - 24M                                  
         DC    AL4(14100)          CAST=1                                       
         DC    AL4(14100)              2-4                                      
         DC    AL4(14100)              5+                                       
*                                                                               
         DC    AL2(184,24,0,0,0,0)  98 WORLD - 24M                              
         DC    AL4(15000)          CAST=1                                       
         DC    AL4(15000)              2-4                                      
         DC    AL4(15000)              5+                                       
*                                                                               
         DC    AL2(365,24,0,0,0,0)  01 WORLD - 24M                              
         DC    AL4(15900)          CAST=1                                       
         DC    AL4(15900)              2-4                                      
         DC    AL4(15900)              5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
*                                                                               
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(22000)          ANN ALONE                                    
         DC    AL4(22000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(16205)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(14340)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(12725)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(16915)          SE                                           
         DC    AL4(7550)           C3,C6                                        
         DC    AL4(12080)          C9                                           
*                                                                               
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(50000)              PRINCIPAL ON  CAMERA                     
         DC    AL4(37595)                  "     OFF   "                        
         DC    AL4(36605)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(32405)                "    6-8    "                          
         DC    AL4(26800)                "     9+    "                          
         DC    AL4(21205)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(18400)                "    6-8    "                          
         DC    AL4(15005)                "     9+    "                          
         DC    AL4(27500)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(41970)              HAND MODEL UNLIMITED                     
         DC    AL4(15965)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(27915)              HAND MODEL 13 WEEKS                      
         DC    AL4(77010)    PIL       PILOT LOCATION RATE                      
         DC    AL4(59215)    PI        PILOT STUDIO RATE                        
         DC    AL4(32425)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(8015)     C3,C6     CONTRACTORS                              
         DC    AL4(15810)    C9             "                                   
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(50000)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(37595)                                               
         DC    AL1(50),AL3(36605)                                               
         DC    AL1(50),AL3(32405)                                               
         DC    AL1(50),AL3(26800)                                               
         DC    AL1(50),AL3(21205)                                               
         DC    AL1(50),AL3(18400)                                               
         DC    AL1(50),AL3(15005)                                               
         DC    AL1(50),AL3(27500)                                               
         DC    AL1(50),AL3(41970)                                               
         DC    AL1(50),AL3(15965)                                               
         DC    AL1(50),AL3(27915)                                               
         DC    AL1(50),AL3(77010)          PIL                                  
         DC    AL1(50),AL3(59215)          PI                                   
         DC    AL1(50),AL3(32425)          SE                                   
         DC    AL1(50),AL3(8015)           C3,C6                                
         DC    AL1(50),AL3(15810)          C9                                   
*                                                                               
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(41970)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(41970)                                                       
         DC    AL4(77010)          PIL                                          
         DC    AL4(59215)          PI                                           
         DC    AL4(32425)          SE                                           
         DC    AL4(8015)           C3,C6                                        
         DC    AL4(15810)          C9                                           
         EJECT                                                                  
         DC    AL2(66,24,1,255)        CSS (6/02-5/03)                          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(32500)              GROUP ONE                                
         DC    AL4(30000)              GROUP TWO                                
         DC    AL4(27500)              GROUP THREE                              
*                                                                               
         DC    AL2(66,24,1,255)        CSS (6/03-5/04)                          
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(33470)              GROUP ONE                                
         DC    AL4(30900)              GROUP TWO                                
         DC    AL4(28320)              GROUP THREE                              
*                                                                               
         DC    AL2(66,24,1,255)        CSS (6/04-5/05)                          
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(34480)              GROUP ONE                                
         DC    AL4(31830)              GROUP TWO                                
         DC    AL4(29170)              GROUP THREE                              
         EJECT                                                                  
         DC    AL2(63,32,0,0,0,0)  TV HOLDING RATES - HLD                       
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
*                                                                               
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(50000)                                               
         DC    AL1(50),AL3(37595)                                               
         DC    AL1(50),AL3(36605)                                               
         DC    AL1(50),AL3(32405)                                               
         DC    AL1(50),AL3(26800)                                               
         DC    AL1(50),AL3(21205)                                               
         DC    AL1(50),AL3(18400)                                               
         DC    AL1(50),AL3(15005)                                               
         DC    AL1(50),AL3(27500)                                               
         DC    AL1(50),AL3(41970)                                               
         DC    AL1(50),AL3(15965)                                               
         DC    AL1(50),AL3(27915)                                               
         DC    AL1(50),AL3(77010)  PIL                                          
         DC    AL1(50),AL3(59215)  PI                                           
         DC    AL1(50),AL3(32425)  SE                                           
         DC    AL1(50),AL3(8015)   C3,C6                                        
         DC    AL1(50),AL3(15810)  C9                                           
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(50000)                                              
         DC    AL1(200),AL3(37595)                                              
         DC    AL1(200),AL3(36605)                                              
         DC    AL1(200),AL3(32405)                                              
         DC    AL1(200),AL3(26800)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(15965)                                              
         DC    AL1(200),AL3(27915)                                              
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV                              
         DC    AL4(150000)             PRINCIPAL ON  CAMERA                     
         DC    AL4(112785)                 "     OFF   "                        
         DC    AL4(109815)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(97215)                "    6-8    "                          
         DC    AL4(80400)                "     9+    "                          
         DC    AL4(63615)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(55200)                "    6-8    "                          
         DC    AL4(45015)                "     9+    "                          
         DC    AL4(82500)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(125910)             HAND MODEL UNLIMITED                     
         DC    AL4(47895)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(83745)              HAND MODEL 13 WEEKS                      
         DC    AL4(231030)   PIL       PILOT LOCATION RATE                      
         DC    AL4(177645)   PI        PILOT STUDIO RATE                        
         DC    AL4(88425)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(24045)    C3,C6     CONTRACTORS                              
         DC    AL4(47430)    C9             "                                   
*                                                                               
         DC    AL2(77,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(66000)          ANN ALONE                                    
         DC    AL4(66000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(48615)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(43020)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(38175)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(50745)          SE                                           
         DC    AL4(22650)          C3,C6                                        
         DC    AL4(36240)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    INTERNET TV                              
         DC    AL4(150000)             PRINCIPAL ON  CAMERA                     
         DC    AL4(112785)                 "     OFF   "                        
         DC    AL4(109815)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(97215)                "    6-8    "                          
         DC    AL4(80400)                "     9+    "                          
         DC    AL4(63615)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(55200)                "    6-8    "                          
         DC    AL4(45015)                "     9+    "                          
         DC    AL4(82500)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(125910)             HAND MODEL UNLIMITED                     
         DC    AL4(47895)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(83745)              HAND MODEL 13 WEEKS                      
         DC    AL4(231030)   PIL       PILOT LOCATION RATE                      
         DC    AL4(177645)   PI        PILOT STUDIO RATE                        
         DC    AL4(88425)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(24045)    C3,C6     CONTRACTORS                              
         DC    AL4(47430)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(66000)          ANN ALONE                                    
         DC    AL4(66000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(48615)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(43020)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(38175)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(50745)          SE                                           
         DC    AL4(22650)          C3,C6                                        
         DC    AL4(36240)          C9                                           
         EJECT                                                                  
*              CABLE RATES (YEAR 1)                                             
*                                                                               
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(1,0,0,0)        YEAR 1                                       
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
*                                                                               
         DC    AL2(41,12,2,95)     MINIMUM COVERS UPTO 95                       
         DC    AL1(1,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,96,96)                                                 
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(210)                                                         
         DC    AL4(0)                                                           
         DC    AL4(129)                                                         
         DC    AL4(89)                                                          
         DC    AL4(64)                                                          
         DC    AL4(75)                                                          
         DC    AL4(38)                                                          
         DC    AL4(15)                                                          
*                                                                               
         DC    AL2(41,44,97,100)                                                
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(485)                                                         
         DC    AL4(0)                                                           
         DC    AL4(354)                                                         
         DC    AL4(314)                                                         
         DC    AL4(259)                                                         
         DC    AL4(205)                                                         
         DC    AL4(178)                                                         
         DC    AL4(145)                                                         
*                                                                               
         DC    AL2(41,44,101,106)                                               
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(411)                                                         
         DC    AL4(0)                                                           
         DC    AL4(302)                                                         
         DC    AL4(266)                                                         
         DC    AL4(221)                                                         
         DC    AL4(175)                                                         
         DC    AL4(151)                                                         
         DC    AL4(123)                                                         
*                                                                               
         DC    AL2(41,44,107,107)                                               
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(411)                                                         
         DC    AL4(22)                                                          
         DC    AL4(302)                                                         
         DC    AL4(266)                                                         
         DC    AL4(221)                                                         
         DC    AL4(175)                                                         
         DC    AL4(151)                                                         
         DC    AL4(123)                                                         
*                                                                               
         DC    AL2(41,44,108,150)                                               
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(411)                                                         
         DC    AL4(281)                                                         
         DC    AL4(302)                                                         
         DC    AL4(266)                                                         
         DC    AL4(221)                                                         
         DC    AL4(175)                                                         
         DC    AL4(151)                                                         
         DC    AL4(123)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(338)                                                         
         DC    AL4(232)                                                         
         DC    AL4(248)                                                         
         DC    AL4(220)                                                         
         DC    AL4(182)                                                         
         DC    AL4(144)                                                         
         DC    AL4(125)                                                         
         DC    AL4(101)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(43)                                                          
         DC    AL4(29)                                                          
         DC    AL4(32)                                                          
         DC    AL4(28)                                                          
         DC    AL4(23)                                                          
         DC    AL4(18)                                                          
         DC    AL4(16)                                                          
         DC    AL4(12)                                                          
*                                                                               
         DC    AL2(41,44,1001,2000)                                             
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(15)                                                          
         DC    AL4(10)                                                          
         DC    AL4(11)                                                          
         DC    AL4(10)                                                          
         DC    AL4(8)                                                           
         DC    AL4(6)                                                           
         DC    AL4(6)                                                           
         DC    AL4(5)                                                           
         EJECT                                                                  
*              CABLE RATES (YEAR 2)                                             
*                                                                               
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
*                                                                               
         DC    AL2(41,12,2,76)     MINIMUM COVERS UPTO 76                       
         DC    AL1(2,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,77,77)                                                 
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(392)                                                         
         DC    AL4(0)                                                           
         DC    AL4(290)                                                         
         DC    AL4(217)                                                         
         DC    AL4(186)                                                         
         DC    AL4(149)                                                         
         DC    AL4(113)                                                         
         DC    AL4(128)                                                         
*                                                                               
         DC    AL2(41,44,78,93)                                                 
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(596)                                                         
         DC    AL4(0)                                                           
         DC    AL4(435)                                                         
         DC    AL4(386)                                                         
         DC    AL4(318)                                                         
         DC    AL4(252)                                                         
         DC    AL4(219)                                                         
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(41,44,94,94)                                                 
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(596)                                                         
         DC    AL4(311)                                                         
         DC    AL4(435)                                                         
         DC    AL4(386)                                                         
         DC    AL4(318)                                                         
         DC    AL4(252)                                                         
         DC    AL4(219)                                                         
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(41,44,95,100)                                                
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(596)                                                         
         DC    AL4(374)                                                         
         DC    AL4(435)                                                         
         DC    AL4(386)                                                         
         DC    AL4(318)                                                         
         DC    AL4(252)                                                         
         DC    AL4(219)                                                         
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(506)                                                         
         DC    AL4(316)                                                         
         DC    AL4(371)                                                         
         DC    AL4(327)                                                         
         DC    AL4(272)                                                         
         DC    AL4(215)                                                         
         DC    AL4(186)                                                         
         DC    AL4(152)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(416)                                                         
         DC    AL4(261)                                                         
         DC    AL4(305)                                                         
         DC    AL4(270)                                                         
         DC    AL4(224)                                                         
         DC    AL4(177)                                                         
         DC    AL4(153)                                                         
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(53)                                                          
         DC    AL4(33)                                                          
         DC    AL4(39)                                                          
         DC    AL4(35)                                                          
         DC    AL4(29)                                                          
         DC    AL4(23)                                                          
         DC    AL4(20)                                                          
         DC    AL4(15)                                                          
*                                                                               
         DC    AL2(41,44,1001,2000)                                             
         DC    AL1(2,0,0,0)                                                     
         DC    AL4(18)                                                          
         DC    AL4(10)                                                          
         DC    AL4(13)                                                          
         DC    AL4(12)                                                          
         DC    AL4(10)                                                          
         DC    AL4(8)                                                           
         DC    AL4(7)                                                           
         DC    AL4(5)                                                           
         EJECT                                                                  
*              CABLE RATES (YEAR 3)                                             
*                                                                               
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
*                                                                               
         DC    AL2(41,12,2,61)     MINIMUM COVERS UPTO 61                       
         DC    AL1(3,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,62,62)                                                 
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(698)                                                         
         DC    AL4(0)                                                           
         DC    AL4(529)                                                         
         DC    AL4(409)                                                         
         DC    AL4(368)                                                         
         DC    AL4(291)                                                         
         DC    AL4(216)                                                         
         DC    AL4(211)                                                         
*                                                                               
         DC    AL2(41,44,63,81)                                                 
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(729)                                                         
         DC    AL4(0)                                                           
         DC    AL4(532)                                                         
         DC    AL4(472)                                                         
         DC    AL4(389)                                                         
         DC    AL4(308)                                                         
         DC    AL4(268)                                                         
         DC    AL4(218)                                                         
*                                                                               
         DC    AL2(41,44,82,82)                                                 
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(729)                                                         
         DC    AL4(273)                                                         
         DC    AL4(532)                                                         
         DC    AL4(472)                                                         
         DC    AL4(389)                                                         
         DC    AL4(308)                                                         
         DC    AL4(268)                                                         
         DC    AL4(218)                                                         
*                                                                               
         DC    AL2(41,44,83,100)                                                
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(729)                                                         
         DC    AL4(424)                                                         
         DC    AL4(532)                                                         
         DC    AL4(472)                                                         
         DC    AL4(389)                                                         
         DC    AL4(308)                                                         
         DC    AL4(268)                                                         
         DC    AL4(218)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(619)                                                         
         DC    AL4(359)                                                         
         DC    AL4(453)                                                         
         DC    AL4(400)                                                         
         DC    AL4(332)                                                         
         DC    AL4(263)                                                         
         DC    AL4(228)                                                         
         DC    AL4(185)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(509)                                                         
         DC    AL4(296)                                                         
         DC    AL4(373)                                                         
         DC    AL4(330)                                                         
         DC    AL4(274)                                                         
         DC    AL4(217)                                                         
         DC    AL4(187)                                                         
         DC    AL4(152)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(64)                                                          
         DC    AL4(37)                                                          
         DC    AL4(48)                                                          
         DC    AL4(42)                                                          
         DC    AL4(35)                                                          
         DC    AL4(28)                                                          
         DC    AL4(24)                                                          
         DC    AL4(18)                                                          
*                                                                               
         DC    AL2(41,44,1001,2000)                                             
         DC    AL1(3,0,0,0)                                                     
         DC    AL4(60)                                                          
         DC    AL4(36)                                                          
         DC    AL4(44)                                                          
         DC    AL4(39)                                                          
         DC    AL4(32)                                                          
         DC    AL4(26)                                                          
         DC    AL4(22)                                                          
         DC    AL4(18)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
*                                                                               
         DC    AL2(42,84,1,255,0,0)  DEM (TV)                                   
         DC    AL4(37595)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(18800)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(27445)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(24300)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(20095)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(9500)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(9500)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(9500)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(15963)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(27915)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(8015)           C3,C6                                        
         DC    AL4(15810)          C9                                           
         DC    AL4(14530)          'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(43,48,1,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(15160)          ANN ALONE                                    
         DC    AL4(15160)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(10005)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(10005)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(10005)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(16915)          SE                                           
         DC    AL4(7550)           C3,C6                                        
         DC    AL4(12080)          C9                                           
         DC    AL4(15300)          SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(25580)                                                       
         DC    AL4(25580)                                                       
         DC    AL4(25580)                                                       
         DC    AL4(25580)                                                       
         DC    AL4(25580)                                                       
         DC    AL4(10070)                                                       
         EJECT                                                                  
*              FOREIGN REUSE                                                    
*                                                                               
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(150000)         (3 X 50000)                                  
         DC    AL4(112785)                                                      
         DC    AL4(109815)                                                      
         DC    AL4(97215)                                                       
         DC    AL4(80400)                                                       
         DC    AL4(63615)                                                       
         DC    AL4(55200)                                                       
         DC    AL4(45015)                                                       
         DC    AL4(82500)                                                       
         DC    AL4(125910)                                                      
         DC    AL4(47895)                                                       
         DC    AL4(83745)                                                       
         DC    AL4(231030)         PIL                                          
         DC    AL4(177645)         PI                                           
         DC    AL4(88425)          SE                                           
         DC    AL4(24045)          C3,C6                                        
         DC    AL4(47430)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(100000)                                                      
         DC    AL4(75190)                                                       
         DC    AL4(73210)                                                       
         DC    AL4(64810)                                                       
         DC    AL4(53600)                                                       
         DC    AL4(42410)                                                       
         DC    AL4(36800)                                                       
         DC    AL4(30010)                                                       
         DC    AL4(55002)                                                       
         DC    AL4(83936)                                                       
         DC    AL4(31926)                                                       
         DC    AL4(55830)                                                       
         DC    AL4(154020)          PIL                                         
         DC    AL4(118430)          PI                                          
         DC    AL4(32425)           SE                                          
         DC    AL4(16030)           C3,C6                                       
         DC    AL4(31620)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 8X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(400000)        (8 X 50000)                                   
         DC    AL4(300760)                                                      
         DC    AL4(292840)                                                      
         DC    AL4(259240)                                                      
         DC    AL4(214400)                                                      
         DC    AL4(169640)                                                      
         DC    AL4(147200)                                                      
         DC    AL4(120040)                                                      
         DC    AL4(220000)                                                      
         DC    AL4(335760)                                                      
         DC    AL4(127720)                                                      
         DC    AL4(223320)                                                      
         DC    AL4(616080)         PIL                                          
         DC    AL4(473720)         PI                                           
         DC    AL4(235800)         SE                                           
         DC    AL4(64120)          C3,C6                                        
         DC    AL4(126480)         C9                                           
*                                                                               
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(46695)          N/D                                          
         DC    AL4(46695)          P,ANN,S,D,ACR                                
         DC    AL4(27085)          3-5 GROUP                                    
         DC    AL4(18680)          6-8 GROUP                                    
         DC    AL4(14940)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(53250)          P,ANN,ACR                                    
         DC    AL4(55295)          S,D                                          
         DC    AL4(36050)          3-5 GROUP                                    
         DC    AL4(28840)          6-8 GROUP                                    
         DC    AL4(18035)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
*                                                                               
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK                                      
         DC    AL4(168000)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(126340)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(122985)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(108860)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(90040)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(71370)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(61825)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(50405)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(424)                                                         
         DC    AL4(308)                                                         
         DC    AL4(301)                                                         
         DC    AL4(278)                                                         
         DC    AL4(216)                                                         
         DC    AL4(178)                                                         
         DC    AL4(161)                                                         
         DC    AL4(115)                                                         
         EJECT                                                                  
*              ADDENDUM USES                                                    
*                                                                               
ADTTBL   DC    AL2(65,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA                
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(22900)          OFF                                          
         DC    AL4(16700)                                                       
         DC    AL4(14800)                                                       
         DC    AL4(12200)                                                       
         DC    AL4(9700)                                                        
         DC    AL4(8400)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(30500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(21800)          SOLO/DUO ON CAM                              
         DC    AL4(16400)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(70,88,0,0,0,0)  1 WEEK - GA                                  
         DC    AL4(32600)          ON CAMERA                                    
         DC    AL4(24500)          OFF                                          
         DC    AL4(17900)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(13100)                                                       
         DC    AL4(10400)                                                       
         DC    AL4(9000)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(32600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(23325)          SOLO/DUO ON CAM                              
         DC    AL4(17600)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(71,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS               
         DC    AL4(25600)          ON CAMERA                                    
         DC    AL4(19230)          OFF                                          
         DC    AL4(19750)                                                       
         DC    AL4(16930)                                                       
         DC    AL4(13480)                                                       
         DC    AL4(8570)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(5015)                                                        
         DC    AL4(16535)                                                       
         DC    AL4(22155)                                                       
         DC    AL4(9330)                                                        
         DC    AL4(14735)                                                       
         DC    AL4(24500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0,0,0)  TV SESSION - 2 WEEK - NW MULTI MKT           
         DC    AL4(31900)          ON CAMERA                                    
         DC    AL4(24000)          OFF                                          
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(31900)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0)      TV SESSION - 2 WEEK - NW SINGLE MKT          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(29200)          ON CAMERA                                    
         DC    AL4(20400)          OFF                                          
         DC    AL4(17700)                                                       
         DC    AL4(17700)                                                       
         DC    AL4(17700)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(29200)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(80,88,0,0,0,0)  4 WEEK - GA                                  
         DC    AL4(34800)          ON CAMERA                                    
         DC    AL4(26200)          OFF                                          
         DC    AL4(19000)                                                       
         DC    AL4(16900)                                                       
         DC    AL4(13900)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(30900)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(24900)          SOLO/DUO ON CAM                              
         DC    AL4(18700)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(81,80,0,0,0,0)  31 DAY - KS                                  
         DC    AL4(32815)          ON CAMERA                                    
         DC    AL4(24660)          OFF                                          
         DC    AL4(24455)                                                       
         DC    AL4(20795)                                                       
         DC    AL4(16510)                                                       
         DC    AL4(10765)                                                       
         DC    AL4(8465)                                                        
         DC    AL4(6375)                                                        
         DC    AL4(16535)                                                       
         DC    AL4(22155)                                                       
         DC    AL4(9330)                                                        
         DC    AL4(14735)                                                       
         DC    AL4(31400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(90,88,0,0,0,0)  13 WEEK - GA                                 
         DC    AL4(43500)          ON CAMERA                                    
         DC    AL4(32700)          OFF                                          
         DC    AL4(23800)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(12000)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(15000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(38600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)                                                       
         DC    AL4(7675)                                                        
         DC    AL4(15135)                                                       
         DC    AL4(31100)          SOLO/DUO ON CAM                              
         DC    AL4(23400)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(91,80,0,0,0,0)  13 WEEKS - KS                                
         DC    AL4(40025)          ON CAMERA                                    
         DC    AL4(29990)          OFF                                          
         DC    AL4(28945)                                                       
         DC    AL4(24660)                                                       
         DC    AL4(19750)                                                       
         DC    AL4(12645)                                                       
         DC    AL4(10240)                                                       
         DC    AL4(7630)                                                        
         DC    AL4(16535)                                                       
         DC    AL4(22155)                                                       
         DC    AL4(9330)                                                        
         DC    AL4(14735)                                                       
         DC    AL4(38300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(92,80,0,0,0,0)  13 WEEKS - TX                                
         DC    AL4(40000)          ON CAMERA                                    
         DC    AL4(30075)          OFF                                          
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
         DC    AL4(16965)                                                       
         DC    AL4(16965)                                                       
         DC    AL4(16965)                                                       
         DC    AL4(22000)                                                       
         DC    AL4(33575)                                                       
         DC    AL4(12770)                                                       
         DC    AL4(22330)                                                       
         DC    AL4(40000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0,0,0)  13 WEEKS - NW MULTI MARKET                   
         DC    AL4(37500)          ON CAMERA                                    
         DC    AL4(28200)          OFF                                          
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(16000)                                                       
         DC    AL4(16000)                                                       
         DC    AL4(37500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0)      13 WEEKS - NW SINGLE MARKET                  
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(34300)          ON CAMERA                                    
         DC    AL4(24100)          OFF                                          
         DC    AL4(20900)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(34300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(32425)          SE                                           
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
ADOTBL   DC    AL2(100,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA            
         DC    AL4(13400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(6900)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(9100)           SOLO/DUO                                     
*                                                                               
         DC    AL2(105,48,0,0,0,0)  1 WEEK - GA                                 
         DC    AL4(14300)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(7400)                                                        
         DC    AL4(6400)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(9800)                                                        
*                                                                               
         DC    AL2(106,44,0,0,0,0)  1 WEEK - KS                                 
         DC    AL4(10890)                                                       
         DC    AL4(10890)                                                       
         DC    AL4(6820)                                                        
         DC    AL4(5720)                                                        
         DC    AL4(5060)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(108,44,0,0,0,0)  2 WEEK - NW MULTLIPLE MARKETS               
         DC    AL4(14800)                                                       
         DC    AL4(14800)                                                       
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(108,44,0,0)      2 WEEK - NW SINGLE MARKET                   
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(10500)                                                       
         DC    AL4(10500)                                                       
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(115,48,0,0,0,0)  4 WEEK - GA                                 
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(7800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(10400)                                                       
*                                                                               
         DC    AL2(116,44,0,0,0,0)  31 DAY - KS                                 
         DC    AL4(13970)                                                       
         DC    AL4(13970)                                                       
         DC    AL4(8250)                                                        
         DC    AL4(7260)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(125,48,0,0,0,0)  13 WEEK - GA                                
         DC    AL4(19100)                                                       
         DC    AL4(19100)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(7500)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(13000)                                                       
*                                                                               
         DC    AL2(126,44,0,0,0,0)  13 WEEK - KS                                
         DC    AL4(17050)                                                       
         DC    AL4(17050)                                                       
         DC    AL4(9570)                                                        
         DC    AL4(8470)                                                        
         DC    AL4(7590)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(127,44,0,0,0,0)  13 WEEK - TX                                
         DC    AL4(17600)                                                       
         DC    AL4(17600)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0,0,0)  13 WEEK - NW  MULTIPLE MARKETS              
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(9600)                                                        
         DC    AL4(9600)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0)      13 WEEK - NW  SINGLE MARKET                 
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
ADHTAB   DC    AL2(225,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                 
         DC    AL4(43500)                                                       
         DC    AL4(32700)                                                       
         DC    AL4(23800)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(17400)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(31100)                                                       
         DC    AL4(23400)                                                       
*                                                                               
         DC    AL2(226,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                 
         DC    AL4(40025)           ON CAMERA                                   
         DC    AL4(29990)           OFF                                         
         DC    AL4(28945)                                                       
         DC    AL4(24660)                                                       
         DC    AL4(19750)                                                       
*                                                                               
         DC    AL2(227,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                 
         DC    AL4(40000)           ON CAMERA                                   
         DC    AL4(30075)           OFF                                         
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
*                                                                               
         DC    AL2(228,32,0,0,0,0)  ADDENDUM HOLDING RATES - NW                 
         DC    AL4(37500)           ON CAMERA   MULTIPLE MARKET                 
         DC    AL4(28200)           OFF                                         
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
*                                                                               
         DC    AL2(228,32,0,0)      ADDENDUM HOLDING RATES - NW                 
         DC    AL1(1,0,0,0)         SINGLE MARKET                               
         DC    AL4(34300)           ON CAMERA                                   
         DC    AL4(24100)           OFF                                         
         DC    AL4(20900)                                                       
         DC    AL4(20900)                                                       
         DC    AL4(20900)                                                       
*                                                                               
*                                   ADDENDUM REINSTSATEMENT-GA                  
ARNTAB   DC    AL2(230,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                 
         DC    AL1(200),AL3(43500)                                              
         DC    AL1(200),AL3(32700)                                              
         DC    AL1(200),AL3(23800)                                              
         DC    AL1(200),AL3(21100)                                              
         DC    AL1(200),AL3(17400)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(31100)                                              
         DC    AL1(200),AL3(23400)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(231,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(40025)             ON CAMERA                        
         DC    AL1(200),AL3(29990)             OFF                              
         DC    AL1(200),AL3(28945)                                              
         DC    AL1(200),AL3(24660)                                              
         DC    AL1(200),AL3(19750)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(232,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(40000)             ON CAMERA                        
         DC    AL1(200),AL3(30075)             OFF                              
         DC    AL1(200),AL3(29285)                                              
         DC    AL1(200),AL3(29285)                                              
         DC    AL1(200),AL3(29285)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(37500)             ON CAMERA  MULTIPLE MKTS         
         DC    AL1(200),AL3(28200)             OFF                              
         DC    AL1(200),AL3(27500)                                              
         DC    AL1(200),AL3(27500)                                              
         DC    AL1(200),AL3(27500)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0)       - 2X ADDENDUM HOLDING RATES                
         DC    AL1(1,0,0,0)          SINGLE MARKET                              
         DC    AL1(200),AL3(34300)             ON CAMERA                        
         DC    AL1(200),AL3(24100)             OFF                              
         DC    AL1(200),AL3(20900)                                              
         DC    AL1(200),AL3(20900)                                              
         DC    AL1(200),AL3(20900)                                              
*                                                                               
ADDTAB   DC    AL2(205,80,0,0,0,0)  ADDENDUM DEMO (TV) - GA                     
         DC    AL4(32700)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(16400)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(32700)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32700)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(32700)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(16400)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(16400)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(16400)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(206,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                     
         DC    AL4(8985)           'ON'                                         
         DC    AL4(7940)           'OFF'                                        
         DC    AL4(8985)                                                        
         DC    AL4(8985)                                                        
         DC    AL4(8985)                                                        
         DC    AL4(7940)                                                        
         DC    AL4(7940)                                                        
         DC    AL4(7940)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(207,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                     
         DC    AL4(30075)          'ON'                                         
         DC    AL4(15040)          'OFF'                                        
         DC    AL4(21955)                                                       
         DC    AL4(21955)                                                       
         DC    AL4(21955)                                                       
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(30075)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(11625)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(208,80,0,0,0,0)  ADDENDUM DEMO (TV) - NW                     
         DC    AL4(28200)          'ON'                                         
         DC    AL4(14100)          'OFF'                                        
         DC    AL4(20600)                                                       
         DC    AL4(20600)                                                       
         DC    AL4(20600)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(8100)           EXTRA                                        
         DC    4AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(215,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA              
         DC    AL4(13400)          ANN ALONE                                    
         DC    AL4(13400)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(13400)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(13400)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(13400)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(216,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS              
         DC    AL4(7260)           ANN ALONE                                    
         DC    AL4(7260)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(7260)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7260)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7260)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(217,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX              
         DC    AL4(12130)          ANN ALONE                                    
         DC    AL4(12130)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8005)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8005)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8005)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(12240)          SOLO/DUO                                     
*                                                                               
         DC    AL2(218,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - NW              
         DC    AL4(12000)          ANN ALONE                                    
         DC    AL4(12000)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8100)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8100)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8100)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
*                                                                               
ADWTAB   DC    AL2(135,52,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(22900)          OFF                                          
         DC    AL4(16700)                                                       
         DC    AL4(14800)                                                       
         DC    AL4(12200)                                                       
         DC    AL4(9700)                                                        
         DC    AL4(8400)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(21800)          SOLO/DUO ON CAMERA                           
         DC    AL4(16400)          OFF CAMERA                                   
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    AL2(135,52,61,255,0,0)  UNITS 61+                                
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
*                                                                               
         DC    AL2(140,52,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(32600)          ON CAMERA                                    
         DC    AL4(24500)          OFF                                          
         DC    AL4(17900)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(13100)                                                       
         DC    AL4(10400)                                                       
         DC    AL4(9000)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(23325)                                                       
         DC    AL4(17600)                                                       
*                                                                               
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
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    AL2(141,44,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(25600)          ON CAMERA                                    
         DC    AL4(19230)          OFF                                          
         DC    AL4(19750)                                                       
         DC    AL4(16930)                                                       
         DC    AL4(13480)                                                       
         DC    AL4(8570)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(5015)                                                        
*                                                                               
         DC    AL2(141,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(970)                                                         
         DC    AL4(970)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
*                                                                               
         DC    AL2(143,44,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(31900)          ON CAMERA                                    
         DC    AL4(24000)          OFF                                          
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
*                                                                               
         DC    AL2(143,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1193)                                                        
*                                                                               
         DC    AL2(150,52,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(34800)          ON CAMERA                                    
         DC    AL4(26200)          OFF                                          
         DC    AL4(19000)                                                       
         DC    AL4(16900)                                                       
         DC    AL4(13900)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(24900)          SOLO/DUO ON CAMERA                           
         DC    AL4(18700)          OFF CAMERA                                   
*                                                                               
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
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    AL2(151,44,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(32815)          ON CAMERA                                    
         DC    AL4(24660)          OFF                                          
         DC    AL4(24455)                                                       
         DC    AL4(20795)                                                       
         DC    AL4(16510)                                                       
         DC    AL4(10765)                                                       
         DC    AL4(8465)                                                        
         DC    AL4(6375)                                                        
*                                                                               
         DC    AL2(151,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(970)                                                         
         DC    AL4(970)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
*                                                                               
         DC    AL2(160,52,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(43500)          ON CAMERA                                    
         DC    AL4(32700)          OFF                                          
         DC    AL4(23800)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(12000)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(31100)          SOLO/DUO ON CAMERA                           
         DC    AL4(23400)          OFF                                          
*                                                                               
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
*                                                                               
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
*                                                                               
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
*                                                                               
         DC    AL2(161,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                      
         DC    AL4(40025)          ON CAMERA                                    
         DC    AL4(29990)          OFF                                          
         DC    AL4(28945)                                                       
         DC    AL4(24660)                                                       
         DC    AL4(19750)                                                       
         DC    AL4(12645)                                                       
         DC    AL4(10240)                                                       
         DC    AL4(7630)                                                        
*                                                                               
         DC    AL2(161,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(970)                                                         
         DC    AL4(970)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(265)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
*                                                                               
         DC    AL2(162,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                      
         DC    AL4(40000)           ON CAMERA                                   
         DC    AL4(30075)           OFF                                         
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
         DC    AL4(29285)                                                       
         DC    AL4(16965)                                                       
         DC    AL4(16965)                                                       
         DC    AL4(16965)                                                       
*                                                                               
         DC    AL2(162,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1395)                                                        
         DC    AL4(955)                                                         
         DC    AL4(1090)                                                        
         DC    AL4(1090)                                                        
         DC    AL4(1090)                                                        
         DC    AL4(385)                                                         
         DC    AL4(385)                                                         
         DC    AL4(385)                                                         
*                                                                               
         DC    AL2(162,44,26,255,0,0)  UNITS 26+                                
         DC    AL4(520)                                                         
         DC    AL4(405)                                                         
         DC    AL4(560)                                                         
         DC    AL4(560)                                                         
         DC    AL4(560)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
*                                                                               
         DC    AL2(163,44,1,1,0,0)  13 WEEKS - NW - UNIT 1                      
         DC    AL4(37500)          ON CAMERA                                    
         DC    AL4(28200)          OFF                                          
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
*                                                                               
         DC    AL2(163,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1744)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1193)                                                        
         DC    AL4(1193)                                                        
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
*                                                                               
         DC    AL2(170,40,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(13400)          ANN ALONE                                    
         DC    AL4(13400)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(6900)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6000)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5300)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(13400)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(9100)           SOLO/DUO                                     
*                                                                               
         DC    AL2(170,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(197)                                                         
         DC    AL4(197)                                                         
         DC    AL4(71)                                                          
         DC    AL4(62)                                                          
         DC    AL4(54)                                                          
         DC    AL4(0)                                                           
         DC    AL4(137)                                                         
*                                                                               
         DC    AL2(170,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(148)                                                         
         DC    AL4(148)                                                         
         DC    AL4(62)                                                          
         DC    AL4(47)                                                          
         DC    AL4(47)                                                          
         DC    AL4(0)                                                           
         DC    AL4(104)                                                         
*                                                                               
         DC    AL2(170,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(148)                                                         
         DC    AL4(148)                                                         
         DC    AL4(34)                                                          
         DC    AL4(29)                                                          
         DC    AL4(29)                                                          
         DC    AL4(0)                                                           
         DC    AL4(104)                                                         
*                                                                               
         DC    AL2(175,40,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(14300)          ANN ALONE                                    
         DC    AL4(14300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7400)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6400)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5600)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(14300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(9800)           SOLO/DUO                                     
*                                                                               
         DC    AL2(175,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(211)                                                         
         DC    AL4(211)                                                         
         DC    AL4(77)                                                          
         DC    AL4(66)                                                          
         DC    AL4(58)                                                          
         DC    AL4(0)                                                           
         DC    AL4(147)                                                         
*                                                                               
         DC    AL2(175,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(66)                                                          
         DC    AL4(50)                                                          
         DC    AL4(50)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
*                                                                               
         DC    AL2(175,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(37)                                                          
         DC    AL4(32)                                                          
         DC    AL4(32)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
*                                                                               
         DC    AL2(176,36,0,0,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(10890)                                                       
         DC    AL4(10890)                                                       
         DC    AL4(6820)                                                        
         DC    AL4(5720)                                                        
         DC    AL4(5060)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(176,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(365)                                                         
         DC    AL4(365)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(365)                                                         
*                                                                               
         DC    AL2(178,36,0,0,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(14800)                                                       
         DC    AL4(14800)                                                       
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    AL4(14800)          SE                                           
*                                                                               
         DC    AL2(178,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
*                                                                               
         DC    AL2(185,40,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(15300)          ANN ALONE                                    
         DC    AL4(15300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6800)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6000)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(10400)          SOLO/DUO                                     
*                                                                               
         DC    AL2(185,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(225)                                                         
         DC    AL4(225)                                                         
         DC    AL4(82)                                                          
         DC    AL4(70)                                                          
         DC    AL4(62)                                                          
         DC    AL4(0)                                                           
         DC    AL4(157)                                                         
*                                                                               
         DC    AL2(185,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(169)                                                         
         DC    AL4(169)                                                         
         DC    AL4(70)                                                          
         DC    AL4(54)                                                          
         DC    AL4(54)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
*                                                                               
         DC    AL2(185,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(169)                                                         
         DC    AL4(169)                                                         
         DC    AL4(39)                                                          
         DC    AL4(34)                                                          
         DC    AL4(34)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
*                                                                               
         DC    AL2(186,36,0,0,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(13970)                                                       
         DC    AL4(13970)                                                       
         DC    AL4(8250)                                                        
         DC    AL4(7260)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(186,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(365)                                                         
         DC    AL4(365)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(365)                                                         
*                                                                               
         DC    AL2(195,40,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(19100)          ANN ALONE                                    
         DC    AL4(19100)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(9800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8500)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7500)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(19100)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(13000)          SOLO/DUO                                     
*                                                                               
         DC    AL2(195,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(281)                                                         
         DC    AL4(281)                                                         
         DC    AL4(102)                                                         
         DC    AL4(88)                                                          
         DC    AL4(77)                                                          
         DC    AL4(0)                                                           
         DC    AL4(196)                                                         
*                                                                               
         DC    AL2(195,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(211)                                                         
         DC    AL4(211)                                                         
         DC    AL4(88)                                                          
         DC    AL4(67)                                                          
         DC    AL4(67)                                                          
         DC    AL4(0)                                                           
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(195,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(211)                                                         
         DC    AL4(211)                                                         
         DC    AL4(49)                                                          
         DC    AL4(42)                                                          
         DC    AL4(42)                                                          
         DC    AL4(0)                                                           
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(196,36,0,0,0,0)  13 WEEK - KS - UNIT 1                       
         DC    AL4(17050)                                                       
         DC    AL4(17050)                                                       
         DC    AL4(9570)                                                        
         DC    AL4(8470)                                                        
         DC    AL4(7590)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(196,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(365)                                                         
         DC    AL4(365)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(365)                                                         
*                                                                               
         DC    AL2(197,36,0,0,0,0)  13 WEEK - TX - UNIT 1                       
         DC    AL4(17600)                                                       
         DC    AL4(17600)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(12965)                                                       
         DC    AL4(17600)                                                       
*                                                                               
         DC    AL2(197,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(135)                                                         
         DC    AL4(260)                                                         
*                                                                               
         DC    AL2(198,36,0,0,0,0)  13 WEEK - NW - UNIT 1                       
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(9600)                                                        
         DC    AL4(9600)                                                        
         DC    AL4(9600)                                                        
         DC    AL4(17400)                                                       
*                                                                               
         DC    AL2(198,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         DC    AL4(323)                                                         
         EJECT                                                                  
*              ADDENDUM CABLE                                                   
*                                                                               
         DC    AL2(262,44,0,0,0,0)  1-50,000 SUBSCRIBERS TX                     
         DC    AL4(2000)            PRINCIPAL ON CAMERA                         
         DC    AL4(1500)              "     OFF CAMERA                          
         DC    AL4(1550)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1550)              "   6-8 "    "                            
         DC    AL4(1550)              "    9+ "    "                            
         DC    AL4(1000)            GROUP 3-5 OFF CAMERA                        
         DC    AL4(1000)              "   6-8  "    "                           
         DC    AL4(1000)              "    9+  "    "                           
*                                                                               
         DC    AL2(263,44,0,0,0,0)  1-50,000 SUBSCRIBERS NW                     
         DC    AL4(530)             PRINCIPAL ON CAMERA                         
         DC    AL4(370)               "     OFF CAMERA                          
         DC    AL4(420)             GROUP 3-5 ON CAMERA                         
         DC    AL4(290)               "   6-8 "    "                            
         DC    AL4(360)               "    9+ "    "                            
         DC    AL4(150)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(120)               "   6-8  "    "                           
         DC    AL4(100)               "    9+  "    "                           
*                                                                               
         DC    AL2(272,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS TX               
         DC    AL4(4000)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(3100)                                                        
         DC    AL4(3100)                                                        
         DC    AL4(3100)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
*                                                                               
         DC    AL2(273,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS NW               
         DC    AL4(1070)                                                        
         DC    AL4(730)                                                         
         DC    AL4(900)                                                         
         DC    AL4(720)                                                         
         DC    AL4(590)                                                         
         DC    AL4(300)                                                         
         DC    AL4(230)                                                         
         DC    AL4(195)                                                         
*                                                                               
         DC    AL2(282,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS TX              
         DC    AL4(6000)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4700)                                                        
         DC    AL4(4700)                                                        
         DC    AL4(4700)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
*                                                                               
         DC    AL2(283,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS NW              
         DC    AL4(1605)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1250)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(880)                                                         
         DC    AL4(445)                                                         
         DC    AL4(350)                                                         
         DC    AL4(295)                                                         
*                                                                               
         DC    AL2(292,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS TX              
         DC    AL4(8000)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
*                                                                               
         DC    AL2(293,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS NW              
         DC    AL4(2140)                                                        
         DC    AL4(1525)                                                        
         DC    AL4(1670)                                                        
         DC    AL4(1440)                                                        
         DC    AL4(1175)                                                        
         DC    AL4(595)                                                         
         DC    AL4(465)                                                         
         DC    AL4(390)                                                         
*                                                                               
         DC    AL2(302,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS TX              
         DC    AL4(10000)                                                       
         DC    AL4(6800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(2800)                                                        
*                                                                               
         DC    AL2(303,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS NW              
         DC    AL4(2675)                                                        
         DC    AL4(1830)                                                        
         DC    AL4(2090)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1460)                                                        
         DC    AL4(745)                                                         
         DC    AL4(580)                                                         
         DC    AL4(490)                                                         
*                                                                               
         DC    AL2(312,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS TX              
         DC    AL4(20000)                                                       
         DC    AL4(13650)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
*                                                                               
         DC    AL2(313,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS NW              
         DC    AL4(5350)                                                        
         DC    AL4(3665)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(3595)                                                        
         DC    AL4(2930)                                                        
         DC    AL4(1480)                                                        
         DC    AL4(1155)                                                        
         DC    AL4(970)                                                         
*                                                                               
         DC    AL2(322,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS TX              
         DC    AL4(30000)                                                       
         DC    AL4(20500)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(23300)                                                       
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
*                                                                               
         DC    AL2(323,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS NW              
         DC    AL4(8020)                                                        
         DC    AL4(5495)                                                        
         DC    AL4(6265)                                                        
         DC    AL4(5395)                                                        
         DC    AL4(4395)                                                        
         DC    AL4(2225)                                                        
         DC    AL4(1740)                                                        
         DC    AL4(1455)                                                        
*                                                                               
         DC    AL2(332,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS TX            
         DC    AL4(40000)                                                       
         DC    AL4(27300)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
*                                                                               
         DC    AL2(333,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS NW            
         DC    AL4(10695)                                                       
         DC    AL4(7325)                                                        
         DC    AL4(8345)                                                        
         DC    AL4(7190)                                                        
         DC    AL4(5860)                                                        
         DC    AL4(2965)                                                        
         DC    AL4(2320)                                                        
         DC    AL4(1945)                                                        
*                                                                               
         DC    AL2(342,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS TX                 
         DC    AL4(40000)                                                       
         DC    AL4(27300)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(31100)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
*                                                                               
         EJECT                                                                  
*&&DO                                                                           
*              INSERTS FOR BOOKENDS                                             
*                                                                               
IFBTAB   DC    AL2(44,44,1,1,0,0)  UNIT 1                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(44,44,2,3,0,0)  UNITS 2,3                                    
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(14125)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(10700)                                               
         DC    AL1(50),AL3(10700)                                               
*                                                                               
         DC    AL2(44,44,4,13,0,0)  UNITS 4-13                                  
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(14125)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(10700)                                               
         DC    AL1(25),AL3(10700)                                               
*                                                                               
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
         DC    AL4(14755)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
*                                                                               
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(8235)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
*                                                                               
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(4510)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
*                                                                               
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
         DC    AL4(27500)                                                       
         DC    AL4(41970)                                                       
         DC    AL4(15965)                                                       
         DC    AL4(27915)                                                       
         DC    AL4(77010)          PIL                                          
         DC    AL4(59215)          PI                                           
         DC    AL4(32425)          SE                                           
         DC    AL4(8015)           C3,C6                                        
         DC    AL4(15810)          C9                                           
*                                                                               
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(14755)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(14755)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
         DC    AL4(11180)                                                       
*                                                                               
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(8235)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
         DC    AL4(6205)                                                        
*                                                                               
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(4510)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(4510)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
         DC    AL4(3385)                                                        
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(22000)                                                       
         DC    AL4(22000)                                                       
         DC    AL4(16205)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(12725)                                                       
         DC    AL4(16915)                                                       
         DC    AL4(7550)                                                        
         DC    AL4(12080)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
         DC    AL4(9105)                                                        
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
         DC    AL4(6535)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
         DC    AL4(3565)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(80000)          (1.6 X 50000, BUT WANT NEAREST .05)          
         DC    AL4(60150)          (1.6 X 37595, BUT WANT NEAREST .05)          
         DC    AL4(58570)          (1.6 X 36605, BUT WANT NEAREST .05)          
         DC    AL4(51850)          (1.6 X 32405, BUT WANT NEAREST .05)          
         DC    AL4(42880)          (1.6 X 26800, BUT WANT NEAREST .05)          
         DC    AL4(33930)          (1.6 X 21205, BUT WANT NEAREST .05)          
         DC    AL4(29440)          (1.6 X 18400, BUT WANT NEAREST .05)          
         DC    AL4(24010)          (1.6 X 15005, BUT WANT NEAREST .05)          
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(35200)          (1.6 X 22000, BUT WANT NEAREST .05)          
         DC    AL4(35200)          (1.6 X 22000, BUT WANT NEAREST .05)          
         DC    AL4(25930)          (1.6 X 16205, BUT WANT NEAREST .05)          
         DC    AL4(22945)          (1.6 X 14340, BUT WANT NEAREST .05)          
         DC    AL4(20360)          (1.6 X 12725, BUT WANT NEAREST .05)          
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
*                                                                               
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(2210)            PRINCIPAL ON CAMERA                         
         DC    AL4(1510)                "     OFF CAMERA                        
         DC    AL4(1730)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1490)              "   6-8 "    "                            
         DC    AL4(1210)              "    9+ "    "                            
         DC    AL4(615)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(475)               "   6-8  "    "                           
         DC    AL4(400)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(4435)                                                        
         DC    AL4(3035)                                                        
         DC    AL4(3455)                                                        
         DC    AL4(2975)                                                        
         DC    AL4(2425)                                                        
         DC    AL4(1225)                                                        
         DC    AL4(960)                                                         
         DC    AL4(805)                                                         
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(6645)                                                        
         DC    AL4(4545)                                                        
         DC    AL4(5185)                                                        
         DC    AL4(4465)                                                        
         DC    AL4(3635)                                                        
         DC    AL4(1835)                                                        
         DC    AL4(1440)                                                        
         DC    AL4(1205)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(8860)                                                        
         DC    AL4(6065)                                                        
         DC    AL4(6910)                                                        
         DC    AL4(5955)                                                        
         DC    AL4(4855)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(1915)                                                        
         DC    AL4(1605)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(11070)                                                       
         DC    AL4(7580)                                                        
         DC    AL4(8635)                                                        
         DC    AL4(7440)                                                        
         DC    AL4(6065)                                                        
         DC    AL4(3070)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2015)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(22155)                                                       
         DC    AL4(15170)                                                       
         DC    AL4(17285)                                                       
         DC    AL4(14885)                                                       
         DC    AL4(12130)                                                       
         DC    AL4(6130)                                                        
         DC    AL4(4795)                                                        
         DC    AL4(4015)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(33220)                                                       
         DC    AL4(22745)                                                       
         DC    AL4(25920)                                                       
         DC    AL4(22325)                                                       
         DC    AL4(18200)                                                       
         DC    AL4(9205)                                                        
         DC    AL4(7195)                                                        
         DC    AL4(6030)                                                        
*        SPACE 1                                                                
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(44295)                                                       
         DC    AL4(30330)                                                       
         DC    AL4(34565)                                                       
         DC    AL4(29770)                                                       
         DC    AL4(24270)                                                       
         DC    AL4(12275)                                                       
         DC    AL4(9595)                                                        
         DC    AL4(8040)                                                        
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(50000)                                                       
         DC    AL4(37595)                                                       
         DC    AL4(36605)                                                       
         DC    AL4(32405)                                                       
         DC    AL4(26800)                                                       
         DC    AL4(21205)                                                       
         DC    AL4(18400)                                                       
         DC    AL4(15005)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
*                                                                               
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(11805)          ON CAMERA                                    
         DC    AL4(8945)           OFF                                          
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(8945)                                                        
         DC    AL4(8945)                                                        
         DC    AL4(8945)                                                        
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(11805)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8945)           SE                                           
         DC    AL4(8945)           C3,C6                                        
         DC    AL4(8945)           C9                                           
*                                                                               
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(6590)           ON CAMERA                                    
         DC    AL4(4965)           OFF                                          
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(4965)                                                        
         DC    AL4(4965)                                                        
         DC    AL4(4965)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(6590)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4965)           SE                                           
         DC    AL4(4965)           C3,C6                                        
         DC    AL4(4965)           C9                                           
*                                                                               
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(3610)           ON CAMERA                                    
         DC    AL4(2710)           OFF                                          
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(2710)                                                        
         DC    AL4(2710)                                                        
         DC    AL4(2710)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(3610)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2710)           SE                                           
         DC    AL4(2710)           C3,C6                                        
         DC    AL4(2710)           C9                                           
*                                                                               
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(7400)                                                        
*                                                                               
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(5230)                                                        
*                                                                               
         DC    AL2(248,44,50,255,0,0)  UNITS 50+                                
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
*                                                                               
         DC    AL2(249,88,1,4,0,0)  GA - TV, REGULAR, UNITS 1-4                 
         DC    AL4(12800)          ON CAMERA                                    
         DC    AL4(9700)           OFF                                          
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(9700)                                                        
         DC    AL4(9700)                                                        
         DC    AL4(9700)                                                        
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9700)           SE                                           
         DC    AL4(9700)           C3,C6                                        
         DC    AL4(9700)           C9                                           
         DC    AL4(12800)          SOLO/DUO ON CAMERA                           
         DC    AL4(9700)                    OFF                                 
*                                                                               
         DC    AL2(249,88,5,14,0,0)  UNITS 5-14                                 
         DC    AL4(7200)           ON CAMERA                                    
         DC    AL4(5400)           OFF                                          
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5400)           SE                                           
         DC    AL4(5400)           C3,C6                                        
         DC    AL4(5400)           C9                                           
         DC    AL4(7200)           SOLO/DUO ON CAMERA                           
         DC    AL4(5400)           OFF                                          
*                                                                               
         DC    AL2(249,88,15,255,0,0)  UNITS 15+                                
         DC    AL4(3900)           ON CAMERA                                    
         DC    AL4(2900)           OFF                                          
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2900)           SE                                           
         DC    AL4(2900)           C3,C6                                        
         DC    AL4(2900)           C9                                           
         DC    AL4(3900)           SOLO/DUO ON CAMERA                           
         DC    AL4(2900)           OFF                                          
         EJECT                                                                  
*              RATES FOR NORTHWEST ADDENDUM TAGS                                
*                                                                               
         DC    AL2(340,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  2WK            
         DC    AL4(10200)          ON CAMERA                                    
         DC    AL4(7800)           OFF                                          
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(7800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(7800)           SE                                           
         DC    AL4(7800)           C3,C6                                        
         DC    AL4(7800)           C9                                           
*                                                                               
         DC    AL2(340,80,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(8300)           ON CAMERA                                    
         DC    AL4(6200)           OFF                                          
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6200)           SE                                           
         DC    AL4(6200)           C3,C6                                        
         DC    AL4(6200)           C9                                           
*                                                                               
         DC    AL2(340,80,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(4600)           ON CAMERA                                    
         DC    AL4(3500)           OFF                                          
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3500)           SE                                           
         DC    AL4(3500)           C3,C6                                        
         DC    AL4(3500)           C9                                           
*                                                                               
         DC    AL2(340,80,25,255,0,0)  UNITS 26+  2-WK                          
         DC    AL4(2200)           ON CAMERA                                    
         DC    AL4(1900)           OFF                                          
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(1900)                                                        
         DC    AL4(1900)                                                        
         DC    AL4(1900)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(1900)           SE                                           
         DC    AL4(1900)           C3,C6                                        
         DC    AL4(1900)           C9                                           
*                                                                               
         DC    AL2(341,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  13WK           
         DC    AL4(11900)          ON CAMERA                                    
         DC    AL4(9000)           OFF                                          
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9000)           SE                                           
         DC    AL4(9000)           C3,C6                                        
         DC    AL4(9000)           C9                                           
*                                                                               
         DC    AL2(341,80,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(9800)           ON CAMERA                                    
         DC    AL4(7300)           OFF                                          
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7300)           SE                                           
         DC    AL4(7300)           C3,C6                                        
         DC    AL4(7300)           C9                                           
*                                                                               
         DC    AL2(341,80,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(5400)           ON CAMERA                                    
         DC    AL4(4100)           OFF                                          
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4100)           SE                                           
         DC    AL4(4100)           C3,C6                                        
         DC    AL4(4100)           C9                                           
*                                                                               
         DC    AL2(341,80,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(3000)           ON CAMERA                                    
         DC    AL4(2500)           OFF                                          
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2500)           SE                                           
         DC    AL4(2500)           C3,C6                                        
         DC    AL4(2500)           C9                                           
*                                                                               
         DC    AL2(342,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 2-WK            
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
*                                                                               
         DC    AL2(342,44,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
         DC    AL4(3600)                                                        
*                                                                               
         DC    AL2(342,44,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
*                                                                               
         DC    AL2(342,44,25,255,0,0)  UNITS 26+   2-WK                         
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
         DC    AL4(1100)                                                        
*                                                                               
         DC    AL2(343,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 13-WK           
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
         DC    AL4(6000)                                                        
*                                                                               
         DC    AL2(343,44,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
*                                                                               
         DC    AL2(343,44,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
*                                                                               
         DC    AL2(343,44,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
*                                                                               
         DC    AL2(344,44,1,255,0,0)  ADO TAGS UNITS 1-255                      
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
*                                                                               
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
*                                                                               
         DC    AL2(250,84,0,0,0,0)  PRM FOR SAG AND AFT                         
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(22000)          OFF                                          
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
*                                                                               
         DC    AL2(251,84,0,0,0,0)  PRR FOR AFT AND SAG                         
         DC    AL4(30500)          ON CAMERA                                    
         DC    AL4(22000)          OFF                                          
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
         DC    AL4(50000)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(37595)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(36605)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32405)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(26800)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(21205)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(18400)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(15005)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(50000)                                               
         DC    AL1(50),AL3(37595)                                               
         DC    AL1(50),AL3(36605)                                               
         DC    AL1(50),AL3(32405)                                               
         DC    AL1(50),AL3(26800)                                               
         DC    AL1(50),AL3(21205)                                               
         DC    AL1(50),AL3(18400)                                               
         DC    AL1(50),AL3(15005)                                               
*                                                                               
         DC    AL2(253,44,1,255,0,0)  VNW WITH NO SESS RATE                     
         DC    AL1(50),AL3(47870)                                               
         DC    AL1(50),AL3(35995)                                               
         DC    AL1(50),AL3(35045)                                               
         DC    AL1(50),AL3(31025)                                               
         DC    AL1(50),AL3(25660)                                               
         DC    AL1(50),AL3(20300)                                               
         DC    AL1(50),AL3(17615)                                               
         DC    AL1(50),AL3(14365)                                               
*                                                                               
         DC    AL2(254,32,1,1,0,0)  VNW RADIO 1ST USE AT SESSION FEE            
         DC    AL4(22000)                                                       
         DC    AL4(22000)                                                       
         DC    AL4(16205)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(12725)                                                       
*                                                                               
         DC    AL2(254,32,2,255,0,0)                                            
         DC    AL1(50),AL3(22000)                                               
         DC    AL1(50),AL3(22000)                                               
         DC    AL1(50),AL3(16205)                                               
         DC    AL1(50),AL3(14340)                                               
         DC    AL1(50),AL3(12725)                                               
*                                                                               
         DC    AL2(57,32,1,255,0,0)  VNW RADIO WITH NO SESS FEE                 
         DC    AL1(50),AL3(22000)                                               
         DC    AL1(50),AL3(22000)                                               
         DC    AL1(50),AL3(16205)                                               
         DC    AL1(50),AL3(14340)                                               
         DC    AL1(50),AL3(12725)                                               
*&&                                                                             
         EJECT                                                                  
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
         DC    AL2(66),AL1(UCSS,ALL,NON,0,0,0,TV)                               
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
         DC    AL2(66),AL1(UCSS,ALL,NON,0,0,0,CABLE)                            
         DC    AL2(64),AL1(URRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(64),AL1(USRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(37),AL1(UPPF,ALL,ALL-AFM,0,0,0,TV)                           
*                                                                               
         DC    AL2(62),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(UMVI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(61),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(77),AL1(UMVI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(62),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(UMVN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(61),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(77),AL1(UMVN,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(62),AL1(USMI,USMII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(USMI,USMII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(USMI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(62),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(USMN,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(61),AL1(ULFT,ALL,AFT+NON,0,0,0,RADIO)  LIFT                  
         DC    AL2(62),AL1(ULFT,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(64),AL1(ULFT,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(61),AL1(USLF,ALL,AFT+NON,0,0,0,RADIO)  SPAN LFT              
         DC    AL2(62),AL1(USLF,ALL,ALL-AFM,0,0,0,TV)                           
         DC    AL2(64),AL1(USLF,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(125),AL1(UALF,ALL,AFT+NON,0,0,0,RADIO) AD LIFT               
         DC    AL2(90),AL1(UALF,ALL,ALL-AFM,0,0,0,TV+CABLE)                     
*                                                                               
         DC    AL2(42),AL1(UDEM,ALL,ALL,0,0,0,TV)         DEMO                  
         DC    AL2(43),AL1(UDEM,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(42),AL1(USNA,ALL,ALL,0,0,0,TV)         SPANISH DEMO          
         DC    AL2(43),AL1(USNA,ALL,AFT+NON,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(63),AL1(UHLD,ALL,ALL-AFM,0,0,0,TV)     HLD FEE               
         DC    AL2(63),AL1(USHL,ALL,ALL-AFM,0,0,0,TV)     SPAN HLD              
         DC    AL2(38),AL1(UREN,ALL,ALL-AFM,0,0,0,TV)     REINST                
         DC    AL2(38),AL1(USRE,ALL,ALL-AFM,0,0,0,TV)     REINST                
         DC    AL2(225),AL1(UADH,ALL,ALL-AFM,0,0,0,TV)    AD HLD FEE            
         DC    AL2(230),AL1(UARN,ALL,ALL-AFM,0,0,0,TV)    AD REINST             
*                                                                               
         DC    AL2(00),AL1(UCLA,UCLAREG,ALL,0,0,0,ALL)    CLA                   
         DC    AL2(08),AL1(UCLA,UCLAG13,ALL,0,0,0,ALL)    13 USE GUAR           
         DC    AL2(75),AL1(UPAX,UPAXREG,ALL,0,0,0,ALL)    PAX                   
         DC    AL2(58),AL1(ULNA,ALL,ALL,0,0,0,ALL)        LT NGT ABC            
         DC    AL2(58),AL1(ULNN,ALL,ALL,0,0,0,ALL)               NBC            
         DC    AL2(58),AL1(ULNC,ALL,ALL,0,0,0,ALL)               CBS            
*                                                                               
         DC    AL2(01),AL1(ULOC,ULOCBNY,ALL,0,0,0,ALL)    CLASS A+NY            
         DC    AL2(02),AL1(ULOC,ULOCB,ALL,0,0,0,ALL)      CLASS B-NY            
         DC    AL2(03),AL1(ULOC,ULOCC,ALL,0,0,0,ALL)      CLASS C               
*                                                                               
         DC    AL2(10),AL1(UWSP,UWSP13W,ALL,0,0,0,TV)     13 WK TV              
         DC    AL2(15),AL1(UWSP,UWSP13W,ALL,0,0,0,RADIO)  13 WK RADIO           
         DC    AL2(20),AL1(UWSP,UWSP8W,ALL,0,0,0,RADIO)   8WK                   
*                                                                               
         DC    AL2(04),AL1(UDLR,UDLRANY,ALL,0,0,0,ALL)    DLR A + NY            
         DC    AL2(05),AL1(UDLR,UDLRA,ALL,0,0,0,ALL)      DLR A - NY            
         DC    AL2(06),AL1(UDLR,UDLRBNY,ALL,0,0,0,ALL)    DLR B + NY            
         DC    AL2(07),AL1(UDLR,UDLRB,ALL,0,0,0,ALL)      DLR B - NY            
         DC    AL2(25),AL1(UDLR,UDLRRAD,ALL,0,0,0,ALL)    DLR RADIO             
*                                                                               
         DC    AL2(41),AL1(UCBL,ALL,ALL,0,0,0,ALL)        CABLE                 
         DC    AL2(41),AL1(USCB,ALL,ALL,0,0,0,ALL)        SPAN CABLE            
*                                                                               
         DC    AL2(26),AL1(URNT,URNT1W,ALL,0,0,0,ALL)     RAD NWK 1WK           
         DC    AL2(27),AL1(URNT,URNT4W,ALL,0,0,0,ALL)             4WK           
         DC    AL2(28),AL1(URNT,URNT8W,ALL,0,0,0,ALL)             8WK           
         DC    AL2(29),AL1(URNT,URNT13W,ALL,0,0,0,ALL)           13WK           
         DC    AL2(30),AL1(URNT,URNTAB,ALL,0,0,0,ALL)             A-B           
         DC    AL2(31),AL1(URNT,URNT26U,ALL,0,0,0,ALL)         26 USE           
         DC    AL2(32),AL1(URNT,URNT39U,ALL,0,0,0,ALL)         39 USE           
*                                                                               
         DC    AL2(33),AL1(URRN,ALL,ALL,0,0,0,RADIO)      RAD REG NWK           
*                                                                               
         DC    AL2(39),AL1(URLO,ALL,ALL,0,0,0,ALL)        RAD LCL 13WK          
*                                                                               
         DC    AL2(35),AL1(UMUS,UMUSDUB,ALL,0,0,0,ALL)    DUB                   
         DC    AL2(35),AL1(UMUS,UMUS13W,ALL,0,0,0,ALL)    REUSE                 
         DC    AL2(35),AL1(UMUS,UMUSNEW,ALL,0,0,0,ALL)    NEW                   
         DC    AL2(35),AL1(UMUS,UMUSDSH,ALL,0,0,0,ALL)    DUB TO SHRT           
         DC    AL2(35),AL1(UNBM,ALL,ALL,0,0,0,ALL)        NON-BRD MUS           
         DC    AL2(09),AL1(UMUS,UMUSDUB8,ALL,0,0,0,ALL)   DUB-8WK               
         DC    AL2(09),AL1(UMUS,UMUS8W,ALL,0,0,0,ALL)     REUSE-8WK             
         DC    AL2(09),AL1(UMUS,UMUSNEW8,ALL,0,0,0,ALL)   NEW-8WK               
         DC    AL2(09),AL1(UMUS,UMUSDSH8,ALL,0,0,0,ALL)   DUB TO SH 8W          
         DC    AL2(36),AL1(UFMU,ALL,ALL,0,0,0,ALL)        1ST REUSE             
*                                                                               
         DC    AL2(45),AL1(UFGM,UFGMEU12,ALL,0,0,0,ALL)   AFM F-E 12M           
         DC    AL2(45),AL1(UFGM,UFGMNE12,ALL,0,0,0,ALL)   NOT EUR 12M           
         DC    AL2(47),AL1(UFGM,UFGMEU24,ALL,0,0,0,ALL)   EUR 24M               
         DC    AL2(47),AL1(UFGM,UFGMNE24,ALL,0,0,0,ALL)   NOT EUR 24M           
         DC    AL2(46),AL1(UFGM,UFGMWO12,ALL,0,0,0,ALL)   WRLD 12M              
         DC    AL2(48),AL1(UFGM,UFGMWO24,ALL,0,0,0,ALL)   WRLD 24M              
*                                                                               
         DC    AL2(50),AL1(UFGR,UFGRUK,ALL,0,0,0,TV)      FGN REUSE-UK          
         DC    AL2(51),AL1(UFGR,UFGREUR,ALL,0,0,0,TV)     EUR W/O UK            
         DC    AL2(62),AL1(UFGR,UFGRWOR,ALL,0,0,0,TV)     W W/O UK&EUR          
         DC    AL2(62),AL1(UFGR,UFGRAP,ALL,0,0,0,TV)      ASIAN PAC             
         DC    AL2(62),AL1(UFGR,UFGRJAP,ALL,0,0,0,TV)     JAPAN                 
         DC    AL2(237),AL1(UFGR,UFGRWIDE,ALL,0,0,0,TV)   WLDWIDE               
         DC    AL2(62),AL1(UFGR,UFGRMAJ,ALL,0,0,0,TV)     NEW-W/MAJOR           
         DC    AL2(256),AL1(UFGR,UFGREXT,ALL,0,0,0,TV)    FGR EXT               
         DC    AL2(49),AL1(UFGR,UFGRRAD,ALL,0,0,0,RADIO)  RADIO                 
*                                                                               
         DC    AL2(50),AL1(USFR,USFRA,ALL,0,0,0,TV)         SP FGN A            
         DC    AL2(51),AL1(USFR,USFRB,ALL,0,0,0,TV)         SP FGN B            
         DC    AL2(50),AL1(USFR,USFRC,ALL,0,0,0,TV)         SP FGN C            
*                                                                               
         DC    AL2(62),AL1(UPBS,ALL,ALL-AFM,0,0,0,TV)     PUB SVC               
         DC    AL2(52),AL1(UPBS,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(40),AL1(USNT,ALL,ALL,0,0,0,TV)         SPAN NWK              
         DC    AL2(40),AL1(USNW,ALL,ALL,0,0,0,TV)        SPAN N/W COMB          
         DC    AL2(10),AL1(USWS,ALL,ALL,0,0,0,TV)         SPAN WSP              
*                                                                               
         DC    AL2(65),AL1(UADT,UADT3D,ALL,0,0,0,ALL)   AD SES-TV-3DY           
         DC    AL2(70),AL1(UADT,UADT1W,ALL,0,0,0,ALL)   1WK                     
         DC    AL2(70),AL1(UADT,UADT2W,ALL,0,0,0,ALL)   2WK(NW ONLY)            
         DC    AL2(80),AL1(UADT,UADT4W,ALL,0,0,0,ALL)   4WK                     
         DC    AL2(80),AL1(UADT,UADT31D,ALL,0,0,0,ALL)  31D(KS ONLY)            
         DC    AL2(90),AL1(UADT,UADT13W,ALL,0,0,0,ALL)  13WEEK                  
*                                                                               
         DC    AL2(65),AL1(UARS,UARS3D,ALL,0,0,0,ALL)   AD RER-TV-3D            
         DC    AL2(70),AL1(UARS,UARS1W,ALL,0,0,0,ALL)   1WK                     
         DC    AL2(70),AL1(UARS,UARS2W,ALL,0,0,0,ALL)   2WK(NW ONLY)            
         DC    AL2(80),AL1(UARS,UARS4W,ALL,0,0,0,ALL)   4WK                     
         DC    AL2(80),AL1(UARS,UARS31D,ALL,0,0,0,ALL)  31D(KS ONLY)            
         DC    AL2(90),AL1(UARS,UARS13W,ALL,0,0,0,ALL)  13WK                    
*                                                                               
         DC    AL2(100),AL1(UADO,UADO3D,ALL,0,0,0,ALL)  AD SES-RAD-3D           
         DC    AL2(105),AL1(UADO,UADO1W,ALL,0,0,0,ALL)  1WK                     
         DC    AL2(105),AL1(UADO,UADO2W,ALL,0,0,0,ALL)  2WK(NW ONLY)            
         DC    AL2(115),AL1(UADO,UADO4W,ALL,0,0,0,ALL)  4WK                     
         DC    AL2(115),AL1(UADO,UADO31D,ALL,0,0,0,ALL) 31D(KS ONLY)            
         DC    AL2(125),AL1(UADO,UADO13W,ALL,0,0,0,ALL) 13WK                    
*                                                                               
         DC    AL2(100),AL1(UARR,UARR3D,ALL,0,0,0,ALL)  AD SES-RAD-3D           
         DC    AL2(105),AL1(UARR,UARR1W,ALL,0,0,0,ALL)  1WK                     
         DC    AL2(105),AL1(UARR,UARR2W,ALL,0,0,0,ALL)  2WK(NW ONLY)            
         DC    AL2(115),AL1(UARR,UARR4W,ALL,0,0,0,ALL)  4WK                     
         DC    AL2(115),AL1(UARR,UARR31D,ALL,0,0,0,ALL) 31D(KS ONLY)            
         DC    AL2(125),AL1(UARR,UARR13W,ALL,0,0,0,ALL) 13WK                    
*                                                                               
         DC    AL2(205),AL1(UADD,ALL,ALL,0,0,0,TV)        AD DEMO               
         DC    AL2(215),AL1(UADD,ALL,AFT+NON,0,0,0,RADIO)                       
*                                                                               
         DC    AL2(135),AL1(UADW,UADW3D,ALL,0,0,0,TV)   AD WSP-TV-3D            
         DC    AL2(140),AL1(UADW,UADW1W,ALL,0,0,0,TV)   1WK                     
         DC    AL2(140),AL1(UADW,UADW2W,ALL,0,0,0,TV)   2WK(NW ONLY)            
         DC    AL2(150),AL1(UADW,UADW4W,ALL,0,0,0,TV)   4WK                     
         DC    AL2(150),AL1(UADW,UADW31D,ALL,0,0,0,TV)  31D(KS ONLY)            
         DC    AL2(160),AL1(UADW,UADW13W,ALL,0,0,0,TV)  13WK                    
*                                                                               
         DC    AL2(170),AL1(UADW,UADW3D,ALL,0,0,0,RADIO) AD WSP-RAD-3D          
         DC    AL2(175),AL1(UADW,UADW1W,ALL,0,0,0,RADIO) 1WK                    
         DC    AL2(175),AL1(UADW,UADW2W,ALL,0,0,0,RADIO) 2WK(NW ONLY)           
         DC    AL2(185),AL1(UADW,UADW4W,ALL,0,0,0,RADIO) 4WK                    
         DC    AL2(185),AL1(UADW,UADW31D,ALL,0,0,0,RADIO) 31D(KS ONLY)          
         DC    AL2(195),AL1(UADW,UADW13W,ALL,0,0,0,RADIO) 13WK                  
*                                                                               
         DC    AL2(260),AL1(UACB,UACB50,ALL,0,0,0,ALL)    AD CABLE              
         DC    AL2(270),AL1(UACB,UACB100,ALL,0,0,0,ALL)                         
         DC    AL2(280),AL1(UACB,UACB150,ALL,0,0,0,ALL)                         
         DC    AL2(290),AL1(UACB,UACB200,ALL,0,0,0,ALL)                         
         DC    AL2(300),AL1(UACB,UACB250,ALL,0,0,0,ALL)                         
         DC    AL2(310),AL1(UACB,UACB500,ALL,0,0,0,ALL)                         
         DC    AL2(320),AL1(UACB,UACB750,ALL,0,0,0,ALL)                         
         DC    AL2(330),AL1(UACB,UACB1M,ALL,0,0,0,ALL)                          
         DC    AL2(340),AL1(UACB,UACBMAX,ALL,0,0,0,ALL)                         
*                                                                               
         DC    AL2(260),AL1(UACB,UACBM50,ALL,0,0,0,ALL)    AD CBL+              
         DC    AL2(270),AL1(UACB,UACBM100,ALL,0,0,0,ALL)   MILLION              
         DC    AL2(280),AL1(UACB,UACBM150,ALL,0,0,0,ALL)                        
         DC    AL2(290),AL1(UACB,UACBM200,ALL,0,0,0,ALL)                        
         DC    AL2(300),AL1(UACB,UACBM250,ALL,0,0,0,ALL)                        
         DC    AL2(310),AL1(UACB,UACBM500,ALL,0,0,0,ALL)                        
         DC    AL2(320),AL1(UACB,UACBM750,ALL,0,0,0,ALL)                        
         DC    AL2(330),AL1(UACB,UACBM1M,ALL,0,0,0,ALL)                         
*                                                                               
         DC    AL2(135),AL1(UADC,UADC3D,ALL,0,0,0,TV)    CMB S/W-TV-3DY         
         DC    AL2(140),AL1(UADC,UADC1W,ALL,0,0,0,TV)    1WK                    
         DC    AL2(140),AL1(UADC,UADC2W,ALL,0,0,0,TV)    2WK(NW ONLY)           
         DC    AL2(150),AL1(UADC,UADC4W,ALL,0,0,0,TV)    4WK                    
         DC    AL2(150),AL1(UADC,UADC31D,ALL,0,0,0,TV)   31D(KS ONLY)           
         DC    AL2(160),AL1(UADC,UADC13W,ALL,0,0,0,TV)   13WK                   
*                                                                               
         DC    AL2(170),AL1(UADC,UADC3D,ALL,0,0,0,RADIO) CMB S/W-RAD-3D         
         DC    AL2(175),AL1(UADC,UADC1W,ALL,0,0,0,RADIO) 1WK                    
         DC    AL2(175),AL1(UADC,UADC2W,ALL,0,0,0,RADIO) 2WK(NW ONLY)           
         DC    AL2(185),AL1(UADC,UADC4W,ALL,0,0,0,RADIO) 4WK                    
         DC    AL2(185),AL1(UADC,UADC31D,ALL,0,0,0,RADIO) 31D(KS ONLY)          
         DC    AL2(195),AL1(UADC,UADC13W,ALL,0,0,0,RADIO) 13WK                  
*                                                                               
*        DC    AL2(44),AL1(UIFB,ALL,ALL,0,0,0,TV)       INS FOR BOOKS           
*                                                                               
         DC    AL2(53),AL1(UTAG,UTAGREG,ALL,0,0,0,TV)      TAGS TV              
         DC    AL2(54),AL1(UTAG,UTAGSESS,ALL,0,0,0,TV)     TAGS TV              
         DC    AL2(55),AL1(UTAG,UTAGREG,ALL,0,0,0,RADIO)   TAGS RAD             
         DC    AL2(56),AL1(UTAG,UTAGSESS,ALL,0,0,0,RADIO)  TAGS RAD             
*                                                                               
         DC    AL2(62),AL1(UINR,UINR30D,ALL,0,0,0,TV)    TH/IND-30DY            
         DC    AL2(236),AL1(UINR,UINRUNL,ALL,0,0,0,TV)   UNLIM                  
         DC    AL2(61),AL1(UINR,UINR30D,ALL,0,0,0,RADIO) 30DAYS-RAD             
         DC    AL2(235),AL1(UINR,UINRUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
         DC    AL2(62),AL1(USIN,USIN30D,ALL,0,0,0,TV)   SP IND-30 DA            
         DC    AL2(236),AL1(USIN,USINUNL,ALL,0,0,0,TV)    UNLIM                 
         DC    AL2(61),AL1(USIN,USIN30D,ALL,0,0,0,RADIO)  30DAYS-RAD            
         DC    AL2(235),AL1(USIN,USINUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
*                                                                               
         DC    AL2(238),AL1(ULCB,ULCB50,ALL,0,0,0,ALL)    LCL CAB               
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
******  USE NUMBERS 75 USED FOR 00 PAX                                          
******  USE NUMBER 76,77 USED FOR MOVED TO INTERNET/NEWMEDIA                    
******  USE NUMBER 85,86 USED FOR SPANISH INTERNET                              
******  USE NUMBERS 260-350 USED FOR ADDENDUM CABLE                             
******  USE NUMBERS 359-366 USED FOR MUSIC/FOREIGN MUSIC                        
******  USE NUMBERS 370-372 USED FOR IMS                                        
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
         DC    AL4(9105)           RADIO SESSION TAG FEE                        
         DC    AL4(12800)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(9700)           GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(8100)           GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(8570)           KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(5955)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4070)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(11805)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(8945)           TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(7400)           TX ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(10200)          NW ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(7800)           NW ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4900)           NW ADDENDUM RADIO SESSION TAG FEE            
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
         DC    AL1(14,CTPI)                                                     
         DC    AL1(13,CTPIL)                                                    
*                                  IF DEFINE ROW >= 18, CHANGE SYSCALC          
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    AL1(60,CTCAR)                                                    
         DC    AL1(60,CTGEN)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - TV - OFF CAMERA                                  
         SPACE 1                                                                
OFFCOLS  DS    0CL2                                                             
         DC    AL1(1,CTEXD)        EXD ALWAYS GETS ON CAMERA RATES              
*                                                                               
         DC    AL1(1,CTGAF)                                                     
         DC    AL1(1,CTGRK)                                                     
         DC    AL1(1,CTPRM)                                                     
         DC    AL1(1,CTSCS)                                                     
         DC    AL1(1,CTGRD)                                                     
         DC    AL1(1,CTKMU)                                                     
         DC    AL1(1,CTKHS)                                                     
         DC    AL1(1,CTCOS)                                                     
*                                                                               
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
         DC    AL1(2,CTBBE)                                                     
         DC    AL1(2,CTDO)                                                      
         DC    AL1(2,CTGO)                                                      
         DC    AL1(2,CTGRB)                                                     
         DC    AL1(2,CTEL)                                                      
         DC    AL1(2,CTPRA)                                                     
         DC    AL1(2,CTPTR)                                                     
         DC    AL1(2,CTSPE)                                                     
*                                                                               
         DC    AL1(3,CTGR)                                                      
         DC    AL1(3,CTLMP)                                                     
         DC    AL1(3,CTDEC)                                                     
         DC    AL1(3,CTSDR)                                                     
         DC    AL1(3,CTCOA)                                                     
         DC    AL1(3,CTHA)                                                      
         DC    AL1(3,CTMUA)                                                     
         DC    AL1(3,CTCPT)                                                     
         DC    AL1(3,CTOTC)                                                     
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
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
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
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
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
         DC    AL1(1,CTSL)         SIDELINE                                     
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
         DC    AL1(1,CTZZ)         GENERAL                                      
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
**PAN#1  DC    CL21'019TAGEN61   10/05/11'                                      
         END                                                                    
