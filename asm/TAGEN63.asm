*          DATA SET TAGEN63    AT LEVEL 016 AS OF 09/06/13                      
*PHASE T70263C,*                                                                
         TITLE 'T70263 - TABLES FOR 2006 CONTRACT'                              
T70263   CSECT                                                                  
         DC    AL4(USETBLS-T70263)                                              
         DC    AL4(USELUT-T70263)                                               
         DC    AL4(MAJLUT-T70263)                                               
         DC    AL4(AFMCOLS-T70263)                                              
         DC    AL4(RADCOLS-T70263)                                              
         DC    AL4(OFFCOLS-T70263)                                              
         DC    AL4(ONCOLS-T70263)                                               
         DC    AL4(MSWEET-T70263)                                               
         DC    AL4(TAGFEE-T70263)                                               
         DC    AL4(DIOCOLS-T70263)                                              
         DC    AL4(RTKCOLS-T70263)                                              
         DC    AL4(INDEXT-T70263)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
*                                                                               
USETBLS  DS    0F                                                               
CLATBL   DC    AL2(0,44,1,1,0,0)    CLASS A USE 1                               
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(56710)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(42640)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(41515)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(36755)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(30395)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(24050)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(20870)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17020)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(13005)                                                       
         DC    AL4(10175)                                                       
         DC    AL4(12050)                                                       
         DC    AL4(10320)                                                       
         DC    AL4(8445)                                                        
         DC    AL4(6540)                                                        
         DC    AL4(5685)                                                        
         DC    AL4(4665)                                                        
*                                                                               
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(10320)                                                       
         DC    AL4(8095)                                                        
         DC    AL4(9435)                                                        
         DC    AL4(8550)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6110)                                                        
         DC    AL4(5230)                                                        
         DC    AL4(4270)                                                        
*                                                                               
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(10320)                                                       
         DC    AL4(8095)                                                        
         DC    AL4(8905)                                                        
         DC    AL4(8020)                                                        
         DC    AL4(6570)                                                        
         DC    AL4(5580)                                                        
         DC    AL4(4870)                                                        
         DC    AL4(3990)                                                        
*                                                                               
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(4945)                                                        
         DC    AL4(3675)                                                        
         DC    AL4(3075)                                                        
         DC    AL4(2615)                                                        
         DC    AL4(2120)                                                        
         DC    AL4(2225)                                                        
         DC    AL4(2090)                                                        
         DC    AL4(1735)                                                        
*                                                                               
PAXTAB   DC    AL2(75,44,1,255,0,0)   PAX USE                                   
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(2120)           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(1590)           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(1320)           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(1125)           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(910)            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(960)            'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(895)            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(745)            'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
CLBTAB   DC    AL2(1,44,0,0,0,0)   CLASS B WITH NY                              
         DC    AL4(107295)                                                      
         DC    AL4(76735)                                                       
         DC    AL4(68340)                                                       
         DC    AL4(60430)                                                       
         DC    AL4(49400)                                                       
         DC    AL4(25180)                                                       
         DC    AL4(20990)                                                       
         DC    AL4(17155)                                                       
*                                                                               
CBXTAB   DC    AL2(2,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(87515)                                                       
         DC    AL4(60780)                                                       
         DC    AL4(68340)                                                       
         DC    AL4(60430)                                                       
         DC    AL4(49400)                                                       
         DC    AL4(25180)                                                       
         DC    AL4(20990)                                                       
         DC    AL4(17155)                                                       
*                                                                               
CLCTAB   DC    AL2(3,44,0,0,0,0)   CLASS C                                      
         DC    AL4(52150)                                                       
         DC    AL4(34770)                                                       
         DC    AL4(45200)                                                       
         DC    AL4(40170)                                                       
         DC    AL4(32845)                                                       
         DC    AL4(20030)                                                       
         DC    AL4(16670)                                                       
         DC    AL4(13675)                                                       
*                                                                               
NWKTBL   DC    AL2(58,44,1,1,0,0)  NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(31275)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(23515)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(22900)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(20275)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(16835)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(13265)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(11505)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(9385)           'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
*                                                                               
DANTAB   DC    AL2(4,44,0,0,0,0)   DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(212815)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(148230)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(159795)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(140860)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(109475)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(65325)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(57210)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(40845)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB   DC    AL2(5,44,0,0,0,0)   DEALER A W/O NY                              
         DC    AL4(188210)                                                      
         DC    AL4(135935)                                                      
         DC    AL4(159795)                                                      
         DC    AL4(140860)                                                      
         DC    AL4(109475)                                                      
         DC    AL4(65325)                                                       
         DC    AL4(57210)                                                       
         DC    AL4(40845)                                                       
*                                                                               
DBNTAB   DC    AL2(6,44,0,0,0,0)   CLASS B INCL NY                              
         DC    AL4(327215)                                                      
         DC    AL4(222660)                                                      
         DC    AL4(242950)                                                      
         DC    AL4(214170)                                                      
         DC    AL4(166680)                                                      
         DC    AL4(99520)                                                       
         DC    AL4(87100)                                                       
         DC    AL4(62130)                                                       
*                                                                               
DBXTAB   DC    AL2(7,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(282325)                                                      
         DC    AL4(203585)                                                      
         DC    AL4(242950)                                                      
         DC    AL4(214170)                                                      
         DC    AL4(166680)                                                      
         DC    AL4(99520)                                                       
         DC    AL4(87100)                                                       
         DC    AL4(62130)                                                       
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
*                                                                               
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(159190)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(123165)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(131770)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(117680)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(96670)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(80095)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(69705)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(57030)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
*                                                                               
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(9752)                                                        
         DC    AL4(7407)                                                        
         DC    AL4(7129)                                                        
         DC    AL4(6239)                                                        
         DC    AL4(5095)                                                        
         DC    AL4(4710)                                                        
         DC    AL4(4246)                                                        
         DC    AL4(3500)                                                        
*                                                                               
         DC    AL2(8,44,19,255,0,0)  19+                                        
         DC    AL4(4945)                                                        
         DC    AL4(3675)                                                        
         DC    AL4(3075)                                                        
         DC    AL4(2615)                                                        
         DC    AL4(2120)                                                        
         DC    AL4(2225)                                                        
         DC    AL4(2090)                                                        
         DC    AL4(1735)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(56710)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(42640)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(41515)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(36755)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(30395)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(24050)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(20870)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17020)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(1941)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1513)                                                        
         DC    AL4(1305)                                                        
         DC    AL4(1066)                                                        
         DC    AL4(536)                                                         
         DC    AL4(423)                                                         
         DC    AL4(352)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(780)                                                         
         DC    AL4(661)                                                         
         DC    AL4(548)                                                         
         DC    AL4(226)                                                         
         DC    AL4(155)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(565)                                                         
         DC    AL4(441)                                                         
         DC    AL4(370)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
*                                                                               
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(280)                                                         
         DC    AL4(226)                                                         
         DC    AL4(197)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(111445)                                                      
         DC    AL4(78730)                                                       
         DC    AL4(71370)                                                       
         DC    AL4(63395)                                                       
         DC    AL4(51945)                                                       
         DC    AL4(28630)                                                       
         DC    AL4(23725)                                                       
         DC    AL4(19425)                                                       
*                                                                               
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(780)                                                         
         DC    AL4(661)                                                         
         DC    AL4(548)                                                         
         DC    AL4(226)                                                         
         DC    AL4(155)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(565)                                                         
         DC    AL4(441)                                                         
         DC    AL4(370)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
*                                                                               
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(280)                                                         
         DC    AL4(226)                                                         
         DC    AL4(197)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(97140)                                                       
         DC    AL4(68510)                                                       
         DC    AL4(71370)                                                       
         DC    AL4(63395)                                                       
         DC    AL4(51945)                                                       
         DC    AL4(28630)                                                       
         DC    AL4(23725)                                                       
         DC    AL4(19425)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(780)                                                         
         DC    AL4(661)                                                         
         DC    AL4(548)                                                         
         DC    AL4(226)                                                         
         DC    AL4(155)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(565)                                                         
         DC    AL4(441)                                                         
         DC    AL4(370)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
*                                                                               
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(280)                                                         
         DC    AL4(226)                                                         
         DC    AL4(197)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(153365)                                                      
         DC    AL4(103265)                                                      
         DC    AL4(109810)                                                      
         DC    AL4(90795)                                                       
         DC    AL4(74230)                                                       
         DC    AL4(37835)                                                       
         DC    AL4(30475)                                                       
         DC    AL4(24950)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(720)                                                         
         DC    AL4(565)                                                         
         DC    AL4(280)                                                         
         DC    AL4(226)                                                         
         DC    AL4(197)                                                         
         DC    AL4(137)                                                         
         DC    AL4(78)                                                          
         DC    AL4(78)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(184990)                                                      
         DC    AL4(131390)                                                      
         DC    AL4(138535)                                                      
         DC    AL4(118560)                                                      
         DC    AL4(96905)                                                       
         DC    AL4(45615)                                                       
         DC    AL4(36785)                                                       
         DC    AL4(30060)                                                       
*                                                                               
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(738)                                                         
         DC    AL4(578)                                                         
         DC    AL4(286)                                                         
         DC    AL4(232)                                                         
         DC    AL4(202)                                                         
         DC    AL4(142)                                                         
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(24950)          ANN ALONE                                    
         DC    AL4(24950)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(18380)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(16265)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(14430)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(8390)           SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(191)                                                         
         DC    AL4(163)                                                         
         DC    AL4(144)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(163)                                                         
         DC    AL4(125)                                                         
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(92)                                                          
         DC    AL4(80)                                                          
         DC    AL4(80)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(37355)                                                       
         DC    AL4(37355)                                                       
         DC    AL4(20315)                                                       
         DC    AL4(18035)                                                       
         DC    AL4(16010)                                                       
         DC    AL4(8390)                                                        
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(163)                                                         
         DC    AL4(137)                                                         
         DC    AL4(131)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(92)                                                          
         DC    AL4(80)                                                          
         DC    AL4(80)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(33880)                                                       
         DC    AL4(33880)                                                       
         DC    AL4(20315)                                                       
         DC    AL4(18035)                                                       
         DC    AL4(16010)                                                       
         DC    AL4(8390)                                                        
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(163)                                                         
         DC    AL4(137)                                                         
         DC    AL4(131)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(92)                                                          
         DC    AL4(80)                                                          
         DC    AL4(80)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(45560)                                                       
         DC    AL4(45560)                                                       
         DC    AL4(24260)                                                       
         DC    AL4(18615)                                                       
         DC    AL4(16565)                                                       
         DC    AL4(8390)                                                        
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(137)                                                         
         DC    AL4(137)                                                         
         DC    AL4(131)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(92)                                                          
         DC    AL4(80)                                                          
         DC    AL4(80)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(57570)                                                       
         DC    AL4(57570)                                                       
         DC    AL4(27030)                                                       
         DC    AL4(20915)                                                       
         DC    AL4(18615)                                                       
         DC    AL4(8390)                                                        
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(137)                                                         
         DC    AL4(137)                                                         
         DC    AL4(131)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(276)                                                         
         DC    AL4(276)                                                         
         DC    AL4(92)                                                          
         DC    AL4(80)                                                          
         DC    AL4(80)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(24950) ANN ALONE                                    
         DC    AL1(100),AL3(24950) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(18380) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(16265) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(14430) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL1(80),AL3(367)                                                 
         DC    AL1(80),AL3(367)                                                 
         DC    AL1(95),AL3(191)                                                 
         DC    AL1(95),AL3(163)                                                 
         DC    AL1(95),AL3(144)                                                 
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(163)                                                 
         DC    AL1(95),AL3(125)                                                 
         DC    AL1(95),AL3(125)                                                 
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(92)                                                  
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL1(80),AL3(37355)                                               
         DC    AL1(80),AL3(37355)                                               
         DC    AL1(95),AL3(20315)                                               
         DC    AL1(95),AL3(18035)                                               
         DC    AL1(95),AL3(16010)                                               
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(163)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(131)                                                 
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(92)                                                  
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(33880)                                               
         DC    AL1(80),AL3(33880)                                               
         DC    AL1(95),AL3(20315)                                               
         DC    AL1(95),AL3(18035)                                               
         DC    AL1(95),AL3(16010)                                               
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(163)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(131)                                                 
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(92)                                                  
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL1(80),AL3(45560)                                               
         DC    AL1(80),AL3(45560)                                               
         DC    AL1(95),AL3(24260)                                               
         DC    AL1(95),AL3(18615)                                               
         DC    AL1(95),AL3(16565)                                               
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(131)                                                 
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(92)                                                  
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL1(80),AL3(57570)                                               
         DC    AL1(80),AL3(57570)                                               
         DC    AL1(95),AL3(27030)                                               
         DC    AL1(95),AL3(20915)                                               
         DC    AL1(95),AL3(18615)                                               
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(137)                                                 
         DC    AL1(95),AL3(131)                                                 
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(80),AL3(276)                                                 
         DC    AL1(95),AL3(92)                                                  
         DC    AL1(95),AL3(80)                                                  
         DC    AL1(95),AL3(80)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
*                                                                               
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(67470)          AR,AS,P,ANN                                  
         DC    AL4(53520)          S,1-4MS,1-4SS                                
         DC    AL4(34895)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(27920)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(17450)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
*                                                                               
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(42215)          ANN ALONE                                    
         DC    AL4(42215)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(31680)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(31680)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(31680)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(9425)           SE                                           
*                                                                               
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(68490)                                                       
         DC    AL4(68490)                                                       
         DC    AL4(52670)                                                       
         DC    AL4(47100)                                                       
         DC    AL4(43030)                                                       
         DC    AL4(9425)                                                        
*                                                                               
N08TAB   DC    AL2(28,36,0,0,0,0)  NETWORK 8 WEEK                               
         DC    AL4(109105)                                                      
         DC    AL4(109105)                                                      
         DC    AL4(83945)                                                       
         DC    AL4(74990)                                                       
         DC    AL4(67205)                                                       
         DC    AL4(9425)                                                        
*                                                                               
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(135385)                                                      
         DC    AL4(135385)                                                      
         DC    AL4(104135)                                                      
         DC    AL4(93110)                                                       
         DC    AL4(85300)                                                       
         DC    AL4(9425)                                                        
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
*                                                                               
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(141770)                                                      
         DC    AL4(141770)                                                      
         DC    AL4(109020)                                                      
         DC    AL4(97495)                                                       
         DC    AL4(89320)                                                       
         DC    AL4(9425)                                                        
*                                                                               
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(67700)                                                       
         DC    AL4(67700)                                                       
         DC    AL4(52055)                                                       
         DC    AL4(46450)                                                       
         DC    AL4(42535)                                                       
         DC    AL4(9425)                                                        
*                                                                               
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(101950)                                                      
         DC    AL4(101950)                                                      
         DC    AL4(71385)                                                       
         DC    AL4(63720)                                                       
         DC    AL4(57890)                                                       
         DC    AL4(9425)                                                        
*                                                                               
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(81700)                                                       
         DC    AL4(81700)                                                       
         DC    AL4(38295)                                                       
         DC    AL4(38295)                                                       
         DC    AL4(38295)                                                       
         DC    AL4(9425)                                                        
*                                                                               
         DC    AL2(34,36,0,0,0,0)  REGIONAL - WITH ANY MAJORS                   
         DC    AL4(81700)                                                       
         DC    AL4(81700)                                                       
         DC    AL4(81700)                                                       
         DC    AL4(73530)                                                       
         DC    AL4(66130)                                                       
         DC    AL4(9425)                                                        
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
         DC    AL2(375,24,0,0,0,0) 04 REUSE                                     
         DC    AL4(8250)           CAST=1                                       
         DC    AL4(8250)                2-4                                     
         DC    AL4(8250)                5+                                      
*                                                                               
         DC    AL2(400,24,0,0,0,0) 07 REUSE                                     
         DC    AL4(8625)           CAST=1                                       
         DC    AL4(8625)                2-4                                     
         DC    AL4(8625)                5+                                      
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
         DC    AL2(376,24,0,0,0,0)  04 FIRST REUSE                              
         DC    AL4(6200)           CAST=1                                       
         DC    AL4(3100)                2-4                                     
         DC    AL4(3100)                5+                                      
*                                                                               
         DC    AL2(401,24,0,0,0,0)  07 FIRST REUSE                              
         DC    AL4(6400)           CAST=1                                       
         DC    AL4(3200)                2-4                                     
         DC    AL4(3200)                5+                                      
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
         DC    AL2(377,24,0,0,0,0)  04 SESSION                                  
         DC    AL4(11000)          CAST=1                                       
         DC    AL4(11000)               2-4                                     
         DC    AL4(11000)               5+                                      
*                                                                               
         DC    AL2(402,24,0,0,0,0)  07 SESSION                                  
         DC    AL4(11500)          CAST=1                                       
         DC    AL4(11500)               2-4                                     
         DC    AL4(11500)               5+                                      
*                                                                               
IMSTAB   DC    AL2(59,24,0,0,0,0)  SESSION   OLD IMS RATES                      
         DC    AL4(16548)          ON CAM                                       
         DC    AL4(16363)          OFF CAM                                      
         DC    AL4(16548)          ON CAM - 1 PERSON ALONE                      
*                                                                               
         DC    AL2(257,24,0,0,0,0)  SESSION  >= 2/13/00 IMS RATES               
         DC    AL4(17044)           ON CAM                                      
         DC    AL4(16854)           OFF CAM                                     
         DC    AL4(17044)           ON CAM - 1 PERSON ALONE                     
*                                                                               
         DC    AL2(258,24,0,0,0,0)  SESSION  >= 1/18/01 IMS RATES               
         DC    AL4(17555)           ON CAM                                      
         DC    AL4(17360)           OFF CAM                                     
         DC    AL4(17555)           ON CAM - 1 PERSON ALONE                     
*                                                                               
         DC    AL2(370,24,0,0,0,0)  SESSION  >= 2/16/02 IMS RATES               
         DC    AL4(18082)           ON CAM                                      
         DC    AL4(17881)           OFF CAM                                     
         DC    AL4(18082)           ON CAM - 1 PERSON ALONE                     
*                                                                               
         DC    AL2(371,24,0,0,0,0)  SESSION  >= 2/16/03 IMS RATES               
         DC    AL4(18624)           ON CAM                                      
         DC    AL4(18417)           OFF CAM                                     
         DC    AL4(21948)           ON CAM - 1 PERSON ALONE                     
*                                                                               
         DC    AL2(372,24,0,0,0,0)  SESSION  >= 2/16/04 IMS RATES               
         DC    AL4(19183)           ON CAM                                      
         DC    AL4(18970)           OFF CAM                                     
         DC    AL4(22606)           ON CAM - 1 PERSON ALONE                     
*                                                                               
         DC    AL2(373,24,0,0,0,0)  SESSION  >= 12/01/05 IMS RATES              
         DC    AL4(19758)           ON CAM                                      
         DC    AL4(19539)           OFF CAM                                     
         DC    AL4(23284)           ON CAM - 1 PERSON ALONE                     
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
*                                                                               
         DC    AL2(378,24,0,0,0,0) 04 8-WEEK REUSE                              
         DC    AL1(80),AL3(8250)                                                
         DC    AL1(80),AL3(8250)                                                
         DC    AL1(80),AL3(8250)                                                
*                                                                               
         DC    AL2(403,24,0,0,0,0) 07 8-WEEK REUSE                              
         DC    AL1(80),AL3(8625)                                                
         DC    AL1(80),AL3(8625)                                                
         DC    AL1(80),AL3(8625)                                                
*                                                                               
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
         DC    AL2(379,24,0,0,0,0)  04 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(6875)           CAST=1                                       
         DC    AL4(6875)                2-4                                     
         DC    AL4(6875)                5+                                      
*                                                                               
         DC    AL2(404,24,0,0,0,0)  07 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(7184)           CAST=1                                       
         DC    AL4(7184)                2-4                                     
         DC    AL4(7184)                5+                                      
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
         DC    AL2(380,24,0,0,0,0)  04 WORLD - 12M                              
         DC    AL4(11000)          CAST=1                                       
         DC    AL4(11000)               2-4                                     
         DC    AL4(11000)               5+                                      
*                                                                               
         DC    AL2(405,24,0,0,0,0)  07 WORLD - 12M                              
         DC    AL4(11500)          CAST=1                                       
         DC    AL4(11500)               2-4                                     
         DC    AL4(11500)               5+                                      
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
         DC    AL2(381,24,0,0,0,0)  04 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(10313)          CAST=1                                       
         DC    AL4(10313)               2-4                                     
         DC    AL4(10313)               5+                                      
*                                                                               
         DC    AL2(406,24,0,0,0,0)  07 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(10776)          CAST=1                                       
         DC    AL4(10776)               2-4                                     
         DC    AL4(10776)               5+                                      
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
*                                                                               
         DC    AL2(382,24,0,0,0,0)  04 WORLD - 24M                              
         DC    AL4(16499)          CAST=1                                       
         DC    AL4(16499)              2-4                                      
         DC    AL4(16499)              5+                                       
*                                                                               
         DC    AL2(407,24,0,0,0,0)  07 WORLD - 24M                              
         DC    AL4(17250)          CAST=1                                       
         DC    AL4(17250)              2-4                                      
         DC    AL4(17250)              5+                                       
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
*                                                                               
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(24950)          ANN ALONE                                    
         DC    AL4(24950)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(18380)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(16265)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(14430)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(19185)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(56710)              PRINCIPAL ON  CAMERA                     
         DC    AL4(42640)                  "     OFF   "                        
         DC    AL4(41515)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(36755)                "    6-8    "                          
         DC    AL4(30395)                "     9+    "                          
         DC    AL4(24050)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(20870)                "    6-8    "                          
         DC    AL4(17020)                "     9+    "                          
         DC    AL4(30930)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(47200)              HAND MODEL UNLIMITED                     
         DC    AL4(17955)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(31395)              HAND MODEL 13 WEEKS                      
         DC    AL4(87345)    PIL       PILOT LOCATION RATE                      
         DC    AL4(67160)    PI        PILOT STUDIO RATE                        
         DC    AL4(36775)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(9090)     C3,C6     CONTRACTORS                              
         DC    AL4(17930)    C9             "                                   
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(56710)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(42640)                                               
         DC    AL1(50),AL3(41515)                                               
         DC    AL1(50),AL3(36755)                                               
         DC    AL1(50),AL3(30395)                                               
         DC    AL1(50),AL3(24050)                                               
         DC    AL1(50),AL3(20870)                                               
         DC    AL1(50),AL3(17020)                                               
         DC    AL1(50),AL3(30930)                                               
         DC    AL1(50),AL3(47200)                                               
         DC    AL1(50),AL3(17955)                                               
         DC    AL1(50),AL3(31395)                                               
         DC    AL1(50),AL3(87345)          PIL                                  
         DC    AL1(50),AL3(67160)          PI                                   
         DC    AL1(50),AL3(36775)          SE                                   
         DC    AL1(50),AL3(9090)           C3,C6                                
         DC    AL1(50),AL3(17930)          C9                                   
*                                                                               
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(56710)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(41515)                                                       
         DC    AL4(36755)                                                       
         DC    AL4(30395)                                                       
         DC    AL4(24050)                                                       
         DC    AL4(20870)                                                       
         DC    AL4(17020)                                                       
         DC    AL4(30930)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(30930)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(87345)          PIL                                          
         DC    AL4(67160)          PI                                           
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
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
         DC    AL4(56710)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(41515)                                                       
         DC    AL4(36755)                                                       
         DC    AL4(30395)                                                       
*                                                                               
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(56710)                                               
         DC    AL1(50),AL3(42640)                                               
         DC    AL1(50),AL3(41515)                                               
         DC    AL1(50),AL3(36755)                                               
         DC    AL1(50),AL3(30395)                                               
         DC    AL1(50),AL3(24050)                                               
         DC    AL1(50),AL3(20870)                                               
         DC    AL1(50),AL3(17020)                                               
         DC    AL1(50),AL3(30930)                                               
         DC    AL1(50),AL3(47200)                                               
         DC    AL1(50),AL3(17955)                                               
         DC    AL1(50),AL3(31395)                                               
         DC    AL1(50),AL3(87345)  PIL                                          
         DC    AL1(50),AL3(67160)  PI                                           
         DC    AL1(50),AL3(36775)  SE                                           
         DC    AL1(50),AL3(9090)   C3,C6                                        
         DC    AL1(50),AL3(17930)  C9                                           
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(56710)                                              
         DC    AL1(200),AL3(42640)                                              
         DC    AL1(200),AL3(41515)                                              
         DC    AL1(200),AL3(36755)                                              
         DC    AL1(200),AL3(30395)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(17955)                                              
         DC    AL1(200),AL3(31395)                                              
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV  (3X SESSION)                
         DC    AL4(170130)             PRINCIPAL ON  CAMERA                     
         DC    AL4(127920)                 "     OFF   "                        
         DC    AL4(124545)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(110265)               "    6-8    "                          
         DC    AL4(91185)                "     9+    "                          
         DC    AL4(72150)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(62610)                "    6-8    "                          
         DC    AL4(51060)                "     9+    "                          
         DC    AL4(92790)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(141600)             HAND MODEL UNLIMITED                     
         DC    AL4(53865)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(94185)              HAND MODEL 13 WEEKS                      
         DC    AL4(262035)   PIL       PILOT LOCATION RATE                      
         DC    AL4(201480)   PI        PILOT STUDIO RATE                        
         DC    AL4(110325)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(27270)    C3,C6     CONTRACTORS                              
         DC    AL4(53790)    C9             "                                   
*                                                                               
         DC    AL2(77,44,1,255,0,0)    INTERNET RADIO (3X SESSION)              
         DC    AL4(74850)          ANN ALONE                                    
         DC    AL4(74850)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(55140)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(48795)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(43290)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(57555)          SE                                           
         DC    AL4(25695)          C3,C6                                        
         DC    AL4(41100)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    INTERNET TV (3X SESSION)                 
         DC    AL4(170130)             PRINCIPAL ON  CAMERA                     
         DC    AL4(127920)                 "     OFF   "                        
         DC    AL4(124545)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(110265)               "    6-8    "                          
         DC    AL4(91185)                "     9+    "                          
         DC    AL4(72150)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(62610)                "    6-8    "                          
         DC    AL4(51060)                "     9+    "                          
         DC    AL4(92790)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(141600)             HAND MODEL UNLIMITED                     
         DC    AL4(53865)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(94185)              HAND MODEL 13 WEEKS                      
         DC    AL4(262035)   PIL       PILOT LOCATION RATE                      
         DC    AL4(201480)   PI        PILOT STUDIO RATE                        
         DC    AL4(110325)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(27270)    C3,C6     CONTRACTORS                              
         DC    AL4(53790)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(74850)          ANN ALONE                                    
         DC    AL4(74850)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(55140)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(48795)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(43290)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(57555)          SE                                           
         DC    AL4(25695)          C3,C6                                        
         DC    AL4(41100)          C9                                           
         EJECT                                                                  
*              CABLE RATES (YEAR 2006)                                          
*                                                                               
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(56710)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(41515)                                                       
         DC    AL4(36755)                                                       
         DC    AL4(30395)                                                       
         DC    AL4(24050)                                                       
         DC    AL4(20870)                                                       
         DC    AL4(17020)                                                       
*                                                                               
         DC    AL2(41,12,2,62)     MINIMUM COVERS UPTO 62                       
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,63,63)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(533)                                                         
         DC    AL4(0)                                                           
         DC    AL4(444)                                                         
         DC    AL4(283)                                                         
         DC    AL4(271)                                                         
         DC    AL4(196)                                                         
         DC    AL4(104)                                                         
         DC    AL4(189)                                                         
*                                                                               
         DC    AL2(41,44,64,71)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(811)                                                         
         DC    AL4(0)                                                           
         DC    AL4(593)                                                         
         DC    AL4(526)                                                         
         DC    AL4(432)                                                         
         DC    AL4(342)                                                         
         DC    AL4(298)                                                         
         DC    AL4(243)                                                         
*                                                                               
         DC    AL2(41,44,72,72)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(811)                                                         
         DC    AL4(212)                                                         
         DC    AL4(593)                                                         
         DC    AL4(526)                                                         
         DC    AL4(432)                                                         
         DC    AL4(342)                                                         
         DC    AL4(298)                                                         
         DC    AL4(243)                                                         
*                                                                               
         DC    AL2(41,44,73,100)                                                
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(811)                                                         
         DC    AL4(541)                                                         
         DC    AL4(593)                                                         
         DC    AL4(526)                                                         
         DC    AL4(432)                                                         
         DC    AL4(342)                                                         
         DC    AL4(298)                                                         
         DC    AL4(243)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(689)                                                         
         DC    AL4(458)                                                         
         DC    AL4(505)                                                         
         DC    AL4(445)                                                         
         DC    AL4(370)                                                         
         DC    AL4(293)                                                         
         DC    AL4(253)                                                         
         DC    AL4(206)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(566)                                                         
         DC    AL4(377)                                                         
         DC    AL4(416)                                                         
         DC    AL4(368)                                                         
         DC    AL4(305)                                                         
         DC    AL4(242)                                                         
         DC    AL4(208)                                                         
         DC    AL4(170)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(71)                                                          
         DC    AL4(47)                                                          
         DC    AL4(53)                                                          
         DC    AL4(47)                                                          
         DC    AL4(39)                                                          
         DC    AL4(31)                                                          
         DC    AL4(27)                                                          
         DC    AL4(20)                                                          
*                                                                               
         DC    AL2(41,44,1001,2000)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(67)                                                          
         DC    AL4(46)                                                          
         DC    AL4(49)                                                          
         DC    AL4(43)                                                          
         DC    AL4(36)                                                          
         DC    AL4(29)                                                          
         DC    AL4(24)                                                          
         DC    AL4(20)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
*                                                                               
         DC    AL2(42,84,1,4,0,0)  DEM (TV)                                     
         DC    AL4(42535)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(21320)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(31140)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(27565)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(22795)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(10775)           'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(10775)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(10775)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(17955)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(31395)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(16480)          'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(42,84,5,255,0,0)  DEM (TV)                                   
         DC    AL4(42535)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(21320)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(31140)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(27565)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(22795)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(2694)            'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(2694)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(2694)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(17955)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(31395)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(4120)           'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(43,48,1,4,0,0)  DEM (AFT RADIO)                              
         DC    AL4(17195)          ANN ALONE                                    
         DC    AL4(17195)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(11345)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11345)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11345)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(19185)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(17350)          SOLOS AND DUOS                               
*                                                                               
         DC    AL2(43,48,5,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(17195)          ANN ALONE                                    
         DC    AL4(17195)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(2836)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(2836)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(2836)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(19185)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(4338)           SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(27115)                                                       
         DC    AL4(27115)                                                       
         DC    AL4(27115)                                                       
         DC    AL4(27115)                                                       
         DC    AL4(27115)                                                       
         DC    AL4(10675)                                                       
         EJECT                                                                  
*              FOREIGN REUSE                                                    
*                                                                               
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(170130)         (3 X 50000)                                  
         DC    AL4(127920)                                                      
         DC    AL4(124545)                                                      
         DC    AL4(110265)                                                      
         DC    AL4(91185)                                                       
         DC    AL4(72150)                                                       
         DC    AL4(62610)                                                       
         DC    AL4(51060)                                                       
         DC    AL4(92790)                                                       
         DC    AL4(141600)                                                      
         DC    AL4(53865)                                                       
         DC    AL4(94185)                                                       
         DC    AL4(262035)         PIL                                          
         DC    AL4(201480)         PI                                           
         DC    AL4(110325)         SE                                           
         DC    AL4(27270)          C3,C6                                        
         DC    AL4(51990)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(113420)                                                      
         DC    AL4(85280)                                                       
         DC    AL4(83030)                                                       
         DC    AL4(73510)                                                       
         DC    AL4(60790)                                                       
         DC    AL4(48100)                                                       
         DC    AL4(41740)                                                       
         DC    AL4(34040)                                                       
         DC    AL4(61860)                                                       
         DC    AL4(94400)                                                       
         DC    AL4(35910)                                                       
         DC    AL4(62790)                                                       
         DC    AL4(174690)          PIL                                         
         DC    AL4(134320)          PI                                          
         DC    AL4(73550)           SE                                          
         DC    AL4(18180)           C3,C6                                       
         DC    AL4(35860)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 8X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(453680)        (8 X 56710)                                   
         DC    AL4(341120)                                                      
         DC    AL4(332120)                                                      
         DC    AL4(294040)                                                      
         DC    AL4(243160)                                                      
         DC    AL4(192400)                                                      
         DC    AL4(166960)                                                      
         DC    AL4(136160)                                                      
         DC    AL4(247440)                                                      
         DC    AL4(377600)                                                      
         DC    AL4(143640)                                                      
         DC    AL4(251160)                                                      
         DC    AL4(698790)         PIL                                          
         DC    AL4(537280)         PI                                           
         DC    AL4(294200)         SE                                           
         DC    AL4(72720)          C3,C6                                        
         DC    AL4(143440)         C9                                           
*                                                                               
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(49495)          N/D                                          
         DC    AL4(49495)          P,ANN,S,D,ACR                                
         DC    AL4(28710)          3-5 GROUP                                    
         DC    AL4(19800)          6-8 GROUP                                    
         DC    AL4(15835)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(56445)          P,ANN,ACR                                    
         DC    AL4(58615)          S,D                                          
         DC    AL4(38215)          3-5 GROUP                                    
         DC    AL4(30570)          6-8 GROUP                                    
         DC    AL4(19115)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
*                                                                               
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK                                      
         DC    AL4(204790)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(154005)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(149920)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(132700)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(109760)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(87000)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(75365)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(61445)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(517)                                                         
         DC    AL4(375)                                                         
         DC    AL4(367)                                                         
         DC    AL4(339)                                                         
         DC    AL4(263)                                                         
         DC    AL4(217)                                                         
         DC    AL4(196)                                                         
         DC    AL4(140)                                                         
         EJECT                                                                  
*              ADDENDUM USES                                                    
*                                                                               
ADTTBL   DC    AL2(1000,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA              
         DC    AL4(34500)          ON CAMERA                                    
         DC    AL4(26000)          OFF                                          
         DC    AL4(18900)          ON CAMERA GROUPS 3-5                         
         DC    AL4(16730)                           6-8                         
         DC    AL4(13860)                           9+                          
         DC    AL4(10920)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(9520)                             6-8                        
         DC    AL4(7770)                             9+                         
         DC    AL4(18500)          EXTRA UNLIMITED                              
         DC    AL4(28100)          HAND MODEL UNLIMITED                         
         DC    AL4(18500)          EXTRA INITIAL 13WK                           
         DC    AL4(28100)          HAND MODEL INITIAL 13WK                      
         DC    AL4(34500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(25830)          SOLO/DUO ON CAM                              
         DC    AL4(19390)          SOLO/DUO OFF CAM                             
**                                                                              
         DC    AL2(1020,88,0,0,0,0)  1 WEEK - GA                                
         DC    AL4(37000)          ON CAMERA                                    
         DC    AL4(27800)          OFF                                          
         DC    AL4(20250)                                                       
         DC    AL4(17925)                                                       
         DC    AL4(14850)                                                       
         DC    AL4(11700)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(8325)                                                        
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)                                                       
         DC    AL4(9090)                                                        
         DC    AL4(17930)                                                       
         DC    AL4(27675)          SOLO/DUO ON CAM                              
         DC    AL4(20775)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1021,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS             
         DC    AL4(29035)          ON CAMERA                                    
         DC    AL4(21810)          OFF                                          
         DC    AL4(22400)          ON CAMERA GROUPS 3-5                         
         DC    AL4(19200)                           6-8                         
         DC    AL4(15290)                           9+                          
         DC    AL4(9720)           OFF CAMERA GROUPS 3-5                        
         DC    AL4(7940)                             6-8                        
         DC    AL4(5685)                             9+                         
         DC    AL4(18750)          EXTRA UNLIMITED                              
         DC    AL4(25125)          HAND MODEL UNLIMITED                         
         DC    AL4(10585)          EXTRA INITIAL 13WK                           
         DC    AL4(16710)          HAND MODEL INITIAL 13WK                      
         DC    AL4(29035)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1023,80,0,0,0,0)  TV SESSION - 2 WK - NW MULTI MKT           
         DC    AL4(37100)          ON CAMERA                                    
         DC    AL4(27900)          OFF                                          
         DC    AL4(27100)                                                       
         DC    AL4(27100)                                                       
         DC    AL4(27100)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(37100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1023,80,0,0)      TV SESSION - 2 WK - NW SINGLE MKT          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(33700)          ON CAMERA                                    
         DC    AL4(23600)          OFF                                          
         DC    AL4(20600)                                                       
         DC    AL4(20600)                                                       
         DC    AL4(20600)                                                       
         DC    AL4(14200)                                                       
         DC    AL4(14200)                                                       
         DC    AL4(14200)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(33700)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1024,88,0,0,0,0)  1 WEEK - SG                                
         DC    AL4(16960)          ON CAMERA                                    
         DC    AL4(13780)          OFF                                          
         DC    AL4(38801)          ON CAMERA GROUPS 3-5                         
         DC    AL4(38801)                           6-8                         
         DC    AL4(38801)                           9+                          
         DC    AL4(22477)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(22477)                            6-8                        
         DC    AL4(22477)                            9+                         
         DC    AL4(27348)          EXTRA UNLIMITED                              
         DC    AL4(28100)          HAND MODEL UNLIMITED                         
         DC    AL4(16430)          EXTRA INITIAL 13WK                           
         DC    AL4(28100)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(16960)          SOLO/DUO ON CAM                              
         DC    AL4(13780)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1025,80,0,0,0,0)  1 WEEK - CP                                
         DC    AL4(25520)          ON CAMERA                                    
         DC    AL4(19187)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(23198)          EXTRA UNLIMITED                              
         DC    AL4(35402)          HAND MODEL UNLIMITED                         
         DC    AL4(13467)          EXTRA INITIAL 13WK                           
         DC    AL4(23548)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1028,80,0,0,0,0)  1 WEEK - NO                                
         DC    AL4(17000)          ON CAMERA                                    
         DC    AL4(15000)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(16000)          EXTRA UNLIMITED                              
         DC    AL4(30000)          HAND MODEL UNLIMITED                         
         DC    AL4(9000)           EXTRA INITIAL 13WK                           
         DC    AL4(9600)           HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1029,80,0,0,0,0)  2 WEEK - SL                                
         DC    AL4(28400)          ON CAMERA                                    
         DC    AL4(18200)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(16900)          EXTRA UNLIMITED                              
         DC    AL4(36800)          HAND MODEL UNLIMITED                         
         DC    AL4(10200)          EXTRA INITIAL 13WK                           
         DC    AL4(12700)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1040,88,0,0,0,0)  4 WEEK - GA                                
         DC    AL4(39400)          ON CAMERA                                    
         DC    AL4(29700)          OFF                                          
         DC    AL4(21600)                                                       
         DC    AL4(19120)                                                       
         DC    AL4(15840)                                                       
         DC    AL4(12480)                                                       
         DC    AL4(10880)                                                       
         DC    AL4(8880)                                                        
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(39400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)                                                       
         DC    AL4(9090)                                                        
         DC    AL4(17930)                                                       
         DC    AL4(29520)          SOLO/DUO ON CAM                              
         DC    AL4(22160)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1041,80,0,0,0,0)  31 DAY - KS                                
         DC    AL4(37215)          ON CAMERA                                    
         DC    AL4(27970)          OFF                                          
         DC    AL4(27735)                                                       
         DC    AL4(23585)                                                       
         DC    AL4(18725)                                                       
         DC    AL4(12210)                                                       
         DC    AL4(9605)                                                        
         DC    AL4(7230)                                                        
         DC    AL4(18750)                                                       
         DC    AL4(25125)                                                       
         DC    AL4(10585)                                                       
         DC    AL4(16710)                                                       
         DC    AL4(37215)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1044,88,0,0,0,0)  4 WEEK - SG                                
         DC    AL4(33920)          ON CAMERA                                    
         DC    AL4(25440)          OFF                                          
         DC    AL4(38801)          ON CAMERA GROUPS 3-5                         
         DC    AL4(38801)                           6-8                         
         DC    AL4(38801)                           9+                          
         DC    AL4(22477)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(22477)                            6-8                        
         DC    AL4(22477)                            9+                         
         DC    AL4(27348)          EXTRA UNLIMITED                              
         DC    AL4(28100)          HAND MODEL UNLIMITED                         
         DC    AL4(16430)          EXTRA INITIAL 13WK                           
         DC    AL4(28100)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(33920)          SOLO/DUO ON CAM                              
         DC    AL4(25440)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1045,80,0,0,0,0)  4 WEEK - CP                                
         DC    AL4(31899)          ON CAMERA                                    
         DC    AL4(23984)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(23198)          EXTRA UNLIMITED                              
         DC    AL4(35402)          HAND MODEL UNLIMITED                         
         DC    AL4(13467)          EXTRA INITIAL 13WK                           
         DC    AL4(23548)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1047,80,0,0,0,0)  4 WEEK - CO                                
         DC    AL4(30104)          ON CAMERA                                    
         DC    AL4(23532)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(17702)          EXTRA UNLIMITED                              
         DC    AL4(23532)          HAND MODEL UNLIMITED                         
         DC    AL4(0)              EXTRA INITIAL 13WK                           
         DC    AL4(0)              HAND MODEL INITIAL 13WK                      
         DC    AL4(0)                                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              SE                                           
         DC    AL4(0)              C3,C6                                        
         DC    AL4(0)              C9                                           
*                                                                               
         DC    AL2(1049,80,0,0,0,0)  4 WEEK - SL                                
         DC    AL4(35300)          ON CAMERA                                    
         DC    AL4(23800)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(16900)          EXTRA UNLIMITED                              
         DC    AL4(36800)          HAND MODEL UNLIMITED                         
         DC    AL4(10200)          EXTRA INITIAL 13WK                           
         DC    AL4(12700)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1060,88,0,0,0,0)  13 WEEK - GA                               
         DC    AL4(49300)          ON CAMERA                                    
         DC    AL4(37100)          OFF                                          
         DC    AL4(27000)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(15600)                                                       
         DC    AL4(13600)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(18500)                                                       
         DC    AL4(28100)                                                       
         DC    AL4(49300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)                                                       
         DC    AL4(9090)                                                        
         DC    AL4(17930)                                                       
         DC    AL4(36900)          SOLO/DUO ON CAM                              
         DC    AL4(27700)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1061,80,0,0,0,0)  13 WEEKS - KS                              
         DC    AL4(45400)          ON CAMERA                                    
         DC    AL4(34015)          OFF                                          
         DC    AL4(32830)                                                       
         DC    AL4(27970)                                                       
         DC    AL4(22405)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(11610)                                                       
         DC    AL4(8655)                                                        
         DC    AL4(18750)                                                       
         DC    AL4(25125)                                                       
         DC    AL4(10585)                                                       
         DC    AL4(16710)                                                       
         DC    AL4(45400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1062,80,0,0,0,0)  13 WEEKS - TX                              
         DC    AL4(45370)          ON CAMERA                                    
         DC    AL4(34110)          OFF                                          
         DC    AL4(33210)          ON CAMERA GROUPS 3-5                         
         DC    AL4(33210)                           6-8                         
         DC    AL4(33210)                           9+                          
         DC    AL4(19240)          OFF CAMERA GROUS 3-5                         
         DC    AL4(19240)                           6-8                         
         DC    AL4(19240)                           9+                          
         DC    AL4(24745)          EXTRA UNLIMITED                              
         DC    AL4(37760)          HAND MODEL UNLIMITED                         
         DC    AL4(14365)          EXTRA INITIAL 13WK                           
         DC    AL4(25115)          HAND MODEL INITIAL 13WK                      
         DC    AL4(45370)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1063,80,0,0,0,0)  13 WEEKS - NW MULTI MARKET                 
         DC    AL4(43700)          ON CAMERA                                    
         DC    AL4(32900)          OFF                                          
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(17000)                                                       
         DC    AL4(17000)                                                       
         DC    AL4(43700)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1063,80,0,0)      13 WEEKS - NW SINGLE MARKET                
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(39600)          ON CAMERA                                    
         DC    AL4(27900)          OFF                                          
         DC    AL4(24200)                                                       
         DC    AL4(24200)                                                       
         DC    AL4(24200)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(39600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1064,88,0,0,0,0)  13 WEEK - SG                               
         DC    AL4(47700)          ON CAMERA                                    
         DC    AL4(36040)          OFF                                          
         DC    AL4(38801)          ON CAMERA GROUPS 3-5                         
         DC    AL4(38801)                           6-8                         
         DC    AL4(38801)                           9+                          
         DC    AL4(22477)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(22477)                            6-8                        
         DC    AL4(22477)                            9+                         
         DC    AL4(27348)          EXTRA UNLIMITED                              
         DC    AL4(28100)          HAND MODEL UNLIMITED                         
         DC    AL4(16430)          EXTRA INITIAL 13WK                           
         DC    AL4(28100)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(47700)          SOLO/DUO ON CAM                              
         DC    AL4(36040)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1065,80,0,0,0,0)  13 WEEK - CP                               
         DC    AL4(42533)          ON CAMERA                                    
         DC    AL4(31979)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(23198)          EXTRA UNLIMITED                              
         DC    AL4(35402)          HAND MODEL UNLIMITED                         
         DC    AL4(13467)          EXTRA INITIAL 13WK                           
         DC    AL4(23548)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1066,80,0,0,0,0)  13 WEEK - AZ                               
         DC    AL4(50350)          ON CAMERA                                    
         DC    AL4(36040)          OFF                                          
         DC    AL4(34980)          ON CAMERA GROUPS 3-5                         
         DC    AL4(31058)                           6-8                         
         DC    AL4(25440)                           9+                          
         DC    AL4(23956)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(20670)                            6-8                        
         DC    AL4(17596)                            9+                         
         DC    AL4(0)              EXTRA UNLIMITED                              
         DC    AL4(0)              HAND MODEL UNLIMITED                         
         DC    AL4(0)              EXTRA INITIAL 13WK                           
         DC    AL4(0)              HAND MODEL INITIAL 13WK                      
         DC    AL4(0)                                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1067,80,0,0,0,0)  13 WEEK - CO                               
         DC    AL4(48548)          ON CAMERA                                    
         DC    AL4(36252)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(17702)          EXTRA UNLIMITED                              
         DC    AL4(37524)          HAND MODEL UNLIMITED                         
         DC    AL4(0)              EXTRA INITIAL 13WK                           
         DC    AL4(0)              HAND MODEL INITIAL 13WK                      
         DC    AL4(0)                                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1068,80,0,0,0,0)  13 WEEK - NO                               
         DC    AL4(32500)          ON CAMERA                                    
         DC    AL4(25000)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(16000)          EXTRA UNLIMITED                              
         DC    AL4(30000)          HAND MODEL UNLIMITED                         
         DC    AL4(9000)           EXTRA INITIAL 13WK                           
         DC    AL4(9600)           HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1069,80,0,0,0,0)  13 WEEK - SL                               
         DC    AL4(46300)          ON CAMERA                                    
         DC    AL4(28700)          OFF                                          
         DC    AL4(0)              ON CAMERA GROUPS 3-5                         
         DC    AL4(0)                               6-8                         
         DC    AL4(0)                               9+                          
         DC    AL4(0)              OFF CAMERA GROUPS 3-5                        
         DC    AL4(0)                                6-8                        
         DC    AL4(0)                                9+                         
         DC    AL4(16900)          EXTRA UNLIMITED                              
         DC    AL4(36800)          HAND MODEL UNLIMITED                         
         DC    AL4(10200)          EXTRA INITIAL 13WK                           
         DC    AL4(12700)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1070,80,0,0,0,0)  13 WEEK - TN                               
         DC    AL4(42533)          ON CAMERA                                    
         DC    AL4(31980)          OFF                                          
         DC    AL4(31136)          ON CAMERA GROUPS 3-5                         
         DC    AL4(27566)                           6-8                         
         DC    AL4(22796)                           9+                          
         DC    AL4(18038)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(15653)                            6-8                        
         DC    AL4(12765)                            9+                         
         DC    AL4(0)              EXTRA UNLIMITED                              
         DC    AL4(0)              HAND MODEL UNLIMITED                         
         DC    AL4(0)              EXTRA INITIAL 13WK                           
         DC    AL4(0)              HAND MODEL INITIAL 13WK                      
         DC    AL4(0)                                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
ADOTBL   DC    AL2(1080,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA           
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(8330)                                                        
         DC    AL4(7420)                                                        
         DC    AL4(6580)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(11340)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1100,48,0,0,0,0)  1 WEEK - GA                                
         DC    AL4(16300)                                                       
         DC    AL4(16300)                                                       
         DC    AL4(8925)                                                        
         DC    AL4(7950)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(12150)                                                       
*                                                                               
         DC    AL2(1101,44,0,0,0,0)  1 WEEK - KS                                
         DC    AL4(12350)                                                       
         DC    AL4(12350)                                                       
         DC    AL4(7735)                                                        
         DC    AL4(6485)                                                        
         DC    AL4(5740)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1103,44,0,0,0,0)  2 WEEK - NW MULTLIPLE MARKETS              
         DC    AL4(16700)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1103,44,0,0)      2 WEEK - NW SINGLE MARKET                  
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1105,44,0,0,0,0)  1 WEEK - CP                                
         DC    AL4(11229)                                                       
         DC    AL4(11229)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1108,44,0,0,0,0)  1 WEEK - NO                                
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1109,44,0,0,0,0)  2 WEEK - SL                                
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1120,48,0,0,0,0)  4 WEEK - GA                                
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(9520)                                                        
         DC    AL4(8480)                                                        
         DC    AL4(7520)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(12960)                                                       
*                                                                               
         DC    AL2(1121,44,0,0,0,0)  31 DAY - KS                                
         DC    AL4(15845)                                                       
         DC    AL4(15845)                                                       
         DC    AL4(9360)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(7360)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1125,44,0,0,0,0)  4 WEEK - CP                                
         DC    AL4(14036)                                                       
         DC    AL4(14036)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1129,44,0,0,0,0)  4 WEEK - SL                                
         DC    AL4(16200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1140,48,0,0,0,0)  13 WEEK - GA                               
         DC    AL4(21700)                                                       
         DC    AL4(21700)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(9400)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(16200)                                                       
*                                                                               
         DC    AL2(1141,44,0,0,0,0)  13 WEEK - KS                               
         DC    AL4(19340)                                                       
         DC    AL4(19340)                                                       
         DC    AL4(10855)                                                       
         DC    AL4(9610)                                                        
         DC    AL4(8605)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1142,44,0,0,0,0)  13 WEEK - TX                               
         DC    AL4(19960)                                                       
         DC    AL4(19960)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1143,44,0,0,0,0)  13 WEEK - NW  MULTIPLE MARKETS             
         DC    AL4(19700)                                                       
         DC    AL4(19700)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1143,44,0,0)      13 WEEK - NW  SINGLE MARKET                
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(12800)                                                       
         DC    AL4(12800)                                                       
         DC    AL4(7700)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1144,44,0,0,0,0)  13 WEEK - SG                               
         DC    AL4(20670)                                                       
         DC    AL4(20670)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(15900)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1145,44,0,0,0,0)  13 WEEK - CP                               
         DC    AL4(18714)                                                       
         DC    AL4(18714)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1146,44,0,0,0,0)  13 WEEK - AZ                               
         DC    AL4(23850)                                                       
         DC    AL4(23850)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)              SE                                           
         DC    AL4(0)              C3,C6                                        
         DC    AL4(0)              C9                                           
*                                                                               
         DC    AL2(1148,44,0,0,0,0)  13 WEEK - NO                               
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1149,44,0,0,0,0)  13 WEEK - SL                               
         DC    AL4(18700)                                                       
         DC    AL4(18700)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1150,44,0,0,0,0)  13 WEEK - TN                               
         DC    AL4(18713)                                                       
         DC    AL4(18713)                                                       
         DC    AL4(13785)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)              SE                                           
         DC    AL4(6424)           C3,C6                                        
         DC    AL4(6424)           C9                                           
*                                                                               
ADHTAB   DC    AL2(1540,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                
         DC    AL4(49300)                                                       
         DC    AL4(37100)                                                       
         DC    AL4(27000)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(19800)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(36900)                                                       
         DC    AL4(27700)                                                       
*                                                                               
         DC    AL2(1541,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                
         DC    AL4(45400)           ON CAMERA                                   
         DC    AL4(34015)           OFF                                         
         DC    AL4(32830)                                                       
         DC    AL4(27970)                                                       
         DC    AL4(22405)                                                       
*                                                                               
         DC    AL2(1542,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                
         DC    AL4(45370)           ON CAMERA                                   
         DC    AL4(34110)           OFF                                         
         DC    AL4(33210)                                                       
         DC    AL4(33210)                                                       
         DC    AL4(33210)                                                       
*                                                                               
         DC    AL2(1543,32,0,0,0,0)  ADDENDUM HOLDING RATES - NW                
         DC    AL4(43700)           ON CAMERA   MULTIPLE MARKET                 
         DC    AL4(32900)           OFF                                         
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
*                                                                               
         DC    AL2(1543,32,0,0)      ADDENDUM HOLDING RATES - NW                
         DC    AL1(1,0,0,0)         SINGLE MARKET                               
         DC    AL4(39600)           ON CAMERA                                   
         DC    AL4(27900)           OFF                                         
         DC    AL4(24200)                                                       
         DC    AL4(24200)                                                       
         DC    AL4(24200)                                                       
*                                                                               
         DC    AL2(1544,32,0,0,0,0)  ADDENDUM HOLDING RATES - SG                
         DC    AL4(47700)           ON CAMERA                                   
         DC    AL4(36040)           OFF                                         
         DC    AL4(38801)                                                       
         DC    AL4(38801)                                                       
         DC    AL4(38801)                                                       
*                                                                               
         DC    AL2(1545,32,0,0,0,0)  ADDENDUM HOLDING RATES - CP                
         DC    AL4(42533)           ON CAMERA                                   
         DC    AL4(31979)           OFF                                         
         DC    AL4(38801)                                                       
         DC    AL4(38801)                                                       
         DC    AL4(38801)                                                       
*                                                                               
ARNTAB   DC    AL2(1560,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(49300)                                              
         DC    AL1(200),AL3(37100)                                              
         DC    AL1(200),AL3(27000)                                              
         DC    AL1(200),AL3(23900)                                              
         DC    AL1(200),AL3(19800)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(36900)                                              
         DC    AL1(200),AL3(27700)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(1561,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES               
         DC    AL1(200),AL3(45400)             ON CAMERA                        
         DC    AL1(200),AL3(34015)             OFF                              
         DC    AL1(200),AL3(32830)                                              
         DC    AL1(200),AL3(27970)                                              
         DC    AL1(200),AL3(22405)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(1562,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES               
         DC    AL1(200),AL3(45370)             ON CAMERA                        
         DC    AL1(200),AL3(34110)             OFF                              
         DC    AL1(200),AL3(33210)                                              
         DC    AL1(200),AL3(33210)                                              
         DC    AL1(200),AL3(33210)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(1563,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES               
         DC    AL1(200),AL3(43700)             ON CAMERA  MULTIPLE MKTS         
         DC    AL1(200),AL3(32900)             OFF                              
         DC    AL1(200),AL3(32000)                                              
         DC    AL1(200),AL3(32000)                                              
         DC    AL1(200),AL3(32000)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(1563,32,0,0)       - 2X ADDENDUM HOLDING RATES               
         DC    AL1(1,0,0,0)          SINGLE MARKET                              
         DC    AL1(200),AL3(39600)             ON CAMERA                        
         DC    AL1(200),AL3(27900)             OFF                              
         DC    AL1(200),AL3(24200)                                              
         DC    AL1(200),AL3(24200)                                              
         DC    AL1(200),AL3(24200)                                              
*                                    ADDENDUM REINSTATEMENT - SG                
         DC    AL2(1564,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES               
         DC    AL1(200),AL3(47700)             ON CAMERA                        
         DC    AL1(200),AL3(36040)             OFF                              
         DC    AL1(200),AL3(38801)                                              
         DC    AL1(200),AL3(38801)                                              
         DC    AL1(200),AL3(38801)                                              
*                                    ADDENDUM REINSTATEMENT - CP                
         DC    AL2(1565,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES               
         DC    AL1(200),AL3(42533)             ON CAMERA                        
         DC    AL1(200),AL3(31979)             OFF                              
         DC    AL1(200),AL3(38801)                                              
         DC    AL1(200),AL3(38801)                                              
         DC    AL1(200),AL3(38801)                                              
*                                                                               
ADDTAB   DC    AL2(1160,80,0,0,0,0)  ADDENDUM DEMO (TV) - GA                    
         DC    AL4(37100)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(18700)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(37100)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(37100)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(37100)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(18700)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(18700)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(18700)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1161,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                    
         DC    AL4(10190)          'ON'                                         
         DC    AL4(9005)           'OFF'                                        
         DC    AL4(10190)                                                       
         DC    AL4(10190)                                                       
         DC    AL4(10190)                                                       
         DC    AL4(9005)                                                        
         DC    AL4(9005)                                                        
         DC    AL4(9005)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1162,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                    
         DC    AL4(34030)          'ON'                                         
         DC    AL4(17055)          'OFF'                                        
         DC    AL4(24910)                                                       
         DC    AL4(24910)                                                       
         DC    AL4(24910)                                                       
         DC    AL4(8620)                                                        
         DC    AL4(8620)                                                        
         DC    AL4(8620)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
         DC    AL4(34030)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(17055)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(1163,80,0,0,0,0)  ADDENDUM DEMO (TV) - NW                    
         DC    AL4(32800)          'ON'                                         
         DC    AL4(16400)          'OFF'                                        
         DC    AL4(24000)                                                       
         DC    AL4(24000)                                                       
         DC    AL4(24000)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(9300)           EXTRA                                        
         DC    4AL4(0)             N/D                                          
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(1180,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA             
         DC    AL4(14900)          ANN ALONE                                    
         DC    AL4(14900)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(14900)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(14900)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(14900)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1181,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS             
         DC    AL4(8235)           ANN ALONE                                    
         DC    AL4(8235)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8235)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8235)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8235)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(1182,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX             
         DC    AL4(13755)          ANN ALONE                                    
         DC    AL4(13755)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(9080)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(9080)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(9080)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(13755)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1183,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - NW             
         DC    AL4(8300)          ANN ALONE                                     
         DC    AL4(8300)          AR,AS,P,ANN,1-4MS,1-4SS                       
         DC    AL4(5500)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(5500)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5500)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
*                                                                               
ADWTAB   DC    AL2(1200,52,1,1,0,0)  3 DAY - GA - UNIT 1                        
         DC    AL4(34500)          ON CAMERA                                    
         DC    AL4(26000)          OFF                                          
         DC    AL4(18900)                                                       
         DC    AL4(16730)                                                       
         DC    AL4(13860)                                                       
         DC    AL4(10920)                                                       
         DC    AL4(9520)                                                        
         DC    AL4(7770)                                                        
         DC    AL4(25830)          SOLO/DUO ON CAMERA                           
         DC    AL4(19390)          OFF CAMERA                                   
*                                                                               
         DC    AL2(1200,52,2,25,0,0)  UNITS 2-25                                
         DC    AL4(1181)                                                        
         DC    AL4(809)                                                         
         DC    AL4(688)                                                         
         DC    AL4(594)                                                         
         DC    AL4(485)                                                         
         DC    AL4(244)                                                         
         DC    AL4(192)                                                         
         DC    AL4(160)                                                         
         DC    AL4(883)                                                         
         DC    AL4(604)                                                         
*                                                                               
         DC    AL2(1200,52,26,60,0,0)  UNITS 26-60                              
         DC    AL4(439)                                                         
         DC    AL4(344)                                                         
         DC    AL4(355)                                                         
         DC    AL4(301)                                                         
         DC    AL4(249)                                                         
         DC    AL4(103)                                                         
         DC    AL4(71)                                                          
         DC    AL4(64)                                                          
         DC    AL4(328)                                                         
         DC    AL4(257)                                                         
*                                                                               
         DC    AL2(1200,52,61,255,0,0)  UNITS 61+                               
         DC    AL4(439)                                                         
         DC    AL4(344)                                                         
         DC    AL4(257)                                                         
         DC    AL4(201)                                                         
         DC    AL4(168)                                                         
         DC    AL4(62)                                                          
         DC    AL4(36)                                                          
         DC    AL4(36)                                                          
         DC    AL4(328)                                                         
         DC    AL4(257)                                                         
*                                                                               
         DC    AL2(1220,52,1,1,0,0)  1 WEEK - GA - UNIT 1                       
         DC    AL4(37000)          ON CAMERA                                    
         DC    AL4(27800)          OFF                                          
         DC    AL4(20250)                                                       
         DC    AL4(17925)                                                       
         DC    AL4(14850)                                                       
         DC    AL4(11700)                                                       
         DC    AL4(10200)                                                       
         DC    AL4(8325)                                                        
         DC    AL4(27675)                                                       
         DC    AL4(20775)                                                       
*                                                                               
         DC    AL2(1220,52,2,25,0,0)  UNITS 2-25                                
         DC    AL4(1266)                                                        
         DC    AL4(865)                                                         
         DC    AL4(737)                                                         
         DC    AL4(636)                                                         
         DC    AL4(520)                                                         
         DC    AL4(261)                                                         
         DC    AL4(206)                                                         
         DC    AL4(172)                                                         
         DC    AL4(946)                                                         
         DC    AL4(647)                                                         
*                                                                               
         DC    AL2(1220,52,26,60,0,0)  UNITS 26-60                              
         DC    AL4(470)                                                         
         DC    AL4(369)                                                         
         DC    AL4(380)                                                         
         DC    AL4(322)                                                         
         DC    AL4(267)                                                         
         DC    AL4(110)                                                         
         DC    AL4(76)                                                          
         DC    AL4(69)                                                          
         DC    AL4(351)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(1220,52,61,255,0,0)  UNITS 61+                               
         DC    AL4(470)                                                         
         DC    AL4(369)                                                         
         DC    AL4(275)                                                         
         DC    AL4(215)                                                         
         DC    AL4(180)                                                         
         DC    AL4(67)                                                          
         DC    AL4(38)                                                          
         DC    AL4(38)                                                          
         DC    AL4(351)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(1221,44,1,1,0,0)  1 WEEK - KS - UNIT 1                       
         DC    AL4(29035)          ON CAMERA                                    
         DC    AL4(21810)          OFF                                          
         DC    AL4(22400)                                                       
         DC    AL4(19200)                                                       
         DC    AL4(15290)                                                       
         DC    AL4(9720)                                                        
         DC    AL4(7940)                                                        
         DC    AL4(5685)                                                        
*                                                                               
         DC    AL2(1221,44,2,255,0,0)  UNITS 2+                                 
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(1223,44,1,1,0,0)  2 WEEK - NW - UNIT 1                       
         DC    AL4(37100)          ON CAMERA                                    
         DC    AL4(27900)          OFF                                          
         DC    AL4(27100)                                                       
         DC    AL4(27100)                                                       
         DC    AL4(27100)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
*                                                                               
         DC    AL2(1223,44,2,255,0,0)  UNITS 2+                                 
         DC    AL4(1941)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1328)                                                        
*                                                                               
         DC    AL2(1240,52,1,1,0,0)  4 WEEK - GA - UNIT 1                       
         DC    AL4(39400)          ON CAMERA                                    
         DC    AL4(29700)          OFF                                          
         DC    AL4(21600)                                                       
         DC    AL4(19120)                                                       
         DC    AL4(15840)                                                       
         DC    AL4(12480)                                                       
         DC    AL4(10880)                                                       
         DC    AL4(8880)                                                        
         DC    AL4(29520)          SOLO/DUO ON CAMERA                           
         DC    AL4(22160)          OFF CAMERA                                   
*                                                                               
         DC    AL2(1240,52,2,25,0,0)  UNITS 2-25                                
         DC    AL4(1350)                                                        
         DC    AL4(924)                                                         
         DC    AL4(786)                                                         
         DC    AL4(678)                                                         
         DC    AL4(554)                                                         
         DC    AL4(278)                                                         
         DC    AL4(220)                                                         
         DC    AL4(183)                                                         
         DC    AL4(1010)                                                        
         DC    AL4(690)                                                         
*                                                                               
         DC    AL2(1240,52,26,60,0,0)  UNITS 26-60                              
         DC    AL4(501)                                                         
         DC    AL4(393)                                                         
         DC    AL4(406)                                                         
         DC    AL4(344)                                                         
         DC    AL4(285)                                                         
         DC    AL4(118)                                                         
         DC    AL4(81)                                                          
         DC    AL4(74)                                                          
         DC    AL4(374)                                                         
         DC    AL4(294)                                                         
*                                                                               
         DC    AL2(1240,52,61,255,0,0)  UNITS 61+                               
         DC    AL4(501)                                                         
         DC    AL4(393)                                                         
         DC    AL4(294)                                                         
         DC    AL4(230)                                                         
         DC    AL4(192)                                                         
         DC    AL4(71)                                                          
         DC    AL4(41)                                                          
         DC    AL4(41)                                                          
         DC    AL4(374)                                                         
         DC    AL4(294)                                                         
*                                                                               
         DC    AL2(1241,44,1,1,0,0)  31 DAY - KS - UNIT 1                       
         DC    AL4(37215)          ON CAMERA                                    
         DC    AL4(27970)          OFF                                          
         DC    AL4(27735)                                                       
         DC    AL4(23585)                                                       
         DC    AL4(18725)                                                       
         DC    AL4(12210)                                                       
         DC    AL4(9605)                                                        
         DC    AL4(7230)                                                        
*                                                                               
         DC    AL2(1241,44,2,255,0,0)  UNITS 2+                                 
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(1260,52,1,1,0,0)  13 WEEK - GA - UNIT 1                      
         DC    AL4(49300)          ON CAMERA                                    
         DC    AL4(37100)          OFF                                          
         DC    AL4(27000)                                                       
         DC    AL4(23900)                                                       
         DC    AL4(19800)                                                       
         DC    AL4(15600)                                                       
         DC    AL4(13600)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(36900)          SOLO/DUO ON CAMERA                           
         DC    AL4(27700)          OFF                                          
*                                                                               
         DC    AL2(1260,52,2,25,0,0)  UNITS 2-25                                
         DC    AL4(1688)                                                        
         DC    AL4(1155)                                                        
         DC    AL4(983)                                                         
         DC    AL4(848)                                                         
         DC    AL4(693)                                                         
         DC    AL4(348)                                                         
         DC    AL4(275)                                                         
         DC    AL4(229)                                                         
         DC    AL4(1262)                                                        
         DC    AL4(863)                                                         
*                                                                               
         DC    AL2(1260,52,26,60,0,0)  UNITS 26-60                              
         DC    AL4(626)                                                         
         DC    AL4(513)                                                         
         DC    AL4(507)                                                         
         DC    AL4(430)                                                         
         DC    AL4(356)                                                         
         DC    AL4(147)                                                         
         DC    AL4(101)                                                         
         DC    AL4(92)                                                          
         DC    AL4(468)                                                         
         DC    AL4(367)                                                         
*                                                                               
         DC    AL2(1260,52,61,255,0,0)  UNITS 61+                               
         DC    AL4(626)                                                         
         DC    AL4(513)                                                         
         DC    AL4(367)                                                         
         DC    AL4(287)                                                         
         DC    AL4(240)                                                         
         DC    AL4(89)                                                          
         DC    AL4(51)                                                          
         DC    AL4(51)                                                          
         DC    AL4(468)                                                         
         DC    AL4(367)                                                         
*                                                                               
         DC    AL2(1261,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                     
         DC    AL4(45400)          ON CAMERA                                    
         DC    AL4(34015)          OFF                                          
         DC    AL4(32830)                                                       
         DC    AL4(27970)                                                       
         DC    AL4(22405)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(11610)                                                       
         DC    AL4(8655)                                                        
*                                                                               
         DC    AL2(1261,44,2,255,0,0)  UNITS 2+                                 
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(1262,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                     
         DC    AL4(45370)           ON CAMERA                                   
         DC    AL4(34110)           OFF                                         
         DC    AL4(33210)                                                       
         DC    AL4(33210)                                                       
         DC    AL4(33210)                                                       
         DC    AL4(19240)                                                       
         DC    AL4(19240)                                                       
         DC    AL4(19240)                                                       
*                                                                               
         DC    AL2(1262,44,2,25,0,0)  UNITS 2-25                                
         DC    AL4(1553)                                                        
         DC    AL4(1062)                                                        
         DC    AL4(1210)                                                        
         DC    AL4(1210)                                                        
         DC    AL4(1210)                                                        
         DC    AL4(429)                                                         
         DC    AL4(429)                                                         
         DC    AL4(429)                                                         
*                                                                               
         DC    AL2(1262,44,26,255,0,0)  UNITS 26+                               
         DC    AL4(576)                                                         
         DC    AL4(452)                                                         
         DC    AL4(624)                                                         
         DC    AL4(624)                                                         
         DC    AL4(624)                                                         
         DC    AL4(181)                                                         
         DC    AL4(181)                                                         
         DC    AL4(181)                                                         
*                                                                               
         DC    AL2(1263,44,1,1,0,0)  13 WEEKS - NW - UNIT 1                     
         DC    AL4(43700)          ON CAMERA                                    
         DC    AL4(32900)          OFF                                          
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
         DC    AL4(32000)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
*                                                                               
         DC    AL2(1263,44,2,255,0,0)  UNITS 2+                                 
         DC    AL4(1941)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1941)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1328)                                                        
         DC    AL4(1328)                                                        
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
*                                                                               
         DC    AL2(1280,40,1,1,0,0)  3 DAY - GA - UNIT 1                        
         DC    AL4(15300)          ANN ALONE                                    
         DC    AL4(15300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(8330)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7420)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6580)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(11340)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1280,40,2,25,0,0)  UNITS 2-25                                
         DC    AL4(224)                                                         
         DC    AL4(224)                                                         
         DC    AL4(87)                                                          
         DC    AL4(74)                                                          
         DC    AL4(66)                                                          
         DC    AL4(0)                                                           
         DC    AL4(167)                                                         
*                                                                               
         DC    AL2(1280,40,26,60,0,0)  UNITS 26-60                              
         DC    AL4(167)                                                         
         DC    AL4(167)                                                         
         DC    AL4(74)                                                          
         DC    AL4(57)                                                          
         DC    AL4(57)                                                          
         DC    AL4(0)                                                           
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(1280,40,61,255,0,0)  UNITS 61+                               
         DC    AL4(167)                                                         
         DC    AL4(167)                                                         
         DC    AL4(42)                                                          
         DC    AL4(36)                                                          
         DC    AL4(36)                                                          
         DC    AL4(0)                                                           
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(1300,40,1,1,0,0)  1 WEEK - GA - UNIT 1                       
         DC    AL4(16300)          ANN ALONE                                    
         DC    AL4(16300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(8925)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7950)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7050)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(16300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(12150)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1300,40,2,25,0,0)  UNITS 2-25                                
         DC    AL4(240)                                                         
         DC    AL4(240)                                                         
         DC    AL4(93)                                                          
         DC    AL4(79)                                                          
         DC    AL4(70)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(1300,40,26,60,0,0)  UNITS 26-60                              
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(79)                                                          
         DC    AL4(61)                                                          
         DC    AL4(61)                                                          
         DC    AL4(0)                                                           
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(1300,40,61,255,0,0)  UNITS 61+                               
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(45)                                                          
         DC    AL4(39)                                                          
         DC    AL4(39)                                                          
         DC    AL4(0)                                                           
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(1301,36,1,1,0,0)  1 WEEK - KS - UNIT 1                       
         DC    AL4(12350)                                                       
         DC    AL4(12350)                                                       
         DC    AL4(7735)                                                        
         DC    AL4(6485)                                                        
         DC    AL4(5740)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(1301,36,2,255,0,0)  UNITS 2+                                 
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(1303,36,1,1,0,0)  2 WEEK - NW - UNIT 1                       
         DC    AL4(16700)                                                       
         DC    AL4(16700)                                                       
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(16700)          SE                                           
*                                                                               
         DC    AL2(1303,36,2,255,0,0)  UNITS 2+                                 
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
*                                                                               
         DC    AL2(1320,40,1,1,0,0)  4 WEEK - GA - UNIT 1                       
         DC    AL4(17400)          ANN ALONE                                    
         DC    AL4(17400)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(9520)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8480)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7520)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17400)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(12960)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1320,40,2,25,0,0)  UNITS 2-25                                
         DC    AL4(255)                                                         
         DC    AL4(255)                                                         
         DC    AL4(99)                                                          
         DC    AL4(85)                                                          
         DC    AL4(75)                                                          
         DC    AL4(0)                                                           
         DC    AL4(191)                                                         
*                                                                               
         DC    AL2(1320,40,26,60,0,0)  UNITS 26-60                              
         DC    AL4(192)                                                         
         DC    AL4(192)                                                         
         DC    AL4(85)                                                          
         DC    AL4(65)                                                          
         DC    AL4(65)                                                          
         DC    AL4(0)                                                           
         DC    AL4(143)                                                         
*                                                                               
         DC    AL2(1320,40,61,255,0,0)  UNITS 61+                               
         DC    AL4(192)                                                         
         DC    AL4(192)                                                         
         DC    AL4(48)                                                          
         DC    AL4(42)                                                          
         DC    AL4(42)                                                          
         DC    AL4(0)                                                           
         DC    AL4(143)                                                         
*                                                                               
         DC    AL2(1321,36,1,1,0,0)  31 DAY - KS - UNIT 1                       
         DC    AL4(15845)                                                       
         DC    AL4(15845)                                                       
         DC    AL4(9360)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(7360)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(1321,36,2,255,0,0)  UNITS 2+                                 
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(1340,40,1,1,0,0)  13 WEEK - GA - UNIT 1                      
         DC    AL4(21700)          ANN ALONE                                    
         DC    AL4(21700)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(11900)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(10600)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(9400)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(21700)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(16200)          SOLO/DUO                                     
*                                                                               
         DC    AL2(1340,40,2,25,0,0)  UNITS 2-25                                
         DC    AL4(319)                                                         
         DC    AL4(319)                                                         
         DC    AL4(124)                                                         
         DC    AL4(106)                                                         
         DC    AL4(94)                                                          
         DC    AL4(0)                                                           
         DC    AL4(239)                                                         
*                                                                               
         DC    AL2(1340,40,26,60,0,0)  UNITS 26-60                              
         DC    AL4(240)                                                         
         DC    AL4(240)                                                         
         DC    AL4(106)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(1340,40,61,255,0,0)  UNITS 61+                               
         DC    AL4(240)                                                         
         DC    AL4(240)                                                         
         DC    AL4(60)                                                          
         DC    AL4(52)                                                          
         DC    AL4(52)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(1341,36,1,1,0,0)  13 WEEK - KS - UNIT 1                      
         DC    AL4(19340)                                                       
         DC    AL4(19340)                                                       
         DC    AL4(10855)                                                       
         DC    AL4(9610)                                                        
         DC    AL4(8605)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(1341,36,2,255,0,0)  UNITS 2+                                 
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(1342,36,1,1,0,0)  13 WEEK - TX - UNIT 1                      
         DC    AL4(19960)                                                       
         DC    AL4(19960)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(14705)                                                       
         DC    AL4(19960)                                                       
*                                                                               
         DC    AL2(1342,36,2,25,0,0)  UNITS 2-25                                
         DC    AL4(294)                                                         
         DC    AL4(294)                                                         
         DC    AL4(153)                                                         
         DC    AL4(153)                                                         
         DC    AL4(153)                                                         
         DC    AL4(294)                                                         
*                                                                               
         DC    AL2(1342,36,26,255,0,0)  UNITS 26+                               
         DC    AL4(221)                                                         
         DC    AL4(221)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(130)                                                         
         DC    AL4(221)                                                         
*                                                                               
         DC    AL2(1343,36,1,1,0,0)  13 WEEK - NW - UNIT 1                      
         DC    AL4(19700)                                                       
         DC    AL4(19700)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(19700)                                                       
*                                                                               
         DC    AL2(1343,36,2,255,0,0)  UNITS 2+                                 
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         DC    AL4(367)                                                         
         EJECT                                                                  
*              ADDENDUM CABLE                                                   
*                                                                               
         DC    AL2(1362,44,0,0,0,0)  1-50,000 SUBSCRIBERS TX                    
         DC    AL4(2005)            PRINCIPAL ON CAMERA                         
         DC    AL4(1370)              "     OFF CAMERA                          
         DC    AL4(1570)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1350)              "   6-8 "    "                            
         DC    AL4(1100)              "    9+ "    "                            
         DC    AL4(560)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(430)               "   6-8  "    "                           
         DC    AL4(365)               "    9+  "    "                           
*                                                                               
         DC    AL2(1363,44,0,0,0,0)  1-50,000 SUBSCRIBERS NW                    
         DC    AL4(605)             PRINCIPAL ON CAMERA                         
         DC    AL4(425)               "     OFF CAMERA                          
         DC    AL4(475)             GROUP 3-5 ON CAMERA                         
         DC    AL4(415)               "   6-8 "    "                            
         DC    AL4(335)               "    9+ "    "                            
         DC    AL4(175)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(140)               "   6-8  "    "                           
         DC    AL4(115)               "    9+  "    "                           
*                                                                               
         DC    AL2(1382,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS TX              
         DC    AL4(4025)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(3130)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(1110)                                                        
         DC    AL4(870)                                                         
         DC    AL4(730)                                                         
*                                                                               
         DC    AL2(1403,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS NW              
         DC    AL4(1215)                                                        
         DC    AL4(830)                                                         
         DC    AL4(1025)                                                        
         DC    AL4(820)                                                         
         DC    AL4(675)                                                         
         DC    AL4(345)                                                         
         DC    AL4(265)                                                         
         DC    AL4(225)                                                         
*                                                                               
         DC    AL2(1422,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS TX             
         DC    AL4(6030)                                                        
         DC    AL4(4125)                                                        
         DC    AL4(4710)                                                        
         DC    AL4(4050)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(1670)                                                        
         DC    AL4(1305)                                                        
         DC    AL4(1090)                                                        
*                                                                               
         DC    AL2(1423,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS NW             
         DC    AL4(1825)                                                        
         DC    AL4(1250)                                                        
         DC    AL4(1420)                                                        
         DC    AL4(1230)                                                        
         DC    AL4(1000)                                                        
         DC    AL4(510)                                                         
         DC    AL4(400)                                                         
         DC    AL4(340)                                                         
*                                                                               
         DC    AL2(1422,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS TX             
         DC    AL4(8040)                                                        
         DC    AL4(5505)                                                        
         DC    AL4(6270)                                                        
         DC    AL4(5400)                                                        
         DC    AL4(4405)                                                        
         DC    AL4(2240)                                                        
         DC    AL4(1740)                                                        
         DC    AL4(1455)                                                        
*                                                                               
         DC    AL2(1420,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS NW             
         DC    AL4(2425)                                                        
         DC    AL4(1735)                                                        
         DC    AL4(1895)                                                        
         DC    AL4(1640)                                                        
         DC    AL4(1335)                                                        
         DC    AL4(680)                                                         
         DC    AL4(530)                                                         
         DC    AL4(445)                                                         
*                                                                               
         DC    AL2(1442,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS TX             
         DC    AL4(10045)                                                       
         DC    AL4(6875)                                                        
         DC    AL4(7835)                                                        
         DC    AL4(6750)                                                        
         DC    AL4(5505)                                                        
         DC    AL4(2785)                                                        
         DC    AL4(2180)                                                        
         DC    AL4(1830)                                                        
*                                                                               
         DC    AL2(1443,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS NW             
         DC    AL4(3035)                                                        
         DC    AL4(2080)                                                        
         DC    AL4(2375)                                                        
         DC    AL4(2045)                                                        
         DC    AL4(1660)                                                        
         DC    AL4(850)                                                         
         DC    AL4(665)                                                         
         DC    AL4(555)                                                         
*                                                                               
         DC    AL2(1442,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS TX             
         DC    AL4(20100)                                                       
         DC    AL4(13765)                                                       
         DC    AL4(15685)                                                       
         DC    AL4(13505)                                                       
         DC    AL4(11010)                                                       
         DC    AL4(5565)                                                        
         DC    AL4(4350)                                                        
         DC    AL4(3645)                                                        
*                                                                               
         DC    AL2(1463,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS NW             
         DC    AL4(6070)                                                        
         DC    AL4(4160)                                                        
         DC    AL4(4740)                                                        
         DC    AL4(4080)                                                        
         DC    AL4(3330)                                                        
         DC    AL4(1680)                                                        
         DC    AL4(1315)                                                        
         DC    AL4(1100)                                                        
*                                                                               
         DC    AL2(1482,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS TX             
         DC    AL4(30145)                                                       
         DC    AL4(20635)                                                       
         DC    AL4(23520)                                                       
         DC    AL4(20260)                                                       
         DC    AL4(16515)                                                       
         DC    AL4(8350)                                                        
         DC    AL4(6530)                                                        
         DC    AL4(5470)                                                        
*                                                                               
         DC    AL2(1483,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS NW             
         DC    AL4(9100)                                                        
         DC    AL4(6235)                                                        
         DC    AL4(7105)                                                        
         DC    AL4(6120)                                                        
         DC    AL4(4985)                                                        
         DC    AL4(2530)                                                        
         DC    AL4(1975)                                                        
         DC    AL4(1655)                                                        
*                                                                               
         DC    AL2(1502,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS TX           
         DC    AL4(40190)                                                       
         DC    AL4(27520)                                                       
         DC    AL4(31365)                                                       
         DC    AL4(27010)                                                       
         DC    AL4(22025)                                                       
         DC    AL4(11140)                                                       
         DC    AL4(8705)                                                        
         DC    AL4(7295)                                                        
*                                                                               
         DC    AL2(1503,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS NW           
         DC    AL4(12130)                                                       
         DC    AL4(8310)                                                        
         DC    AL4(9465)                                                        
         DC    AL4(8155)                                                        
         DC    AL4(6650)                                                        
         DC    AL4(3365)                                                        
         DC    AL4(2635)                                                        
         DC    AL4(2210)                                                        
*                                                                               
         DC    AL2(1522,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS TX                
         DC    AL4(45370)                                                       
         DC    AL4(34110)                                                       
         DC    AL4(33210)                                                       
         DC    AL4(29405)                                                       
         DC    AL4(24315)                                                       
         DC    AL4(19240)                                                       
         DC    AL4(16695)                                                       
         DC    AL4(13615)                                                       
*                                                                               
         EJECT                                                                  
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
         EJECT                                                                  
         DC    AL2(53,80,1,24,0,0)  TV TAGS - REGULAR, UNITS 1-24               
         DC    AL4(16735)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(0)                                                           
         DC    AL4(16735)                                                       
         DC    AL4(0)                                                           
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
*                                                                               
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(9340)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
*                                                                               
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(5115)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
*                                                                               
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(56710)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(41515)                                                       
         DC    AL4(36755)                                                       
         DC    AL4(30395)                                                       
         DC    AL4(24050)                                                       
         DC    AL4(20870)                                                       
         DC    AL4(17020)                                                       
         DC    AL4(30390)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(17955)                                                       
         DC    AL4(31395)                                                       
         DC    AL4(87345)          PIL                                          
         DC    AL4(67160)          PI                                           
         DC    AL4(36775)          SE                                           
         DC    AL4(9090)           C3,C6                                        
         DC    AL4(17930)          C9                                           
*                                                                               
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(16735)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(16735)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
         DC    AL4(12685)                                                       
*                                                                               
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(9340)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(9340)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
         DC    AL4(7040)                                                        
*                                                                               
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(5115)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(5115)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
         DC    AL4(3835)                                                        
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(24950)                                                       
         DC    AL4(24950)                                                       
         DC    AL4(18380)                                                       
         DC    AL4(16265)                                                       
         DC    AL4(14430)                                                       
         DC    AL4(19185)                                                       
         DC    AL4(8565)                                                        
         DC    AL4(13700)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
         DC    AL4(10325)                                                       
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
         DC    AL4(7410)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
         DC    AL4(4045)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(90736)          (1.6 X 56710, BUT WANT NEAREST .05)          
         DC    AL4(68224)          (1.6 X 42640, BUT WANT NEAREST .05)          
         DC    AL4(66424)          (1.6 X 41515, BUT WANT NEAREST .05)          
         DC    AL4(58808)          (1.6 X 36755, BUT WANT NEAREST .05)          
         DC    AL4(48632)          (1.6 X 30395, BUT WANT NEAREST .05)          
         DC    AL4(38480)          (1.6 X 24050, BUT WANT NEAREST .05)          
         DC    AL4(33392)          (1.6 X 20870, BUT WANT NEAREST .05)          
         DC    AL4(27232)          (1.6 X 17020, BUT WANT NEAREST .05)          
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(39920)          (1.6 X 24950, BUT WANT NEAREST .05)          
         DC    AL4(39920)          (1.6 X 24950, BUT WANT NEAREST .05)          
         DC    AL4(29410)          (1.6 X 18380, BUT WANT NEAREST .05)          
         DC    AL4(26025)          (1.6 X 16265, BUT WANT NEAREST .05)          
         DC    AL4(23090)          (1.6 X 14430, BUT WANT NEAREST .05)          
*                                                                               
         DC    AL2(440,80,0,0,0,0)  INDUST ON CAMERA - CAT1                     
         DC    AL4(47100)          PRINCIPAL                                    
         DC    AL4(30650)          PHD                                          
         DC    AL4(118550)         P3D                                          
         DC    AL4(165450)         P5D                                          
         DC    AL4(182050)         P6D                                          
         DC    AL4(42100)          SD                                           
         DC    AL4(100800)         S3D                                          
         DC    AL4(168150)         S5D/S6D                                      
         DC    AL4(35200)          GD                                           
         DC    AL4(84650)          G3D                                          
         DC    AL4(141000)         G5D/G6D                                      
         DC    AL4(28400)          GS                                           
         DC    AL4(35600)          SO                                           
         DC    AL4(85700)          NAR                                          
         DC    AL4(12250)          BG                                           
         DC    AL4(13500)          BS                                           
         DC    AL4(22900)          BSB                                          
*                                                                               
         DC    AL2(441,80,0,0,0,0)  INDUST ON CAMERA - CAT2                     
         DC    AL4(58600)          PRINCIPAL                                    
         DC    AL4(38050)          PHD                                          
         DC    AL4(146150)         P3D                                          
         DC    AL4(204850)         P5D                                          
         DC    AL4(225350)         P6D                                          
         DC    AL4(52500)          SD                                           
         DC    AL4(126100)         S3D                                          
         DC    AL4(210100)         S5D/S6D                                      
         DC    AL4(44150)          GD                                           
         DC    AL4(105750)         G3D                                          
         DC    AL4(176300)         G5D/G6D                                      
         DC    AL4(35050)          GS                                           
         DC    AL4(43950)          SO                                           
         DC    AL4(101500)         NAR                                          
         DC    AL4(12250)          BG                                           
         DC    AL4(13500)          BS                                           
         DC    AL4(22900)          BSB                                          
*                                                                               
         DC    AL2(442,36,0,0,0,0)  INDUST OFF CAMERA - CAT1                    
         DC    AL4(0)                                                           
         DC    AL4(38550)          P                                            
         DC    AL4(25300)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(21050)          SO                                           
         DC    AL4(16850)          GS                                           
*                                                                               
         DC    AL2(443,36,0,0,0,0)  INDUST OFF CAMERA - CAT2                    
         DC    AL4(0)                                                           
         DC    AL4(42900)          P                                            
         DC    AL4(28400)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(23850)          SO                                           
         DC    AL4(19050)          GS                                           
*                                                                               
         DC    AL2(450,36,0,0,0,0) INDUSTRIAL STORECASTING - BUYOUT             
         DC    AL4(115650)                          3X 3 MONTH RATE             
         DC    AL4(115650)         P                                            
         DC    AL4(115650)         SOLO/DUO                                     
         DC    AL4(115650)                                                      
         DC    AL4(115650)         SO                                           
         DC    AL4(115650)         GS                                           
*                                                                               
         DC    AL2(451,36,0,0,0,0) INDUSTRIAL STORECASTING - 3M                 
         DC    AL4(38550)                                                       
         DC    AL4(38550)          P                                            
         DC    AL4(38550)          SOLO/DUO                                     
         DC    AL4(38550)                                                       
         DC    AL4(38550)          SO                                           
         DC    AL4(38550)          GS                                           
*                                                                               
         DC    AL2(452,36,0,0,0,0) INDUSTRIAL STORECASTING - 6M/3MEXT           
         DC    AL4(77100)                2X 3 MONTH RATE                        
         DC    AL4(77100)          P                                            
         DC    AL4(77100)          SOLO/DUO                                     
         DC    AL4(77100)                                                       
         DC    AL4(77100)          SO                                           
         DC    AL4(77100)          GS                                           
*                                                                               
         EJECT                                                                  
*                                                                               
         DC    AL2(453,36,0,0,0,0) INDUSTRIAL STORECASTING - 6MEXT              
         DC    AL4(154200)               4X 3 MONTH RATE                        
         DC    AL4(154200)         P                                            
         DC    AL4(154200)         SOLO/DUO                                     
         DC    AL4(154200)                                                      
         DC    AL4(154200)         SO                                           
         DC    AL4(154200)         GS                                           
*                                                                               
         DC    AL2(460,84,0,0,0,0) RTK ON CAMERA - CAT1                         
         DC    AL4(47100)          PRINCIPAL/ST                                 
         DC    AL4(30650)          PHD                                          
         DC    AL4(118550)         P3D                                          
         DC    AL4(165450)         P5D                                          
         DC    AL4(182050)         P6D                                          
         DC    AL4(47100)          S/D                                          
         DC    AL4(28400)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(35600)          SO                                           
         DC    AL4(85700)          NAR                                          
         DC    AL4(12250)          BG                                           
         DC    AL4(13500)          BS                                           
         DC    AL4(22900)          BSB                                          
         DC    AL4(42100)          SD                                           
         DC    AL4(35200)          GD3/GD6/GD9                                  
         DC    AL4(100800)         S3D                                          
         DC    AL4(84650)          G3D                                          
         DC    AL4(168150)         S5D/S6D                                      
         DC    AL4(141000)         G5D/G6D                                      
*                                                                               
         DC    AL2(461,84,0,0,0,0) RTK ON CAMERA - CAT2                         
         DC    AL4(58600)          PRINCIPAL/ST                                 
         DC    AL4(38050)          PHD                                          
         DC    AL4(146150)         P3D                                          
         DC    AL4(204850)         P5D                                          
         DC    AL4(225350)         P6D                                          
         DC    AL4(58600)          S/D                                          
         DC    AL4(35050)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(43950)          SO                                           
         DC    AL4(101500)         NAR                                          
         DC    AL4(12250)          BG                                           
         DC    AL4(13500)          BS                                           
         DC    AL4(22900)          BSB                                          
         DC    AL4(52500)          SD                                           
         DC    AL4(44150)          GD3/GD6/GD9                                  
         DC    AL4(126100)         S3D                                          
         DC    AL4(105750)         G3D                                          
         DC    AL4(210100)         S5D/S6D                                      
         DC    AL4(176300)         G5D/G6D                                      
*                                                                               
         DC    AL2(462,84,0,0,0,0) RTK OFF CAMERA - CAT1                        
         DC    AL4(38550)          PRINCIPAL/ST                                 
         DC    AL4(38550)          PHD                                          
         DC    AL4(38550)          P3D                                          
         DC    AL4(38550)          P5D                                          
         DC    AL4(38550)          P6D                                          
         DC    AL4(25300)          S/D                                          
         DC    AL4(16850)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(21050)          SO                                           
         DC    AL4(38550)          NAR                                          
         DC    AL4(0)              BG           NA                              
         DC    AL4(0)              BS           NA                              
         DC    AL4(0)              BSB          NA                              
         DC    AL4(0)              SD           NA                              
         DC    AL4(0)              GD3/GD6/GD9  NA                              
         DC    AL4(0)              S3D          NA                              
         DC    AL4(0)              G3D          NA                              
         DC    AL4(0)              S5D/S6D      NA                              
         DC    AL4(0)              G5D/G6D      NA                              
*                                                                               
         DC    AL2(463,84,0,0,0,0) RTK OFF CAMERA - CAT2                        
         DC    AL4(42900)          PRINCIPAL/ST                                 
         DC    AL4(42900)          PHD                                          
         DC    AL4(42900)          P3D                                          
         DC    AL4(42900)          P5D                                          
         DC    AL4(42900)          P6D                                          
         DC    AL4(28400)          S/D                                          
         DC    AL4(19050)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(23850)          SO                                           
         DC    AL4(42900)          NAR                                          
         DC    AL4(0)              BG           NA                              
         DC    AL4(0)              BS           NA                              
         DC    AL4(0)              BSB          NA                              
         DC    AL4(0)              SD           NA                              
         DC    AL4(0)              GD3/GD6/GD9  NA                              
         DC    AL4(0)              S3D          NA                              
         DC    AL4(0)              G3D          NA                              
         DC    AL4(0)              S5D/S6D      NA                              
         DC    AL4(0)              G5D/G6D      NA                              
*                                                                               
         EJECT                                                                  
         DC    AL2(470,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT1                  
         DC    AL4(38550)          P    PRINCIPAL                               
         DC    AL4(38550)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(17350)          NP   NON-PRINCIPAL                           
         DC    AL4(25300)          S    SOLO/DUO                                
         DC    AL4(16850)          SO   STEP OUT                                
         DC    AL4(21050)          S16  SOLO <16 BARS                           
         DC    AL4(16850)          G3   GROUP                                   
         DC    AL4(21050)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(16850)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(471,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT2                  
         DC    AL4(42900)          P    PRINCIPAL                               
         DC    AL4(42900)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(19250)          NP   NON-PRINCIPAL                           
         DC    AL4(28400)          S    SOLO/DUO                                
         DC    AL4(19050)          SO   STEP OUT                                
         DC    AL4(23850)          S16  SOLO <16 BARS                           
         DC    AL4(19050)          G3   GROUP                                   
         DC    AL4(23850)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(19050)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(480,16,0,0,0,0) IVR INTERACTIVE VOICE                        
         DC    AL4(21400)          P    PRINCIPAL                               
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
*                                                                               
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(2505)            PRINCIPAL ON CAMERA                         
         DC    AL4(1710)                "     OFF CAMERA                        
         DC    AL4(1960)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1690)              "   6-8 "    "                            
         DC    AL4(1375)              "    9+ "    "                            
         DC    AL4(700)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(540)               "   6-8  "    "                           
         DC    AL4(455)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(5030)                                                        
         DC    AL4(3440)                                                        
         DC    AL4(3915)                                                        
         DC    AL4(3375)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(1390)                                                        
         DC    AL4(1085)                                                        
         DC    AL4(910)                                                         
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(7535)                                                        
         DC    AL4(5155)                                                        
         DC    AL4(5885)                                                        
         DC    AL4(5065)                                                        
         DC    AL4(4125)                                                        
         DC    AL4(2085)                                                        
         DC    AL4(1630)                                                        
         DC    AL4(1365)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(10050)                                                       
         DC    AL4(6880)                                                        
         DC    AL4(7840)                                                        
         DC    AL4(6750)                                                        
         DC    AL4(5505)                                                        
         DC    AL4(2800)                                                        
         DC    AL4(2175)                                                        
         DC    AL4(1820)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(12555)                                                       
         DC    AL4(8595)                                                        
         DC    AL4(9795)                                                        
         DC    AL4(8440)                                                        
         DC    AL4(6880)                                                        
         DC    AL4(3480)                                                        
         DC    AL4(2725)                                                        
         DC    AL4(2285)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(25125)                                                       
         DC    AL4(17205)                                                       
         DC    AL4(19605)                                                       
         DC    AL4(16880)                                                       
         DC    AL4(13760)                                                       
         DC    AL4(6955)                                                        
         DC    AL4(5440)                                                        
         DC    AL4(4555)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(37680)                                                       
         DC    AL4(25795)                                                       
         DC    AL4(29400)                                                       
         DC    AL4(25325)                                                       
         DC    AL4(20645)                                                       
         DC    AL4(10440)                                                       
         DC    AL4(8160)                                                        
         DC    AL4(6835)                                                        
*        SPACE 1                                                                
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(50240)                                                       
         DC    AL4(34400)                                                       
         DC    AL4(39205)                                                       
         DC    AL4(33765)                                                       
         DC    AL4(27530)                                                       
         DC    AL4(13925)                                                       
         DC    AL4(10880)                                                       
         DC    AL4(9120)                                                        
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(56710)                                                       
         DC    AL4(42640)                                                       
         DC    AL4(41515)                                                       
         DC    AL4(36755)                                                       
         DC    AL4(30395)                                                       
         DC    AL4(24050)                                                       
         DC    AL4(20870)                                                       
         DC    AL4(17020)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
*                                                                               
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(13390)          ON CAMERA                                    
         DC    AL4(10150)          OFF                                          
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(10150)                                                       
         DC    AL4(10150)                                                       
         DC    AL4(10150)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(13390)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10150)          SE                                           
         DC    AL4(10150)          C3,C6                                        
         DC    AL4(10150)          C9                                           
*                                                                               
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(7470)           ON CAMERA                                    
         DC    AL4(5630)           OFF                                          
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(5630)                                                        
         DC    AL4(5630)                                                        
         DC    AL4(5630)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(7470)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5630)           SE                                           
         DC    AL4(5630)           C3,C6                                        
         DC    AL4(5630)           C9                                           
*                                                                               
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(4090)           ON CAMERA                                    
         DC    AL4(3070)           OFF                                          
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(3070)                                                        
         DC    AL4(3070)                                                        
         DC    AL4(3070)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3070)           SE                                           
         DC    AL4(3070)           C3,C6                                        
         DC    AL4(3070)           C9                                           
*                                                                               
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
         DC    AL4(8260)                                                        
*                                                                               
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
         DC    AL4(5930)                                                        
*                                                                               
         DC    AL2(248,44,50,255,0,0)  UNITS 50+                                
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         DC    AL4(3235)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
*                                                                               
         DC    AL2(249,88,1,4,0,0)  GA - TV, REGULAR, UNITS 1-4                 
         DC    AL4(14500)          ON CAMERA                                    
         DC    AL4(11000)          OFF                                          
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(11000)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11000)          SE                                           
         DC    AL4(11000)          C3,C6                                        
         DC    AL4(11000)          C9                                           
         DC    AL4(14500)          SOLO/DUO ON CAMERA                           
         DC    AL4(11000)                   OFF                                 
*                                                                               
         DC    AL2(249,88,5,14,0,0)  UNITS 5-14                                 
         DC    AL4(8200)           ON CAMERA                                    
         DC    AL4(6100)           OFF                                          
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(6100)                                                        
         DC    AL4(6100)                                                        
         DC    AL4(6100)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6100)           SE                                           
         DC    AL4(6100)           C3,C6                                        
         DC    AL4(6100)           C9                                           
         DC    AL4(8200)           SOLO/DUO ON CAMERA                           
         DC    AL4(6100)           OFF                                          
*                                                                               
         DC    AL2(249,88,15,255,0,0)  UNITS 15+                                
         DC    AL4(4500)           ON CAMERA                                    
         DC    AL4(3300)           OFF                                          
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3300)           SE                                           
         DC    AL4(3300)           C3,C6                                        
         DC    AL4(3300)           C9                                           
         DC    AL4(4500)           SOLO/DUO ON CAMERA                           
         DC    AL4(3300)           OFF                                          
         EJECT                                                                  
*              RATES FOR NORTHWEST ADDENDUM TAGS                                
*                                                                               
         DC    AL2(340,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  2WK            
         DC    AL4(11800)          ON CAMERA                                    
         DC    AL4(9000)           OFF                                          
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9000)           SE                                           
         DC    AL4(9000)           C3,C6                                        
         DC    AL4(9000)           C9                                           
*                                                                               
         DC    AL2(340,80,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(9500)           ON CAMERA                                    
         DC    AL4(7200)           OFF                                          
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7200)           SE                                           
         DC    AL4(7200)           C3,C6                                        
         DC    AL4(7200)           C9                                           
*                                                                               
         DC    AL2(340,80,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(5300)           ON CAMERA                                    
         DC    AL4(4000)           OFF                                          
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(4000)                                                        
         DC    AL4(4000)                                                        
         DC    AL4(4000)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4000)           SE                                           
         DC    AL4(4000)           C3,C6                                        
         DC    AL4(4000)           C9                                           
*                                                                               
         DC    AL2(340,80,25,255,0,0)  UNITS 26+  2-WK                          
         DC    AL4(2500)           ON CAMERA                                    
         DC    AL4(2200)           OFF                                          
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(2500)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2200)           SE                                           
         DC    AL4(2200)           C3,C6                                        
         DC    AL4(2200)           C9                                           
*                                                                               
         DC    AL2(341,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  13WK           
         DC    AL4(13800)          ON CAMERA                                    
         DC    AL4(10400)          OFF                                          
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(10400)                                                       
         DC    AL4(10400)                                                       
         DC    AL4(10400)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10400)          SE                                           
         DC    AL4(10400)          C3,C6                                        
         DC    AL4(10400)          C9                                           
*                                                                               
         DC    AL2(341,80,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(11300)          ON CAMERA                                    
         DC    AL4(8500)           OFF                                          
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8500)           SE                                           
         DC    AL4(8500)           C3,C6                                        
         DC    AL4(8500)           C9                                           
*                                                                               
         DC    AL2(341,80,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(6300)           ON CAMERA                                    
         DC    AL4(4800)           OFF                                          
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(4800)                                                        
         DC    AL4(4800)                                                        
         DC    AL4(4800)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(6300)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4800)           SE                                           
         DC    AL4(4800)           C3,C6                                        
         DC    AL4(4800)           C9                                           
*                                                                               
         DC    AL2(341,80,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(3500)           ON CAMERA                                    
         DC    AL4(2900)           OFF                                          
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(3500)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2900)           SE                                           
         DC    AL4(2900)           C3,C6                                        
         DC    AL4(2900)           C9                                           
*                                                                               
         DC    AL2(390,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 2-WK            
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
*                                                                               
         DC    AL2(390,44,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
         DC    AL4(4100)                                                        
*                                                                               
         DC    AL2(390,44,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
*                                                                               
         DC    AL2(390,44,25,255,0,0)  UNITS 26+   2-WK                         
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
         DC    AL4(1300)                                                        
*                                                                               
         DC    AL2(343,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 13-WK           
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(6900)                                                        
*                                                                               
         DC    AL2(343,44,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
         DC    AL4(5200)                                                        
*                                                                               
         DC    AL2(343,44,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
         DC    AL4(2900)                                                        
*                                                                               
         DC    AL2(343,44,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
         DC    AL4(1600)                                                        
*                                                                               
         DC    AL2(344,44,1,255,0,0)  GA - ADO TAGS UNITS 1-255                 
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
         DC    AL4(9200)                                                        
*                                                                               
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
*                                                                               
         DC    AL2(250,84,0,0,0,0)  PRM FOR SAG AND AFT                         
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(23400)          OFF                                          
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(32300)          ON                                           
         DC    AL4(9700)           SAG ONLY EXTRA                               
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(251,84,0,0,0,0)  PRR FOR AFT AND SAG                         
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(23400)          OFF                                          
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(9700)           EXTRA                                        
         DC    AL4(32300)          ON                                           
         DC    AL4(9700)           SAG ONLY EXTRA                               
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          OFF                                          
         DC    AL4(23400)          SOLO/DUO OFF CAM                             
         EJECT                                                                  
VNWTAB   DC    AL2(252,44,1,1,0,0)  VNW 1ST USE AT SESSION FEE                  
         DC    AL4(56710)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(42640)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(41515)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(36755)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(30395)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(24050)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(20870)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17020)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(56710)                                               
         DC    AL1(50),AL3(42640)                                               
         DC    AL1(50),AL3(41515)                                               
         DC    AL1(50),AL3(36755)                                               
         DC    AL1(50),AL3(30395)                                               
         DC    AL1(50),AL3(24050)                                               
         DC    AL1(50),AL3(20870)                                               
         DC    AL1(50),AL3(17020)                                               
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
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL2(59),AL1(UIMS,ALL,AFM+NON,0,0,0,ALL)    SESSION               
         DC    AL2(60),AL1(UBSM,ALL,AFM+NON,0,0,0,ALL)                          
         DC    AL2(60),AL1(UMRR,ALL,AFM+NON,0,0,0,ALL)                          
         DC    AL2(61),AL1(UBSR,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(61),AL1(URRR,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(62),AL1(UBSS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(66),AL1(UCSS,ALL,NON,0,0,0,LIKETV)                           
         DC    AL2(62),AL1(URRS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(USRS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(USSS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(USFS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(UDWN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(UFGS,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(62),AL1(UPUB,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(52),AL1(UPUB,ALL,ALL-AFM,0,0,0,RADIO)                        
         DC    AL2(61),AL1(UCNL,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(62),AL1(UCNL,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(64),AL1(UCNL,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(61),AL1(USCN,ALL,AFT+NON,0,0,0,RADIO)      SP CNCL           
         DC    AL2(62),AL1(USCN,ALL,ALL-AFM,0,0,0,LIKETV)     SP CNCL           
         DC    AL2(64),AL1(USCN,ALL,SAG+NON+AFT,0,0,0,CABLE)  SP CNCL           
         DC    AL2(64),AL1(UBSS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(66),AL1(UCSS,ALL,NON,0,0,0,CABLE)                            
         DC    AL2(64),AL1(URRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(64),AL1(USRS,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
         DC    AL2(37),AL1(UPPF,ALL,ALL-AFM,0,0,0,LIKETV)                       
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
         DC    AL2(62),AL1(ULFT,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(64),AL1(ULFT,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(61),AL1(USLF,ALL,AFT+NON,0,0,0,RADIO)  SPAN LFT              
         DC    AL2(62),AL1(USLF,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(64),AL1(USLF,ALL,SAG+NON+AFT,0,0,0,CABLE)                    
*                                                                               
         DC    AL2(125),AL1(UALF,ALL,AFT+NON,0,0,0,RADIO) AD LIFT               
         DC    AL2(90),AL1(UALF,ALL,ALL-AFM,0,0,0,LIKETV+CABLE)                 
*                                                                               
         DC    AL2(42),AL1(UDEM,ALL,ALL,0,0,0,LIKETV)     DEMO                  
         DC    AL2(43),AL1(UDEM,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(42),AL1(USNA,ALL,ALL,0,0,0,LIKETV)                           
         DC    AL2(43),AL1(USNA,ALL,AFT+NON,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(63),AL1(UHLD,ALL,ALL-AFM,0,0,0,LIKETV) HLD FEE               
         DC    AL2(63),AL1(USHL,ALL,ALL-AFM,0,0,0,LIKETV) SPAN HLD              
         DC    AL2(38),AL1(UREN,ALL,ALL-AFM,0,0,0,LIKETV) REINST                
         DC    AL2(38),AL1(USRE,ALL,ALL-AFM,0,0,0,LIKETV) REINST                
         DC    AL2(1540),AL1(UADH,ALL,ALL-AFM,0,0,0,LIKETV) AD HLD FEE          
         DC    AL2(1560),AL1(UARN,ALL,ALL-AFM,0,0,0,LIKETV) AD REINST           
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
         DC    AL2(10),AL1(UWSP,UWSP13W,ALL,0,0,0,LIKETV) 13 WK TV              
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
         DC    AL2(35),AL1(UMUS,UMUSDUB6,ALL,0,0,0,ALL)   DUB-6MO               
         DC    AL2(35),AL1(UMUS,UMUSDSH6,ALL,0,0,0,ALL)   DUB TO SH 6M          
         DC    AL2(35),AL1(UMUS,UMUSDUB1,ALL,0,0,0,ALL)   DUB-1YR               
         DC    AL2(35),AL1(UMUS,UMUSDSH1,ALL,0,0,0,ALL)   DUB TO SH 1Y          
         DC    AL2(35),AL1(UNBM,ALL,ALL,0,0,0,ALL)        NON-BRD MUS           
         DC    AL2(09),AL1(UMUS,UMUSDUB8,ALL,0,0,0,ALL)   DUB-8WK               
         DC    AL2(09),AL1(UMUS,UMUS8W,ALL,0,0,0,ALL)     REUSE-8WK             
         DC    AL2(09),AL1(UMUS,UMUSNEW8,ALL,0,0,0,ALL)   NEW-8WK               
         DC    AL2(09),AL1(UMUS,UMUSDSH8,ALL,0,0,0,ALL)   DUB TO SH 8W          
         DC    AL2(36),AL1(UFMU,ALL,ALL,0,0,0,ALL)        1ST REUSE             
         DC    AL2(35),AL1(UMVM,UMVM1YR,ALL,0,0,0,ALL)    MVM-1YR               
         DC    AL2(35),AL1(UNIM,ALL,ALL,0,0,0,ALL)        NIM                   
         DC    AL2(35),AL1(UIHM,ALL,ALL,0,0,0,ALL)        IHM                   
*                                                                               
         DC    AL2(45),AL1(UFGM,UFGMEU12,ALL,0,0,0,ALL)   AFM F-E 12M           
         DC    AL2(45),AL1(UFGM,UFGMNE12,ALL,0,0,0,ALL)   NOT EUR 12M           
         DC    AL2(47),AL1(UFGM,UFGMEU24,ALL,0,0,0,ALL)   EUR 24M               
         DC    AL2(47),AL1(UFGM,UFGMNE24,ALL,0,0,0,ALL)   NOT EUR 24M           
         DC    AL2(46),AL1(UFGM,UFGMWO12,ALL,0,0,0,ALL)   WRLD 12M              
         DC    AL2(48),AL1(UFGM,UFGMWO24,ALL,0,0,0,ALL)   WRLD 24M              
*                                                                               
         DC    AL2(50),AL1(UFGR,UFGRUK,ALL,0,0,0,LIKETV)  FGN REUSE-UK          
         DC    AL2(51),AL1(UFGR,UFGREUR,ALL,0,0,0,LIKETV) EUR W/O UK            
         DC    AL2(62),AL1(UFGR,UFGRWOR,ALL,0,0,0,LIKETV) W W/O UK&EUR          
         DC    AL2(62),AL1(UFGR,UFGRAP,ALL,0,0,0,LIKETV)  ASIAN PAC             
         DC    AL2(62),AL1(UFGR,UFGRJAP,ALL,0,0,0,LIKETV) JAPAN                 
         DC    AL2(237),AL1(UFGR,UFGRWIDE,ALL,0,0,0,LIKETV) WLDWIDE             
         DC    AL2(62),AL1(UFGR,UFGRMAJ,ALL,0,0,0,LIKETV) NEW-W/MAJOR           
         DC    AL2(256),AL1(UFGR,UFGREXT,ALL,0,0,0,LIKETV) FGR EXT              
         DC    AL2(49),AL1(UFGR,UFGRRAD,ALL,0,0,0,RADIO)  RADIO                 
*                                                                               
         DC    AL2(50),AL1(USFR,USFRA,ALL,0,0,0,LIKETV)     SP FGN A            
         DC    AL2(51),AL1(USFR,USFRB,ALL,0,0,0,LIKETV)     SP FGN B            
         DC    AL2(50),AL1(USFR,USFRC,ALL,0,0,0,LIKETV)     SP FGN C            
*                                                                               
         DC    AL2(62),AL1(UPBS,ALL,ALL-AFM,0,0,0,LIKETV) PUB SVC               
         DC    AL2(52),AL1(UPBS,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(40),AL1(USNT,ALL,ALL,0,0,0,LIKETV)     SPAN NWK              
         DC    AL2(40),AL1(USNW,ALL,ALL,0,0,0,LIKETV)    SPAN N/W COMB          
         DC    AL2(10),AL1(USWS,ALL,ALL,0,0,0,LIKETV)     SPAN WSP              
*                                                                               
         DC    AL2(1000),AL1(UADT,UADT3D,ALL,0,0,0,ALL)   AD SES-TV-3DY         
         DC    AL2(1020),AL1(UADT,UADT1W,ALL,0,0,0,ALL)   1WK                   
         DC    AL2(1020),AL1(UADT,UADT2W,ALL,0,0,0,ALL)   2WK(NW)               
         DC    AL2(1040),AL1(UADT,UADT4W,ALL,0,0,0,ALL)   4WK                   
         DC    AL2(1040),AL1(UADT,UADT31D,ALL,0,0,0,ALL)  31D(KS)               
         DC    AL2(1060),AL1(UADT,UADT13W,ALL,0,0,0,ALL)  13WEEK                
*                                                                               
         DC    AL2(1000),AL1(UARS,UARS3D,ALL,0,0,0,ALL)   AD RER-TV-3D          
         DC    AL2(1020),AL1(UARS,UARS1W,ALL,0,0,0,ALL)   1WK                   
         DC    AL2(1020),AL1(UARS,UARS2W,ALL,0,0,0,ALL)   2WK(NW)               
         DC    AL2(1040),AL1(UARS,UARS4W,ALL,0,0,0,ALL)   4WK                   
         DC    AL2(1040),AL1(UARS,UARS31D,ALL,0,0,0,ALL)  31D(KS)               
         DC    AL2(1060),AL1(UARS,UARS13W,ALL,0,0,0,ALL)  13WK                  
*                                                                               
         DC    AL2(1080),AL1(UADO,UADO3D,ALL,0,0,0,ALL)  AD SES-RAD-3D          
         DC    AL2(1100),AL1(UADO,UADO1W,ALL,0,0,0,ALL)  1WK                    
         DC    AL2(1100),AL1(UADO,UADO2W,ALL,0,0,0,ALL)  2WK(NW)                
         DC    AL2(1120),AL1(UADO,UADO4W,ALL,0,0,0,ALL)  4WK                    
         DC    AL2(1120),AL1(UADO,UADO31D,ALL,0,0,0,ALL) 31D(KS)                
         DC    AL2(1140),AL1(UADO,UADO13W,ALL,0,0,0,ALL) 13WK                   
*                                                                               
         DC    AL2(1080),AL1(UARR,UARR3D,ALL,0,0,0,ALL)  AD SES-RAD-3D          
         DC    AL2(1100),AL1(UARR,UARR1W,ALL,0,0,0,ALL)  1WK                    
         DC    AL2(1100),AL1(UARR,UARR2W,ALL,0,0,0,ALL)  2WK(NW)                
         DC    AL2(1120),AL1(UARR,UARR4W,ALL,0,0,0,ALL)  4WK                    
         DC    AL2(1120),AL1(UARR,UARR31D,ALL,0,0,0,ALL) 31D(KS)                
         DC    AL2(1140),AL1(UARR,UARR13W,ALL,0,0,0,ALL) 13WK                   
*                                                                               
         DC    AL2(1160),AL1(UADD,ALL,ALL,0,0,0,LIKETV)    AD DEMO              
         DC    AL2(1180),AL1(UADD,ALL,AFT+NON,0,0,0,RADIO)                      
*                                                                               
         DC    AL2(1200),AL1(UADW,UADW3D,ALL,0,0,0,LIKETV) AD WSP-TV-3D         
         DC    AL2(1220),AL1(UADW,UADW1W,ALL,0,0,0,LIKETV) 1WK                  
         DC    AL2(1220),AL1(UADW,UADW2W,ALL,0,0,0,LIKETV) 2WK(NW)              
         DC    AL2(1240),AL1(UADW,UADW4W,ALL,0,0,0,LIKETV) 4WK                  
         DC    AL2(1240),AL1(UADW,UADW31D,ALL,0,0,0,LIKETV) 31D(KS)             
         DC    AL2(1260),AL1(UADW,UADW13W,ALL,0,0,0,LIKETV) 13WK                
*                                                                               
         DC    AL2(1280),AL1(UADW,UADW3D,ALL,0,0,0,RADIO) AD WSP-RAD-3D         
         DC    AL2(1300),AL1(UADW,UADW1W,ALL,0,0,0,RADIO) 1WK                   
         DC    AL2(1300),AL1(UADW,UADW2W,ALL,0,0,0,RADIO) 2WK(NW)               
         DC    AL2(1320),AL1(UADW,UADW4W,ALL,0,0,0,RADIO) 4WK                   
         DC    AL2(1320),AL1(UADW,UADW31D,ALL,0,0,0,RADIO) 31D(KS)              
         DC    AL2(1340),AL1(UADW,UADW13W,ALL,0,0,0,RADIO) 13WK                 
*                                                                               
         DC    AL2(1360),AL1(UACB,UACB50,ALL,0,0,0,ALL)    AD CABLE             
         DC    AL2(1380),AL1(UACB,UACB100,ALL,0,0,0,ALL)                        
         DC    AL2(1400),AL1(UACB,UACB150,ALL,0,0,0,ALL)                        
         DC    AL2(1420),AL1(UACB,UACB200,ALL,0,0,0,ALL)                        
         DC    AL2(1440),AL1(UACB,UACB250,ALL,0,0,0,ALL)                        
         DC    AL2(1460),AL1(UACB,UACB500,ALL,0,0,0,ALL)                        
         DC    AL2(1480),AL1(UACB,UACB750,ALL,0,0,0,ALL)                        
         DC    AL2(1500),AL1(UACB,UACB1M,ALL,0,0,0,ALL)                         
         DC    AL2(1520),AL1(UACB,UACBMAX,ALL,0,0,0,ALL)                        
*                                                                               
         DC    AL2(1360),AL1(UACB,UACBM50,ALL,0,0,0,ALL)    AD CBL+             
         DC    AL2(1380),AL1(UACB,UACBM100,ALL,0,0,0,ALL)   MILLION             
         DC    AL2(1400),AL1(UACB,UACBM150,ALL,0,0,0,ALL)                       
         DC    AL2(1420),AL1(UACB,UACBM200,ALL,0,0,0,ALL)                       
         DC    AL2(1440),AL1(UACB,UACBM250,ALL,0,0,0,ALL)                       
         DC    AL2(1460),AL1(UACB,UACBM500,ALL,0,0,0,ALL)                       
         DC    AL2(1480),AL1(UACB,UACBM750,ALL,0,0,0,ALL)                       
         DC    AL2(1500),AL1(UACB,UACBM1M,ALL,0,0,0,ALL)                        
*                                                         CMB S/W-TV            
         DC    AL2(1200),AL1(UADC,UADC3D,ALL,0,0,0,LIKETV) 3DY                  
         DC    AL2(1220),AL1(UADC,UADC1W,ALL,0,0,0,LIKETV) 1WK                  
         DC    AL2(1220),AL1(UADC,UADC2W,ALL,0,0,0,LIKETV) 2WK(NW)              
         DC    AL2(1240),AL1(UADC,UADC4W,ALL,0,0,0,LIKETV) 4WK                  
         DC    AL2(1240),AL1(UADC,UADC31D,ALL,0,0,0,LIKETV) 31D(KS)             
         DC    AL2(1260),AL1(UADC,UADC13W,ALL,0,0,0,LIKETV) 13WK                
*                                                         CMB S/W-RAD           
         DC    AL2(1280),AL1(UADC,UADC3D,ALL,0,0,0,RADIO) 3DY                   
         DC    AL2(1300),AL1(UADC,UADC1W,ALL,0,0,0,RADIO) 1WK                   
         DC    AL2(1300),AL1(UADC,UADC2W,ALL,0,0,0,RADIO) 2WK(NW)               
         DC    AL2(1320),AL1(UADC,UADC4W,ALL,0,0,0,RADIO) 4WK                   
         DC    AL2(1320),AL1(UADC,UADC31D,ALL,0,0,0,RADIO) 31D(KS)              
         DC    AL2(1340),AL1(UADC,UADC13W,ALL,0,0,0,RADIO) 13WK                 
*                                                                               
         DC    AL2(44),AL1(UIFB,ALL,ALL,0,0,0,LIKETV)   INS FOR BOOKS           
*                                                                               
         DC    AL2(53),AL1(UTAG,UTAGREG,ALL,0,0,0,LIKETV)  TAGS TV              
         DC    AL2(54),AL1(UTAG,UTAGSESS,ALL,0,0,0,LIKETV) TAGS TV              
         DC    AL2(55),AL1(UTAG,UTAGREG,ALL,0,0,0,RADIO)   TAGS RAD             
         DC    AL2(56),AL1(UTAG,UTAGSESS,ALL,0,0,0,RADIO)  TAGS RAD             
*                                                                               
         DC    AL2(62),AL1(UINR,UINR30D,ALL,0,0,0,LIKETV) TH/IND-30DY           
         DC    AL2(236),AL1(UINR,UINRUNL,ALL,0,0,0,LIKETV) UNLIM                
         DC    AL2(61),AL1(UINR,UINR30D,ALL,0,0,0,RADIO) 30DAYS-RAD             
         DC    AL2(235),AL1(UINR,UINRUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
         DC    AL2(62),AL1(USIN,USIN30D,ALL,0,0,0,LIKETV) SP IND-30 DA          
         DC    AL2(236),AL1(USIN,USINUNL,ALL,0,0,0,LIKETV) UNLIM                
         DC    AL2(61),AL1(USIN,USIN30D,ALL,0,0,0,RADIO)  30DAYS-RAD            
         DC    AL2(235),AL1(USIN,USINUNL,ALL,0,0,0,RADIO) UNLIM-RAD             
*                                                                               
         DC    AL2(440),AL1(UINS,ALL,ALL,0,0,0,ALL)         INS - CAT1          
*        DC    AL2(441),AL1(UINS,ALL,ALL,0,0,0,ALL)  COMMENT OUT  CAT2          
         DC    AL2(442),AL1(UIDS,ALL,ALL,0,0,0,ALL)         IDS - CAT1          
*        DC    AL2(443),AL1(UIDS,ALL,ALL,0,0,0,ALL)  COMMENT OUT  CAT2          
*                                                                               
         DC    AL2(450),AL1(USTR,USTRBUY,ALL,0,0,0,ALL)  USTR - BUYOUT          
         DC    AL2(451),AL1(USTR,USTR3MO,ALL,0,0,0,ALL)  USTR - 3 MON           
         DC    AL2(452),AL1(USTR,USTR3ME,ALL,0,0,0,ALL)  USTR - 3 MN EX         
         DC    AL2(452),AL1(USTR,USTR6MO,ALL,0,0,0,ALL)  USTR - 6 MON           
         DC    AL2(453),AL1(USTR,USTR6ME,ALL,0,0,0,ALL)  USTR - 6 MN EX         
*                                                                               
         DC    AL2(460),AL1(URTK,ALL,ALL,0,0,0,ALL)  URTK - CAT 1 ON            
         DC    AL2(461),AL1(URTK,ALL,ALL,0,0,0,ALL)  URTK - CAT 2               
         DC    AL2(462),AL1(URTK,ALL,ALL,0,0,0,ALL)  URTK - CAT 1 OFF           
         DC    AL2(463),AL1(URTK,ALL,ALL,0,0,0,ALL)  URTK - CAT 2               
*                                                                               
         DC    AL2(470),AL1(UDIO,ALL,ALL,0,0,0,ALL)  UDIO - CAT 1               
         DC    AL2(471),AL1(UDIO,ALL,ALL,0,0,0,ALL)  UDIO - CAT 2               
*                                                                               
         DC    AL2(480),AL1(UIVR,ALL,ALL,0,0,0,ALL)  UIVR                       
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
         DC    AL2(250),AL1(UPRM,ALL,ALL,0,0,0,LIKETV)                          
         DC    AL2(251),AL1(UPRR,ALL,SAG,0,0,0,LIKETV)                          
         DC    AL2(251),AL1(UPRR,ALL,AFT,0,0,0,LIKETV)                          
*                                                                               
         DC    AL2(62),AL1(UVAR,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(61),AL1(UVAR,ALL,AFT+NON,0,0,0,RADIO)                        
         DC    AL2(252),AL1(UVNW,UVNWSESS,ALL-AFM,0,0,0,LIKETV)                 
         DC    AL2(253),AL1(UVNW,UVNWREG,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(254),AL1(UVNW,UVNWSESS,AFT+NON,0,0,0,RADIO)                  
         DC    AL2(57),AL1(UVNW,UVNWREG,AFT+NON,0,0,0,RADIO)                    
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
******  USE NUMBERS 370-373 USED FOR IMS                                        
******  USE NUMBERS 375-382 USED FOR AFM RATES                                  
******  USE NUMBER 390 USED FOR NW ADDENDUM RADIO TAGS                          
****** USE NUMBERS 400-407 USE FOR 2007 AFM RATES                               
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
         DC    AL4(9740)           RADIO SESSION TAG FEE                        
         DC    AL4(14500)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(11000)          GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(9200)           GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(9720)           KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(6750)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4615)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(13390)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(10150)          TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(8260)           TX ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(11300)          NW ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(8500)           NW ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(5200)           NW ADDENDUM RADIO SESSION TAG FEE            
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
         DC    AL1(2,CTPHD)                                                     
*                                                                               
         DC    AL1(3,CTG3)                                                      
         DC    AL1(3,CTG3M)                                                     
         DC    AL1(3,CTGD3)                                                     
         DC    AL1(3,CTGD)                                                      
         DC    AL1(3,CTGS)                                                      
         DC    AL1(3,CTGS3)                                                     
         DC    AL1(3,CTGSS)                                                     
         DC    AL1(3,CTGSM)                                                     
         DC    AL1(3,CTP3D)                                                     
*                                                                               
         DC    AL1(4,CTG6)                                                      
         DC    AL1(4,CTG6M)                                                     
         DC    AL1(4,CTGD6)                                                     
         DC    AL1(4,CTGS6)                                                     
         DC    AL1(4,CTP5D)                                                     
*                                                                               
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTG9M)                                                     
         DC    AL1(5,CTGD9)                                                     
         DC    AL1(5,CTGS9)                                                     
         DC    AL1(5,CTP6D)                                                     
*                                                                               
         DC    AL1(6,CTSD)                                                      
*                                                                               
         DC    AL1(7,CTS3D)                                                     
*                                                                               
         DC    AL1(8,CTS5D)                                                     
         DC    AL1(8,CTS6D)                                                     
*                                                                               
         DC    AL1(9,CTEXB)                                                     
         DC    AL1(9,CTE)                                                       
         DC    AL1(9,CTSB)                                                      
         DC    AL1(9,CTSI)                                                      
         DC    AL1(9,CTUS)                                                      
         DC    AL1(9,CTGE)                                                      
         DC    AL1(9,CTGD)                                                      
*                                                                               
         DC    AL1(10,CTHMB)                                                    
         DC    AL1(10,CTDEM)                                                    
         DC    AL1(10,CTG3D)                                                    
*                                                                               
         DC    AL1(11,CTEX)                                                     
         DC    AL1(11,CTG5D)                                                    
         DC    AL1(11,CTG6D)                                                    
*                                                                               
         DC    AL1(12,CTHM)                                                     
         DC    AL1(12,CTGS)                                                     
*                                                                               
         DC    AL1(13,CTPIL)                                                    
         DC    AL1(13,CTSO)                                                     
*                                                                               
         DC    AL1(14,CTPI)                                                     
         DC    AL1(14,CTNAR)                                                    
*                                                                               
         DC    AL1(15,CTBG)                                                     
*                                                                               
         DC    AL1(16,CTBS)                                                     
*                                                                               
         DC    AL1(17,CTBSB)                                                    
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
         DC    AL1(3,CTS3D)                                                     
         DC    AL1(3,CTS5D)                                                     
         DC    AL1(3,CTS6D)                                                     
*                                                                               
         DC    AL1(5,CTSO)                                                      
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
*              COLUMN TABLES - DIO                                              
*                                                                               
DIOCOLS  DS    0CL2                                                             
         DC    AL1(1,CTP)          PRINCIPAL                                    
*                                                                               
         DC    AL1(2,CTP3M)        PRINCIPAL 3 MINUTES                          
*                                                                               
         DC    AL1(3,CTNP)         NON-PRINCIPAL                                
*                                                                               
         DC    AL1(4,CTS)          SOLO/DUET                                    
*                                                                               
         DC    AL1(5,CTSO)         STEP OUT                                     
*                                                                               
         DC    AL1(6,CTS16)        SOLO <16 BARS                                
*                                                                               
         DC    AL1(7,CTG3)         GROUP                                        
*                                                                               
         DC    AL1(8,CTG16)        GROUP <16 BARS                               
*                                                                               
         DC    AL1(9,CTC3)         CONTRACTOR                                   
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - RTK- RETAKES                                     
         SPACE 1                                                                
RTKCOLS  DS    0CL2                                                             
         DC    AL1(1,CTP)          PRINCIPAL                                    
         DC    AL1(1,CTST)         STUNT PERFORMER                              
         DC    AL1(1,CTC)          CONTRACTOR                                   
*                                                                               
         DC    AL1(2,CTPHD)        PRINCIPAL HALF DAY                           
*                                                                               
         DC    AL1(3,CTP3D)        PRINCIPAL 3 DAY                              
*                                                                               
         DC    AL1(4,CTP5D)        PRNCIPAL 5 DAY                               
*                                                                               
         DC    AL1(5,CTP6D)        PRINCIPAL 6 DAY                              
*                                                                               
         DC    AL1(6,CTS)          SOLO SINGER                                  
         DC    AL1(6,CTD)          DUO                                          
*                                                                               
         DC    AL1(7,CTG3)         GROUP SINGERS                                
         DC    AL1(7,CTG6)         GROUP SINGERS                                
         DC    AL1(7,CTG9)         GROUP SINGERS                                
         DC    AL1(7,CTC3)         GROUP SINGERS - CONTRACTOR                   
         DC    AL1(7,CTC6)         GROUP SINGERS - CONTRACTOR                   
         DC    AL1(7,CTC9)         GROUP SINGERS - CONTRACTOR                   
*                                                                               
         DC    AL1(8,CTSO)         STEP OUT SINGER                              
*                                                                               
         DC    AL1(9,CTNAR)        NARRATOR                                     
*                                                                               
         DC    AL1(10,CTBG)        BACKGROUND ACTOR                             
*                                                                               
         DC    AL1(11,CTBS)        BACKGROUND ACTOR SPECIAL                     
*                                                                               
         DC    AL1(12,CTBSB)       BACKGROUND ACTOR SILENT                      
*                                                                               
         DC    AL1(13,CTSD)        SOLO DANCER                                  
*                                                                               
         DC    AL1(14,CTGD3)       GROUP DANCER                                 
         DC    AL1(14,CTGD6)       GROUP DANCER                                 
         DC    AL1(14,CTGD9)       GROUP DANCER                                 
*                                                                               
         DC    AL1(15,CTS3D)       SOLO DANCER 3 DAY                            
*                                                                               
         DC    AL1(16,CTG3D)       GROUP DANCERS - 3 DAYS                       
*                                                                               
         DC    AL1(17,CTS5D)       SOLO DANCER 5 DAY                            
*                                                                               
         DC    AL1(18,CTG5D)       GROUP DANCERS - 5 DAYS                       
*                                                                               
         DC    X'FF'                                                            
*              INDUSTRIAL RATES                                                 
*                                                                               
*****    DS    AL4(CAT1),AL4(CAT2)                                              
*                                                                               
INDEXT   DS    0F                                                               
         DC    AL4(11250,11250)    OFF CAMERA SESS - ADDT'L 1/2 HOUR            
*                                  DAY PERFORMER ROW                            
         DC    AL4(93100,17450,17450)     CEILING / OT / DT RATE                
*                                  3-DAY PERFORMER ROW                          
         DC    AL4(279300,17450,23300)    CEILING / OT / DT RATE                
*                                  WEEKLY PERFORMER ROW                         
         DC    AL4(310300,11650,15500)    CEILING / OT / DT RATE                
         DC    AL4(47100,58600)    NARRATOR - ADDITIONAL DAYS                   
*                                                                               
         DC    AL4(11250,11250)    ENTIRE SCRIPT ADD'L 1/2HR                    
*                                                                               
         DC    AL4(20950,20950)    PARTIAL SCRIPT FIRST 1/2HR                   
*                                                                               
         DC    AL4(11250,11250)    ADDITIONAL 1/2HR FOR PRINCIPAL               
*                                                                               
         DC    AL4(6800,7550)      ADDITIONAL 1/2HR FOR NON-PRINCIPAL           
*                                                                               
         DC    AL4(20950,20950)    RETAKES FOR PRINCIPAL - 1/2HR RATE           
*                                                                               
         DC    AL4(2650,2650)      STEP OUT PREMIUM                             
*                                                                               
         DC    AL4(23050,25400)    P3M FIRST 1/2 HR RATE                        
*                                                                               
         DC    AL4(47100,58600)    PHD DAY RATE                                 
*                                                                               
         DC    AL4(10700,10700)    IVR ADD'L 1/2 HOUR RATE                      
*                                                                               
         DC    XL4'FFFFFFFF'                                                    
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
**PAN#1  DC    CL21'016TAGEN63   09/06/13'                                      
         END                                                                    
