*          DATA SET TAGEN62    AT LEVEL 012 AS OF 10/05/11                      
*PHASE T70262C,*                                                                
         TITLE 'T00A8F - TABLES FOR 2003 CONTRACT'                              
TACURR   CSECT                                                                  
         DC    AL4(USETBLS-TACURR)                                              
         DC    AL4(USELUT-TACURR)                                               
         DC    AL4(MAJLUT-TACURR)                                               
         DC    AL4(AFMCOLS-TACURR)                                              
         DC    AL4(RADCOLS-TACURR)                                              
         DC    AL4(OFFCOLS-TACURR)                                              
         DC    AL4(ONCOLS-TACURR)                                               
         DC    AL4(MSWEET-TACURR)                                               
         DC    AL4(TAGFEE-TACURR)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
*                                                                               
USETBLS  DS    0F                                                               
CLATBL   DC    AL2(0,44,1,1,0,0)    CLASS A USE 1                               
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(53500)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(40225)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(39165)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(34675)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(28675)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(22690)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(19690)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(16055)          'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    AL4(29505)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(22185)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(21605)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(19125)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(15880)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(12515)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(10855)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(8855)           'OFF' 1-4M9,1-4S9,D9,S9                      
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
         DC    AL4(150180)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(116195)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(124310)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(111020)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(91200)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(75560)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(65760)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(53800)          'OFF' 1-4M9,1-4S9,D9,S9                      
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
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(53500)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(40225)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(39165)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(34675)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(28675)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(22690)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(19690)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(16055)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(1831)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1427)                                                        
         DC    AL4(1231)                                                        
         DC    AL4(1006)                                                        
         DC    AL4(506)                                                         
         DC    AL4(399)                                                         
         DC    AL4(332)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(736)                                                         
         DC    AL4(624)                                                         
         DC    AL4(517)                                                         
         DC    AL4(213)                                                         
         DC    AL4(146)                                                         
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(533)                                                         
         DC    AL4(416)                                                         
         DC    AL4(349)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
*                                                                               
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(264)                                                         
         DC    AL4(213)                                                         
         DC    AL4(186)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(105135)                                                      
         DC    AL4(74275)                                                       
         DC    AL4(67330)                                                       
         DC    AL4(59805)                                                       
         DC    AL4(49005)                                                       
         DC    AL4(27010)                                                       
         DC    AL4(22380)                                                       
         DC    AL4(18325)                                                       
*                                                                               
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(736)                                                         
         DC    AL4(624)                                                         
         DC    AL4(517)                                                         
         DC    AL4(213)                                                         
         DC    AL4(146)                                                         
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(533)                                                         
         DC    AL4(416)                                                         
         DC    AL4(349)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
*                                                                               
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(264)                                                         
         DC    AL4(213)                                                         
         DC    AL4(186)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(91640)                                                       
         DC    AL4(64630)                                                       
         DC    AL4(67330)                                                       
         DC    AL4(59805)                                                       
         DC    AL4(49005)                                                       
         DC    AL4(27010)                                                       
         DC    AL4(22380)                                                       
         DC    AL4(18325)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(736)                                                         
         DC    AL4(624)                                                         
         DC    AL4(517)                                                         
         DC    AL4(213)                                                         
         DC    AL4(146)                                                         
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(533)                                                         
         DC    AL4(416)                                                         
         DC    AL4(349)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
*                                                                               
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(264)                                                         
         DC    AL4(213)                                                         
         DC    AL4(186)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(144685)                                                      
         DC    AL4(97420)                                                       
         DC    AL4(103595)                                                      
         DC    AL4(85655)                                                       
         DC    AL4(70030)                                                       
         DC    AL4(35695)                                                       
         DC    AL4(28750)                                                       
         DC    AL4(23540)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(679)                                                         
         DC    AL4(533)                                                         
         DC    AL4(264)                                                         
         DC    AL4(213)                                                         
         DC    AL4(186)                                                         
         DC    AL4(129)                                                         
         DC    AL4(74)                                                          
         DC    AL4(74)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(174520)                                                      
         DC    AL4(123955)                                                      
         DC    AL4(130695)                                                      
         DC    AL4(111850)                                                      
         DC    AL4(91420)                                                       
         DC    AL4(43035)                                                       
         DC    AL4(34705)                                                       
         DC    AL4(28360)                                                       
*                                                                               
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(696)                                                         
         DC    AL4(545)                                                         
         DC    AL4(270)                                                         
         DC    AL4(219)                                                         
         DC    AL4(191)                                                         
         DC    AL4(134)                                                         
         DC    AL4(79)                                                          
         DC    AL4(79)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(23540)          ANN ALONE                                    
         DC    AL4(23540)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(17340)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(15345)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(13615)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(7915)           SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(180)                                                         
         DC    AL4(154)                                                         
         DC    AL4(136)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(154)                                                         
         DC    AL4(118)                                                         
         DC    AL4(118)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(87)                                                          
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(35240)                                                       
         DC    AL4(35240)                                                       
         DC    AL4(19165)                                                       
         DC    AL4(17015)                                                       
         DC    AL4(15105)                                                       
         DC    AL4(7915)                                                        
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(154)                                                         
         DC    AL4(129)                                                         
         DC    AL4(124)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(87)                                                          
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(31960)                                                       
         DC    AL4(31960)                                                       
         DC    AL4(19165)                                                       
         DC    AL4(17015)                                                       
         DC    AL4(15105)                                                       
         DC    AL4(7915)                                                        
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(154)                                                         
         DC    AL4(129)                                                         
         DC    AL4(124)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(87)                                                          
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(42980)                                                       
         DC    AL4(42980)                                                       
         DC    AL4(22885)                                                       
         DC    AL4(17560)                                                       
         DC    AL4(15625)                                                       
         DC    AL4(7915)                                                        
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(129)                                                         
         DC    AL4(129)                                                         
         DC    AL4(124)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(87)                                                          
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(54310)                                                       
         DC    AL4(54310)                                                       
         DC    AL4(25500)                                                       
         DC    AL4(19730)                                                       
         DC    AL4(17560)                                                       
         DC    AL4(7915)                                                        
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(129)                                                         
         DC    AL4(129)                                                         
         DC    AL4(124)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(260)                                                         
         DC    AL4(260)                                                         
         DC    AL4(87)                                                          
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(23540) ANN ALONE                                    
         DC    AL1(100),AL3(23540) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(17340) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(15345) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(13615) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL1(80),AL3(346)                                                 
         DC    AL1(80),AL3(346)                                                 
         DC    AL1(95),AL3(180)                                                 
         DC    AL1(95),AL3(154)                                                 
         DC    AL1(95),AL3(136)                                                 
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(154)                                                 
         DC    AL1(95),AL3(118)                                                 
         DC    AL1(95),AL3(118)                                                 
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(87)                                                  
         DC    AL1(95),AL3(75)                                                  
         DC    AL1(95),AL3(75)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL1(80),AL3(35240)                                               
         DC    AL1(80),AL3(35240)                                               
         DC    AL1(95),AL3(19165)                                               
         DC    AL1(95),AL3(17015)                                               
         DC    AL1(95),AL3(15105)                                               
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(154)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(124)                                                 
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(87)                                                  
         DC    AL1(95),AL3(75)                                                  
         DC    AL1(95),AL3(75)                                                  
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(31960)                                               
         DC    AL1(80),AL3(31960)                                               
         DC    AL1(95),AL3(19165)                                               
         DC    AL1(95),AL3(17015)                                               
         DC    AL1(95),AL3(15105)                                               
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(154)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(124)                                                 
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(87)                                                  
         DC    AL1(95),AL3(75)                                                  
         DC    AL1(95),AL3(75)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL1(80),AL3(42980)                                               
         DC    AL1(80),AL3(42980)                                               
         DC    AL1(95),AL3(22885)                                               
         DC    AL1(95),AL3(17560)                                               
         DC    AL1(95),AL3(15625)                                               
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(124)                                                 
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(87)                                                  
         DC    AL1(95),AL3(75)                                                  
         DC    AL1(95),AL3(75)                                                  
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL1(80),AL3(54310)                                               
         DC    AL1(80),AL3(54310)                                               
         DC    AL1(95),AL3(25500)                                               
         DC    AL1(95),AL3(19730)                                               
         DC    AL1(95),AL3(17560)                                               
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(129)                                                 
         DC    AL1(95),AL3(124)                                                 
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(80),AL3(260)                                                 
         DC    AL1(95),AL3(87)                                                  
         DC    AL1(95),AL3(75)                                                  
         DC    AL1(95),AL3(75)                                                  
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
         DC    AL4(29885)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(29885)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(29885)          1-4M9,1-4S9,D9,S9                            
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
         DC    AL4(70745)                                                       
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
         DC    AL2(375,24,0,0,0,0) 04 REUSE                                     
         DC    AL4(8250)           CAST=1                                       
         DC    AL4(8250)                2-4                                     
         DC    AL4(8250)                5+                                      
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
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
*                                                                               
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(23540)          ANN ALONE                                    
         DC    AL4(23540)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(17340)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(15345)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(13615)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(18100)          SE                                           
         DC    AL4(8080)           C3,C6                                        
         DC    AL4(12925)          C9                                           
*                                                                               
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(53500)              PRINCIPAL ON  CAMERA                     
         DC    AL4(40225)                  "     OFF   "                        
         DC    AL4(39165)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(34675)                "    6-8    "                          
         DC    AL4(28675)                "     9+    "                          
         DC    AL4(22690)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(19690)                "    6-8    "                          
         DC    AL4(16055)                "     9+    "                          
         DC    AL4(29180)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(44530)              HAND MODEL UNLIMITED                     
         DC    AL4(16940)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(29620)              HAND MODEL 13 WEEKS                      
         DC    AL4(82400)    PIL       PILOT LOCATION RATE                      
         DC    AL4(63360)    PI        PILOT STUDIO RATE                        
         DC    AL4(34695)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(8575)     C3,C6     CONTRACTORS                              
         DC    AL4(16915)    C9             "                                   
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(53500)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(40225)                                               
         DC    AL1(50),AL3(39165)                                               
         DC    AL1(50),AL3(34675)                                               
         DC    AL1(50),AL3(28675)                                               
         DC    AL1(50),AL3(22690)                                               
         DC    AL1(50),AL3(19690)                                               
         DC    AL1(50),AL3(16055)                                               
         DC    AL1(50),AL3(29180)                                               
         DC    AL1(50),AL3(44530)                                               
         DC    AL1(50),AL3(16940)                                               
         DC    AL1(50),AL3(29620)                                               
         DC    AL1(50),AL3(82400)          PIL                                  
         DC    AL1(50),AL3(63360)          PI                                   
         DC    AL1(50),AL3(34695)          SE                                   
         DC    AL1(50),AL3(8575)           C3,C6                                
         DC    AL1(50),AL3(16915)          C9                                   
*                                                                               
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22690)                                                       
         DC    AL4(19690)                                                       
         DC    AL4(16055)                                                       
         DC    AL4(29180)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(29180)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(82400)          PIL                                          
         DC    AL4(63360)          PI                                           
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
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
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
*                                                                               
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(53500)                                               
         DC    AL1(50),AL3(40225)                                               
         DC    AL1(50),AL3(39165)                                               
         DC    AL1(50),AL3(34675)                                               
         DC    AL1(50),AL3(28675)                                               
         DC    AL1(50),AL3(22690)                                               
         DC    AL1(50),AL3(19690)                                               
         DC    AL1(50),AL3(16055)                                               
         DC    AL1(50),AL3(29180)                                               
         DC    AL1(50),AL3(44530)                                               
         DC    AL1(50),AL3(16940)                                               
         DC    AL1(50),AL3(29620)                                               
         DC    AL1(50),AL3(82400)  PIL                                          
         DC    AL1(50),AL3(63360)  PI                                           
         DC    AL1(50),AL3(34695)  SE                                           
         DC    AL1(50),AL3(8575)   C3,C6                                        
         DC    AL1(50),AL3(16915)  C9                                           
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(53500)                                              
         DC    AL1(200),AL3(40225)                                              
         DC    AL1(200),AL3(39165)                                              
         DC    AL1(200),AL3(34675)                                              
         DC    AL1(200),AL3(28675)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(16940)                                              
         DC    AL1(200),AL3(29620)                                              
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV  (3X SESSION)                
         DC    AL4(160500)             PRINCIPAL ON  CAMERA                     
         DC    AL4(120675)                 "     OFF   "                        
         DC    AL4(117495)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(104025)               "    6-8    "                          
         DC    AL4(86025)                "     9+    "                          
         DC    AL4(68070)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(59070)                "    6-8    "                          
         DC    AL4(48165)                "     9+    "                          
         DC    AL4(87540)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(133590)             HAND MODEL UNLIMITED                     
         DC    AL4(50820)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(88860)              HAND MODEL 13 WEEKS                      
         DC    AL4(247200)   PIL       PILOT LOCATION RATE                      
         DC    AL4(190080)   PI        PILOT STUDIO RATE                        
         DC    AL4(104085)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(25725)    C3,C6     CONTRACTORS                              
         DC    AL4(50745)    C9             "                                   
*                                                                               
         DC    AL2(77,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(70620)          ANN ALONE                                    
         DC    AL4(70620)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(52020)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(46035)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(40845)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(54300)          SE                                           
         DC    AL4(24240)          C3,C6                                        
         DC    AL4(38775)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    INTERNET TV (3X SESSION)                 
         DC    AL4(160500)             PRINCIPAL ON  CAMERA                     
         DC    AL4(120675)                 "     OFF   "                        
         DC    AL4(117495)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(104025)               "    6-8    "                          
         DC    AL4(86025)                "     9+    "                          
         DC    AL4(68070)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(59070)                "    6-8    "                          
         DC    AL4(48165)                "     9+    "                          
         DC    AL4(87540)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(133590)             HAND MODEL UNLIMITED                     
         DC    AL4(50820)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(88860)              HAND MODEL 13 WEEKS                      
         DC    AL4(247200)   PIL       PILOT LOCATION RATE                      
         DC    AL4(190080)   PI        PILOT STUDIO RATE                        
         DC    AL4(104085)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(25725)    C3,C6     CONTRACTORS                              
         DC    AL4(50745)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(70620)          ANN ALONE                                    
         DC    AL4(70620)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(52020)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(46035)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(40845)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(54300)          SE                                           
         DC    AL4(24240)          C3,C6                                        
         DC    AL4(38775)          C9                                           
         EJECT                                                                  
*              CABLE RATES (YEAR 2003)                                          
*                                                                               
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22690)                                                       
         DC    AL4(19690)                                                       
         DC    AL4(16055)                                                       
*                                                                               
         DC    AL2(41,12,2,62)     MINIMUM COVERS UPTO 62                       
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,63,63)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(495)                                                         
         DC    AL4(0)                                                           
         DC    AL4(402)                                                         
         DC    AL4(273)                                                         
         DC    AL4(279)                                                         
         DC    AL4(209)                                                         
         DC    AL4(113)                                                         
         DC    AL4(172)                                                         
*                                                                               
         DC    AL2(41,44,64,71)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(765)                                                         
         DC    AL4(0)                                                           
         DC    AL4(559)                                                         
         DC    AL4(496)                                                         
         DC    AL4(408)                                                         
         DC    AL4(323)                                                         
         DC    AL4(281)                                                         
         DC    AL4(229)                                                         
*                                                                               
         DC    AL2(41,44,72,72)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(765)                                                         
         DC    AL4(195)                                                         
         DC    AL4(559)                                                         
         DC    AL4(496)                                                         
         DC    AL4(408)                                                         
         DC    AL4(323)                                                         
         DC    AL4(281)                                                         
         DC    AL4(229)                                                         
*                                                                               
         DC    AL2(41,44,73,100)                                                
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(765)                                                         
         DC    AL4(510)                                                         
         DC    AL4(559)                                                         
         DC    AL4(496)                                                         
         DC    AL4(408)                                                         
         DC    AL4(323)                                                         
         DC    AL4(281)                                                         
         DC    AL4(229)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(650)                                                         
         DC    AL4(432)                                                         
         DC    AL4(476)                                                         
         DC    AL4(420)                                                         
         DC    AL4(349)                                                         
         DC    AL4(276)                                                         
         DC    AL4(239)                                                         
         DC    AL4(194)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(534)                                                         
         DC    AL4(356)                                                         
         DC    AL4(392)                                                         
         DC    AL4(347)                                                         
         DC    AL4(288)                                                         
         DC    AL4(228)                                                         
         DC    AL4(196)                                                         
         DC    AL4(160)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(67)                                                          
         DC    AL4(44)                                                          
         DC    AL4(50)                                                          
         DC    AL4(44)                                                          
         DC    AL4(37)                                                          
         DC    AL4(29)                                                          
         DC    AL4(25)                                                          
         DC    AL4(19)                                                          
*                                                                               
         DC    AL2(41,44,1001,2000)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(63)                                                          
         DC    AL4(43)                                                          
         DC    AL4(46)                                                          
         DC    AL4(41)                                                          
         DC    AL4(34)                                                          
         DC    AL4(27)                                                          
         DC    AL4(23)                                                          
         DC    AL4(19)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
*                                                                               
         DC    AL2(42,84,1,4,0,0)  DEM (TV)                                     
         DC    AL4(40125)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(20115)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(29375)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(26005)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(21505)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(10165)           'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(10165)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(10165)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(16940)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(29620)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
         DC    AL4(15545)          'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(42,84,5,255,0,0)  DEM (TV)                                   
         DC    AL4(40125)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(20115)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(29375)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(26005)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(21505)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(2541)            'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(2541)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(2541)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(16940)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(29620)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
         DC    AL4(3886)           'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(43,48,1,4,0,0)  DEM (AFT RADIO)                              
         DC    AL4(16220)          ANN ALONE                                    
         DC    AL4(16220)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(10705)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(10705)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(10705)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(18100)          SE                                           
         DC    AL4(8080)           C3,C6                                        
         DC    AL4(12925)          C9                                           
         DC    AL4(16370)          SOLOS AND DUOS                               
*                                                                               
         DC    AL2(43,48,5,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(16220)          ANN ALONE                                    
         DC    AL4(16220)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(2675)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(2675)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(2675)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(18100)          SE                                           
         DC    AL4(8080)           C3,C6                                        
         DC    AL4(12925)          C9                                           
         DC    AL4(4095)           SOLOS AND DUOS                               
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
         DC    AL4(160500)         (3 X 50000)                                  
         DC    AL4(120675)                                                      
         DC    AL4(117495)                                                      
         DC    AL4(104025)                                                      
         DC    AL4(86025)                                                       
         DC    AL4(68070)                                                       
         DC    AL4(59070)                                                       
         DC    AL4(48165)                                                       
         DC    AL4(87540)                                                       
         DC    AL4(133590)                                                      
         DC    AL4(50820)                                                       
         DC    AL4(88860)                                                       
         DC    AL4(247200)         PIL                                          
         DC    AL4(190080)         PI                                           
         DC    AL4(104085)         SE                                           
         DC    AL4(25725)          C3,C6                                        
         DC    AL4(50745)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(107000)                                                      
         DC    AL4(80450)                                                       
         DC    AL4(78330)                                                       
         DC    AL4(69350)                                                       
         DC    AL4(57350)                                                       
         DC    AL4(45380)                                                       
         DC    AL4(39380)                                                       
         DC    AL4(32110)                                                       
         DC    AL4(58360)                                                       
         DC    AL4(89060)                                                       
         DC    AL4(33880)                                                       
         DC    AL4(59240)                                                       
         DC    AL4(164800)          PIL                                         
         DC    AL4(126720)          PI                                          
         DC    AL4(69390)           SE                                          
         DC    AL4(17150)           C3,C6                                       
         DC    AL4(33830)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 8X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(428000)        (8 X 53500)                                   
         DC    AL4(321800)                                                      
         DC    AL4(313320)                                                      
         DC    AL4(277400)                                                      
         DC    AL4(229400)                                                      
         DC    AL4(181520)                                                      
         DC    AL4(157520)                                                      
         DC    AL4(128440)                                                      
         DC    AL4(233440)                                                      
         DC    AL4(356240)                                                      
         DC    AL4(135520)                                                      
         DC    AL4(236960)                                                      
         DC    AL4(659200)         PIL                                          
         DC    AL4(506880)         PI                                           
         DC    AL4(277560)         SE                                           
         DC    AL4(68600)          C3,C6                                        
         DC    AL4(135320)         C9                                           
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
         DC    AL4(193200)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(145290)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(141435)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(125190)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(103545)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(82075)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(71100)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(57965)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(488)                                                         
         DC    AL4(354)                                                         
         DC    AL4(346)                                                         
         DC    AL4(320)                                                         
         DC    AL4(248)                                                         
         DC    AL4(205)                                                         
         DC    AL4(185)                                                         
         DC    AL4(132)                                                         
         EJECT                                                                  
*              ADDENDUM USES                                                    
*                                                                               
ADTTBL   DC    AL2(65,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA                
         DC    AL4(32635)          ON CAMERA                                    
         DC    AL4(24505)          OFF                                          
         DC    AL4(17870)                                                       
         DC    AL4(15835)                                                       
         DC    AL4(13055)                                                       
         DC    AL4(10380)                                                       
         DC    AL4(8990)                                                        
         DC    AL4(7385)                                                        
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(32635)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8210)           C3,C6                                        
         DC    AL4(16195)          C9                                           
         DC    AL4(23325)          SOLO/DUO ON CAM                              
         DC    AL4(17550)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(70,88,0,0,0,0)  1 WEEK - GA                                  
         DC    AL4(34880)          ON CAMERA                                    
         DC    AL4(26215)          OFF                                          
         DC    AL4(19155)                                                       
         DC    AL4(16905)                                                       
         DC    AL4(14015)                                                       
         DC    AL4(11130)                                                       
         DC    AL4(9630)                                                        
         DC    AL4(7920)                                                        
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(34880)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)                                                       
         DC    AL4(8210)                                                        
         DC    AL4(16195)                                                       
         DC    AL4(24955)          SOLO/DUO ON CAM                              
         DC    AL4(18830)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(71,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS               
         DC    AL4(27390)          ON CAMERA                                    
         DC    AL4(20575)          OFF                                          
         DC    AL4(21130)          ON CAMERA GROUPS 3-5                         
         DC    AL4(18115)                           6-8                         
         DC    AL4(14425)                           9+                          
         DC    AL4(9170)           OFF CAMERA GROUPS 3-5                        
         DC    AL4(7490)                             6-8                        
         DC    AL4(5365)                             9+                         
         DC    AL4(17690)          EXTRA UNLIMITED                              
         DC    AL4(23705)          HAND MODEL UNLIMITED                         
         DC    AL4(9985)           EXTRA INITIAL 13WK                           
         DC    AL4(15765)          HAND MODEL INITIAL 13WK                      
         DC    AL4(24500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0,0,0)  TV SESSION - 2 WEEK - NW MULTI MKT           
         DC    AL4(35000)          ON CAMERA                                    
         DC    AL4(26300)          OFF                                          
         DC    AL4(25600)                                                       
         DC    AL4(25600)                                                       
         DC    AL4(25600)                                                       
         DC    AL4(14900)                                                       
         DC    AL4(14900)                                                       
         DC    AL4(14900)                                                       
         DC    AL4(22500)                                                       
         DC    AL4(22500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(13500)                                                       
         DC    AL4(35000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0)      TV SESSION - 2 WEEK - NW SINGLE MKT          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(31800)          ON CAMERA                                    
         DC    AL4(22300)          OFF                                          
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(12300)                                                       
         DC    AL4(31800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(80,88,0,0,0,0)  4 WEEK - GA                                  
         DC    AL4(37235)          ON CAMERA                                    
         DC    AL4(28035)          OFF                                          
         DC    AL4(20330)                                                       
         DC    AL4(18085)                                                       
         DC    AL4(14875)                                                       
         DC    AL4(11770)                                                       
         DC    AL4(10270)                                                       
         DC    AL4(8345)                                                        
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(33065)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)                                                       
         DC    AL4(8210)                                                        
         DC    AL4(16195)                                                       
         DC    AL4(26645)          SOLO/DUO ON CAM                              
         DC    AL4(20010)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(81,80,0,0,0,0)  31 DAY - KS                                  
         DC    AL4(35110)          ON CAMERA                                    
         DC    AL4(26385)          OFF                                          
         DC    AL4(26165)                                                       
         DC    AL4(22250)                                                       
         DC    AL4(17665)                                                       
         DC    AL4(11520)                                                       
         DC    AL4(9060)                                                        
         DC    AL4(6820)                                                        
         DC    AL4(17690)                                                       
         DC    AL4(23705)                                                       
         DC    AL4(9985)                                                        
         DC    AL4(15765)                                                       
         DC    AL4(31400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(90,88,0,0,0,0)  13 WEEK - GA                                 
         DC    AL4(46545)          ON CAMERA                                    
         DC    AL4(34990)          OFF                                          
         DC    AL4(25465)                                                       
         DC    AL4(22575)                                                       
         DC    AL4(18620)                                                       
         DC    AL4(14765)                                                       
         DC    AL4(12840)                                                       
         DC    AL4(10485)                                                       
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(16050)                                                       
         DC    AL4(26750)                                                       
         DC    AL4(41300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)                                                       
         DC    AL4(8210)                                                        
         DC    AL4(16195)                                                       
         DC    AL4(33275)          SOLO/DUO ON CAM                              
         DC    AL4(25040)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(91,80,0,0,0,0)  13 WEEKS - KS                                
         DC    AL4(42830)          ON CAMERA                                    
         DC    AL4(32090)          OFF                                          
         DC    AL4(30970)                                                       
         DC    AL4(26385)                                                       
         DC    AL4(21135)                                                       
         DC    AL4(13530)                                                       
         DC    AL4(10955)                                                       
         DC    AL4(8165)                                                        
         DC    AL4(17690)                                                       
         DC    AL4(23705)                                                       
         DC    AL4(9985)                                                        
         DC    AL4(15765)                                                       
         DC    AL4(38300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(92,80,0,0,0,0)  13 WEEKS - TX                                
         DC    AL4(42800)          ON CAMERA                                    
         DC    AL4(32180)          OFF                                          
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
         DC    AL4(18150)                                                       
         DC    AL4(18150)                                                       
         DC    AL4(18150)                                                       
         DC    AL4(23345)                                                       
         DC    AL4(35625)                                                       
         DC    AL4(13550)                                                       
         DC    AL4(23695)                                                       
         DC    AL4(42800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0,0,0)  13 WEEKS - NW MULTI MARKET                   
         DC    AL4(41200)          ON CAMERA                                    
         DC    AL4(31000)          OFF                                          
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(22500)                                                       
         DC    AL4(22500)                                                       
         DC    AL4(16000)                                                       
         DC    AL4(16000)                                                       
         DC    AL4(41200)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0)      13 WEEKS - NW SINGLE MARKET                  
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(37400)          ON CAMERA                                    
         DC    AL4(26300)          OFF                                          
         DC    AL4(22800)                                                       
         DC    AL4(22800)                                                       
         DC    AL4(22800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(21100)                                                       
         DC    AL4(14600)                                                       
         DC    AL4(14600)                                                       
         DC    AL4(37400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
ADOTBL   DC    AL2(100,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA            
         DC    AL4(14340)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(7385)                                                        
         DC    AL4(6420)                                                        
         DC    AL4(5670)                                                        
         DC    AL4(16450)          SE                                           
         DC    AL4(7345)           C3,C6                                        
         DC    AL4(11750)          C9                                           
         DC    AL4(9735)           SOLO/DUO                                     
*                                                                               
         DC    AL2(105,48,0,0,0,0)  1 WEEK - GA                                 
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(7920)                                                        
         DC    AL4(6850)                                                        
         DC    AL4(5990)                                                        
         DC    AL4(16450)          SE                                           
         DC    AL4(7345)           C3,C6                                        
         DC    AL4(11750)          C9                                           
         DC    AL4(10485)                                                       
*                                                                               
         DC    AL2(106,44,0,0,0,0)  1 WEEK - KS                                 
         DC    AL4(11650)                                                       
         DC    AL4(11650)                                                       
         DC    AL4(7295)                                                        
         DC    AL4(6120)                                                        
         DC    AL4(5415)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(108,44,0,0,0,0)  2 WEEK - NW MULTLIPLE MARKETS               
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(108,44,0,0)      2 WEEK - NW SINGLE MARKET                   
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(10300)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(6200)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(115,48,0,0,0,0)  4 WEEK - GA                                 
         DC    AL4(16370)                                                       
         DC    AL4(16370)                                                       
         DC    AL4(8345)                                                        
         DC    AL4(7275)                                                        
         DC    AL4(6420)                                                        
         DC    AL4(16450)          SE                                           
         DC    AL4(7345)           C3,C6                                        
         DC    AL4(11750)          C9                                           
         DC    AL4(11130)                                                       
*                                                                               
         DC    AL2(116,44,0,0,0,0)  31 DAY - KS                                 
         DC    AL4(14950)                                                       
         DC    AL4(14950)                                                       
         DC    AL4(8830)                                                        
         DC    AL4(7770)                                                        
         DC    AL4(6945)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(125,48,0,0,0,0)  13 WEEK - GA                                
         DC    AL4(20435)                                                       
         DC    AL4(20435)                                                       
         DC    AL4(10485)                                                       
         DC    AL4(9095)                                                        
         DC    AL4(8025)                                                        
         DC    AL4(16450)          SE                                           
         DC    AL4(7345)           C3,C6                                        
         DC    AL4(11750)          C9                                           
         DC    AL4(13910)                                                       
*                                                                               
         DC    AL2(126,44,0,0,0,0)  13 WEEK - KS                                
         DC    AL4(18245)                                                       
         DC    AL4(18245)                                                       
         DC    AL4(10240)                                                       
         DC    AL4(9065)                                                        
         DC    AL4(8120)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(127,44,0,0,0,0)  13 WEEK - TX                                
         DC    AL4(18830)                                                       
         DC    AL4(18830)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0,0,0)  13 WEEK - NW  MULTIPLE MARKETS              
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0)      13 WEEK - NW  SINGLE MARKET                 
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(12100)                                                       
         DC    AL4(12100)                                                       
         DC    AL4(7300)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
ADHTAB   DC    AL2(225,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                 
         DC    AL4(46545)                                                       
         DC    AL4(34990)                                                       
         DC    AL4(25465)                                                       
         DC    AL4(22575)                                                       
         DC    AL4(18620)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(33275)                                                       
         DC    AL4(25040)                                                       
*                                                                               
         DC    AL2(226,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                 
         DC    AL4(42830)           ON CAMERA                                   
         DC    AL4(32090)           OFF                                         
         DC    AL4(30970)                                                       
         DC    AL4(26385)                                                       
         DC    AL4(21135)                                                       
*                                                                               
         DC    AL2(227,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                 
         DC    AL4(42800)           ON CAMERA                                   
         DC    AL4(32180)           OFF                                         
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
*                                                                               
         DC    AL2(228,32,0,0,0,0)  ADDENDUM HOLDING RATES - NW                 
         DC    AL4(41200)           ON CAMERA   MULTIPLE MARKET                 
         DC    AL4(31000)           OFF                                         
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
*                                                                               
         DC    AL2(228,32,0,0)      ADDENDUM HOLDING RATES - NW                 
         DC    AL1(1,0,0,0)         SINGLE MARKET                               
         DC    AL4(37400)           ON CAMERA                                   
         DC    AL4(26300)           OFF                                         
         DC    AL4(22800)                                                       
         DC    AL4(22800)                                                       
         DC    AL4(22800)                                                       
*                                                                               
*                                   ADDENDUM REINSTSATEMENT-GA                  
ARNTAB   DC    AL2(230,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                 
         DC    AL1(200),AL3(46545)                                              
         DC    AL1(200),AL3(34990)                                              
         DC    AL1(200),AL3(25465)                                              
         DC    AL1(200),AL3(22575)                                              
         DC    AL1(200),AL3(18620)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(33275)                                              
         DC    AL1(200),AL3(25040)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(231,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(42830)             ON CAMERA                        
         DC    AL1(200),AL3(32090)             OFF                              
         DC    AL1(200),AL3(30970)                                              
         DC    AL1(200),AL3(26385)                                              
         DC    AL1(200),AL3(21135)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(232,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(42800)             ON CAMERA                        
         DC    AL1(200),AL3(32180)             OFF                              
         DC    AL1(200),AL3(31330)                                              
         DC    AL1(200),AL3(31330)                                              
         DC    AL1(200),AL3(31330)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(41200)             ON CAMERA  MULTIPLE MKTS         
         DC    AL1(200),AL3(31000)             OFF                              
         DC    AL1(200),AL3(30200)                                              
         DC    AL1(200),AL3(30200)                                              
         DC    AL1(200),AL3(30200)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0)       - 2X ADDENDUM HOLDING RATES                
         DC    AL1(1,0,0,0)          SINGLE MARKET                              
         DC    AL1(200),AL3(37400)             ON CAMERA                        
         DC    AL1(200),AL3(26300)             OFF                              
         DC    AL1(200),AL3(22800)                                              
         DC    AL1(200),AL3(22800)                                              
         DC    AL1(200),AL3(22800)                                              
*                                                                               
ADDTAB   DC    AL2(205,80,0,0,0,0)  ADDENDUM DEMO (TV) - GA                     
         DC    AL4(34990)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(17550)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(34990)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(34990)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(34990)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(17550)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(17550)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17550)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(8210)           C3,C6                                        
         DC    AL4(16195)          C9                                           
*                                                                               
         DC    AL2(206,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                     
         DC    AL4(9615)           'ON'                                         
         DC    AL4(8495)           'OFF'                                        
         DC    AL4(9615)                                                        
         DC    AL4(9615)                                                        
         DC    AL4(9615)                                                        
         DC    AL4(8495)                                                        
         DC    AL4(8495)                                                        
         DC    AL4(8495)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(207,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                     
         DC    AL4(32100)          'ON'                                         
         DC    AL4(16090)          'OFF'                                        
         DC    AL4(23500)                                                       
         DC    AL4(23500)                                                       
         DC    AL4(23500)                                                       
         DC    AL4(8130)                                                        
         DC    AL4(8130)                                                        
         DC    AL4(8130)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
         DC    AL4(32100)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(16090)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(208,80,0,0,0,0)  ADDENDUM DEMO (TV) - NW                     
         DC    AL4(30900)          'ON'                                         
         DC    AL4(15500)          'OFF'                                        
         DC    AL4(22600)                                                       
         DC    AL4(22600)                                                       
         DC    AL4(22600)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    AL4(8200)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(8800)           EXTRA                                        
         DC    4AL4(0)             N/D                                          
         DC    AL4(7675)           C3,C6                                        
         DC    AL4(15135)          C9                                           
*                                                                               
         DC    AL2(215,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA              
         DC    AL4(14340)          ANN ALONE                                    
         DC    AL4(14340)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(14340)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(14340)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(14340)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(16450)          SE                                           
         DC    AL4(7345)           C3,C6                                        
         DC    AL4(11750)          C9                                           
*                                                                               
         DC    AL2(216,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS              
         DC    AL4(7770)           ANN ALONE                                    
         DC    AL4(7770)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(7770)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7770)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7770)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
*                                                                               
         DC    AL2(217,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX              
         DC    AL4(12975)          ANN ALONE                                    
         DC    AL4(12975)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8565)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8565)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8565)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         DC    AL4(12240)          SOLO/DUO                                     
*                                                                               
         DC    AL2(218,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - NW              
         DC    AL4(7800)          ANN ALONE                                     
         DC    AL4(7800)          AR,AS,P,ANN,1-4MS,1-4SS                       
         DC    AL4(5200)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(5200)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5200)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15375)          SE                                           
         DC    AL4(6865)           C3,C6                                        
         DC    AL4(10980)          C9                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
*                                                                               
ADWTAB   DC    AL2(135,52,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(32635)          ON CAMERA                                    
         DC    AL4(24505)          OFF                                          
         DC    AL4(17870)                                                       
         DC    AL4(15835)                                                       
         DC    AL4(13055)                                                       
         DC    AL4(10380)                                                       
         DC    AL4(8990)                                                        
         DC    AL4(7385)                                                        
         DC    AL4(23325)          SOLO/DUO ON CAMERA                           
         DC    AL4(17550)          OFF CAMERA                                   
*                                                                               
         DC    AL2(135,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1136)                                                        
         DC    AL4(777)                                                         
         DC    AL4(661)                                                         
         DC    AL4(570)                                                         
         DC    AL4(466)                                                         
         DC    AL4(234)                                                         
         DC    AL4(185)                                                         
         DC    AL4(154)                                                         
         DC    AL4(849)                                                         
         DC    AL4(581)                                                         
*                                                                               
         DC    AL2(135,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(421)                                                         
         DC    AL4(330)                                                         
         DC    AL4(341)                                                         
         DC    AL4(288)                                                         
         DC    AL4(239)                                                         
         DC    AL4(98)                                                          
         DC    AL4(67)                                                          
         DC    AL4(62)                                                          
         DC    AL4(315)                                                         
         DC    AL4(247)                                                         
*                                                                               
         DC    AL2(135,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(421)                                                         
         DC    AL4(330)                                                         
         DC    AL4(247)                                                         
         DC    AL4(192)                                                         
         DC    AL4(161)                                                         
         DC    AL4(59)                                                          
         DC    AL4(34)                                                          
         DC    AL4(34)                                                          
         DC    AL4(315)                                                         
         DC    AL4(247)                                                         
*                                                                               
         DC    AL2(140,52,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(34880)          ON CAMERA                                    
         DC    AL4(26215)          OFF                                          
         DC    AL4(19155)                                                       
         DC    AL4(16905)                                                       
         DC    AL4(14015)                                                       
         DC    AL4(11130)                                                       
         DC    AL4(9630)                                                        
         DC    AL4(7920)                                                        
         DC    AL4(24955)                                                       
         DC    AL4(18830)                                                       
*                                                                               
         DC    AL2(140,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1217)                                                        
         DC    AL4(833)                                                         
         DC    AL4(708)                                                         
         DC    AL4(612)                                                         
         DC    AL4(499)                                                         
         DC    AL4(251)                                                         
         DC    AL4(197)                                                         
         DC    AL4(164)                                                         
         DC    AL4(910)                                                         
         DC    AL4(621)                                                         
*                                                                               
         DC    AL2(140,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(451)                                                         
         DC    AL4(355)                                                         
         DC    AL4(365)                                                         
         DC    AL4(310)                                                         
         DC    AL4(256)                                                         
         DC    AL4(105)                                                         
         DC    AL4(72)                                                          
         DC    AL4(66)                                                          
         DC    AL4(338)                                                         
         DC    AL4(265)                                                         
*                                                                               
         DC    AL2(140,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(451)                                                         
         DC    AL4(355)                                                         
         DC    AL4(265)                                                         
         DC    AL4(206)                                                         
         DC    AL4(173)                                                         
         DC    AL4(64)                                                          
         DC    AL4(37)                                                          
         DC    AL4(37)                                                          
         DC    AL4(338)                                                         
         DC    AL4(265)                                                         
*                                                                               
         DC    AL2(141,44,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(27390)          ON CAMERA                                    
         DC    AL4(20575)          OFF                                          
         DC    AL4(21130)                                                       
         DC    AL4(18115)                                                       
         DC    AL4(14425)                                                       
         DC    AL4(9170)                                                        
         DC    AL4(7490)                                                        
         DC    AL4(5365)                                                        
*                                                                               
         DC    AL2(141,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1020)                                                        
         DC    AL4(1020)                                                        
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(143,44,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(35000)          ON CAMERA                                    
         DC    AL4(26300)          OFF                                          
         DC    AL4(25600)                                                       
         DC    AL4(25600)                                                       
         DC    AL4(25600)                                                       
         DC    AL4(14900)                                                       
         DC    AL4(14900)                                                       
         DC    AL4(14900)                                                       
*                                                                               
         DC    AL2(143,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1831)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1253)                                                        
*                                                                               
         DC    AL2(150,52,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(37235)          ON CAMERA                                    
         DC    AL4(28035)          OFF                                          
         DC    AL4(20330)                                                       
         DC    AL4(18085)                                                       
         DC    AL4(14875)                                                       
         DC    AL4(11770)                                                       
         DC    AL4(10270)                                                       
         DC    AL4(8345)                                                        
         DC    AL4(26640)          SOLO/DUO ON CAMERA                           
         DC    AL4(20010)          OFF CAMERA                                   
*                                                                               
         DC    AL2(150,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1298)                                                        
         DC    AL4(888)                                                         
         DC    AL4(755)                                                         
         DC    AL4(652)                                                         
         DC    AL4(532)                                                         
         DC    AL4(267)                                                         
         DC    AL4(211)                                                         
         DC    AL4(175)                                                         
         DC    AL4(970)                                                         
         DC    AL4(663)                                                         
*                                                                               
         DC    AL2(150,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(481)                                                         
         DC    AL4(378)                                                         
         DC    AL4(390)                                                         
         DC    AL4(330)                                                         
         DC    AL4(273)                                                         
         DC    AL4(113)                                                         
         DC    AL4(77)                                                          
         DC    AL4(70)                                                          
         DC    AL4(360)                                                         
         DC    AL4(282)                                                         
*                                                                               
         DC    AL2(150,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(438)                                                         
         DC    AL4(347)                                                         
         DC    AL4(282)                                                         
         DC    AL4(220)                                                         
         DC    AL4(185)                                                         
         DC    AL4(68)                                                          
         DC    AL4(39)                                                          
         DC    AL4(39)                                                          
         DC    AL4(360)                                                         
         DC    AL4(282)                                                         
*                                                                               
         DC    AL2(151,44,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(35110)          ON CAMERA                                    
         DC    AL4(26385)          OFF                                          
         DC    AL4(26165)                                                       
         DC    AL4(22250)                                                       
         DC    AL4(17665)                                                       
         DC    AL4(11520)                                                       
         DC    AL4(9060)                                                        
         DC    AL4(6820)                                                        
*                                                                               
         DC    AL2(151,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1020)                                                        
         DC    AL4(1020)                                                        
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(160,52,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(46545)          ON CAMERA                                    
         DC    AL4(34990)          OFF                                          
         DC    AL4(25465)                                                       
         DC    AL4(22575)                                                       
         DC    AL4(18620)                                                       
         DC    AL4(14765)                                                       
         DC    AL4(12840)                                                       
         DC    AL4(10485)                                                       
         DC    AL4(33275)          SOLO/DUO ON CAMERA                           
         DC    AL4(25040)          OFF                                          
*                                                                               
         DC    AL2(160,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1623)                                                        
         DC    AL4(1110)                                                        
         DC    AL4(944)                                                         
         DC    AL4(815)                                                         
         DC    AL4(666)                                                         
         DC    AL4(334)                                                         
         DC    AL4(264)                                                         
         DC    AL4(219)                                                         
         DC    AL4(1213)                                                        
         DC    AL4(829)                                                         
*                                                                               
         DC    AL2(160,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(602)                                                         
         DC    AL4(472)                                                         
         DC    AL4(487)                                                         
         DC    AL4(413)                                                         
         DC    AL4(342)                                                         
         DC    AL4(141)                                                         
         DC    AL4(96)                                                          
         DC    AL4(88)                                                          
         DC    AL4(450)                                                         
         DC    AL4(353)                                                         
*                                                                               
         DC    AL2(160,52,61,255,0,0)  UNITS 61+                                
         DC    AL4(602)                                                         
         DC    AL4(472)                                                         
         DC    AL4(353)                                                         
         DC    AL4(274)                                                         
         DC    AL4(231)                                                         
         DC    AL4(85)                                                          
         DC    AL4(49)                                                          
         DC    AL4(49)                                                          
         DC    AL4(450)                                                         
         DC    AL4(353)                                                         
*                                                                               
         DC    AL2(161,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                      
         DC    AL4(42830)          ON CAMERA                                    
         DC    AL4(32090)          OFF                                          
         DC    AL4(30970)                                                       
         DC    AL4(26385)                                                       
         DC    AL4(21135)                                                       
         DC    AL4(13530)                                                       
         DC    AL4(10955)                                                       
         DC    AL4(8165)                                                        
*                                                                               
         DC    AL2(161,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1020)                                                        
         DC    AL4(1020)                                                        
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(280)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(162,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                      
         DC    AL4(42800)           ON CAMERA                                   
         DC    AL4(32180)           OFF                                         
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
         DC    AL4(31330)                                                       
         DC    AL4(18150)                                                       
         DC    AL4(18150)                                                       
         DC    AL4(18150)                                                       
*                                                                               
         DC    AL2(162,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1465)                                                        
         DC    AL4(1002)                                                        
         DC    AL4(1142)                                                        
         DC    AL4(1142)                                                        
         DC    AL4(1142)                                                        
         DC    AL4(405)                                                         
         DC    AL4(405)                                                         
         DC    AL4(405)                                                         
*                                                                               
         DC    AL2(162,44,26,255,0,0)  UNITS 26+                                
         DC    AL4(543)                                                         
         DC    AL4(426)                                                         
         DC    AL4(589)                                                         
         DC    AL4(589)                                                         
         DC    AL4(589)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
*                                                                               
         DC    AL2(163,44,1,1,0,0)  13 WEEKS - NW - UNIT 1                      
         DC    AL4(41200)          ON CAMERA                                    
         DC    AL4(31000)          OFF                                          
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
         DC    AL4(30200)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
*                                                                               
         DC    AL2(163,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1831)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1831)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1253)                                                        
         DC    AL4(1253)                                                        
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
*                                                                               
         DC    AL2(170,40,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(14340)          ANN ALONE                                    
         DC    AL4(14340)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7385)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6420)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5670)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(14340)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(9735)           SOLO/DUO                                     
*                                                                               
         DC    AL2(170,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(210)                                                         
         DC    AL4(210)                                                         
         DC    AL4(75)                                                          
         DC    AL4(66)                                                          
         DC    AL4(57)                                                          
         DC    AL4(0)                                                           
         DC    AL4(146)                                                         
*                                                                               
         DC    AL2(170,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(66)                                                          
         DC    AL4(50)                                                          
         DC    AL4(50)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
*                                                                               
         DC    AL2(170,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(158)                                                         
         DC    AL4(158)                                                         
         DC    AL4(36)                                                          
         DC    AL4(31)                                                          
         DC    AL4(31)                                                          
         DC    AL4(0)                                                           
         DC    AL4(111)                                                         
*                                                                               
         DC    AL2(175,40,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(15300)          ANN ALONE                                    
         DC    AL4(15300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(7920)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6850)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5990)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(10485)          SOLO/DUO                                     
*                                                                               
         DC    AL2(175,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(225)                                                         
         DC    AL4(225)                                                         
         DC    AL4(82)                                                          
         DC    AL4(70)                                                          
         DC    AL4(62)                                                          
         DC    AL4(0)                                                           
         DC    AL4(157)                                                         
*                                                                               
         DC    AL2(175,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(169)                                                         
         DC    AL4(169)                                                         
         DC    AL4(70)                                                          
         DC    AL4(53)                                                          
         DC    AL4(53)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
*                                                                               
         DC    AL2(175,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(169)                                                         
         DC    AL4(169)                                                         
         DC    AL4(39)                                                          
         DC    AL4(34)                                                          
         DC    AL4(34)                                                          
         DC    AL4(0)                                                           
         DC    AL4(118)                                                         
*                                                                               
         DC    AL2(176,36,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(11650)                                                       
         DC    AL4(11650)                                                       
         DC    AL4(7295)                                                        
         DC    AL4(6120)                                                        
         DC    AL4(5415)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(176,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(390)                                                         
         DC    AL4(390)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(390)                                                         
*                                                                               
         DC    AL2(178,36,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(15800)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(15800)          SE                                           
*                                                                               
         DC    AL2(178,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
*                                                                               
         DC    AL2(185,40,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(16370)          ANN ALONE                                    
         DC    AL4(16370)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(8345)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7275)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6420)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(16370)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(11130)          SOLO/DUO                                     
*                                                                               
         DC    AL2(185,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(240)                                                         
         DC    AL4(240)                                                         
         DC    AL4(87)                                                          
         DC    AL4(74)                                                          
         DC    AL4(66)                                                          
         DC    AL4(0)                                                           
         DC    AL4(167)                                                         
*                                                                               
         DC    AL2(185,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(74)                                                          
         DC    AL4(57)                                                          
         DC    AL4(57)                                                          
         DC    AL4(0)                                                           
         DC    AL4(126)                                                         
*                                                                               
         DC    AL2(185,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(41)                                                          
         DC    AL4(36)                                                          
         DC    AL4(36)                                                          
         DC    AL4(0)                                                           
         DC    AL4(126)                                                         
*                                                                               
         DC    AL2(186,36,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(14950)                                                       
         DC    AL4(14950)                                                       
         DC    AL4(8830)                                                        
         DC    AL4(7770)                                                        
         DC    AL4(6945)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(186,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(390)                                                         
         DC    AL4(390)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(390)                                                         
*                                                                               
         DC    AL2(195,40,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(20435)          ANN ALONE                                    
         DC    AL4(20435)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(10485)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(9095)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8025)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(20435)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(13910)          SOLO/DUO                                     
*                                                                               
         DC    AL2(195,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(300)                                                         
         DC    AL4(300)                                                         
         DC    AL4(109)                                                         
         DC    AL4(94)                                                          
         DC    AL4(82)                                                          
         DC    AL4(0)                                                           
         DC    AL4(209)                                                         
*                                                                               
         DC    AL2(195,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(225)                                                         
         DC    AL4(225)                                                         
         DC    AL4(94)                                                          
         DC    AL4(71)                                                          
         DC    AL4(71)                                                          
         DC    AL4(0)                                                           
         DC    AL4(158)                                                         
*                                                                               
         DC    AL2(195,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(225)                                                         
         DC    AL4(225)                                                         
         DC    AL4(52)                                                          
         DC    AL4(44)                                                          
         DC    AL4(44)                                                          
         DC    AL4(0)                                                           
         DC    AL4(158)                                                         
*                                                                               
         DC    AL2(196,36,1,1,0,0)  13 WEEK - KS - UNIT 1                       
         DC    AL4(18245)                                                       
         DC    AL4(18245)                                                       
         DC    AL4(10240)                                                       
         DC    AL4(9065)                                                        
         DC    AL4(8120)                                                        
         DC    AL4(15375)          SE                                           
*                                                                               
         DC    AL2(196,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(390)                                                         
         DC    AL4(390)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(160)                                                         
         DC    AL4(390)                                                         
*                                                                               
         DC    AL2(197,36,1,1,0,0)  13 WEEK - TX - UNIT 1                       
         DC    AL4(18830)                                                       
         DC    AL4(18830)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(13870)                                                       
         DC    AL4(18830)                                                       
*                                                                               
         DC    AL2(197,36,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(277)                                                         
         DC    AL4(277)                                                         
         DC    AL4(144)                                                         
         DC    AL4(144)                                                         
         DC    AL4(144)                                                         
         DC    AL4(277)                                                         
*                                                                               
         DC    AL2(197,36,26,255,0,0)  UNITS 26+                                
         DC    AL4(208)                                                         
         DC    AL4(208)                                                         
         DC    AL4(123)                                                         
         DC    AL4(123)                                                         
         DC    AL4(123)                                                         
         DC    AL4(208)                                                         
*                                                                               
         DC    AL2(198,36,1,1,0,0)  13 WEEK - NW - UNIT 1                       
         DC    AL4(18600)                                                       
         DC    AL4(18600)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(10300)                                                       
         DC    AL4(18600)                                                       
*                                                                               
         DC    AL2(198,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         DC    AL4(346)                                                         
         EJECT                                                                  
*              ADDENDUM CABLE                                                   
*                                                                               
         DC    AL2(262,44,0,0,0,0)  1-50,000 SUBSCRIBERS TX                     
         DC    AL4(2365)            PRINCIPAL ON CAMERA                         
         DC    AL4(1615)              "     OFF CAMERA                          
         DC    AL4(1850)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1595)              "   6-8 "    "                            
         DC    AL4(1295)              "    9+ "    "                            
         DC    AL4(660)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(510)               "   6-8  "    "                           
         DC    AL4(430)               "    9+  "    "                           
*                                                                               
         DC    AL2(263,44,0,0,0,0)  1-50,000 SUBSCRIBERS NW                     
         DC    AL4(570)             PRINCIPAL ON CAMERA                         
         DC    AL4(400)               "     OFF CAMERA                          
         DC    AL4(450)             GROUP 3-5 ON CAMERA                         
         DC    AL4(390)               "   6-8 "    "                            
         DC    AL4(315)               "    9+ "    "                            
         DC    AL4(165)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(130)               "   6-8  "    "                           
         DC    AL4(110)               "    9+  "    "                           
*                                                                               
         DC    AL2(272,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS TX               
         DC    AL4(4745)                                                        
         DC    AL4(3245)                                                        
         DC    AL4(3695)                                                        
         DC    AL4(3185)                                                        
         DC    AL4(2595)                                                        
         DC    AL4(1310)                                                        
         DC    AL4(1025)                                                        
         DC    AL4(860)                                                         
*                                                                               
         DC    AL2(273,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS NW               
         DC    AL4(1145)                                                        
         DC    AL4(785)                                                         
         DC    AL4(965)                                                         
         DC    AL4(775)                                                         
         DC    AL4(635)                                                         
         DC    AL4(325)                                                         
         DC    AL4(250)                                                         
         DC    AL4(210)                                                         
*                                                                               
         DC    AL2(282,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS TX              
         DC    AL4(7110)                                                        
         DC    AL4(4865)                                                        
         DC    AL4(5550)                                                        
         DC    AL4(4780)                                                        
         DC    AL4(3890)                                                        
         DC    AL4(1965)                                                        
         DC    AL4(1540)                                                        
         DC    AL4(1290)                                                        
*                                                                               
         DC    AL2(283,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS NW              
         DC    AL4(1720)                                                        
         DC    AL4(1180)                                                        
         DC    AL4(1340)                                                        
         DC    AL4(1160)                                                        
         DC    AL4(945)                                                         
         DC    AL4(480)                                                         
         DC    AL4(375)                                                         
         DC    AL4(320)                                                         
*                                                                               
         DC    AL2(292,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS TX              
         DC    AL4(9480)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(7395)                                                        
         DC    AL4(6370)                                                        
         DC    AL4(5195)                                                        
         DC    AL4(2640)                                                        
         DC    AL4(2050)                                                        
         DC    AL4(1715)                                                        
*                                                                               
         DC    AL2(293,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS NW              
         DC    AL4(2290)                                                        
         DC    AL4(1635)                                                        
         DC    AL4(1790)                                                        
         DC    AL4(1545)                                                        
         DC    AL4(1260)                                                        
         DC    AL4(640)                                                         
         DC    AL4(500)                                                         
         DC    AL4(420)                                                         
*                                                                               
         DC    AL2(302,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS TX              
         DC    AL4(11845)                                                       
         DC    AL4(8110)                                                        
         DC    AL4(9240)                                                        
         DC    AL4(7960)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(3285)                                                        
         DC    AL4(2575)                                                        
         DC    AL4(2155)                                                        
*                                                                               
         DC    AL2(303,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS NW              
         DC    AL4(2865)                                                        
         DC    AL4(1960)                                                        
         DC    AL4(2240)                                                        
         DC    AL4(1930)                                                        
         DC    AL4(1565)                                                        
         DC    AL4(800)                                                         
         DC    AL4(625)                                                         
         DC    AL4(525)                                                         
*                                                                               
         DC    AL2(312,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS TX              
         DC    AL4(23705)                                                       
         DC    AL4(16230)                                                       
         DC    AL4(18495)                                                       
         DC    AL4(15925)                                                       
         DC    AL4(12980)                                                       
         DC    AL4(6560)                                                        
         DC    AL4(5130)                                                        
         DC    AL4(4295)                                                        
*                                                                               
         DC    AL2(313,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS NW              
         DC    AL4(5725)                                                        
         DC    AL4(3925)                                                        
         DC    AL4(4470)                                                        
         DC    AL4(3850)                                                        
         DC    AL4(3140)                                                        
         DC    AL4(1585)                                                        
         DC    AL4(1240)                                                        
         DC    AL4(1040)                                                        
*                                                                               
         DC    AL2(322,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS TX              
         DC    AL4(35545)                                                       
         DC    AL4(24335)                                                       
         DC    AL4(27735)                                                       
         DC    AL4(23890)                                                       
         DC    AL4(19475)                                                       
         DC    AL4(9850)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(6450)                                                        
*                                                                               
         DC    AL2(323,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS NW              
         DC    AL4(8585)                                                        
         DC    AL4(5880)                                                        
         DC    AL4(6705)                                                        
         DC    AL4(5775)                                                        
         DC    AL4(4705)                                                        
         DC    AL4(2385)                                                        
         DC    AL4(1865)                                                        
         DC    AL4(1560)                                                        
*                                                                               
         DC    AL2(332,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS TX            
         DC    AL4(47395)                                                       
         DC    AL4(32455)                                                       
         DC    AL4(36985)                                                       
         DC    AL4(31855)                                                       
         DC    AL4(25970)                                                       
         DC    AL4(13135)                                                       
         DC    AL4(10265)                                                       
         DC    AL4(8605)                                                        
*                                                                               
         DC    AL2(333,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS NW            
         DC    AL4(11445)                                                       
         DC    AL4(7840)                                                        
         DC    AL4(8930)                                                        
         DC    AL4(7695)                                                        
         DC    AL4(6275)                                                        
         DC    AL4(3175)                                                        
         DC    AL4(2485)                                                        
         DC    AL4(2085)                                                        
*                                                                               
         DC    AL2(342,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS TX                 
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22690)                                                       
         DC    AL4(19690)                                                       
         DC    AL4(16055)                                                       
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
         DC    AL4(15790)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(0)                                                           
         DC    AL4(15790)                                                       
         DC    AL4(0)                                                           
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
*                                                                               
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(8810)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
*                                                                               
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(4825)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
*                                                                               
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22690)                                                       
         DC    AL4(19690)                                                       
         DC    AL4(16055)                                                       
         DC    AL4(29180)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(16940)                                                       
         DC    AL4(29620)                                                       
         DC    AL4(82400)          PIL                                          
         DC    AL4(63360)          PI                                           
         DC    AL4(34695)          SE                                           
         DC    AL4(8575)           C3,C6                                        
         DC    AL4(16915)          C9                                           
*                                                                               
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(15790)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(15790)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
         DC    AL4(11965)                                                       
*                                                                               
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(8810)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(8810)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
         DC    AL4(6640)                                                        
*                                                                               
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(4825)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(4825)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
         DC    AL4(3620)                                                        
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(23540)                                                       
         DC    AL4(23540)                                                       
         DC    AL4(17340)                                                       
         DC    AL4(15345)                                                       
         DC    AL4(13615)                                                       
         DC    AL4(18100)                                                       
         DC    AL4(8080)                                                        
         DC    AL4(12925)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
         DC    AL4(9740)                                                        
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
         DC    AL4(6990)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
         DC    AL4(3815)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(85600)          (1.6 X 53500, BUT WANT NEAREST .05)          
         DC    AL4(64360)          (1.6 X 40225, BUT WANT NEAREST .05)          
         DC    AL4(62665)          (1.6 X 39165, BUT WANT NEAREST .05)          
         DC    AL4(55480)          (1.6 X 34675, BUT WANT NEAREST .05)          
         DC    AL4(45880)          (1.6 X 28675, BUT WANT NEAREST .05)          
         DC    AL4(36305)          (1.6 X 22690, BUT WANT NEAREST .05)          
         DC    AL4(31505)          (1.6 X 19690, BUT WANT NEAREST .05)          
         DC    AL4(25690)          (1.6 X 16055, BUT WANT NEAREST .05)          
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(37665)          (1.6 X 23540, BUT WANT NEAREST .05)          
         DC    AL4(37665)          (1.6 X 23540, BUT WANT NEAREST .05)          
         DC    AL4(27745)          (1.6 X 17340, BUT WANT NEAREST .05)          
         DC    AL4(24550)          (1.6 X 15345, BUT WANT NEAREST .05)          
         DC    AL4(21785)          (1.6 X 13615, BUT WANT NEAREST .05)          
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
*                                                                               
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(2365)            PRINCIPAL ON CAMERA                         
         DC    AL4(1615)                "     OFF CAMERA                        
         DC    AL4(1850)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1595)              "   6-8 "    "                            
         DC    AL4(1295)              "    9+ "    "                            
         DC    AL4(660)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(510)               "   6-8  "    "                           
         DC    AL4(430)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(4745)                                                        
         DC    AL4(3245)                                                        
         DC    AL4(3695)                                                        
         DC    AL4(3185)                                                        
         DC    AL4(2595)                                                        
         DC    AL4(1310)                                                        
         DC    AL4(1025)                                                        
         DC    AL4(860)                                                         
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(7110)                                                        
         DC    AL4(4865)                                                        
         DC    AL4(5550)                                                        
         DC    AL4(4780)                                                        
         DC    AL4(3890)                                                        
         DC    AL4(1965)                                                        
         DC    AL4(1540)                                                        
         DC    AL4(1290)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(9480)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(7395)                                                        
         DC    AL4(6370)                                                        
         DC    AL4(5195)                                                        
         DC    AL4(2640)                                                        
         DC    AL4(2050)                                                        
         DC    AL4(1715)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(11845)                                                       
         DC    AL4(8110)                                                        
         DC    AL4(9240)                                                        
         DC    AL4(7960)                                                        
         DC    AL4(6490)                                                        
         DC    AL4(3285)                                                        
         DC    AL4(2570)                                                        
         DC    AL4(2155)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(23705)                                                       
         DC    AL4(16230)                                                       
         DC    AL4(18495)                                                       
         DC    AL4(15925)                                                       
         DC    AL4(12980)                                                       
         DC    AL4(6560)                                                        
         DC    AL4(5130)                                                        
         DC    AL4(4295)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(35545)                                                       
         DC    AL4(24335)                                                       
         DC    AL4(27735)                                                       
         DC    AL4(23890)                                                       
         DC    AL4(19475)                                                       
         DC    AL4(9850)                                                        
         DC    AL4(7700)                                                        
         DC    AL4(6450)                                                        
*        SPACE 1                                                                
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(47395)                                                       
         DC    AL4(32455)                                                       
         DC    AL4(36985)                                                       
         DC    AL4(31855)                                                       
         DC    AL4(25970)                                                       
         DC    AL4(13135)                                                       
         DC    AL4(10265)                                                       
         DC    AL4(8605)                                                        
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(53500)                                                       
         DC    AL4(40225)                                                       
         DC    AL4(39165)                                                       
         DC    AL4(34675)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22690)                                                       
         DC    AL4(19690)                                                       
         DC    AL4(16055)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
*                                                                               
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(12630)          ON CAMERA                                    
         DC    AL4(9570)           OFF                                          
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(9570)                                                        
         DC    AL4(9570)                                                        
         DC    AL4(9570)                                                        
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(12630)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9570)           SE                                           
         DC    AL4(9570)           C3,C6                                        
         DC    AL4(9570)           C9                                           
*                                                                               
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(7050)           ON CAMERA                                    
         DC    AL4(5310)           OFF                                          
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(5310)                                                        
         DC    AL4(5310)                                                        
         DC    AL4(5310)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5310)           SE                                           
         DC    AL4(5310)           C3,C6                                        
         DC    AL4(5310)           C9                                           
*                                                                               
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(3860)           ON CAMERA                                    
         DC    AL4(2895)           OFF                                          
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(2895)                                                        
         DC    AL4(2895)                                                        
         DC    AL4(2895)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(3860)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2895)           SE                                           
         DC    AL4(2895)           C3,C6                                        
         DC    AL4(2895)           C9                                           
*                                                                               
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
*                                                                               
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
         DC    AL4(5590)                                                        
*                                                                               
         DC    AL2(248,44,50,255,0,0)  UNITS 50+                                
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         DC    AL4(3050)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
*                                                                               
         DC    AL2(249,88,1,4,0,0)  GA - TV, REGULAR, UNITS 1-4                 
         DC    AL4(13695)          ON CAMERA                                    
         DC    AL4(10380)          OFF                                          
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(10380)                                                       
         DC    AL4(10380)                                                       
         DC    AL4(10380)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(13695)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10380)          SE                                           
         DC    AL4(10380)          C3,C6                                        
         DC    AL4(10380)          C9                                           
         DC    AL4(13695)          SOLO/DUO ON CAMERA                           
         DC    AL4(10380)                   OFF                                 
*                                                                               
         DC    AL2(249,88,5,14,0,0)  UNITS 5-14                                 
         DC    AL4(7705)           ON CAMERA                                    
         DC    AL4(5780)           OFF                                          
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(5780)                                                        
         DC    AL4(5780)                                                        
         DC    AL4(5780)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(7705)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5780)           SE                                           
         DC    AL4(5780)           C3,C6                                        
         DC    AL4(5780)           C9                                           
         DC    AL4(7705)           SOLO/DUO ON CAMERA                           
         DC    AL4(5780)           OFF                                          
*                                                                               
         DC    AL2(249,88,15,255,0,0)  UNITS 15+                                
         DC    AL4(4175)           ON CAMERA                                    
         DC    AL4(3105)           OFF                                          
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(3105)                                                        
         DC    AL4(3105)                                                        
         DC    AL4(3105)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(4175)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3105)           SE                                           
         DC    AL4(3105)           C3,C6                                        
         DC    AL4(3105)           C9                                           
         DC    AL4(4175)           SOLO/DUO ON CAMERA                           
         DC    AL4(3105)           OFF                                          
         EJECT                                                                  
*              RATES FOR NORTHWEST ADDENDUM TAGS                                
*                                                                               
         DC    AL2(340,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  2WK            
         DC    AL4(11100)          ON CAMERA                                    
         DC    AL4(8500)           OFF                                          
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8500)           SE                                           
         DC    AL4(8500)           C3,C6                                        
         DC    AL4(8500)           C9                                           
*                                                                               
         DC    AL2(340,80,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(9000)           ON CAMERA                                    
         DC    AL4(6800)           OFF                                          
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6800)           SE                                           
         DC    AL4(6800)           C3,C6                                        
         DC    AL4(6800)           C9                                           
*                                                                               
         DC    AL2(340,80,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(5000)           ON CAMERA                                    
         DC    AL4(3800)           OFF                                          
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(3800)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3800)           SE                                           
         DC    AL4(3800)           C3,C6                                        
         DC    AL4(3800)           C9                                           
*                                                                               
         DC    AL2(340,80,25,255,0,0)  UNITS 26+  2-WK                          
         DC    AL4(2400)           ON CAMERA                                    
         DC    AL4(2000)           OFF                                          
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2000)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2000)           SE                                           
         DC    AL4(2000)           C3,C6                                        
         DC    AL4(2000)           C9                                           
*                                                                               
         DC    AL2(341,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  13WK           
         DC    AL4(13000)          ON CAMERA                                    
         DC    AL4(9800)           OFF                                          
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(9800)                                                        
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9800)           SE                                           
         DC    AL4(9800)           C3,C6                                        
         DC    AL4(9800)           C9                                           
*                                                                               
         DC    AL2(341,80,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(10700)          ON CAMERA                                    
         DC    AL4(8000)           OFF                                          
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(8000)                                                        
         DC    AL4(8000)                                                        
         DC    AL4(8000)                                                        
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(10700)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8000)           SE                                           
         DC    AL4(8000)           C3,C6                                        
         DC    AL4(8000)           C9                                           
*                                                                               
         DC    AL2(341,80,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(5900)           ON CAMERA                                    
         DC    AL4(4500)           OFF                                          
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(4500)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(5900)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4500)           SE                                           
         DC    AL4(4500)           C3,C6                                        
         DC    AL4(4500)           C9                                           
*                                                                               
         DC    AL2(341,80,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(3300)           ON CAMERA                                    
         DC    AL4(2700)           OFF                                          
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(3300)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2700)           SE                                           
         DC    AL4(2700)           C3,C6                                        
         DC    AL4(2700)           C9                                           
*                                                                               
         DC    AL2(342,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 2-WK            
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
         DC    AL4(5300)                                                        
*                                                                               
         DC    AL2(342,44,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
*                                                                               
         DC    AL2(342,44,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(2200)                                                        
*                                                                               
         DC    AL2(342,44,25,255,0,0)  UNITS 26+   2-WK                         
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
         DC    AL4(1200)                                                        
*                                                                               
         DC    AL2(343,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 13-WK           
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
         DC    AL4(6500)                                                        
*                                                                               
         DC    AL2(343,44,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
         DC    AL4(4900)                                                        
*                                                                               
         DC    AL2(343,44,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
*                                                                               
         DC    AL2(343,44,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
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
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(22700)          OFF                                          
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(17500)          OFF                                          
         DC    AL4(17500)          OFF                                          
         DC    AL4(17500)          OFF                                          
         DC    AL4(9400)           EXTRA                                        
         DC    AL4(9400)           EXTRA                                        
         DC    AL4(9400)           EXTRA                                        
         DC    AL4(9400)           EXTRA                                        
         DC    AL4(31400)          ON                                           
         DC    AL4(9400)           SAG ONLY EXTRA                               
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21275)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(251,84,0,0,0,0)  PRR FOR AFT AND SAG                         
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(22700)          OFF                                          
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(31400)          ON CAMERA                                    
         DC    AL4(18500)          OFF                                          
         DC    AL4(18500)          OFF                                          
         DC    AL4(18500)          OFF                                          
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(0)              EXTRA                                        
         DC    AL4(31400)          ON                                           
         DC    AL4(0)              N/D                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21500)          OFF                                          
         DC    AL4(21275)          SOLO/DUO OFF CAM                             
         EJECT                                                                  
*&&DO                                                                           
VNWTAB   DC    AL2(252,44,1,1,0,0)  VNW 1ST USE AT SESSION FEE                  
         DC    AL4(53500)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(40225)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(39165)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(34675)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(28675)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(22690)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(19690)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(16055)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(53500)                                               
         DC    AL1(50),AL3(40225)                                               
         DC    AL1(50),AL3(39165)                                               
         DC    AL1(50),AL3(34675)                                               
         DC    AL1(50),AL3(28675)                                               
         DC    AL1(50),AL3(22690)                                               
         DC    AL1(50),AL3(19690)                                               
         DC    AL1(50),AL3(16055)                                               
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
******  USE NUMBERS 370-373 USED FOR IMS                                        
******  USE NUMBERS 375-382 USED FOR AFM RATES                                  
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
         DC    AL4(13695)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(10380)          GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(8665)           GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(9170)           KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(6370)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4355)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(12630)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(9570)           TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(7790)           TX ADDENDUM RADIO SESSION TAG FEE            
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
**PAN#1  DC    CL21'012TAGEN62   10/05/11'                                      
         END                                                                    
