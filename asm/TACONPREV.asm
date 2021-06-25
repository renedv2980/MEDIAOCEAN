*          DATA SET TACONPREV  AT LEVEL 163 AS OF 11/02/16                      
*PHASE T00A8FC,*                                                                
         TITLE 'T00A8F - TABLES FOR 2013 CONTRACT'                              
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
         DC    AL4(DIOCOLS-TACURR)                                              
         DC    AL4(RTKCOLS-TACURR)                                              
         DC    AL4(INDEXT-TACURR)                                               
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
*                                                                               
USETBLS  DS    0F                                                               
CLATBL   DC    AL2(0,44,1,1,0,0)    CLASS A USE 1                               
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(62775)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(47200)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(45955)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(40690)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(33645)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(26620)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(23105)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(18840)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(14395)                                                       
         DC    AL4(11265)                                                       
         DC    AL4(13340)                                                       
         DC    AL4(11420)                                                       
         DC    AL4(9350)                                                        
         DC    AL4(7240)                                                        
         DC    AL4(6290)                                                        
         DC    AL4(5160)                                                        
*                                                                               
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(11420)                                                       
         DC    AL4(8960)                                                        
         DC    AL4(10445)                                                       
         DC    AL4(9465)                                                        
         DC    AL4(7740)                                                        
         DC    AL4(6765)                                                        
         DC    AL4(5790)                                                        
         DC    AL4(4730)                                                        
*                                                                               
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(11420)                                                       
         DC    AL4(8960)                                                        
         DC    AL4(9860)                                                        
         DC    AL4(8880)                                                        
         DC    AL4(7270)                                                        
         DC    AL4(6175)                                                        
         DC    AL4(5390)                                                        
         DC    AL4(4415)                                                        
*                                                                               
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(5475)                                                        
         DC    AL4(4070)                                                        
         DC    AL4(3405)                                                        
         DC    AL4(2895)                                                        
         DC    AL4(2350)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(2315)                                                        
         DC    AL4(1920)                                                        
*                                                                               
PAXTAB   DC    AL2(75,44,1,255,0,0)   PAX USE                                   
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(2350)           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(1760)           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(1465)           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(1245)           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(1005)           'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(1065)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(990)            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(825)            'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
CLBTAB   DC    AL2(1,44,0,0,0,0)   CLASS B WITH NY                              
         DC    AL4(118775)                                                      
         DC    AL4(84945)                                                       
         DC    AL4(75645)                                                       
         DC    AL4(66890)                                                       
         DC    AL4(54685)                                                       
         DC    AL4(27875)                                                       
         DC    AL4(23235)                                                       
         DC    AL4(18990)                                                       
*                                                                               
CBXTAB   DC    AL2(2,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(96875)                                                       
         DC    AL4(67285)                                                       
         DC    AL4(75645)                                                       
         DC    AL4(66890)                                                       
         DC    AL4(54685)                                                       
         DC    AL4(27875)                                                       
         DC    AL4(23235)                                                       
         DC    AL4(18990)                                                       
*                                                                               
CLCTAB   DC    AL2(3,44,0,0,0,0)   CLASS C                                      
         DC    AL4(57730)                                                       
         DC    AL4(38490)                                                       
         DC    AL4(50030)                                                       
         DC    AL4(44465)                                                       
         DC    AL4(36360)                                                       
         DC    AL4(22170)                                                       
         DC    AL4(18455)                                                       
         DC    AL4(15135)                                                       
*                                                                               
NWKTBL   DC    AL2(58,44,1,1,0,0)  NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(34525)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(25960)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(25275)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(22380)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(18505)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(14640)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(12710)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(10360)          'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
*                                                                               
DANTAB   DC    AL2(4,44,0,0,0,0)   DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(235580)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(164085)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(176890)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(155925)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(121185)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(72315)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(63330)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(45215)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB   DC    AL2(5,44,0,0,0,0)   DEALER A W/O NY                              
         DC    AL4(208345)                                                      
         DC    AL4(150470)                                                      
         DC    AL4(176890)                                                      
         DC    AL4(155925)                                                      
         DC    AL4(121185)                                                      
         DC    AL4(72315)                                                       
         DC    AL4(63330)                                                       
         DC    AL4(45215)                                                       
*                                                                               
DBNTAB   DC    AL2(6,44,0,0,0,0)   CLASS B INCL NY                              
         DC    AL4(362215)                                                      
         DC    AL4(246475)                                                      
         DC    AL4(268940)                                                      
         DC    AL4(237080)                                                      
         DC    AL4(184510)                                                      
         DC    AL4(110165)                                                      
         DC    AL4(96420)                                                       
         DC    AL4(68775)                                                       
*                                                                               
DBXTAB   DC    AL2(7,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(312520)                                                      
         DC    AL4(225360)                                                      
         DC    AL4(268940)                                                      
         DC    AL4(237080)                                                      
         DC    AL4(184510)                                                      
         DC    AL4(110165)                                                      
         DC    AL4(96420)                                                       
         DC    AL4(68775)                                                       
*                                                                               
DANTAB8  DC    AL2(210,44,0,0,0,0) DEALER A INCL NY 8 WEEKS                     
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(117790)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(82040)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(88445)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(77965)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(60590)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(36155)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(31665)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(22605)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB8  DC    AL2(211,44,0,0,0,0) DEALER A W/O NY 8 WEEKS                      
         DC    AL4(104170)                                                      
         DC    AL4(75235)                                                       
         DC    AL4(88445)                                                       
         DC    AL4(77965)                                                       
         DC    AL4(60590)                                                       
         DC    AL4(36155)                                                       
         DC    AL4(31665)                                                       
         DC    AL4(22605)                                                       
*                                                                               
DBNTAB8  DC    AL2(212,44,0,0,0,0) CLASS B INCL NY 8 WEEKS                      
         DC    AL4(181105)                                                      
         DC    AL4(123240)                                                      
         DC    AL4(134470)                                                      
         DC    AL4(118540)                                                      
         DC    AL4(92255)                                                       
         DC    AL4(55085)                                                       
         DC    AL4(48210)                                                       
         DC    AL4(34385)                                                       
*                                                                               
DBXTAB8  DC    AL2(213,44,0,0,0,0) CLASS B W/O NY 8 WEEKS                       
         DC    AL4(156260)                                                      
         DC    AL4(112680)                                                      
         DC    AL4(134470)                                                      
         DC    AL4(118540)                                                      
         DC    AL4(92255)                                                       
         DC    AL4(55080)                                                       
         DC    AL4(48210)                                                       
         DC    AL4(34385)                                                       
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
*                                                                               
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(176215)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(136335)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(145860)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(130270)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(107005)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(88665)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(77165)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(63130)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
*                                                                               
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(10795)                                                       
         DC    AL4(8199)                                                        
         DC    AL4(7892)                                                        
         DC    AL4(6906)                                                        
         DC    AL4(5640)                                                        
         DC    AL4(5214)                                                        
         DC    AL4(4700)                                                        
         DC    AL4(3874)                                                        
*                                                                               
         DC    AL2(8,44,19,255,0,0)  19+ (LOOK AT CLA, 14- EA)                  
         DC    AL4(5475)                                                        
         DC    AL4(4070)                                                        
         DC    AL4(3405)                                                        
         DC    AL4(2895)                                                        
         DC    AL4(2350)                                                        
         DC    AL4(2465)                                                        
         DC    AL4(2315)                                                        
         DC    AL4(1920)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(62775)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(47200)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(45955)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(40690)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(33645)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(26620)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(23105)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(18840)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(2149)                                                        
         DC    AL4(1470)                                                        
         DC    AL4(1675)                                                        
         DC    AL4(1445)                                                        
         DC    AL4(1180)                                                        
         DC    AL4(594)                                                         
         DC    AL4(469)                                                         
         DC    AL4(390)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(864)                                                         
         DC    AL4(731)                                                         
         DC    AL4(606)                                                         
         DC    AL4(250)                                                         
         DC    AL4(172)                                                         
         DC    AL4(157)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(625)                                                         
         DC    AL4(489)                                                         
         DC    AL4(409)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
*                                                                               
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(310)                                                         
         DC    AL4(250)                                                         
         DC    AL4(218)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(123365)                                                      
         DC    AL4(87155)                                                       
         DC    AL4(79000)                                                       
         DC    AL4(70175)                                                       
         DC    AL4(57500)                                                       
         DC    AL4(31695)                                                       
         DC    AL4(26260)                                                       
         DC    AL4(21500)                                                       
*                                                                               
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(864)                                                         
         DC    AL4(731)                                                         
         DC    AL4(606)                                                         
         DC    AL4(250)                                                         
         DC    AL4(172)                                                         
         DC    AL4(157)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(625)                                                         
         DC    AL4(489)                                                         
         DC    AL4(409)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
*                                                                               
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(310)                                                         
         DC    AL4(250)                                                         
         DC    AL4(218)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(107530)                                                      
         DC    AL4(75840)                                                       
         DC    AL4(79000)                                                       
         DC    AL4(70175)                                                       
         DC    AL4(57500)                                                       
         DC    AL4(31695)                                                       
         DC    AL4(26260)                                                       
         DC    AL4(21500)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(864)                                                         
         DC    AL4(731)                                                         
         DC    AL4(606)                                                         
         DC    AL4(250)                                                         
         DC    AL4(172)                                                         
         DC    AL4(157)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(625)                                                         
         DC    AL4(489)                                                         
         DC    AL4(409)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
*                                                                               
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(310)                                                         
         DC    AL4(250)                                                         
         DC    AL4(218)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(169770)                                                      
         DC    AL4(114310)                                                      
         DC    AL4(121555)                                                      
         DC    AL4(100505)                                                      
         DC    AL4(82170)                                                       
         DC    AL4(41880)                                                       
         DC    AL4(33735)                                                       
         DC    AL4(27620)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(797)                                                         
         DC    AL4(625)                                                         
         DC    AL4(310)                                                         
         DC    AL4(250)                                                         
         DC    AL4(218)                                                         
         DC    AL4(152)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(204775)                                                      
         DC    AL4(145445)                                                      
         DC    AL4(153350)                                                      
         DC    AL4(131240)                                                      
         DC    AL4(107270)                                                      
         DC    AL4(50495)                                                       
         DC    AL4(40720)                                                       
         DC    AL4(33275)                                                       
*                                                                               
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(817)                                                         
         DC    AL4(640)                                                         
         DC    AL4(317)                                                         
         DC    AL4(257)                                                         
         DC    AL4(224)                                                         
         DC    AL4(157)                                                         
         DC    AL4(93)                                                          
         DC    AL4(93)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(27860)          ANN ALONE                                    
         DC    AL4(27860)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(20525)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(18165)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(16110)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(9370)           SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(213)                                                         
         DC    AL4(182)                                                         
         DC    AL4(161)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(182)                                                         
         DC    AL4(140)                                                         
         DC    AL4(140)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(103)                                                         
         DC    AL4(89)                                                          
         DC    AL4(89)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(41715)                                                       
         DC    AL4(41715)                                                       
         DC    AL4(22685)                                                       
         DC    AL4(20140)                                                       
         DC    AL4(17875)                                                       
         DC    AL4(9370)                                                        
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(182)                                                         
         DC    AL4(153)                                                         
         DC    AL4(146)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(103)                                                         
         DC    AL4(89)                                                          
         DC    AL4(89)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(37835)                                                       
         DC    AL4(37835)                                                       
         DC    AL4(22685)                                                       
         DC    AL4(20140)                                                       
         DC    AL4(17875)                                                       
         DC    AL4(9370)                                                        
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(182)                                                         
         DC    AL4(153)                                                         
         DC    AL4(146)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(103)                                                         
         DC    AL4(89)                                                          
         DC    AL4(89)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(50880)                                                       
         DC    AL4(50880)                                                       
         DC    AL4(27095)                                                       
         DC    AL4(20785)                                                       
         DC    AL4(18495)                                                       
         DC    AL4(9370)                                                        
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(153)                                                         
         DC    AL4(153)                                                         
         DC    AL4(146)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(103)                                                         
         DC    AL4(89)                                                          
         DC    AL4(89)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(64290)                                                       
         DC    AL4(64290)                                                       
         DC    AL4(30185)                                                       
         DC    AL4(23355)                                                       
         DC    AL4(20785)                                                       
         DC    AL4(9370)                                                        
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(153)                                                         
         DC    AL4(153)                                                         
         DC    AL4(146)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(308)                                                         
         DC    AL4(308)                                                         
         DC    AL4(103)                                                         
         DC    AL4(89)                                                          
         DC    AL4(89)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(27860) ANN ALONE                                    
         DC    AL1(100),AL3(27860) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(20525) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(18165) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(16110) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(328)                                                         
         DC    AL4(328)                                                         
         DC    AL4(202)                                                         
         DC    AL4(173)                                                         
         DC    AL4(153)                                                         
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(173)                                                         
         DC    AL4(133)                                                         
         DC    AL4(133)                                                         
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(98)                                                          
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(33370)                                                       
         DC    AL4(33370)                                                       
         DC    AL4(21550)                                                       
         DC    AL4(19135)                                                       
         DC    AL4(16980)                                                       
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(173)                                                         
         DC    AL4(145)                                                         
         DC    AL4(139)                                                         
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(98)                                                          
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(30270)                                                       
         DC    AL4(30270)                                                       
         DC    AL4(21550)                                                       
         DC    AL4(19135)                                                       
         DC    AL4(16980)                                                       
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(173)                                                         
         DC    AL4(145)                                                         
         DC    AL4(139)                                                         
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(98)                                                          
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(40705)                                                       
         DC    AL4(40705)                                                       
         DC    AL4(25740)                                                       
         DC    AL4(19745)                                                       
         DC    AL4(17570)                                                       
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(145)                                                         
         DC    AL4(145)                                                         
         DC    AL4(139)                                                         
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(98)                                                          
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(51430)                                                       
         DC    AL4(51430)                                                       
         DC    AL4(28675)                                                       
         DC    AL4(22185)                                                       
         DC    AL4(19745)                                                       
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(145)                                                         
         DC    AL4(145)                                                         
         DC    AL4(139)                                                         
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(98)                                                          
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
*                                                                               
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(75345)          AR,AS,P,ANN                                  
         DC    AL4(59770)          S,1-4MS,1-4SS                                
         DC    AL4(38965)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(31180)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(19490)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(19710)          SE                                           
*                                                                               
DLRTAB8  DC    AL2(214,36,0,0,0,0)  DEALER COMMERCIALS 8 WEEKS                  
         DC    AL4(37670)          AR,AS,P,ANN                                  
         DC    AL4(29885)          S,1-4MS,1-4SS                                
         DC    AL4(19485)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(15590)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(9745)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(9855)           SE                                           
*                                                                               
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(47145)          ANN ALONE                                    
         DC    AL4(47145)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(35380)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(35380)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(35380)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(10525)          SE                                           
*                                                                               
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(76485)                                                       
         DC    AL4(76485)                                                       
         DC    AL4(58820)                                                       
         DC    AL4(52595)                                                       
         DC    AL4(48050)                                                       
         DC    AL4(10525)                                                       
*                                                                               
N08TAB   DC    AL2(28,36,0,0,0,0)  NETWORK 8 WEEK                               
         DC    AL4(121835)                                                      
         DC    AL4(121835)                                                      
         DC    AL4(93740)                                                       
         DC    AL4(83740)                                                       
         DC    AL4(75050)                                                       
         DC    AL4(10525)                                                       
*                                                                               
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(151190)                                                      
         DC    AL4(151190)                                                      
         DC    AL4(116285)                                                      
         DC    AL4(103975)                                                      
         DC    AL4(95255)                                                       
         DC    AL4(10525)                                                       
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
*                                                                               
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(158315)                                                      
         DC    AL4(158315)                                                      
         DC    AL4(121745)                                                      
         DC    AL4(108875)                                                      
         DC    AL4(99745)                                                       
         DC    AL4(10525)                                                       
*                                                                               
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(75600)                                                       
         DC    AL4(75600)                                                       
         DC    AL4(58130)                                                       
         DC    AL4(51980)                                                       
         DC    AL4(47500)                                                       
         DC    AL4(10525)                                                       
*                                                                               
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(113850)                                                      
         DC    AL4(113850)                                                      
         DC    AL4(79715)                                                       
         DC    AL4(71160)                                                       
         DC    AL4(64645)                                                       
         DC    AL4(10525)                                                       
*                                                                               
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(91235)                                                       
         DC    AL4(91235)                                                       
         DC    AL4(42765)                                                       
         DC    AL4(42765)                                                       
         DC    AL4(42765)                                                       
         DC    AL4(10525)                                                       
*                                                                               
         DC    AL2(34,36,0,0,0,0)  REGIONAL - WITH ANY MAJORS                   
         DC    AL4(91235)                                                       
         DC    AL4(91235)                                                       
         DC    AL4(91235)                                                       
         DC    AL4(82115)                                                       
         DC    AL4(73850)                                                       
         DC    AL4(10525)                                                       
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
         DC    AL2(415,24,0,0,0,0) 10 REUSE                                     
         DC    AL4(9000)           CAST=1                                       
         DC    AL4(9000)                2-4                                     
         DC    AL4(9000)                5+                                      
*                                                                               
         DC    AL2(423,24,0,0,0,0) 13 REUSE                                     
         DC    AL4(9540)           CAST=1                                       
         DC    AL4(9540)                2-4                                     
         DC    AL4(9540)                5+                                      
*                                                                               
         DC    AL2(431,24,0,0,0,0) 14 REUSE                                     
         DC    AL4(9540)           CAST=1                                       
         DC    AL4(9540)                2-4                                     
         DC    AL4(9540)                5+                                      
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
         DC    AL2(416,24,0,0,0,0)  10 FIRST REUSE                              
         DC    AL4(6800)           CAST=1                                       
         DC    AL4(3400)                2-4                                     
         DC    AL4(3400)                5+                                      
*                                                                               
         DC    AL2(424,24,0,0,0,0)  13 FIRST REUSE                              
         DC    AL4(7210)           CAST=1                                       
         DC    AL4(3605)                2-4                                     
         DC    AL4(3605)                5+                                      
*                                                                               
         DC    AL2(432,24,0,0,0,0)  14 FIRST REUSE                              
         DC    AL4(7208)           CAST=1                                       
         DC    AL4(3604)                2-4                                     
         DC    AL4(3604)                5+                                      
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
         DC    AL2(417,24,0,0,0,0)  10 SESSION                                  
         DC    AL4(12000)          CAST=1                                       
         DC    AL4(12000)               2-4                                     
         DC    AL4(12000)               5+                                      
*                                                                               
         DC    AL2(425,24,0,0,0,0)  13 SESSION                                  
         DC    AL4(12720)          CAST=1                                       
         DC    AL4(12720)               2-4                                     
         DC    AL4(12720)               5+                                      
*                                                                               
         DC    AL2(433,24,0,0,0,0)  14 SESSION                                  
         DC    AL4(12720)          CAST=1                                       
         DC    AL4(12720)               2-4                                     
         DC    AL4(12720)               5+                                      
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
         DC    AL2(418,24,0,0,0,0) 10 8-WEEK REUSE                              
         DC    AL1(80),AL3(9000)                                                
         DC    AL1(80),AL3(9000)                                                
         DC    AL1(80),AL3(9000)                                                
*                                                                               
         DC    AL2(426,24,0,0,0,0) 13 8-WEEK REUSE                              
         DC    AL1(80),AL3(9540)                                                
         DC    AL1(80),AL3(9540)                                                
         DC    AL1(80),AL3(9540)                                                
*                                                                               
         DC    AL2(434,24,0,0,0,0) 14 8-WEEK REUSE                              
         DC    AL1(80),AL3(9540)                                                
         DC    AL1(80),AL3(9540)                                                
         DC    AL1(80),AL3(9540)                                                
*                                                                               
MVMTAB   DC    AL2(500,24,0,0,0,0)  8-WEEK REUSE                                
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
         DC    AL1(80),AL3(7050)                                                
*                                                                               
         DC    AL2(501,24,0,0,0,0)  98 8-WEEK REUSE                             
         DC    AL1(80),AL3(7500)                                                
         DC    AL1(80),AL3(7500)                                                
         DC    AL1(80),AL3(7500)                                                
*                                                                               
         DC    AL2(502,24,0,0,0,0) 01 8-WEEK REUSE                              
         DC    AL1(80),AL3(7950)                                                
         DC    AL1(80),AL3(7950)                                                
         DC    AL1(80),AL3(7950)                                                
*                                                                               
         DC    AL2(503,24,0,0,0,0) 04 8-WEEK REUSE                              
         DC    AL1(80),AL3(8250)                                                
         DC    AL1(80),AL3(8250)                                                
         DC    AL1(80),AL3(8250)                                                
*                                                                               
         DC    AL2(504,24,0,0,0,0) 07 8-WEEK REUSE                              
         DC    AL1(80),AL3(8625)                                                
         DC    AL1(80),AL3(8625)                                                
         DC    AL1(80),AL3(8625)                                                
*                                                                               
         DC    AL2(505,24,0,0,0,0) 10 8-WEEK REUSE                              
         DC    AL1(38),AL3(9000)                                                
         DC    AL1(38),AL3(9000)                                                
         DC    AL1(38),AL3(9000)                                                
*                                                                               
         DC    AL2(506,24,0,0,0,0) 13 8-WEEK REUSE                              
         DC    AL1(38),AL3(9540)                                                
         DC    AL1(38),AL3(9540)                                                
         DC    AL1(38),AL3(9540)                                                
*                                                                               
         DC    AL2(507,24,0,0,0,0) 14 8-WEEK REUSE                              
         DC    AL1(100),AL3(10000)                                              
         DC    AL1(100),AL3(10000)                                              
         DC    AL1(100),AL3(10000)                                              
*                                                                               
                                                                                
MVMTAB1Y DC    AL2(510,24,0,0,0,0)  REUSE 1YR                                   
         DC    AL4(7050)           CAST=1                                       
         DC    AL4(7050)                2-4                                     
         DC    AL4(7050)                5+                                      
*                                                                               
         DC    AL2(511,24,0,0,0,0)  98 REUSE 1YR                                
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
*                                                                               
         DC    AL2(512,24,0,0,0,0) 01 REUSE 1YR                                 
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
*                                                                               
         DC    AL2(513,24,0,0,0,0) 04 REUSE 1YR                                 
         DC    AL4(8250)           CAST=1                                       
         DC    AL4(8250)                2-4                                     
         DC    AL4(8250)                5+                                      
*                                                                               
         DC    AL2(514,24,0,0,0,0) 07 REUSE 1YR                                 
         DC    AL4(8625)           CAST=1                                       
         DC    AL4(8625)                2-4                                     
         DC    AL4(8625)                5+                                      
*                                                                               
         DC    AL2(515,24,0,0,0,0) 10 REUSE 1YR                                 
         DC    AL4(9000)           CAST=1                                       
         DC    AL4(9000)                2-4                                     
         DC    AL4(9000)                5+                                      
*                                                                               
         DC    AL2(516,24,0,0,0,0) 13 REUSE 1YR                                 
         DC    AL4(9540)           CAST=1                                       
         DC    AL4(9540)                2-4                                     
         DC    AL4(9540)                5+                                      
*                                                                               
         DC    AL2(517,24,0,0,0,0) 14 REUSE 1YR                                 
         DC    AL4(30000)          CAST=1                                       
         DC    AL4(30000)               2-4                                     
         DC    AL4(30000)               5+                                      
*                                                                               
                                                                                
NIMTAB6M DC    AL2(520,24,0,0,0,0)  REUSE 6MO                                   
         DC    AL4(7050)           CAST=1                                       
         DC    AL4(7050)                2-4                                     
         DC    AL4(7050)                5+                                      
*                                                                               
         DC    AL2(521,24,0,0,0,0)  98 REUSE 6MO                                
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
*                                                                               
         DC    AL2(522,24,0,0,0,0) 01 REUSE 6MO                                 
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
*                                                                               
         DC    AL2(523,24,0,0,0,0) 04 REUSE 6MO                                 
         DC    AL4(8250)           CAST=1                                       
         DC    AL4(8250)                2-4                                     
         DC    AL4(8250)                5+                                      
*                                                                               
         DC    AL2(524,24,0,0,0,0) 07 REUSE 6MO                                 
         DC    AL4(8625)           CAST=1                                       
         DC    AL4(8625)                2-4                                     
         DC    AL4(8625)                5+                                      
*                                                                               
         DC    AL2(525,24,0,0,0,0) 10 REUSE 6MO                                 
         DC    AL4(9000)           CAST=1                                       
         DC    AL4(9000)                2-4                                     
         DC    AL4(9000)                5+                                      
*                                                                               
         DC    AL2(526,24,0,0,0,0) 13 REUSE 6MO                                 
         DC    AL4(9540)           CAST=1                                       
         DC    AL4(9540)                2-4                                     
         DC    AL4(9540)                5+                                      
*                                                                               
         DC    AL2(527,24,0,0,0,0) 14 REUSE 6MO                                 
         DC    AL4(20000)          CAST=1                                       
         DC    AL4(20000)               2-4                                     
         DC    AL4(20000)               5+                                      
*                                                                               
                                                                                
NIMTAB8W DC    AL2(530,24,0,0,0,0)  REUSE 6MO                                   
         DC    AL4(7050)           CAST=1                                       
         DC    AL4(7050)                2-4                                     
         DC    AL4(7050)                5+                                      
*                                                                               
         DC    AL2(531,24,0,0,0,0)  98 REUSE 6MO                                
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
*                                                                               
         DC    AL2(532,24,0,0,0,0) 01 REUSE 6MO                                 
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
*                                                                               
         DC    AL2(533,24,0,0,0,0) 04 REUSE 6MO                                 
         DC    AL4(8250)           CAST=1                                       
         DC    AL4(8250)                2-4                                     
         DC    AL4(8250)                5+                                      
*                                                                               
         DC    AL2(534,24,0,0,0,0) 07 REUSE 6MO                                 
         DC    AL4(8625)           CAST=1                                       
         DC    AL4(8625)                2-4                                     
         DC    AL4(8625)                5+                                      
*                                                                               
         DC    AL2(535,24,0,0,0,0) 10 REUSE 6MO                                 
         DC    AL4(9000)           CAST=1                                       
         DC    AL4(9000)                2-4                                     
         DC    AL4(9000)                5+                                      
*                                                                               
         DC    AL2(536,24,0,0,0,0) 13 REUSE 6MO                                 
         DC    AL4(9540)           CAST=1                                       
         DC    AL4(9540)                2-4                                     
         DC    AL4(9540)                5+                                      
*                                                                               
         DC    AL2(537,24,0,0,0,0) 14 REUSE 6MO                                 
         DC    AL4(10000)          CAST=1                                       
         DC    AL4(10000)               2-4                                     
         DC    AL4(10000)               5+                                      
*                                                                               
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
         DC    AL2(419,24,0,0,0,0)  10 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
*                                                                               
         DC    AL2(427,24,0,0,0,0)  13 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
*                                                                               
         DC    AL2(435,24,0,0,0,0)  14 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(7950)           CAST=1                                       
         DC    AL4(7950)                2-4                                     
         DC    AL4(7950)                5+                                      
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
         DC    AL2(420,24,0,0,0,0)  10 WORLD - 12M                              
         DC    AL4(12000)          CAST=1                                       
         DC    AL4(12000)               2-4                                     
         DC    AL4(12000)               5+                                      
*                                                                               
         DC    AL2(428,24,0,0,0,0)  13 WORLD - 12M                              
         DC    AL4(12720)          CAST=1                                       
         DC    AL4(12720)               2-4                                     
         DC    AL4(12720)               5+                                      
*                                                                               
         DC    AL2(436,24,0,0,0,0)  14 WORLD - 12M                              
         DC    AL4(12720)          CAST=1                                       
         DC    AL4(12720)               2-4                                     
         DC    AL4(12720)               5+                                      
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
         DC    AL2(421,24,0,0,0,0)  10 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(11250)          CAST=1                                       
         DC    AL4(11250)               2-4                                     
         DC    AL4(11250)               5+                                      
*                                                                               
         DC    AL2(429,24,0,0,0,0)  13 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(11925)          CAST=1                                       
         DC    AL4(11925)               2-4                                     
         DC    AL4(11925)               5+                                      
*                                                                               
         DC    AL2(437,24,0,0,0,0)  14 EUROPE OR OUTSIDE EUROPE-24M             
         DC    AL4(11925)          CAST=1                                       
         DC    AL4(11925)               2-4                                     
         DC    AL4(11925)               5+                                      
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
*                                                                               
         DC    AL2(422,24,0,0,0,0)  10 WORLD - 24M                              
         DC    AL4(18000)          CAST=1                                       
         DC    AL4(18000)              2-4                                      
         DC    AL4(18000)              5+                                       
*                                                                               
         DC    AL2(430,24,0,0,0,0)  13 WORLD - 24M                              
         DC    AL4(19080)          CAST=1                                       
         DC    AL4(19080)              2-4                                      
         DC    AL4(19080)              5+                                       
*                                                                               
         DC    AL2(438,24,0,0,0,0)  14 WORLD - 24M                              
         DC    AL4(19080)          CAST=1                                       
         DC    AL4(19080)              2-4                                      
         DC    AL4(19080)              5+                                       
*                                                                               
*              MEDIA BUYOUT TABLES                                              
*                                                                               
MBOTAB   DC    AL2(538,24,0,0,0,0) INIT                                         
         DC    AL4(124500)         CAST=1                                       
         DC    AL4(124500)              2-4                                     
         DC    AL4(124500)              5+                                      
*                                                                               
         DC    AL2(539,24,0,0,0,0) REUSE                                        
         DC    AL4(93375)          CAST=1                                       
         DC    AL4(93375)               2-4                                     
         DC    AL4(93375)               5+                                      
*                                                                               
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
*                                                                               
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(27860)          ANN ALONE                                    
         DC    AL4(27860)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(20525)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(18165)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(16110)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(21425)          SE                                           
         DC    AL4(9565)           C3,C6                                        
         DC    AL4(15300)          C9                                           
*                                                                               
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(62775)              PRINCIPAL ON  CAMERA                     
         DC    AL4(47200)                  "     OFF   "                        
         DC    AL4(45955)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(40690)                "    6-8    "                          
         DC    AL4(33645)                "     9+    "                          
         DC    AL4(26620)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(23105)                "    6-8    "                          
         DC    AL4(18840)                "     9+    "                          
         DC    AL4(34240)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(52245)              HAND MODEL UNLIMITED                     
         DC    AL4(19875)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(34750)              HAND MODEL 13 WEEKS                      
         DC    AL4(96690)    PIL       PILOT LOCATION RATE                      
         DC    AL4(74345)    PI        PILOT STUDIO RATE                        
         DC    AL4(41070)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(10065)    C3,C6     CONTRACTORS                              
         DC    AL4(19850)    C9             "                                   
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(62775)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(47200)                                               
         DC    AL1(50),AL3(45955)                                               
         DC    AL1(50),AL3(40690)                                               
         DC    AL1(50),AL3(33645)                                               
         DC    AL1(50),AL3(26620)                                               
         DC    AL1(50),AL3(23105)                                               
         DC    AL1(50),AL3(18840)                                               
         DC    AL1(50),AL3(34240)                                               
         DC    AL1(50),AL3(52245)                                               
         DC    AL1(50),AL3(19875)                                               
         DC    AL1(50),AL3(34750)                                               
         DC    AL1(50),AL3(96690)          PIL                                  
         DC    AL1(50),AL3(74345)          PI                                   
         DC    AL1(50),AL3(41070)          SE                                   
         DC    AL1(50),AL3(10065)          C3,C6                                
         DC    AL1(50),AL3(19850)          C9                                   
*                                                                               
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(62775)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(45955)                                                       
         DC    AL4(40690)                                                       
         DC    AL4(33645)                                                       
         DC    AL4(26620)                                                       
         DC    AL4(23105)                                                       
         DC    AL4(18840)                                                       
         DC    AL4(34240)                                                       
         DC    AL4(52245)                                                       
         DC    AL4(19875)                                                       
         DC    AL4(34750)                                                       
         DC    AL4(96690)          PIL                                          
         DC    AL4(74345)          PI                                           
         DC    AL4(41070)          SE                                           
         DC    AL4(10065)          C3,C6                                        
         DC    AL4(19850)          C9                                           
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
         DC    AL4(62775)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(45955)                                                       
         DC    AL4(40690)                                                       
         DC    AL4(33645)                                                       
*                                                                               
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(62775)                                               
         DC    AL1(50),AL3(47200)                                               
         DC    AL1(50),AL3(45955)                                               
         DC    AL1(50),AL3(40690)                                               
         DC    AL1(50),AL3(33645)                                               
         DC    AL1(50),AL3(26620)                                               
         DC    AL1(50),AL3(23105)                                               
         DC    AL1(50),AL3(18840)                                               
         DC    AL1(50),AL3(34240)                                               
         DC    AL1(50),AL3(52245)                                               
         DC    AL1(50),AL3(19875)                                               
         DC    AL1(50),AL3(34750)                                               
         DC    AL1(50),AL3(96690)  PIL                                          
         DC    AL1(50),AL3(74345)  PI                                           
         DC    AL1(50),AL3(41070)  SE                                           
         DC    AL1(50),AL3(10065)  C3,C6                                        
         DC    AL1(50),AL3(19850)  C9                                           
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(62775)                                              
         DC    AL1(200),AL3(47200)                                              
         DC    AL1(200),AL3(45955)                                              
         DC    AL1(200),AL3(40690)                                              
         DC    AL1(200),AL3(33645)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(19875)                                              
         DC    AL1(200),AL3(34750)                                              
*                                                                               
         DC    AL2(69,80,1,255,0,0)  INTERNET, 8 WEEK (1.33X)                   
         DC    AL4(83490)              PRINCIPAL ON  CAMERA                     
         DC    AL4(62775)                  "     OFF   "                        
         DC    AL4(61120)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(54120)                "    6-8    "                          
         DC    AL4(44750)                "     9+    "                          
         DC    AL4(35405)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(30730)                "    6-8    "                          
         DC    AL4(25055)                "     9+    "                          
         DC    AL4(45540)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(69485)              HAND MODEL UNLIMITED                     
         DC    AL4(26435)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(46220)              HAND MODEL 13 WEEKS                      
         DC    AL4(128600)   PIL       PILOT LOCATION RATE                      
         DC    AL4(98880)    PI        PILOT STUDIO RATE                        
         DC    AL4(54625)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(13385)    C3,C6     CONTRACTORS                              
         DC    AL4(26400)    C9             "                                   
*                                                                               
         DC    AL2(87,80,1,255,0,0)  MOVE TO INTERNET, 8 WEEK (1.50X)           
         DC    AL4(94165)              PRINCIPAL ON  CAMERA                     
         DC    AL4(70800)                  "     OFF   "                        
         DC    AL4(68935)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(61035)                "    6-8    "                          
         DC    AL4(50470)                "     9+    "                          
         DC    AL4(39930)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(34660)                "    6-8    "                          
         DC    AL4(28260)                "     9+    "                          
         DC    AL4(51360)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(78370)              HAND MODEL UNLIMITED                     
         DC    AL4(29815)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(52125)              HAND MODEL 13 WEEKS                      
         DC    AL4(145035)   PIL       PILOT LOCATION RATE                      
         DC    AL4(111520)   PI        PILOT STUDIO RATE                        
         DC    AL4(61605)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(15100)    C3,C6     CONTRACTORS                              
         DC    AL4(29775)    C9             "                                   
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV  (3.5X SESSION)              
         DC    AL4(219715)             PRINCIPAL ON  CAMERA                     
         DC    AL4(165200)                 "     OFF   "                        
         DC    AL4(160845)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(142415)               "    6-8    "                          
         DC    AL4(117760)               "     9+    "                          
         DC    AL4(93170)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(80870)                "    6-8    "                          
         DC    AL4(65940)                "     9+    "                          
         DC    AL4(119840)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(182860)             HAND MODEL UNLIMITED                     
         DC    AL4(69565)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(121625)             HAND MODEL 13 WEEKS                      
         DC    AL4(338415)   PIL       PILOT LOCATION RATE                      
         DC    AL4(260210)   PI        PILOT STUDIO RATE                        
         DC    AL4(143745)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(35230)    C3,C6     CONTRACTORS                              
         DC    AL4(69475)    C9             "                                   
*                                                                               
         DC    AL2(78,80,1,255,0,0)  INTERNET RADIO (1.33X SESSION)             
         DC    AL4(37055)          ANN ALONE                                    
         DC    AL4(37055)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(27300)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(24160)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(21425)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(28495)          SE                                           
         DC    AL4(12720)          C3,C6                                        
         DC    AL4(20350)          C9                                           
*                                                                               
         DC    AL2(88,44,1,255,0,0)  MOVE TO INTERNET RADIO (1.5X SESS)         
         DC    AL4(41790)          ANN ALONE                                    
         DC    AL4(41790)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(30790)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(27250)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(24165)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(32140)          SE                                           
         DC    AL4(14350)          C3,C6                                        
         DC    AL4(28260)          C9                                           
*                                                                               
         DC    AL2(77,80,1,255,0,0)    INTERNET RADIO (3.5X SESSION)            
         DC    AL4(97510)          ANN ALONE                                    
         DC    AL4(97510)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(71840)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(63580)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(56385)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(74990)          SE                                           
         DC    AL4(33480)          C3,C6                                        
         DC    AL4(53550)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    MOVE TO INTERNET TV (4X SESSION)         
         DC    AL4(251100)             PRINCIPAL ON  CAMERA                     
         DC    AL4(188800)                 "     OFF   "                        
         DC    AL4(183820)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(162760)               "    6-8    "                          
         DC    AL4(134580)               "     9+    "                          
         DC    AL4(106480)             GROUPS 3-5 OFF CAMERA                    
         DC    AL4(92420)                "    6-8    "                          
         DC    AL4(75360)                "     9+    "                          
         DC    AL4(136960)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(208980)             HAND MODEL UNLIMITED                     
         DC    AL4(79500)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(139000)             HAND MODEL 13 WEEKS                      
         DC    AL4(386760)   PIL       PILOT LOCATION RATE                      
         DC    AL4(297380)   PI        PILOT STUDIO RATE                        
         DC    AL4(164280)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(40260)    C3,C6     CONTRACTORS                              
         DC    AL4(79400)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    MOVE TO INTERNET RADIO (4X SESS)         
         DC    AL4(111440)         ANN ALONE                                    
         DC    AL4(111440)         AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(82100)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(72660)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(64440)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(85700)          SE                                           
         DC    AL4(38260)          C3,C6                                        
         DC    AL4(61200)          C9                                           
*                                                                               
         DC    AL2(540,80,1,255,0,0)  MOVE TO INTERNET, 4 WEEK (1.25X)          
         DC    AL4(78469)              PRINCIPAL ON  CAMERA                     
         DC    AL4(59000)                  "     OFF   "                        
         DC    AL4(57444)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(50863)                "    6-8    "                          
         DC    AL4(42056)                "     9+    "                          
         DC    AL4(33275)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(28881)                "    6-8    "                          
         DC    AL4(23550)                "     9+    "                          
         DC    AL4(42800)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(65306)              HAND MODEL UNLIMITED                     
         DC    AL4(24844)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(43438)              HAND MODEL 13 WEEKS                      
         DC    AL4(120863)   PIL       PILOT LOCATION RATE                      
         DC    AL4(92931)    PI        PILOT STUDIO RATE                        
         DC    AL4(51338)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(12581)    C3,C6     CONTRACTORS                              
         DC    AL4(24813)    C9             "                                   
*                                                                               
         DC    AL2(541,44,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(34825)          ANN ALONE                                    
         DC    AL4(34825)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(25656)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(22706)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(20138)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(26781)          SE                                           
         DC    AL4(11956)          C3,C6                                        
         DC    AL4(19125)          C9                                           
*                                                                               
         DC    AL2(542,80,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(34825)          ANN ALONE                                    
         DC    AL4(34825)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(25656)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(22706)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(20138)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(26781)          SE                                           
         DC    AL4(11956)          C3,C6                                        
         DC    AL4(19125)          C9                                           
*                                                                               
         EJECT                                                                  
*              CABLE RATES (YEAR 2013)                                          
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(62775)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(45955)                                                       
         DC    AL4(40690)                                                       
         DC    AL4(33645)                                                       
         DC    AL4(26620)                                                       
         DC    AL4(23105)                                                       
         DC    AL4(18840)                                                       
*                                                                               
         DC    AL2(41,12,2,60)     MINIMUM COVERS UPTO 60                       
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,61,61)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(51)                                                          
         DC    AL4(0)                                                           
         DC    AL4(115)                                                         
         DC    AL4(5)                                                           
         DC    AL4(23)                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(35)                                                          
*                                                                               
         DC    AL2(41,44,62,62)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(916)                                                         
         DC    AL4(0)                                                           
         DC    AL4(670)                                                         
         DC    AL4(595)                                                         
         DC    AL4(488)                                                         
         DC    AL4(362)                                                         
         DC    AL4(239)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(41,44,63,69)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(916)                                                         
         DC    AL4(0)                                                           
         DC    AL4(670)                                                         
         DC    AL4(595)                                                         
         DC    AL4(488)                                                         
         DC    AL4(386)                                                         
         DC    AL4(337)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(41,44,70,70)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(916)                                                         
         DC    AL4(20)                                                          
         DC    AL4(670)                                                         
         DC    AL4(595)                                                         
         DC    AL4(488)                                                         
         DC    AL4(386)                                                         
         DC    AL4(337)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(41,44,71,100)                                                
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(916)                                                         
         DC    AL4(611)                                                         
         DC    AL4(670)                                                         
         DC    AL4(595)                                                         
         DC    AL4(488)                                                         
         DC    AL4(386)                                                         
         DC    AL4(337)                                                         
         DC    AL4(275)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(778)                                                         
         DC    AL4(517)                                                         
         DC    AL4(570)                                                         
         DC    AL4(502)                                                         
         DC    AL4(418)                                                         
         DC    AL4(331)                                                         
         DC    AL4(286)                                                         
         DC    AL4(233)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(639)                                                         
         DC    AL4(426)                                                         
         DC    AL4(470)                                                         
         DC    AL4(416)                                                         
         DC    AL4(345)                                                         
         DC    AL4(273)                                                         
         DC    AL4(235)                                                         
         DC    AL4(192)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(75)                                                          
         DC    AL4(50)                                                          
         DC    AL4(56)                                                          
         DC    AL4(50)                                                          
         DC    AL4(41)                                                          
         DC    AL4(33)                                                          
         DC    AL4(29)                                                          
         DC    AL4(21)                                                          
*                                                                               
         DC    AL2(41,44,1001,2500)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(71)                                                          
         DC    AL4(49)                                                          
         DC    AL4(52)                                                          
         DC    AL4(46)                                                          
         DC    AL4(38)                                                          
         DC    AL4(31)                                                          
         DC    AL4(25)                                                          
         DC    AL4(21)                                                          
*                                                                               
         DC    AL2(41,44,2501,3000)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(16)                                                          
         DC    AL4(11)                                                          
         DC    AL4(12)                                                          
         DC    AL4(10)                                                          
         DC    AL4(09)                                                          
         DC    AL4(07)                                                          
         DC    AL4(06)                                                          
         DC    AL4(05)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
*                                                                               
         DC    AL2(42,84,1,4,0,0)  DEM (TV)                                     
         DC    AL4(47080)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(23600)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(34465)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(30515)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(25235)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(11925)           'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(11925)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(11925)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(19875)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(34750)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(10065)          C3,C6                                        
         DC    AL4(19850)          C9                                           
         DC    AL4(18245)          'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(42,84,5,255,0,0)  DEM (TV)                                   
         DC    AL4(47080)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(23600)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(34465)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(30515)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(25235)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(2980)            'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(2980)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(2980)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(19875)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(34750)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(10065)          C3,C6                                        
         DC    AL4(19850)          C9                                           
         DC    AL4(4560)           'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(43,48,1,4,0,0)  DEM (AFT RADIO)                              
         DC    AL4(19200)          ANN ALONE                                    
         DC    AL4(19200)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(12665)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(12665)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(12665)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(21425)          SE                                           
         DC    AL4(9565)           C3,C6                                        
         DC    AL4(15300)          C9                                           
         DC    AL4(19375)          SOLOS AND DUOS                               
*                                                                               
         DC    AL2(43,48,5,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(19200)          ANN ALONE                                    
         DC    AL4(19200)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(3165)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(3165)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(3165)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(21425)          SE                                           
         DC    AL4(9565)           C3,C6                                        
         DC    AL4(15300)          C9                                           
         DC    AL4(4845)           SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(30280)                                                       
         DC    AL4(30280)                                                       
         DC    AL4(30280)                                                       
         DC    AL4(30280)                                                       
         DC    AL4(30280)                                                       
         DC    AL4(11920)                                                       
         EJECT                                                                  
*              FOREIGN REUSE                                                    
*                                                                               
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(188325)         (3 X 62775)                                  
         DC    AL4(141600)                                                      
         DC    AL4(137865)                                                      
         DC    AL4(122070)                                                      
         DC    AL4(100935)                                                      
         DC    AL4(79860)                                                       
         DC    AL4(69315)                                                       
         DC    AL4(56520)                                                       
         DC    AL4(102720)                                                      
         DC    AL4(156735)                                                      
         DC    AL4(59625)                                                       
         DC    AL4(104250)                                                      
         DC    AL4(290070)         PIL                                          
         DC    AL4(223035)         PI                                           
         DC    AL4(123210)         SE                                           
         DC    AL4(30195)          C3,C6                                        
         DC    AL4(59550)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(125550)         (2 X 62775)                                  
         DC    AL4(94400)                                                       
         DC    AL4(91910)                                                       
         DC    AL4(81380)                                                       
         DC    AL4(67290)                                                       
         DC    AL4(53240)                                                       
         DC    AL4(46210)                                                       
         DC    AL4(37680)                                                       
         DC    AL4(68480)                                                       
         DC    AL4(104490)                                                      
         DC    AL4(39750)                                                       
         DC    AL4(69500)                                                       
         DC    AL4(193380)          PIL                                         
         DC    AL4(148690)          PI                                          
         DC    AL4(82140)           SE                                          
         DC    AL4(20130)           C3,C6                                       
         DC    AL4(39700)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 9X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(564975)        (9 X 62775) SINCE ASIA PACIFIC IS 2X          
         DC    AL4(424800)                                                      
         DC    AL4(413595)                                                      
         DC    AL4(366210)                                                      
         DC    AL4(302805)                                                      
         DC    AL4(239580)                                                      
         DC    AL4(207945)                                                      
         DC    AL4(169560)                                                      
         DC    AL4(308160)                                                      
         DC    AL4(470205)                                                      
         DC    AL4(178875)                                                      
         DC    AL4(312750)                                                      
         DC    AL4(870210)         PIL                                          
         DC    AL4(669105)         PI                                           
         DC    AL4(369630)         SE                                           
         DC    AL4(90585)          C3,C6                                        
         DC    AL4(178650)         C9                                           
*                                                                               
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(55275)          N/D                                          
         DC    AL4(55275)          P,ANN,S,D,ACR                                
         DC    AL4(32060)          3-5 GROUP                                    
         DC    AL4(22110)          6-8 GROUP                                    
         DC    AL4(17680)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(63035)          P,ANN,ACR                                    
         DC    AL4(65455)          S,D                                          
         DC    AL4(42675)          3-5 GROUP                                    
         DC    AL4(34135)          6-8 GROUP                                    
         DC    AL4(21350)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
*                                                                               
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK (2009 RATE * 1.06)* 1.10)            
         DC    AL4(249360)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(187520)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(182550)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(161585)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(133645)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(105935)         'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(91770)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(74815)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(601)                                                         
         DC    AL4(436)                                                         
         DC    AL4(426)                                                         
         DC    AL4(394)                                                         
         DC    AL4(306)                                                         
         DC    AL4(253)                                                         
         DC    AL4(228)                                                         
         DC    AL4(162)                                                         
         EJECT                                                                  
*              SPANISH WILDSPOT TABLES                                          
*                                                                               
SWSTAB   DC    AL2(110,44,1,1,0,0)  UNIT 1 (2009 RATE * 1.06)* 1.05)            
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(65910)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(49560)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(48255)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(42725)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(35325)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(27955)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(24260)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(19785)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(110,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(2256)                                                        
         DC    AL4(1544)                                                        
         DC    AL4(1759)                                                        
         DC    AL4(1517)                                                        
         DC    AL4(1239)                                                        
         DC    AL4(623)                                                         
         DC    AL4(492)                                                         
         DC    AL4(410)                                                         
*                                                                               
         DC    AL2(110,44,26,60,0,0)  UNITS 26-60                               
         DC    AL4(837)                                                         
         DC    AL4(657)                                                         
         DC    AL4(907)                                                         
         DC    AL4(768)                                                         
         DC    AL4(637)                                                         
         DC    AL4(263)                                                         
         DC    AL4(180)                                                         
         DC    AL4(165)                                                         
*                                                                               
         DC    AL2(110,44,61,125,0,0)  UNITS 61-125                             
         DC    AL4(837)                                                         
         DC    AL4(657)                                                         
         DC    AL4(657)                                                         
         DC    AL4(513)                                                         
         DC    AL4(430)                                                         
         DC    AL4(159)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
*                                                                               
         DC    AL2(110,44,126,255,0,0)  UNITS 126+                              
         DC    AL4(837)                                                         
         DC    AL4(657)                                                         
         DC    AL4(325)                                                         
         DC    AL4(263)                                                         
         DC    AL4(229)                                                         
         DC    AL4(159)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
         EJECT                                                                  
*              SPANISH FOREIGN REUSE                                            
*                                                                               
SFRTBL   DC    AL2(410,80,0,0,0,0)  SFRA,SFRC - 4X SESSION RATE                 
         DC    AL4(251100)         (4 X 62775)                                  
         DC    AL4(188800)                                                      
         DC    AL4(183820)                                                      
         DC    AL4(162760)                                                      
         DC    AL4(134580)                                                      
         DC    AL4(106480)                                                      
         DC    AL4(92420)                                                       
         DC    AL4(75360)                                                       
         DC    AL4(136960)                                                      
         DC    AL4(208980)                                                      
         DC    AL4(79500)                                                       
         DC    AL4(139000)                                                      
         DC    AL4(386760)         PIL                                          
         DC    AL4(297380)         PI                                           
         DC    AL4(164280)         SE                                           
         DC    AL4(40260)          C3,C6                                        
         DC    AL4(79400)          C9                                           
*                                                                               
         DC    AL2(411,80,0,0,0,0)  SFRB - 3X SESSION RATE                      
         DC    AL4(188325)         (3 X 62775)                                  
         DC    AL4(141600)                                                      
         DC    AL4(137865)                                                      
         DC    AL4(122070)                                                      
         DC    AL4(100935)                                                      
         DC    AL4(79860)                                                       
         DC    AL4(69315)                                                       
         DC    AL4(56520)                                                       
         DC    AL4(102720)                                                      
         DC    AL4(156735)                                                      
         DC    AL4(59625)                                                       
         DC    AL4(104250)                                                      
         DC    AL4(290070)         PIL                                          
         DC    AL4(223035)         PI                                           
         DC    AL4(123210)         SE                                           
         DC    AL4(30195)          C3,C6                                        
         DC    AL4(59550)          C9                                           
         EJECT                                                                  
*                                                                               
*              ADDENDUM USES                                                    
*                                                                               
ADTTBL   DC    AL2(65,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA                
         DC    AL4(38220)          ON CAMERA                                    
         DC    AL4(28800)          OFF                                          
         DC    AL4(27987)          ON CAMERA GROUPS 3-5                         
         DC    AL4(24780)                           6-8                         
         DC    AL4(20490)                           9+                          
         DC    AL4(16212)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(14071)                            6-8                        
         DC    AL4(11474)                            9+                         
         DC    AL4(27400)          EXTRA UNLIMITED                              
         DC    AL4(41800)          HAND MODEL UNLIMITED                         
         DC    AL4(15900)          EXTRA INITIAL 13WK                           
         DC    AL4(27800)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(38220)          SOLO/DUO ON CAM                              
         DC    AL4(28800)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(70,88,0,0,0,0)  1 WEEK - GA                                  
         DC    AL4(41000)          ON CAMERA                                    
         DC    AL4(30800)          OFF                                          
         DC    AL4(29986)          ON CAMERA GROUPS 3-5                         
         DC    AL4(26550)                           6-8                         
         DC    AL4(21953)                           9+                          
         DC    AL4(17370)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(15076)                            6-8                        
         DC    AL4(12293)                            9+                         
         DC    AL4(27400)          EXTRA UNLIMITED                              
         DC    AL4(41800)          HAND MODEL UNLIMITED                         
         DC    AL4(15900)          EXTRA INITIAL 13WK                           
         DC    AL4(27800)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(41000)          SOLO/DUO ON CAM                              
         DC    AL4(30800)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(71,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS               
         DC    AL4(30777)          ON CAMERA                                    
         DC    AL4(23120)          OFF                                          
         DC    AL4(23744)          ON CAMERA GROUPS 3-5                         
         DC    AL4(20352)                           6-8                         
         DC    AL4(16207)                           9+                          
         DC    AL4(10303)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(8416)                             6-8                        
         DC    AL4(6026)                             9+                         
         DC    AL4(19875)          EXTRA UNLIMITED                              
         DC    AL4(26633)          HAND MODEL UNLIMITED                         
         DC    AL4(11220)          EXTRA INITIAL 13WK                           
         DC    AL4(17713)          HAND MODEL INITIAL 13WK                      
         DC    AL4(29035)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0,0,0)  TV SESSION - 2 WEEK - NW MULTI MKT           
         DC    AL4(41350)          ON CAMERA                                    
         DC    AL4(30850)          OFF                                          
         DC    AL4(30100)                                                       
         DC    AL4(30100)                                                       
         DC    AL4(30100)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(26400)                                                       
         DC    AL4(26400)                                                       
         DC    AL4(15150)                                                       
         DC    AL4(15150)                                                       
         DC    AL4(41350)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0)      TV SESSION - 2 WEEK - NW SINGLE MKT          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(37400)          ON CAMERA                                    
         DC    AL4(26400)          OFF                                          
         DC    AL4(22900)          ON CAMERA GROUPS 3-5                         
         DC    AL4(22900)                           6-8                         
         DC    AL4(22900)                           9+                          
         DC    AL4(15800)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(15800)                            6-8                        
         DC    AL4(15800)                            9+                         
         DC    AL4(23744)          EXTRA UNLIMITED                              
         DC    AL4(23744)          HAND MODEL UNLIMITED                         
         DC    AL4(14500)          EXTRA INITIAL 13WK                           
         DC    AL4(14500)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(80,88,0,0,0,0)  4 WEEK - GA                                  
         DC    AL4(43700)          ON CAMERA                                    
         DC    AL4(32900)          OFF                                          
         DC    AL4(31985)          ON CAMERA GROUPS 3-5                         
         DC    AL4(28320)                           6-8                         
         DC    AL4(23417)                           9+                          
         DC    AL4(18528)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(16081)                            6-8                        
         DC    AL4(13113)                            9+                         
         DC    AL4(27400)          EXTRA UNLIMITED                              
         DC    AL4(41800)          HAND MODEL UNLIMITED                         
         DC    AL4(15900)          EXTRA INITIAL 13WK                           
         DC    AL4(27800)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(43700)          SOLO/DUO ON CAM                              
         DC    AL4(32900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(81,80,0,0,0,0)  31 DAY - KS                                  
         DC    AL4(39450)          ON CAMERA                                    
         DC    AL4(29648)          OFF                                          
         DC    AL4(29400)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(19849)                                                       
         DC    AL4(12943)                                                       
         DC    AL4(10181)                                                       
         DC    AL4(7664)                                                        
         DC    AL4(19875)          EXTRA UNLIMITED                              
         DC    AL4(26633)          HAND MODEL UNLIMITED                         
         DC    AL4(11220)          EXTRA INITIAL 13WK                           
         DC    AL4(17713)          HAND MODEL INITIAL 13WK                      
         DC    AL4(37215)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(90,88,0,0,0,0)  13 WEEK - GA                                 
         DC    AL4(54600)          ON CAMERA                                    
         DC    AL4(41100)          OFF                                          
         DC    AL4(39981)          ON CAMERA GROUPS 3-5                         
         DC    AL4(35400)                           6-8                         
         DC    AL4(29271)                           9+                          
         DC    AL4(23159)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(20101)                            6-8                        
         DC    AL4(16391)                            9+                         
         DC    AL4(27400)          EXTRA UNLIMITED                              
         DC    AL4(41800)          HAND MODEL UNLIMITED                         
         DC    AL4(15900)          EXTRA INITIAL 13WK                           
         DC    AL4(27800)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(54600)          SOLO/DUO ON CAM                              
         DC    AL4(41100)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(91,80,0,0,0,0)  13 WEEKS - KS                                
         DC    AL4(48124)          ON CAMERA                                    
         DC    AL4(36056)          OFF                                          
         DC    AL4(34800)                                                       
         DC    AL4(29648)                                                       
         DC    AL4(23749)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(12306)                                                       
         DC    AL4(9174)                                                        
         DC    AL4(19875)          EXTRA UNLIMITED                              
         DC    AL4(26633)          HAND MODEL UNLIMITED                         
         DC    AL4(11220)          EXTRA INITIAL 13WK                           
         DC    AL4(17713)          HAND MODEL INITIAL 13WK                      
         DC    AL4(45400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(92,80,0,0,0,0)  13 WEEKS - TX                                
         DC    AL4(50220)          ON CAMERA                                    
         DC    AL4(37760)          OFF                                          
         DC    AL4(36764)          ON CAMERA GROUPS 3-5                         
         DC    AL4(36764)                           6-8                         
         DC    AL4(36764)                           9+                          
         DC    AL4(21296)          OFF CAMERA GROUS 3-5                         
         DC    AL4(21296)                           6-8                         
         DC    AL4(21296)                           9+                          
         DC    AL4(27392)          EXTRA UNLIMITED                              
         DC    AL4(41796)          HAND MODEL UNLIMITED                         
         DC    AL4(15900)          EXTRA INITIAL 13WK                           
         DC    AL4(27800)          HAND MODEL INITIAL 13WK                      
         DC    AL4(50220)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0,0,0)  13 WEEKS - NW MULTI MARKET                   
         DC    AL4(48550)          ON CAMERA                                    
         DC    AL4(36350)          OFF                                          
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(20450)                                                       
         DC    AL4(20450)                                                       
         DC    AL4(20450)                                                       
         DC    AL4(26400)                                                       
         DC    AL4(26400)                                                       
         DC    AL4(18000)                                                       
         DC    AL4(18000)                                                       
         DC    AL4(48550)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0)      13 WEEKS - NW SINGLE MARKET                  
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(44100)          ON CAMERA                                    
         DC    AL4(31050)          OFF                                          
         DC    AL4(26900)                                                       
         DC    AL4(26900)                                                       
         DC    AL4(26900)                                                       
         DC    AL4(18550)                                                       
         DC    AL4(18550)                                                       
         DC    AL4(18550)                                                       
         DC    AL4(23744)                                                       
         DC    AL4(23744)                                                       
         DC    AL4(17300)                                                       
         DC    AL4(17300)                                                       
         DC    AL4(44100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
ADOTBL   DC    AL2(100,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA            
         DC    AL4(16900)                                                       
         DC    AL4(16900)                                                       
         DC    AL4(12500)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(9800)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(16900)          SOLO/DUO                                     
*                                                                               
         DC    AL2(105,48,0,0,0,0)  1 WEEK - GA                                 
         DC    AL4(18200)                                                       
         DC    AL4(18200)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(11800)                                                       
         DC    AL4(10500)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(18200)                                                       
*                                                                               
         DC    AL2(106,44,0,0,0,0)  1 WEEK - KS                                 
         DC    AL4(13091)                                                       
         DC    AL4(13091)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(6874)                                                        
         DC    AL4(6084)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
*                                  RADIO                                        
         DC    AL2(108,44,0,0,0,0)  2 WEEK - NW MULTLIPLE MARKETS               
         DC    AL4(18650)                                                       
         DC    AL4(18650)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
*                                  RADIO                                        
         DC    AL2(108,44,0,0)      2 WEEK - NW SINGLE MARKET                   
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(12100)                                                       
         DC    AL4(12100)                                                       
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(115,48,0,0,0,0)  4 WEEK - GA                                 
         DC    AL4(19400)                                                       
         DC    AL4(19400)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(11200)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(19400)                                                       
*                                                                               
         DC    AL2(116,44,0,0,0,0)  31 DAY - KS                                 
         DC    AL4(16800)                                                       
         DC    AL4(16800)                                                       
         DC    AL4(9922)                                                        
         DC    AL4(8729)                                                        
         DC    AL4(7802)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(125,48,0,0,0,0)  13 WEEK - GA                                
         DC    AL4(24200)                                                       
         DC    AL4(24200)                                                       
         DC    AL4(17900)                                                       
         DC    AL4(15800)                                                       
         DC    AL4(14000)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(24200)                                                       
*                                                                               
         DC    AL2(126,44,0,0,0,0)  13 WEEK - KS                                
         DC    AL4(20500)                                                       
         DC    AL4(20500)                                                       
         DC    AL4(11506)                                                       
         DC    AL4(10187)                                                       
         DC    AL4(9121)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(127,44,0,0,0,0)  13 WEEK - TX                                
         DC    AL4(22288)                                                       
         DC    AL4(22288)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0,0,0)  13 WEEK - NW  MULTIPLE MARKETS              
         DC    AL4(21950)                                                       
         DC    AL4(21950)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0)      13 WEEK - NW  SINGLE MARKET                 
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(14200)                                                       
         DC    AL4(14200)                                                       
         DC    AL4(8600)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(8600)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
ADHTAB   DC    AL2(225,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                 
         DC    AL4(54600)                                                       
         DC    AL4(41100)                                                       
         DC    AL4(39981)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(29271)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(38500)                                                       
         DC    AL4(28900)                                                       
*                                                                               
         DC    AL2(226,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                 
         DC    AL4(48124)           ON CAMERA                                   
         DC    AL4(36056)           OFF                                         
         DC    AL4(34800)                                                       
         DC    AL4(29648)                                                       
         DC    AL4(23749)                                                       
*                                                                               
         DC    AL2(227,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                 
         DC    AL4(50220)           ON CAMERA                                   
         DC    AL4(37760)           OFF                                         
         DC    AL4(36764)                                                       
         DC    AL4(36764)                                                       
         DC    AL4(36764)                                                       
*                                                                               
         DC    AL2(228,32,0,0,0,0)  ADDENDUM HOLDING RATES - NW                 
         DC    AL4(48550)           ON CAMERA   MULTIPLE MARKET                 
         DC    AL4(36350)           OFF                                         
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
*                                                                               
         DC    AL2(228,32,0,0)      ADDENDUM HOLDING RATES - NW                 
         DC    AL1(1,0,0,0)         SINGLE MARKET                               
         DC    AL4(44100)           ON CAMERA                                   
         DC    AL4(31050)           OFF                                         
         DC    AL4(26900)                                                       
         DC    AL4(26900)                                                       
         DC    AL4(26900)                                                       
*                                                                               
*                                   ADDENDUM REINSTSATEMENT-GA                  
ARNTAB   DC    AL2(230,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                 
         DC    AL1(200),AL3(54600)                                              
         DC    AL1(200),AL3(41100)                                              
         DC    AL1(200),AL3(39981)                                              
         DC    AL1(200),AL3(35400)                                              
         DC    AL1(200),AL3(29271)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(38500)                                              
         DC    AL1(200),AL3(28900)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(231,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(48124)             ON CAMERA                        
         DC    AL1(200),AL3(36056)             OFF                              
         DC    AL1(200),AL3(34800)                                              
         DC    AL1(200),AL3(29648)                                              
         DC    AL1(200),AL3(23749)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(232,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(50220)             ON CAMERA                        
         DC    AL1(200),AL3(37760)             OFF                              
         DC    AL1(200),AL3(36764)                                              
         DC    AL1(200),AL3(36764)                                              
         DC    AL1(200),AL3(36764)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(48550)             ON CAMERA  MULTIPLE MKTS         
         DC    AL1(200),AL3(36350)             OFF                              
         DC    AL1(200),AL3(35400)                                              
         DC    AL1(200),AL3(35400)                                              
         DC    AL1(200),AL3(35400)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0)       - 2X ADDENDUM HOLDING RATES                
         DC    AL1(1,0,0,0)          SINGLE MARKET                              
         DC    AL1(200),AL3(44100)             ON CAMERA                        
         DC    AL1(200),AL3(31050)             OFF                              
         DC    AL1(200),AL3(26900)                                              
         DC    AL1(200),AL3(26900)                                              
         DC    AL1(200),AL3(26900)                                              
*                                                                               
ADDTAB   DC    AL2(205,88,0,0,0,0)  ADDENDUM DEMO (TV) - GA                     
         DC    AL4(40960)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(20532)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(29985)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(26548)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(21954)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(10375)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(10375)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(10375)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(40960)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(15873)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(206,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                     
         DC    AL4(10801)          'ON'                                         
         DC    AL4(9545)           'OFF'                                        
         DC    AL4(10801)                                                       
         DC    AL4(10801)                                                       
         DC    AL4(10801)                                                       
         DC    AL4(9545)                                                        
         DC    AL4(9545)                                                        
         DC    AL4(9545)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(207,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                     
         DC    AL4(37668)          'ON'                                         
         DC    AL4(18880)          'OFF'                                        
         DC    AL4(27570)          (80% OF NATIONAL)                            
         DC    AL4(24410)                                                       
         DC    AL4(20190)                                                       
         DC    AL4(9540)                                                        
         DC    AL4(9540)                                                        
         DC    AL4(9540)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(8050)           C3,C6                                        
         DC    AL4(15880)          C9                                           
         DC    AL4(37668)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(14595)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(208,80,0,0,0,0)  ADDENDUM DEMO (TV) - NW                     
         DC    AL4(36250)          'ON'                                         
         DC    AL4(18150)          'OFF'                                        
         DC    AL4(26500)                                                       
         DC    AL4(26500)                                                       
         DC    AL4(26500)                                                       
         DC    AL4(9650)                                                        
         DC    AL4(9650)                                                        
         DC    AL4(9650)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(10388)          EXTRA                                        
         DC    4AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(215,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA              
         DC    AL4(16704)          ANN ALONE                                    
         DC    AL4(16704)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(11019)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11019)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11019)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(216,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS              
         DC    AL4(8729)           ANN ALONE                                    
         DC    AL4(8729)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8729)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8729)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8729)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(217,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX              
         DC    AL4(15360)          ANN ALONE                                    
         DC    AL4(15360)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(10130)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(10130)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(10130)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17140)          SE                                           
         DC    AL4(7650)           C3,C6                                        
         DC    AL4(12240)          C9                                           
         DC    AL4(15500)          SOLO/DUO                                     
*                                                                               
         DC    AL2(218,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - NW              
         DC    AL4(9200)          ANN ALONE                                     
         DC    AL4(9200)          AR,AS,P,ANN,1-4MS,1-4SS                       
         DC    AL4(6150)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(6150)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6150)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
*                                                                               
ADWTAB   DC    AL2(135,52,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(38220)          ON CAMERA                                    
         DC    AL4(28800)          OFF                                          
         DC    AL4(27987)                                                       
         DC    AL4(24780)                                                       
         DC    AL4(20490)                                                       
         DC    AL4(16212)                                                       
         DC    AL4(14071)                                                       
         DC    AL4(11474)                                                       
         DC    AL4(38220)          SOLO/DUO ON CAMERA                           
         DC    AL4(28800)          OFF CAMERA                                   
*                                                                               
         DC    AL2(135,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1309)                                                        
         DC    AL4(895)                                                         
         DC    AL4(1020)                                                        
         DC    AL4(880)                                                         
         DC    AL4(719)                                                         
         DC    AL4(362)                                                         
         DC    AL4(286)                                                         
         DC    AL4(238)                                                         
         DC    AL4(1309)                                                        
         DC    AL4(895)                                                         
*                                                                               
         DC    AL2(135,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
         DC    AL4(526)                                                         
         DC    AL4(445)                                                         
         DC    AL4(369)                                                         
         DC    AL4(152)                                                         
         DC    AL4(105)                                                         
         DC    AL4(96)                                                          
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
*                                                                               
         DC    AL2(135,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
         DC    AL4(381)                                                         
         DC    AL4(298)                                                         
         DC    AL4(249)                                                         
         DC    AL4(93)                                                          
         DC    AL4(52)                                                          
         DC    AL4(52)                                                          
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
*                                                                               
         DC    AL2(135,52,126,255,0,0)  UNITS 126+                              
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
         DC    AL4(189)                                                         
         DC    AL4(152)                                                         
         DC    AL4(133)                                                         
         DC    AL4(93)                                                          
         DC    AL4(52)                                                          
         DC    AL4(52)                                                          
         DC    AL4(485)                                                         
         DC    AL4(381)                                                         
*                                                                               
         DC    AL2(140,52,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(41000)          ON CAMERA                                    
         DC    AL4(30800)          OFF                                          
         DC    AL4(29986)                                                       
         DC    AL4(26550)                                                       
         DC    AL4(21953)                                                       
         DC    AL4(17370)                                                       
         DC    AL4(15076)                                                       
         DC    AL4(12293)                                                       
         DC    AL4(41000)                                                       
         DC    AL4(30800)                                                       
*                                                                               
         DC    AL2(140,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1402)                                                        
         DC    AL4(959)                                                         
         DC    AL4(1093)                                                        
         DC    AL4(943)                                                         
         DC    AL4(770)                                                         
         DC    AL4(388)                                                         
         DC    AL4(306)                                                         
         DC    AL4(254)                                                         
         DC    AL4(1402)                                                        
         DC    AL4(959)                                                         
*                                                                               
         DC    AL2(140,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
         DC    AL4(564)                                                         
         DC    AL4(477)                                                         
         DC    AL4(395)                                                         
         DC    AL4(163)                                                         
         DC    AL4(112)                                                         
         DC    AL4(102)                                                         
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
*                                                                               
         DC    AL2(140,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
         DC    AL4(408)                                                         
         DC    AL4(319)                                                         
         DC    AL4(267)                                                         
         DC    AL4(99)                                                          
         DC    AL4(56)                                                          
         DC    AL4(56)                                                          
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
*                                                                               
         DC    AL2(140,52,126,225,0,0)  UNITS 126+                              
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
         DC    AL4(202)                                                         
         DC    AL4(163)                                                         
         DC    AL4(142)                                                         
         DC    AL4(99)                                                          
         DC    AL4(56)                                                          
         DC    AL4(56)                                                          
         DC    AL4(520)                                                         
         DC    AL4(408)                                                         
*                                                                               
         DC    AL2(141,44,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(30777)          ON CAMERA                                    
         DC    AL4(23120)          OFF                                          
         DC    AL4(23744)                                                       
         DC    AL4(20352)                                                       
         DC    AL4(16207)                                                       
         DC    AL4(10303)                                                       
         DC    AL4(8416)                                                        
         DC    AL4(6026)                                                        
*                                                                               
         DC    AL2(141,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1145)                                                        
         DC    AL4(1145)                                                        
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
*                                                                               
         DC    AL2(143,44,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(41350)          ON CAMERA                                    
         DC    AL4(30850)          OFF                                          
         DC    AL4(30100)                                                       
         DC    AL4(30100)                                                       
         DC    AL4(30100)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
         DC    AL4(17400)                                                       
*                                                                               
         DC    AL2(143,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(2150)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
*                                                                               
         DC    AL2(150,52,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(43700)          ON CAMERA                                    
         DC    AL4(32900)          OFF                                          
         DC    AL4(31985)                                                       
         DC    AL4(28320)                                                       
         DC    AL4(23417)                                                       
         DC    AL4(18528)                                                       
         DC    AL4(16081)                                                       
         DC    AL4(13113)                                                       
         DC    AL4(43700)          SOLO/DUO ON CAMERA                           
         DC    AL4(32900)          OFF CAMERA                                   
*                                                                               
         DC    AL2(150,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1496)                                                        
         DC    AL4(1023)                                                        
         DC    AL4(1166)                                                        
         DC    AL4(1006)                                                        
         DC    AL4(821)                                                         
         DC    AL4(413)                                                         
         DC    AL4(326)                                                         
         DC    AL4(271)                                                         
         DC    AL4(1496)                                                        
         DC    AL4(1023)                                                        
*                                                                               
         DC    AL2(150,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
         DC    AL4(601)                                                         
         DC    AL4(509)                                                         
         DC    AL4(422)                                                         
         DC    AL4(174)                                                         
         DC    AL4(120)                                                         
         DC    AL4(109)                                                         
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
*                                                                               
         DC    AL2(150,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
         DC    AL4(435)                                                         
         DC    AL4(340)                                                         
         DC    AL4(285)                                                         
         DC    AL4(106)                                                         
         DC    AL4(60)                                                          
         DC    AL4(60)                                                          
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
*                                                                               
         DC    AL2(150,52,126,225,0,0)  UNITS 126+                              
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
         DC    AL4(216)                                                         
         DC    AL4(174)                                                         
         DC    AL4(152)                                                         
         DC    AL4(106)                                                         
         DC    AL4(60)                                                          
         DC    AL4(60)                                                          
         DC    AL4(554)                                                         
         DC    AL4(435)                                                         
*                                                                               
         DC    AL2(151,44,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(39450)          ON CAMERA                                    
         DC    AL4(29648)          OFF                                          
         DC    AL4(29400)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(19849)                                                       
         DC    AL4(12943)                                                       
         DC    AL4(10181)                                                       
         DC    AL4(7664)                                                        
*                                                                               
         DC    AL2(151,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1145)                                                        
         DC    AL4(1145)                                                        
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
*                                                                               
         DC    AL2(160,52,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(54600)          ON CAMERA                                    
         DC    AL4(41100)          OFF                                          
         DC    AL4(39981)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(29271)                                                       
         DC    AL4(23159)                                                       
         DC    AL4(20101)                                                       
         DC    AL4(16391)                                                       
         DC    AL4(54600)          SOLO/DUO ON CAM                              
         DC    AL4(41100)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(160,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1870)                                                        
         DC    AL4(1279)                                                        
         DC    AL4(1457)                                                        
         DC    AL4(1257)                                                        
         DC    AL4(1027)                                                        
         DC    AL4(517)                                                         
         DC    AL4(408)                                                         
         DC    AL4(339)                                                         
         DC    AL4(1870)                                                        
         DC    AL4(1279)                                                        
*                                                                               
         DC    AL2(160,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
         DC    AL4(752)                                                         
         DC    AL4(636)                                                         
         DC    AL4(527)                                                         
         DC    AL4(218)                                                         
         DC    AL4(150)                                                         
         DC    AL4(137)                                                         
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
*                                                                               
         DC    AL2(160,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
         DC    AL4(544)                                                         
         DC    AL4(425)                                                         
         DC    AL4(356)                                                         
         DC    AL4(132)                                                         
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
*                                                                               
         DC    AL2(160,52,126,255,0,0)  UNITS 126+                              
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
         DC    AL4(270)                                                         
         DC    AL4(218)                                                         
         DC    AL4(190)                                                         
         DC    AL4(132)                                                         
         DC    AL4(75)                                                          
         DC    AL4(75)                                                          
         DC    AL4(693)                                                         
         DC    AL4(544)                                                         
*                                                                               
         DC    AL2(161,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                      
         DC    AL4(48124)          ON CAMERA                                    
         DC    AL4(36056)          OFF                                          
         DC    AL4(34800)                                                       
         DC    AL4(29648)                                                       
         DC    AL4(23749)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(12306)                                                       
         DC    AL4(9174)                                                        
*                                                                               
         DC    AL2(161,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1145)                                                        
         DC    AL4(1145)                                                        
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(313)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
         DC    AL4(159)                                                         
*                                                                               
         DC    AL2(162,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                      
         DC    AL4(50220)           ON CAMERA                                   
         DC    AL4(37760)           OFF                                         
         DC    AL4(36764)                                                       
         DC    AL4(36764)                                                       
         DC    AL4(36764)                                                       
         DC    AL4(21296)                                                       
         DC    AL4(21296)                                                       
         DC    AL4(21296)                                                       
*                                                                               
         DC    AL2(162,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1719)                                                        
         DC    AL4(1176)                                                        
         DC    AL4(1340)                                                        
         DC    AL4(1340)                                                        
         DC    AL4(1340)                                                        
         DC    AL4(475)                                                         
         DC    AL4(475)                                                         
         DC    AL4(475)                                                         
*                                                                               
         DC    AL2(162,44,26,255,0,0)  UNITS 26+                                
         DC    AL4(638)                                                         
         DC    AL4(500)                                                         
         DC    AL4(691)                                                         
         DC    AL4(691)                                                         
         DC    AL4(691)                                                         
         DC    AL4(200)                                                         
         DC    AL4(200)                                                         
         DC    AL4(200)                                                         
*                                                                               
         DC    AL2(163,44,1,1,0,0)  13 WEEKS - NW - UNIT 1                      
         DC    AL4(48550)          ON CAMERA                                    
         DC    AL4(36350)          OFF                                          
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(20450)                                                       
         DC    AL4(20450)                                                       
         DC    AL4(20450)                                                       
*                                                                               
         DC    AL2(163,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(2150)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(2150)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
*                                                                               
         DC    AL2(170,40,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(16900)          ANN ALONE                                    
         DC    AL4(16900)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(12500)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11100)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(9800)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15800)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(16900)          SOLO/DUO                                     
*                                                                               
         DC    AL2(170,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(250)                                                         
         DC    AL4(250)                                                         
         DC    AL4(130)                                                         
         DC    AL4(111)                                                         
         DC    AL4(85)                                                          
         DC    AL4(0)                                                           
         DC    AL4(250)                                                         
*                                                                               
         DC    AL2(170,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(188)                                                         
         DC    AL4(188)                                                         
         DC    AL4(111)                                                         
         DC    AL4(85)                                                          
         DC    AL4(85)                                                          
         DC    AL4(0)                                                           
         DC    AL4(188)                                                         
*                                                                               
         DC    AL2(170,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(188)                                                         
         DC    AL4(188)                                                         
         DC    AL4(63)                                                          
         DC    AL4(54)                                                          
         DC    AL4(54)                                                          
         DC    AL4(0)                                                           
         DC    AL4(188)                                                         
*                                                                               
         DC    AL2(175,40,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(18200)          ANN ALONE                                    
         DC    AL4(18200)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(13400)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11800)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(10500)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17200)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(18200)          SOLO/DUO                                     
*                                                                               
         DC    AL2(175,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(268)                                                         
         DC    AL4(268)                                                         
         DC    AL4(139)                                                         
         DC    AL4(119)                                                         
         DC    AL4(91)                                                          
         DC    AL4(0)                                                           
         DC    AL4(268)                                                         
*                                                                               
         DC    AL2(175,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(201)                                                         
         DC    AL4(201)                                                         
         DC    AL4(119)                                                         
         DC    AL4(91)                                                          
         DC    AL4(91)                                                          
         DC    AL4(0)                                                           
         DC    AL4(201)                                                         
*                                                                               
         DC    AL2(175,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(201)                                                         
         DC    AL4(201)                                                         
         DC    AL4(119)                                                         
         DC    AL4(91)                                                          
         DC    AL4(91)                                                          
         DC    AL4(0)                                                           
         DC    AL4(201)                                                         
*                                                                               
         DC    AL2(176,36,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(13091)                                                       
         DC    AL4(13091)                                                       
         DC    AL4(8200)                                                        
         DC    AL4(6874)                                                        
         DC    AL4(6084)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(176,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(440)                                                         
         DC    AL4(440)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(440)                                                         
*                                                                               
         DC    AL2(178,36,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(18650)                                                       
         DC    AL4(18650)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(13800)                                                       
         DC    AL4(18650)          SE                                           
*                                                                               
         DC    AL2(178,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
*                                                                               
         DC    AL2(185,40,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(19400)          ANN ALONE                                    
         DC    AL4(19400)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(14300)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(12600)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11200)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(18300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(19400)          SOLO/DUO                                     
*                                                                               
         DC    AL2(185,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(285)                                                         
         DC    AL4(285)                                                         
         DC    AL4(148)                                                         
         DC    AL4(127)                                                         
         DC    AL4(97)                                                          
         DC    AL4(0)                                                           
         DC    AL4(285)                                                         
*                                                                               
         DC    AL2(185,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(214)                                                         
         DC    AL4(214)                                                         
         DC    AL4(127)                                                         
         DC    AL4(97)                                                          
         DC    AL4(97)                                                          
         DC    AL4(0)                                                           
         DC    AL4(214)                                                         
*                                                                               
         DC    AL2(185,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(214)                                                         
         DC    AL4(214)                                                         
         DC    AL4(72)                                                          
         DC    AL4(62)                                                          
         DC    AL4(62)                                                          
         DC    AL4(0)                                                           
         DC    AL4(214)                                                         
*                                                                               
         DC    AL2(186,36,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(16800)                                                       
         DC    AL4(16800)                                                       
         DC    AL4(9922)                                                        
         DC    AL4(8729)                                                        
         DC    AL4(7802)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(186,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(440)                                                         
         DC    AL4(440)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(440)                                                         
*                                                                               
         DC    AL2(195,40,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(24200)          ANN ALONE                                    
         DC    AL4(24200)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(17900)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(15800)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(14000)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(22900)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(24200)          SOLO/DUO                                     
*                                                                               
         DC    AL2(195,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(357)                                                         
         DC    AL4(357)                                                         
         DC    AL4(185)                                                         
         DC    AL4(158)                                                         
         DC    AL4(122)                                                         
         DC    AL4(0)                                                           
         DC    AL4(357)                                                         
*                                                                               
         DC    AL2(195,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(268)                                                         
         DC    AL4(268)                                                         
         DC    AL4(158)                                                         
         DC    AL4(122)                                                         
         DC    AL4(122)                                                         
         DC    AL4(0)                                                           
         DC    AL4(268)                                                         
*                                                                               
         DC    AL2(195,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(268)                                                         
         DC    AL4(268)                                                         
         DC    AL4(90)                                                          
         DC    AL4(77)                                                          
         DC    AL4(77)                                                          
         DC    AL4(0)                                                           
         DC    AL4(268)                                                         
*                                                                               
         DC    AL2(196,36,1,1,0,0)  13 WEEK - KS - UNIT 1                       
         DC    AL4(20500)                                                       
         DC    AL4(20500)                                                       
         DC    AL4(11506)                                                       
         DC    AL4(10187)                                                       
         DC    AL4(9121)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(196,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(440)                                                         
         DC    AL4(440)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(180)                                                         
         DC    AL4(440)                                                         
*                                                                               
         DC    AL2(197,36,1,1,0,0)  13 WEEK - TX - UNIT 1                       
         DC    AL4(22288)                                                       
         DC    AL4(22288)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(16420)                                                       
         DC    AL4(22288)                                                       
*                                                                               
         DC    AL2(197,36,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(328)                                                         
         DC    AL4(328)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(328)                                                         
*                                                                               
         DC    AL2(197,36,26,255,0,0)  UNITS 26+                                
         DC    AL4(246)                                                         
         DC    AL4(246)                                                         
         DC    AL4(146)                                                         
         DC    AL4(146)                                                         
         DC    AL4(146)                                                         
         DC    AL4(246)                                                         
*                                                                               
         DC    AL2(198,36,1,1,0,0)  13 WEEK - NW - UNIT 1                       
         DC    AL4(21950)                                                       
         DC    AL4(21950)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(16200)                                                       
         DC    AL4(21950)                                                       
*                                                                               
         DC    AL2(198,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         DC    AL4(410)                                                         
         EJECT                                                                  
*              ADDENDUM CABLE                                                   
*                                                                               
         DC    AL2(260,44,0,0,0,0)  1-50,000 SUBSCRIBERS GA                     
         DC    AL4(2078)            PRINCIPAL ON CAMERA                         
         DC    AL4(1418)              "     OFF CAMERA                          
         DC    AL4(1628)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1403)              "   6-8 "    "                            
         DC    AL4(1140)              "    9+ "    "                            
         DC    AL4(581)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(450)               "   6-8  "    "                           
         DC    AL4(379)                  "    9+  "    "                        
*                                                                               
         DC    AL2(262,44,0,0,0,0)  1-50,000 SUBSCRIBERS TX                     
         DC    AL4(2216)            PRINCIPAL ON CAMERA                         
         DC    AL4(1512)              "     OFF CAMERA                          
         DC    AL4(1736)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1496)              "   6-8 "    "                            
         DC    AL4(1216)              "    9+ "    "                            
         DC    AL4(620)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(480)               "   6-8  "    "                           
         DC    AL4(404)               "    9+  "    "                           
*                                                                               
         DC    AL2(263,44,0,0,0,0)  1-50,000 SUBSCRIBERS NW                     
         DC    AL4(675)             PRINCIPAL ON CAMERA                         
         DC    AL4(475)               "     OFF CAMERA                          
         DC    AL4(550)             GROUP 3-5 ON CAMERA                         
         DC    AL4(450)               "   6-8 "    "                            
         DC    AL4(375)               "    9+ "    "                            
         DC    AL4(200)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(150)               "   6-8  "    "                           
         DC    AL4(125)               "    9+  "    "                           
*                                                                               
         DC    AL2(270,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS GA               
         DC    AL4(4178)                                                        
         DC    AL4(2854)                                                        
         DC    AL4(3251)                                                        
         DC    AL4(2831)                                                        
         DC    AL4(2280)                                                        
         DC    AL4(1151)                                                        
         DC    AL4(904)                                                         
         DC    AL4(754)                                                         
*                                                                               
         DC    AL2(272,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS TX               
         DC    AL4(4456)                                                        
         DC    AL4(3044)                                                        
         DC    AL4(3468)                                                        
         DC    AL4(2988)                                                        
         DC    AL4(2432)                                                        
         DC    AL4(1228)                                                        
         DC    AL4(964)                                                         
         DC    AL4(804)                                                         
*                                                                               
         DC    AL2(273,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS NW               
         DC    AL4(1350)                                                        
         DC    AL4(925)                                                         
         DC    AL4(1150)                                                        
         DC    AL4(900)                                                         
         DC    AL4(750)                                                         
         DC    AL4(375)                                                         
         DC    AL4(300)                                                         
         DC    AL4(250)                                                         
*                                                                               
         DC    AL2(280,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS GA              
         DC    AL4(6255)                                                        
         DC    AL4(4283)                                                        
         DC    AL4(4886)                                                        
         DC    AL4(4204)                                                        
         DC    AL4(3428)                                                        
         DC    AL4(1729)                                                        
         DC    AL4(1350)                                                        
         DC    AL4(1133)                                                        
*                                                                               
         DC    AL2(282,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS TX              
         DC    AL4(6672)                                                        
         DC    AL4(4568)                                                        
         DC    AL4(5212)                                                        
         DC    AL4(4484)                                                        
         DC    AL4(3656)                                                        
         DC    AL4(1844)                                                        
         DC    AL4(1440)                                                        
         DC    AL4(1208)                                                        
*                                                                               
         DC    AL2(283,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS NW              
         DC    AL4(2050)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1575)                                                        
         DC    AL4(1350)                                                        
         DC    AL4(1125)                                                        
         DC    AL4(575)                                                         
         DC    AL4(450)                                                         
         DC    AL4(375)                                                         
*                                                                               
         DC    AL2(290,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS GA              
         DC    AL4(8344)                                                        
         DC    AL4(5711)                                                        
         DC    AL4(6506)                                                        
         DC    AL4(5606)                                                        
         DC    AL4(4571)                                                        
         DC    AL4(2325)                                                        
         DC    AL4(1804)                                                        
         DC    AL4(1511)                                                        
*                                                                               
         DC    AL2(292,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS TX              
         DC    AL4(8900)                                                        
         DC    AL4(6092)                                                        
         DC    AL4(6940)                                                        
         DC    AL4(5980)                                                        
         DC    AL4(4876)                                                        
         DC    AL4(2480)                                                        
         DC    AL4(1924)                                                        
         DC    AL4(1612)                                                        
*                                                                               
         DC    AL2(293,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS NW              
         DC    AL4(2700)                                                        
         DC    AL4(1925)                                                        
         DC    AL4(2100)                                                        
         DC    AL4(1825)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(750)                                                         
         DC    AL4(600)                                                         
         DC    AL4(500)                                                         
*                                                                               
         DC    AL2(300,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS GA              
         DC    AL4(10421)                                                       
         DC    AL4(7136)                                                        
         DC    AL4(8134)                                                        
         DC    AL4(7009)                                                        
         DC    AL4(5711)                                                        
         DC    AL4(2891)                                                        
         DC    AL4(2261)                                                        
         DC    AL4(1898)                                                        
*                                                                               
         DC    AL2(302,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS TX              
         DC    AL4(11116)                                                       
         DC    AL4(7612)                                                        
         DC    AL4(8676)                                                        
         DC    AL4(7476)                                                        
         DC    AL4(6092)                                                        
         DC    AL4(3084)                                                        
         DC    AL4(2412)                                                        
         DC    AL4(2024)                                                        
*                                                                               
         DC    AL2(303,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS NW              
         DC    AL4(3375)                                                        
         DC    AL4(2325)                                                        
         DC    AL4(2650)                                                        
         DC    AL4(2275)                                                        
         DC    AL4(1850)                                                        
         DC    AL4(950)                                                         
         DC    AL4(750)                                                         
         DC    AL4(625)                                                         
*                                                                               
         DC    AL2(310,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS GA              
         DC    AL4(20861)                                                       
         DC    AL4(14284)                                                       
         DC    AL4(16279)                                                       
         DC    AL4(14018)                                                       
         DC    AL4(11423)                                                       
         DC    AL4(5775)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(3780)                                                        
*                                                                               
         DC    AL2(312,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS TX              
         DC    AL4(22252)                                                       
         DC    AL4(15236)                                                       
         DC    AL4(17364)                                                       
         DC    AL4(14952)                                                       
         DC    AL4(12184)                                                       
         DC    AL4(6160)                                                        
         DC    AL4(4816)                                                        
         DC    AL4(4032)                                                        
*                                                                               
         DC    AL2(313,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS NW              
         DC    AL4(6750)                                                        
         DC    AL4(4625)                                                        
         DC    AL4(5275)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(1875)                                                        
         DC    AL4(1475)                                                        
         DC    AL4(1225)                                                        
*                                                                               
         DC    AL2(320,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS GA              
         DC    AL4(31283)                                                       
         DC    AL4(21416)                                                       
         DC    AL4(24405)                                                       
         DC    AL4(21023)                                                       
         DC    AL4(17141)                                                       
         DC    AL4(8666)                                                        
         DC    AL4(6773)                                                        
         DC    AL4(5678)                                                        
*                                                                               
         DC    AL2(322,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS TX              
         DC    AL4(33368)                                                       
         DC    AL4(22844)                                                       
         DC    AL4(26032)                                                       
         DC    AL4(22424)                                                       
         DC    AL4(18284)                                                       
         DC    AL4(9244)                                                        
         DC    AL4(7224)                                                        
         DC    AL4(6056)                                                        
*                                                                               
         DC    AL2(323,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS NW              
         DC    AL4(10125)                                                       
         DC    AL4(6950)                                                        
         DC    AL4(7900)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(5550)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2200)                                                        
         DC    AL4(1850)                                                        
*                                                                               
         DC    AL2(330,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS GA            
         DC    AL4(41711)                                                       
         DC    AL4(28560)                                                       
         DC    AL4(32546)                                                       
         DC    AL4(28031)                                                       
         DC    AL4(22856)                                                       
         DC    AL4(11558)                                                       
         DC    AL4(9030)                                                        
         DC    AL4(7571)                                                        
*                                                                               
         DC    AL2(332,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS TX            
         DC    AL4(44492)                                                       
         DC    AL4(30464)                                                       
         DC    AL4(34716)                                                       
         DC    AL4(29900)                                                       
         DC    AL4(24380)                                                       
         DC    AL4(12328)                                                       
         DC    AL4(9632)                                                        
         DC    AL4(8076)                                                        
*                                                                               
         DC    AL2(333,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS NW            
         DC    AL4(13500)                                                       
         DC    AL4(9250)                                                        
         DC    AL4(10525)                                                       
         DC    AL4(9075)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(3750)                                                        
         DC    AL4(2925)                                                        
         DC    AL4(2450)                                                        
*                                                                               
         DC    AL2(350,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS GA                 
         DC    AL4(47081)                                                       
         DC    AL4(35400)                                                       
         DC    AL4(34466)                                                       
         DC    AL4(30518)                                                       
         DC    AL4(25234)                                                       
         DC    AL4(19965)                                                       
         DC    AL4(17329)                                                       
         DC    AL4(14130)                                                       
*                                                                               
         DC    AL2(352,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS TX                 
         DC    AL4(50220)                                                       
         DC    AL4(37760)                                                       
         DC    AL4(36764)                                                       
         DC    AL4(32552)                                                       
         DC    AL4(26916)                                                       
         DC    AL4(21296)                                                       
         DC    AL4(18484)                                                       
         DC    AL4(15072)                                                       
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
         DC    AL1(50),AL3(14975)                                               
         DC    AL1(50),AL3(11340)                                               
         DC    AL1(50),AL3(14975)                                               
         DC    AL1(50),AL3(14975)                                               
         DC    AL1(50),AL3(14975)                                               
         DC    AL1(50),AL3(11340)                                               
         DC    AL1(50),AL3(11340)                                               
         DC    AL1(50),AL3(11340)                                               
*                                                                               
         DC    AL2(44,44,4,13,0,0)  UNITS 4-13                                  
         DC    AL1(25),AL3(14975)                                               
         DC    AL1(25),AL3(11340)                                               
         DC    AL1(25),AL3(14975)                                               
         DC    AL1(25),AL3(14975)                                               
         DC    AL1(25),AL3(14975)                                               
         DC    AL1(25),AL3(11340)                                               
         DC    AL1(25),AL3(11340)                                               
         DC    AL1(25),AL3(11340)                                               
*                                                                               
         DC    AL2(44,44,14,255,0,0)  UNITS 14+                                 
         DC    AL1(15),AL3(14975)                                               
         DC    AL1(15),AL3(11340)                                               
         DC    AL1(15),AL3(14975)                                               
         DC    AL1(15),AL3(14975)                                               
         DC    AL1(15),AL3(14975)                                               
         DC    AL1(15),AL3(11340)                                               
         DC    AL1(15),AL3(11340)                                               
         DC    AL1(15),AL3(11340)                                               
         EJECT                                                                  
         DC    AL2(53,80,1,24,0,0)  TV TAGS - REGULAR, UNITS 1-24               
         DC    AL4(18525)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(0)                                                           
         DC    AL4(18525)                                                       
         DC    AL4(0)                                                           
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
*                                                                               
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(10340)                                                       
         DC    AL4(7790)                                                        
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
*                                                                               
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(5660)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
*                                                                               
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(62775)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(45955)                                                       
         DC    AL4(40690)                                                       
         DC    AL4(33645)                                                       
         DC    AL4(26620)                                                       
         DC    AL4(23105)                                                       
         DC    AL4(18840)                                                       
         DC    AL4(34240)                                                       
         DC    AL4(52245)                                                       
         DC    AL4(19875)                                                       
         DC    AL4(34750)                                                       
         DC    AL4(96690)          PIL                                          
         DC    AL4(74345)          PI                                           
         DC    AL4(41070)          SE                                           
         DC    AL4(10065)          C3,C6                                        
         DC    AL4(19850)          C9                                           
*                                                                               
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(18525)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(18525)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
         DC    AL4(14040)                                                       
*                                                                               
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(10340)                                                       
         DC    AL4(7790)                                                        
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(10340)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
         DC    AL4(7790)                                                        
*                                                                               
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(5660)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(5660)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
         DC    AL4(4245)                                                        
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(27860)                                                       
         DC    AL4(27860)                                                       
         DC    AL4(20525)                                                       
         DC    AL4(18165)                                                       
         DC    AL4(16110)                                                       
         DC    AL4(21425)                                                       
         DC    AL4(9565)                                                        
         DC    AL4(15300)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
         DC    AL4(11530)                                                       
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
         DC    AL4(8275)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
         DC    AL4(4515)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(100440)         (1.6 X SESSION)                              
         DC    AL4(75520)                                                       
         DC    AL4(73528)                                                       
         DC    AL4(65104)                                                       
         DC    AL4(53832)                                                       
         DC    AL4(42592)                                                       
         DC    AL4(36968)                                                       
         DC    AL4(30144)                                                       
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(44576)          (1.6 X SESSION)                              
         DC    AL4(44576)                                                       
         DC    AL4(32840)                                                       
         DC    AL4(29064)                                                       
         DC    AL4(25776)                                                       
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2015 - OCT 31, 2016)  YEAR 1                              
         DC    AL2(440,80,0,0,0,0) INDUST ON CAMERA - CAT1                      
         DC    AL4(50450)          PRINCIPAL                                    
         DC    AL4(32800)          PHD                                          
         DC    AL4(127000)         P3D                                          
         DC    AL4(177300)         P5D                                          
         DC    AL4(195100)         P6D                                          
         DC    AL4(45100)          SD                                           
         DC    AL4(108000)         S3D                                          
         DC    AL4(180200)         S5D/S6D                                      
         DC    AL4(37700)          GD                                           
         DC    AL4(90750)          G3D                                          
         DC    AL4(151100)         G5D/G6D                                      
         DC    AL4(30450)          GS                                           
         DC    AL4(38150)          SO                                           
         DC    AL4(91750)          NAR                                          
         DC    AL4(13150)          BG                                           
         DC    AL4(14450)          BS                                           
         DC    AL4(24500)          BSB                                          
*                                                                               
         DC    AL2(441,80,0,0,0,0) INDUST ON CAMERA - CAT2                      
         DC    AL4(62800)          PRINCIPAL                                    
         DC    AL4(40800)          PHD                                          
         DC    AL4(156600)         P3D                                          
         DC    AL4(219550)         P5D                                          
         DC    AL4(241500)         P6D                                          
         DC    AL4(56250)          SD                                           
         DC    AL4(135100)         S3D                                          
         DC    AL4(225150)         S5D/S6D                                      
         DC    AL4(47350)          GD                                           
         DC    AL4(113300)         G3D                                          
         DC    AL4(188950)         G5D/G6D                                      
         DC    AL4(37550)          GS                                           
         DC    AL4(47100)          SO                                           
         DC    AL4(108800)         NAR                                          
         DC    AL4(13150)          BG                                           
         DC    AL4(14450)          BS                                           
         DC    AL4(24500)          BSB                                          
*                                                                               
         DC    AL2(442,36,0,0,0,0) INDUST OFF CAMERA - CAT1                     
         DC    AL4(0)                                                           
         DC    AL4(41300)          P                                            
         DC    AL4(27100)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(22550)          SO                                           
         DC    AL4(18100)          GS                                           
*                                                                               
         DC    AL2(443,36,0,0,0,0) INDUST OFF CAMERA - CAT2                     
         DC    AL4(0)                                                           
         DC    AL4(46000)          P                                            
         DC    AL4(30450)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(25600)          SO                                           
         DC    AL4(20450)          GS                                           
*                                                                               
* INDUSTRIALS (NOV 1, 2016 - APR 30, 2018)  YEAR 2   +5 TO NUMBER               
         DC    AL2(445,80,0,0,0,0) INDUST ON CAMERA - CAT1                      
         DC    AL4(51950)          PRINCIPAL                                    
         DC    AL4(33750)          PHD                                          
         DC    AL4(130800)         P3D                                          
         DC    AL4(182600)         P5D                                          
         DC    AL4(200950)         P6D                                          
         DC    AL4(46450)          SD                                           
         DC    AL4(111250)         S3D                                          
         DC    AL4(185600)         S5D/S6D                                      
         DC    AL4(38850)          GD                                           
         DC    AL4(93450)          G3D                                          
         DC    AL4(155650)         G5D/G6D                                      
         DC    AL4(31350)          GS                                           
         DC    AL4(39300)          SO                                           
         DC    AL4(94500)          NAR                                          
         DC    AL4(13550)          BG                                           
         DC    AL4(14900)          BS                                           
         DC    AL4(25250)          BSB                                          
*                                                                               
         DC    AL2(446,80,0,0,0,0) INDUST ON CAMERA - CAT2                      
         DC    AL4(64700)          PRINCIPAL                                    
         DC    AL4(42050)          PHD                                          
         DC    AL4(161300)         P3D                                          
         DC    AL4(226150)         P5D                                          
         DC    AL4(248750)         P6D                                          
         DC    AL4(57950)          SD                                           
         DC    AL4(139150)         S3D                                          
         DC    AL4(231900)         S5D/S6D                                      
         DC    AL4(48750)          GD                                           
         DC    AL4(116700)         G3D                                          
         DC    AL4(194600)         G5D/G6D                                      
         DC    AL4(38700)          GS                                           
         DC    AL4(48500)          SO                                           
         DC    AL4(112100)         NAR                                          
         DC    AL4(13550)          BG                                           
         DC    AL4(14900)          BS                                           
         DC    AL4(25250)          BSB                                          
*                                                                               
         DC    AL2(447,36,0,0,0,0) INDUST OFF CAMERA - CAT1                     
         DC    AL4(0)                                                           
         DC    AL4(42550)          P                                            
         DC    AL4(27900)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(23250)          SO                                           
         DC    AL4(18650)          GS                                           
*                                                                               
         DC    AL2(448,36,0,0,0,0) INDUST OFF CAMERA - CAT2                     
         DC    AL4(0)                                                           
         DC    AL4(47400)          P                                            
         DC    AL4(31350)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(26350)          SO                                           
         DC    AL4(21050)          GS                                           
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2015 - OCT 31, 2016)  YEAR 1                              
         DC    AL2(450,36,0,0,0,0) INDUSTRIAL STORECASTING - BUYOUT             
         DC    AL4(123900)                          3X 3 MONTH RATE             
         DC    AL4(123900)         P                                            
         DC    AL4(123900)         SOLO/DUO                                     
         DC    AL4(123900)                                                      
         DC    AL4(123900)         SO                                           
         DC    AL4(123900)         GS                                           
*                                                                               
         DC    AL2(451,36,0,0,0,0) INDUSTRIAL STORECASTING - 3M                 
         DC    AL4(41300)                                                       
         DC    AL4(41300)          P                                            
         DC    AL4(41300)          SOLO/DUO                                     
         DC    AL4(41300)                                                       
         DC    AL4(41300)          SO                                           
         DC    AL4(41300)          GS                                           
*                                                                               
         DC    AL2(452,36,0,0,0,0) INDUSTRIAL STORECASTING - 6M/3MEXT           
         DC    AL4(82600)                2X 3 MONTH RATE                        
         DC    AL4(82600)          P                                            
         DC    AL4(82600)          SOLO/DUO                                     
         DC    AL4(82600)                                                       
         DC    AL4(82600)          SO                                           
         DC    AL4(82600)          GS                                           
*                                                                               
         DC    AL2(453,36,0,0,0,0) INDUSTRIAL STORECASTING - 6MEXT              
         DC    AL4(165200)               4X 3 MONTH RATE                        
         DC    AL4(165200)         P                                            
         DC    AL4(165200)         SOLO/DUO                                     
         DC    AL4(165200)                                                      
         DC    AL4(165200)         SO                                           
         DC    AL4(165200)         GS                                           
*                                                                               
* INDUSTRIALS (NOV 1, 2016 - APR 30, 2018)  YEAR 2      +5 TO NUMBER            
         DC    AL2(455,36,0,0,0,0) INDUSTRIAL STORECASTING - BUYOUT             
         DC    AL4(127650)                          3X 3 MONTH RATE             
         DC    AL4(127650)         P                                            
         DC    AL4(127650)         SOLO/DUO                                     
         DC    AL4(127650)                                                      
         DC    AL4(127650)         SO                                           
         DC    AL4(127650)         GS                                           
*                                                                               
         DC    AL2(456,36,0,0,0,0) INDUSTRIAL STORECASTING - 3M                 
         DC    AL4(42550)                                                       
         DC    AL4(42550)          P                                            
         DC    AL4(42550)          SOLO/DUO                                     
         DC    AL4(42550)                                                       
         DC    AL4(42550)          SO                                           
         DC    AL4(42550)          GS                                           
*                                                                               
         DC    AL2(457,36,0,0,0,0) INDUSTRIAL STORECASTING - 6M/3MEXT           
         DC    AL4(85100)                2X 3 MONTH RATE                        
         DC    AL4(85100)          P                                            
         DC    AL4(85100)          SOLO/DUO                                     
         DC    AL4(85100)                                                       
         DC    AL4(85100)          SO                                           
         DC    AL4(85100)          GS                                           
*                                                                               
         DC    AL2(458,36,0,0,0,0) INDUSTRIAL STORECASTING - 6MEXT              
         DC    AL4(170200)               4X 3 MONTH RATE                        
         DC    AL4(170200)         P                                            
         DC    AL4(170200)         SOLO/DUO                                     
         DC    AL4(170200)                                                      
         DC    AL4(170200)         SO                                           
         DC    AL4(170200)         GS                                           
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2015 - OCT 31, 2016)  YEAR 1                              
         DC    AL2(460,84,0,0,0,0) RTK ON CAMERA - CAT1                         
         DC    AL4(50450)          PRINCIPAL/ST                                 
         DC    AL4(32800)          PHD                                          
         DC    AL4(127000)         P3D                                          
         DC    AL4(177300)         P5D                                          
         DC    AL4(195100)         P6D                                          
         DC    AL4(50450)          S/D                                          
         DC    AL4(30450)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(38150)          SO                                           
         DC    AL4(91750)          NAR                                          
         DC    AL4(13150)          BG                                           
         DC    AL4(14450)          BS                                           
         DC    AL4(24500)          BSB                                          
         DC    AL4(45100)          SD                                           
         DC    AL4(37700)          GD3/GD6/GD9                                  
         DC    AL4(108000)         S3D                                          
         DC    AL4(90750)          G3D                                          
         DC    AL4(180200)         S5D/S6D                                      
         DC    AL4(151100)         G5D/G6D                                      
*                                                                               
         DC    AL2(461,84,0,0,0,0) RTK ON CAMERA - CAT2                         
         DC    AL4(62800)          PRINCIPAL/ST                                 
         DC    AL4(40800)          PHD                                          
         DC    AL4(156600)         P3D                                          
         DC    AL4(219550)         P5D                                          
         DC    AL4(241500)         P6D                                          
         DC    AL4(62800)          S/D                                          
         DC    AL4(37550)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(47100)          SO                                           
         DC    AL4(108800)         NAR                                          
         DC    AL4(13150)          BG                                           
         DC    AL4(14450)          BS                                           
         DC    AL4(24500)          BSB                                          
         DC    AL4(56250)          SD                                           
         DC    AL4(47350)          GD3/GD6/GD9                                  
         DC    AL4(135100)         S3D                                          
         DC    AL4(113300)         G3D                                          
         DC    AL4(225150)         S5D/S6D                                      
         DC    AL4(188950)         G5D/G6D                                      
*                                                                               
         DC    AL2(462,84,0,0,0,0) RTK OFF CAMERA - CAT1                        
         DC    AL4(41300)          PRINCIPAL/ST                                 
         DC    AL4(41300)          PHD                                          
         DC    AL4(41300)          P3D                                          
         DC    AL4(41300)          P5D                                          
         DC    AL4(41300)          P6D                                          
         DC    AL4(27100)          S/D                                          
         DC    AL4(18100)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(22550)          SO                                           
         DC    AL4(41300)          NAR                                          
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
         DC    AL4(46000)          PRINCIPAL/ST                                 
         DC    AL4(46000)          PHD                                          
         DC    AL4(46000)          P3D                                          
         DC    AL4(46000)          P5D                                          
         DC    AL4(46000)          P6D                                          
         DC    AL4(30450)          S/D                                          
         DC    AL4(20450)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(25600)          SO                                           
         DC    AL4(46000)          NAR                                          
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
* INDUSTRIALS (NOV 1, 2016 - APR 30, 2018)  YEAR 2      +5 TO NUMBER            
         DC    AL2(465,84,0,0,0,0) RTK ON CAMERA - CAT1                         
         DC    AL4(51950)          PRINCIPAL/ST                                 
         DC    AL4(33750)          PHD                                          
         DC    AL4(130800)         P3D                                          
         DC    AL4(182600)         P5D                                          
         DC    AL4(200950)         P6D                                          
         DC    AL4(51950)          S/D                                          
         DC    AL4(31350)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(39300)          SO                                           
         DC    AL4(94500)          NAR                                          
         DC    AL4(13550)          BG                                           
         DC    AL4(14900)          BS                                           
         DC    AL4(25250)          BSB                                          
         DC    AL4(46450)          SD                                           
         DC    AL4(38850)          GD3/GD6/GD9                                  
         DC    AL4(111250)         S3D                                          
         DC    AL4(93450)          G3D                                          
         DC    AL4(185600)         S5D/S6D                                      
         DC    AL4(155650)         G5D/G6D                                      
*                                                                               
         DC    AL2(466,84,0,0,0,0) RTK ON CAMERA - CAT2                         
         DC    AL4(64700)          PRINCIPAL/ST                                 
         DC    AL4(42050)          PHD                                          
         DC    AL4(161300)         P3D                                          
         DC    AL4(226150)         P5D                                          
         DC    AL4(248750)         P6D                                          
         DC    AL4(64700)          S/D                                          
         DC    AL4(38700)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(48500)          SO                                           
         DC    AL4(112100)         NAR                                          
         DC    AL4(13550)          BG                                           
         DC    AL4(14900)          BS                                           
         DC    AL4(25250)          BSB                                          
         DC    AL4(57950)          SD                                           
         DC    AL4(48750)          GD3/GD6/GD9                                  
         DC    AL4(139150)         S3D                                          
         DC    AL4(116700)         G3D                                          
         DC    AL4(231900)         S5D/S6D                                      
         DC    AL4(194600)         G5D/G6D                                      
*                                                                               
         DC    AL2(467,84,0,0,0,0) RTK OFF CAMERA - CAT1                        
         DC    AL4(42550)          PRINCIPAL/ST                                 
         DC    AL4(42550)          PHD                                          
         DC    AL4(42550)          P3D                                          
         DC    AL4(42550)          P5D                                          
         DC    AL4(42550)          P6D                                          
         DC    AL4(27900)          S/D                                          
         DC    AL4(18650)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(23250)          SO                                           
         DC    AL4(42550)          NAR                                          
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
         DC    AL2(468,84,0,0,0,0) RTK OFF CAMERA - CAT2                        
         DC    AL4(47400)          PRINCIPAL/ST                                 
         DC    AL4(47400)          PHD                                          
         DC    AL4(47400)          P3D                                          
         DC    AL4(47400)          P5D                                          
         DC    AL4(47400)          P6D                                          
         DC    AL4(31350)          S/D                                          
         DC    AL4(21050)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(26350)          SO                                           
         DC    AL4(47400)          NAR                                          
         DC    AL4(0)              BG           NA                              
         DC    AL4(0)              BS           NA                              
         DC    AL4(0)              BSB          NA                              
         DC    AL4(0)              SD           NA                              
         DC    AL4(0)              GD3/GD6/GD9  NA                              
         DC    AL4(0)              S3D          NA                              
         DC    AL4(0)              G3D          NA                              
         DC    AL4(0)              S5D/S6D      NA                              
         DC    AL4(0)              G5D/G6D      NA                              
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2015 - OCT 31, 2016)  YEAR 1                              
         DC    AL2(470,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT1                  
         DC    AL4(41300)          P    PRINCIPAL                               
         DC    AL4(41300)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(18600)          NP   NON-PRINCIPAL                           
         DC    AL4(27100)          S    SOLO/DUO                                
         DC    AL4(18100)          SO   STEP OUT + STEP OUT PREM INDEXT         
         DC    AL4(22550)          S16  SOLO <16 BARS                           
         DC    AL4(18100)          G3   GROUP                                   
         DC    AL4(22550)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(18100)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(471,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT2                  
         DC    AL4(46000)          P    PRINCIPAL                               
         DC    AL4(46000)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(20650)          NP   NON-PRINCIPAL                           
         DC    AL4(30450)          S    SOLO/DUO                                
         DC    AL4(20450)          SO   STEP OUT + STEP OUT PREM INDEXT         
         DC    AL4(25600)          S16  SOLO <16 BARS                           
         DC    AL4(20450)          G3   GROUP                                   
         DC    AL4(25600)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(20450)          C3   CONTRACTOR                              
*                                                                               
* INDUSTRIALS (NOV 1, 2016 - APR 30, 2018)  YEAR 2      +5 TO NUMBER            
         DC    AL2(475,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT1                  
         DC    AL4(42550)          P    PRINCIPAL                               
         DC    AL4(42550)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(19150)          NP   NON-PRINCIPAL                           
         DC    AL4(27900)          S    SOLO/DUO                                
         DC    AL4(18650)          SO   STEP OUT + STEP OUT PREM INDEXT         
         DC    AL4(23250)          S16  SOLO <16 BARS                           
         DC    AL4(18650)          G3   GROUP                                   
         DC    AL4(23250)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(18650)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(476,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT2                  
         DC    AL4(47400)          P    PRINCIPAL                               
         DC    AL4(47400)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(21250)          NP   NON-PRINCIPAL                           
         DC    AL4(31350)          S    SOLO/DUO                                
         DC    AL4(21050)          SO   STEP OUT + STEP OUT PREM INDEXT         
         DC    AL4(26350)          S16  SOLO <16 BARS                           
         DC    AL4(21050)          G3   GROUP                                   
         DC    AL4(26350)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(21050)          C3   CONTRACTOR                              
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2015 - OCT 31, 2016)  YEAR 1                              
         DC    AL2(480,16,0,0,0,0) IVR INTERACTIVE VOICE                        
         DC    AL4(41300)          P    PRINCIPAL                               
*                                                                               
* INDUSTRIALS (NOV 1, 2016 - APR 30, 2018)  YEAR 2      +5 TO NUMBER            
         DC    AL2(485,16,0,0,0,0) IVR INTERACTIVE VOICE                        
         DC    AL4(42550)          P    PRINCIPAL                               
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
*                                                                               
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(2770)            PRINCIPAL ON CAMERA                         
         DC    AL4(1890)                "     OFF CAMERA                        
         DC    AL4(2170)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1870)              "   6-8 "    "                            
         DC    AL4(1520)              "    9+ "    "                            
         DC    AL4(775)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(600)               "   6-8  "    "                           
         DC    AL4(505)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(5570)                                                        
         DC    AL4(3805)                                                        
         DC    AL4(4335)                                                        
         DC    AL4(3735)                                                        
         DC    AL4(3040)                                                        
         DC    AL4(1535)                                                        
         DC    AL4(1205)                                                        
         DC    AL4(1005)                                                        
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(8340)                                                        
         DC    AL4(5710)                                                        
         DC    AL4(6515)                                                        
         DC    AL4(5605)                                                        
         DC    AL4(4570)                                                        
         DC    AL4(2305)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1510)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(11125)                                                       
         DC    AL4(7615)                                                        
         DC    AL4(8675)                                                        
         DC    AL4(7475)                                                        
         DC    AL4(6095)                                                        
         DC    AL4(3100)                                                        
         DC    AL4(2405)                                                        
         DC    AL4(2015)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(13895)                                                       
         DC    AL4(9515)                                                        
         DC    AL4(10845)                                                       
         DC    AL4(9345)                                                        
         DC    AL4(7615)                                                        
         DC    AL4(3855)                                                        
         DC    AL4(3015)                                                        
         DC    AL4(2530)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(27815)                                                       
         DC    AL4(19045)                                                       
         DC    AL4(21705)                                                       
         DC    AL4(18690)                                                       
         DC    AL4(15230)                                                       
         DC    AL4(7700)                                                        
         DC    AL4(6020)                                                        
         DC    AL4(5040)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(41710)                                                       
         DC    AL4(28555)                                                       
         DC    AL4(32540)                                                       
         DC    AL4(28030)                                                       
         DC    AL4(22855)                                                       
         DC    AL4(11555)                                                       
         DC    AL4(9030)                                                        
         DC    AL4(7570)                                                        
*                                                                               
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(55615)                                                       
         DC    AL4(38080)                                                       
         DC    AL4(43395)                                                       
         DC    AL4(37375)                                                       
         DC    AL4(30475)                                                       
         DC    AL4(15410)                                                       
         DC    AL4(12040)                                                       
         DC    AL4(10095)                                                       
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(62775)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(45955)                                                       
         DC    AL4(40690)                                                       
         DC    AL4(33645)                                                       
         DC    AL4(26620)                                                       
         DC    AL4(23105)                                                       
         DC    AL4(18840)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
*                                                                               
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(14820)          ON CAMERA                                    
         DC    AL4(11232)          OFF                                          
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(11232)                                                       
         DC    AL4(11232)                                                       
         DC    AL4(11232)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(14820)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11232)          SE                                           
         DC    AL4(11232)          C3,C6                                        
         DC    AL4(11232)          C9                                           
*                                                                               
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(8272)           ON CAMERA                                    
         DC    AL4(6232)           OFF                                          
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(6232)                                                        
         DC    AL4(6232)                                                        
         DC    AL4(6232)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(8272)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6232)           SE                                           
         DC    AL4(6232)           C3,C6                                        
         DC    AL4(6232)           C9                                           
*                                                                               
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(4528)           ON CAMERA                                    
         DC    AL4(3396)           OFF                                          
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(3396)                                                        
         DC    AL4(3396)                                                        
         DC    AL4(3396)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(4528)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3396)           SE                                           
         DC    AL4(3396)           C3,C6                                        
         DC    AL4(3396)           C9                                           
*                                                                               
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
         DC    AL4(9224)                                                        
*                                                                               
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
         DC    AL4(6620)                                                        
*                                                                               
         DC    AL2(248,44,50,255,0,0)  UNITS 51+                                
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         DC    AL4(3612)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
*                                                                               
         DC    AL2(249,88,1,24,0,0)  GA - TV, REGULAR, UNITS 1-24               
         DC    AL4(16117)          ON CAMERA                                    
         DC    AL4(12215)          OFF                                          
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(12215)                                                       
         DC    AL4(12215)                                                       
         DC    AL4(12215)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(16117)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(12215)          SE                                           
         DC    AL4(12215)          C3,C6                                        
         DC    AL4(12215)          C9                                           
         DC    AL4(16117)          SOLO/DUO ON CAMERA                           
         DC    AL4(12215)                   OFF                                 
*                                                                               
         DC    AL2(249,88,25,49,0,0)  UNITS 25-49                               
         DC    AL4(8996)           ON CAMERA                                    
         DC    AL4(6777)           OFF                                          
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(6777)                                                        
         DC    AL4(6777)                                                        
         DC    AL4(6777)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(8996)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6777)           SE                                           
         DC    AL4(6777)           C3,C6                                        
         DC    AL4(6777)           C9                                           
         DC    AL4(8996)           SOLO/DUO ON CAMERA                           
         DC    AL4(6777)           OFF                                          
*                                                                               
         DC    AL2(249,88,50,255,0,0)  UNITS 51+                                
         DC    AL4(4924)           ON CAMERA                                    
         DC    AL4(3693)           OFF                                          
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(3693)                                                        
         DC    AL4(3693)                                                        
         DC    AL4(3693)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(4924)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3693)           SE                                           
         DC    AL4(3693)           C3,C6                                        
         DC    AL4(3693)           C9                                           
         DC    AL4(4924)           SOLO/DUO ON CAMERA                           
         DC    AL4(3693)           OFF                                          
         EJECT                                                                  
*              RATES FOR NORTHWEST ADDENDUM TAGS                                
*                                                                               
         DC    AL2(340,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  2WK            
         DC    AL4(13150)          ON CAMERA                                    
         DC    AL4(10050)          OFF                                          
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(10050)                                                       
         DC    AL4(10050)                                                       
         DC    AL4(10050)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(13150)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10050)          SE                                           
         DC    AL4(10050)          C3,C6                                        
         DC    AL4(10050)          C9                                           
*                                                                               
         DC    AL2(340,80,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(10600)          ON CAMERA                                    
         DC    AL4(8050)           OFF                                          
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(8050)                                                        
         DC    AL4(8050)                                                        
         DC    AL4(8050)                                                        
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8050)           SE                                           
         DC    AL4(8050)           C3,C6                                        
         DC    AL4(8050)           C9                                           
*                                                                               
         DC    AL2(340,80,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(5950)           ON CAMERA                                    
         DC    AL4(4450)           OFF                                          
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(4450)                                                        
         DC    AL4(4450)                                                        
         DC    AL4(4450)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(5950)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4450)           SE                                           
         DC    AL4(4450)           C3,C6                                        
         DC    AL4(4450)           C9                                           
*                                                                               
         DC    AL2(340,80,25,255,0,0)  UNITS 26+  2-WK                          
         DC    AL4(2850)           ON CAMERA                                    
         DC    AL4(2450)           OFF                                          
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2450)                                                        
         DC    AL4(2450)                                                        
         DC    AL4(2450)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(2850)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2450)           SE                                           
         DC    AL4(2450)           C3,C6                                        
         DC    AL4(2450)           C9                                           
*                                                                               
         DC    AL2(341,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  13WK           
         DC    AL4(15350)          ON CAMERA                                    
         DC    AL4(11550)          OFF                                          
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(11550)                                                       
         DC    AL4(11550)                                                       
         DC    AL4(11550)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(15350)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11550)          SE                                           
         DC    AL4(11550)          C3,C6                                        
         DC    AL4(11550)          C9                                           
*                                                                               
         DC    AL2(341,80,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(12600)          ON CAMERA                                    
         DC    AL4(9450)           OFF                                          
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(9450)                                                        
         DC    AL4(9450)                                                        
         DC    AL4(9450)                                                        
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9450)           SE                                           
         DC    AL4(9450)           C3,C6                                        
         DC    AL4(9450)           C9                                           
*                                                                               
         DC    AL2(341,80,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(7000)           ON CAMERA                                    
         DC    AL4(5100)           OFF                                          
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(5100)                                                        
         DC    AL4(5100)                                                        
         DC    AL4(5100)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(7000)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5100)           SE                                           
         DC    AL4(5100)           C3,C6                                        
         DC    AL4(5100)           C9                                           
*                                                                               
         DC    AL2(341,80,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(3900)           ON CAMERA                                    
         DC    AL4(2750)           OFF                                          
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(2750)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(3900)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2750)           SE                                           
         DC    AL4(2750)           C3,C6                                        
         DC    AL4(2750)           C9                                           
*                                                                               
         DC    AL2(390,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 2-WK            
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
         DC    AL4(6150)                                                        
*                                                                               
         DC    AL2(390,44,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
         DC    AL4(4550)                                                        
*                                                                               
         DC    AL2(390,44,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
         DC    AL4(2550)                                                        
*                                                                               
         DC    AL2(390,44,25,255,0,0)  UNITS 26+   2-WK                         
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
         DC    AL4(1500)                                                        
*                                                                               
         DC    AL2(343,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 13-WK           
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
         DC    AL4(7650)                                                        
*                                                                               
         DC    AL2(343,44,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
         DC    AL4(5850)                                                        
*                                                                               
         DC    AL2(343,44,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
         DC    AL4(3200)                                                        
*                                                                               
         DC    AL2(343,44,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
         DC    AL4(1800)                                                        
*                                                                               
         DC    AL2(344,44,1,24,0,0)  GA - ADO TAGS UNITS 1-24                   
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
         DC    AL4(10031)                                                       
*                                                                               
         DC    AL2(344,44,25,49,0,0)  GA - ADO TAGS UNITS 25-49                 
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
         DC    AL4(7199)                                                        
*                                                                               
         DC    AL2(344,44,50,255,0,0)  GA - ADO TAGS UNITS 50+                  
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
         DC    AL4(3928)                                                        
*                                                                               
*                                     (LOOK @ TASYSCALC / KSFRSTAG)             
         DC    AL2(345,44,1,255,0,0)  KS - ADO TAGS UNITS 1-24                  
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
         DC    AL4(4892)                                                        
*                                                                               
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
*                                                                               
         DC    AL2(250,84,0,0,0,0)  PRM FOR SAG AND AFT                         
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(25440)          OFF                                          
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(34240)          ON                                           
         DC    AL4(10280)          SAG ONLY EXTRA                               
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(251,84,0,0,0,0)  PRR FOR AFT AND SAG                         
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(24805)          OFF                                          
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(34240)          ON CAMERA                                    
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(10280)          EXTRA                                        
         DC    AL4(34240)          ON                                           
         DC    AL4(10280)          N/D                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          OFF                                          
         DC    AL4(24805)          SOLO/DUO OFF CAM                             
         EJECT                                                                  
VNWTAB   DC    AL2(252,44,1,1,0,0)  VNW 1ST USE AT SESSION FEE                  
         DC    AL4(62775)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(47200)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(45955)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(40690)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(33645)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(26620)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(23105)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(18840)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(62775)                                               
         DC    AL1(50),AL3(47200)                                               
         DC    AL1(50),AL3(45955)                                               
         DC    AL1(50),AL3(40690)                                               
         DC    AL1(50),AL3(33645)                                               
         DC    AL1(50),AL3(26620)                                               
         DC    AL1(50),AL3(23105)                                               
         DC    AL1(50),AL3(18840)                                               
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
EQYTAB   DC    AL2(255,80,1,1,0,0)  EQUITY 6M  TV                               
         DC    AL4(70300)              PRINCIPAL ON  CAMERA                     
         DC    AL4(70300)                  "     OFF   "                        
         DC    AL4(70300)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(70300)                "    6-8    "                          
         DC    AL4(70300)                "     9+    "                          
         DC    AL4(70300)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(70300)                "    6-8    "                          
         DC    AL4(70300)                "     9+    "                          
         DC    AL4(70300)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(70300)              HAND MODEL UNLIMITED                     
         DC    AL4(70300)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(70300)              HAND MODEL 13 WEEKS                      
         DC    AL4(70300)    PIL       PILOT LOCATION RATE                      
         DC    AL4(70300)    PI        PILOT STUDIO RATE                        
         DC    AL4(70300)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(70300)    C3,C6     CONTRACTORS                              
         DC    AL4(70300)    C9             "                                   
*                                                                               
         DC    AL2(261,80,1,1,0,0)  EQUITY 1Y  TV                               
         DC    AL4(123365)             PRINCIPAL ON  CAMERA                     
         DC    AL4(123365)                 "     OFF   "                        
         DC    AL4(123365)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(123365)               "    6-8    "                          
         DC    AL4(123365)               "     9+    "                          
         DC    AL4(123365)             GROUPS 3-5 OFF CAMERA                    
         DC    AL4(123365)               "    6-8    "                          
         DC    AL4(123365)               "     9+    "                          
         DC    AL4(123365)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(123365)             HAND MODEL UNLIMITED                     
         DC    AL4(123365)             COMM'L EXTRA 13 WEEKS                    
         DC    AL4(123365)             HAND MODEL 13 WEEKS                      
         DC    AL4(123365)   PIL       PILOT LOCATION RATE                      
         DC    AL4(123365)   PI        PILOT STUDIO RATE                        
         DC    AL4(123365)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(123365)   C3,C6     CONTRACTORS                              
         DC    AL4(123365)   C9             "                                   
*                                                                               
         DC    AL2(264,44,1,1,0,0)  EQUITY 6M  RADIO                            
         DC    AL4(25000)              ANN ALONE                                
         DC    AL4(25000)              AR,AS,P,ANN,S,1-4MS,1-4SS                
         DC    AL4(25000)              1-4M3,1-4S3,D3,S3                        
         DC    AL4(25000)              1-4M6,1-4S6,D6,S6                        
         DC    AL4(25000)              1-4M9,1-4S9,D9,S9                        
         DC    AL4(25000)              SE                                       
         DC    AL4(25000)              C3,C6                                    
         DC    AL4(25000)              C9                                       
*                                                                               
         DC    AL2(268,44,1,1,0,0)  EQUITY 1Y  RADIO                            
         DC    AL4(41715)              ANN ALONE                                
         DC    AL4(41715)              AR,AS,P,ANN,S,1-4MS,1-4SS                
         DC    AL4(41715)              1-4M3,1-4S3,D3,S3                        
         DC    AL4(41715)              1-4M6,1-4S6,D6,S6                        
         DC    AL4(41715)              1-4M9,1-4S9,D9,S9                        
         DC    AL4(41715)              SE                                       
         DC    AL4(41715)              C3,C6                                    
         DC    AL4(41715)              C9                                       
*                                                                               
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
         DC    AL2(61),AL1(UBSR,ALL,AFT+NON,0,0,0)                              
         DC    AL1(RADIO+INTERNET+NEWMEDIA)                                     
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
         DC    AL2(540),AL1(UMVI,UMVII4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(87),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(UMVI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(UMVI,UMVII4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(88),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(UMVI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(UMVN,UMVNI4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(87),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(UMVN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(UMVN,UMVNI4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(88),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(UMVN,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(USMI,USMII4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(87),AL1(USMI,USMII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(USMI,USMII4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(88),AL1(USMI,USMII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(USMI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(USMN,USMNI4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(87),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(USMN,USMNI4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(88),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(USMN,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(UINU,UINUI4W,ALL-AFM,0,0,0,INTERNET)                
         DC    AL2(69),AL1(UINU,UINUI8W,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(76),AL1(UINU,UINUI1Y,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(76),AL1(UINU,UINUEXT,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(542),AL1(UINU,UINUR4W,AFT+NON,0,0,0,INTERNET)                
         DC    AL2(78),AL1(UINU,UINUR8W,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(77),AL1(UINU,UINUR1Y,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(77),AL1(UINU,UINUREX,AFT+NON,0,0,0,INTERNET)                 
*                                                                               
         DC    AL2(540),AL1(UNMU,UNMUI4W,ALL-AFM,0,0,0,NEWMEDIA)                
         DC    AL2(69),AL1(UNMU,UNMUI8W,ALL-AFM,0,0,0,NEWMEDIA)                 
         DC    AL2(76),AL1(UNMU,UNMUI1Y,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(UNMU,UNMUEXT,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(542),AL1(UNMU,UNMUR4W,AFT+NON,0,0,0,NEWMEDIA)                
         DC    AL2(78),AL1(UNMU,UNMUR8W,AFT+NON,0,0,0,NEWMEDIA)                 
         DC    AL2(77),AL1(UNMU,UNMUR1Y,AFT+NON,0,0,0,LIKETV)                   
         DC    AL2(77),AL1(UNMU,UNMUREX,AFT+NON,0,0,0,LIKETV)                   
*                                                                               
         DC    AL2(540),AL1(USIU,USIUI4W,ALL-AFM,0,0,0,INTERNET)                
         DC    AL2(69),AL1(USIU,USIUI8W,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(76),AL1(USIU,USIUI1Y,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(76),AL1(USIU,USIUEXT,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(542),AL1(USIU,USIUR4W,AFT+NON,0,0,0,INTERNET)                
         DC    AL2(78),AL1(USIU,USIUR8W,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(77),AL1(USIU,USIUR1Y,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(77),AL1(USIU,USIUREX,AFT+NON,0,0,0,INTERNET)                 
*                                                                               
         DC    AL2(540),AL1(USNU,USNUI4W,ALL-AFM,0,0,0,NEWMEDIA)                
         DC    AL2(69),AL1(USNU,USNUI8W,ALL-AFM,0,0,0,NEWMEDIA)                 
         DC    AL2(76),AL1(USNU,USNUI1Y,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(USNU,USNUEXT,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(542),AL1(USNU,USNUR4W,AFT+NON,0,0,0,NEWMEDIA)                
         DC    AL2(78),AL1(USNU,USNUR8W,AFT+NON,0,0,0,NEWMEDIA)                 
         DC    AL2(77),AL1(USNU,USNUR1Y,AFT+NON,0,0,0,LIKETV)                   
         DC    AL2(77),AL1(USNU,USNUREX,AFT+NON,0,0,0,LIKETV)                   
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
         DC    AL2(42),AL1(USNA,ALL,ALL,0,0,0,LIKETV)     DEMO                  
         DC    AL2(43),AL1(USNA,ALL,AFT+NON,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(63),AL1(UHLD,ALL,ALL-AFM,0,0,0,LIKETV+CABLE) HLD FEE         
         DC    AL2(63),AL1(USHL,ALL,ALL-AFM,0,0,0,LIKETV+CABLE) SPA HLD         
         DC    AL2(38),AL1(UREN,ALL,ALL-AFM,0,0,0,LIKETV) REINST                
         DC    AL2(38),AL1(USRE,ALL,ALL-AFM,0,0,0,LIKETV) REINST                
         DC    AL2(225),AL1(UADH,ALL,ALL-AFM,0,0,0,LIKETV+CABLE) AD HLD         
         DC    AL2(230),AL1(UARN,ALL,ALL-AFM,0,0,0,LIKETV) AD REINST            
*                                                                               
         DC    AL2(00),AL1(UCLA,UCLAREG,ALL,0,0,0,ALL)    CLA                   
         DC    AL2(08),AL1(UCLA,UCLAG13,ALL,0,0,0,ALL)    13 USE GUAR           
         DC    AL2(75),AL1(UPAX,UPAXREG,ALL,0,0,0,ALL)    PAX                   
         DC    AL2(58),AL1(ULNA,ALL,ALL,0,0,0,ALL)        LT NGT ABC            
         DC    AL2(58),AL1(ULNN,ALL,ALL,0,0,0,ALL)               NBC            
         DC    AL2(58),AL1(ULNC,ALL,ALL,0,0,0,ALL)               CBS            
         DC    AL2(58),AL1(ULNF,ALL,ALL,0,0,0,ALL)               FOX            
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
         DC    AL2(210),AL1(UDLR,UDLRANY8,ALL,0,0,0,ALL)  DLR A + NY 8W         
         DC    AL2(211),AL1(UDLR,UDLRA8,ALL,0,0,0,ALL)    DLR A - NY 8W         
         DC    AL2(212),AL1(UDLR,UDLRBNY8,ALL,0,0,0,ALL)  DLR B + NY 8W         
         DC    AL2(213),AL1(UDLR,UDLRB8,ALL,0,0,0,ALL)    DLR B - NY 8W         
         DC    AL2(214),AL1(UDLR,UDLRRAD8,ALL,0,0,0,ALL)  DLR RADIO  8W         
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
         DC    AL2(35),AL1(UMUS,UMUS2DS,ALL,0,0,0,ALL)    2ND DUB TO SH         
         DC    AL2(35),AL1(UNBM,ALL,ALL,0,0,0,ALL)        NON-BRD MUS           
         DC    AL2(09),AL1(UMUS,UMUSDUB8,ALL,0,0,0,ALL)   DUB-8WK               
         DC    AL2(09),AL1(UMUS,UMUS8W,ALL,0,0,0,ALL)     REUSE-8WK             
         DC    AL2(09),AL1(UMUS,UMUSNEW8,ALL,0,0,0,ALL)   NEW-8WK               
         DC    AL2(09),AL1(UMUS,UMUSDSH8,ALL,0,0,0,ALL)   DUB TO SH 8W          
         DC    AL2(36),AL1(UFMU,ALL,ALL,0,0,0,ALL)        1ST REUSE             
         DC    AL2(500),AL1(UMVM,UMVM8WK,ALL,0,0,0,ALL)   MVM-8WK               
         DC    AL2(510),AL1(UMVM,UMVM1YR,ALL,0,0,0,ALL)   MVM-1YR               
         DC    AL2(520),AL1(UMVM,UMVM26W,ALL,0,0,0,ALL)   MVM-26W               
         DC    AL2(530),AL1(UNIM,UNIM8WK,ALL,0,0,0,ALL)   NIM-8WK               
         DC    AL2(510),AL1(UNIM,UNIM1YR,ALL,0,0,0,ALL)   NIM-1YR               
         DC    AL2(520),AL1(UNIM,UNIM6MO,ALL,0,0,0,ALL)   NIM-6MO               
         DC    AL2(35),AL1(UIHM,ALL,ALL,0,0,0,ALL)        IHM                   
         DC    AL2(538),AL1(UMBO,UMBOI1Y,ALL,0,0,0,ALL)   MBO-INIT              
         DC    AL2(539),AL1(UMBO,UMBOR1Y,ALL,0,0,0,ALL)   MBO-REUSE             
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
         DC    AL2(410),AL1(USFR,USFRA,ALL,0,0,0,LIKETV)     SP FGN A           
         DC    AL2(411),AL1(USFR,USFRB,ALL,0,0,0,LIKETV)     SP FGN B           
         DC    AL2(410),AL1(USFR,USFRC,ALL,0,0,0,LIKETV)     SP FGN C           
*                                                                               
         DC    AL2(62),AL1(UPBS,ALL,ALL-AFM,0,0,0,LIKETV) PUB SVC               
         DC    AL2(52),AL1(UPBS,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(40),AL1(USNT,ALL,ALL,0,0,0,LIKETV)    SPAN NWK               
         DC    AL2(40),AL1(USNW,ALL,ALL,0,0,0,LIKETV)    SPAN N/W COMB          
         DC    AL2(110),AL1(USWS,ALL,ALL,0,0,0,LIKETV)   SPAN WSP               
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
         DC    AL2(205),AL1(UADD,ALL,ALL,0,0,0,LIKETV)    AD DEMO               
         DC    AL2(215),AL1(UADD,ALL,AFT+NON,0,0,0,RADIO)                       
*                                                                               
         DC    AL2(135),AL1(UADW,UADW3D,ALL,0,0,0,LIKETV) AD WSP-TV-3D          
         DC    AL2(140),AL1(UADW,UADW1W,ALL,0,0,0,LIKETV) 1WK                   
         DC    AL2(140),AL1(UADW,UADW2W,ALL,0,0,0,LIKETV) 2WK(NW ONLY)          
         DC    AL2(150),AL1(UADW,UADW4W,ALL,0,0,0,LIKETV) 4WK                   
         DC    AL2(150),AL1(UADW,UADW31D,ALL,0,0,0,LIKETV) 31D(KS ONLY)         
         DC    AL2(160),AL1(UADW,UADW13W,ALL,0,0,0,LIKETV) 13WK                 
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
         DC    AL2(350),AL1(UACB,UACBMAX,ALL,0,0,0,ALL)                         
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
         DC    AL2(135),AL1(UADC,UADC3D,ALL,0,0,0,LIKETV) CMB S/W-TV-3D         
         DC    AL2(140),AL1(UADC,UADC1W,ALL,0,0,0,LIKETV) 1WK                   
         DC    AL2(140),AL1(UADC,UADC2W,ALL,0,0,0,LIKETV) 2WK(NW ONLY)          
         DC    AL2(150),AL1(UADC,UADC4W,ALL,0,0,0,LIKETV) 4WK                   
         DC    AL2(150),AL1(UADC,UADC31D,ALL,0,0,0,LIKETV) 31D(KS ONLY)         
         DC    AL2(160),AL1(UADC,UADC13W,ALL,0,0,0,LIKETV) 13WK                 
*                                                                               
         DC    AL2(170),AL1(UADC,UADC3D,ALL,0,0,0,RADIO) CMB S/W-RAD-3D         
         DC    AL2(175),AL1(UADC,UADC1W,ALL,0,0,0,RADIO) 1WK                    
         DC    AL2(175),AL1(UADC,UADC2W,ALL,0,0,0,RADIO) 2WK(NW ONLY)           
         DC    AL2(185),AL1(UADC,UADC4W,ALL,0,0,0,RADIO) 4WK                    
         DC    AL2(185),AL1(UADC,UADC31D,ALL,0,0,0,RADIO) 31D(KS ONLY)          
         DC    AL2(195),AL1(UADC,UADC13W,ALL,0,0,0,RADIO) 13WK                  
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
*        DC    AL2(441),AL1(UINS,ALL,ALL,0,0,0,ALL) COMMENT OUT   CAT2          
         DC    AL2(442),AL1(UIDS,ALL,ALL,0,0,0,ALL)         IDS - CAT1          
*        DC    AL2(443),AL1(UIDS,ALL,ALL,0,0,0,ALL) COMMENT OUT   CAT2          
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
         DC    AL2(255),AL1(UEQY,UEQY6M,SAG+AFT,0,0,0,LIKETV)                   
         DC    AL2(261),AL1(UEQY,UEQY1Y,SAG+AFT,0,0,0,LIKETV)                   
         DC    AL2(264),AL1(UEQY,UEQY6M,SAG+AFT,0,0,0,RADIO)                    
         DC    AL2(268),AL1(UEQY,UEQY1Y,SAG+AFT,0,0,0,RADIO)                    
         DC    AL2(255),AL1(UEQY,UEQY6M,SAG+AFT,0,0,0,CABLE)                    
         DC    AL2(261),AL1(UEQY,UEQY1Y,SAG+AFT,0,0,0,CABLE)                    
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
****** USE NUMBERS 410-411 USE FOR NEW SFR RATES                                
******  USE NUMBERS 423-439 USED FOR AFM RATES                                  
******  USE NUMBERS 500-507 USED FOR MVM 8WK RATES                              
******  USE NUMBERS 510-517 USED FOR MVM/NIM 1YR RATES                          
******  USE NUMBERS 520-527 USED FOR NIM 6MO RATES                              
******  USE NUMBERS 530-537 USED FOR NIM 8WK RATES                              
******  USE NUMBERS 538-539 USED FOR MBO RATES                                  
******  USE NUMBERS 540-542 USED FOR INTERNET/NEWMEDIA 4WK TV/RAD               
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              PERCENTAGE TABLES FOR MULTITRACKING & SWEETENING                 
*                                                                               
         SPACE 3                                                                
MSWEET   DS    0CL3                                                             
         DC    AL1(CTSS1),AL2(100)   1979 CODE            SWEETENING            
         DC    AL1(CTSS2),AL2(200)                        1-2 SINGERS           
         DC    AL1(CTSS3),AL2(300)                                              
         DC    AL1(CTSS4),AL2(400)                                              
         DC    X'FF',AL2(50)                              ALL OTHERS            
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
         DC    AL4(14975)          TV SESSION TAG FEE - ON CAMERA               
         DC    AL4(11345)          TV SESSION TAG FEE - OFF                     
         DC    AL4(10325)          RADIO SESSION TAG FEE                        
         DC    AL4(16110)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(12190)          GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(10070)          GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(10303)          KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(7155)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4892)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(14820)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(11230)          TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(9220)           TX ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(15350)          NW ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(11550)          NW ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(7650)           NW ADDENDUM RADIO SESSION TAG FEE            
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
         DC    AL1(4,CTGD9)                                                     
         DC    AL1(4,CTGDP)                                                     
*                                                                               
         DC    AL1(5,CTG9)                                                      
         DC    AL1(5,CTG9M)                                                     
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
*                                  IF DEFINE ROW >= 18, CHANGE SYSCALC          
         DC    AL1(14,CTPI)                                                     
         DC    AL1(14,CTNAR)                                                    
*                                  IF DEFINE ROW >= 18, CHANGE SYSCALC          
         DC    AL1(15,CTBG)                                                     
*                                                                               
         DC    AL1(16,CTBS)                                                     
*                                                                               
         DC    AL1(17,CTBSB)                                                    
*                                                                               
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
         DC    AL1(7,CTGD9)                                                     
         DC    AL1(7,CTGDP)                                                     
*                                                                               
         DC    AL1(8,CTG9)                                                      
         DC    AL1(8,CTG9M)                                                     
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
         DC    AL1(4,CTGDP)                                                     
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
         DC    AL1(14,CTGDP)       GROUP DANCER                                 
*                                                                               
         DC    AL1(15,CTS3D)       SOLO DANCER 3 DAY                            
*                                                                               
         DC    AL1(16,CTG3D)       GROUP DANCERS - 3 DAYS                       
*                                                                               
         DC    AL1(17,CTS5D)       SOLO DANCER 5 DAY                            
         DC    AL1(17,CTS6D)       SOLO DANCER 6 DAY                            
*                                                                               
         DC    AL1(18,CTG5D)       GROUP DANCERS - 5 DAYS                       
         DC    AL1(18,CTG6D)       GROUP DANCERS - 6 DAYS                       
*                                                                               
         DC    X'FF'                                                            
*              INDUSTRIAL RATES                                                 
*                                                                               
*****    DS    AL4(CAT1),AL4(CAT2)                                              
*                                                                               
**********************************************************************          
* YEAR 1 (IF SIZE CHANGES (128 BYTES) UPDATE TASYSCALC)              *          
**********************************************************************          
INDEXT   DS    0F                                                               
         DC    AL4(12100,12100)    OFF CAMERA SESS - ADDT'L 1/2 HOUR            
*                                  DAY PERFORMER ROW                            
         DC    AL4(99750,18700,18700)     CEILING / OT / DT RATE                
*                                  3-DAY PERFORMER ROW                          
         DC    AL4(299300,18700,25000)    CEILING / OT / DT RATE                
*                                  WEEKLY PERFORMER ROW                         
         DC    AL4(332550,12500,16600)    CEILING / OT / DT RATE                
*                                                                               
         DC    AL4(50450,62800)    NARRATOR - ADDITIONAL DAYS                   
*                                                                               
         DC    AL4(12100,12100)    ENTIRE SCRIPT ADD'L 1/2HR                    
*                                                                               
         DC    AL4(22450,22450)    PARTIAL SCRIPT FIRST 1/2HR                   
*                                                                               
         DC    AL4(12100,12100)    ADDITIONAL 1/2HR FOR PRINCIPAL               
*                                                                               
         DC    AL4(7300,8100)      ADDITIONAL 1/2HR FOR NON-PRINCIPAL           
*                                                                               
         DC    AL4(22450,22450)    RETAKES FOR PRINCIPAL - 1/2HR RATE           
*                                                                               
         DC    AL4(2850,2850)      STEP OUT PREMIUM                             
*                                                                               
         DC    AL4(24650,27200)    P3M FIRST 1/2 HR RATE                        
*                                                                               
         DC    AL4(50450,62800)    PHD DAY RATE                                 
*                                                                               
         DC    AL4(12100,12100)    IVR ADD'L 1/2 HOUR RATE, CAT1 AND 2          
*                                                                               
         DC    XL4'FFFFFFFF'                                                    
**********************************************************************          
* YEAR 2  (+128 FROM INDEXT)                                         *          
**********************************************************************          
         DC    AL4(12450,12450)    OFF CAMERA SESS - ADDT'L 1/2 HOUR            
*                                  DAY PERFORMER ROW                            
         DC    AL4(102750,19250,19250)     CEILING / OT / DT RATE               
*                                  3-DAY PERFORMER ROW                          
         DC    AL4(308300,19250,25750)    CEILING / OT / DT RATE                
*                                  WEEKLY PERFORMER ROW                         
         DC    AL4(342550,12900,17100)    CEILING / OT / DT RATE                
*                                                                               
         DC    AL4(51950,64700)    NARRATOR - ADDITIONAL DAYS                   
*                                                                               
         DC    AL4(12450,12450)    ENTIRE SCRIPT ADD'L 1/2HR                    
*                                                                               
         DC    AL4(23100,23100)    PARTIAL SCRIPT FIRST 1/2HR                   
*                                                                               
         DC    AL4(12450,12450)    ADDITIONAL 1/2HR FOR PRINCIPAL               
*                                                                               
         DC    AL4(7500,8350)      ADDITIONAL 1/2HR FOR NON-PRINCIPAL           
*                                                                               
         DC    AL4(23100,23100)    RETAKES FOR PRINCIPAL - 1/2HR RATE           
*                                                                               
         DC    AL4(2950,2950)      STEP OUT PREMIUM                             
*                                                                               
         DC    AL4(25400,28000)    P3M FIRST 1/2 HR RATE                        
*                                                                               
         DC    AL4(51950,64700)    PHD DAY RATE                                 
*                                                                               
         DC    AL4(12450,12450)    IVR ADD'L 1/2 HOUR RATE, CAT1 AND 2          
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
**PAN#1  DC    CL21'163TACONPREV 11/02/16'                                      
         END                                                                    
