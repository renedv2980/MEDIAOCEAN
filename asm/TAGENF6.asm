*          DATA SET TAGENF6    AT LEVEL 002 AS OF 07/12/16                      
*PHASE T702F6B,*                                                                
         TITLE 'T702F6 - TABLES FOR 2009 CONTRACT'                              
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
         DC    AL4(59220)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(44530)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(43355)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(38385)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(31740)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(25115)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(21795)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17775)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(13580)                                                       
         DC    AL4(10625)                                                       
         DC    AL4(12585)                                                       
         DC    AL4(10775)                                                       
         DC    AL4(8820)                                                        
         DC    AL4(6830)                                                        
         DC    AL4(5935)                                                        
         DC    AL4(4870)                                                        
*                                                                               
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(10775)                                                       
         DC    AL4(8455)                                                        
         DC    AL4(9855)                                                        
         DC    AL4(8930)                                                        
         DC    AL4(7300)                                                        
         DC    AL4(6380)                                                        
         DC    AL4(5460)                                                        
         DC    AL4(4460)                                                        
*                                                                               
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(10775)                                                       
         DC    AL4(8455)                                                        
         DC    AL4(9300)                                                        
         DC    AL4(8375)                                                        
         DC    AL4(6860)                                                        
         DC    AL4(5825)                                                        
         DC    AL4(5085)                                                        
         DC    AL4(4165)                                                        
*                                                                               
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(5165)                                                        
         DC    AL4(3840)                                                        
         DC    AL4(3210)                                                        
         DC    AL4(2730)                                                        
         DC    AL4(2215)                                                        
         DC    AL4(2325)                                                        
         DC    AL4(2185)                                                        
         DC    AL4(1810)                                                        
*                                                                               
PAXTAB   DC    AL2(75,44,1,255,0,0)   PAX USE                                   
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(2215)           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(1660)           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(1380)           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(1175)           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(950)            'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(1005)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(935)            'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(780)            'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
CLBTAB   DC    AL2(1,44,0,0,0,0)   CLASS B WITH NY                              
         DC    AL4(112050)                                                      
         DC    AL4(80135)                                                       
         DC    AL4(71365)                                                       
         DC    AL4(63105)                                                       
         DC    AL4(51590)                                                       
         DC    AL4(26295)                                                       
         DC    AL4(21920)                                                       
         DC    AL4(17915)                                                       
*                                                                               
CBXTAB   DC    AL2(2,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(91390)                                                       
         DC    AL4(63475)                                                       
         DC    AL4(71365)                                                       
         DC    AL4(63105)                                                       
         DC    AL4(51590)                                                       
         DC    AL4(26295)                                                       
         DC    AL4(21920)                                                       
         DC    AL4(17915)                                                       
*                                                                               
CLCTAB   DC    AL2(3,44,0,0,0,0)   CLASS C                                      
         DC    AL4(54460)                                                       
         DC    AL4(36310)                                                       
         DC    AL4(47200)                                                       
         DC    AL4(41950)                                                       
         DC    AL4(34300)                                                       
         DC    AL4(20915)                                                       
         DC    AL4(17410)                                                       
         DC    AL4(14280)                                                       
*                                                                               
NWKTBL   DC    AL2(58,44,1,1,0,0)  NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(32660)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(24555)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(23915)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(21175)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(17580)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(13855)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(12015)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(9800)           'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
*                                                                               
DANTAB   DC    AL2(4,44,0,0,0,0)   DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(222245)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(154795)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(166875)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(147100)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(114325)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(68220)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(59745)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(42655)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB   DC    AL2(5,44,0,0,0,0)   DEALER A W/O NY                              
         DC    AL4(196550)                                                      
         DC    AL4(141955)                                                      
         DC    AL4(166875)                                                      
         DC    AL4(147100)                                                      
         DC    AL4(114325)                                                      
         DC    AL4(68220)                                                       
         DC    AL4(59745)                                                       
         DC    AL4(42655)                                                       
*                                                                               
DBNTAB   DC    AL2(6,44,0,0,0,0)   CLASS B INCL NY                              
         DC    AL4(341710)                                                      
         DC    AL4(232525)                                                      
         DC    AL4(253715)                                                      
         DC    AL4(223660)                                                      
         DC    AL4(174065)                                                      
         DC    AL4(103930)                                                      
         DC    AL4(90960)                                                       
         DC    AL4(64880)                                                       
*                                                                               
DBXTAB   DC    AL2(7,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(294830)                                                      
         DC    AL4(212605)                                                      
         DC    AL4(253715)                                                      
         DC    AL4(223660)                                                      
         DC    AL4(174065)                                                      
         DC    AL4(103930)                                                      
         DC    AL4(90960)                                                       
         DC    AL4(64880)                                                       
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
*                                                                               
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(166240)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(128620)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(137605)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(122895)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(100950)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(83645)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(72795)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(59555)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
*                                                                               
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(10184)                                                       
         DC    AL4(7735)                                                        
         DC    AL4(7445)                                                        
         DC    AL4(6515)                                                        
         DC    AL4(5321)                                                        
         DC    AL4(4919)                                                        
         DC    AL4(4434)                                                        
         DC    AL4(3655)                                                        
*                                                                               
         DC    AL2(8,44,19,255,0,0)  19+ (LOOK AT CLA, 14- EA)                  
         DC    AL4(5165)                                                        
         DC    AL4(3840)                                                        
         DC    AL4(3210)                                                        
         DC    AL4(2730)                                                        
         DC    AL4(2215)                                                        
         DC    AL4(2325)                                                        
         DC    AL4(2185)                                                        
         DC    AL4(1810)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(59220)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(44530)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(43355)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(38385)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(31740)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(25115)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(21795)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(17775)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(2027)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(1580)                                                        
         DC    AL4(1363)                                                        
         DC    AL4(1113)                                                        
         DC    AL4(560)                                                         
         DC    AL4(442)                                                         
         DC    AL4(368)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(815)                                                         
         DC    AL4(690)                                                         
         DC    AL4(572)                                                         
         DC    AL4(236)                                                         
         DC    AL4(162)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(590)                                                         
         DC    AL4(461)                                                         
         DC    AL4(386)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
*                                                                               
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(292)                                                         
         DC    AL4(236)                                                         
         DC    AL4(206)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(116380)                                                      
         DC    AL4(82220)                                                       
         DC    AL4(74530)                                                       
         DC    AL4(66205)                                                       
         DC    AL4(54245)                                                       
         DC    AL4(29900)                                                       
         DC    AL4(24775)                                                       
         DC    AL4(20285)                                                       
*                                                                               
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(815)                                                         
         DC    AL4(690)                                                         
         DC    AL4(572)                                                         
         DC    AL4(236)                                                         
         DC    AL4(162)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(590)                                                         
         DC    AL4(461)                                                         
         DC    AL4(386)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
*                                                                               
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(292)                                                         
         DC    AL4(236)                                                         
         DC    AL4(206)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(101445)                                                      
         DC    AL4(71545)                                                       
         DC    AL4(74530)                                                       
         DC    AL4(66205)                                                       
         DC    AL4(54245)                                                       
         DC    AL4(29900)                                                       
         DC    AL4(24775)                                                       
         DC    AL4(20285)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(815)                                                         
         DC    AL4(690)                                                         
         DC    AL4(572)                                                         
         DC    AL4(236)                                                         
         DC    AL4(162)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(590)                                                         
         DC    AL4(461)                                                         
         DC    AL4(386)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
*                                                                               
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(292)                                                         
         DC    AL4(236)                                                         
         DC    AL4(206)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(160160)                                                      
         DC    AL4(107840)                                                      
         DC    AL4(114675)                                                      
         DC    AL4(94815)                                                       
         DC    AL4(77520)                                                       
         DC    AL4(39510)                                                       
         DC    AL4(31825)                                                       
         DC    AL4(26055)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(752)                                                         
         DC    AL4(590)                                                         
         DC    AL4(292)                                                         
         DC    AL4(236)                                                         
         DC    AL4(206)                                                         
         DC    AL4(143)                                                         
         DC    AL4(81)                                                          
         DC    AL4(81)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(193185)                                                      
         DC    AL4(137210)                                                      
         DC    AL4(144670)                                                      
         DC    AL4(123810)                                                      
         DC    AL4(101200)                                                      
         DC    AL4(47635)                                                       
         DC    AL4(38415)                                                       
         DC    AL4(31390)                                                       
*                                                                               
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(771)                                                         
         DC    AL4(604)                                                         
         DC    AL4(299)                                                         
         DC    AL4(242)                                                         
         DC    AL4(211)                                                         
         DC    AL4(148)                                                         
         DC    AL4(88)                                                          
         DC    AL4(88)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(26285)          ANN ALONE                                    
         DC    AL4(26285)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(19365)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(17135)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(15200)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(8840)           SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(387)                                                         
         DC    AL4(387)                                                         
         DC    AL4(201)                                                         
         DC    AL4(172)                                                         
         DC    AL4(152)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(172)                                                         
         DC    AL4(132)                                                         
         DC    AL4(132)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(97)                                                          
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(39355)                                                       
         DC    AL4(39355)                                                       
         DC    AL4(21400)                                                       
         DC    AL4(19000)                                                       
         DC    AL4(16865)                                                       
         DC    AL4(8840)                                                        
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(172)                                                         
         DC    AL4(144)                                                         
         DC    AL4(138)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(97)                                                          
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(35695)                                                       
         DC    AL4(35695)                                                       
         DC    AL4(21400)                                                       
         DC    AL4(19000)                                                       
         DC    AL4(16865)                                                       
         DC    AL4(8840)                                                        
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(172)                                                         
         DC    AL4(144)                                                         
         DC    AL4(138)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(97)                                                          
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(48000)                                                       
         DC    AL4(48000)                                                       
         DC    AL4(25560)                                                       
         DC    AL4(19610)                                                       
         DC    AL4(17450)                                                       
         DC    AL4(8840)                                                        
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(144)                                                         
         DC    AL4(144)                                                         
         DC    AL4(138)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(97)                                                          
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(60650)                                                       
         DC    AL4(60650)                                                       
         DC    AL4(28475)                                                       
         DC    AL4(22035)                                                       
         DC    AL4(19610)                                                       
         DC    AL4(8840)                                                        
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(144)                                                         
         DC    AL4(144)                                                         
         DC    AL4(138)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(291)                                                         
         DC    AL4(291)                                                         
         DC    AL4(97)                                                          
         DC    AL4(84)                                                          
         DC    AL4(84)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(26285) ANN ALONE                                    
         DC    AL1(100),AL3(26285) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(19365) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(17135) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(15200) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL1(80),AL3(387)                                                 
         DC    AL1(80),AL3(387)                                                 
         DC    AL1(95),AL3(201)                                                 
         DC    AL1(95),AL3(172)                                                 
         DC    AL1(95),AL3(152)                                                 
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(172)                                                 
         DC    AL1(95),AL3(132)                                                 
         DC    AL1(95),AL3(132)                                                 
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(97)                                                  
         DC    AL1(95),AL3(84)                                                  
         DC    AL1(95),AL3(84)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL1(80),AL3(39355)                                               
         DC    AL1(80),AL3(39355)                                               
         DC    AL1(95),AL3(21400)                                               
         DC    AL1(95),AL3(19000)                                               
         DC    AL1(95),AL3(16865)                                               
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(172)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(138)                                                 
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(97)                                                  
         DC    AL1(95),AL3(84)                                                  
         DC    AL1(95),AL3(84)                                                  
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL1(80),AL3(35695)                                               
         DC    AL1(80),AL3(35695)                                               
         DC    AL1(95),AL3(21400)                                               
         DC    AL1(95),AL3(19000)                                               
         DC    AL1(95),AL3(16865)                                               
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(172)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(138)                                                 
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(97)                                                  
         DC    AL1(95),AL3(84)                                                  
         DC    AL1(95),AL3(84)                                                  
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL1(80),AL3(48000)                                               
         DC    AL1(80),AL3(48000)                                               
         DC    AL1(95),AL3(25560)                                               
         DC    AL1(95),AL3(19610)                                               
         DC    AL1(95),AL3(17450)                                               
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(138)                                                 
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(97)                                                  
         DC    AL1(95),AL3(84)                                                  
         DC    AL1(95),AL3(84)                                                  
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL1(80),AL3(60650)                                               
         DC    AL1(80),AL3(60650)                                               
         DC    AL1(95),AL3(28475)                                               
         DC    AL1(95),AL3(22035)                                               
         DC    AL1(95),AL3(19610)                                               
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(144)                                                 
         DC    AL1(95),AL3(138)                                                 
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(80),AL3(291)                                                 
         DC    AL1(95),AL3(97)                                                  
         DC    AL1(95),AL3(84)                                                  
         DC    AL1(95),AL3(84)                                                  
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
*                                                                               
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(71080)          AR,AS,P,ANN                                  
         DC    AL4(56385)          S,1-4MS,1-4SS                                
         DC    AL4(36760)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(29415)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(18385)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(18595)          SE                                           
*                                                                               
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(44475)          ANN ALONE                                    
         DC    AL4(44475)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(33375)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(33375)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(33375)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(9930)           SE                                           
*                                                                               
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(72155)                                                       
         DC    AL4(72155)                                                       
         DC    AL4(55490)                                                       
         DC    AL4(49620)                                                       
         DC    AL4(45330)                                                       
         DC    AL4(9930)                                                        
*                                                                               
N08TAB   DC    AL2(28,36,0,0,0,0)  NETWORK 8 WEEK                               
         DC    AL4(114940)                                                      
         DC    AL4(114940)                                                      
         DC    AL4(88435)                                                       
         DC    AL4(79000)                                                       
         DC    AL4(70800)                                                       
         DC    AL4(9930)                                                        
*                                                                               
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(142630)                                                      
         DC    AL4(142630)                                                      
         DC    AL4(109705)                                                      
         DC    AL4(98090)                                                       
         DC    AL4(89865)                                                       
         DC    AL4(9930)                                                        
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
*                                                                               
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(149355)                                                      
         DC    AL4(149355)                                                      
         DC    AL4(114855)                                                      
         DC    AL4(102710)                                                      
         DC    AL4(94100)                                                       
         DC    AL4(9930)                                                        
*                                                                               
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(71320)                                                       
         DC    AL4(71320)                                                       
         DC    AL4(54840)                                                       
         DC    AL4(49040)                                                       
         DC    AL4(44810)                                                       
         DC    AL4(9930)                                                        
*                                                                               
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(107405)                                                      
         DC    AL4(107405)                                                      
         DC    AL4(75205)                                                       
         DC    AL4(67130)                                                       
         DC    AL4(60985)                                                       
         DC    AL4(9930)                                                        
*                                                                               
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(86070)                                                       
         DC    AL4(86070)                                                       
         DC    AL4(40345)                                                       
         DC    AL4(40345)                                                       
         DC    AL4(40345)                                                       
         DC    AL4(9930)                                                        
*                                                                               
         DC    AL2(34,36,0,0,0,0)  REGIONAL - WITH ANY MAJORS                   
         DC    AL4(86070)                                                       
         DC    AL4(86070)                                                       
         DC    AL4(86070)                                                       
         DC    AL4(77465)                                                       
         DC    AL4(69670)                                                       
         DC    AL4(9930)                                                        
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
         DC    AL2(419,24,0,0,0,0)  10 EUROPE OR OUTSIDE EUROPE-12M             
         DC    AL4(7500)           CAST=1                                       
         DC    AL4(7500)                2-4                                     
         DC    AL4(7500)                5+                                      
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
         EJECT                                                                  
*              HOLDING AND SESSION FEES                                         
*                                                                               
         DC    AL2(61,44,1,255,0,0)  AFT RADIO BASE SESSION RATES               
         DC    AL4(26285)          ANN ALONE                                    
         DC    AL4(26285)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(19365)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(17135)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(15200)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(20210)          SE                                           
         DC    AL4(9025)           C3,C6                                        
         DC    AL4(14435)          C9                                           
*                                                                               
         DC    AL2(62,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(59220)              PRINCIPAL ON  CAMERA                     
         DC    AL4(44530)                  "     OFF   "                        
         DC    AL4(43355)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(38385)                "    6-8    "                          
         DC    AL4(31740)                "     9+    "                          
         DC    AL4(25115)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(21795)                "    6-8    "                          
         DC    AL4(17775)                "     9+    "                          
         DC    AL4(32300)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(49290)              HAND MODEL UNLIMITED                     
         DC    AL4(18750)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(32785)              HAND MODEL 13 WEEKS                      
         DC    AL4(91215)    PIL       PILOT LOCATION RATE                      
         DC    AL4(70135)    PI        PILOT STUDIO RATE                        
         DC    AL4(38745)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(9495)     C3,C6     CONTRACTORS                              
         DC    AL4(18725)    C9             "                                   
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(59220)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(44530)                                               
         DC    AL1(50),AL3(43355)                                               
         DC    AL1(50),AL3(38385)                                               
         DC    AL1(50),AL3(31740)                                               
         DC    AL1(50),AL3(25115)                                               
         DC    AL1(50),AL3(21795)                                               
         DC    AL1(50),AL3(17775)                                               
         DC    AL1(50),AL3(32300)                                               
         DC    AL1(50),AL3(49290)                                               
         DC    AL1(50),AL3(18750)                                               
         DC    AL1(50),AL3(32785)                                               
         DC    AL1(50),AL3(91215)          PIL                                  
         DC    AL1(50),AL3(70135)          PI                                   
         DC    AL1(50),AL3(38745)          SE                                   
         DC    AL1(50),AL3(9495)           C3,C6                                
         DC    AL1(50),AL3(18725)          C9                                   
*                                                                               
         DC    AL2(64,80,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(59220)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(43355)                                                       
         DC    AL4(38385)                                                       
         DC    AL4(31740)                                                       
         DC    AL4(25115)                                                       
         DC    AL4(21795)                                                       
         DC    AL4(17775)                                                       
         DC    AL4(32300)                                                       
         DC    AL4(49290)                                                       
         DC    AL4(18750)                                                       
         DC    AL4(32785)                                                       
         DC    AL4(91215)          PIL                                          
         DC    AL4(70135)          PI                                           
         DC    AL4(38745)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
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
         DC    AL4(59220)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(43355)                                                       
         DC    AL4(38385)                                                       
         DC    AL4(31740)                                                       
*                                                                               
         DC    AL2(37,80,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(59220)                                               
         DC    AL1(50),AL3(44530)                                               
         DC    AL1(50),AL3(43355)                                               
         DC    AL1(50),AL3(38385)                                               
         DC    AL1(50),AL3(31740)                                               
         DC    AL1(50),AL3(25115)                                               
         DC    AL1(50),AL3(21795)                                               
         DC    AL1(50),AL3(17775)                                               
         DC    AL1(50),AL3(32300)                                               
         DC    AL1(50),AL3(49290)                                               
         DC    AL1(50),AL3(18750)                                               
         DC    AL1(50),AL3(32785)                                               
         DC    AL1(50),AL3(91215)  PIL                                          
         DC    AL1(50),AL3(70135)  PI                                           
         DC    AL1(50),AL3(38745)  SE                                           
         DC    AL1(50),AL3(9495)   C3,C6                                        
         DC    AL1(50),AL3(18725)  C9                                           
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(59220)                                              
         DC    AL1(200),AL3(44530)                                              
         DC    AL1(200),AL3(43355)                                              
         DC    AL1(200),AL3(38385)                                              
         DC    AL1(200),AL3(31740)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(18750)                                              
         DC    AL1(200),AL3(32785)                                              
*                                                                               
         DC    AL2(69,80,1,255,0,0)  MOVE TO INTERNET, 8 WEEK (1.33X)           
         DC    AL4(78765)              PRINCIPAL ON  CAMERA                     
         DC    AL4(59225)                  "     OFF   "                        
         DC    AL4(57660)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(51050)                "    6-8    "                          
         DC    AL4(42215)                "     9+    "                          
         DC    AL4(33405)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(28985)                "    6-8    "                          
         DC    AL4(23640)                "     9+    "                          
         DC    AL4(42960)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(65555)              HAND MODEL UNLIMITED                     
         DC    AL4(24940)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(43605)              HAND MODEL 13 WEEKS                      
         DC    AL4(121315)   PIL       PILOT LOCATION RATE                      
         DC    AL4(93280)    PI        PILOT STUDIO RATE                        
         DC    AL4(51080)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(12630)    C3,C6     CONTRACTORS                              
         DC    AL4(24905)    C9             "                                   
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV  (3X SESSION)                
         DC    AL4(207270)             PRINCIPAL ON  CAMERA                     
         DC    AL4(155855)                 "     OFF   "                        
         DC    AL4(151745)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(134350)               "    6-8    "                          
         DC    AL4(111090)               "     9+    "                          
         DC    AL4(87905)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(76285)                "    6-8    "                          
         DC    AL4(62215)                "     9+    "                          
         DC    AL4(113050)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(172515)             HAND MODEL UNLIMITED                     
         DC    AL4(65625)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(114750)             HAND MODEL 13 WEEKS                      
         DC    AL4(319255)   PIL       PILOT LOCATION RATE                      
         DC    AL4(245473)   PI        PILOT STUDIO RATE                        
         DC    AL4(135610)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(33235)    C3,C6     CONTRACTORS                              
         DC    AL4(65540)    C9             "                                   
*                                                                               
         DC    AL2(78,44,1,255,0,0)  INTERNET RADIO (1.33X SESSION)             
         DC    AL4(34960)          ANN ALONE                                    
         DC    AL4(34960)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(25755)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(22790)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(20215)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(26880)          SE                                           
         DC    AL4(12005)          C3,C6                                        
         DC    AL4(19200)          C9                                           
*                                                                               
         DC    AL2(77,44,1,255,0,0)    INTERNET RADIO (3.5X SESSION)            
         DC    AL4(92000)          ANN ALONE                                    
         DC    AL4(92000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(67780)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(59975)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(53200)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(70735)          SE                                           
         DC    AL4(31590)          C3,C6                                        
         DC    AL4(50525)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    INTERNET TV (3X SESSION)                 
         DC    AL4(207270)             PRINCIPAL ON  CAMERA                     
         DC    AL4(155855)                 "     OFF   "                        
         DC    AL4(151745)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(134350)               "    6-8    "                          
         DC    AL4(111090)               "     9+    "                          
         DC    AL4(87905)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(76285)                "    6-8    "                          
         DC    AL4(62215)                "     9+    "                          
         DC    AL4(113050)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(172515)             HAND MODEL UNLIMITED                     
         DC    AL4(65625)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(114750)             HAND MODEL 13 WEEKS                      
         DC    AL4(319255)   PIL       PILOT LOCATION RATE                      
         DC    AL4(245473)   PI        PILOT STUDIO RATE                        
         DC    AL4(135610)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(33235)    C3,C6     CONTRACTORS                              
         DC    AL4(65540)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    INTERNET RADIO                           
         DC    AL4(92000)          ANN ALONE                                    
         DC    AL4(92000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(67780)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(59975)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(53200)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(70735)          SE                                           
         DC    AL4(31590)          C3,C6                                        
         DC    AL4(50525)          C9                                           
*                                                                               
         DC    AL2(540,80,1,255,0,0)  MOVE TO INTERNET, 4 WEEK (1.25X)          
         DC    AL4(74025)              PRINCIPAL ON  CAMERA                     
         DC    AL4(55663)                  "     OFF   "                        
         DC    AL4(54194)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(47981)                "    6-8    "                          
         DC    AL4(39675)                "     9+    "                          
         DC    AL4(31394)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(27244)                "    6-8    "                          
         DC    AL4(22219)                "     9+    "                          
         DC    AL4(40375)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(61613)              HAND MODEL UNLIMITED                     
         DC    AL4(23438)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(40981)              HAND MODEL 13 WEEKS                      
         DC    AL4(114019)   PIL       PILOT LOCATION RATE                      
         DC    AL4(87669)    PI        PILOT STUDIO RATE                        
         DC    AL4(48431)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(11869)    C3,C6     CONTRACTORS                              
         DC    AL4(23406)    C9             "                                   
*                                                                               
         DC    AL2(541,44,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(32856)          ANN ALONE                                    
         DC    AL4(32856)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(24206)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(21419)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(19000)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(25263)          SE                                           
         DC    AL4(11281)          C3,C6                                        
         DC    AL4(18044)          C9                                           
*                                                                               
         DC    AL2(542,80,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(32856)          ANN ALONE                                    
         DC    AL4(32856)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(24206)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(21419)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(19000)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(25263)          SE                                           
         DC    AL4(11281)          C3,C6                                        
         DC    AL4(18044)          C9                                           
*                                                                               
         EJECT                                                                  
*              CABLE RATES (YEAR 2009)                                          
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(59220)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(43355)                                                       
         DC    AL4(38385)                                                       
         DC    AL4(31740)                                                       
         DC    AL4(25115)                                                       
         DC    AL4(21795)                                                       
         DC    AL4(17775)                                                       
*                                                                               
         DC    AL2(41,12,2,60)     MINIMUM COVERS UPTO 60                       
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,61,61)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(34)                                                          
         DC    AL4(0)                                                           
         DC    AL4(97)                                                          
         DC    AL4(0)                                                           
         DC    AL4(20)                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(24)                                                          
*                                                                               
         DC    AL2(41,44,62,62)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(864)                                                         
         DC    AL4(0)                                                           
         DC    AL4(632)                                                         
         DC    AL4(547)                                                         
         DC    AL4(460)                                                         
         DC    AL4(353)                                                         
         DC    AL4(221)                                                         
         DC    AL4(259)                                                         
*                                                                               
         DC    AL2(41,44,63,70)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(864)                                                         
         DC    AL4(0)                                                           
         DC    AL4(632)                                                         
         DC    AL4(561)                                                         
         DC    AL4(460)                                                         
         DC    AL4(364)                                                         
         DC    AL4(318)                                                         
         DC    AL4(259)                                                         
*                                                                               
         DC    AL2(41,44,71,71)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(864)                                                         
         DC    AL4(566)                                                         
         DC    AL4(632)                                                         
         DC    AL4(561)                                                         
         DC    AL4(460)                                                         
         DC    AL4(364)                                                         
         DC    AL4(318)                                                         
         DC    AL4(259)                                                         
*                                                                               
         DC    AL2(41,44,72,100)                                                
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(864)                                                         
         DC    AL4(576)                                                         
         DC    AL4(632)                                                         
         DC    AL4(561)                                                         
         DC    AL4(460)                                                         
         DC    AL4(364)                                                         
         DC    AL4(318)                                                         
         DC    AL4(259)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(734)                                                         
         DC    AL4(488)                                                         
         DC    AL4(538)                                                         
         DC    AL4(474)                                                         
         DC    AL4(394)                                                         
         DC    AL4(312)                                                         
         DC    AL4(270)                                                         
         DC    AL4(220)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(603)                                                         
         DC    AL4(402)                                                         
         DC    AL4(443)                                                         
         DC    AL4(392)                                                         
         DC    AL4(325)                                                         
         DC    AL4(258)                                                         
         DC    AL4(222)                                                         
         DC    AL4(181)                                                         
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
         DC    AL4(44420)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(22265)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(32520)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(28785)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(23805)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(11250)           'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(11250)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(11250)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(18750)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(32785)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(17210)          'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(42,84,5,255,0,0)  DEM (TV)                                   
         DC    AL4(44420)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(22265)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(32520)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(28785)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(23805)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(2815)            'OFF' 1-4M3,1-4S3,D3,S3                     
         DC    AL4(2815)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(2815)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(18750)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(32785)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(4305)           'OFF' SOLO/DUO                               
*                                                                               
         DC    AL2(43,48,1,4,0,0)  DEM (AFT RADIO)                              
         DC    AL4(18115)          ANN ALONE                                    
         DC    AL4(18115)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(11950)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11950)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11950)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(20210)          SE                                           
         DC    AL4(9025)           C3,C6                                        
         DC    AL4(14435)          C9                                           
         DC    AL4(18280)          SOLOS AND DUOS                               
*                                                                               
         DC    AL2(43,48,5,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(18115)          ANN ALONE                                    
         DC    AL4(18115)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(2985)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(2985)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(2985)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(20210)          SE                                           
         DC    AL4(9025)           C3,C6                                        
         DC    AL4(14435)          C9                                           
         DC    AL4(4570)           SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(28565)                                                       
         DC    AL4(28565)                                                       
         DC    AL4(28565)                                                       
         DC    AL4(28565)                                                       
         DC    AL4(28565)                                                       
         DC    AL4(11245)                                                       
         EJECT                                                                  
*              FOREIGN REUSE                                                    
*                                                                               
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(177660)         (3 X 59220)                                  
         DC    AL4(133590)                                                      
         DC    AL4(130065)                                                      
         DC    AL4(115155)                                                      
         DC    AL4(95220)                                                       
         DC    AL4(75345)                                                       
         DC    AL4(65385)                                                       
         DC    AL4(53325)                                                       
         DC    AL4(96900)                                                       
         DC    AL4(147870)                                                      
         DC    AL4(56250)                                                       
         DC    AL4(98355)                                                       
         DC    AL4(273645)         PIL                                          
         DC    AL4(210405)         PI                                           
         DC    AL4(115215)         SE                                           
         DC    AL4(28485)          C3,C6                                        
         DC    AL4(56175)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(118440)         (2 X 59220)                                  
         DC    AL4(89060)                                                       
         DC    AL4(86710)                                                       
         DC    AL4(76770)                                                       
         DC    AL4(63480)                                                       
         DC    AL4(50230)                                                       
         DC    AL4(43590)                                                       
         DC    AL4(35550)                                                       
         DC    AL4(64600)                                                       
         DC    AL4(98580)                                                       
         DC    AL4(37500)                                                       
         DC    AL4(65570)                                                       
         DC    AL4(182430)          PIL                                         
         DC    AL4(140270)          PI                                          
         DC    AL4(76810)           SE                                          
         DC    AL4(18990)           C3,C6                                       
         DC    AL4(37450)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 8X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(473760)        (8 X 56710)                                   
         DC    AL4(356240)                                                      
         DC    AL4(346840)                                                      
         DC    AL4(307080)                                                      
         DC    AL4(253920)                                                      
         DC    AL4(200920)                                                      
         DC    AL4(174360)                                                      
         DC    AL4(142200)                                                      
         DC    AL4(258400)                                                      
         DC    AL4(394320)                                                      
         DC    AL4(150000)                                                      
         DC    AL4(262280)                                                      
         DC    AL4(729720)         PIL                                          
         DC    AL4(561080)         PI                                           
         DC    AL4(307240)         SE                                           
         DC    AL4(75960)          C3,C6                                        
         DC    AL4(149800)         C9                                           
*                                                                               
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(52145)          N/D                                          
         DC    AL4(52145)          P,ANN,S,D,ACR                                
         DC    AL4(30245)          3-5 GROUP                                    
         DC    AL4(20860)          6-8 GROUP                                    
         DC    AL4(16680)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(59465)          P,ANN,ACR                                    
         DC    AL4(61750)          S,D                                          
         DC    AL4(40260)          3-5 GROUP                                    
         DC    AL4(32205)          6-8 GROUP                                    
         DC    AL4(20140)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
*                                                                               
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK                                      
         DC    AL4(213860)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(160825)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(156560)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(138580)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(114620)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(90855)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(78705)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(64165)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(540)                                                         
         DC    AL4(392)                                                         
         DC    AL4(383)                                                         
         DC    AL4(354)                                                         
         DC    AL4(275)                                                         
         DC    AL4(227)                                                         
         DC    AL4(205)                                                         
         DC    AL4(146)                                                         
         EJECT                                                                  
*              SPANISH FOREIGN REUSE                                            
*                                                                               
SFRTBL   DC    AL2(410,80,0,0,0,0)  SFRA,SFRC - 4X SESSION RATE                 
         DC    AL4(236880)         (4 X 59220)                                  
         DC    AL4(178120)                                                      
         DC    AL4(173420)                                                      
         DC    AL4(153540)                                                      
         DC    AL4(126960)                                                      
         DC    AL4(100460)                                                      
         DC    AL4(87180)                                                       
         DC    AL4(71100)                                                       
         DC    AL4(129200)                                                      
         DC    AL4(197160)                                                      
         DC    AL4(75000)                                                       
         DC    AL4(131140)                                                      
         DC    AL4(364860)         PIL                                          
         DC    AL4(280540)         PI                                           
         DC    AL4(154980)         SE                                           
         DC    AL4(37980)          C3,C6                                        
         DC    AL4(74900)          C9                                           
*                                                                               
         DC    AL2(411,80,0,0,0,0)  SFRB - 3X SESSION RATE                      
         DC    AL4(177660)         (3 X 59220)                                  
         DC    AL4(133590)                                                      
         DC    AL4(130065)                                                      
         DC    AL4(115155)                                                      
         DC    AL4(95220)                                                       
         DC    AL4(75345)                                                       
         DC    AL4(65385)                                                       
         DC    AL4(53325)                                                       
         DC    AL4(96900)                                                       
         DC    AL4(147870)                                                      
         DC    AL4(56250)                                                       
         DC    AL4(98355)                                                       
         DC    AL4(273645)         PIL                                          
         DC    AL4(210405)         PI                                           
         DC    AL4(116235)         SE                                           
         DC    AL4(28485)          C3,C6                                        
         DC    AL4(56175)          C9                                           
         EJECT                                                                  
*                                                                               
*              ADDENDUM USES                                                    
*                                                                               
ADTTBL   DC    AL2(65,88,0,0,0,0)  TV SESSION RATES - 3 DAY - GA                
         DC    AL4(36100)          ON CAMERA                                    
         DC    AL4(27100)          OFF                                          
         DC    AL4(19700)          ON CAMERA GROUPS 3-5                         
         DC    AL4(17500)                           6-8                         
         DC    AL4(14400)                           9+                          
         DC    AL4(11400)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(9900)                             6-8                        
         DC    AL4(8100)                             9+                         
         DC    AL4(25800)          EXTRA UNLIMITED                              
         DC    AL4(39400)          HAND MODEL UNLIMITED                         
         DC    AL4(15000)          EXTRA INITIAL 13WK                           
         DC    AL4(26200)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(38500)          SOLO/DUO ON CAM                              
         DC    AL4(28900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(70,88,0,0,0,0)  1 WEEK - GA                                  
         DC    AL4(38600)          ON CAMERA                                    
         DC    AL4(29100)          OFF                                          
         DC    AL4(21100)          ON CAMERA GROUPS 3-5                         
         DC    AL4(18700)                           6-8                         
         DC    AL4(15500)                           9+                          
         DC    AL4(12200)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(10600)                            6-8                        
         DC    AL4(8700)                             9+                         
         DC    AL4(25800)          EXTRA UNLIMITED                              
         DC    AL4(39400)          HAND MODEL UNLIMITED                         
         DC    AL4(15000)          EXTRA INITIAL 13WK                           
         DC    AL4(26200)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(38500)          SOLO/DUO ON CAM                              
         DC    AL4(28900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(71,80,0,0,0,0)  TV SESSION RATES - 1 WEEK - KS               
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
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0,0,0)  TV SESSION - 2 WEEK - NW MULTI MKT           
         DC    AL4(39000)          ON CAMERA                                    
         DC    AL4(29100)          OFF                                          
         DC    AL4(28400)                                                       
         DC    AL4(28400)                                                       
         DC    AL4(28400)                                                       
         DC    AL4(16400)                                                       
         DC    AL4(16400)                                                       
         DC    AL4(16400)                                                       
         DC    AL4(24900)                                                       
         DC    AL4(24900)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(14300)                                                       
         DC    AL4(39000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(73,80,0,0)      TV SESSION - 2 WEEK - NW SINGLE MKT          
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(35300)          ON CAMERA                                    
         DC    AL4(24900)          OFF                                          
         DC    AL4(21600)          ON CAMERA GROUPS 3-5                         
         DC    AL4(21600)                           6-8                         
         DC    AL4(21600)                           9+                          
         DC    AL4(14900)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(14900)                            6-8                        
         DC    AL4(14900)                            9+                         
         DC    AL4(22400)          EXTRA UNLIMITED                              
         DC    AL4(22400)          HAND MODEL UNLIMITED                         
         DC    AL4(13700)          EXTRA INITIAL 13WK                           
         DC    AL4(13700)          HAND MODEL INITIAL 13WK                      
         DC    AL4(35300)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(80,88,0,0,0,0)  4 WEEK - GA                                  
         DC    AL4(41200)          ON CAMERA                                    
         DC    AL4(31000)          OFF                                          
         DC    AL4(22600)          ON CAMERA GROUPS 3-5                         
         DC    AL4(20000)                           6-8                         
         DC    AL4(16500)                           9+                          
         DC    AL4(13100)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(11300)                            6-8                        
         DC    AL4(9200)                             9+                         
         DC    AL4(25800)          EXTRA UNLIMITED                              
         DC    AL4(39400)          HAND MODEL UNLIMITED                         
         DC    AL4(15000)          EXTRA INITIAL 13WK                           
         DC    AL4(26200)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(38500)          SOLO/DUO ON CAM                              
         DC    AL4(28900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(81,80,0,0,0,0)  31 DAY - KS                                  
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
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(90,88,0,0,0,0)  13 WEEK - GA                                 
         DC    AL4(51500)          ON CAMERA                                    
         DC    AL4(38700)          OFF                                          
         DC    AL4(28200)          ON CAMERA GROUPS 3-5                         
         DC    AL4(25000)                           6-8                         
         DC    AL4(20600)                           9+                          
         DC    AL4(16300)          OFF CAMERA GROUPS 3-5                        
         DC    AL4(14200)                            6-8                        
         DC    AL4(11600)                            9+                         
         DC    AL4(25800)          EXTRA UNLIMITED                              
         DC    AL4(39400)          HAND MODEL UNLIMITED                         
         DC    AL4(15000)          EXTRA INITIAL 13WK                           
         DC    AL4(26200)          HAND MODEL INITIAL 13WK                      
         DC    AL4(36100)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
         DC    AL4(38500)          SOLO/DUO ON CAM                              
         DC    AL4(28900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(91,80,0,0,0,0)  13 WEEKS - KS                                
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
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(92,80,0,0,0,0)  13 WEEKS - TX                                
         DC    AL4(47375)          ON CAMERA                                    
         DC    AL4(35625)          OFF                                          
         DC    AL4(34680)          ON CAMERA GROUPS 3-5                         
         DC    AL4(34680)                           6-8                         
         DC    AL4(34680)                           9+                          
         DC    AL4(20090)          OFF CAMERA GROUS 3-5                         
         DC    AL4(20090)                           6-8                         
         DC    AL4(20090)                           9+                          
         DC    AL4(25840)          EXTRA UNLIMITED                              
         DC    AL4(39430)          HAND MODEL UNLIMITED                         
         DC    AL4(15000)          EXTRA INITIAL 13WK                           
         DC    AL4(26230)          HAND MODEL INITIAL 13WK                      
         DC    AL4(47375)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0,0,0)  13 WEEKS - NW MULTI MARKET                   
         DC    AL4(45800)          ON CAMERA                                    
         DC    AL4(34300)          OFF                                          
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
         DC    AL4(19300)                                                       
         DC    AL4(19300)                                                       
         DC    AL4(19300)                                                       
         DC    AL4(24900)                                                       
         DC    AL4(24900)                                                       
         DC    AL4(17000)                                                       
         DC    AL4(17000)                                                       
         DC    AL4(45800)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(93,80,0,0)      13 WEEKS - NW SINGLE MARKET                  
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(41600)          ON CAMERA                                    
         DC    AL4(29300)          OFF                                          
         DC    AL4(25400)                                                       
         DC    AL4(25400)                                                       
         DC    AL4(25400)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(22400)                                                       
         DC    AL4(16300)                                                       
         DC    AL4(16300)                                                       
         DC    AL4(41600)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(36775)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
ADOTBL   DC    AL2(100,48,0,0,0,0)  RADIO SESSION RATES - 3 DAY - GA            
         DC    AL4(16000)                                                       
         DC    AL4(16000)                                                       
         DC    AL4(8800)                                                        
         DC    AL4(7800)                                                        
         DC    AL4(6900)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(11340)          SOLO/DUO                                     
*                                                                               
         DC    AL2(105,48,0,0,0,0)  1 WEEK - GA                                 
         DC    AL4(17200)                                                       
         DC    AL4(17200)                                                       
         DC    AL4(9400)                                                        
         DC    AL4(8300)                                                        
         DC    AL4(7400)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(12150)                                                       
*                                                                               
         DC    AL2(106,44,0,0,0,0)  1 WEEK - KS                                 
         DC    AL4(12350)                                                       
         DC    AL4(12350)                                                       
         DC    AL4(7735)                                                        
         DC    AL4(6485)                                                        
         DC    AL4(5740)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
*                                  RADIO                                        
         DC    AL2(108,44,0,0,0,0)  2 WEEK - NW MULTLIPLE MARKETS               
         DC    AL4(17600)                                                       
         DC    AL4(17600)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
*                                  RADIO                                        
         DC    AL2(108,44,0,0)      2 WEEK - NW SINGLE MARKET                   
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(11400)                                                       
         DC    AL4(11400)                                                       
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(115,48,0,0,0,0)  4 WEEK - GA                                 
         DC    AL4(18300)                                                       
         DC    AL4(18300)                                                       
         DC    AL4(10100)                                                       
         DC    AL4(8900)                                                        
         DC    AL4(7900)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(12960)                                                       
*                                                                               
         DC    AL2(116,44,0,0,0,0)  31 DAY - KS                                 
         DC    AL4(15845)                                                       
         DC    AL4(15845)                                                       
         DC    AL4(9360)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(7360)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(125,48,0,0,0,0)  13 WEEK - GA                                
         DC    AL4(22900)                                                       
         DC    AL4(22900)                                                       
         DC    AL4(12600)                                                       
         DC    AL4(11100)                                                       
         DC    AL4(9900)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(16200)                                                       
*                                                                               
         DC    AL2(126,44,0,0,0,0)  13 WEEK - KS                                
         DC    AL4(19340)                                                       
         DC    AL4(19340)                                                       
         DC    AL4(10855)                                                       
         DC    AL4(9610)                                                        
         DC    AL4(8605)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(127,44,0,0,0,0)  13 WEEK - TX                                
         DC    AL4(21030)                                                       
         DC    AL4(21030)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0,0,0)  13 WEEK - NW  MULTIPLE MARKETS              
         DC    AL4(20700)                                                       
         DC    AL4(20700)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(128,44,0,0)      13 WEEK - NW  SINGLE MARKET                 
         DC    AL1(1,0,0,0)                                                     
         DC    AL4(13400)                                                       
         DC    AL4(13400)                                                       
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
ADHTAB   DC    AL2(225,88,0,0,0,0)  ADDENDUM HOLDING RATES - GA                 
         DC    AL4(51500)                                                       
         DC    AL4(38700)                                                       
         DC    AL4(28200)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(20600)                                                       
         DC    12AL4(0)                                                         
         DC    AL4(38500)                                                       
         DC    AL4(28900)                                                       
*                                                                               
         DC    AL2(226,32,0,0,0,0)  ADDENDUM HOLDING RATES - KS                 
         DC    AL4(45400)           ON CAMERA                                   
         DC    AL4(34015)           OFF                                         
         DC    AL4(32830)                                                       
         DC    AL4(27970)                                                       
         DC    AL4(22405)                                                       
*                                                                               
         DC    AL2(227,32,0,0,0,0)  ADDENDUM HOLDING RATES - TX                 
         DC    AL4(47375)           ON CAMERA                                   
         DC    AL4(35625)           OFF                                         
         DC    AL4(34680)                                                       
         DC    AL4(34680)                                                       
         DC    AL4(34680)                                                       
*                                                                               
         DC    AL2(228,32,0,0,0,0)  ADDENDUM HOLDING RATES - NW                 
         DC    AL4(45800)           ON CAMERA   MULTIPLE MARKET                 
         DC    AL4(34300)           OFF                                         
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
*                                                                               
         DC    AL2(228,32,0,0)      ADDENDUM HOLDING RATES - NW                 
         DC    AL1(1,0,0,0)         SINGLE MARKET                               
         DC    AL4(41600)           ON CAMERA                                   
         DC    AL4(29300)           OFF                                         
         DC    AL4(25400)                                                       
         DC    AL4(25400)                                                       
         DC    AL4(25400)                                                       
*                                                                               
*                                   ADDENDUM REINSTSATEMENT-GA                  
ARNTAB   DC    AL2(230,88,0,0,0,0)  - 2X ADDENDUM HOLDING RATES                 
         DC    AL1(200),AL3(51500)                                              
         DC    AL1(200),AL3(38700)                                              
         DC    AL1(200),AL3(28200)                                              
         DC    AL1(200),AL3(25000)                                              
         DC    AL1(200),AL3(20600)                                              
         DC    12AL4(0)                                                         
         DC    AL1(200),AL3(38500)                                              
         DC    AL1(200),AL3(28900)                                              
*                                    ADDENDUM REINSTSTATEMENT - KS              
         DC    AL2(231,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(45400)             ON CAMERA                        
         DC    AL1(200),AL3(34015)             OFF                              
         DC    AL1(200),AL3(32830)                                              
         DC    AL1(200),AL3(27970)                                              
         DC    AL1(200),AL3(22405)                                              
*                                    ADDENDUM REINSTATEMENT - TX                
         DC    AL2(232,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(47375)             ON CAMERA                        
         DC    AL1(200),AL3(35625)             OFF                              
         DC    AL1(200),AL3(34680)                                              
         DC    AL1(200),AL3(34680)                                              
         DC    AL1(200),AL3(34680)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0,0,0)   - 2X ADDENDUM HOLDING RATES                
         DC    AL1(200),AL3(45800)             ON CAMERA  MULTIPLE MKTS         
         DC    AL1(200),AL3(34300)             OFF                              
         DC    AL1(200),AL3(33400)                                              
         DC    AL1(200),AL3(33400)                                              
         DC    AL1(200),AL3(33400)                                              
*                                    ADDENDUM REINSTATEMENT - NW                
         DC    AL2(233,32,0,0)       - 2X ADDENDUM HOLDING RATES                
         DC    AL1(1,0,0,0)          SINGLE MARKET                              
         DC    AL1(200),AL3(41600)             ON CAMERA                        
         DC    AL1(200),AL3(29300)             OFF                              
         DC    AL1(200),AL3(25400)                                              
         DC    AL1(200),AL3(25400)                                              
         DC    AL1(200),AL3(25400)                                              
*                                                                               
ADDTAB   DC    AL2(205,80,0,0,0,0)  ADDENDUM DEMO (TV) - GA                     
         DC    AL4(38600)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(19400)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(38600)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(38600)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(38600)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(19400)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(19400)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(19400)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    7AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(206,80,0,0,0,0)  ADDENDUM DEMO (TV) - KS                     
         DC    AL4(10190)          'ON'                                         
         DC    AL4(9005)           'OFF'                                        
         DC    AL4(10190)                                                       
         DC    AL4(10190)                                                       
         DC    AL4(10190)                                                       
         DC    AL4(9005)                                                        
         DC    AL4(9005)                                                        
         DC    AL4(9005)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(207,88,0,0,0,0)  ADDENDUM DEMO (TV) - TX                     
         DC    AL4(35535)          'ON'                                         
         DC    AL4(17810)          'OFF'                                        
         DC    AL4(26015)                                                       
         DC    AL4(23030)                                                       
         DC    AL4(19045)                                                       
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    AL4(9000)                                                        
         DC    7AL4(0)             N/D                                          
         DC    AL4(7595)           C3,C6                                        
         DC    AL4(14980)          C9                                           
         DC    AL4(35535)          SOLO/DUO ON CAM (SAME AS PRINCIPAL)          
         DC    AL4(13770)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(208,80,0,0,0,0)  ADDENDUM DEMO (TV) - NW                     
         DC    AL4(34200)          'ON'                                         
         DC    AL4(17100)          'OFF'                                        
         DC    AL4(25000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    AL4(9100)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(9800)           EXTRA                                        
         DC    4AL4(0)             N/D                                          
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(215,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - GA              
         DC    AL4(15800)          ANN ALONE                                    
         DC    AL4(15800)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(11500)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11500)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(11500)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(216,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - KS              
         DC    AL4(8235)           ANN ALONE                                    
         DC    AL4(8235)           AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(8235)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8235)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(8235)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
*                                                                               
         DC    AL2(217,48,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - TX              
         DC    AL4(14490)          ANN ALONE                                    
         DC    AL4(14490)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(9560)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(9560)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(9560)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         DC    AL4(14625)          SOLO/DUO                                     
*                                                                               
         DC    AL2(218,44,0,0,0,0)  ADDENDUM DEMO (AFT RADIO) - NW              
         DC    AL4(8700)          ANN ALONE                                     
         DC    AL4(8700)          AR,AS,P,ANN,1-4MS,1-4SS                       
         DC    AL4(5800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(5800)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(5800)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17650)          SE                                           
         DC    AL4(8565)           C3,C6                                        
         DC    AL4(13700)          C9                                           
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES - TV                                    
*                                                                               
ADWTAB   DC    AL2(135,52,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(36100)          ON CAMERA                                    
         DC    AL4(27100)          OFF                                          
         DC    AL4(19700)                                                       
         DC    AL4(17500)                                                       
         DC    AL4(14400)                                                       
         DC    AL4(11400)                                                       
         DC    AL4(9900)                                                        
         DC    AL4(8100)                                                        
         DC    AL4(38500)          SOLO/DUO ON CAMERA                           
         DC    AL4(28900)          OFF CAMERA                                   
*                                                                               
         DC    AL2(135,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1234)                                                        
         DC    AL4(845)                                                         
         DC    AL4(719)                                                         
         DC    AL4(620)                                                         
         DC    AL4(506)                                                         
         DC    AL4(255)                                                         
         DC    AL4(201)                                                         
         DC    AL4(167)                                                         
         DC    AL4(926)                                                         
         DC    AL4(634)                                                         
*                                                                               
         DC    AL2(135,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(458)                                                         
         DC    AL4(359)                                                         
         DC    AL4(371)                                                         
         DC    AL4(314)                                                         
         DC    AL4(260)                                                         
         DC    AL4(107)                                                         
         DC    AL4(74)                                                          
         DC    AL4(67)                                                          
         DC    AL4(344)                                                         
         DC    AL4(269)                                                         
*                                                                               
         DC    AL2(135,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(458)                                                         
         DC    AL4(359)                                                         
         DC    AL4(268)                                                         
         DC    AL4(210)                                                         
         DC    AL4(176)                                                         
         DC    AL4(65)                                                          
         DC    AL4(37)                                                          
         DC    AL4(37)                                                          
         DC    AL4(344)                                                         
         DC    AL4(269)                                                         
*                                                                               
         DC    AL2(135,52,126,255,0,0)  UNITS 126+                              
         DC    AL4(458)                                                         
         DC    AL4(359)                                                         
         DC    AL4(133)                                                         
         DC    AL4(107)                                                         
         DC    AL4(94)                                                          
         DC    AL4(65)                                                          
         DC    AL4(37)                                                          
         DC    AL4(37)                                                          
         DC    AL4(344)                                                         
         DC    AL4(269)                                                         
*                                                                               
         DC    AL2(140,52,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(38600)          ON CAMERA                                    
         DC    AL4(29100)          OFF                                          
         DC    AL4(21100)                                                       
         DC    AL4(18700)                                                       
         DC    AL4(15500)                                                       
         DC    AL4(12200)                                                       
         DC    AL4(10600)                                                       
         DC    AL4(8700)                                                        
         DC    AL4(27675)                                                       
         DC    AL4(20775)                                                       
*                                                                               
         DC    AL2(140,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1322)                                                        
         DC    AL4(905)                                                         
         DC    AL4(770)                                                         
         DC    AL4(664)                                                         
         DC    AL4(543)                                                         
         DC    AL4(273)                                                         
         DC    AL4(215)                                                         
         DC    AL4(179)                                                         
         DC    AL4(992)                                                         
         DC    AL4(679)                                                         
*                                                                               
         DC    AL2(140,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
         DC    AL4(397)                                                         
         DC    AL4(336)                                                         
         DC    AL4(279)                                                         
         DC    AL4(115)                                                         
         DC    AL4(79)                                                          
         DC    AL4(72)                                                          
         DC    AL4(368)                                                         
         DC    AL4(289)                                                         
*                                                                               
         DC    AL2(140,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
         DC    AL4(288)                                                         
         DC    AL4(225)                                                         
         DC    AL4(188)                                                         
         DC    AL4(70)                                                          
         DC    AL4(39)                                                          
         DC    AL4(39)                                                          
         DC    AL4(368)                                                         
         DC    AL4(289)                                                         
*                                                                               
         DC    AL2(140,52,126,225,0,0)  UNITS 126+                              
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
         DC    AL4(142)                                                         
         DC    AL4(115)                                                         
         DC    AL4(100)                                                         
         DC    AL4(70)                                                          
         DC    AL4(39)                                                          
         DC    AL4(39)                                                          
         DC    AL4(368)                                                         
         DC    AL4(289)                                                         
*                                                                               
         DC    AL2(141,44,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(29035)          ON CAMERA                                    
         DC    AL4(21810)          OFF                                          
         DC    AL4(22400)                                                       
         DC    AL4(19200)                                                       
         DC    AL4(15290)                                                       
         DC    AL4(9720)                                                        
         DC    AL4(7940)                                                        
         DC    AL4(5685)                                                        
*                                                                               
         DC    AL2(141,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(143,44,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(39000)          ON CAMERA                                    
         DC    AL4(29100)          OFF                                          
         DC    AL4(28400)                                                       
         DC    AL4(28400)                                                       
         DC    AL4(28400)                                                       
         DC    AL4(16400)                                                       
         DC    AL4(16400)                                                       
         DC    AL4(16400)                                                       
*                                                                               
         DC    AL2(143,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(2027)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(1387)                                                        
*                                                                               
         DC    AL2(150,52,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(41200)          ON CAMERA                                    
         DC    AL4(31000)          OFF                                          
         DC    AL4(22600)                                                       
         DC    AL4(20000)                                                       
         DC    AL4(16540)                                                       
         DC    AL4(13100)                                                       
         DC    AL4(11300)                                                       
         DC    AL4(9200)                                                        
         DC    AL4(30900)          SOLO/DUO ON CAMERA                           
         DC    AL4(23250)          OFF CAMERA                                   
*                                                                               
         DC    AL2(150,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1411)                                                        
         DC    AL4(966)                                                         
         DC    AL4(822)                                                         
         DC    AL4(709)                                                         
         DC    AL4(579)                                                         
         DC    AL4(291)                                                         
         DC    AL4(230)                                                         
         DC    AL4(191)                                                         
         DC    AL4(1058)                                                        
         DC    AL4(725)                                                         
*                                                                               
         DC    AL2(150,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(523)                                                         
         DC    AL4(410)                                                         
         DC    AL4(424)                                                         
         DC    AL4(359)                                                         
         DC    AL4(297)                                                         
         DC    AL4(123)                                                         
         DC    AL4(84)                                                          
         DC    AL4(77)                                                          
         DC    AL4(392)                                                         
         DC    AL4(308)                                                         
*                                                                               
         DC    AL2(150,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(523)                                                         
         DC    AL4(410)                                                         
         DC    AL4(307)                                                         
         DC    AL4(240)                                                         
         DC    AL4(201)                                                         
         DC    AL4(74)                                                          
         DC    AL4(42)                                                          
         DC    AL4(42)                                                          
         DC    AL4(392)                                                         
         DC    AL4(308)                                                         
*                                                                               
         DC    AL2(150,52,126,225,0,0)  UNITS 126+                              
         DC    AL4(523)                                                         
         DC    AL4(410)                                                         
         DC    AL4(152)                                                         
         DC    AL4(123)                                                         
         DC    AL4(107)                                                         
         DC    AL4(74)                                                          
         DC    AL4(42)                                                          
         DC    AL4(42)                                                          
         DC    AL4(392)                                                         
         DC    AL4(308)                                                         
*                                                                               
         DC    AL2(151,44,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(37215)          ON CAMERA                                    
         DC    AL4(27970)          OFF                                          
         DC    AL4(27735)                                                       
         DC    AL4(23585)                                                       
         DC    AL4(18725)                                                       
         DC    AL4(12210)                                                       
         DC    AL4(9605)                                                        
         DC    AL4(7230)                                                        
*                                                                               
         DC    AL2(151,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(160,52,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(51500)          ON CAMERA                                    
         DC    AL4(38700)          OFF                                          
         DC    AL4(28200)                                                       
         DC    AL4(25000)                                                       
         DC    AL4(20600)                                                       
         DC    AL4(16300)                                                       
         DC    AL4(14200)                                                       
         DC    AL4(11600)                                                       
         DC    AL4(38500)          SOLO/DUO ON CAM                              
         DC    AL4(28900)          SOLO/DUO OFF CAM                             
*                                                                               
         DC    AL2(160,52,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1763)                                                        
         DC    AL4(1207)                                                        
         DC    AL4(1027)                                                        
         DC    AL4(886)                                                         
         DC    AL4(723)                                                         
         DC    AL4(364)                                                         
         DC    AL4(287)                                                         
         DC    AL4(239)                                                         
         DC    AL4(1322)                                                        
         DC    AL4(905)                                                         
*                                                                               
         DC    AL2(160,52,26,60,0,0)  UNITS 26-60                               
         DC    AL4(654)                                                         
         DC    AL4(513)                                                         
         DC    AL4(530)                                                         
         DC    AL4(449)                                                         
         DC    AL4(372)                                                         
         DC    AL4(153)                                                         
         DC    AL4(105)                                                         
         DC    AL4(96)                                                          
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
*                                                                               
         DC    AL2(160,52,61,125,0,0)  UNITS 61-125                             
         DC    AL4(654)                                                         
         DC    AL4(513)                                                         
         DC    AL4(384)                                                         
         DC    AL4(300)                                                         
         DC    AL4(251)                                                         
         DC    AL4(93)                                                          
         DC    AL4(53)                                                          
         DC    AL4(53)                                                          
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
*                                                                               
         DC    AL2(160,52,126,255,0,0)  UNITS 126+                              
         DC    AL4(654)                                                         
         DC    AL4(513)                                                         
         DC    AL4(190)                                                         
         DC    AL4(153)                                                         
         DC    AL4(134)                                                         
         DC    AL4(93)                                                          
         DC    AL4(53)                                                          
         DC    AL4(53)                                                          
         DC    AL4(491)                                                         
         DC    AL4(385)                                                         
*                                                                               
         DC    AL2(161,44,1,1,0,0)  13 WEEKS - KS - UNIT 1                      
         DC    AL4(45400)          ON CAMERA                                    
         DC    AL4(34015)          OFF                                          
         DC    AL4(32830)                                                       
         DC    AL4(27970)                                                       
         DC    AL4(22405)                                                       
         DC    AL4(14340)                                                       
         DC    AL4(11610)                                                       
         DC    AL4(8655)                                                        
*                                                                               
         DC    AL2(161,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(1080)                                                        
         DC    AL4(1080)                                                        
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(295)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(162,44,1,1,0,0)  13 WEEKS - TX - UNIT 1                      
         DC    AL4(47375)           ON CAMERA                                   
         DC    AL4(35625)           OFF                                         
         DC    AL4(34680)                                                       
         DC    AL4(34680)                                                       
         DC    AL4(34680)                                                       
         DC    AL4(20090)                                                       
         DC    AL4(20090)                                                       
         DC    AL4(20090)                                                       
*                                                                               
         DC    AL2(162,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(1622)                                                        
         DC    AL4(1110)                                                        
         DC    AL4(1264)                                                        
         DC    AL4(1264)                                                        
         DC    AL4(1264)                                                        
         DC    AL4(448)                                                         
         DC    AL4(448)                                                         
         DC    AL4(448)                                                         
*                                                                               
         DC    AL2(162,44,26,255,0,0)  UNITS 26+                                
         DC    AL4(602)                                                         
         DC    AL4(472)                                                         
         DC    AL4(652)                                                         
         DC    AL4(652)                                                         
         DC    AL4(652)                                                         
         DC    AL4(189)                                                         
         DC    AL4(189)                                                         
         DC    AL4(189)                                                         
*                                                                               
         DC    AL2(163,44,1,1,0,0)  13 WEEKS - NW - UNIT 1                      
         DC    AL4(45800)          ON CAMERA                                    
         DC    AL4(34300)          OFF                                          
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
         DC    AL4(33400)                                                       
         DC    AL4(19300)                                                       
         DC    AL4(19300)                                                       
         DC    AL4(19300)                                                       
*                                                                               
         DC    AL2(163,44,2,255,0,0)  UNITS 2+                                  
         DC    AL4(2027)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(2027)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(1387)                                                        
         DC    AL4(1387)                                                        
         EJECT                                                                  
*              ADDENDUM WILDSPOT TABLES -  RADIO                                
*                                                                               
         DC    AL2(170,40,1,1,0,0)  3 DAY - GA - UNIT 1                         
         DC    AL4(16000)          ANN ALONE                                    
         DC    AL4(16000)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(8800)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(7800)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(6900)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(15800)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(11500)          SOLO/DUO                                     
*                                                                               
         DC    AL2(170,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(236)                                                         
         DC    AL4(236)                                                         
         DC    AL4(91)                                                          
         DC    AL4(78)                                                          
         DC    AL4(33)                                                          
         DC    AL4(0)                                                           
         DC    AL4(167)                                                         
*                                                                               
         DC    AL2(170,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(177)                                                         
         DC    AL4(177)                                                         
         DC    AL4(78)                                                          
         DC    AL4(60)                                                          
         DC    AL4(60)                                                          
         DC    AL4(0)                                                           
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(170,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(177)                                                         
         DC    AL4(177)                                                         
         DC    AL4(44)                                                          
         DC    AL4(38)                                                          
         DC    AL4(38)                                                          
         DC    AL4(0)                                                           
         DC    AL4(125)                                                         
*                                                                               
         DC    AL2(175,40,1,1,0,0)  1 WEEK - GA - UNIT 1                        
         DC    AL4(17200)          ANN ALONE                                    
         DC    AL4(17200)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(9400)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(8300)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7400)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(17200)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(11500)          SOLO/DUO                                     
*                                                                               
         DC    AL2(175,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(253)                                                         
         DC    AL4(253)                                                         
         DC    AL4(98)                                                          
         DC    AL4(84)                                                          
         DC    AL4(35)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(175,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(190)                                                         
         DC    AL4(190)                                                         
         DC    AL4(84)                                                          
         DC    AL4(64)                                                          
         DC    AL4(64)                                                          
         DC    AL4(0)                                                           
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(175,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(190)                                                         
         DC    AL4(190)                                                         
         DC    AL4(47)                                                          
         DC    AL4(41)                                                          
         DC    AL4(41)                                                          
         DC    AL4(0)                                                           
         DC    AL4(134)                                                         
*                                                                               
         DC    AL2(176,36,1,1,0,0)  1 WEEK - KS - UNIT 1                        
         DC    AL4(12350)                                                       
         DC    AL4(12350)                                                       
         DC    AL4(7735)                                                        
         DC    AL4(6485)                                                        
         DC    AL4(5740)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(176,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(178,36,1,1,0,0)  2 WEEK - NW - UNIT 1                        
         DC    AL4(17600)                                                       
         DC    AL4(17600)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(13000)                                                       
         DC    AL4(17600)          SE                                           
*                                                                               
         DC    AL2(178,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
*                                                                               
         DC    AL2(185,40,1,1,0,0)  4 WEEK - GA - UNIT 1                        
         DC    AL4(18300)          ANN ALONE                                    
         DC    AL4(18300)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(10100)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(8900)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(7900)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(18300)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(12960)          SOLO/DUO                                     
*                                                                               
         DC    AL2(185,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(270)                                                         
         DC    AL4(270)                                                         
         DC    AL4(105)                                                         
         DC    AL4(89)                                                          
         DC    AL4(38)                                                          
         DC    AL4(0)                                                           
         DC    AL4(191)                                                         
*                                                                               
         DC    AL2(185,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(203)                                                         
         DC    AL4(203)                                                         
         DC    AL4(89)                                                          
         DC    AL4(69)                                                          
         DC    AL4(69)                                                          
         DC    AL4(0)                                                           
         DC    AL4(143)                                                         
*                                                                               
         DC    AL2(185,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(203)                                                         
         DC    AL4(203)                                                         
         DC    AL4(50)                                                          
         DC    AL4(44)                                                          
         DC    AL4(44)                                                          
         DC    AL4(0)                                                           
         DC    AL4(143)                                                         
*                                                                               
         DC    AL2(186,36,1,1,0,0)  31 DAY - KS - UNIT 1                        
         DC    AL4(15845)                                                       
         DC    AL4(15845)                                                       
         DC    AL4(9360)                                                        
         DC    AL4(8235)                                                        
         DC    AL4(7360)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(186,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(195,40,1,1,0,0)  13 WEEK - GA - UNIT 1                       
         DC    AL4(22900)          ANN ALONE                                    
         DC    AL4(22900)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(12600)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(11100)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(9900)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(22900)          SE (ONLY GETS PAID FOR FIRST UNIT)           
         DC    AL4(16200)          SOLO/DUO                                     
*                                                                               
         DC    AL2(195,40,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(337)                                                         
         DC    AL4(337)                                                         
         DC    AL4(131)                                                         
         DC    AL4(112)                                                         
         DC    AL4(47)                                                          
         DC    AL4(0)                                                           
         DC    AL4(239)                                                         
*                                                                               
         DC    AL2(195,40,26,60,0,0)  UNITS 26-60                               
         DC    AL4(253)                                                         
         DC    AL4(253)                                                         
         DC    AL4(112)                                                         
         DC    AL4(86)                                                          
         DC    AL4(86)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(195,40,61,255,0,0)  UNITS 61+                                
         DC    AL4(253)                                                         
         DC    AL4(253)                                                         
         DC    AL4(63)                                                          
         DC    AL4(55)                                                          
         DC    AL4(55)                                                          
         DC    AL4(0)                                                           
         DC    AL4(179)                                                         
*                                                                               
         DC    AL2(196,36,1,1,0,0)  13 WEEK - KS - UNIT 1                       
         DC    AL4(19340)                                                       
         DC    AL4(19340)                                                       
         DC    AL4(10855)                                                       
         DC    AL4(9610)                                                        
         DC    AL4(8605)                                                        
         DC    AL4(17650)          SE                                           
*                                                                               
         DC    AL2(196,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(415)                                                         
         DC    AL4(415)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(170)                                                         
         DC    AL4(415)                                                         
*                                                                               
         DC    AL2(197,36,1,1,0,0)  13 WEEK - TX - UNIT 1                       
         DC    AL4(21030)                                                       
         DC    AL4(21030)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(15490)                                                       
         DC    AL4(21030)                                                       
*                                                                               
         DC    AL2(197,36,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(310)                                                         
         DC    AL4(310)                                                         
         DC    AL4(161)                                                         
         DC    AL4(161)                                                         
         DC    AL4(161)                                                         
         DC    AL4(310)                                                         
*                                                                               
         DC    AL2(197,36,26,255,0,0)  UNITS 26+                                
         DC    AL4(233)                                                         
         DC    AL4(233)                                                         
         DC    AL4(138)                                                         
         DC    AL4(138)                                                         
         DC    AL4(138)                                                         
         DC    AL4(233)                                                         
*                                                                               
         DC    AL2(198,36,1,1,0,0)  13 WEEK - NW - UNIT 1                       
         DC    AL4(20700)                                                       
         DC    AL4(20700)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(15300)                                                       
         DC    AL4(20700)                                                       
*                                                                               
         DC    AL2(198,36,2,255,0,0)  UNITS 2+                                  
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         DC    AL4(386)                                                         
         EJECT                                                                  
*              ADDENDUM CABLE                                                   
*                                                                               
         DC    AL2(262,44,0,0,0,0)  1-50,000 SUBSCRIBERS TX                     
         DC    AL4(2090)            PRINCIPAL ON CAMERA                         
         DC    AL4(1430)              "     OFF CAMERA                          
         DC    AL4(1635)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1410)              "   6-8 "    "                            
         DC    AL4(1150)              "    9+ "    "                            
         DC    AL4(585)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(450)               "   6-8  "    "                           
         DC    AL4(380)               "    9+  "    "                           
*                                                                               
         DC    AL2(263,44,0,0,0,0)  1-50,000 SUBSCRIBERS NW                     
         DC    AL4(635)             PRINCIPAL ON CAMERA                         
         DC    AL4(445)               "     OFF CAMERA                          
         DC    AL4(500)             GROUP 3-5 ON CAMERA                         
         DC    AL4(435)               "   6-8 "    "                            
         DC    AL4(350)               "    9+ "    "                            
         DC    AL4(185)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(145)               "   6-8  "    "                           
         DC    AL4(120)               "    9+  "    "                           
*                                                                               
         DC    AL2(272,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS TX               
         DC    AL4(4205)                                                        
         DC    AL4(2870)                                                        
         DC    AL4(3270)                                                        
         DC    AL4(2820)                                                        
         DC    AL4(2295)                                                        
         DC    AL4(1160)                                                        
         DC    AL4(910)                                                         
         DC    AL4(760)                                                         
*                                                                               
         DC    AL2(273,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS NW               
         DC    AL4(1275)                                                        
         DC    AL4(870)                                                         
         DC    AL4(1075)                                                        
         DC    AL4(860)                                                         
         DC    AL4(710)                                                         
         DC    AL4(360)                                                         
         DC    AL4(280)                                                         
         DC    AL4(235)                                                         
*                                                                               
         DC    AL2(282,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS TX              
         DC    AL4(6295)                                                        
         DC    AL4(4310)                                                        
         DC    AL4(4915)                                                        
         DC    AL4(4230)                                                        
         DC    AL4(3450)                                                        
         DC    AL4(1740)                                                        
         DC    AL4(1360)                                                        
         DC    AL4(1140)                                                        
*                                                                               
         DC    AL2(283,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS NW              
         DC    AL4(1915)                                                        
         DC    AL4(1315)                                                        
         DC    AL4(1490)                                                        
         DC    AL4(1290)                                                        
         DC    AL4(1050)                                                        
         DC    AL4(535)                                                         
         DC    AL4(420)                                                         
         DC    AL4(355)                                                         
*                                                                               
         DC    AL2(292,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS TX              
         DC    AL4(8395)                                                        
         DC    AL4(5750)                                                        
         DC    AL4(6550)                                                        
         DC    AL4(5640)                                                        
         DC    AL4(4600)                                                        
         DC    AL4(2340)                                                        
         DC    AL4(1815)                                                        
         DC    AL4(1520)                                                        
*                                                                               
         DC    AL2(293,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS NW              
         DC    AL4(2545)                                                        
         DC    AL4(1820)                                                        
         DC    AL4(1990)                                                        
         DC    AL4(1720)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(715)                                                         
         DC    AL4(555)                                                         
         DC    AL4(465)                                                         
*                                                                               
         DC    AL2(302,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS TX              
         DC    AL4(10490)                                                       
         DC    AL4(7180)                                                        
         DC    AL4(8185)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(5750)                                                        
         DC    AL4(2910)                                                        
         DC    AL4(2275)                                                        
         DC    AL4(1910)                                                        
*                                                                               
         DC    AL2(303,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS NW              
         DC    AL4(3185)                                                        
         DC    AL4(2185)                                                        
         DC    AL4(2495)                                                        
         DC    AL4(2145)                                                        
         DC    AL4(1745)                                                        
         DC    AL4(895)                                                         
         DC    AL4(700)                                                         
         DC    AL4(585)                                                         
*                                                                               
         DC    AL2(312,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS TX              
         DC    AL4(20990)                                                       
         DC    AL4(14370)                                                       
         DC    AL4(16380)                                                       
         DC    AL4(14105)                                                       
         DC    AL4(11495)                                                       
         DC    AL4(5810)                                                        
         DC    AL4(4545)                                                        
         DC    AL4(3805)                                                        
*                                                                               
         DC    AL2(313,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS NW              
         DC    AL4(6375)                                                        
         DC    AL4(4370)                                                        
         DC    AL4(4975)                                                        
         DC    AL4(4285)                                                        
         DC    AL4(3495)                                                        
         DC    AL4(1765)                                                        
         DC    AL4(1380)                                                        
         DC    AL4(1155)                                                        
*                                                                               
         DC    AL2(322,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS TX              
         DC    AL4(31480)                                                       
         DC    AL4(21550)                                                       
         DC    AL4(24560)                                                       
         DC    AL4(21155)                                                       
         DC    AL4(17250)                                                       
         DC    AL4(8720)                                                        
         DC    AL4(6815)                                                        
         DC    AL4(5710)                                                        
*                                                                               
         DC    AL2(323,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS NW              
         DC    AL4(9555)                                                        
         DC    AL4(6545)                                                        
         DC    AL4(7460)                                                        
         DC    AL4(6425)                                                        
         DC    AL4(5235)                                                        
         DC    AL4(2655)                                                        
         DC    AL4(2075)                                                        
         DC    AL4(1740)                                                        
*                                                                               
         DC    AL2(332,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS TX            
         DC    AL4(41970)                                                       
         DC    AL4(28740)                                                       
         DC    AL4(32750)                                                       
         DC    AL4(28210)                                                       
         DC    AL4(23000)                                                       
         DC    AL4(11630)                                                       
         DC    AL4(9090)                                                        
         DC    AL4(7620)                                                        
*                                                                               
         DC    AL2(333,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS NW            
         DC    AL4(12735)                                                       
         DC    AL4(8725)                                                        
         DC    AL4(9940)                                                        
         DC    AL4(8565)                                                        
         DC    AL4(6985)                                                        
         DC    AL4(3535)                                                        
         DC    AL4(2765)                                                        
         DC    AL4(2320)                                                        
*                                                                               
         DC    AL2(342,44,0,0,0,0)  1,000,0001 + SUBSCRIBERS TX                 
         DC    AL4(47375)                                                       
         DC    AL4(35625)                                                       
         DC    AL4(34685)                                                       
         DC    AL4(30710)                                                       
         DC    AL4(25390)                                                       
         DC    AL4(20090)                                                       
         DC    AL4(17435)                                                       
         DC    AL4(14220)                                                       
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
         DC    AL4(17475)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(0)                                                           
         DC    AL4(17475)                                                       
         DC    AL4(0)                                                           
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
*                                                                               
         DC    AL2(53,80,25,49,0,0)  UNITS 25-49                                
         DC    AL4(9755)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
*                                                                               
         DC    AL2(53,80,50,255,0,0)  UNITS 50+                                 
         DC    AL4(5340)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
*                                                                               
         DC    AL2(54,80,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(59220)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(43355)                                                       
         DC    AL4(38385)                                                       
         DC    AL4(31740)                                                       
         DC    AL4(25115)                                                       
         DC    AL4(21795)                                                       
         DC    AL4(17775)                                                       
         DC    AL4(32300)                                                       
         DC    AL4(49290)                                                       
         DC    AL4(18750)                                                       
         DC    AL4(32785)                                                       
         DC    AL4(91215)          PIL                                          
         DC    AL4(70135)          PI                                           
         DC    AL4(38745)          SE                                           
         DC    AL4(9495)           C3,C6                                        
         DC    AL4(18725)          C9                                           
*                                                                               
         DC    AL2(54,80,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(17475)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(17475)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
         DC    AL4(13245)                                                       
*                                                                               
         DC    AL2(54,80,26,50,0,0)  UNITS 26-50                                
         DC    AL4(9755)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(9755)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
         DC    AL4(7350)                                                        
*                                                                               
         DC    AL2(54,80,51,255,0,0)  UNITS 51+                                 
         DC    AL4(5340)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(5340)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
         DC    AL4(4005)                                                        
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(26285)                                                       
         DC    AL4(26285)                                                       
         DC    AL4(19365)                                                       
         DC    AL4(17135)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(20210)                                                       
         DC    AL4(9025)                                                        
         DC    AL4(14435)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
         DC    AL4(10875)                                                       
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
         DC    AL4(7805)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
         DC    AL4(4260)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(94750)          (1.6 X 59220, BUT WANT NEAREST .05)          
         DC    AL4(71250)          (1.6 X 44530, BUT WANT NEAREST .05)          
         DC    AL4(69370)          (1.6 X 43355, BUT WANT NEAREST .05)          
         DC    AL4(61415)          (1.6 X 38385, BUT WANT NEAREST .05)          
         DC    AL4(50785)          (1.6 X 31740, BUT WANT NEAREST .05)          
         DC    AL4(40185)          (1.6 X 25115, BUT WANT NEAREST .05)          
         DC    AL4(34870)          (1.6 X 21795, BUT WANT NEAREST .05)          
         DC    AL4(28440)          (1.6 X 17775, BUT WANT NEAREST .05)          
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(42055)          (1.6 X 26285, BUT WANT NEAREST .05)          
         DC    AL4(42055)          (1.6 X 26285, BUT WANT NEAREST .05)          
         DC    AL4(30985)          (1.6 X 19365, BUT WANT NEAREST .05)          
         DC    AL4(27415)          (1.6 X 17135, BUT WANT NEAREST .05)          
         DC    AL4(24320)          (1.6 X 15200, BUT WANT NEAREST .05)          
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2011 - OCT 31, 2012)  YEAR 1                              
         DC    AL2(440,80,0,0,0,0) INDUST ON CAMERA - CAT1                      
         DC    AL4(48050)          PRINCIPAL                                    
         DC    AL4(31250)          PHD                                          
         DC    AL4(120900)         P3D                                          
         DC    AL4(168750)         P5D                                          
         DC    AL4(185700)         P6D                                          
         DC    AL4(42950)          SD                                           
         DC    AL4(102800)         S3D                                          
         DC    AL4(171500)         S5D/S6D                                      
         DC    AL4(35900)          GD                                           
         DC    AL4(86350)          G3D                                          
         DC    AL4(143800)         G5D/G6D                                      
         DC    AL4(28950)          GS                                           
         DC    AL4(36300)          SO                                           
         DC    AL4(87350)          NAR                                          
         DC    AL4(12500)          BG                                           
         DC    AL4(13750)          BS                                           
         DC    AL4(23350)          BSB                                          
*                                                                               
         DC    AL2(441,80,0,0,0,0) INDUST ON CAMERA - CAT2                      
         DC    AL4(59750)          PRINCIPAL                                    
         DC    AL4(38850)          PHD                                          
         DC    AL4(149050)         P3D                                          
         DC    AL4(208950)         P5D                                          
         DC    AL4(229850)         P6D                                          
         DC    AL4(53550)          SD                                           
         DC    AL4(128600)         S3D                                          
         DC    AL4(214300)         S5D/S6D                                      
         DC    AL4(45050)          GD                                           
         DC    AL4(107850)         G3D                                          
         DC    AL4(179850)         G5D/G6D                                      
         DC    AL4(35750)          GS                                           
         DC    AL4(44850)          SO                                           
         DC    AL4(103550)         NAR                                          
         DC    AL4(12500)          BG                                           
         DC    AL4(13750)          BS                                           
         DC    AL4(23350)          BSB                                          
*                                                                               
         DC    AL2(442,36,0,0,0,0) INDUST OFF CAMERA - CAT1                     
         DC    AL4(0)                                                           
         DC    AL4(39300)          P                                            
         DC    AL4(25800)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(21450)          SO                                           
         DC    AL4(17200)          GS                                           
*                                                                               
         DC    AL2(443,36,0,0,0,0) INDUST OFF CAMERA - CAT2                     
         DC    AL4(0)                                                           
         DC    AL4(43750)          P                                            
         DC    AL4(28950)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(24350)          SO                                           
         DC    AL4(19450)          GS                                           
*                                                                               
* INDUSTRIALS (NOV 1, 2012 - APR 30, 2014)  YEAR 2   +5 TO NUMBER               
         DC    AL2(445,80,0,0,0,0) INDUST ON CAMERA - CAT1                      
         DC    AL4(49000)          PRINCIPAL                                    
         DC    AL4(31850)          PHD                                          
         DC    AL4(123300)         P3D                                          
         DC    AL4(172150)         P5D                                          
         DC    AL4(189400)         P6D                                          
         DC    AL4(43800)          SD                                           
         DC    AL4(104850)         S3D                                          
         DC    AL4(174950)         S5D/S6D                                      
         DC    AL4(36600)          GD                                           
         DC    AL4(88100)          G3D                                          
         DC    AL4(146700)         G5D/G6D                                      
         DC    AL4(29550)          GS                                           
         DC    AL4(37050)          SO                                           
         DC    AL4(89100)          NAR                                          
         DC    AL4(12750)          BG                                           
         DC    AL4(14050)          BS                                           
         DC    AL4(23800)          BSB                                          
*                                                                               
         DC    AL2(446,80,0,0,0,0) INDUST ON CAMERA - CAT2                      
         DC    AL4(60950)          PRINCIPAL                                    
         DC    AL4(39600)          PHD                                          
         DC    AL4(152050)         P3D                                          
         DC    AL4(213150)         P5D                                          
         DC    AL4(234450)         P6D                                          
         DC    AL4(54600)          SD                                           
         DC    AL4(131150)         S3D                                          
         DC    AL4(218600)         S5D/S6D                                      
         DC    AL4(45950)          GD                                           
         DC    AL4(110000)         G3D                                          
         DC    AL4(183450)         G5D/G6D                                      
         DC    AL4(36450)          GS                                           
         DC    AL4(45750)          SO                                           
         DC    AL4(105600)         NAR                                          
         DC    AL4(12750)          BG                                           
         DC    AL4(14050)          BS                                           
         DC    AL4(23800)          BSB                                          
*                                                                               
         DC    AL2(447,36,0,0,0,0) INDUST OFF CAMERA - CAT1                     
         DC    AL4(0)                                                           
         DC    AL4(40100)          P                                            
         DC    AL4(26300)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(21900)          SO                                           
         DC    AL4(17550)          GS                                           
*                                                                               
         DC    AL2(448,36,0,0,0,0) INDUST OFF CAMERA - CAT2                     
         DC    AL4(0)                                                           
         DC    AL4(44650)          P                                            
         DC    AL4(29550)          SOLO/DUO                                     
         DC    AL4(0)                                                           
         DC    AL4(24850)          SO                                           
         DC    AL4(19850)          GS                                           
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2011 - OCT 31, 2012)  YEAR 1                              
         DC    AL2(450,36,0,0,0,0) INDUSTRIAL STORECASTING - BUYOUT             
         DC    AL4(117900)                          3X 3 MONTH RATE             
         DC    AL4(117900)         P                                            
         DC    AL4(117900)         SOLO/DUO                                     
         DC    AL4(117900)                                                      
         DC    AL4(117900)         SO                                           
         DC    AL4(117900)         GS                                           
*                                                                               
         DC    AL2(451,36,0,0,0,0) INDUSTRIAL STORECASTING - 3M                 
         DC    AL4(39300)                                                       
         DC    AL4(39300)          P                                            
         DC    AL4(39300)          SOLO/DUO                                     
         DC    AL4(39300)                                                       
         DC    AL4(39300)          SO                                           
         DC    AL4(39300)          GS                                           
*                                                                               
         DC    AL2(452,36,0,0,0,0) INDUSTRIAL STORECASTING - 6M/3MEXT           
         DC    AL4(78600)                2X 3 MONTH RATE                        
         DC    AL4(78600)          P                                            
         DC    AL4(78600)          SOLO/DUO                                     
         DC    AL4(78600)                                                       
         DC    AL4(78600)          SO                                           
         DC    AL4(78600)          GS                                           
*                                                                               
         DC    AL2(453,36,0,0,0,0) INDUSTRIAL STORECASTING - 6MEXT              
         DC    AL4(157200)               4X 3 MONTH RATE                        
         DC    AL4(157200)         P                                            
         DC    AL4(157200)         SOLO/DUO                                     
         DC    AL4(157200)                                                      
         DC    AL4(157200)         SO                                           
         DC    AL4(157200)         GS                                           
*                                                                               
* INDUSTRIALS (NOV 1, 2012 - APR 30, 2014)  YEAR 2      +5 TO NUMBER            
         DC    AL2(455,36,0,0,0,0) INDUSTRIAL STORECASTING - BUYOUT             
         DC    AL4(120300)                          3X 3 MONTH RATE             
         DC    AL4(120300)         P                                            
         DC    AL4(120300)         SOLO/DUO                                     
         DC    AL4(120300)                                                      
         DC    AL4(120300)         SO                                           
         DC    AL4(120300)         GS                                           
*                                                                               
         DC    AL2(456,36,0,0,0,0) INDUSTRIAL STORECASTING - 3M                 
         DC    AL4(40100)                                                       
         DC    AL4(40100)          P                                            
         DC    AL4(40100)          SOLO/DUO                                     
         DC    AL4(40100)                                                       
         DC    AL4(40100)          SO                                           
         DC    AL4(40100)          GS                                           
*                                                                               
         DC    AL2(457,36,0,0,0,0) INDUSTRIAL STORECASTING - 6M/3MEXT           
         DC    AL4(80200)                2X 3 MONTH RATE                        
         DC    AL4(80200)          P                                            
         DC    AL4(80200)          SOLO/DUO                                     
         DC    AL4(80200)                                                       
         DC    AL4(80200)          SO                                           
         DC    AL4(80200)          GS                                           
*                                                                               
         DC    AL2(458,36,0,0,0,0) INDUSTRIAL STORECASTING - 6MEXT              
         DC    AL4(160400)               4X 3 MONTH RATE                        
         DC    AL4(160400)         P                                            
         DC    AL4(160400)         SOLO/DUO                                     
         DC    AL4(160400)                                                      
         DC    AL4(160400)         SO                                           
         DC    AL4(160400)         GS                                           
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2011 - OCT 31, 2012)  YEAR 1                              
         DC    AL2(460,84,0,0,0,0) RTK ON CAMERA - CAT1                         
         DC    AL4(48050)          PRINCIPAL/ST                                 
         DC    AL4(31250)          PHD                                          
         DC    AL4(120900)         P3D                                          
         DC    AL4(168750)         P5D                                          
         DC    AL4(185700)         P6D                                          
         DC    AL4(48050)          S/D                                          
         DC    AL4(28950)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(36300)          SO                                           
         DC    AL4(87350)          NAR                                          
         DC    AL4(12500)          BG                                           
         DC    AL4(13750)          BS                                           
         DC    AL4(23350)          BSB                                          
         DC    AL4(42950)          SD                                           
         DC    AL4(35900)          GD3/GD6/GD9                                  
         DC    AL4(102800)         S3D                                          
         DC    AL4(86350)          G3D                                          
         DC    AL4(171500)         S5D/S6D                                      
         DC    AL4(143800)         G5D/G6D                                      
*                                                                               
         DC    AL2(461,84,0,0,0,0) RTK ON CAMERA - CAT2                         
         DC    AL4(59750)          PRINCIPAL/ST                                 
         DC    AL4(38850)          PHD                                          
         DC    AL4(149050)         P3D                                          
         DC    AL4(208950)         P5D                                          
         DC    AL4(229850)         P6D                                          
         DC    AL4(59750)          S/D                                          
         DC    AL4(35750)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(44850)          SO                                           
         DC    AL4(103500)         NAR                                          
         DC    AL4(12500)          BG                                           
         DC    AL4(13750)          BS                                           
         DC    AL4(23350)          BSB                                          
         DC    AL4(53550)          SD                                           
         DC    AL4(45050)          GD3/GD6/GD9                                  
         DC    AL4(128600)         S3D                                          
         DC    AL4(107850)         G3D                                          
         DC    AL4(214300)         S5D/S6D                                      
         DC    AL4(179850)         G5D/G6D                                      
*                                                                               
         DC    AL2(462,84,0,0,0,0) RTK OFF CAMERA - CAT1                        
         DC    AL4(39300)          PRINCIPAL/ST                                 
         DC    AL4(39300)          PHD                                          
         DC    AL4(39300)          P3D                                          
         DC    AL4(39300)          P5D                                          
         DC    AL4(39300)          P6D                                          
         DC    AL4(25800)          S/D                                          
         DC    AL4(17200)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(21450)          SO                                           
         DC    AL4(39300)          NAR                                          
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
         DC    AL4(43750)          PRINCIPAL/ST                                 
         DC    AL4(43750)          PHD                                          
         DC    AL4(43750)          P3D                                          
         DC    AL4(43750)          P5D                                          
         DC    AL4(43750)          P6D                                          
         DC    AL4(28950)          S/D                                          
         DC    AL4(19450)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(24350)          SO                                           
         DC    AL4(43750)          NAR                                          
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
* INDUSTRIALS (NOV 1, 2012 - APR 30, 2014)  YEAR 2      +5 TO NUMBER            
         DC    AL2(465,84,0,0,0,0) RTK ON CAMERA - CAT1                         
         DC    AL4(49000)          PRINCIPAL/ST                                 
         DC    AL4(31850)          PHD                                          
         DC    AL4(123300)         P3D                                          
         DC    AL4(172150)         P5D                                          
         DC    AL4(189400)         P6D                                          
         DC    AL4(49000)          S/D                                          
         DC    AL4(29550)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(37050)          SO                                           
         DC    AL4(89100)          NAR                                          
         DC    AL4(12750)          BG                                           
         DC    AL4(14050)          BS                                           
         DC    AL4(23800)          BSB                                          
         DC    AL4(43800)          SD                                           
         DC    AL4(36600)          GD3/GD6/GD9                                  
         DC    AL4(104850)         S3D                                          
         DC    AL4(88100)          G3D                                          
         DC    AL4(174950)         S5D/S6D                                      
         DC    AL4(146700)         G5D/G6D                                      
*                                                                               
         DC    AL2(466,84,0,0,0,0) RTK ON CAMERA - CAT2                         
         DC    AL4(60950)          PRINCIPAL/ST                                 
         DC    AL4(39600)          PHD                                          
         DC    AL4(152050)         P3D                                          
         DC    AL4(213150)         P5D                                          
         DC    AL4(234450)         P6D                                          
         DC    AL4(60950)          S/D                                          
         DC    AL4(36450)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(45750)          SO                                           
         DC    AL4(105600)         NAR                                          
         DC    AL4(12750)          BG                                           
         DC    AL4(14050)          BS                                           
         DC    AL4(23800)          BSB                                          
         DC    AL4(54600)          SD                                           
         DC    AL4(45950)          GD3/GD6/GD9                                  
         DC    AL4(131150)         S3D                                          
         DC    AL4(110000)         G3D                                          
         DC    AL4(218600)         S5D/S6D                                      
         DC    AL4(183450)         G5D/G6D                                      
*                                                                               
         DC    AL2(467,84,0,0,0,0) RTK OFF CAMERA - CAT1                        
         DC    AL4(40100)          PRINCIPAL/ST                                 
         DC    AL4(40100)          PHD                                          
         DC    AL4(40100)          P3D                                          
         DC    AL4(40100)          P5D                                          
         DC    AL4(40100)          P6D                                          
         DC    AL4(26300)          S/D                                          
         DC    AL4(17550)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(21900)          SO                                           
         DC    AL4(40100)          NAR                                          
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
         DC    AL4(44650)          PRINCIPAL/ST                                 
         DC    AL4(44650)          PHD                                          
         DC    AL4(44650)          P3D                                          
         DC    AL4(44650)          P5D                                          
         DC    AL4(44650)          P6D                                          
         DC    AL4(29550)          S/D                                          
         DC    AL4(19850)          G3/G6/G9/C3/C6/C9                            
         DC    AL4(24850)          SO                                           
         DC    AL4(44650)          NAR                                          
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
* INDUSTRIALS (MAY 1, 2011 - OCT 31, 2012)  YEAR 1                              
         DC    AL2(470,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT1                  
         DC    AL4(39300)          P    PRINCIPAL                               
         DC    AL4(39300)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(17700)          NP   NON-PRINCIPAL                           
         DC    AL4(25800)          S    SOLO/DUO                                
         DC    AL4(17200)          SO   STEP OUT                                
         DC    AL4(21450)          S16  SOLO <16 BARS                           
         DC    AL4(17200)          G3   GROUP                                   
         DC    AL4(21450)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(17200)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(471,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT2                  
         DC    AL4(43750)          P    PRINCIPAL                               
         DC    AL4(43750)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(19650)          NP   NON-PRINCIPAL                           
         DC    AL4(28950)          S    SOLO/DUO                                
         DC    AL4(19450)          SO   STEP OUT                                
         DC    AL4(24350)          S16  SOLO <16 BARS                           
         DC    AL4(19450)          G3   GROUP                                   
         DC    AL4(24350)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(19450)          C3   CONTRACTOR                              
*                                                                               
* INDUSTRIALS (NOV 1, 2012 - APR 30, 2014)  YEAR 2      +5 TO NUMBER            
         DC    AL2(475,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT1                  
         DC    AL4(40100)          P    PRINCIPAL                               
         DC    AL4(40100)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(18050)          NP   NON-PRINCIPAL                           
         DC    AL4(26300)          S    SOLO/DUO                                
         DC    AL4(17550)          SO   STEP OUT                                
         DC    AL4(21900)          S16  SOLO <16 BARS                           
         DC    AL4(17550)          G3   GROUP                                   
         DC    AL4(21900)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(17550)          C3   CONTRACTOR                              
*                                                                               
         DC    AL2(476,48,0,0,0,0) DIO INDUSTRIAL AUDIO - CAT2                  
         DC    AL4(44650)          P    PRINCIPAL                               
         DC    AL4(44650)          P3M  PRINCIPAL - 3 MINUTE                    
         DC    AL4(20050)          NP   NON-PRINCIPAL                           
         DC    AL4(29550)          S    SOLO/DUO                                
         DC    AL4(19850)          SO   STEP OUT                                
         DC    AL4(24850)          S16  SOLO <16 BARS                           
         DC    AL4(19850)          G3   GROUP                                   
         DC    AL4(24850)          G16  GROUP LESS THAN 16 BARS                 
         DC    AL4(19850)          C3   CONTRACTOR                              
         EJECT                                                                  
*                                                                               
* INDUSTRIALS (MAY 1, 2011 - OCT 31, 2012)  YEAR 1                              
         DC    AL2(480,16,0,0,0,0) IVR INTERACTIVE VOICE                        
         DC    AL4(21850)          P    PRINCIPAL                               
*                                                                               
* INDUSTRIALS (NOV 1, 2012 - APR 30, 2014)  YEAR 2      +5 TO NUMBER            
         DC    AL2(485,16,0,0,0,0) IVR INTERACTIVE VOICE                        
         DC    AL4(22300)          P    PRINCIPAL                               
         EJECT                                                                  
*              LOCAL CABLE TABLES                                               
*                                                                               
LCBTAB   DC    AL2(238,44,0,0,0,0)  1-50,000 SUBSCRIBERS                        
         DC    AL4(2615)            PRINCIPAL ON CAMERA                         
         DC    AL4(1785)                "     OFF CAMERA                        
         DC    AL4(2045)            GROUP 3-5 ON CAMERA                         
         DC    AL4(1765)              "   6-8 "    "                            
         DC    AL4(1435)              "    9+ "    "                            
         DC    AL4(730)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(565)               "   6-8  "    "                           
         DC    AL4(475)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(5255)                                                        
         DC    AL4(3590)                                                        
         DC    AL4(4090)                                                        
         DC    AL4(3525)                                                        
         DC    AL4(2870)                                                        
         DC    AL4(1450)                                                        
         DC    AL4(1135)                                                        
         DC    AL4(950)                                                         
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(7870)                                                        
         DC    AL4(5385)                                                        
         DC    AL4(6145)                                                        
         DC    AL4(5290)                                                        
         DC    AL4(4310)                                                        
         DC    AL4(2175)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1425)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(10495)                                                       
         DC    AL4(7185)                                                        
         DC    AL4(8185)                                                        
         DC    AL4(7050)                                                        
         DC    AL4(5750)                                                        
         DC    AL4(2925)                                                        
         DC    AL4(2270)                                                        
         DC    AL4(1900)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(13110)                                                       
         DC    AL4(8975)                                                        
         DC    AL4(10230)                                                       
         DC    AL4(8815)                                                        
         DC    AL4(7185)                                                        
         DC    AL4(3635)                                                        
         DC    AL4(2845)                                                        
         DC    AL4(2385)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(26240)                                                       
         DC    AL4(17965)                                                       
         DC    AL4(20475)                                                       
         DC    AL4(17630)                                                       
         DC    AL4(14370)                                                       
         DC    AL4(7265)                                                        
         DC    AL4(5680)                                                        
         DC    AL4(4755)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(39350)                                                       
         DC    AL4(26940)                                                       
         DC    AL4(30700)                                                       
         DC    AL4(26445)                                                       
         DC    AL4(21560)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(8520)                                                        
         DC    AL4(7140)                                                        
*                                                                               
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(52465)                                                       
         DC    AL4(35925)                                                       
         DC    AL4(40940)                                                       
         DC    AL4(35260)                                                       
         DC    AL4(28750)                                                       
         DC    AL4(14540)                                                       
         DC    AL4(11360)                                                       
         DC    AL4(9525)                                                        
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(59220)                                                       
         DC    AL4(44530)                                                       
         DC    AL4(43355)                                                       
         DC    AL4(38385)                                                       
         DC    AL4(31740)                                                       
         DC    AL4(25115)                                                       
         DC    AL4(21795)                                                       
         DC    AL4(17775)                                                       
         EJECT                                                                  
*              RATES FOR TEXAS ADDENDUM TAGS                                    
*                                                                               
         DC    AL2(247,80,1,24,0,0)  TX - TV, REGULAR, UNITS 1-24               
         DC    AL4(13980)          ON CAMERA                                    
         DC    AL4(10596)          OFF                                          
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(10596)                                                       
         DC    AL4(10596)                                                       
         DC    AL4(10596)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(13980)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10596)          SE                                           
         DC    AL4(10596)          C3,C6                                        
         DC    AL4(10596)          C9                                           
*                                                                               
         DC    AL2(247,80,25,49,0,0)  UNITS 25-49                               
         DC    AL4(7804)           ON CAMERA                                    
         DC    AL4(5880)           OFF                                          
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(5880)                                                        
         DC    AL4(5880)                                                        
         DC    AL4(5880)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(7804)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5880)           SE                                           
         DC    AL4(5880)           C3,C6                                        
         DC    AL4(5880)           C9                                           
*                                                                               
         DC    AL2(247,80,50,255,0,0)  UNITS 50+                                
         DC    AL4(4272)           ON CAMERA                                    
         DC    AL4(3204)           OFF                                          
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(3204)                                                        
         DC    AL4(3204)                                                        
         DC    AL4(3204)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(4272)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(3204)           SE                                           
         DC    AL4(3204)           C3,C6                                        
         DC    AL4(3204)           C9                                           
*                                                                               
         DC    AL2(248,44,1,24,0,0)  RADIO TAGS - REGULAR, UNITS 1-24           
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
         DC    AL4(8700)                                                        
*                                                                               
         DC    AL2(248,44,25,49,0,0)  UNITS 25-49                               
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
         DC    AL4(6245)                                                        
*                                                                               
         DC    AL2(248,44,50,255,0,0)  UNITS 50+                                
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         DC    AL4(3410)                                                        
         EJECT                                                                  
*              RATES FOR GEORGIA ADDENDUM TAGS                                  
*                                                                               
         DC    AL2(249,88,1,24,0,0)  GA - TV, REGULAR, UNITS 1-24               
         DC    AL4(15200)          ON CAMERA                                    
         DC    AL4(11500)          OFF                                          
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(11500)                                                       
         DC    AL4(11500)                                                       
         DC    AL4(11500)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(15200)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(11500)          SE                                           
         DC    AL4(11500)          C3,C6                                        
         DC    AL4(11500)          C9                                           
         DC    AL4(15200)          SOLO/DUO ON CAMERA                           
         DC    AL4(11500)                   OFF                                 
*                                                                               
         DC    AL2(249,88,25,49,0,0)  UNITS 25-49                               
         DC    AL4(8500)           ON CAMERA                                    
         DC    AL4(6400)           OFF                                          
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(6400)                                                        
         DC    AL4(6400)                                                        
         DC    AL4(6400)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(8500)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(6400)           SE                                           
         DC    AL4(6400)           C3,C6                                        
         DC    AL4(6400)           C9                                           
         DC    AL4(8500)           SOLO/DUO ON CAMERA                           
         DC    AL4(6400)           OFF                                          
*                                                                               
         DC    AL2(249,88,50,255,0,0)  UNITS 51+                                
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
         DC    AL4(4600)           SOLO/DUO ON CAMERA                           
         DC    AL4(3500)           OFF                                          
         EJECT                                                                  
*              RATES FOR NORTHWEST ADDENDUM TAGS                                
*                                                                               
         DC    AL2(340,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  2WK            
         DC    AL4(12400)          ON CAMERA                                    
         DC    AL4(9500)           OFF                                          
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(12400)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(9500)           SE                                           
         DC    AL4(9500)           C3,C6                                        
         DC    AL4(9500)           C9                                           
*                                                                               
         DC    AL2(340,80,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(10000)          ON CAMERA                                    
         DC    AL4(7600)           OFF                                          
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    AL4(7600)                                                        
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(10000)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(7600)           SE                                           
         DC    AL4(7600)           C3,C6                                        
         DC    AL4(7600)           C9                                           
*                                                                               
         DC    AL2(340,80,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(5600)           ON CAMERA                                    
         DC    AL4(4200)           OFF                                          
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(4200)                                                        
         DC    AL4(4200)                                                        
         DC    AL4(4200)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(5600)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4200)           SE                                           
         DC    AL4(4200)           C3,C6                                        
         DC    AL4(4200)           C9                                           
*                                                                               
         DC    AL2(340,80,25,255,0,0)  UNITS 26+  2-WK                          
         DC    AL4(2700)           ON CAMERA                                    
         DC    AL4(2300)           OFF                                          
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2300)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(2700)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2300)           SE                                           
         DC    AL4(2300)           C3,C6                                        
         DC    AL4(2300)           C9                                           
*                                                                               
         DC    AL2(341,80,1,3,0,0)  NW - TV, REGULAR, UNITS 1-4  13WK           
         DC    AL4(14500)          ON CAMERA                                    
         DC    AL4(10900)          OFF                                          
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(10900)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(14500)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(10900)          SE                                           
         DC    AL4(10900)          C3,C6                                        
         DC    AL4(10900)          C9                                           
*                                                                               
         DC    AL2(341,80,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(11900)          ON CAMERA                                    
         DC    AL4(8900)           OFF                                          
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(8900)                                                        
         DC    AL4(8900)                                                        
         DC    AL4(8900)                                                        
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(11900)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8900)           SE                                           
         DC    AL4(8900)           C3,C6                                        
         DC    AL4(8900)           C9                                           
*                                                                               
         DC    AL2(341,80,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(6600)           ON CAMERA                                    
         DC    AL4(5000)           OFF                                          
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(5000)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(6600)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(5000)           SE                                           
         DC    AL4(5000)           C3,C6                                        
         DC    AL4(5000)           C9                                           
*                                                                               
         DC    AL2(341,80,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(3700)           ON CAMERA                                    
         DC    AL4(2600)           OFF                                          
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(2600)                                                        
         DC    AL4(2600)                                                        
         DC    AL4(2600)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(2600)           SE                                           
         DC    AL4(2600)           C3,C6                                        
         DC    AL4(2600)           C9                                           
*                                                                               
         DC    AL2(390,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 2-WK            
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
         DC    AL4(5800)                                                        
*                                                                               
         DC    AL2(390,44,4,11,0,0)  UNITS 5-12  2-WK                           
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
         DC    AL4(4300)                                                        
*                                                                               
         DC    AL2(390,44,12,24,0,0)  UNITS 13-25  2-WK                         
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
         DC    AL4(2400)                                                        
*                                                                               
         DC    AL2(390,44,25,255,0,0)  UNITS 26+   2-WK                         
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
         DC    AL4(1400)                                                        
*                                                                               
         DC    AL2(343,44,1,3,0,0)  RADIO TAGS - REG, UNITS 1-4 13-WK           
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
         DC    AL4(7200)                                                        
*                                                                               
         DC    AL2(343,44,4,11,0,0)  UNITS 5-12  13-WK                          
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
         DC    AL4(5500)                                                        
*                                                                               
         DC    AL2(343,44,12,24,0,0)  UNITS 13-25  13-WK                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
         DC    AL4(3000)                                                        
*                                                                               
         DC    AL2(343,44,25,255,0,0)  UNITS 26+  13-WK                         
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
         DC    AL4(1700)                                                        
*                                                                               
         DC    AL2(344,44,1,24,0,0)  GA - ADO TAGS UNITS 1-24                   
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
         DC    AL4(9500)                                                        
*                                                                               
         DC    AL2(344,44,25,49,0,0)  GA - ADO TAGS UNITS 25-49                 
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
         DC    AL4(6800)                                                        
*                                                                               
         DC    AL2(344,44,50,255,0,0)  GA - ADO TAGS UNITS 50+                  
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
         DC    AL4(3700)                                                        
*                                                                               
         EJECT                                                                  
*              RATES FOR PROMOS                                                 
*                                                                               
         DC    AL2(250,84,0,0,0,0)  PRM FOR SAG AND AFT                         
         DC    AL4(32300)          ON CAMERA                                    
         DC    AL4(24000)          OFF                                          
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
         DC    AL4(9700)           N/D                                          
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
EQYTAB   DC    AL2(255,80,1,1,0,0)  EQUITY 6M  TV                               
         DC    AL4(66337)              PRINCIPAL ON  CAMERA                     
         DC    AL4(66337)                  "     OFF   "                        
         DC    AL4(66337)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(66337)                "    6-8    "                          
         DC    AL4(66337)                "     9+    "                          
         DC    AL4(66337)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(66337)                "    6-8    "                          
         DC    AL4(66337)                "     9+    "                          
         DC    AL4(66337)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(66337)              HAND MODEL UNLIMITED                     
         DC    AL4(66337)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(66337)              HAND MODEL 13 WEEKS                      
         DC    AL4(66337)    PIL       PILOT LOCATION RATE                      
         DC    AL4(66337)    PI        PILOT STUDIO RATE                        
         DC    AL4(66337)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(66337)    C3,C6     CONTRACTORS                              
         DC    AL4(66337)    C9             "                                   
*                                                                               
         DC    AL2(261,80,1,1,0,0)  EQUITY 1Y  TV                               
         DC    AL4(116380)             PRINCIPAL ON  CAMERA                     
         DC    AL4(116380)                 "     OFF   "                        
         DC    AL4(116380)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(116380)               "    6-8    "                          
         DC    AL4(116380)               "     9+    "                          
         DC    AL4(116380)             GROUPS 3-5 OFF CAMERA                    
         DC    AL4(116380)               "    6-8    "                          
         DC    AL4(116380)               "     9+    "                          
         DC    AL4(116380)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(116380)             HAND MODEL UNLIMITED                     
         DC    AL4(116380)             COMM'L EXTRA 13 WEEKS                    
         DC    AL4(116380)             HAND MODEL 13 WEEKS                      
         DC    AL4(116380)   PIL       PILOT LOCATION RATE                      
         DC    AL4(116380)   PI        PILOT STUDIO RATE                        
         DC    AL4(116380)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(116380)   C3,C6     CONTRACTORS                              
         DC    AL4(116380)   C9             "                                   
*                                                                               
         DC    AL2(264,44,1,1,0,0)  EQUITY 6M  RADIO                            
         DC    AL4(23613)              ANN ALONE                                
         DC    AL4(23613)              AR,AS,P,ANN,S,1-4MS,1-4SS                
         DC    AL4(23613)              1-4M3,1-4S3,D3,S3                        
         DC    AL4(23613)              1-4M6,1-4S6,D6,S6                        
         DC    AL4(23613)              1-4M9,1-4S9,D9,S9                        
         DC    AL4(23613)              SE                                       
         DC    AL4(23613)              C3,C6                                    
         DC    AL4(23613)              C9                                       
*                                                                               
         DC    AL2(268,44,1,1,0,0)  EQUITY 1Y  RADIO                            
         DC    AL4(39355)              ANN ALONE                                
         DC    AL4(39355)              AR,AS,P,ANN,S,1-4MS,1-4SS                
         DC    AL4(39355)              1-4M3,1-4S3,D3,S3                        
         DC    AL4(39355)              1-4M6,1-4S6,D6,S6                        
         DC    AL4(39355)              1-4M9,1-4S9,D9,S9                        
         DC    AL4(39355)              SE                                       
         DC    AL4(39355)              C3,C6                                    
         DC    AL4(39355)              C9                                       
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
         DC    AL2(69),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(UMVI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(UMVI,UMVII4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(78),AL1(UMVI,UMVII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(77),AL1(UMVI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(UMVN,UMVNI4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(69),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(76),AL1(UMVN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(UMVN,UMVNI4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(78),AL1(UMVN,UMVNI8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(77),AL1(UMVN,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(USMI,USMII4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(69),AL1(USMI,USMII8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMI,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(USMI,USMII4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(62),AL1(USMI,USMII8W,ALL-AFM,0,0,0,RADIO)                    
         DC    AL2(86),AL1(USMI,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(540),AL1(USMN,USMNI4W,ALL-AFM,0,0,0,LIKETV)                  
         DC    AL2(69),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USMN,ALL,ALL-AFM,0,0,0,LIKETV)                       
         DC    AL2(541),AL1(USMN,USMNI4W,ALL-AFM,0,0,0,RADIO)                   
         DC    AL2(62),AL1(USMN,USMNI8W,ALL-AFM,0,0,0,RADIO)                    
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
         DC    AL2(85),AL1(USIU,USIUI1Y,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(85),AL1(USIU,USIUEXT,ALL-AFM,0,0,0,INTERNET)                 
         DC    AL2(542),AL1(USIU,USIUR4W,AFT+NON,0,0,0,INTERNET)                
         DC    AL2(62),AL1(USIU,USIUR8W,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(86),AL1(USIU,USIUR1Y,AFT+NON,0,0,0,INTERNET)                 
         DC    AL2(86),AL1(USIU,USIUREX,AFT+NON,0,0,0,INTERNET)                 
*                                                                               
         DC    AL2(540),AL1(USNU,USNUI4W,ALL-AFM,0,0,0,NEWMEDIA)                
         DC    AL2(69),AL1(USNU,USNUI8W,ALL-AFM,0,0,0,NEWMEDIA)                 
         DC    AL2(85),AL1(USNU,USNUI1Y,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(85),AL1(USNU,USNUEXT,ALL-AFM,0,0,0,LIKETV)                   
         DC    AL2(542),AL1(USNU,USNUR4W,AFT+NON,0,0,0,NEWMEDIA)                
         DC    AL2(62),AL1(USNU,USNUR8W,AFT+NON,0,0,0,NEWMEDIA)                 
         DC    AL2(86),AL1(USNU,USNUR1Y,AFT+NON,0,0,0,LIKETV)                   
         DC    AL2(86),AL1(USNU,USNUREX,AFT+NON,0,0,0,LIKETV)                   
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
         DC    AL2(500),AL1(UMVM,UMVM8WK,ALL,0,0,0,ALL)   MVM-8WK               
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
         DC    AL2(410),AL1(USFR,USFRA,ALL,0,0,0,LIKETV)     SP FGN A           
         DC    AL2(411),AL1(USFR,USFRB,ALL,0,0,0,LIKETV)     SP FGN B           
         DC    AL2(410),AL1(USFR,USFRC,ALL,0,0,0,LIKETV)     SP FGN C           
*                                                                               
         DC    AL2(62),AL1(UPBS,ALL,ALL-AFM,0,0,0,LIKETV) PUB SVC               
         DC    AL2(52),AL1(UPBS,ALL,ALL-AFM,0,0,0,RADIO)                        
*                                                                               
         DC    AL2(40),AL1(USNT,ALL,ALL,0,0,0,LIKETV)     SPAN NWK              
         DC    AL2(40),AL1(USNW,ALL,ALL,0,0,0,LIKETV)    SPAN N/W COMB          
         DC    AL2(10),AL1(USWS,ALL,ALL,0,0,0,LIKETV)     SPAN WSP              
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
****** USE NUMBERS 540-542 USE FOR INTERNET/NEWMEDIA 4WK TV/RAD                 
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
         DC    AL4(15200)          GA ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(11500)          GA ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(9500)           GA ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(9720)           KS ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(6750)           KS ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(4615)           KS ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(13980)          TX ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(10596)          TX ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(8700)           TX ADDENDUM RADIO SESSION TAG FEE            
         DC    AL4(11900)          NW ADDEN TV SESSION TAG FEE - ON CAM         
         DC    AL4(8900)           NW ADDEN TV SESSION TAG FEE - OFF            
         DC    AL4(5500)           NW ADDENDUM RADIO SESSION TAG FEE            
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
**********************************************************************          
* YEAR 1 (IF SIZE CHANGES (128 BYTES) UPDATE TASYSCALC)              *          
**********************************************************************          
INDEXT   DS    0F                                                               
         DC    AL4(11500,11500)    OFF CAMERA SESS - ADDT'L 1/2 HOUR            
*                                  DAY PERFORMER ROW                            
         DC    AL4(94950,17800,17800)     CEILING / OT / DT RATE                
*                                  3-DAY PERFORMER ROW                          
         DC    AL4(284900,17800,23750)    CEILING / OT / DT RATE                
*                                  WEEKLY PERFORMER ROW                         
         DC    AL4(316500,11900,15800)    CEILING / OT / DT RATE                
*                                                                               
         DC    AL4(48050,59750)    NARRATOR - ADDITIONAL DAYS                   
*                                                                               
         DC    AL4(11500,11500)    ENTIRE SCRIPT ADD'L 1/2HR                    
*                                                                               
         DC    AL4(21350,21350)    PARTIAL SCRIPT FIRST 1/2HR                   
*                                                                               
         DC    AL4(11500,11500)    ADDITIONAL 1/2HR FOR PRINCIPAL               
*                                                                               
         DC    AL4(6800,7550)      ADDITIONAL 1/2HR FOR NON-PRINCIPAL           
*                                                                               
         DC    AL4(21350,21350)    RETAKES FOR PRINCIPAL - 1/2HR RATE           
*                                                                               
         DC    AL4(6950,7700)      STEP OUT PREMIUM                             
*                                                                               
         DC    AL4(23500,25900)    P3M FIRST 1/2 HR RATE                        
*                                                                               
         DC    AL4(48050,59750)    PHD DAY RATE                                 
*                                                                               
         DC    AL4(10900,10900)    IVR ADD'L 1/2 HOUR RATE, CAT1 AND 2          
*                                                                               
         DC    XL4'FFFFFFFF'                                                    
**********************************************************************          
* YEAR 2  (+128 FROM INDEXT)                                         *          
**********************************************************************          
         DC    AL4(11750,11750)    OFF CAMERA SESS - ADDT'L 1/2 HOUR            
*                                  DAY PERFORMER ROW                            
         DC    AL4(96850,18150,18150)     CEILING / OT / DT RATE                
*                                  3-DAY PERFORMER ROW                          
         DC    AL4(290600,18150,24250)    CEILING / OT / DT RATE                
*                                  WEEKLY PERFORMER ROW                         
         DC    AL4(322850,12150,16100)    CEILING / OT / DT RATE                
*                                                                               
         DC    AL4(49000,60950)    NARRATOR - ADDITIONAL DAYS                   
*                                                                               
         DC    AL4(11750,11750)    ENTIRE SCRIPT ADD'L 1/2HR                    
*                                                                               
         DC    AL4(21800,21800)    PARTIAL SCRIPT FIRST 1/2HR                   
*                                                                               
         DC    AL4(11750,11750)    ADDITIONAL 1/2HR FOR PRINCIPAL               
*                                                                               
         DC    AL4(7100,7850)      ADDITIONAL 1/2HR FOR NON-PRINCIPAL           
*                                                                               
         DC    AL4(21800,21800)    RETAKES FOR PRINCIPAL - 1/2HR RATE           
*                                                                               
         DC    AL4(2750,2750)      STEP OUT PREMIUM                             
*                                                                               
         DC    AL4(23950,26400)    P3M FIRST 1/2 HR RATE                        
*                                                                               
         DC    AL4(49000,60950)    PHD DAY RATE                                 
*                                                                               
         DC    AL4(11100,11100)    IVR ADD'L 1/2 HOUR RATE, CAT1 AND 2          
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
**PAN#1  DC    CL21'002TAGENF6   07/12/16'                                      
         END                                                                    
