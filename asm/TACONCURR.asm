*          DATA SET TACONCURR  AT LEVEL 237 AS OF 11/02/16                      
*PHASE T00A8EC,*                                                                
         TITLE 'T00A8E - TABLES FOR 2016 CONTRACT'                              
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
         DC    AL4(67169)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(50504)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(49172)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(43538)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(36000)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(28483)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(24722)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(20159)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(0,44,2,2,0,0)   CLASS A USE 2                                
         DC    AL4(15711)                                                       
         DC    AL4(12295)                                                       
         DC    AL4(14559)                                                       
         DC    AL4(12464)                                                       
         DC    AL4(10205)                                                       
         DC    AL4(7902)                                                        
         DC    AL4(6865)                                                        
         DC    AL4(5632)                                                        
*                                                                               
         DC    AL2(0,44,3,3,0,0)   CLASS A USE 3                                
         DC    AL4(12464)                                                       
         DC    AL4(9779)                                                        
         DC    AL4(11400)                                                       
         DC    AL4(10330)                                                       
         DC    AL4(8447)                                                        
         DC    AL4(7383)                                                        
         DC    AL4(6319)                                                        
         DC    AL4(5162)                                                        
*                                                                               
         DC    AL2(0,44,4,13,0,0)  CLASS A USES 4-13                            
         DC    AL4(12464)                                                       
         DC    AL4(9779)                                                        
         DC    AL4(10761)                                                       
         DC    AL4(9692)                                                        
         DC    AL4(7934)                                                        
         DC    AL4(6739)                                                        
         DC    AL4(5883)                                                        
         DC    AL4(4819)                                                        
*                                                                               
         DC    AL2(0,44,14,255,0,0)  CLASS A USES 14+                           
         DC    AL4(5975)                                                        
         DC    AL4(4442)                                                        
         DC    AL4(3716)                                                        
         DC    AL4(3160)                                                        
         DC    AL4(2565)                                                        
         DC    AL4(2690)                                                        
         DC    AL4(2527)                                                        
         DC    AL4(2095)                                                        
*                                                                               
PAXTAB   DC    AL2(75,44,1,255,0,0)   PAX USE                                   
*              CATEGORIES ARE SAME FOR ALL OTHER CLA'S, CLB'S, CBX, CLC         
         DC    AL4(2515)           'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(1883)           'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(1568)           'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(1332)           'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(1075)           'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(1140)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(1059)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(883)            'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
CLBTAB   DC    AL2(1,44,0,0,0,0)   CLASS B WITH NY                              
         DC    AL4(127089)                                                      
         DC    AL4(90891)                                                       
         DC    AL4(80940)                                                       
         DC    AL4(71572)                                                       
         DC    AL4(58513)                                                       
         DC    AL4(29826)                                                       
         DC    AL4(24861)                                                       
         DC    AL4(20319)                                                       
*                                                                               
CBXTAB   DC    AL2(2,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(103656)                                                      
         DC    AL4(71995)                                                       
         DC    AL4(80940)                                                       
         DC    AL4(71572)                                                       
         DC    AL4(58513)                                                       
         DC    AL4(29826)                                                       
         DC    AL4(24861)                                                       
         DC    AL4(20319)                                                       
*                                                                               
CLCTAB   DC    AL2(3,44,0,0,0,0)   CLASS C                                      
         DC    AL4(61771)                                                       
         DC    AL4(41184)                                                       
         DC    AL4(53532)                                                       
         DC    AL4(47578)                                                       
         DC    AL4(38905)                                                       
         DC    AL4(23722)                                                       
         DC    AL4(19747)                                                       
         DC    AL4(16194)                                                       
*                                                                               
NWKTBL   DC    AL2(58,44,1,1,0,0)  NWK (LNA,LNB,LNC) USE 1                      
         DC    AL4(36947)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(27777)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(27044)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(23947)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(19800)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(15670)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(13600)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(11091)          'OFF' 1-4M9,1-4S9,D9,S9                      
         EJECT                                                                  
*              DEALER CYCLE TABLES                                              
*                                                                               
DANTAB   DC    AL2(4,44,0,0,0,0)   DEALER A INCL NY                             
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(252071)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(175571)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(189272)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(166840)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(129668)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(77377)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(67763)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(48380)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB   DC    AL2(5,44,0,0,0,0)   DEALER A W/O NY                              
         DC    AL4(222929)                                                      
         DC    AL4(161003)                                                      
         DC    AL4(189272)                                                      
         DC    AL4(166840)                                                      
         DC    AL4(129668)                                                      
         DC    AL4(77377)                                                       
         DC    AL4(67763)                                                       
         DC    AL4(48380)                                                       
*                                                                               
DBNTAB   DC    AL2(6,44,0,0,0,0)   CLASS B INCL NY                              
         DC    AL4(387570)                                                      
         DC    AL4(263728)                                                      
         DC    AL4(287766)                                                      
         DC    AL4(253676)                                                      
         DC    AL4(197426)                                                      
         DC    AL4(117877)                                                      
         DC    AL4(103169)                                                      
         DC    AL4(73589)                                                       
*                                                                               
DBXTAB   DC    AL2(7,44,0,0,0,0)   CLASS B W/O NY                               
         DC    AL4(334396)                                                      
         DC    AL4(241135)                                                      
         DC    AL4(287766)                                                      
         DC    AL4(253676)                                                      
         DC    AL4(197426)                                                      
         DC    AL4(117877)                                                      
         DC    AL4(103169)                                                      
         DC    AL4(73589)                                                       
*                                                                               
DANTAB8  DC    AL2(210,44,0,0,0,0) DEALER A INCL NY 8 WEEKS                     
*              CATEGORIES ARE SAME FOR DAX, DBM, DBX(SAME AS CLA, ETC.)         
         DC    AL4(126036)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(87786)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(94636)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(83420)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(64834)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(38689)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(33882)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(24190)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
DAXTAB8  DC    AL2(211,44,0,0,0,0) DEALER A W/O NY 8 WEEKS                      
         DC    AL4(111465)                                                      
         DC    AL4(80502)                                                       
         DC    AL4(94636)                                                       
         DC    AL4(83420)                                                       
         DC    AL4(64834)                                                       
         DC    AL4(38689)                                                       
         DC    AL4(33882)                                                       
         DC    AL4(24190)                                                       
*                                                                               
DBNTAB8  DC    AL2(212,44,0,0,0,0) CLASS B INCL NY 8 WEEKS                      
         DC    AL4(193785)                                                      
         DC    AL4(131865)                                                      
         DC    AL4(143885)                                                      
         DC    AL4(126840)                                                      
         DC    AL4(98715)                                                       
         DC    AL4(58940)                                                       
         DC    AL4(51585)                                                       
         DC    AL4(36795)                                                       
*                                                                               
DBXTAB8  DC    AL2(213,44,0,0,0,0) CLASS B W/O NY 8 WEEKS                       
         DC    AL4(167200)                                                      
         DC    AL4(120570)                                                      
         DC    AL4(143885)                                                      
         DC    AL4(126840)                                                      
         DC    AL4(98715)                                                       
         DC    AL4(58940)                                                       
         DC    AL4(51585)                                                       
         DC    AL4(36795)                                                       
         EJECT                                                                  
*              CLASS A GUARANTEED USAGE                                         
*                                                                               
G13TAB   DC    AL2(8,44,1,1,0,0)   13 USE                                       
         DC    AL4(192321)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(148796)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(159192)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(142177)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(116785)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(96769)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(84218)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(68900)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(8,12,2,13,0,0)   (2-13)                                      
*                                                                               
         DC    AL2(8,44,14,18,0,0)  14-18                                       
         DC    AL4(11782)                                                       
         DC    AL4(8948)                                                        
         DC    AL4(8613)                                                        
         DC    AL4(7537)                                                        
         DC    AL4(6156)                                                        
         DC    AL4(5691)                                                        
         DC    AL4(5130)                                                        
         DC    AL4(4228)                                                        
*                                                                               
         DC    AL2(8,44,19,255,0,0)  19+ (LOOK AT CLA, 14- EA)                  
         DC    AL4(5975)                                                        
         DC    AL4(4442)                                                        
         DC    AL4(3716)                                                        
         DC    AL4(3160)                                                        
         DC    AL4(2565)                                                        
         DC    AL4(2690)                                                        
         DC    AL4(2527)                                                        
         DC    AL4(2095)                                                        
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - TV                                 
*                                                                               
WSPTAB   DC    AL2(10,44,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(67169)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(50504)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(49172)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(43538)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(36000)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(28483)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(24722)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(20159)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(10,44,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(2299)                                                        
         DC    AL4(1573)                                                        
         DC    AL4(1792)                                                        
         DC    AL4(1546)                                                        
         DC    AL4(1262)                                                        
         DC    AL4(635)                                                         
         DC    AL4(501)                                                         
         DC    AL4(417)                                                         
*                                                                               
         DC    AL2(10,44,26,60,0,0)  UNITS 26-60                                
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(924)                                                         
         DC    AL4(783)                                                         
         DC    AL4(649)                                                         
         DC    AL4(268)                                                         
         DC    AL4(184)                                                         
         DC    AL4(168)                                                         
*                                                                               
         DC    AL2(10,44,61,125,0,0)  UNITS 61-125                              
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(669)                                                         
         DC    AL4(523)                                                         
         DC    AL4(438)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
*                                                                               
         DC    AL2(10,44,126,255,0,0)  UNITS 126+                               
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(331)                                                         
         DC    AL4(268)                                                         
         DC    AL4(234)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - NEW YORK - TV                                  
*                                                                               
         DC    AL2(11,44,0,0,0,0)  NY ALONE                                     
         DC    AL4(132001)                                                      
         DC    AL4(93256)                                                       
         DC    AL4(84530)                                                       
         DC    AL4(75087)                                                       
         DC    AL4(61525)                                                       
         DC    AL4(33914)                                                       
         DC    AL4(28098)                                                       
         DC    AL4(23005)                                                       
                                                                                
         DC    AL2(11,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(924)                                                         
         DC    AL4(783)                                                         
         DC    AL4(649)                                                         
         DC    AL4(268)                                                         
         DC    AL4(184)                                                         
         DC    AL4(168)                                                         
*                                                                               
         DC    AL2(11,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(669)                                                         
         DC    AL4(523)                                                         
         DC    AL4(438)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
*                                                                               
         DC    AL2(11,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(331)                                                         
         DC    AL4(268)                                                         
         DC    AL4(234)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - CHICAGO OR LA - TV                             
*                                                                               
         DC    AL2(12,44,0,0,0,0)  CHI OR LA ALONE                              
         DC    AL4(115057)                                                      
         DC    AL4(81149)                                                       
         DC    AL4(84530)                                                       
         DC    AL4(75087)                                                       
         DC    AL4(61525)                                                       
         DC    AL4(33914)                                                       
         DC    AL4(28098)                                                       
         DC    AL4(23005)                                                       
*                                                                               
         DC    AL2(12,44,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(924)                                                         
         DC    AL4(783)                                                         
         DC    AL4(649)                                                         
         DC    AL4(268)                                                         
         DC    AL4(184)                                                         
         DC    AL4(168)                                                         
*                                                                               
         DC    AL2(12,44,36,100,0,0)  UNITS 36-100                              
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(669)                                                         
         DC    AL4(523)                                                         
         DC    AL4(438)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
*                                                                               
         DC    AL2(12,44,101,255,0,0)  UNITS 101+                               
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(331)                                                         
         DC    AL4(268)                                                         
         DC    AL4(234)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - TV                       
*                                                                               
         DC    AL2(13,44,0,0,0,0)  TWO OF NY LA CHI                             
         DC    AL4(181654)                                                      
         DC    AL4(122312)                                                      
         DC    AL4(130064)                                                      
         DC    AL4(107540)                                                      
         DC    AL4(87922)                                                       
         DC    AL4(44812)                                                       
         DC    AL4(36096)                                                       
         DC    AL4(29553)                                                       
*                                                                               
         DC    AL2(13,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(853)                                                         
         DC    AL4(669)                                                         
         DC    AL4(331)                                                         
         DC    AL4(268)                                                         
         DC    AL4(234)                                                         
         DC    AL4(162)                                                         
         DC    AL4(92)                                                          
         DC    AL4(92)                                                          
*                                                                               
         DC    AL2(14,44,0,0,0,0)  ALL THREE MAJORS                             
         DC    AL4(219109)                                                      
         DC    AL4(155626)                                                      
         DC    AL4(164085)                                                      
         DC    AL4(140427)                                                      
         DC    AL4(114779)                                                      
         DC    AL4(54030)                                                       
         DC    AL4(43570)                                                       
         DC    AL4(35604)                                                       
*                                                                               
         DC    AL2(14,44,1,255,0,0)  UNITS 1+                                   
         DC    AL4(874)                                                         
         DC    AL4(685)                                                         
         DC    AL4(339)                                                         
         DC    AL4(274)                                                         
         DC    AL4(239)                                                         
         DC    AL4(168)                                                         
         DC    AL4(100)                                                         
         DC    AL4(100)                                                         
         EJECT                                                                  
*              WILDSPOT TABLES - NO MAJORS - RADIO                              
*                                                                               
         DC    AL2(15,36,1,1,0,0)  UNIT 1                                       
*              SAME CATEGORIES FOR ALL RADIO WILDSPOTS (ENTRIES 15-19)          
         DC    AL4(29810)          ANN ALONE                                    
         DC    AL4(29810)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(21962)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(19437)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(17238)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(10026)          SE (ONLY GETS PAID FOR FIRST UNIT)           
*                                                                               
         DC    AL2(15,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(439)                                                         
         DC    AL4(439)                                                         
         DC    AL4(228)                                                         
         DC    AL4(195)                                                         
         DC    AL4(172)                                                         
*                                                                               
         DC    AL2(15,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(195)                                                         
         DC    AL4(150)                                                         
         DC    AL4(150)                                                         
*                                                                               
         DC    AL2(15,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(110)                                                         
         DC    AL4(95)                                                          
         DC    AL4(95)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(16,36,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(44635)                                                       
         DC    AL4(44635)                                                       
         DC    AL4(24273)                                                       
         DC    AL4(21550)                                                       
         DC    AL4(19126)                                                       
         DC    AL4(10026)                                                       
*                                                                               
         DC    AL2(16,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(195)                                                         
         DC    AL4(164)                                                         
         DC    AL4(156)                                                         
*                                                                               
         DC    AL2(16,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(110)                                                         
         DC    AL4(95)                                                          
         DC    AL4(95)                                                          
*                                                                               
         DC    AL2(17,36,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(40483)                                                       
         DC    AL4(40483)                                                       
         DC    AL4(24273)                                                       
         DC    AL4(21550)                                                       
         DC    AL4(19126)                                                       
         DC    AL4(10026)                                                       
*                                                                               
         DC    AL2(17,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(195)                                                         
         DC    AL4(164)                                                         
         DC    AL4(156)                                                         
*                                                                               
         DC    AL2(17,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(110)                                                         
         DC    AL4(95)                                                          
         DC    AL4(95)                                                          
         EJECT                                                                  
*              WILDSPOT TABLES - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(18,36,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(54442)                                                       
         DC    AL4(54442)                                                       
         DC    AL4(28992)                                                       
         DC    AL4(22240)                                                       
         DC    AL4(19790)                                                       
         DC    AL4(10026)                                                       
*                                                                               
         DC    AL2(18,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(164)                                                         
         DC    AL4(164)                                                         
         DC    AL4(156)                                                         
*                                                                               
         DC    AL2(18,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(110)                                                         
         DC    AL4(95)                                                          
         DC    AL4(95)                                                          
*                                                                               
         DC    AL2(19,36,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(68790)                                                       
         DC    AL4(68790)                                                       
         DC    AL4(32298)                                                       
         DC    AL4(24990)                                                       
         DC    AL4(22240)                                                       
         DC    AL4(10026)                                                       
*                                                                               
         DC    AL2(19,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(164)                                                         
         DC    AL4(164)                                                         
         DC    AL4(156)                                                         
*                                                                               
         DC    AL2(19,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(330)                                                         
         DC    AL4(330)                                                         
         DC    AL4(110)                                                         
         DC    AL4(95)                                                          
         DC    AL4(95)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT(WS8) - NO MAJORS - RADIO                         
*                                                                               
         DC    AL2(20,32,1,1,0,0)   UNIT 1                                      
*              SAME CATEGORIES FOR ALL WS8 WILDSPOTS (ENTRIES 20-24)            
         DC    AL1(100),AL3(29810) ANN ALONE                                    
         DC    AL1(100),AL3(29810) AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL1(100),AL3(21962) 1-4M3,1-4S3,D3,S3                            
         DC    AL1(100),AL3(19437) 1-4M6,1-4S6,D6,S6                            
         DC    AL1(100),AL3(17238) 1-4M9,1-4S9,D9,S9                            
*                                                                               
         DC    AL2(20,32,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(351)                                                         
         DC    AL4(351)                                                         
         DC    AL4(217)                                                         
         DC    AL4(185)                                                         
         DC    AL4(164)                                                         
*                                                                               
         DC    AL2(20,32,26,60,0,0)  UNITS 26-60                                
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(185)                                                         
         DC    AL4(142)                                                         
         DC    AL4(142)                                                         
*                                                                               
         DC    AL2(20,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(105)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT - ONE MAJOR - RADIO                              
*                                                                               
         DC    AL2(21,32,0,0,0,0)  NEW YORK ALONE                               
         DC    AL4(35706)                                                       
         DC    AL4(35706)                                                       
         DC    AL4(23059)                                                       
         DC    AL4(20474)                                                       
         DC    AL4(18169)                                                       
*                                                                               
         DC    AL2(21,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(185)                                                         
         DC    AL4(156)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(21,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(105)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
*                                                                               
         DC    AL2(22,32,0,0,0,0)  CHICAGO OR LA ALONE                          
         DC    AL4(32389)                                                       
         DC    AL4(32389)                                                       
         DC    AL4(23059)                                                       
         DC    AL4(20474)                                                       
         DC    AL4(18169)                                                       
*                                                                               
         DC    AL2(22,32,1,35,0,0)  UNITS 1-35                                  
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(185)                                                         
         DC    AL4(156)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(22,32,36,255,0,0)  UNITS 36+                                 
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(105)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
         EJECT                                                                  
*              8 WEEK WILDSPOT - TWO OR THREE MAJORS - RADIO                    
*                                                                               
         DC    AL2(23,32,0,0,0,0)  ANY TWO ALONE                                
         DC    AL4(43554)                                                       
         DC    AL4(43554)                                                       
         DC    AL4(27542)                                                       
         DC    AL4(21127)                                                       
         DC    AL4(18800)                                                       
*                                                                               
         DC    AL2(23,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(156)                                                         
         DC    AL4(156)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(23,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(105)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
*                                                                               
         DC    AL2(24,32,0,0,0,0)  ALL THREE ALONE                              
         DC    AL4(55030)                                                       
         DC    AL4(55030)                                                       
         DC    AL4(30682)                                                       
         DC    AL4(23738)                                                       
         DC    AL4(21127)                                                       
*                                                                               
         DC    AL2(24,32,1,60,0,0)  UNITS 1-60                                  
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(156)                                                         
         DC    AL4(156)                                                         
         DC    AL4(148)                                                         
*                                                                               
         DC    AL2(24,32,61,255,0,0)  UNITS 61+                                 
         DC    AL4(264)                                                         
         DC    AL4(264)                                                         
         DC    AL4(105)                                                         
         DC    AL4(90)                                                          
         DC    AL4(90)                                                          
         EJECT                                                                  
*              DEALER AND NETWORK TABLES - RADIO                                
*                                                                               
DLRTAB   DC    AL2(25,36,0,0,0,0)  DEALER COMMERCIALS                           
         DC    AL4(80619)          AR,AS,P,ANN                                  
         DC    AL4(63954)          S,1-4MS,1-4SS                                
         DC    AL4(41693)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(33363)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(20854)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(21090)          SE                                           
*                                                                               
DLRTAB8  DC    AL2(214,36,0,0,0,0)  DEALER COMMERCIALS 8 WEEKS                  
         DC    AL4(40307)          AR,AS,P,ANN                                  
         DC    AL4(31977)          S,1-4MS,1-4SS                                
         DC    AL4(20849)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(16681)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(10427)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(10545)          SE                                           
*                                                                               
N01TAB   DC    AL2(26,36,0,0,0,0)  NETWORK 1 WEEK                               
*              CATEGORIES SAME FOR DLR,N01,N04,N13,NAB,U26,U39,R13              
         DC    AL4(50445)          ANN ALONE                                    
         DC    AL4(50445)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(37857)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(37857)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(37857)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(13643)          SE                                           
*                                                                               
N04TAB   DC    AL2(27,36,0,0,0,0)  NETWORK 4 WEEK                               
         DC    AL4(81839)                                                       
         DC    AL4(81839)                                                       
         DC    AL4(62937)                                                       
         DC    AL4(56277)                                                       
         DC    AL4(51414)                                                       
         DC    AL4(13643)                                                       
*                                                                               
N08TAB   DC    AL2(28,36,0,0,0,0)  NETWORK 8 WEEK                               
         DC    AL4(130363)                                                      
         DC    AL4(130363)                                                      
         DC    AL4(100302)                                                      
         DC    AL4(89602)                                                       
         DC    AL4(80304)                                                       
         DC    AL4(13643)                                                       
*                                                                               
N13TAB   DC    AL2(29,36,0,0,0,0)  NETWORK 13 WEEK                              
         DC    AL4(161773)                                                      
         DC    AL4(161773)                                                      
         DC    AL4(124425)                                                      
         DC    AL4(111253)                                                      
         DC    AL4(101923)                                                      
         DC    AL4(13643)                                                       
         EJECT                                                                  
*              NETWORK AND REGIONAL NETWORK - RADIO                             
*                                                                               
NABTAB   DC    AL2(30,36,0,0,0,0)  ACROSS-THE-BOARD                             
         DC    AL4(169397)                                                      
         DC    AL4(169397)                                                      
         DC    AL4(130267)                                                      
         DC    AL4(116496)                                                      
         DC    AL4(106727)                                                      
         DC    AL4(13643)                                                       
*                                                                               
U26TAB   DC    AL2(31,36,0,0,0,0)  26 USE LIMIT                                 
         DC    AL4(80892)                                                       
         DC    AL4(80892)                                                       
         DC    AL4(62199)                                                       
         DC    AL4(55619)                                                       
         DC    AL4(50825)                                                       
         DC    AL4(13643)                                                       
*                                                                               
U39TAB   DC    AL2(32,36,0,0,0,0)  39 USE LIMIT                                 
         DC    AL4(121820)                                                      
         DC    AL4(121820)                                                      
         DC    AL4(85295)                                                       
         DC    AL4(76141)                                                       
         DC    AL4(69170)                                                       
         DC    AL4(13643)                                                       
*                                                                               
R13TAB   DC    AL2(33,36,0,0,0,0)  REGIONAL - NO MAJORS                         
         DC    AL4(97621)                                                       
         DC    AL4(97621)                                                       
         DC    AL4(45759)                                                       
         DC    AL4(45759)                                                       
         DC    AL4(45759)                                                       
         DC    AL4(13643)                                                       
*                                                                               
         DC    AL2(34,36,0,0,0,0)  REGIONAL - WITH ANY MAJORS                   
         DC    AL4(97621)                                                       
         DC    AL4(97621)                                                       
         DC    AL4(97621)                                                       
         DC    AL4(87863)                                                       
         DC    AL4(79020)                                                       
         DC    AL4(13643)                                                       
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
         DC    AL4(29810)          ANN ALONE                                    
         DC    AL4(29810)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(21962)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(19437)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(17238)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(22925)          SE                                           
         DC    AL4(10235)          C3,C6                                        
         DC    AL4(16371)          C9                                           
*                                                                               
         DC    AL2(62,96,1,255,0,0)  NON-AFM TV BASE SESSION RATES              
         DC    AL4(67169)              PRINCIPAL ON  CAMERA                     
         DC    AL4(50504)                  "     OFF   "                        
         DC    AL4(49172)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(43538)                "    6-8    "                          
         DC    AL4(36000)                "     9+    "                          
         DC    AL4(28483)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(24722)                "    6-8    "                          
         DC    AL4(20159)                "     9+    "                          
         DC    AL4(36637)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(55902)              HAND MODEL UNLIMITED                     
         DC    AL4(21266)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(37183)              HAND MODEL 13 WEEKS                      
         DC    AL4(103458)   PIL       PILOT LOCATION RATE                      
         DC    AL4(79549)    PI        PILOT STUDIO RATE                        
         DC    AL4(35588)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(10770)    C3,C6     CONTRACTORS                              
         DC    AL4(21240)    C9             "                                   
         DC    2AL4(0)                 SOLO/DUO                                 
         DC    AL4(40301)    ESB       STAND-IN UNLIMITED                       
         DC    AL4(23393)    ESI       STAND-IN EXTRA                           
*                                                                               
         DC    AL2(256,80,1,255,0,0)  NON-AFM TV BASE SESSION RATES             
         DC    AL1(50),AL3(67169)                     FOR FGR EXTENSION         
         DC    AL1(50),AL3(50504)                                               
         DC    AL1(50),AL3(49172)                                               
         DC    AL1(50),AL3(43538)                                               
         DC    AL1(50),AL3(36000)                                               
         DC    AL1(50),AL3(28483)                                               
         DC    AL1(50),AL3(24722)                                               
         DC    AL1(50),AL3(20159)                                               
         DC    AL1(50),AL3(36637)                                               
         DC    AL1(50),AL3(55902)                                               
         DC    AL1(50),AL3(21266)                                               
         DC    AL1(50),AL3(37183)                                               
         DC    AL1(50),AL3(103458)         PIL                                  
         DC    AL1(50),AL3(79549)          PI                                   
         DC    AL1(50),AL3(35588)          SE                                   
         DC    AL1(50),AL3(10770)          C3,C6                                
         DC    AL1(50),AL3(21240)          C9                                   
*                                                                               
         DC    AL2(64,96,1,255,0,0)  NON-AFM CABLE BASE SESSION RATES           
         DC    AL4(67169)                                                       
         DC    AL4(50504)                                                       
         DC    AL4(49172)                                                       
         DC    AL4(43538)                                                       
         DC    AL4(36000)                                                       
         DC    AL4(28483)                                                       
         DC    AL4(24722)                                                       
         DC    AL4(20159)                                                       
         DC    AL4(36637)                                                       
         DC    AL4(55902)                                                       
         DC    AL4(21266)                                                       
         DC    AL4(37183)                                                       
         DC    AL4(103458)         PIL                                          
         DC    AL4(79549)          PI                                           
         DC    AL4(35588)          SE                                           
         DC    AL4(10770)          C3,C6                                        
         DC    AL4(21240)          C9                                           
         DC    2AL4(0)                     SOLO/DUO                             
         DC    AL4(40301)          ESB     STAND-IN UNLIMITED                   
         DC    AL4(23393)          ESI     STAND-IN EXTRA                       
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
         DC    AL4(67169)                                                       
         DC    AL4(50504)                                                       
         DC    AL4(49172)                                                       
         DC    AL4(43538)                                                       
         DC    AL4(36000)                                                       
*                                                                               
         DC    AL2(37,96,0,0,0,0)  TV POSTPONEMENT FEE RATES - 1/2 SESS         
         DC    AL1(50),AL3(67169)                                               
         DC    AL1(50),AL3(50504)                                               
         DC    AL1(50),AL3(49172)                                               
         DC    AL1(50),AL3(43538)                                               
         DC    AL1(50),AL3(36000)                                               
         DC    AL1(50),AL3(28483)                                               
         DC    AL1(50),AL3(24722)                                               
         DC    AL1(50),AL3(20159)                                               
         DC    AL1(50),AL3(36637)                                               
         DC    AL1(50),AL3(55902)                                               
         DC    AL1(50),AL3(21266)                                               
         DC    AL1(50),AL3(37183)                                               
         DC    AL1(50),AL3(103458) PIL                                          
         DC    AL1(50),AL3(79549)  PI                                           
         DC    AL1(50),AL3(35588)  SE                                           
         DC    AL1(50),AL3(10770)  C3,C6                                        
         DC    AL1(50),AL3(21240)  C9                                           
         DC    AL1(50),AL3(0)              SOLO/DUO                             
         DC    AL1(50),AL3(0)              SOLO/DUO                             
         DC    AL1(50),AL3(40301)  ESB     STAND-IN UNLIMITED                   
         DC    AL1(50),AL3(23393)  ESI     STAND-IN EXTRA                       
*                                                                               
         DC    AL2(38,60,0,0,0,0)  REN - REINSTATEMENT-2X SESSION RATE          
         DC    AL1(200),AL3(67169)                                              
         DC    AL1(200),AL3(50504)                                              
         DC    AL1(200),AL3(49172)                                              
         DC    AL1(200),AL3(43538)                                              
         DC    AL1(200),AL3(36000)                                              
         DC    5AL4(0)                                                          
         DC    AL1(200),AL3(21266)                                              
         DC    AL1(200),AL3(37183)                                              
*                                                                               
         DC    AL2(69,80,1,255,0,0)  INTERNET, 8 WEEK (1.50X)                   
         DC    AL4(100754)             PRINCIPAL ON  CAMERA                     
         DC    AL4(75756)                  "     OFF   "                        
         DC    AL4(73758)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(65307)                "    6-8    "                          
         DC    AL4(54000)                "     9+    "                          
         DC    AL4(42725)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(37083)                "    6-8    "                          
         DC    AL4(30239)                "     9+    "                          
         DC    AL4(54956)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(83853)              HAND MODEL UNLIMITED                     
         DC    AL4(31899)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(55775)              HAND MODEL 13 WEEKS                      
         DC    AL4(155187)   PIL       PILOT LOCATION RATE                      
         DC    AL4(119324)   PI        PILOT STUDIO RATE                        
         DC    AL4(53382)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(16155)    C3,C6     CONTRACTORS                              
         DC    AL4(31860)    C9             "                                   
*                                                                               
         DC    AL2(87,80,1,255,0,0)  MOVE TO INTERNET, 8 WEEK (1.75X)           
         DC    AL4(117546)             PRINCIPAL ON  CAMERA                     
         DC    AL4(88382)                  "     OFF   "                        
         DC    AL4(86051)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(76192)                "    6-8    "                          
         DC    AL4(63000)                "     9+    "                          
         DC    AL4(49845)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(43264)                "    6-8    "                          
         DC    AL4(35278)                "     9+    "                          
         DC    AL4(64115)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(97829)              HAND MODEL UNLIMITED                     
         DC    AL4(37216)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(65070)              HAND MODEL 13 WEEKS                      
         DC    AL4(181051)   PIL       PILOT LOCATION RATE                      
         DC    AL4(139211)   PI        PILOT STUDIO RATE                        
         DC    AL4(62279)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(18848)    C3,C6     CONTRACTORS                              
         DC    AL4(37170)    C9             "                                   
*                                                                               
         DC    AL2(76,80,1,255,0,0)    INTERNET TV  (3.75X SESSION)             
         DC    AL4(251884)             PRINCIPAL ON  CAMERA                     
         DC    AL4(189390)                 "     OFF   "                        
         DC    AL4(184395)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(163268)               "    6-8    "                          
         DC    AL4(135000)               "     9+    "                          
         DC    AL4(106811)             GROUPS 3-5 OFF CAMERA                    
         DC    AL4(92708)                "    6-8    "                          
         DC    AL4(75596)                "     9+    "                          
         DC    AL4(137389)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(209632)             HAND MODEL UNLIMITED                     
         DC    AL4(79748)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(139436)             HAND MODEL 13 WEEKS                      
         DC    AL4(387967)   PIL       PILOT LOCATION RATE                      
         DC    AL4(298309)   PI        PILOT STUDIO RATE                        
         DC    AL4(133455)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(40388)    C3,C6     CONTRACTORS                              
         DC    AL4(79650)    C9             "                                   
*                                                                               
         DC    AL2(78,80,1,255,0,0)  INTERNET RADIO (1.33X SESSION)             
         DC    AL4(39647)          ANN ALONE                                    
         DC    AL4(39647)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(29209)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(25851)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(22927)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(30490)          SE                                           
         DC    AL4(13613)          C3,C6                                        
         DC    AL4(21773)          C9                                           
*                                                                               
         DC    AL2(88,44,1,255,0,0)  MOVE TO INTERNET RADIO (1.5X SESS)         
         DC    AL4(44715)          ANN ALONE                                    
         DC    AL4(44715)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(32943)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(29156)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(25857)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(34388)          SE                                           
         DC    AL4(15352)          C3,C6                                        
         DC    AL4(24557)          C9                                           
*                                                                               
         DC    AL2(77,80,1,255,0,0)    INTERNET RADIO (3.5X SESSION)            
         DC    AL4(104335)         ANN ALONE                                    
         DC    AL4(104335)         AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(76867)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(68030)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(60333)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(80238)          SE                                           
         DC    AL4(35822)          C3,C6                                        
         DC    AL4(57299)          C9                                           
*                                                                               
         DC    AL2(85,80,1,255,0,0)    MOVE TO INTERNET TV (4.25X SESS)         
         DC    AL4(285468)             PRINCIPAL ON  CAMERA                     
         DC    AL4(214642)                 "     OFF   "                        
         DC    AL4(208981)             GROUPS 3-5 ON CAMERA                     
         DC    AL4(185037)               "    6-8    "                          
         DC    AL4(153000)               "     9+    "                          
         DC    AL4(121053)             GROUPS 3-5 OFF CAMERA                    
         DC    AL4(105069)               "    6-8    "                          
         DC    AL4(85676)                "     9+    "                          
         DC    AL4(155707)             COMM'L EXTRA UNLIMITED                   
         DC    AL4(237584)             HAND MODEL UNLIMITED                     
         DC    AL4(90381)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(158028)             HAND MODEL 13 WEEKS                      
         DC    AL4(439697)   PIL       PILOT LOCATION RATE                      
         DC    AL4(338083)   PI        PILOT STUDIO RATE                        
         DC    AL4(151249)   SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(45773)    C3,C6     CONTRACTORS                              
         DC    AL4(90270)    C9             "                                   
*                                                                               
         DC    AL2(86,44,1,255,0,0)    MOVE TO INTERNET RADIO (4X SESS)         
         DC    AL4(119240)         ANN ALONE                                    
         DC    AL4(119240)         AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(87848)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(77748)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(68952)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(91700)          SE                                           
         DC    AL4(40940)          C3,C6                                        
         DC    AL4(65484)          C9                                           
*                                                                               
         DC    AL2(540,80,1,255,0,0)  MOVE TO INTERNET, 4 WEEK (1.25X)          
         DC    AL4(83961)              PRINCIPAL ON  CAMERA                     
         DC    AL4(63130)                  "     OFF   "                        
         DC    AL4(61465)              GROUPS 3-5 ON CAMERA                     
         DC    AL4(54423)                "    6-8    "                          
         DC    AL4(45000)                "     9+    "                          
         DC    AL4(35604)              GROUPS 3-5 OFF CAMERA                    
         DC    AL4(30903)                "    6-8    "                          
         DC    AL4(25199)                "     9+    "                          
         DC    AL4(45796)              COMM'L EXTRA UNLIMITED                   
         DC    AL4(69878)              HAND MODEL UNLIMITED                     
         DC    AL4(26583)              COMM'L EXTRA 13 WEEKS                    
         DC    AL4(46479)              HAND MODEL 13 WEEKS                      
         DC    AL4(129322)   PIL       PILOT LOCATION RATE                      
         DC    AL4(99436)    PI        PILOT STUDIO RATE                        
         DC    AL4(44485)    SE        SOUND EFFECTS, AFTRA                     
         DC    AL4(13463)    C3,C6     CONTRACTORS                              
         DC    AL4(26550)    C9             "                                   
*                                                                               
         DC    AL2(541,44,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(37263)          ANN ALONE                                    
         DC    AL4(37263)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    AL4(27453)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(24296)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(21548)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(28656)          SE                                           
         DC    AL4(12794)          C3,C6                                        
         DC    AL4(20464)          C9                                           
*                                                                               
         DC    AL2(542,80,1,255,0,0)    MOVE TO INT RADIO 4WK (1.25X)           
         DC    AL4(37263)          ANN ALONE                                    
         DC    AL4(37263)          AR,AS,P,ANN,S,1-4MS,1-4SS                    
         DC    3AL4(0)             N/A                                          
         DC    AL4(27453)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(24296)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(21548)          1-4M9,1-4S9,D9,S9                            
         DC    6AL4(0)             N/A                                          
         DC    AL4(28656)          SE                                           
         DC    AL4(12794)          C3,C6                                        
         DC    AL4(20464)          C9                                           
*                                                                               
         DC    AL2(543,44,1,1,0,0)    SOCIAL MEDIA REUSE                        
         DC    AL4(10075)          PRINCIPAL ON CAMERA                          
         DC    AL4(7576)           PRINCIPAL OFF CAMERA                         
         DC    AL4(7376)           GROUPS 3-5 ON CAMERA                         
         DC    AL4(6531)             "    6-8    "                              
         DC    AL4(5400)             "     9+    "                              
         DC    AL4(4272)           GROUPS 3-5 OFF CAMERA                        
         DC    AL4(3708)             "    6-8    "                              
         DC    AL4(3024)             "     9+    "                              
*                                                                               
         EJECT                                                                  
*              CABLE RATES (YEAR 2016)                                          
         DC    AL2(41,44,1,1)      CBL & SCB - MINIMUM                          
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(67169)                                                       
         DC    AL4(50504)                                                       
         DC    AL4(49172)                                                       
         DC    AL4(43538)                                                       
         DC    AL4(36000)                                                       
         DC    AL4(28483)                                                       
         DC    AL4(24722)                                                       
         DC    AL4(20159)                                                       
*                                                                               
         DC    AL2(41,12,2,56)     MINIMUM COVERS UPTO 56                       
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(41,44,57,57)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(48)                                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(41,44,58,58)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(943)                                                         
         DC    AL4(0)                                                           
         DC    AL4(760)                                                         
         DC    AL4(554)                                                         
         DC    AL4(524)                                                         
         DC    AL4(371)                                                         
         DC    AL4(234)                                                         
         DC    AL4(279)                                                         
*                                                                               
         DC    AL2(41,44,59,65)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(1039)                                                        
         DC    AL4(0)                                                           
         DC    AL4(760)                                                         
         DC    AL4(674)                                                         
         DC    AL4(553)                                                         
         DC    AL4(438)                                                         
         DC    AL4(382)                                                         
         DC    AL4(311)                                                         
*                                                                               
         DC    AL2(41,44,66,66)                                                 
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(1039)                                                        
         DC    AL4(218)                                                         
         DC    AL4(760)                                                         
         DC    AL4(674)                                                         
         DC    AL4(553)                                                         
         DC    AL4(438)                                                         
         DC    AL4(382)                                                         
         DC    AL4(311)                                                         
*                                                                               
         DC    AL2(41,44,67,100)                                                
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(1039)                                                        
         DC    AL4(692)                                                         
         DC    AL4(760)                                                         
         DC    AL4(674)                                                         
         DC    AL4(553)                                                         
         DC    AL4(438)                                                         
         DC    AL4(382)                                                         
         DC    AL4(311)                                                         
*                                                                               
         DC    AL2(41,44,101,150)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(882)                                                         
         DC    AL4(587)                                                         
         DC    AL4(647)                                                         
         DC    AL4(570)                                                         
         DC    AL4(474)                                                         
         DC    AL4(375)                                                         
         DC    AL4(325)                                                         
         DC    AL4(264)                                                         
*                                                                               
         DC    AL2(41,44,151,200)                                               
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(725)                                                         
         DC    AL4(483)                                                         
         DC    AL4(533)                                                         
         DC    AL4(471)                                                         
         DC    AL4(391)                                                         
         DC    AL4(310)                                                         
         DC    AL4(267)                                                         
         DC    AL4(218)                                                         
*                                                                               
         DC    AL2(41,44,201,1000)                                              
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(85)                                                          
         DC    AL4(57)                                                          
         DC    AL4(64)                                                          
         DC    AL4(57)                                                          
         DC    AL4(47)                                                          
         DC    AL4(37)                                                          
         DC    AL4(32)                                                          
         DC    AL4(24)                                                          
*                                                                               
         DC    AL2(41,44,1001,2500)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(81)                                                          
         DC    AL4(55)                                                          
         DC    AL4(59)                                                          
         DC    AL4(52)                                                          
         DC    AL4(43)                                                          
         DC    AL4(35)                                                          
         DC    AL4(29)                                                          
         DC    AL4(24)                                                          
*                                                                               
         DC    AL2(41,44,2501,3000)                                             
         DC    AL1(0,0,0,0)                                                     
         DC    AL4(18)                                                          
         DC    AL4(12)                                                          
         DC    AL4(14)                                                          
         DC    AL4(11)                                                          
         DC    AL4(10)                                                          
         DC    AL4(08)                                                          
         DC    AL4(07)                                                          
         DC    AL4(06)                                                          
         EJECT                                                                  
*              NON-AIR DEMOS AND FOREIGN USEAGE                                 
*                                                                               
         DC    AL2(42,96,1,4,0,0)  DEM (TV)                                     
         DC    AL4(50380)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(25255)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(36880)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32655)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(27000)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(12760)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(12760)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(12760)          'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(21266)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(37183)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(10770)          C3,C6                                        
         DC    AL4(21240)          C9                                           
         DC    AL4(19522)          'OFF' SOLO/DUO                               
         DC    2AL4(0)             N/D                                          
         DC    AL4(23393)          STAND-IN 13 WEEKS                            
*                                                                               
         DC    AL2(42,96,5,255,0,0)  DEM (TV)                                   
         DC    AL4(50380)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(25255)          'OFF' AR,AS,P,ANN,D,1-4MS,1-4SS              
         DC    AL4(36880)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(32655)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(27000)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(3191)           'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(3191)           'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(3191)           'OFF' 1-4M9,1-4S9,D9,S9                      
         DC    2AL4(0)             N/D                                          
         DC    AL4(21266)          COMMERCIAL EXTRA 13 WEEKS                    
         DC    AL4(37183)          HAND MODEL 13 WEEKS                          
         DC    3AL4(0)             N/D                                          
         DC    AL4(10770)          C3,C6                                        
         DC    AL4(21240)          C9                                           
         DC    AL4(4880)           'OFF' SOLO/DUO                               
         DC    2AL4(0)             N/D                                          
         DC    AL4(23393)          STAND-IN 13 WEEKS                            
*                                                                               
         DC    AL2(43,48,1,4,0,0)  DEM (AFT RADIO)                              
         DC    AL4(20544)          ANN ALONE                                    
         DC    AL4(20544)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(13552)          1-4M3,1-4S3,D3,S3                            
         DC    AL4(13552)          1-4M6,1-4S6,D6,S6                            
         DC    AL4(13552)          1-4M9,1-4S9,D9,S9                            
         DC    AL4(22925)          SE                                           
         DC    AL4(10235)          C3,C6                                        
         DC    AL4(16371)          C9                                           
         DC    AL4(20731)          SOLOS AND DUOS                               
*                                                                               
         DC    AL2(43,48,5,255,0,0)  DEM (AFT RADIO)                            
         DC    AL4(20544)          ANN ALONE                                    
         DC    AL4(20544)          AR,AS,P,ANN,1-4MS,1-4SS                      
         DC    AL4(3387)           1-4M3,1-4S3,D3,S3                            
         DC    AL4(3387)           1-4M6,1-4S6,D6,S6                            
         DC    AL4(3387)           1-4M9,1-4S9,D9,S9                            
         DC    AL4(22925)          SE                                           
         DC    AL4(10235)          C3,C6                                        
         DC    AL4(16371)          C9                                           
         DC    AL4(5184)           SOLOS AND DUOS                               
         EJECT                                                                  
L13TAB   DC    AL2(39,36,0,0,0,0)  LOCAL 13 WEEK - RADIO                        
         DC    AL4(32400)                                                       
         DC    AL4(32400)                                                       
         DC    AL4(32400)                                                       
         DC    AL4(32400)                                                       
         DC    AL4(32400)                                                       
         DC    AL4(32400)                                                       
         EJECT                                                                  
*              FOREIGN REUSE                                                    
*                                                                               
         DC    AL2(50,80,0,0,0,0)  UK - 3X SESSION RATE (CAN'T USE MULT         
*                                  FACTOR, WON'T FIT IN AL1)                    
         DC    AL4(201507)         (3 X 67169)                                  
         DC    AL4(151512)                                                      
         DC    AL4(147516)                                                      
         DC    AL4(130614)                                                      
         DC    AL4(108000)                                                      
         DC    AL4(85449)                                                       
         DC    AL4(74166)                                                       
         DC    AL4(60477)                                                       
         DC    AL4(109911)                                                      
         DC    AL4(167706)                                                      
         DC    AL4(63798)                                                       
         DC    AL4(111549)                                                      
         DC    AL4(310374)         PIL                                          
         DC    AL4(238647)         PI                                           
         DC    AL4(131835)         SE                                           
         DC    AL4(32310)          C3,C6                                        
         DC    AL4(63720)          C9                                           
*                                                                               
         DC    AL2(51,80,0,0,0,0)  EUROPE W/O UK - 2X SESSION RATE              
         DC    AL4(134338)         (2 X 67169)                                  
         DC    AL4(101008)                                                      
         DC    AL4(98344)                                                       
         DC    AL4(87076)                                                       
         DC    AL4(72000)                                                       
         DC    AL4(56966)                                                       
         DC    AL4(49444)                                                       
         DC    AL4(40318)                                                       
         DC    AL4(73274)                                                       
         DC    AL4(111804)                                                      
         DC    AL4(42532)                                                       
         DC    AL4(74366)                                                       
         DC    AL4(206916)          PIL                                         
         DC    AL4(159098)          PI                                          
         DC    AL4(71176)           SE                                          
         DC    AL4(21540)           C3,C6                                       
         DC    AL4(42480)           C9                                          
*                                                                               
         DC    AL2(237,80,0,0,0,0)  WORLDWIDE - 9X SESSION RATE (CAN'T          
*                                 USE MULT FACTOR, WON'T FIT IN AL1)            
         DC    AL4(604521)        (9 X 67169) SINCE ASIA PACIFIC IS 2X          
         DC    AL4(454536)                                                      
         DC    AL4(442548)                                                      
         DC    AL4(391842)                                                      
         DC    AL4(324000)                                                      
         DC    AL4(256347)                                                      
         DC    AL4(222498)                                                      
         DC    AL4(181431)                                                      
         DC    AL4(329733)                                                      
         DC    AL4(503118)                                                      
         DC    AL4(191394)                                                      
         DC    AL4(334647)                                                      
         DC    AL4(931122)         PIL                                          
         DC    AL4(715941)         PI                                           
         DC    AL4(320292)         SE                                           
         DC    AL4(96930)          C3,C6                                        
         DC    AL4(191160)         C9                                           
*                                                                               
         DC    AL2(49,32,0,0,0,0)  RADIO                                        
         DC    AL4(59144)          N/D                                          
         DC    AL4(59144)          P,ANN,S,D,ACR                                
         DC    AL4(34304)          3-5 GROUP                                    
         DC    AL4(23658)          6-8 GROUP                                    
         DC    AL4(18918)          9+                                           
         EJECT                                                                  
         DC    AL2(52,32,0,0,0,0)  PUB AND PBS RADIO                            
         DC    AL4(67447)          P,ANN,ACR                                    
         DC    AL4(70037)          S,D                                          
         DC    AL4(45662)          3-5 GROUP                                    
         DC    AL4(36524)          6-8 GROUP                                    
         DC    AL4(22845)          9+                                           
         EJECT                                                                  
*              SPANISH USES                                                     
*                                                                               
SNTTBL   DC    AL2(40,44,0,0,0,0)  NETWORK (2009 RATE * 1.06)* 1.10)            
         DC    AL4(266815)         'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(200646)         'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(195329)         'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(172896)         'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(143000)         'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(113350)         'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(98194)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(80052)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
SNWTBL   DC    AL2(40,44,1,255,0,0)  NETWK/WSP COMBINED (UNITS 1-255)           
         DC    AL4(643)                                                         
         DC    AL4(467)                                                         
         DC    AL4(456)                                                         
         DC    AL4(422)                                                         
         DC    AL4(328)                                                         
         DC    AL4(270)                                                         
         DC    AL4(244)                                                         
         DC    AL4(174)                                                         
         EJECT                                                                  
*              SPANISH WILDSPOT TABLES                                          
*                                                                               
SWSTAB   DC    AL2(110,44,1,1,0,0)  UNIT 1 (2009 RATE * 1.06)* 1.05)            
*              SAME CATEGORIES FOR ALL TV WILDSPOTS (ENTRIES 10-14)             
         DC    AL4(70524)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(53029)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(51633)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(45716)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(37798)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(29912)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(25958)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(21170)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(110,44,2,25,0,0)  UNITS 2-25                                 
         DC    AL4(2414)                                                        
         DC    AL4(1652)                                                        
         DC    AL4(1882)                                                        
         DC    AL4(1623)                                                        
         DC    AL4(1326)                                                        
         DC    AL4(667)                                                         
         DC    AL4(526)                                                         
         DC    AL4(439)                                                         
*                                                                               
         DC    AL2(110,44,26,60,0,0)  UNITS 26-60                               
         DC    AL4(896)                                                         
         DC    AL4(703)                                                         
         DC    AL4(970)                                                         
         DC    AL4(822)                                                         
         DC    AL4(682)                                                         
         DC    AL4(281)                                                         
         DC    AL4(193)                                                         
         DC    AL4(177)                                                         
*                                                                               
         DC    AL2(110,44,61,125,0,0)  UNITS 61-125                             
         DC    AL4(896)                                                         
         DC    AL4(703)                                                         
         DC    AL4(703)                                                         
         DC    AL4(549)                                                         
         DC    AL4(460)                                                         
         DC    AL4(170)                                                         
         DC    AL4(96)                                                          
         DC    AL4(96)                                                          
*                                                                               
         DC    AL2(110,44,126,255,0,0)  UNITS 126+                              
         DC    AL4(896)                                                         
         DC    AL4(703)                                                         
         DC    AL4(348)                                                         
         DC    AL4(281)                                                         
         DC    AL4(245)                                                         
         DC    AL4(170)                                                         
         DC    AL4(96)                                                          
         DC    AL4(96)                                                          
         EJECT                                                                  
*              SPANISH FOREIGN REUSE                                            
*                                                                               
SFRTBL   DC    AL2(410,80,0,0,0,0)  SFRA,SFRC - 4X SESSION RATE                 
         DC    AL4(268676)         (4 X 67169)                                  
         DC    AL4(202016)                                                      
         DC    AL4(196688)                                                      
         DC    AL4(174152)                                                      
         DC    AL4(144000)                                                      
         DC    AL4(113932)                                                      
         DC    AL4(98888)                                                       
         DC    AL4(80636)                                                       
         DC    AL4(146548)                                                      
         DC    AL4(223608)                                                      
         DC    AL4(85064)                                                       
         DC    AL4(148732)                                                      
         DC    AL4(413832)         PIL                                          
         DC    AL4(318196)         PI                                           
         DC    AL4(142352)         SE                                           
         DC    AL4(43080)          C3,C6                                        
         DC    AL4(84960)          C9                                           
*                                                                               
         DC    AL2(411,80,0,0,0,0)  SFRB - 3X SESSION RATE                      
         DC    AL4(201507)         (3 X 67169)                                  
         DC    AL4(151512)                                                      
         DC    AL4(147516)                                                      
         DC    AL4(130614)                                                      
         DC    AL4(108000)                                                      
         DC    AL4(85449)                                                       
         DC    AL4(74166)                                                       
         DC    AL4(60477)                                                       
         DC    AL4(109911)                                                      
         DC    AL4(167706)                                                      
         DC    AL4(63798)                                                       
         DC    AL4(111549)                                                      
         DC    AL4(310374)         PIL                                          
         DC    AL4(238647)         PI                                           
         DC    AL4(106764)         SE                                           
         DC    AL4(32310)          C3,C6                                        
         DC    AL4(63720)          C9                                           
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
         DC    AL2(53,96,1,24,0,0)  TV TAGS - REGULAR, UNITS 1-24               
         DC    AL4(19822)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(19822)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    2AL4(0)             N/D                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(53,96,25,49,0,0)  UNITS 25-49                                
         DC    AL4(11064)                                                       
         DC    AL4(8335)                                                        
         DC    AL4(11064)                                                       
         DC    AL4(11064)                                                       
         DC    AL4(11064)                                                       
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(11064)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(53,96,50,255,0,0)  UNITS 50+                                 
         DC    AL4(6056)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(6056)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(54,96,1,1,0,0)  TV TAGS - W/1 SESS FEE                       
         DC    AL4(67169)                                                       
         DC    AL4(50504)                                                       
         DC    AL4(49172)                                                       
         DC    AL4(43538)                                                       
         DC    AL4(36000)                                                       
         DC    AL4(28483)                                                       
         DC    AL4(24722)                                                       
         DC    AL4(20159)                                                       
         DC    AL4(36637)                                                       
         DC    AL4(55902)                                                       
         DC    AL4(21266)                                                       
         DC    AL4(37183)                                                       
         DC    AL4(103458)         PIL                                          
         DC    AL4(79549)          PI                                           
         DC    AL4(35588)          SE                                           
         DC    AL4(10770)          C3,C6                                        
         DC    AL4(21240)          C9                                           
         DC    2AL4(0)                     SOLO/DUO                             
         DC    AL4(40301)          ESB     STAND-IN UNLIMITED                   
         DC    AL4(23393)          ESI     STAND-IN EXTRA                       
*                                                                               
         DC    AL2(54,96,2,25,0,0)  UNITS 2-25                                  
         DC    AL4(19822)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(19822)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(19822)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    AL4(15023)                                                       
         DC    2AL4(0)                     SOLO/DUO                             
         DC    AL4(0)              ESB     STAND-IN UNLIMITED                   
         DC    AL4(0)              ESI     STAND-IN EXTRA                       
*                                                                               
         DC    AL2(54,96,26,50,0,0)  UNITS 26-50                                
         DC    AL4(11064)                                                       
         DC    AL4(8335)                                                        
         DC    AL4(11064)                                                       
         DC    AL4(11064)                                                       
         DC    AL4(11064)                                                       
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(11064)                                                       
         DC    AL4(0)              N/D                                          
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    AL4(8335)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(54,96,51,255,0,0)  UNITS 51+                                 
         DC    AL4(6056)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(6056)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(6056)                                                        
         DC    AL4(0)              N/D                                          
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    AL4(4542)                                                        
         DC    2AL4(0)             N/D                                          
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
*                                                                               
         DC    AL2(55,44,1,25,0,0)  AFT RADIO TAGS - REGULAR                    
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
*                                                                               
         DC    AL2(55,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
*                                                                               
         DC    AL2(55,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
*                                                                               
         DC    AL2(56,44,1,1,0,0)  AFT RADIO TAGS - WITH 1 SESSION FEE          
         DC    AL4(29810)                                                       
         DC    AL4(29810)                                                       
         DC    AL4(21962)                                                       
         DC    AL4(19437)                                                       
         DC    AL4(17238)                                                       
         DC    AL4(22925)                                                       
         DC    AL4(10235)                                                       
         DC    AL4(16371)                                                       
*                                                                               
         DC    AL2(56,44,2,25,0,0)                                              
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
         DC    AL4(12337)                                                       
*                                                                               
         DC    AL2(56,44,26,50,0,0)  AFT RADIO TAGS - REGULAR                   
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
         DC    AL4(8854)                                                        
*                                                                               
         DC    AL2(56,44,51,255,0,0)  AFT RADIO TAGS - REGULAR                  
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
         DC    AL4(4831)                                                        
*                                                                               
         EJECT                                                                  
INRUNLTB DC    AL2(236,44,0,0,0,0)  THEAT/INDUST REUSE-TV UNLIMITED USE         
         DC    AL4(107470)         (1.6 X SESSION)                              
         DC    AL4(80806)                                                       
         DC    AL4(78675)                                                       
         DC    AL4(69661)                                                       
         DC    AL4(57600)                                                       
         DC    AL4(45573)                                                       
         DC    AL4(39555)                                                       
         DC    AL4(32254)                                                       
*                                                                               
         DC    AL2(235,32,0,0,0,0)  THEAT/INDUST REUSE-RAD UNLIM USE            
         DC    AL4(47696)          (1.6 X SESSION)                              
         DC    AL4(47696)                                                       
         DC    AL4(35139)                                                       
         DC    AL4(31099)                                                       
         DC    AL4(27581)                                                       
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
         DC    AL4(2964)            PRINCIPAL ON CAMERA                         
         DC    AL4(2022)                "     OFF CAMERA                        
         DC    AL4(2322)            GROUP 3-5 ON CAMERA                         
         DC    AL4(2001)              "   6-8 "    "                            
         DC    AL4(1626)              "    9+ "    "                            
         DC    AL4(829)             GROUP 3-5 OFF CAMERA                        
         DC    AL4(642)               "   6-8  "    "                           
         DC    AL4(540)               "    9+  "    "                           
*                                                                               
         DC    AL2(239,44,0,0,0,0)  50,001-100,000 SUBSCRIBERS                  
         DC    AL4(5960)                                                        
         DC    AL4(4071)                                                        
         DC    AL4(4638)                                                        
         DC    AL4(3996)                                                        
         DC    AL4(3253)                                                        
         DC    AL4(1642)                                                        
         DC    AL4(1289)                                                        
         DC    AL4(1075)                                                        
*                                                                               
         DC    AL2(240,44,0,0,0,0)  100,001-150,000 SUBSCRIBERS                 
         DC    AL4(8924)                                                        
         DC    AL4(6110)                                                        
         DC    AL4(6971)                                                        
         DC    AL4(5997)                                                        
         DC    AL4(4890)                                                        
         DC    AL4(2466)                                                        
         DC    AL4(1926)                                                        
         DC    AL4(1616)                                                        
*                                                                               
         DC    AL2(241,44,0,0,0,0)  150,001-200,000 SUBSCRIBERS                 
         DC    AL4(11904)                                                       
         DC    AL4(8148)                                                        
         DC    AL4(9282)                                                        
         DC    AL4(7998)                                                        
         DC    AL4(6522)                                                        
         DC    AL4(3317)                                                        
         DC    AL4(2573)                                                        
         DC    AL4(2156)                                                        
*                                                                               
         DC    AL2(242,44,0,0,0,0)  200,001-250,000 SUBSCRIBERS                 
         DC    AL4(14868)                                                       
         DC    AL4(10181)                                                       
         DC    AL4(11604)                                                       
         DC    AL4(9999)                                                        
         DC    AL4(8148)                                                        
         DC    AL4(4125)                                                        
         DC    AL4(3226)                                                        
         DC    AL4(2707)                                                        
*                                                                               
         DC    AL2(243,44,0,0,0,0)  250,001-500,000 SUBSCRIBERS                 
         DC    AL4(29762)                                                       
         DC    AL4(20378)                                                       
         DC    AL4(23224)                                                       
         DC    AL4(19998)                                                       
         DC    AL4(16296)                                                       
         DC    AL4(8239)                                                        
         DC    AL4(6441)                                                        
         DC    AL4(5393)                                                        
*                                                                               
         DC    AL2(244,44,0,0,0,0)  500,001-750,000 SUBSCRIBERS                 
         DC    AL4(44630)                                                       
         DC    AL4(30554)                                                       
         DC    AL4(34818)                                                       
         DC    AL4(29992)                                                       
         DC    AL4(24455)                                                       
         DC    AL4(12364)                                                       
         DC    AL4(9662)                                                        
         DC    AL4(8100)                                                        
*                                                                               
         DC    AL2(245,44,0,0,0,0)  750,001-1 MILLION SUBSCRIBERS               
         DC    AL4(59508)                                                       
         DC    AL4(40746)                                                       
         DC    AL4(46433)                                                       
         DC    AL4(39991)                                                       
         DC    AL4(32608)                                                       
         DC    AL4(16489)                                                       
         DC    AL4(12883)                                                       
         DC    AL4(10802)                                                       
*                                                                               
         DC    AL2(246,44,0,0,0,0)  OVER 1 MILLION SUBSCRIBERS                  
         DC    AL4(67169)                                                       
         DC    AL4(50504)                                                       
         DC    AL4(49172)                                                       
         DC    AL4(43538)                                                       
         DC    AL4(36000)                                                       
         DC    AL4(28483)                                                       
         DC    AL4(24722)                                                       
         DC    AL4(20159)                                                       
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
         DC    AL4(67169)          'ON' AR,AS,P,ANN,S,D,1-4MS,1-4SS             
         DC    AL4(50504)          'OFF' AR,AS,P,ANN,S,D,1-4MS,1-4SS            
         DC    AL4(49172)          'ON' 1-4M3,1-4S3,D3,S3                       
         DC    AL4(43538)          'ON' 1-4M6,1-4S6,D6,S6                       
         DC    AL4(36000)          'ON' 1-4M9,1-4S9,D9,S9                       
         DC    AL4(28483)          'OFF' 1-4M3,1-4S3,D3,S3                      
         DC    AL4(24722)          'OFF' 1-4M6,1-4S6,D6,S6                      
         DC    AL4(20159)          'OFF' 1-4M9,1-4S9,D9,S9                      
*                                                                               
         DC    AL2(252,44,2,255,0,0)                                            
         DC    AL1(50),AL3(67169)                                               
         DC    AL1(50),AL3(50504)                                               
         DC    AL1(50),AL3(49172)                                               
         DC    AL1(50),AL3(43538)                                               
         DC    AL1(50),AL3(36000)                                               
         DC    AL1(50),AL3(28483)                                               
         DC    AL1(50),AL3(24722)                                               
         DC    AL1(50),AL3(20159)                                               
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
         DC    AL2(543),AL1(USOM,ALL,ALL-AFM,0,0,0,INTERNET+NEWMEDIA)           
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
******  USE NUMBERS 543 USED FOR SOCIAL MEDIA                                   
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
*                                  SKIP ROW 18, USED SOLO/DUO                   
         DC    AL1(20,CTESB)                                                    
*                                                                               
         DC    AL1(21,CTESI)                                                    
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
**PAN#1  DC    CL21'237TACONCURR 11/02/16'                                      
         END                                                                    
