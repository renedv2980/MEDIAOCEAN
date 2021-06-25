*          DATA SET SPREPM704  AT LEVEL 004 AS OF 11/07/83                      
*PHASE SPM704T,+0,NOAUTO                                                        
         TITLE 'SPREPM704-PB MARKET PERFORMANCE TABLES'                         
         PRINT NOGEN                                                            
SPM704   CSECT                                                                  
         USING *,11                                                             
         USING *,11,9                                                           
         DC    A(ROWDEF01)         DETAIL                                       
         DC    A(ROWDEF02)         DETAIL - DAYPART TOTAL                       
         DC    A(ROWDEF03)         DETAIL - DAYPART GROUP TOTAL                 
         DC    A(ROWDEF04)         DETAIL - TOTAL                               
         DC    A(ROWDEF05)         PRIMARY DEMO                                 
         DC    A(ROWDEF06)                                                      
         DC    A(ROWDEF07)                                                      
         DC    A(ROWDEF08)                                                      
         DC    A(ROWDEF09)         CLIENT                                       
         DC    A(ROWDEF10)                                                      
         DC    A(ROWDEF11)                                                      
         DC    A(ROWDEF12)                                                      
         DC    A(ROWDEF13)         AGENCY RECAP - PRIMARY DEMO                  
         DC    A(ROWDEF14)                                                      
         DC    A(ROWDEF15)                                                      
         DC    A(ROWDEF16)                                                      
         DC    A(ROWDEF17)         AGENCY RECAP - CLIENT                        
         DC    A(ROWDEF18)                                                      
         DC    A(ROWDEF19)                                                      
         DC    A(ROWDEF20)                                                      
         DC    A(ROWDEF21)         ALL AGENCIES - PRIMARY DEMO                  
         DC    A(ROWDEF22)                                                      
         DC    A(ROWDEF23)                                                      
         DC    A(ROWDEF24)                                                      
         DC    A(ROWDEF25)         ALL AGENCIES - CLIENT                        
         DC    A(ROWDEF26)                                                      
         DC    A(ROWDEF27)                                                      
         DC    A(ROWDEF28)                                                      
         DC    A(ROWDEF29)         CLIENT - SECONDARY DEMO SUMMARIES            
         DC    A(ROWDEF30)                                                      
         DC    A(ROWDEF31)                                                      
         DC    A(ROWDEF32)                                                      
         DC    A(ROWDEF33)                                                      
         DC    A(ROWDEF34)                                                      
         DC    A(ROWDEF35)                                                      
         DC    A(ROWDEF36)                                                      
         DC    A(ROWDEF37)         AGY RECAP - SECONDARY DEMO                   
         DC    A(ROWDEF38)                                                      
         DC    A(ROWDEF39)                                                      
         DC    A(ROWDEF40)                                                      
         DC    A(ROWDEF41)                                                      
         DC    A(ROWDEF42)                                                      
         DC    A(ROWDEF43)                                                      
         DC    A(ROWDEF44)                                                      
         DC    A(ROWDEF45)         ALL AGY - SECONDARY DEMO                     
         DC    A(ROWDEF46)                                                      
         DC    A(ROWDEF47)                                                      
         DC    A(ROWDEF48)                                                      
         DC    A(ROWDEF49)                                                      
         DC    A(ROWDEF50)                                                      
         DC    A(ROWDEF51)                                                      
         DC    A(ROWDEF52)                                                      
         DC    A(ROWDEF53)         O+S LINE 4                                   
         DC    A(ROWDEF54)         O+S LINE 8                                   
         DC    A(ROWDEF55)         O+S LINE 16                                  
         DC    A(ROWDEF56)         O+S LINE 24                                  
         DC    A(ROWDEF57)         O+S LINE 32                                  
         DC    A(ROWDEF58)         O+S LINE 36                                  
         DC    A(ROWDEF59)         O+S LINE 40                                  
         DC    A(ROWDEF60)         O+S LINE 44                                  
         DC    A(ROWDEF61)         O+S LINE 48                                  
         DC    A(ROWDEF62)         O+S LINE 52                                  
         DC    A(ROWDEF63)         SPILL ONLY LINE 4                            
         DC    A(ROWDEF64)         SPILL ONLY LINE 8                            
         DC    A(ROWDEF65)         SPILL ONLY LINE 16                           
         DC    A(ROWDEF66)         SPILL ONLY LINE 24                           
         DC    A(ROWDEF67)         SPILL ONLY LINE 32                           
         DC    A(ROWDEF68)         SPILL ONLY LINE 36                           
         DC    A(ROWDEF69)         SPILL ONLY LINE 40                           
         DC    A(ROWDEF70)         SPILL ONLY LINE 44                           
         DC    A(ROWDEF71)         SPILL ONLY LINE 48                           
         DC    A(ROWDEF72)         SPILL ONLY LINE 52                           
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF01 DC    A(PRNTDEF1)         DETAIL LINES                                 
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              1                                            
         MEXT  X'01'               1                                            
         MEXT  WORK,2              2    AGENCY CODE                             
         MEXT  PRIMARY             3                                            
         MEXT  X'00'               SECONDARY GROUP                              
         MEXT  WORK+2,3            ALPHA BRAND                                  
         MEXT  BRAND               4                                            
         MEXT  SUB-DAYPART         5                                            
         MEXT  DAYPART             6                                            
         MEXT  SPOT-LENGTH         7                                            
         MEXT  X'0001'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0002'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0003'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0004'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF05 DC    A(PRNTDEF1)         PRIMARY DEMO TOTALS (NO AGENCIES)            
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0005'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0006'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF07 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0007'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF08 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0008'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF09 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0009'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF10 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'000A'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF11 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'000B'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF12 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'000C'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF13 DC    A(PRNTDEF1)         PRIMARY DEMO TABLE (AGENCY RECAP)            
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'000D'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'000E'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'000F'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF16 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0010'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF17 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0011'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF18 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0012'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF19 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0013'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF20 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0014'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF21 DC    A(PRNTDEF1)         ALL AGENCIES                                 
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0015'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF22 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0016'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF23 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0017'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF24 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'0018'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF25 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0019'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF26 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'001A'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF27 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'001B'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF28 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'001C'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF29 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'01'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'001D'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF30 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'001E'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF31 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'001F'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF32 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0020'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF33 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'0021'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF34 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'0022'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF35 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0023'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF36 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0024'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF37 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'0025'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF38 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'0026'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF39 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0027'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF40 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0028'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF41 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'0029'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF42 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'002A'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF43 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'002B'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF44 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'002C'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF45 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'002D'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF46 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'             RECORD CODE                                    
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'002E'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF47 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'002F'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF48 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0030'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF49 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'0031'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF50 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT LENGTH                                  
         MEXT  X'0032'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF51 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  SUB-DAYPART         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0033'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF52 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'0034'                                                          
         DC    X'FF'                                                            
ROWDEF53 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FF35'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF54 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FF36'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF55 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  X'0102'                                                          
         MEXT  X'0102'                                                          
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FF37'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF56 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FF38'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF57 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF39'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF58 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF3A'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF59 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF3B'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF60 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF3C'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF61 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF3D'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF62 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF3E'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF63 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FE3F'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF64 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FE40'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF65 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FE41'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF66 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFFFF'                                                    
         MEXT  X'FE42'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF67 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FE43'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF68 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FF44'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF69 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FE45'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF70 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FE46'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF71 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FE47'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF72 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFFFF'       DAYPART/SPOT LENGTH                          
         MEXT  X'FE48'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  GOAL-DEM1,PERIOD    1                                            
         MEXT  GOAL-DEM1EQV,PERIOD 2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  GOAL-$EQV,PERIOD    4                                            
         MEXT  LOCKIN-DEM1,PERIOD  5                                            
         MEXT  LOCKIN-DEM1EQV,PERIOD                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  LOCKIN-$EQV,PERIOD  8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  BUY-DEM1,PERIOD     10                                           
         MEXT  BUY-DEM1EQV,PERIOD  11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  BUY-$EQV,PERIOD     13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         MEXT  BUY-DEM2,PERIOD     15                                           
         MEXT  BUY-DEM2EQV,PERIOD  16                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  GOAL-DEM1,PERIOD    1         (NO EQUIVS)                        
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  LOCKIN-DEM1,PERIOD  5                                            
         MEXT  ZERO                6                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  ZERO                8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  BUY-DEM1,PERIOD     10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         MEXT  BUY-DEM2,PERIOD     15                                           
         MEXT  ZERO                16                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  ZERO                1         (DOLLARS AND SPOTS ONLY)           
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  ZERO                5                                            
         MEXT  ZERO                6                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  ZERO                8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  ZERO                10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  GOAL-DEM1EQV,PERIOD                                              
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  GOAL-$EQV,PERIOD                                                 
         MEXT  LOCKIN-DEM1,PERIOD                                               
         MEXT  LOCKIN-DEM1EQV,PERIOD                                            
         MEXT  LOCKIN-$,PERIOD                                                  
         MEXT  LOCKIN-$EQV,PERIOD                                               
         MEXT  LOCKIN-SPOTS,PERIOD                                              
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  BUY-DEM1EQV,PERIOD                                               
         MEXT  BUY-$,PERIOD                                                     
         MEXT  BUY-$EQV,PERIOD                                                  
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF5  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  ZERO                                                             
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  ZERO                                                             
         MEXT  LOCKIN-DEM1,PERIOD                                               
         MEXT  ZERO                                                             
         MEXT  LOCKIN-$,PERIOD                                                  
         MEXT  ZERO                                                             
         MEXT  LOCKIN-SPOTS,PERIOD                                              
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  ZERO                                                             
         MEXT  BUY-$,PERIOD                                                     
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF6  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM1,TOTAL                                                   
         MEXT  BUY-DEM1EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM2,TOTAL                                                   
         MEXT  BUY-DEM2EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM3,TOTAL                                                   
         MEXT  BUY-DEM3EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM4,TOTAL                                                   
         MEXT  BUY-DEM4EQV,TOTAL                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF7  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM5,TOTAL                                                   
         MEXT  BUY-DEM5EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM6,TOTAL                                                   
         MEXT  BUY-DEM6EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM7,TOTAL                                                   
         MEXT  BUY-DEM7EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM8,TOTAL                                                   
         MEXT  BUY-DEM8EQV,TOTAL                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,11                                                 
         MEDIT CPM,12,COL,1,3                                                   
         MEDIT CPM,39,COL,5,7                                                   
         MEDIT NUMBER,59,COL,9                                                  
         MEDIT CPM,133,COL,10,12                                                
         MEDIT IMPS,118,COL,12,15                                               
         MEDIT NUMBER,102,COL,14                                                
         MEDIT PERCENT,113,COL,14,9                                             
         MEDIT PERCENT,157,COL,10,1                                             
         MEDIT PERCENT,161,COL,12,3                                             
         DC    X'FF'                                                            
PRNTDEF2 MEDIT DAYPART,1,KEY,11                                                 
         MEDIT CPM,11,COL,3,1                                                   
         MEDIT CPM,36,COL,7,5                                                   
         MEDIT CPM,61,COL,11,9                                                  
         MEDIT CPM,86,COL,15,13                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPM704 11/07/83'                                      
         END                                                                    
