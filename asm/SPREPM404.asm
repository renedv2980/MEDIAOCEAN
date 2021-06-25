*          DATA SET SPREPM404  AT LEVEL 007 AS OF 05/16/86                      
*PHASE SPC404T,+0,NOAUTO                                                        
         TITLE 'SPREPM402-MARKET PERFORMANCE TABLES'                            
         PRINT NOGEN                                                            
SPM404   CSECT                                                                  
         USING *,11,9                                                           
         DC    A(ROWDEF01)         DETAIL                                       
         DC    A(ROWDEF02)         DETAIL - DAYPART TOTAL                       
         DC    A(ROWDEF03)         DETAIL - DPT GROUP TOTAL                     
         DC    A(ROWDEF04)         DETAIL - TOTAL                               
         DC    A(ROWDEF05)         MONTH PRIMARY DEMOO (CD2-PD2)                
         DC    A(ROWDEF06)                                                      
         DC    A(ROWDEF07)                                                      
         DC    A(ROWDEF08)                                                      
         DC    A(ROWDEF09)         PERIOD DETAIL   CD1-PD1                      
         DC    A(ROWDEF10)                                                      
         DC    A(ROWDEF11)                                                      
         DC    A(ROWDEF12)                                                      
         DC    A(ROWDEF13)         PERIOD PRIMARY WITH DEMOS  CD1-PD1           
         DC    A(ROWDEF14)                                                      
         DC    A(ROWDEF15)                                                      
         DC    A(ROWDEF16)                                                      
         DC    A(ROWDEF17)         CLIENT TOTALS-DOLLARS AND SPOTS              
         DC    A(ROWDEF18)                                                      
         DC    A(ROWDEF19)                                                      
         DC    A(ROWDEF20)                                                      
         DC    A(ROWDEF21)         DEMO TOTALS 1-4                              
         DC    A(ROWDEF22)                                                      
         DC    A(ROWDEF23)                                                      
         DC    A(ROWDEF24)                                                      
         DC    A(ROWDEF25)                                                      
         DC    A(ROWDEF26)         DEMO TOTALS 5-8                              
         DC    A(ROWDEF27)                                                      
         DC    A(ROWDEF28)                                                      
         DC    A(ROWDEF29)                                                      
         DC    A(ROWDEF30)         DEMO TOTALS 9-12                             
         DC    A(ROWDEF31)                                                      
         DC    A(ROWDEF32)                                                      
         DC    A(ROWDEF33)         DEMO TOTALS 13-14                            
         DC    A(ROWDEF34)                                                      
         DC    A(ROWDEF35)                                                      
         DC    A(ROWDEF36)                                                      
         DC    A(ROWDEF37)                                                      
         DC    A(ROWDEF38)                                                      
         DC    A(ROWDEF39)                                                      
         DC    A(ROWDEF40)                                                      
         DC    A(ROWDEF41)                                                      
         DC    A(ROWDEF42)                                                      
         DC    A(ROWDEF43)                                                      
         DC    A(ROWDEF44)                                                      
         DC    A(ROWDEF45)         SPILL ONLY LINES                             
         DC    A(ROWDEF46)                                                      
         DC    A(ROWDEF47)                                                      
         DC    A(ROWDEF48)                                                      
         DC    A(ROWDEF49)                                                      
         DC    A(ROWDEF50)                                                      
         DC    A(ROWDEF51)                                                      
         DC    A(ROWDEF52)                                                      
         DC    A(ROWDEF53)                                                      
         DC    A(ROWDEF54)                                                      
         DC    A(ROWDEF55)                                                      
         DC    A(ROWDEF56)                                                      
         DC    A(ROWDEF57)                                                      
         DC    A(ROWDEF58)                                                      
         DC    A(ROWDEF59)                                                      
         DC    A(ROWDEF60)                                                      
         DC    A(ROWDEF61)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROW DEFINITIONS                                                  
         SPACE 3                                                                
ROWDEF01 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF05 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'05'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'06'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF07 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'07'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF08 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'08'                                                            
         DC    X'FF'                                                            
ROWDEF09 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'09'                                                            
         DC    X'FF'                                                            
ROWDEF10 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'0A'                                                            
         DC    X'FF'                                                            
ROWDEF11 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'0B'                                                            
         DC    X'FF'                                                            
ROWDEF12 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'0C'                                                            
         DC    X'FF'                                                            
ROWDEF13 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'0D'                                                            
         DC    X'FF'                                                            
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'0E'                                                            
         DC    X'FF'                                                            
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'0F'                                                            
         DC    X'FF'                                                            
ROWDEF16 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'10'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF17 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'11'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF18 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'12'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF19 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'13'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF20 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'0000'                                                          
         MEXT  X'14'                                                            
         DC    X'FF'                                                            
ROWDEF21 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'15'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF22 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'16'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF23 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'17'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF24 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'18'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF25 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'19'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF26 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1A'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF27 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1B'               RECORD  NUMBER                               
         DC    X'FF'                                                            
ROWDEF28 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1C'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF29 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1D'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF30 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1E'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF31 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'1F'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF32 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'20'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF33 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'21'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF34 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  DAYPART             DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'22'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF35 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'23'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF36 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'24'               RECORD NUMBER                                
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF37 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'25'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF38 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'26'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF39 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'27'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF40 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'28'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF41 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'29'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF42 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF00'                                                          
         MEXT  X'2A'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF43 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'FF00'                                                          
         MEXT  X'2B'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF44 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'FF00'                                                          
         MEXT  X'2C'                                                            
         DC    X'FF'                                                            
ROWDEF45 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'2D'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF46 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'2E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF47 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'2F'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF48 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'30'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF49 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'31'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF50 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF01'                                                          
         MEXT  X'32'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF51 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'FF01'                                                          
         MEXT  X'33'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF52 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  X'FF'               SPOT-LENGTH                                  
         MEXT  X'FF01'                                                          
         MEXT  X'34'                                                            
         DC    X'FF'                                                            
*FROM ROW 4                                                                     
ROWDEF53 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'35'                                                            
         DC    X'FF'                                                            
*FROM ROW 8                                                                     
ROWDEF54 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  WORK,1                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'36'                                                            
         DC    X'FF'                                                            
*FROM ROW 12                                                                    
ROWDEF55 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+1,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'37'                                                            
         DC    X'FF'                                                            
*FROM ROW 16                                                                    
ROWDEF56 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'38'                                                            
         DC    X'FF'                                                            
*FROM ROW 20                                                                    
ROWDEF57 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'0000'                                                          
         MEXT  X'39'                                                            
         DC    X'FF'                                                            
*FROM ROW24                                                                     
ROWDEF58 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'01'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'3A'               RECORD NUMBER                                
         DC    X'FF'                                                            
*FROM ROW 28                                                                    
ROWDEF59 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'02'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'3B'               RECORD NUMBER                                
         DC    X'FF'                                                            
*FROM ROW 32                                                                    
ROWDEF60 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'03'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'3C'               RECORD NUMBER                                
         DC    X'FF'                                                            
*FROM ROW 36                                                                    
ROWDEF61 DC    A(PRNTDEF3)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  PRIMARY             PRIMARY                                      
         MEXT  X'04'               RECORD NUMBER                                
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'         SUB-DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT-LENGTH                                  
         MEXT  X'0000'                                                          
         MEXT  X'3D'               RECORD NUMBER                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  GOAL-DEM1,PERIOD         1                                       
         MEXT  GOAL-DEM1EQV,PERIOD      2                                       
         MEXT  GOAL-$,PERIOD            3                                       
         MEXT  GOAL-$EQV,PERIOD         4                                       
         MEXT  BUY-DEM1,PERIOD          5                                       
         MEXT  BUY-DEM1EQV,PERIOD       6                                       
         MEXT  BUY-$,PERIOD             7                                       
         MEXT  BUY-$EQV,PERIOD          8                                       
         MEXT  BUY-SPOTS,PERIOD         9                                       
         MEXT  BUY-DEM2,PERIOD          10                                      
         MEXT  BUY-DEM2EQV,PERIOD       11                                      
         MEXT  BUY-DEM3,PERIOD          12                                      
         MEXT  BUY-DEM3EQV,PERIOD       13                                      
         MEXT  BUY-DEM4,PERIOD          14                                      
         MEXT  BUY-DEM4EQV,PERIOD       15                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  ZERO                     1                                       
         MEXT  ZERO                     2                                       
         MEXT  GOAL-$,PERIOD            3                                       
         MEXT  ZERO                     4                                       
         MEXT  ZERO                     5                                       
         MEXT  ZERO                     6                                       
         MEXT  BUY-$,PERIOD             7                                       
         MEXT  ZERO                     8                                       
         MEXT  BUY-SPOTS,PERIOD         9                                       
         MEXT  ZERO                     10                                      
         MEXT  ZERO                     11                                      
         MEXT  ZERO                     12                                      
         MEXT  ZERO                     13                                      
         MEXT  ZERO                     14                                      
         MEXT  ZERO                     15                                      
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF3  MEXT  GOAL-DEM1,MONTH1         1                                       
         MEXT  GOAL-DEM1EQV,MONTH1      2                                       
         MEXT  GOAL-$,MONTH1            3                                       
         MEXT  GOAL-$EQV,MONTH1         4                                       
         MEXT  BUY-DEM1,MONTH1          5                                       
         MEXT  BUY-DEM1EQV,MONTH1       6                                       
         MEXT  BUY-$,MONTH1             7                                       
         MEXT  BUY-$EQV,MONTH1          8                                       
         MEXT  BUY-SPOTS,MONTH1         9                                       
         MEXT  GOAL-DEM1,MONTH2         10                                      
         MEXT  GOAL-DEM1EQV,MONTH2      11                                      
         MEXT  GOAL-$,MONTH2            12                                      
         MEXT  GOAL-$EQV,MONTH2         13                                      
         MEXT  BUY-DEM1,MONTH2          14                                      
         MEXT  BUY-DEM1EQV,MONTH2       15                                      
         MEXT  BUY-$,MONTH2             16                                      
         MEXT  BUY-$EQV,MONTH2          17                                      
         MEXT  BUY-SPOTS,MONTH2         18                                      
         MEXT  GOAL-DEM1,MONTH3         19                                      
         MEXT  GOAL-DEM1EQV,MONTH3      20                                      
         MEXT  GOAL-$,MONTH3            21                                      
         MEXT  GOAL-$EQV,MONTH3        22                                       
         MEXT  BUY-DEM1,MONTH3          23                                      
         MEXT  BUY-DEM1EQV,MONTH3       24                                      
         MEXT  BUY-$,MONTH3             25                                      
         MEXT  BUY-$EQV,MONTH3          26                                      
         MEXT  BUY-SPOTS,MONTH3         27                                      
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  BUY-$,TOTAL               1                                      
         MEXT  BUY-$EQV,TOTAL            2                                      
         MEXT  BUY-DEM1,TOTAL            3                                      
         MEXT  BUY-DEM1EQV,TOTAL         4                                      
         MEXT  BUY-$,TOTAL               5                                      
         MEXT  BUY-$EQV,TOTAL            6                                      
         MEXT  BUY-DEM2,TOTAL            7                                      
         MEXT  BUY-DEM2EQV,TOTAL         8                                      
         MEXT  BUY-$,TOTAL               9                                      
         MEXT  BUY-$EQV,TOTAL           10                                      
         MEXT  BUY-DEM3,TOTAL           11                                      
         MEXT  BUY-DEM3EQV,TOTAL        12                                      
         MEXT  BUY-$,TOTAL              13                                      
         MEXT  BUY-$EQV,TOTAL           14                                      
         MEXT  BUY-DEM4,TOTAL           15                                      
         MEXT  BUY-DEM4EQV,TOTAL        16                                      
         DC    X'FF'                                                            
COLDEF5  MEXT  BUY-$,TOTAL                                                      
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
COLDEF6  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM9,TOTAL                                                   
         MEXT  BUY-DEM9EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM10,TOTAL                                                  
         MEXT  BUY-DEM10EQV,TOTAL                                               
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM11,TOTAL                                                  
         MEXT  BUY-DEM11EQV,TOTAL                                               
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM12,TOTAL                                                  
         MEXT  BUY-DEM12EQV,TOTAL                                               
         DC    X'FF'                                                            
COLDEF7  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM13,TOTAL                                                  
         MEXT  BUY-DEM13EQV,TOTAL                                               
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM14,TOTAL                                                  
         MEXT  BUY-DEM14EQV,TOTAL                                               
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,9                                                  
         MEDIT CPM,11,COL,1,3                                                   
         MEDIT CPM,36,COL,5,7                                                   
*        MEDIT AVERAGE,63,COL,9,5                                               
         MEDIT WAVE,63,COL,9,5,28                                               
         MEDIT PERCENT,76,COL,5,1                                               
         MEDIT PERCENT,81,COL,7,3                                               
         MEDIT IMPS3,87,COL,7,10,12,14                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
PRNTDEF2 MEDIT DAYPART,1,KEY,9                                                  
         MEDIT MONTHLY,12,COL,1,3,5,7                                           
         MEDIT MONTHLY,52,COL,10,12,14,16                                       
         MEDIT MONTHLY,92,COL,19,21,23,25                                       
         DC    X'FF'                                                            
PRNTDEF3 MEDIT DAYPART,1,KEY,9                                                  
         MEDIT CPM,11,COL,3,1                                                   
         MEDIT CPM,36,COL,7,5                                                   
         MEDIT CPM,61,COL,11,9                                                  
         MEDIT CPM,86,COL,15,13                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPM404 05/16/86'                                      
         END                                                                    
