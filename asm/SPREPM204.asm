*          DATA SET SPREPM204  AT LEVEL 009 AS OF 03/04/88                      
*PHASE SPM204T,+0,NOAUTO                                                        
         TITLE 'SPREPM204-BRAND PERFORMANCE TABLES'                             
         PRINT NOGEN                                                            
SPM204   CSECT                                                                  
         USING *,11                                                             
         DC    A(ROWDEF01)         DETAIL                                       
         DC    A(ROWDEF02)         DETAIL - DAYPART TOTAL                       
         DC    A(ROWDEF03)         DETAIL - DPT GROUP TOTAL                     
         DC    A(ROWDEF04)         DETAIL - TOTAL                               
         DC    A(ROWDEF05)         PRIMARY DEMO - DETAIL                        
         DC    A(ROWDEF06)         PD - DAYPART TOTAL                           
         DC    A(ROWDEF07)         PD - DPT GROUP TOTAL                         
         DC    A(ROWDEF08)         PD - TOTAL                                   
         DC    A(ROWDEF09)         CLIENT - DETAIL                              
         DC    A(ROWDEF10)         CLIENT - DAYPART TOTAL                       
         DC    A(ROWDEF11)         CLIENT - DPT GROUP TOTAL                     
         DC    A(ROWDEF12)         CLIENT - TOTAL                               
         DC    A(ROWDEF13)         DETAIL +SPILL                                
         DC    A(ROWDEF14)         PD - +SPILL                                  
         DC    A(ROWDEF15)                                                      
         DC    A(ROWDEF16)         SPILL ONLY TABLES                            
         DC    A(ROWDEF17)                                                      
         DC    A(ROWDEF18)                                                      
         DC    A(ROWDEF19)                                                      
         DC    A(ROWDEF20)                                                      
         DC    A(ROWDEF21)                                                      
         DC    A(ROWDEF22)                                                      
         DC    A(ROWDEF23)                                                      
         DC    A(ROWDEF24)                                                      
         DC    A(ROWDEF25)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROW DEFINITIONS                                                  
         SPACE 3                                                                
ROWDEF01 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1              1                                            
         MEXT  X'00'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'01'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1              1                                            
         MEXT  X'00'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'02'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1              1                                            
         MEXT  X'00'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'03'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1              1                                            
         MEXT  X'00'               2                                            
         MEXT  X'FFFFFFFF'         3                                            
         MEXT  X'FFFFFFFF'         4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'04'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF05 DC    A(PRNTDEF1)         PRIMARY DEMO TABLES                          
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'05'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'06'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF07 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'         4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'07'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF08 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  PRIMARY             2                                            
         MEXT  X'FFFFFFFF'         3                                            
         MEXT  X'FFFFFFFF'         4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'08'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF09 DC    A(PRNTDEF1)         CLIENT TABLES                                
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'09'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF10 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'0A'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF11 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'         4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'0B'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF12 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  X'FF'               2                                            
         MEXT  X'FFFFFFFF'         3                                            
         MEXT  X'FFFFFFFF'         4                                            
         MEXT  X'FF'               5                                            
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'0C'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF13 DC    A(PRNTDEF1)         ORIG + SPILL TABLES                          
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'0D'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'0E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                  WAS COLDEF6                          
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'0F'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF16 DC    A(PRNTDEF1)         SPILL ONLY TABLES                            
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF010000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'10'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF17 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF010000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'11'                                                            
         DC    X'FF'                                                            
ROWDEF18 DC    A(PRNTDEF1)         O+S FROM ROW 13                              
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'12'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF19 DC    A(PRNTDEF1)         FROM ROW 14                                  
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'13'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF20 DC    A(PRNTDEF1)         FROM ROW 15                                  
         DC    A(COLDEF3)          WAS COLDEF6                                  
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF000000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'14'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF21 DC    A(PRNTDEF1)         SPILL ONLY-FROM ROW 16                       
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF010000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'15'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF22 DC    A(PRNTDEF1)         FROM ROW 17                                  
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF010000'                                                      
         MEXT  ZERO                                                             
         MEXT  X'16'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF23 DC    A(PRNTDEF1)         FROM ROW 4                                   
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                1                                          
         MEXT  X'00'                 2                                          
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'17'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF24 DC    A(PRNTDEF1)         FROM ROW 8                                   
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  PRIMARY             2                                            
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'18'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF25 DC    A(PRNTDEF1)         FROM ROW DEF 12                              
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1            1                                            
         MEXT  X'FF'               2                                            
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  X'19'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  GOAL-DEM1,PERIOD    1                                            
         MEXT  GOAL-DEM1EQV,PERIOD 2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  GOAL-$EQV,PERIOD    4                                            
         MEXT  BUY-DEM1,PERIOD     5                                            
         MEXT  BUY-DEM1EQV,PERIOD  6                                            
         MEXT  BUY-$,PERIOD        7                                            
         MEXT  BUY-$EQV,PERIOD     8                                            
         MEXT  BUY-SPOTS,PERIOD    9                                            
         MEXT  BUY-DEM2,PERIOD     10                                           
         MEXT  BUY-DEM2EQV,PERIOD  11                                           
         MEXT  BUY-DEM3,PERIOD     12                                           
         MEXT  BUY-DEM3EQV,PERIOD  13                                           
         MEXT  BUY-DEM4,PERIOD     14                                           
         MEXT  BUY-DEM4EQV,PERIOD  15                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  GOAL-DEM1,PERIOD    1         (NO EQUIVS)                        
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  BUY-DEM1,PERIOD     5                                            
         MEXT  ZERO                6                                            
         MEXT  BUY-$,PERIOD        7                                            
         MEXT  ZERO                8                                            
         MEXT  BUY-SPOTS,PERIOD    9                                            
         MEXT  BUY-DEM2,PERIOD     10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-DEM3,PERIOD     12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-DEM4,PERIOD     14                                           
         MEXT  ZERO                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  ZERO                1         (DOLLARS AND SPOTS ONLY)           
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  ZERO                5                                            
         MEXT  ZERO                6                                            
         MEXT  BUY-$,PERIOD        7                                            
         MEXT  ZERO                8                                            
         MEXT  BUY-SPOTS,PERIOD    9                                            
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  GOAL-DEM1EQV,PERIOD                                              
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  GOAL-$EQV,PERIOD                                                 
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
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  ZERO                                                             
         MEXT  BUY-$,PERIOD                                                     
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF6  MEXT  GOAL-DEM1,PERIOD    ORIG + SPILL                                 
         MEXT  GOAL-DEM1EQV,PERIOD                                              
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  GOAL-$EQV,PERIOD                                                 
         MEXT  BUY-DEM1,PERIOD     5                                            
         MEXT  BUY-DEM1EQV,PERIOD  6                                            
         MEXT  BUY-$,PERIOD        7                                            
         MEXT  BUY-$EQV,PERIOD     8                                            
         MEXT  BUY-SPOTS,PERIOD    9                                            
         MEXT  BUY-DEM2,PERIOD     10                                           
         MEXT  BUY-DEM2EQV,PERIOD  11                                           
         MEXT  BUY-DEM3,PERIOD     12                                           
         MEXT  BUY-DEM3EQV,PERIOD  13                                           
         MEXT  BUY-DEM4,PERIOD     14                                           
         MEXT  BUY-DEM4EQV,PERIOD  15                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF7  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  ZERO                                                             
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  ZERO                                                             
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  BUY-DEM1EQV,PERIOD                                               
         MEXT  BUY-$,PERIOD                                                     
         MEXT  BUY-$EQV,PERIOD                                                  
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
         PRINT NOGEN                                                            
PRNTDEF1 MEDIT DAYPART,1,KEY,3                                                  
         MEDIT CPM,11,COL,1,3                                                   
         MEDIT CPM,36,COL,5,7                                                   
         MEDIT WAVE,63,COL,9,5,16                                               
         MEDIT PERCENT,76,COL,5,1                                               
         MEDIT PERCENT,81,COL,7,3                                               
         MEDIT IMPS3,87,COL,7,10,12,14                                          
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPM204 03/04/88'                                      
         END                                                                    
