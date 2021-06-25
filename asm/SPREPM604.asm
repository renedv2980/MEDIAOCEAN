*          DATA SET SPREPM604  AT LEVEL 009 AS OF 11/04/83                      
*PHASE SPM604T,+0,NOAUTO                                                        
         TITLE 'SPREPM604-PB BRAND PERFORMANCE TABLES'                          
         PRINT NOGEN                                                            
SPM604   CSECT                                                                  
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
         DC    A(ROWDEF13)         O+S DETAIL                                   
         DC    A(ROWDEF14)         O+S PD                                       
         DC    A(ROWDEF15)         SPILL-DETAIL                                 
         DC    A(ROWDEF16)         SPILL-PD                                     
         DC    A(ROWDEF17)                                                      
         DC    A(ROWDEF18)                                                      
         DC    A(ROWDEF19)                                                      
         DC    A(ROWDEF20)                                                      
         DC    A(ROWDEF21)                                                      
         DC    A(ROWDEF22)                                                      
         DC    A(ROWDEF23)                                                      
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
         MEXT  X'00'                                                            
         MEXT  X'01'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF05 DC    A(PRNTDEF1)         PRIMARY DEMO TABLES                          
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'05'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'06'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF07 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'07'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF08 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'08'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF09 DC    A(PRNTDEF1)         CLIENT TABLES                                
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'09'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF10 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'0A'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF11 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  SUB-DAYPART                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'0B'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF12 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'0C'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF13 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'0D'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'0E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FE'                                                            
         MEXT  X'0F'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF16 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FE'                                                            
         MEXT  X'10'                                                            
         DC    X'FF'                                                            
*FROM ROW 4                                                                     
ROWDEF17 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'11'                                                            
         DC    X'FF'                                                            
*FROM ROW 8                                                                     
ROWDEF18 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'12'                                                            
         DC    X'FF'                                                            
*FROM ROW 12                                                                    
ROWDEF19 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'13'                                                            
         DC    X'FF'                                                            
*FROM ROW 13                                                                    
ROWDEF20 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'14'                                                            
         DC    X'FF'                                                            
*FROM ROW 14                                                                    
ROWDEF21 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'15'                                                            
         DC    X'FF'                                                            
*FROM ROW 15                                                                    
ROWDEF22 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'00'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'16'                                                            
         DC    X'FF'                                                            
*FROM ROW 16                                                                    
ROWDEF23 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'17'                                                            
         DC    X'FF'                                                            
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
         MEXT  BUY-SPOTS,PERIOD   14                                            
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
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,3                                                  
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
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPM604 11/04/83'                                      
         END                                                                    
