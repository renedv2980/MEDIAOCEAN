*          DATA SET SPREPCD04  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SPCD04A                                                                  
         TITLE 'SPREPCA04-COKE PERF TABLES'                                     
         PRINT NOGEN                                                            
*                                                                               
**********************************************************************          
*                                                                               
*        NOTE- THIS IS A CLONE OF SPREPCC04                                     
*                                                                               
**********************************************************************          
*                                                                               
SPCD04   CSECT                                                                  
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
         DC    A(ROWDEF13)         O+S                                          
         DC    A(ROWDEF14)                                                      
         DC    A(ROWDEF15)         SPILL ONLY                                   
         DC    A(ROWDEF16)                                                      
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
         MEXT  HALF,1              1                                            
         MEXT  X'00'               2                                            
         MEXT  WORK,5 3                                                         
         MEXT  SUB-DAYPART         8                                            
         MEXT  DAYPART             9                                            
         MEXT  SPOT-LENGTH         10                                           
         MEXT  X'00' 11                                                         
         MEXT  X'01'               12                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  X'00'                                                            
         MEXT  X'02'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
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
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  WORK,5                                                           
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
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'0C'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF13 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'0D'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FF'                                                            
         MEXT  X'0E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF17 DC    A(PRNTDEF1)         FROM ROW 4                                   
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'11'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF18 DC    A(PRNTDEF1)         FROM ROW 8                                   
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'12'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF19 DC    A(PRNTDEF1)         FROM ROW 12                                  
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  X'FF'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'13'                                                            
         DC    X'FF'                                                            
ROWDEF20 DC    A(PRNTDEF1)         FROM ROW 13                                  
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'14'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF21 DC    A(PRNTDEF1)         FROM ROW 14                                  
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'15'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FE'                                                            
         MEXT  X'0F'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF16 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'FE'                                                            
         MEXT  X'10'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF22 DC    A(PRNTDEF1)         FROM ROW 15                                  
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'00'                                                            
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'16'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF23 DC    A(PRNTDEF1)         FROM ROW 16                                  
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF+1,1                                                         
         MEXT  PRIMARY                                                          
         MEXT  WORK,5                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'17'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  GOAL-DEM1,WEEK1                                                  
         MEXT  GOAL-DEM1EQV,WEEK1                                               
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  GOAL-$EQV,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  BUY-DEM1EQV,WEEK1                                                
         MEXT  BUY-$,WEEK1                                                      
         MEXT  BUY-$EQV,WEEK1                                                   
         MEXT  BUY-SPOTS,WEEK1                                                  
         MEXT  BUY-DEM2,WEEK1                                                   
         MEXT  BUY-DEM2EQV,WEEK1                                                
         MEXT  BUY-DEM3,WEEK1                                                   
         MEXT  BUY-DEM3EQV,WEEK1                                                
         MEXT  BUY-DEM4,WEEK1                                                   
         MEXT  BUY-DEM4EQV,WEEK1                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  GOAL-DEM1,WEEK1               (NO EQUIVS)                        
         MEXT  ZERO                                                             
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  ZERO                                                             
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  ZERO                                                             
         MEXT  BUY-$,WEEK1                                                      
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,WEEK1                                                  
         MEXT  BUY-DEM2,WEEK1                                                   
         MEXT  ZERO                                                             
         MEXT  BUY-DEM3,WEEK1                                                   
         MEXT  ZERO                                                             
         MEXT  BUY-DEM4,WEEK1                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  ZERO                     (DOLLARS AND SPOTS ONLY)                
         MEXT  ZERO                                                             
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  BUY-$,WEEK1                                                      
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,WEEK1                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  GOAL-DEM1,WEEK1          (PRIMARY DEMO)                          
         MEXT  GOAL-DEM1EQV,WEEK1                                               
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  GOAL-$EQV,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  BUY-DEM1EQV,WEEK1                                                
         MEXT  BUY-$,WEEK1                                                      
         MEXT  BUY-$EQV,WEEK1                                                   
         MEXT  BUY-SPOTS,WEEK1                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF5  MEXT  GOAL-DEM1,WEEK1                                                  
         MEXT  ZERO                                                             
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  ZERO                                                             
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  ZERO                                                             
         MEXT  BUY-$,WEEK1                                                      
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,WEEK1                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,8                                                  
         MEDIT CPM,11,COL,1,3                                                   
         MEDIT CPM,36,COL,5,7                                                   
*        MEDIT AVERAGE,63,COL,9,5                                               
         MEDIT WAVE,63,COL,9,5,16                                               
         MEDIT PERCENT,76,COL,5,1                                               
         MEDIT PERCENT,81,COL,7,3                                               
         MEDIT IMPS3,87,COL,7,10,12,14                                          
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPCD04 08/29/00'                                      
         END                                                                    
