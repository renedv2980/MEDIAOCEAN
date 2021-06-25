*          DATA SET SPREPF404  AT LEVEL 011 AS OF 11/15/84                      
*PHASE SPF404T,+0,NOAUTO                                                        
         TITLE 'SPREPF404-COMMERCIAL PERFORMANCE REPORT'                        
         PRINT NOGEN                                                            
SPF404   CSECT                                                                  
         USING *,11                                                             
         DC    A(ROWDEF01)         DETAIL                                       
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
         MEXT  WORK+2,8                                                         
         MEXT  WORK+10,2                                                        
         MEXT  X'01'               6                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
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
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,3                                                  
         MEDIT CPM,11,COL,1,3                                                   
         MEDIT CPM,36,COL,5,7                                                   
         MEDIT AVERAGE,63,COL,9,5                                               
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
**PAN#1  DC    CL21'011SPREPF404 11/15/84'                                      
         END                                                                    
