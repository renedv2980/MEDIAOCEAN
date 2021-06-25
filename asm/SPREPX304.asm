*          DATA SET SPREPX304  AT LEVEL 013 AS OF 08/16/83                      
*PHASE SPX304T,*,NOAUTO                                                         
         TITLE 'SPREPX304 - CHILD SPOT PERFORMANCE'                             
         PRINT NOGEN                                                            
SPX304   CSECT                                                                  
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
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROW DEFINITIONS                                                  
         SPACE 3                                                                
ROWDEF01 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'0100'             1,2                                          
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  X'01'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'0100'             1,2                                          
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  X'02'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'0100'             1,2                                          
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'03'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF04 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF1)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'0100'             1,2                                          
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'04'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF05 DC    A(PRNTDEF1)         PRIMARY DEMO TABLES                          
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  X'05'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF06 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  X'06'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF07 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  PRIMARY             2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'07'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF08 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF4)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  PRIMARY             2                                            
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'08'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF09 DC    A(PRNTDEF1)         CLIENT TABLES                                
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  SPOT-LENGTH         5                                            
         MEXT  X'09'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF10 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  DAYPART             4                                            
         MEXT  X'FF'               5                                            
         MEXT  X'0A'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF11 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  X'FF'               2                                            
         MEXT  SUB-DAYPART         3                                            
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'0B'               6                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
ROWDEF12 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(2)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,20                                                          
         MEXT  X'02'               1                                            
         MEXT  X'FF'               2                                            
         MEXT  X'FFFFFFFF'           3                                          
         MEXT  X'FFFFFFFF'           4                                          
         MEXT  X'FF'               5                                            
         MEXT  X'0C'               6                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  CHILD-PAY,WEEK1                                                  
         MEXT  CHILD-NTP,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1      5                                            
         MEXT  BUY-DEM1EQV,WEEK1   6                                            
         MEXT  BUY-$,WEEK1         7                                            
         MEXT  BUY-$EQV,WEEK1      8                                            
         MEXT  BUY-SPOTS,WEEK1     9                                            
         MEXT  BUY-DEM2,WEEK1      10                                           
         MEXT  BUY-DEM2EQV,WEEK1   11                                           
         MEXT  BUY-DEM3,WEEK1      12                                           
         MEXT  BUY-DEM3EQV,WEEK1   13                                           
         MEXT  BUY-DEM4,WEEK1      14                                           
         MEXT  BUY-DEM4EQV,WEEK1   15                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  CHILD-PAY,WEEK1     NO EQUIVS                                    
         MEXT  CHILD-NTP,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1      5                                            
         MEXT  ZERO                6                                            
         MEXT  BUY-$,WEEK1         7                                            
         MEXT  ZERO                8                                            
         MEXT  BUY-SPOTS,WEEK1     9                                            
         MEXT  BUY-DEM2,WEEK1      10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-DEM3,WEEK1      12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-DEM4,WEEK1      14                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  CHILD-PAY,WEEK1     (DOLLARS AND SPOTS ONLY)                     
         MEXT  CHILD-NTP,WEEK1                                                  
         MEXT  ZERO                5                                            
         MEXT  ZERO                6                                            
         MEXT  BUY-$,WEEK1         7                                            
         MEXT  ZERO                8                                            
         MEXT  BUY-SPOTS,WEEK1     9                                            
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  CHILD-PAY,WEEK1                                                  
         MEXT  CHILD-NTP,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  BUY-DEM1EQV,WEEK1                                                
         MEXT  BUY-$,WEEK1                                                      
         MEXT  BUY-$EQV,WEEK1                                                   
         MEXT  BUY-SPOTS,WEEK1                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF5  MEXT  CHILD-PAY,WEEK1                                                  
         MEXT  CHILD-NTP,WEEK1                                                  
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  ZERO                                                             
         MEXT  BUY-$,WEEK1                                                      
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,WEEK1                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,11,KEY,23                                                
         MEDIT NUMBER,76,COL,7                                                  
         MEDIT CPM,54,COL,3,5                                                   
         MEDIT IMPS3,87,COL,5,8,10,12                                           
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPX304 08/16/83'                                      
         END                                                                    
