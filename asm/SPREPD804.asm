*          DATA SET SPREPD804  AT LEVEL 016 AS OF 09/15/83                      
*PHASE SPD804T,+0,NOAUTO                                                        
         TITLE 'SPREPD804-SWEEP REPORT TABLES'                                  
         PRINT NOGEN                                                            
SPD804   CSECT                                                                  
         USING *,11                                                             
         DC    A(ROWDEF01)                                                      
         DC    A(ROWDEF02)                                                      
         DC    A(ROWDEF03)                                                      
         DC    A(ROWDEF04)                                                      
         DC    A(ROWDEF05)                                                      
         DC    A(ROWDEF06)                                                      
         DC    A(ROWDEF07)                                                      
         DC    A(ROWDEF08)                                                      
         DC    A(ROWDEF09)                                                      
         DC    A(ROWDEF10)                                                      
         DC    A(ROWDEF11)                                                      
         DC    A(ROWDEF12)                                                      
         DC    A(ROWDEF13)                                                      
         DC    A(ROWDEF14)                                                      
         DC    A(ROWDEF15)                                                      
         DC    A(ROWDEF16)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF01 DC    A(PRTDEF2)          DAYPART DETAIL -DEMOS 1-4                    
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  WORK+1,1                                                         
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF02 DC    A(PRTDEF2)          TOTAL DEMOS 1-4                              
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRTDEF2)          DAYPART DETAIL - DEMOS  5-8                  
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  WORK+1,1                                                         
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRTDEF2)          TOTAL DEMOS 5-8                              
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
ROWDEF05 DC    A(PRTDEF2)          DAYPART DETAIL - DEMOS 9-12                  
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  WORK+1,1                                                         
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRTDEF2)          TOTAL DEMOS 9-12                             
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
ROWDEF07 DC    A(PRTDEF2)          DAYPART DETAIL - DEMOS 13-14                 
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  SUB-DAYPART                                                      
         MEXT  DAYPART                                                          
         MEXT  SPOT-LENGTH                                                      
         MEXT  WORK+1,1                                                         
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF08 DC    A(PRTDEF2)          TOTAL DEMOS 13-14                            
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF09 DC    A(PRTDEF2)          TOTAL DEMOS 1-4 SPILL                        
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FD'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF10 DC    A(PRTDEF2)          TOTAL DEMOS 5-8 (SPILL)                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FD'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF11 DC    A(PRTDEF2)          TOTAL DEMOS 9-12 (SPILL)                     
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FD'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF12 DC    A(PRTDEF2)          TOTAL DEMOS 13-14 (SPILL)                    
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FD'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF13 DC    A(PRTDEF2)          TOTAL DEMOS 1-4 (ORIG)                       
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FE'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF14 DC    A(PRTDEF2)          TOTAL DEMOS 5-8 (ORIG)                       
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FE'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'02'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF15 DC    A(PRTDEF2)          TOTAL DEMOS 9-12 (ORIG)                      
         DC    A(COLDEF4)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FE'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'03'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF16 DC    A(PRTDEF2)          TOTAL DEMOS 13-14 (ORIG)                     
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FE'                                                            
         MEXT  WORK+1,1                                                         
         MEXT  X'04'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF2  MEXT  BUY-$                                                            
         MEXT  BUY-$EQV                                                         
         MEXT  BUY-SPOTS                                                        
         MEXT  BUY-DEM1                                                         
         MEXT  BUY-DEM1EQV                                                      
         MEXT  BUY-DEM2                                                         
         MEXT  BUY-DEM2EQV                                                      
         MEXT  BUY-DEM3                                                         
         MEXT  BUY-DEM3EQV                                                      
         MEXT  BUY-DEM4                                                         
         MEXT  BUY-DEM4EQV                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  BUY-DEM5                                                         
         MEXT  BUY-DEM5EQV                                                      
         MEXT  BUY-DEM6                                                         
         MEXT  BUY-DEM6EQV                                                      
         MEXT  BUY-DEM7                                                         
         MEXT  BUY-DEM7EQV                                                      
         MEXT  BUY-DEM8                                                         
         MEXT  BUY-DEM8EQV                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF4  MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  BUY-DEM9                                                         
         MEXT  BUY-DEM9EQV                                                      
         MEXT  BUY-DEM10                                                        
         MEXT  BUY-DEM10EQV                                                     
         MEXT  BUY-DEM11                                                        
         MEXT  BUY-DEM11EQV                                                     
         MEXT  BUY-DEM12                                                        
         MEXT  BUY-DEM12EQV                                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF5  MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  BUY-DEM13                                                        
         MEXT  BUY-DEM13EQV                                                     
         MEXT  BUY-DEM14                                                        
         MEXT  BUY-DEM14EQV                                                     
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         MEXT  ZERO                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
* PRINT DEFINITIONS                                                             
PRTDEF1  MEDIT DAYPART,1,KEY,1                                                  
         DC    X'FF'                                                            
PRTDEF2  DC    X'FF'                                                            
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPD804 09/15/83'                                      
         END                                                                    
         PRINT OFF                                                              
