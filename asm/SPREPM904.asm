*          DATA SET SPREPM904  AT LEVEL 008 AS OF 08/29/00                      
*PHASE SPM904A                                                                  
         TITLE 'TABLES FOR BRAND MEDIA PLAN'                                    
SPM904   CSECT                                                                  
         USING *,11                                                             
         PRINT NOGEN                                                            
         EJECT                                                                  
*              REPORT DEFINITION TABLES                                         
         SPACE 3                                                                
         DC    A(RD111)            FIRST QUARTER WEEKS                          
         DC    A(RD112)                                                         
         DC    A(RD113)                                                         
         SPACE 1                                                                
         DC    A(RD211)            SECOND QUARTER WEEKS                         
         DC    A(RD212)                                                         
         DC    A(RD213)                                                         
         SPACE 1                                                                
         DC    A(RD311)            THIRD QUARTER WEEKS                          
         DC    A(RD312)                                                         
         DC    A(RD313)                                                         
         SPACE 1                                                                
         DC    A(RD411)            FOURTH QUARTER WEEKS                         
         DC    A(RD412)                                                         
         DC    A(RD413)                                                         
         SPACE 1                                                                
         DC    A(RD511)            MONTH RECAP                                  
         DC    A(RD512)                                                         
         DC    A(RD513)                                                         
         SPACE 1                                                                
         DC    A(RD611)            QUARTER RECAP                                
         DC    A(RD612)                                                         
         DC    A(RD613)                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROW DEFINITIONS --  FIRST QUARTER WEEKS                          
         SPACE 3                                                                
RD111    DC    A(0)                DAYPART/SPOT-LENGTH DETAIL                   
         DC    A(CD1)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD112    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD1)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD113    DC    A(0)                 TOTAL                                       
         DC    A(CD1)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROW DEFINITIONS --  SECOND QUARTER WEEKS                         
         SPACE 3                                                                
RD211    DC    A(0)                DAYPART/SPOT-LENGTH DETAIL                   
         DC    A(CD2)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD212    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD2)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD213    DC    A(0)                 TOTAL                                       
         DC    A(CD2)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROW DEFINITIONS --  THIRD QUARTER WEEKS                          
         SPACE 3                                                                
RD311    DC    A(0)                DAYPART/SPOT-LENGTH DETAIL                   
         DC    A(CD3)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'03'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD312    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD3)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'03'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD313    DC    A(0)                 TOTAL                                       
         DC    A(CD3)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'03'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROW DEFINITIONS --  FOURTH QUARTER WEEKS                         
         SPACE 3                                                                
RD411    DC    A(0)                DAY PART/SPOT-LENGTH DETAIL                  
         DC    A(CD4)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'04'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD412    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD4)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'04'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD413    DC    A(0)                 TOTAL                                       
         DC    A(CD4)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'04'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROW DEFINITIONS --  MONTHLY ANALYSIS                             
         SPACE 3                                                                
RD511    DC    A(0)                DAYPART/SPOT-LENGTH DETAIL                   
         DC    A(CD5)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'05'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD512    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD5)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'05'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD513    DC    A(0)                 TOTAL                                       
         DC    A(CD5)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'05'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              ROW DEFINITIONS --  QUARTER ANALYSIS                             
         SPACE 3                                                                
RD611    DC    A(0)                DAYPART/SPOT-LENGTH DETAIL                   
         DC    A(CD6)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'06'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'01'                                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
RD612    DC    A(0)                DAYPART TOTAL                                
         DC    A(CD6)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'06'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  DAYPART-GROUP                                                    
         MEXT  X'FF02'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
RD613    DC    A(0)                 TOTAL                                       
         DC    A(CD6)                                                           
         DC    8X'00'                                                           
         MEXT  WORK,1                                                           
         MEXT  X'06'                                                            
         MEXT  WORK+1,41                                                        
         MEXT  X'FFFFFFFFFF03'                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              COLUMN DEFINITION - FIRST QUARTER WEEKS                          
         SPACE 3                                                                
CD1      DS    0D                                                               
         MEXT  GOAL-$,WEEK1                                                     
         MEXT  GOAL-$,WEEK2                                                     
         MEXT  GOAL-$,WEEK3                                                     
         MEXT  GOAL-$,WEEK4                                                     
         MEXT  GOAL-$,WEEK5                                                     
         MEXT  GOAL-$,WEEK6                                                     
         MEXT  GOAL-$,WEEK7                                                     
         MEXT  GOAL-$,WEEK8                                                     
         MEXT  GOAL-$,WEEK9                                                     
         MEXT  GOAL-$,WEEK10                                                    
         MEXT  GOAL-$,WEEK11                                                    
         MEXT  GOAL-$,WEEK12                                                    
         MEXT  GOAL-$,WEEK13                                                    
         MEXT  GOAL-$,WEEK14                                                    
         MEXT  GOAL-$,MONTH1                                                    
         MEXT  GOAL-$,MONTH2                                                    
         MEXT  GOAL-$,MONTH3                                                    
         MEXT  GOAL-$,QUARTER1                                                  
         SPACE 3                                                                
         MEXT  GOAL-DEM1,WEEK1                                                  
         MEXT  GOAL-DEM1,WEEK2                                                  
         MEXT  GOAL-DEM1,WEEK3                                                  
         MEXT  GOAL-DEM1,WEEK4                                                  
         MEXT  GOAL-DEM1,WEEK5                                                  
         MEXT  GOAL-DEM1,WEEK6                                                  
         MEXT  GOAL-DEM1,WEEK7                                                  
         MEXT  GOAL-DEM1,WEEK8                                                  
         MEXT  GOAL-DEM1,WEEK9                                                  
         MEXT  GOAL-DEM1,WEEK10                                                 
         MEXT  GOAL-DEM1,WEEK11                                                 
         MEXT  GOAL-DEM1,WEEK12                                                 
         MEXT  GOAL-DEM1,WEEK13                                                 
         MEXT  GOAL-DEM1,WEEK14                                                 
         MEXT  GOAL-DEM1,MONTH1                                                 
         MEXT  GOAL-DEM1,MONTH2                                                 
         MEXT  GOAL-DEM1,MONTH3                                                 
         MEXT  GOAL-DEM1,QUARTER1                                               
         EJECT                                                                  
         MEXT   BUY-$,WEEK1                                                     
         MEXT   BUY-$,WEEK2                                                     
         MEXT   BUY-$,WEEK3                                                     
         MEXT   BUY-$,WEEK4                                                     
         MEXT   BUY-$,WEEK5                                                     
         MEXT   BUY-$,WEEK6                                                     
         MEXT   BUY-$,WEEK7                                                     
         MEXT   BUY-$,WEEK8                                                     
         MEXT   BUY-$,WEEK9                                                     
         MEXT   BUY-$,WEEK10                                                    
         MEXT   BUY-$,WEEK11                                                    
         MEXT   BUY-$,WEEK12                                                    
         MEXT   BUY-$,WEEK13                                                    
         MEXT   BUY-$,WEEK14                                                    
         MEXT   BUY-$,MONTH1                                                    
         MEXT   BUY-$,MONTH2                                                    
         MEXT   BUY-$,MONTH3                                                    
         MEXT   BUY-$,QUARTER1                                                  
         SPACE 3                                                                
         MEXT  BUY-DEM1,WEEK1                                                   
         MEXT  BUY-DEM1,WEEK2                                                   
         MEXT  BUY-DEM1,WEEK3                                                   
         MEXT  BUY-DEM1,WEEK4                                                   
         MEXT  BUY-DEM1,WEEK5                                                   
         MEXT  BUY-DEM1,WEEK6                                                   
         MEXT  BUY-DEM1,WEEK7                                                   
         MEXT  BUY-DEM1,WEEK8                                                   
         MEXT  BUY-DEM1,WEEK9                                                   
         MEXT  BUY-DEM1,WEEK10                                                  
         MEXT  BUY-DEM1,WEEK11                                                  
         MEXT  BUY-DEM1,WEEK12                                                  
         MEXT  BUY-DEM1,WEEK13                                                  
         MEXT  BUY-DEM1,WEEK14                                                  
         MEXT  BUY-DEM1,MONTH1                                                  
         MEXT  BUY-DEM1,MONTH2                                                  
         MEXT  BUY-DEM1,MONTH3                                                  
         MEXT  BUY-DEM1,QUARTER1                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITIONS - SECOND QUARTER WEEKS                        
         SPACE 3                                                                
CD2      DS    0D                                                               
         MEXT  GOAL-$,WEEK15                                                    
         MEXT  GOAL-$,WEEK16                                                    
         MEXT  GOAL-$,WEEK17                                                    
         MEXT  GOAL-$,WEEK18                                                    
         MEXT  GOAL-$,WEEK19                                                    
         MEXT  GOAL-$,WEEK20                                                    
         MEXT  GOAL-$,WEEK21                                                    
         MEXT  GOAL-$,WEEK22                                                    
         MEXT  GOAL-$,WEEK23                                                    
         MEXT  GOAL-$,WEEK24                                                    
         MEXT  GOAL-$,WEEK25                                                    
         MEXT  GOAL-$,WEEK26                                                    
         MEXT  GOAL-$,WEEK27                                                    
         MEXT  GOAL-$,WEEK28                                                    
         MEXT  GOAL-$,MONTH4                                                    
         MEXT  GOAL-$,MONTH5                                                    
         MEXT  GOAL-$,MONTH6                                                    
         MEXT  GOAL-$,QUARTER2                                                  
         SPACE 3                                                                
         MEXT  GOAL-DEM1,WEEK15                                                 
         MEXT  GOAL-DEM1,WEEK16                                                 
         MEXT  GOAL-DEM1,WEEK17                                                 
         MEXT  GOAL-DEM1,WEEK18                                                 
         MEXT  GOAL-DEM1,WEEK19                                                 
         MEXT  GOAL-DEM1,WEEK20                                                 
         MEXT  GOAL-DEM1,WEEK21                                                 
         MEXT  GOAL-DEM1,WEEK22                                                 
         MEXT  GOAL-DEM1,WEEK23                                                 
         MEXT  GOAL-DEM1,WEEK24                                                 
         MEXT  GOAL-DEM1,WEEK25                                                 
         MEXT  GOAL-DEM1,WEEK26                                                 
         MEXT  GOAL-DEM1,WEEK27                                                 
         MEXT  GOAL-DEM1,WEEK28                                                 
         MEXT  GOAL-DEM1,MONTH4                                                 
         MEXT  GOAL-DEM1,MONTH5                                                 
         MEXT  GOAL-DEM1,MONTH6                                                 
         MEXT  GOAL-DEM1,QUARTER2                                               
         EJECT                                                                  
         MEXT   BUY-$,WEEK15                                                    
         MEXT   BUY-$,WEEK16                                                    
         MEXT   BUY-$,WEEK17                                                    
         MEXT   BUY-$,WEEK18                                                    
         MEXT   BUY-$,WEEK19                                                    
         MEXT   BUY-$,WEEK20                                                    
         MEXT   BUY-$,WEEK21                                                    
         MEXT   BUY-$,WEEK22                                                    
         MEXT   BUY-$,WEEK23                                                    
         MEXT   BUY-$,WEEK24                                                    
         MEXT   BUY-$,WEEK25                                                    
         MEXT   BUY-$,WEEK26                                                    
         MEXT   BUY-$,WEEK27                                                    
         MEXT   BUY-$,WEEK28                                                    
         MEXT   BUY-$,MONTH4                                                    
         MEXT   BUY-$,MONTH5                                                    
         MEXT   BUY-$,MONTH6                                                    
         MEXT   BUY-$,QUARTER2                                                  
         SPACE 3                                                                
         MEXT  BUY-DEM1,WEEK15                                                  
         MEXT  BUY-DEM1,WEEK16                                                  
         MEXT  BUY-DEM1,WEEK17                                                  
         MEXT  BUY-DEM1,WEEK18                                                  
         MEXT  BUY-DEM1,WEEK19                                                  
         MEXT  BUY-DEM1,WEEK20                                                  
         MEXT  BUY-DEM1,WEEK21                                                  
         MEXT  BUY-DEM1,WEEK22                                                  
         MEXT  BUY-DEM1,WEEK23                                                  
         MEXT  BUY-DEM1,WEEK24                                                  
         MEXT  BUY-DEM1,WEEK25                                                  
         MEXT  BUY-DEM1,WEEK26                                                  
         MEXT  BUY-DEM1,WEEK27                                                  
         MEXT  BUY-DEM1,WEEK28                                                  
         MEXT  BUY-DEM1,MONTH4                                                  
         MEXT  BUY-DEM1,MONTH5                                                  
         MEXT  BUY-DEM1,MONTH6                                                  
         MEXT  BUY-DEM1,QUARTER2                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITION - THIRD QUARTER WEEKS                          
         SPACE 3                                                                
CD3      DS    0D                                                               
         MEXT  GOAL-$,WEEK29                                                    
         MEXT  GOAL-$,WEEK30                                                    
         MEXT  GOAL-$,WEEK31                                                    
         MEXT  GOAL-$,WEEK32                                                    
         MEXT  GOAL-$,WEEK33                                                    
         MEXT  GOAL-$,WEEK34                                                    
         MEXT  GOAL-$,WEEK35                                                    
         MEXT  GOAL-$,WEEK36                                                    
         MEXT  GOAL-$,WEEK37                                                    
         MEXT  GOAL-$,WEEK38                                                    
         MEXT  GOAL-$,WEEK39                                                    
         MEXT  GOAL-$,WEEK40                                                    
         MEXT  GOAL-$,WEEK41                                                    
         MEXT  GOAL-$,WEEK42                                                    
         MEXT  GOAL-$,MONTH7                                                    
         MEXT  GOAL-$,MONTH8                                                    
         MEXT  GOAL-$,MONTH9                                                    
         MEXT  GOAL-$,QUARTER3                                                  
         SPACE 3                                                                
         MEXT  GOAL-DEM1,WEEK29                                                 
         MEXT  GOAL-DEM1,WEEK30                                                 
         MEXT  GOAL-DEM1,WEEK31                                                 
         MEXT  GOAL-DEM1,WEEK32                                                 
         MEXT  GOAL-DEM1,WEEK33                                                 
         MEXT  GOAL-DEM1,WEEK34                                                 
         MEXT  GOAL-DEM1,WEEK35                                                 
         MEXT  GOAL-DEM1,WEEK36                                                 
         MEXT  GOAL-DEM1,WEEK37                                                 
         MEXT  GOAL-DEM1,WEEK38                                                 
         MEXT  GOAL-DEM1,WEEK39                                                 
         MEXT  GOAL-DEM1,WEEK40                                                 
         MEXT  GOAL-DEM1,WEEK41                                                 
         MEXT  GOAL-DEM1,WEEK42                                                 
         MEXT  GOAL-DEM1,MONTH7                                                 
         MEXT  GOAL-DEM1,MONTH8                                                 
         MEXT  GOAL-DEM1,MONTH9                                                 
         MEXT  GOAL-DEM1,QUARTER3                                               
         EJECT                                                                  
         MEXT   BUY-$,WEEK29                                                    
         MEXT   BUY-$,WEEK30                                                    
         MEXT   BUY-$,WEEK31                                                    
         MEXT   BUY-$,WEEK32                                                    
         MEXT   BUY-$,WEEK33                                                    
         MEXT   BUY-$,WEEK34                                                    
         MEXT   BUY-$,WEEK35                                                    
         MEXT   BUY-$,WEEK36                                                    
         MEXT   BUY-$,WEEK37                                                    
         MEXT   BUY-$,WEEK38                                                    
         MEXT   BUY-$,WEEK39                                                    
         MEXT   BUY-$,WEEK40                                                    
         MEXT   BUY-$,WEEK41                                                    
         MEXT   BUY-$,WEEK42                                                    
         MEXT   BUY-$,MONTH7                                                    
         MEXT   BUY-$,MONTH8                                                    
         MEXT   BUY-$,MONTH9                                                    
         MEXT   BUY-$,QUARTER3                                                  
         SPACE 3                                                                
         MEXT  BUY-DEM1,WEEK29                                                  
         MEXT  BUY-DEM1,WEEK30                                                  
         MEXT  BUY-DEM1,WEEK31                                                  
         MEXT  BUY-DEM1,WEEK32                                                  
         MEXT  BUY-DEM1,WEEK33                                                  
         MEXT  BUY-DEM1,WEEK34                                                  
         MEXT  BUY-DEM1,WEEK35                                                  
         MEXT  BUY-DEM1,WEEK36                                                  
         MEXT  BUY-DEM1,WEEK37                                                  
         MEXT  BUY-DEM1,WEEK38                                                  
         MEXT  BUY-DEM1,WEEK39                                                  
         MEXT  BUY-DEM1,WEEK40                                                  
         MEXT  BUY-DEM1,WEEK41                                                  
         MEXT  BUY-DEM1,WEEK42                                                  
         MEXT  BUY-DEM1,MONTH7                                                  
         MEXT  BUY-DEM1,MONTH8                                                  
         MEXT  BUY-DEM1,MONTH9                                                  
         MEXT  BUY-DEM1,QUARTER3                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITIONS - FOURTH QUARTER WEEKS                        
         SPACE 3                                                                
CD4      DS    0D                                                               
         MEXT  GOAL-$,WEEK43                                                    
         MEXT  GOAL-$,WEEK44                                                    
         MEXT  GOAL-$,WEEK45                                                    
         MEXT  GOAL-$,WEEK46                                                    
         MEXT  GOAL-$,WEEK47                                                    
         MEXT  GOAL-$,WEEK48                                                    
         MEXT  GOAL-$,WEEK49                                                    
         MEXT  GOAL-$,WEEK50                                                    
         MEXT  GOAL-$,WEEK51                                                    
         MEXT  GOAL-$,WEEK52                                                    
         MEXT  GOAL-$,WEEK53                                                    
         MEXT  GOAL-$,WEEK54                                                    
         MEXT  GOAL-$,WEEK55                                                    
         MEXT  GOAL-$,WEEK56                                                    
         MEXT  GOAL-$,MONTH10                                                   
         MEXT  GOAL-$,MONTH11                                                   
         MEXT  GOAL-$,MONTH12                                                   
         MEXT  GOAL-$,QUARTER4                                                  
         EJECT 3                                                                
         MEXT  GOAL-DEM1,WEEK43                                                 
         MEXT  GOAL-DEM1,WEEK44                                                 
         MEXT  GOAL-DEM1,WEEK45                                                 
         MEXT  GOAL-DEM1,WEEK46                                                 
         MEXT  GOAL-DEM1,WEEK47                                                 
         MEXT  GOAL-DEM1,WEEK48                                                 
         MEXT  GOAL-DEM1,WEEK49                                                 
         MEXT  GOAL-DEM1,WEEK50                                                 
         MEXT  GOAL-DEM1,WEEK51                                                 
         MEXT  GOAL-DEM1,WEEK52                                                 
         MEXT  GOAL-DEM1,WEEK53                                                 
         MEXT  GOAL-DEM1,WEEK54                                                 
         MEXT  GOAL-DEM1,WEEK55                                                 
         MEXT  GOAL-DEM1,WEEK56                                                 
         MEXT  GOAL-DEM1,MONTH10                                                
         MEXT  GOAL-DEM1,MONTH11                                                
         MEXT  GOAL-DEM1,MONTH12                                                
         MEXT  GOAL-DEM1,QUARTER4                                               
         EJECT                                                                  
         MEXT   BUY-$,WEEK43                                                    
         MEXT   BUY-$,WEEK44                                                    
         MEXT   BUY-$,WEEK45                                                    
         MEXT   BUY-$,WEEK46                                                    
         MEXT   BUY-$,WEEK47                                                    
         MEXT   BUY-$,WEEK48                                                    
         MEXT   BUY-$,WEEK49                                                    
         MEXT   BUY-$,WEEK50                                                    
         MEXT   BUY-$,WEEK51                                                    
         MEXT   BUY-$,WEEK52                                                    
         MEXT   BUY-$,WEEK53                                                    
         MEXT   BUY-$,WEEK54                                                    
         MEXT   BUY-$,WEEK55                                                    
         MEXT   BUY-$,WEEK56                                                    
         MEXT   BUY-$,MONTH10                                                   
         MEXT   BUY-$,MONTH11                                                   
         MEXT   BUY-$,MONTH12                                                   
         MEXT   BUY-$,QUARTER4                                                  
         SPACE 3                                                                
         MEXT  BUY-DEM1,WEEK43                                                  
         MEXT  BUY-DEM1,WEEK44                                                  
         MEXT  BUY-DEM1,WEEK45                                                  
         MEXT  BUY-DEM1,WEEK46                                                  
         MEXT  BUY-DEM1,WEEK47                                                  
         MEXT  BUY-DEM1,WEEK48                                                  
         MEXT  BUY-DEM1,WEEK49                                                  
         MEXT  BUY-DEM1,WEEK50                                                  
         MEXT  BUY-DEM1,WEEK51                                                  
         MEXT  BUY-DEM1,WEEK52                                                  
         MEXT  BUY-DEM1,WEEK53                                                  
         MEXT  BUY-DEM1,WEEK54                                                  
         MEXT  BUY-DEM1,WEEK55                                                  
         MEXT  BUY-DEM1,WEEK56                                                  
         MEXT  BUY-DEM1,MONTH10                                                 
         MEXT  BUY-DEM1,MONTH11                                                 
         MEXT  BUY-DEM1,MONTH12                                                 
         MEXT  BUY-DEM1,QUARTER4                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITION - MONTHLY ANALYSIS                             
         SPACE 3                                                                
CD5      DS    0D                                                               
         MEXT  GOAL-$,MONTH1                                                    
         MEXT  GOAL-$,MONTH2                                                    
         MEXT  GOAL-$,MONTH3                                                    
         MEXT  GOAL-$,MONTH4                                                    
         MEXT  GOAL-$,MONTH5                                                    
         MEXT  GOAL-$,MONTH6                                                    
         MEXT  GOAL-$,MONTH7                                                    
         MEXT  GOAL-$,MONTH8                                                    
         MEXT  GOAL-$,MONTH9                                                    
         MEXT  GOAL-$,MONTH10                                                   
         MEXT  GOAL-$,MONTH11                                                   
         MEXT  GOAL-$,MONTH12                                                   
         MEXT  GOAL-$,MONTH13                                                   
         MEXT  GOAL-$,PERIOD                                                    
         SPACE 3                                                                
         MEXT  GOAL-DEM1,MONTH1                                                 
         MEXT  GOAL-DEM1,MONTH2                                                 
         MEXT  GOAL-DEM1,MONTH3                                                 
         MEXT  GOAL-DEM1,MONTH4                                                 
         MEXT  GOAL-DEM1,MONTH5                                                 
         MEXT  GOAL-DEM1,MONTH6                                                 
         MEXT  GOAL-DEM1,MONTH7                                                 
         MEXT  GOAL-DEM1,MONTH8                                                 
         MEXT  GOAL-DEM1,MONTH9                                                 
         MEXT  GOAL-DEM1,MONTH10                                                
         MEXT  GOAL-DEM1,MONTH11                                                
         MEXT  GOAL-DEM1,MONTH12                                                
         MEXT  GOAL-DEM1,MONTH13                                                
         MEXT  GOAL-DEM1,PERIOD                                                 
         EJECT                                                                  
         MEXT   BUY-$,MONTH1                                                    
         MEXT   BUY-$,MONTH2                                                    
         MEXT   BUY-$,MONTH3                                                    
         MEXT   BUY-$,MONTH4                                                    
         MEXT   BUY-$,MONTH5                                                    
         MEXT   BUY-$,MONTH6                                                    
         MEXT   BUY-$,MONTH7                                                    
         MEXT   BUY-$,MONTH8                                                    
         MEXT   BUY-$,MONTH9                                                    
         MEXT   BUY-$,MONTH10                                                   
         MEXT   BUY-$,MONTH11                                                   
         MEXT   BUY-$,MONTH12                                                   
         MEXT  BUY-$,MONTH13                                                    
         MEXT   BUY-$,PERIOD                                                    
         SPACE 3                                                                
         MEXT  BUY-DEM1,MONTH1                                                  
         MEXT  BUY-DEM1,MONTH2                                                  
         MEXT  BUY-DEM1,MONTH3                                                  
         MEXT  BUY-DEM1,MONTH4                                                  
         MEXT  BUY-DEM1,MONTH5                                                  
         MEXT  BUY-DEM1,MONTH6                                                  
         MEXT  BUY-DEM1,MONTH7                                                  
         MEXT  BUY-DEM1,MONTH8                                                  
         MEXT  BUY-DEM1,MONTH9                                                  
         MEXT  BUY-DEM1,MONTH10                                                 
         MEXT  BUY-DEM1,MONTH11                                                 
         MEXT  BUY-DEM1,MONTH12                                                 
         MEXT  BUY-DEM1,MONTH13                                                 
         MEXT  BUY-DEM1,PERIOD                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN DEFINITION - QUARTERLY RECAP                              
         SPACE 3                                                                
CD6      DS    0D                                                               
         MEXT  GOAL-$,QUARTER1                                                  
         MEXT  GOAL-$,QUARTER2                                                  
         MEXT  GOAL-$,QUARTER3                                                  
         MEXT  GOAL-$,QUARTER4                                                  
         MEXT  GOAL-$,PERIOD                                                    
         SPACE 1                                                                
         MEXT  GOAL-DEM1,QUARTER1                                               
         MEXT  GOAL-DEM1,QUARTER2                                               
         MEXT  GOAL-DEM1,QUARTER3                                               
         MEXT  GOAL-DEM1,QUARTER4                                               
         MEXT  GOAL-DEM1,PERIOD                                                 
         SPACE 1                                                                
         MEXT  BUY-$,QUARTER1                                                   
         MEXT  BUY-$,QUARTER2                                                   
         MEXT  BUY-$,QUARTER3                                                   
         MEXT  BUY-$,QUARTER4                                                   
         MEXT  BUY-$,PERIOD                                                     
         SPACE 1                                                                
         MEXT  BUY-DEM1,QUARTER1                                                
         MEXT  BUY-DEM1,QUARTER2                                                
         MEXT  BUY-DEM1,QUARTER3                                                
         MEXT  BUY-DEM1,QUARTER4                                                
         MEXT  BUY-DEM1,PERIOD                                                  
         SPACE 1                                                                
         MEXT  CHILD-PAY,QUARTER1                                               
         MEXT  CHILD-PAY,QUARTER2                                               
         MEXT  CHILD-PAY,QUARTER3                                               
         MEXT  CHILD-PAY,QUARTER4                                               
         MEXT  CHILD-PAY,PERIOD                                                 
         SPACE 1                                                                
         MEXT  CHILD-NTP,QUARTER1                                               
         MEXT  CHILD-NTP,QUARTER2                                               
         MEXT  CHILD-NTP,QUARTER3                                               
         MEXT  CHILD-NTP,QUARTER4                                               
         MEXT  CHILD-NTP,PERIOD                                                 
         DC    X'FF'                                                            
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPM904 08/29/00'                                      
         END                                                                    
