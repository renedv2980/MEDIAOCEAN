*          DATA SET SPREPXD04  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SPD204A                                                                  
         TITLE 'SPREPD204-BTS,BRS,SAL TABLES'                                   
         PRINT NOGEN                                                            
SPD204   CSECT                                                                  
         USING *,11                                                             
         DC    A(ROWDEF01)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
* ROW DEFINITIONS                                                               
ROWDEF01 DC    A(PRNTDEF1)         DUMMY ROW DEFINITION                         
         DC    A(COLDEF1)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  X'9001'                                                          
         MEXT  X'00'                                                            
         MEXT  X'00000000'                                                      
         MEXT  X'00'                                                            
         MEXT  X'01'                                                            
* COLUMN DEFINITIONS                                                            
COLDEF1  MEXT  BUY-SPOTS           1                                            
         MEXT  BUY-$               2                                            
         MEXT  BUY-$EQV            3                                            
         MEXT  BUY-DEM1            4                                            
         MEXT  BUY-DEM1EQV         5                                            
         MEXT  BUY-DEM2            6                                            
         MEXT  BUY-DEM2EQV         7                                            
         MEXT  BUY-DEM3            8                                            
         MEXT  BUY-DEM3EQV         9                                            
         MEXT  BUY-DEM4           10                                            
         MEXT  BUY-DEM4EQV        11                                            
         DC    X'FF'                                                            
PRNTDEF1 MEDIT NUMBER,1,COL,1                                                   
         MEDIT CPM,11,COL,4,2                                                   
         MEDIT IMPS3,35,2,6,8,10                                                
         DC    X'FF'                                                            
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
