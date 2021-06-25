*          DATA SET GABOG02    AT LEVEL 016 AS OF 08/22/00                      
*PHASE TB1802A                                                                  
*                                                                               
***********************************************************************         
*---------------- DICE DATA STRUCTURES FOR 4X4 BOARD -----------------*         
GABOG02  CSECT                                                                  
*                                                                               
         DC    AL2(ADJDIE4X-ADJDIE4)                                            
ADJDIE4  DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,0)                    
RDIE01   DC    AL1(2,5,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                           
RDIE02   DC    AL1(1,3,5,6,7,0,0,0,0,0,0,0,0,0,0,0,0)                           
RDIE03   DC    AL1(2,4,6,7,8,0,0,0,0,0,0,0,0,0,0,0,0)                           
RDIE04   DC    AL1(3,7,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                           
RDIE05   DC    AL1(1,2,6,9,10,0,0,0,0,0,0,0,0,0,0,0,0)                          
RDIE06   DC    AL1(1,2,3,5,7,9,10,11,0,0,0,0,0,0,0,0,0)                         
RDIE07   DC    AL1(2,3,4,6,8,10,11,12,0,0,0,0,0,0,0,0,0)                        
RDIE08   DC    AL1(3,4,7,11,12,0,0,0,0,0,0,0,0,0,0,0,0)                         
RDIE09   DC    AL1(5,6,10,13,14,0,0,0,0,0,0,0,0,0,0,0,0)                        
RDIE10   DC    AL1(5,6,7,9,11,13,14,15,0,0,0,0,0,0,0,0,0)                       
RDIE11   DC    AL1(6,7,8,10,12,14,15,16,0,0,0,0,0,0,0,0,0)                      
RDIE12   DC    AL1(7,8,11,15,16,0,0,0,0,0,0,0,0,0,0,0,0)                        
RDIE13   DC    AL1(9,10,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                         
RDIE14   DC    AL1(9,10,11,13,15,0,0,0,0,0,0,0,0,0,0,0,0)                       
RDIE15   DC    AL1(10,11,12,14,16,0,0,0,0,0,0,0,0,0,0,0,0)                      
RDIE16   DC    AL1(11,12,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0)                        
ADJDIE4X EQU   *                                                                
         SPACE 3                                                                
*------------------------ LETTERS ON DICE FACES ----------------------*         
         DC    AL2(DICE4X-DICE4BY4)                                             
DICE4BY4 DC    CL24'AAEEGNABBJOOACHOPSAFFKPS'                                   
         DC    CL24'AOOTTWCIMOTUDEILRXDELRVY'                                   
         DC    CL24'DISTTYEEGHNWEEINSUEHRTVW'                                   
         DC    CL24'EIOSSTELRTTYHIMNQUHLNNRZ'                                   
DICE4X   EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*---------------- DICE DATA STRUCTURES FOR 5X5 BOARD -----------------*         
*                                                                               
         DC    AL2(ADJDIE5X-ADJDIE5)                                            
ADJDIE5  DC    AL1(1,2,3,4,5,6,7,8,9,10,11,12,13)                               
         DC    AL1(14,15,16,17,18,19,20,21,22,23,24,25,0)                       
VDIE01   DC    AL1(2,6,7),23AL1(0)                                              
VDIE02   DC    AL1(1,3,6,7,8),21AL1(0)                                          
VDIE03   DC    AL1(2,4,7,8,9),21AL1(0)                                          
VDIE04   DC    AL1(3,5,8,9,10),21AL1(0)                                         
VDIE05   DC    AL1(4,9,10),23AL1(0)                                             
VDIE06   DC    AL1(1,2,7,11,12),21AL1(0)                                        
VDIE07   DC    AL1(1,2,3,6,8,11,12,13),18AL1(0)                                 
VDIE08   DC    AL1(2,3,4,7,9,12,13,14),18AL1(0)                                 
VDIE09   DC    AL1(3,4,5,8,10,13,14,15),18AL1(0)                                
VDIE10   DC    AL1(4,5,9,14,15),21AL1(0)                                        
VDIE11   DC    AL1(6,7,12,16,17),21AL1(0)                                       
VDIE12   DC    AL1(6,7,8,11,13,16,17,18),18AL1(0)                               
VDIE13   DC    AL1(7,8,9,12,14,17,18,19),18AL1(0)                               
VDIE14   DC    AL1(8,9,10,13,15,18,19,20),18AL1(0)                              
VDIE15   DC    AL1(9,10,14,19,20),21AL1(0)                                      
VDIE16   DC    AL1(11,12,17,21,22),21AL1(0)                                     
VDIE17   DC    AL1(11,12,13,16,18,21,22,23),18AL1(0)                            
VDIE18   DC    AL1(12,13,14,17,19,22,23,24),18AL1(0)                            
VDIE19   DC    AL1(13,14,15,18,20,23,24,25),18AL1(0)                            
VDIE20   DC    AL1(14,15,19,24,25),21AL1(0)                                     
VDIE21   DC    AL1(16,17,22),23AL1(0)                                           
VDIE22   DC    AL1(16,17,18,21,23),21AL1(0)                                     
VDIE23   DC    AL1(17,18,19,22,24),21AL1(0)                                     
VDIE24   DC    AL1(18,19,20,23,25),21AL1(0)                                     
VDIE25   DC    AL1(19,20,24),23AL1(0)                                           
ADJDIE5X EQU   *                                                                
         SPACE 3                                                                
*------------------------ LETTERS ON DICE FACES ----------------------*         
         DC    AL2(DICE5X-DICE5BY5)                                             
DICE5BY5 DC    CL6'ENSSSU'                                                      
         DC    CL6'CEIILT'                                                      
         DC    CL6'DHLNOR'                                                      
         DC    CL6'OOOTTU'                                                      
         DC    CL6'AAFIRS'                                                      
         DC    CL6'EIIITT'                                                      
         DC    CL6'DDHNOT'                                                      
         DC    CL6'ADENNN'                                                      
         DC    CL6'CEIPST'                                                      
         DC    CL6'CEILPT'                                                      
         DC    CL6'BJKQXZ'                                                      
         DC    CL6'DHHLOR'                                                      
         DC    CL6'CCENST'                                                      
         DC    CL6'AEGMNN'                                                      
         DC    CL6'GORRVW'                                                      
         DC    CL6'AAAFRS'                                                      
         DC    CL6'AEEEEM'                                                      
         DC    CL6'AFIRSY'                                                      
         DC    CL6'NOOTUW'                                                      
         DC    CL6'EMOTTT'                                                      
         DC    CL6'FIPRSY'                                                      
         DC    CL6'AAEEEE'                                                      
         DC    CL6'IPRRRY'                                                      
         DC    CL6'AEEGUM'                                                      
         DC    CL6'CEIILT'                                                      
DICE5X   EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016GABOG02   08/22/00'                                      
         END                                                                    
