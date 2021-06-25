*          DATA SET NENAV9C    AT LEVEL 001 AS OF 04/25/16                      
*PHASE T3189CA                                                                  
         TITLE 'NENAV9C - DUMMY PHASE FOR ADDITIONAL WORKING STORAGE'           
*======================================================================         
* WHEN YOU FIND THIS AND WONDER WHAT IT'S FOR,                                  
* IT'S USED WHEN YOU JUST HAVE TO HAVE MORE WORKING STORAGE.                    
*                                                                               
* IT IS MORE FORMAL THAN JUST USING THE END OF YOUR PROGRAM, BECAUSE            
* THERE'S NO EASY WAY TO CHECK IF YOU'VE GONE PAST THE END OF THE               
* PROGRAM AREA. THIS WILL ABSOLUTELY STOP THAT FROM HAPPENING.                  
*                                                                               
* SO YOU HAVE TO DO A CALLOV CALL TO LOAD THIS PHASE AND THAT WILL              
* PROTECT YOU.                                                                  
*======================================================================         
                                                                                
T3189C   CSECT                                                                  
         DC    C'*T3189C*'         OVERLAY ID                                   
         DC    A(T3189CX-T3189CA)  LENGTH OF OVERLAY                            
         DC    F'0'                NOT USED                                     
T3189CA  DS    (40*1024)C          OVERLAY STORAGE                              
T3189CX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001NENAV9C   04/25/16'                                      
         END                                                                    
