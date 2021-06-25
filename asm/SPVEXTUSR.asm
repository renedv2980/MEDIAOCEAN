*          DATA SET SPVEXTUSR  AT LEVEL 002 AS OF 07/25/08                      
*PHASE PVEXTUSA                                                                 
******** LIVE PHASE NAME IS PVEXTUSR                                            
***********************************************************************         
*                                                                     *         
* THIS IS THE PANVALET USER EXIT CONTROL MODULE. IT DEFINES THE       *         
* LOAD MODULE NAMES AND FUNCTIONS OF USER-PROVIDED EXITS.             *         
*                                                                     *         
* THIS MODULE MUST BE LINKED LIVE WITH MEMBER NAME PVEXTUSR. THIS IS  *         
* REQUIRED BY PANVALET. THE LOAD MODULE MUST RESIDE IN THE SAME       *         
* LOAD LIBRARY AS OTHER PANVALET SYSTEM LOAD MODULES. WE CURRENTLY    *         
* KEEP IT IN 'SYS1.PAN.LINKLIB'.                                      *         
*                                                                     *         
* SEE THE PANVALET DOCUMENTATION FOR MORE DETAILS.                    *         
*                                                                     *         
***********************************************************************         
PVEXTUSR CSECT                                                                  
         EXITDEF BEGIN                                                          
         EXITDEF NAME=PVRACF,TYPE=SECURITY,EVENTS=($MEM001)                     
         EXITDEF END                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPVEXTUSR 07/25/08'                                      
         END                                                                    
