*          DATA SET ACGETURL   AT LEVEL 005 AS OF 08/05/20                      
*PHASE GETURLA                                                                  
*                                                                               
**********************************************************************          
*                                                                    *          
* PARAMETERS ARE AS FOLLOWS                                          *          
*                                                                    *          
* P1  BYTE  0     X'00'                                              *          
*     BYTE 1-3    A(URLTAB) - RETURNED FROM ACGETURL                 *          
*                                                                    *          
**********************************************************************          
         TITLE 'GETURL - ACCOUNT FORMAT MODULE'                                 
                                                                                
* YNGX 004 08AUG19 <DSRD-23323> RE-LINK TO INCLUDE NEW ACURLTAB                 
                                                                                
GETURL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*GETURL*                                             
         USING WORKD,RC                                                         
*                                                                               
         MVC   PARMS(PARMSLNQ),0(R1)                                            
*                                                                               
         LA    R1,AGYURL           SET ADDRESSABILITY TO TABLE                  
         L     R2,PAURLTAB         GET ADDR OF STORAGE FOR TABLE ADDR           
         ST    R1,0(R2)                                                         
         J     EXITY                                                            
*                                                                               
EXITN    LHI   RE,0                                                             
         J     EXIT                                                             
EXITY    LHI   RE,1                                                             
EXIT     CHI   RE,1                                                             
EXITCC   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* URL TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
* URLTAB                                                                        
       ++INCLUDE ACURLTAB                                                       
         EJECT                                                                  
***********************************************************************         
* STORAGE DSECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
PARMS    DS    0A                                                               
PAURLTAB DS    AL4                 ADDRESS FOR URLTAB                           
PARMSLNQ EQU   *-PARMS                                                          
*                                                                               
WORKX    EQU   *                                                                
         SPACE 1                                                                
*********************************************************************           
* EQUATES                                                           *           
*********************************************************************           
         SPACE 1                                                                
EOF      EQU   X'FF'                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACGETURL  08/05/20'                                      
         END                                                                    
