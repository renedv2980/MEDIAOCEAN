*          DATA SET ACGETAUL   AT LEVEL 007 AS OF 10/27/14                      
*PHASE GETAULA                                                                  
*                                                                               
**********************************************************************          
*                                                                    *          
* PARAMETERS ARE AS FOLLOWS                                          *          
*                                                                    *          
* P1  BYTE  0     X'00'                                              *          
*     BYTE 1-3    A(URLTAB) - RETURNED FROM ACGETURL                 *          
*                                                                    *          
**********************************************************************          
         TITLE 'GETAUL - GET AURA URL'                                          
GETAUL   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,*GETAUL*                                             
         USING WORKD,RC                                                         
*                                                                               
         MVC   PARMS(PARMSLNQ),0(R1)                                            
*                                                                               
         LA    R1,AURURL           SET ADDRESSABILITY TO TABLE                  
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
* AURURL                                                                        
       ++INCLUDE ACAURTAB                                                       
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
**PAN#1  DC    CL21'007ACGETAUL  10/27/14'                                      
         END                                                                    
