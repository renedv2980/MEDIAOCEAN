*          DATA SET NEMED43    AT LEVEL 019 AS OF 08/10/00                      
*PHASE T31E43A                                                                  
         TITLE 'T31E43 - EDIT FOR PROGRAM LIST'                                 
T31E43   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PRED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 TO PASS ARGS TO PRINT         
         USING PROGCOM,R7                                                       
*                                  USE W/S AREA 3 TO PASS MKTS TO PRINT         
         EJECT                                                                  
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          FIELDS REQUIRED                              
         LA    R2,PRGNETH                                                       
         NETGO NVNETALL,DMCB,PAKMKT    VALIDATE NETWORK. GET PACKED MKT         
         OC    PAKMKT,PAKMKT       MARKET=ALL                                   
         BNZ   ED2                                                              
         CLC   CONWHEN(2),=C'ON'   ONLY OVERNIGHT                               
         BE    ED2                                                              
         CLC   CONWHEN(2),=C'OV'                                                
         BE    ED2                                                              
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
*                                                                               
ED2      LA    R2,PRGSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
         MVC   STRTFILT,NBSELSTR   SAVE THE EBCDIC DATE                         
*                                                                               
         LA    R2,PRGENDH                                                       
         NETGO NVENDDAT,DMCB                                                    
         MVC   ENDFILT,NBSELEND    SAVE EBCDIC END DATE                         
*                                                                               
         LA    R2,PRGDAYH                                                       
         NETGO NVDAY,DMCB,DAYFILT                                               
*                                                                               
         MVI   FTERMFLG,1          DAYPART FILTER                               
         LA    R2,PRGDPTH                                                       
         MVI   DPTFILT,0                                                        
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDX                                                              
         NETGO NVDPT,DMCB,PRGDPTN                                               
         MVC   DPTFILT,FLD                                                      
         SPACE 2                                                                
EDX      XIT1                                                                   
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE3D                                                       
PROGCOM  DSECT                                                                  
*** COMMON WITH PRINT                                                           
PAKMKT   DS    CL2                                                              
STRTFILT DS    CL6                                                              
ENDFILT  DS    CL6                                                              
DAYFILT  DS    CL1                                                              
DPTFILT  DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019NEMED43   08/10/00'                                      
         END                                                                    
