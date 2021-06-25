*          DATA SET NEMED0F    AT LEVEL 014 AS OF 08/10/00                      
*PHASE T31E0FA                                                                  
*                                                                               
         TITLE 'T31E0F - EDIT FOR EGS REPORT'                                   
*****************************************************************               
* T31E0F (NEMED0F) - TEST REPORT.                                               
*                                                                               
* OUTPUT-   IO1 - SECOND I/O AREA. CONTAINS THE CLIENT RECORD.                  
*                                                                               
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
NEMED0F  CSECT                                                                  
         NMOD1 0,**EGED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R2,ANETWS1          USE 1ST W/S AREA TO PASS CLIENT              
         ST    R2,NBACLI           I/O AREA TO SAVE CLIENT RECORD               
*                                    TO PASS TO PRINT MODULE                    
         MVI   FTERMFLG,1          ALL ARGS OPTIONAL                            
*                                                                               
         LA    R2,UNLCLIH          CLIENT                                       
         NETGO NVCLIALL,DMCB,UNLCLIN       FILL IN UNLCLIN.                     
*                                  (RETURNS CLIENT RECORD AT AIO2)              
         OI    UNLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
EGSPG    LA    R2,UNLPGRPH         PRODUCT GROUP                                
         NETGO NVGETFLD,DMCB                                                    
         BZ    EGSPRD                                                           
         MVC   NBSELPGR,FLD        IF PROD GROUP, SKIP PROD                     
         MVC   NBSELPRD,=C'POL'                                                 
         B     EGSEST                                                           
*                                                                               
*                                                                               
EGSPRD   LA    R2,UNLPROH           PRODUCT                                     
         NETGO NVPRDALL,DMCB,UNLPRON     FILL IN UNLPRON.                       
         OI    UNLPRONH+6,X'80'    TRANSMIT PRON                                
*                                                                               
EGSEST   LA    R2,UNLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,UNLESTN      FILL IN UNLESTN.                      
         OI    UNLESTNH+6,X'80'    TRANSMIT ESTN                                
*                                                                               
         LA    R2,UNLEFLTH         ESTIMATE FILTERS                             
         NETGO NVEFILT,DMCB                                                     
*                                                                               
         LA    R2,UNLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,UNLPAKH          PACKAGE (REQUIRED. NO SPEC CASES)            
         NETGO NVPAKLOK,DMCB,UNLPAKN      AND FILL IN UNLPAKN.                  
         OI    UNLPAKNH+6,X'80'    TRANSMIT PAKN                                
*                                                                               
         LA    R2,UNLPRGH          PROGRAM                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EGSSTR                                                           
         MVC   NBSELPRG(6),FLD                                                  
*                                                                               
EGSSTR   LA    R2,UNLSTRH          START DATE                                   
         NETGO NVGETFLD,DMCB       ALLOW NO DATES EVEN IF NO EST GIVEN          
         BZ    EGSADAT                                                          
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,UNLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
EGSADAT  XC    EBCDAT,EBCDAT                                                    
         LA    R2,UNLADTH          ACTIVITY DATE                                
         NETGO NVGETFLD,DMCB                                                    
         BZ    ENDIT                                                            
         GOTO1 DATVAL,DMCB,(0,FLD),WORK     VALIDATE IT                         
         OC    DMCB(4),DMCB        CHECK FOR ERROR                              
         BZ    GOTERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,EBCDAT)                                  
*                                                                               
*** PROD GROUP. NO NEED TO VALIDATE.                                            
*                                                                               
ENDIT    LA    R2,UNLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
*                                                                               
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
*                                                                               
GOTERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
*                                                                               
         SPACE 2                                                                
OI1T     EQU   X'80'                                                            
*                                                                               
         EJECT                                                                  
***  INCLUDE NETINCLS *******                                                   
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
**** ARGS TO EDIT                                                               
EBCDAT   DS    CL3                 ACTIVITY DATE EBCD FORMAT                    
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMED4FD                                                       
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014NEMED0F   08/10/00'                                      
         END                                                                    
