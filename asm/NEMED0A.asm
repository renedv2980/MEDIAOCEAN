*          DATA SET NEMED0A    AT LEVEL 004 AS OF 08/10/00                      
*PHASE T31E0AA                                                                  
*                                                                               
         TITLE 'T31E0A - EDIT FOR NET ARMY INTERFACE TAPE'                      
**********************************************************************          
* NEMED0A(T31E0A) - THIS EDITS NETARMY INTERFACE TAPE SCREEN         *          
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS  *          
*                           MANY USEFUL ADDRESSES, DATA              *          
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.      *          
*                      MANY FIELDS FILLED IN BY NETIO.               *          
*                                                                               
*                                                                               
*  CALLS TO -                                                        *          
*   NVVALID - VALIDATION ROUTINES.                                   *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
T31E0A   CSECT                                                                  
         NMOD1 0,**NARM**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         ST    R2,RELO                                                          
         EJECT                                                                  
*************  INITIALIZE NETBLOCK*************                                 
*                             ASSUMES NETBLOCK IS ALREADY INITIALIZED           
*                             DONE BY CALL TO NVAGY OR NVAGYOUT                 
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                    NEEDED IF ALREADY VALIDATED BITS           
*                                    NOT USED                                   
*                                    TO PASS TO PRINT MODULE                    
*                                                                               
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLI,DMCB,SPLCLIN      FILL IN NAME                             
         OI    SPLCLINH+6,X'80'           TRANSMIT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRD,DMCB,SPLPRON      AND FILL IN NAME                         
         OI    SPLPRONH+6,X'80'           TRANSMIT PRODUCT NAME                 
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         MVI   FTERMFLG,1                 SET OPTIONAL FLAG                     
         NETGO NVESTRNG,DMCB,SPLESTN      AND FILL IN NAME                      
         OI    SPLESTNH+6,X'80'           TRANSMIT NAME                         
*                                                                               
*                                                                               
EDT10    DS    0H                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE                                   
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE                                     
         SPACE 3                                                                
*                                                                               
EDITXX   LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
         SPACE                                                                  
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
MYWORKD  DSECT                     *** MY WORK DSECT USING ANETWS2 ***          
RELO     DS    F                                                                
*                                                                               
* NETINCLS                                                                      
* DDCOMFACSD                                                                    
* NEGETNUND                                                                     
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF7D                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEMED0A   08/10/00'                                      
         END                                                                    
