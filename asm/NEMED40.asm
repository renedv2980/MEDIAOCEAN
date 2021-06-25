*          DATA SET NEMED40    AT LEVEL 031 AS OF 08/10/00                      
*PHASE T31E40A                                                                  
*                                                                               
         TITLE 'T31E40 - EDIT FOR UNIT LIST'                                    
*****************************************************************               
* T31E40 (NEMED40) - THIS EDITS THE UNITLIST SCREEN AND READS THE               
*                     APPROPRIATE PACKAGE RECORD.                               
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS             
*                           MANY USEFUL ADDRESSES, DATA                         
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.                 
*                      MANY FIELDS FILLED IN BY NETIO.                          
*           IO1 - SECOND I/O AREA. CONTAINS THE CLIENT RECORD.                  
*                                                                               
* GLOBALS - R2 - POINTS TO CURRENT FIELD ON SCREEN                              
*                                                                               
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
NEMED40  CSECT                                                                  
         NMOD1 0,**ULED**                                                       
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
*                                                                               
         LA    R2,UNLCLIH          CLIENT (REQUIRED. NO SPEC CASES)             
         NETGO NVCLI,DMCB,UNLCLIN      AND FILL IN UNLCLIN.                     
*                                  (RETURNS CLIENT RECORD AT AIO2)              
         OI    UNLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         LA    R2,UNLPROH          PRODUCT (REQUIRED. NO SPEC CASES)            
         NETGO NVPRD,DMCB,UNLPRON      AND FILL IN UNLPRON.                     
         OI    UNLPRONH+6,X'80'    TRANSMIT PRON                                
*                                                                               
         LA    R2,UNLESTH          ESTIMATE (REQUIRED. NO SPEC CASES)           
         NETGO NVEST,DMCB,UNLESTN      AND FILL IN UNLESTN.                     
         OI    UNLESTNH+6,X'80'    TRANSMIT ESTN                                
*                                                                               
         LA    R2,UNLNETH          NETWORK (REQUIRED. 'ALL' ALLOWED)            
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,UNLPAKH          PACKAGE (REQUIRED. NO SPEC CASES)            
         NETGO NVPAK,DMCB,UNLPAKN      AND FILL IN UNLPAKN.                     
         OI    UNLPAKNH+6,X'80'    TRANSMIT PAKN                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL FIELDS                              
         LA    R2,UNLSEQH          SEQUENCE                                     
         NETGO NVPSEQ,DMCB                                                      
*                                                                               
         LA    R2,UNLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
*                                                                               
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
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
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE0D                                                       
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031NEMED40   08/10/00'                                      
         END                                                                    
