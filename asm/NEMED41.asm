*          DATA SET NEMED41    AT LEVEL 044 AS OF 12/15/98                      
*PHASE T31E41A,+0                                                               
*                                                                               
         TITLE 'T31E41 - EDIT FOR NETWORK UPDATE'                               
*****************************************************************               
* T31E41          - THIS EDITS THE UNIVLIST SCREEN AND                          
*                    READS THE APPROPRIATE UNIVERSE RECORD.                     
*                                                                               
* INPUTS - PARAMETER 1 - LOCATION OF THE GEND SPOOL DSECT. CONTAINS             
*                           MANY USEFUL ADDRESSES, DATA                         
*                                                                               
* OUTPUTS - NETBLOCK - THE BLOCK USED TO READ THE NETWORK FILE.                 
*                      MANY FIELDS FILLED IN BY NETIO.                          
*           IO1 - NEWUNIV ELEM-CODE/NEW INTG/ FLAGS                             
*                                                                               
*                                                                               
*  CALLS TO -                                                                   
*   NVVALID - VALIDATION ROUTINES.                                              
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
T31E41   CSECT                                                                  
         NMOD1 0,**UPED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          W/S TO BE PASSED TO PRINT OVERLAY            
         USING PASDATA,R7                                                       
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
         LA    R2,UVLCLIH          CLIENT (REQUIRED. NO SPEC CASES)             
         NETGO NVCLI,DMCB,UVLCLIN      AND FILL IN UNLCLIN.                     
*                                  (RETURNS CLIENT RECORD AT AIO2)              
         OI    UVLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         LA    R2,UVLESTH          ESTIMATE (REQUIRED. NO SPEC CASES)           
         NETGO NVEST,DMCB,UVLESTN      AND FILL IN UNLESTN.                     
         OI    UVLESTNH+6,X'80'    TRANSMIT ESTN                                
*                                                                               
         LA    R2,UVLNETH          NETWORK (REQUIRED. 'ALL' ALLOWED)            
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         MVI   FTERMFLG,1               SET OPTIONAL FLAG                       
         LA    R2,UVLPAKH             PACKAGE (OPTIONAL)                        
         CLC   UVLPAK(3),=C'ALL'                                                
         BE    EDT03                                                            
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDT03                                                            
         NETGO NVPAK,DMCB,UVLPAKN  ELSE VALIDATE/FILL IN UVLPAKN                
         OI    UVLPAKNH+6,X'80'    TRANSMIT PAKN                                
*                                                                               
EDT03    LA    R2,UVLSTRH          START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,UVLENDH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         MVI   ANYFLG,0                                                         
         MVI   UNVFLG,0                                                         
         LA    R2,UVLUNVH          VALIDATE UNIV CODE                           
         MVI   FTERMFLG,1          OPTIONAL                                     
         NETGO NVGETFLD,DMCB,=F'9999'                                           
         BZ    EDT10                                                            
         MVI   ANYFLG,1            SET OPTIONAL FIELD INPUT FLAG                
         MVI   UNVFLG,C'Y'         SET UNIVERSE FLAG                            
         LTR   R0,R0                                                            
         BNZ   EDT05                                                            
         MVI   ERROR,NOTNUM                                                     
         GOTO1 TRAPERR                                                          
EDT05    CVD   R0,DUB                                                           
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
*                                  TO ISOLATE UNIVERSE CODE                     
         LA    R4,BLOCK            NOW GET UNIV REC                             
         USING GUVD,R4             AND SAVE UNIV ELEM IN NEWUNIVS               
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAGY,NBSELAGY                                                  
         MVC   GUVCODE,DUB+5                                                    
         MVC   NEWUNCD,DUB+5       SET NEW UNV CODE IN W/S                      
         MVC   NEWUNVP,UVLUNV      SET NEW UNIV CD IN PRINT FORM                
         LA    R3,NEWUNIVS                                                      
         ST    R3,GUVAOUT                                                       
         MVC   GUVCMFCS,ACOMFACS                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A32'                                        
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         CLI   GUVERROR,0                                                       
         BNE   EDT8                                                             
         CLI   GUVUTYPE,C'C'                                                    
         BNE   *+8                                                              
         MVI   UNSTFLG,C'Y'                                                     
         B     EDT10                                                            
EDT8     MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR                                                          
*                                                                               
EDT10    MVI   INTGFLG,0                                                        
         LA    R2,UVLINTGH         INTEGRATION NUM                              
         CLI   5(R2),0                                                          
         BE    EDITX                                                            
         MVI   ANYFLG,1                                                         
         ZIC   R4,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,UVLINTG,(R4)                                        
         CLI   0(R1),0                                                          
         BE    EDT12                                                            
         MVI   ERROR,NOTNUM                                                     
         GOTO1 TRAPERR                                                          
EDT12    MVC   NEWINTG,4(R1)       SET INTG NUM                                 
         MVC   NEWINTGP,UVLINTG    SET INTG IN PRINT FORMAT                     
         MVI   INTGFLG,C'Y'        SET INTEGRATION FLAG                         
         SPACE                                                                  
EDITX    DS    0H                                                               
         CLI   ANYFLG,0          TEST IF ANY INPUT IN OPTIONAL FIELDS           
         BNE   EDITA                                                            
         LA    R2,UVLUNVH          IF NO INPUT/ ERROR                           
EDINV    MVI   ERROR,INVALID                                                    
         GOTO1 TRAPERR                                                          
*                                                                               
EDITA    DS    0H                                                               
*                                  DIG OUT ANY OPTIONS                          
         MVI   FROZ,0                                                           
         LA    R2,UVLOPTH          OPTIONS                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDITXX                                                           
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
         SPACE 1                                                                
EDOPT    CLC   12(4,R3),=C'FROZ'  ALLOW FROZEN PACKAGES                         
         BNE   EDOPT2                                                           
         MVC   FROZ,22(R3)                                                      
         CLI   FROZ,C'N'                                                        
         BE    EDT18                                                            
         CLI   FROZ,C'Y'                                                        
         BE    EDT18                                                            
         B     EDINV                                                            
*                                                                               
EDOPT2   DS    0H                                                               
         B     EDINV                                                            
         SPACE 1                                                                
EDT18    LA    R3,32(R3)                                                        
         BCT   R0,EDOPT                                                         
                                                                                
EDITXX   DS    0H                                                               
* - NEED TO HAVE COPIES WRITTERN TO RECOVERY FILE                               
         ICM   R1,15,TWAMASTC                                                   
         BZ    ENDMST                                                           
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
         DROP  R1                                                               
ENDMST   DS    0H                                                               
                                                                                
         LA    R2,UVLCLIH          NORMAL END OF EDIT                           
         B     XMOD                                                             
*                                                                               
*                                                                               
XMOD     XIT1                                                                   
         SPACE 2                                                                
TRAPERR  EQU   ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
*** PASDATA WORKING STORAGE (PASSED TO PRINT OVERLAY) ***                       
*                                                                               
PASDATA  DSECT                                                                  
         DS    0F                                                               
NEWUNIVS DS    CL200                                                            
*                                                                               
NEWINTG  DS    F                                                                
NEWINTGP DS    CL10                INTG IN PRINT FORMAT                         
NEWUNCD  DS    CL2                                                              
NEWUNVP  DS    CL4                 UNIV IN PRINT FORMAT                         
UNVFLG   DS    CL1                                                              
INTGFLG  DS    CL1                                                              
UNSTFLG  DS    CL1                                                              
FROZ     DS    CL1                                                              
*                                                                               
ANYFLG   DS    CL1                                                              
*                                                                               
         EJECT                                                                  
* NETINCLS                                                                      
* DDCOMFACSD                                                                    
* NEGETNUND                                                                     
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
       ++INCLUDE NEGETNUND                                                      
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE1D                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044NEMED41   12/15/98'                                      
         END                                                                    
