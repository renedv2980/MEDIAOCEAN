*          DATA SET NEMED0C    AT LEVEL 012 AS OF 09/07/10                      
*PHASE T31E0CA                                                                  
         TITLE 'T31E0C - EDIT FOR FIS'                                          
T31E0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FIED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          PASS ARGS TO EDIT IN W/S 1                   
*        L     R7,ANETWS2          PASS ARGS TO EDIT IN W/S 2                   
         USING WSD,R7                                                           
         EJECT                                                                  
*              INITIALIZE NETBLOCK                                              
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   NBDATA,C'P'         WILL WANT PACKAGES FIRST                     
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
*        NETGO NVCLI,DMCB,SPLCLIN,ANETWS4                                       
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIN                                
         L     R0,ANETWS3      MOVE CLIENT REC TO ANETWS3                       
         ST    R0,NBACLI                                                        
         L     RE,NBAIO                                                         
         USING CLTHDR,RE                                                        
         LH    R1,CLEN                                                          
         DROP  RE                                                               
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON   FILL IN PRODUCT NAME                        
         OI    SPLPRONH+6,X'80'    TRANSMIT PRON                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL. NO RESPONSE =ALL.                  
         LA    R2,SPLESTH          ESTIMATE.                                    
         NETGO NVESTRNG,DMCB,SPLESTN     FILL IN EST NAME. ALLOW RANGE          
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH          DAYPART                                      
         NETGO NVDPT,DMCB,SPLDPTN                                               
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'    XMIT PACKAGE NAME                            
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE. OPTIONAL.                        
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL.                          
*                                   CKS THAT END DATE IS NOT B4 STRT            
*              EDIT OPTIONS                                                     
         SPACE 3                                                                
ED3B     LA    R2,SPLOPTH                                                       
         MVI   NETOPT,C'N'         SET DEFAULTS                                 
         MVI   INTOPT,C'+'                                                      
         CLI   5(R2),0                                                          
         BE    ED8                                                              
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    EDERR                                                            
         LA    R3,BLOCK                                                         
         SPACE 1                                                                
ED4      CLC   12(4,R3),=C'NET '                                                
         BNE   *+12                                                             
         MVI   NETOPT,C'Y'                                                      
         B     ED5                                                              
         SPACE 1                                                                
         CLC   12(4,R3),=C'INT '                                                
         BNE   *+12                                                             
         MVI   INTOPT,C'I'                                                      
         B     ED5                                                              
         SPACE 1                                                                
         CLC   12(4,R3),=C'-INT'                                                
         BNE   *+12                                                             
         MVI   INTOPT,C'-'                                                      
         B     ED5                                                              
         SPACE 1                                                                
         CLC   12(4,R3),=C'+INT'                                                
         BNE   *+12                                                             
         MVI   INTOPT,C'+'                                                      
         B     ED5                                                              
         B     EDERR                                                            
         SPACE 1                                                                
ED5      LA    R3,32(R3)                                                        
         BCT   R0,ED4                                                           
         B     ED8                                                              
*                                                                               
EDERR    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
*                                                                               
ED8      LA    R2,SPLCLIH                                                       
         SPACE 1                                                                
         B     XMOD                                                             
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFCD                                                       
*                                                                               
WSD      DSECT                                                                  
*** PASSED TO PRINT                                                             
DPFILT   DS    CL1                                                              
NETOPT   DS    CL1                                                              
INTOPT   DS    CL1                                                              
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEMED0C   09/07/10'                                      
         END                                                                    
