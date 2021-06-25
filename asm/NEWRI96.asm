*          DATA SET NEWRI96    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T32096A                                                                  
         TITLE 'T32096 - CUTIN EDIT'                                            
T3296A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*NECT*,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS2                                                       
         USING MYWORKD,R7                                                       
         LR    RE,R7                                                            
         LA    RF,WORKLENE                                                      
         XCEF                                                                   
         L     R6,ANETWS4                                                       
         ST    R6,ACLIST                                                        
         ST    R2,RELO                                                          
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   *+8                                                              
         BAS   RE,EDITMOD                                                       
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
EDITMOD  NTR1                                                                   
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         L     R4,ACLIST                                                        
         NETGO NVCLI,DMCB,SPLCLIN,(R4)                                          
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         NETGO NVPRDALL,DMCB,SPLPRON                                            
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         NETGO NVNETALL,DMCB,0                                                  
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLPKGH                PACKAGE                                
         NETGO NVPAKLOK,DMCB,0                                                  
*                                                                               
         LA    R2,SPLDPTH                DAYPART                                
         NETGO NVDPTALL,DMCB,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH               START DATE                             
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH               END DATE                               
         NETGO NVENDDAT,DMCB                                                    
         SPACE                                                                  
*                                                                               
*                                                                               
         LA    R2,SPLTITLH                    TITLE                             
         NETGO NVGETFLD,DMCB                                                    
         BZ    EDOPT                                                            
         MVC   TITLE,FLD                                                        
         MVI   QTITLE,C'Y'                                                      
                                                                                
EDOPT    DS    0H                                                               
         LA    R2,SPLOPTH                    OPTION                             
         CLI   5(R2),0                                                          
         BE    EDTX                                                             
         CLC   =C'TRAFFIC',SPLOPT                                               
         BNE   EDINV                                                            
         MVI   TRAFPRD,C'Y'                                                     
*                                                                               
EDTX     LA    R2,SPLCLIH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
EDERR    GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
               LTORG                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
MYWORKD  DSECT                                                                  
QTITLE   DS    CL1                 *** PASSED TO PRINT MODULE                   
TRAFPRD  DS    CL1                 ***                                          
TITLE    DS    CL40                ***                                          
ACLIST   DS    F                   ***                                          
*                                                                               
RELO     DS    F                                                                
*                                                                               
WORKLENE EQU   *-QTITLE                                                         
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEWRI96   05/01/02'                                      
         END                                                                    
