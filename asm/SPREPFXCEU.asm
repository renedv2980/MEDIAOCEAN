*          DATA SET SPREPFXCEU AT LEVEL 001 AS OF 06/12/95                      
*PHASE SPFX02V                                                                  
         TITLE 'SPFX02 - SET USER FIELDS FOR WITO'                              
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       XC    KEY,KEY                                                          
         MVI   KEY+1,X'21'         FIRST CLIENT RECORD FOR WITO                 
         GOTO1 HIGH                                                             
                                                                                
FX20     CLI   KEY+1,X'22'         ONLY DO TV & RADIO                           
         BH    FXEND                                                            
         OC    KEY+4(9),KEY+4                                                   
         BNZ   FX40                                                             
                                                                                
         L     R6,ADCLT                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING CLTHDR,R6                                                        
         MVC   CEU1(24),CEU1WILA                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX30                                                             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE',KEY+14,(R6)                   
FX30     GOTO1 HEXOUT,DMCB,(R6),P+8,13,=C'TOG'                                  
         GOTO1 HEXOUT,DMCB,1048(R6),P+35,25,=C'TOG'                             
         GOTO1 CLUNPK,DMCB,2(R6),P                                              
         GOTO1 REPORT                                                           
                                                                                
FX40     MVI   KEY+4,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX20                                                             
FXEND    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
CEU1WILA DC    C'SUPERVISOR/OFFICE   ',X'0020C000'                              
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
STAWORK  DS    XL31                                                             
BAGYTAB  DS    16XL4                                                            
         DS    F                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
                                                                                
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXCEU06/12/95'                                      
         END                                                                    
