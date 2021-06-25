*          DATA SET SPREPTR02  AT LEVEL 016 AS OF 05/01/02                      
*PHASE SPTR02A,*                                                                
*                                                                               
         TITLE 'SPTR02 - CREATE TRAFFIC BILLING REPORT'                         
**********************************************************************          
* THIS VERSION OF PROGRAM PRINTS A BILLING REPORT FOR ENTIRE SPTFILE *          
**********************************************************************          
SPTR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPTR02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPTR02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         STM   R9,RC,SPTRR9                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP10                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
SP10     LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         MVC   TODAY+4(2),=C'01'                                                
         MVC   SVSTART,TODAY                                                    
         GOTO1 DATCON,(R1),(0,SVSTART),(3,ESTSTB)                               
         GOTO1 ADDAY,(R1),SVSTART,SVEND,F'31'                                   
SP14     GOTO1 (RF),(R1),SVEND,SVEND,F'-1'                                      
         CLC   SVSTART(4),SVEND                                                 
         BNE   SP14                                                             
         GOTO1 DATCON,DMCB,(0,SVEND),(3,ESTNDB)                                 
         XC    SW,SW               RESET ALL SWITCHES                           
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),=X'0A2411'   SET FOR 1ST INSTR KEY                        
         SPACE                                                                  
* DO FIRST READ                                                                 
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             PASS RECORD LENGTH, COMMAND         C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
         B     SP24                                                             
*                                                                               
SP20     GOTO1 DATAMGR,DMCB,                                           C        
               DMRSEQ,             PASS RECORD LENGTH, COMMAND         C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
*                                                                               
SP24     CLC   KEY(2),=X'0A24'     TEST E-O-F                                   
         BE    SP30                                                             
         GOTO1 AENDREQ                                                          
SP30     MVC   WORK(3),KEY                                                      
         NI    WORK+2,X'F0'        FORCE MEDIA BLANK                            
         CLC   WORK(3),COMPKEY     SAME A                                       
         BE    SP40                YES                                          
         MVI   AGYSW,0             RESET AGENCY SW                              
         MVI   CLTSW,0             RESET AGENCY SW                              
         MVI   FORCEHED,C'Y'       FORCE HEADING                                
*                                                                               
         MVC   COMPKEY,WORK        SAVE A-M/CLT/PRD/MKT/STA/EST                 
*                                                                               
SP40     MVI   ELCODE,X'10'                                                     
         L     R6,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,GETREC,SPTFILE,KEY+14,(R6),DMWORK                   
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   SAVEAGY,20(R6)      SAVE ALPHA AGY CODE                          
         SPACE                                                                  
         LA    R6,24(,R6)          POINT TO 1ST ELEMENT                         
         CLI   0(R6),X'10'                                                      
         BE    SP52                                                             
SP50     BAS   RE,NEXTEL                                                        
         BNE   SP20                END OF RECORDD                               
         USING INSDTAEL,R6                                                      
SP52     CLC   INSDATE,ESTSTB      IF BEFORE START                              
         BL    SP50                BYPASS                                       
         CLC   INSDATE,ESTNDB      IF AFTER END                                 
         BH    SP50                BYPASS                                       
*                                                                               
* READ NEW CLTHDR *                                                             
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SAVEKEY+2                                               
         MVC   WORK(13),KEY                                                     
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             DIR READ HIGH                       C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
         MVC   CLTNM,=CL24'** UNKNOWN CLIENT **'                                
         CLC   KEY(13),WORK                                                     
         BNE   SP54                                                             
         L     R6,ADCLT                                                         
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,                                                 C        
               SPTFILE,            ADDR OF FILE                        C        
               KEY+14,             KEY ADDRESS                         C        
               (R6),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
*                                                                               
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVC   CLTNM,CNAME                                                      
SP54     GOTO1 CLUNPK,DMCB,SAVEKEY+3,CLT                                        
*                                                                               
         CLI   AGYSW,1             IS AGENCY READ IN YET                        
         BE    SP56                YES                                          
         XC    KEY,KEY                                                          
         MVI   KEY,06                                                           
         MVC   KEY+1(2),SAVEAGY                                                 
         MVC   WORK(13),KEY                                                     
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             DIR READ HIGH                       C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
         MVC   AGYNM,=CL33'** UNKNOWN AGENCY **'                                
         MVI   AGYSW,1             IS AGENCY READ IN YET                        
         CLC   KEY(13),WORK                                                     
         BNE   SP56                                                             
         L     R6,ADAGY                                                         
         GOTO1 DATAMGR,DMCB,                                           C        
               GETREC,                                                 C        
               SPTFILE,            ADDR OF FILE                        C        
               KEY+14,             KEY ADDRESS                         C        
               (R6),               WORK AREA ADDRESS                   C        
               DMWORK                                                           
         USING AGYHDRD,R6                                                       
         MVC   AGYNM,AGYNAME                                                    
         MVC   AGYADR,AGYADDR                                                   
SP56     MVC   PAGY,SAVEAGY                                                     
         MVC   WORK(1),SAVEKEY+2                                                
         NI    WORK,X'0F'                                                       
         ZIC   R1,WORK                                                          
         LA    R1,MEDIAS(R1)                                                    
         MVC   PMED,0(R1)                                                       
         MVC   PCLT,CLT                                                         
         MVC   PCLTNM,CLTNM                                                     
*                                                                               
         GOTO1 REPORT                                                           
         MVC   KEY,SAVEKEY         RESTORE DIRECTORY FOR SEQ READ               
         MVI   KEY+5,255           FORCE TO NEXT CLIENT                         
         GOTO1 DATAMGR,DMCB,                                           C        
               DMRDHI,             DIR READ HIGH                       C        
               SPTDIR,             ADDR OF FILE                        C        
               KEY,                KEY ADDRESS                         C        
               KEY                 WORK AREA ADDRESS                            
*                                                                               
         B     SP24                                                             
*                                                                               
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
NEXTEL2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* HEADLINE HOOK                                                                 
*                                                                               
         DS    0D                                                               
         USING *,RF                                                             
HDHK     NTR1                                                                   
         LM    R9,RC,SPTRR9                                                     
         B     HDHK2                                                            
*                                                                               
SPTRR9   DC    4F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SVSTART),(8,H3+44)                                
         MVC   H3+55(2),=C'TO'                                                  
         GOTO1 DATCON,DMCB,(0,SVEND),(8,H3+59)                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
COMPKEY  DS    CL3                                                              
SAVEKEY  DS    CL18                                                             
SAVEAGY  DS    CL2                                                              
MEDIAS   DC    C'*TRN************'                                              
ELCODE   DS    CL1                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
*        PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
*                                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
         DS    CL4                                                              
PAGY     DS    CL2                                                              
         DS    CL7                                                              
PMED     DS    CL1                                                              
         DS    CL5                                                              
PCLT     DS    CL3                                                              
         DS    CL3                                                              
PCLTNM   DS    CL20                                                             
         ORG                                                                    
 END                                                                            
