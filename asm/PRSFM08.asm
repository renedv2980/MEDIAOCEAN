*          DATA SET PRSFM08    AT LEVEL 049 AS OF 02/25/15                      
*PHASE T41C08A,*                                                                
         TITLE 'T41C08  REGION RECORDS'                                         
*                                                                               
****  CHANGE LOG                                                                
*                                                                               
*                                                                               
*  BPLA  02/15     ERREX2 BUG FIX                                               
*                                                                               
*  BPLA  02/10     ADD BILLING REP FIELD (LIKE DIVISION)                        
*                                                                               
*  SMYE  09/97     TREAT ALL ENTERED IN DIVISION AND/OR REGION LIKE             
*                  NO ENTRY FOR LIST AND REPORT                                 
*                                                                               
*  SMYE  05/06/96  ALTERED ELEM LENGTH AND RECORD LENGTH IN VR PROC             
*  **************  (COMMENTED OUT 09/97)  *************************             
*                                                                               
T41C08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C08                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         BAS   RE,CLRNAME                                                       
*                                                                               
         LA    R2,DIVMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,DIVCLIH          CLIENT                                       
         GOTO1 VALICLT                                                          
         XC    QDIV,QDIV                                                        
         XC    SVDIV,SVDIV                                                      
         LA    R6,SVKEY                                                         
         USING PREGKEY,R6                                                       
         XC    PREGKEY,PREGKEY        CLEAR                                     
         MVC   PREGKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREGKMED,QMED                   MEDIA CODE                       
         MVI   PREGKRCD,X'04'                  ID                               
         MVC   PREGKCLT,QCLT                   CLIENT                           
         LA    R2,DIVDIVH          DIVISION                                     
         CLC   8(3,R2),=C'ALL'     ALL ENTERED IN DIVISION ?                    
         BE    VKD                 YES - TREAT LIKE NO ENTRY                    
         CLI   5(R2),0                                                          
         BNE   VK2                                                              
VKD      CLI   ACTNUM,ACTLIST     NO DIV  - OK IF LIST                          
         BE    VK3                                                              
         CLI   ACTNUM,ACTREP      OK IF REPORT                                  
         BE    VK3                                                              
         CLI   ACTNUM,X'0C'       REPORTING                                     
         BE    VK3                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    VKMISS                                                           
*                                                                               
VK2      GOTO1 VALIDIV                                                          
         CLI   DIVNM,0            MEANS DIVISION NOT FOUND                      
         BE    VKDERR                                                           
         FOUT  DIVDIVNH,DIVNM,20                                                
         MVC   PREGKDIV,QDIV                   DIVISION                         
         MVC   SVDIV,QDIV                                                       
         B     VK3D                                                             
*                                                                               
VK3      DS    0H                                                               
VK3D     XC    SVREG,SVREG                                                      
         XC    QREG,QREG                                                        
         LA    R2,DIVREGH          REGION                                       
         CLC   8(3,R2),=C'ALL'     ALL ENTERED IN REGION ?                      
         BE    VK3F                YES - TREAT LIKE NO ENTRY                    
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
VK3F     CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK6                                                              
         CLI   ACTNUM,ACTREP       NO, OK IF REPORT                             
         BE    VK6                                                              
         CLI   ACTNUM,X'0C'        ALSO REPORTING                               
         BE    VK6                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    VKMISS                                                           
         B     VK4D                                                             
*                                                                               
VK4      DS    0H                                                               
         LA    R2,DIVDIVH        DIVISION REQUIRED IF REGION INPUT              
         CLI   5(R2),0                                                          
         BE    VKMISS                                                           
VK4D     LA    R2,DIVREGH                                                       
         GOTO1 VALIREG                                                          
         CLI   ACTNUM,ACTADD           SEE IF ADDING                            
         BNE   VK5                                                              
         CLI   REGNM,0                 IF NOT ZERO REG EXISTED                  
         BNE   VKDUP                                                            
VK5      FOUT  DIVREGNH,REGNM,20                                                
         MVC   PREGKREG,QREG                                                    
         MVC   SVREG,QREG                                                       
*                                                                               
*                                                                               
VK6      DS    0H                                                               
         MVC   KEY(25),SVKEY       SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKDERR   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
VKMISS   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
VKDUP    MVI   ERROR,DUPLICAT                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PREGREC,R6                                                       
         FOUT  DIVDIVH,PREGKDIV,3                                               
         FOUT  DIVDIVNH,DIVNM,20                                                
         FOUT  DIVREGH,PREGKREG,3                                               
         FOUT  DIVREGNH,PREGNAME,20                                             
*                                                                               
         LA    R2,DIVREGH                                                       
         MVC   DIVREG,SPACES                                                    
         MVC   DIVREG(3),PREGKREG                                               
         MVI   DIVREGH+5,3                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMS                    
         MVC   SVKEY,KEY           SAVE THE RECORD KEY                          
         L     R6,AIO                                                           
         USING PREGREC,R6                                                       
         LA    R2,DIVRNAMH                                                      
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VR99                                                             
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VR50                                                             
         XC    PREGREC(200),PREGREC                                             
*****    XC    PREGREC(67),PREGREC                                              
         MVC   PREGREC(13),KEY                                                  
         MVC   PREGLEN,=H'173'     140 + 33                                     
         MVC   PREGELEM(2),=X'048C'                                             
*****    MVC   PREGLEN,=H'65'       32 + 33     NEW RECORD AND                  
*****    MVC   PREGELEM(2),=X'0420'               ELEMENT LENGTHS               
*                                                                               
VR50     MVC   PREGNAME,DIVRNAM                                                 
         OC    PREGNAME,SPACES                                                  
*                                                                               
         XC    PREGREP,PREGREP     CLEAR REP FIELD                              
         FOUT  DIVREPNH,SPACES,30  CLEAR REP NAME FIELD                         
         LA    R2,DIVREPH                                                       
         CLI   5(R2),0             ANYTHING ?                                   
         BE    VR900               NO - DONE                                    
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         NUMERIC                                      
         BNO   VR99N               NO - INVALID REGION                          
*   MOVE NUMERIC SCREEN FLD TO RIGHT-JUSTIFIED, ZERO-FILLED RECORD FLD          
         MVC   PREGREP,=C'0000'    ZERO FILL                                    
         LA    R3,PREGREP                                                       
         LA    R4,4                MAX LENGTH OF SCREEN FIELD                   
         ZIC   R5,5(R2)            ACTUAL LENGTH OF SCREEN FIELD                
         SR    R4,R5               DISPLACEMENT INTO PREGREP                    
         AR    R3,R4                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),DIVREP      SCREEN TO RECORD FIELD                       
*                                  VALIDATE REP                                 
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         MVC   PREPKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREPKMED,QMED                   MEDIA CODE                       
         MVI   PREPKRCD,X'11'                  ID                               
         MVC   PREPKREP,PREGREP                REP CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VR99NF              ERROR - REP NOT FOUND                        
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     VR900                                                            
*                                                                               
VR99     DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR99N    DS    0H                                                               
         MVI   ERROR,INVREG       INVALID REGION                                
         B     TRAPERR                                                          
*                                                                               
VR99NF   DS    0H                                                               
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VR99C    DS    0H                                                               
         CLI   ERROR,X'FF'         TEST ERROR ALREADY SHOWN                     
         BE    VR99E                                                            
         B     TRAPERR             NO- LET GENCON DO IT                         
*                                                                               
VR99E    GOTO1 ERREX2                                                           
*                                                                               
*NOP*VR900    B     EXIT                                                        
VR900    B     DR                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         MVC   X(4),AIO               SAVE I/O POINTER                          
         XC    DIVRNAM,DIVRNAM                                                  
         FOUT  DIVRNAMH                                                         
*                                                                               
         L     R6,AIO                                                           
         USING PREGREC,R6                                                       
*                                                                               
DR20     DS    0H                                                               
         MVC   DIVRNAM,PREGNAME                                                 
         FOUT  DIVRNAMH                                                         
*                                                                               
         FOUT  DIVREPNH,SPACES,30                                               
         XC    DIVREP,DIVREP                                                    
         FOUT  DIVREPH                                                          
         CLI   PREGREP,C' '        ANY REP CODE ?                               
         BNH   DR900               NO                                           
         MVC   DIVREP,PREGREP                                                   
*                                  READ FOR REP NAME                            
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         MVC   PREPKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREPKMED,QMED                   MEDIA CODE                       
         MVI   PREPKRCD,X'11'                  ID                               
         MVC   PREPKREP,PREGREP                REP CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    DR50                GET RECORD FOR DISPLAY                       
         MVC   DIVREPN(24),=C'*REP RECORD NOT ON FILE*'                         
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     DR900                                                            
DR50     DS    0H                                                               
         MVC   AIO,AIO2            USE AIO2 FOR REP RECORD                      
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         MVC   DIVREPN,PREPNAME                                                 
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         CLI   ACTNUM,ACTADD       ADD ?                                        
         BE    DR900               YES                                          
         GOTO1 GETREC              READ FOR UPDATE                              
*                                                                               
         DROP  R4                                                               
*                                                                               
DR900    DS    0H                                                               
         MVC   AIO,X               RESTORE AIO                                  
         B     EXIT                                                             
         SPACE 3                                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING PREGREC,R6                                                       
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO CHECK VS. KEYSAVE                         
*                                                                               
         MVC   PREGKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREGKCLT,QCLT                   CLIENT                           
         MVC   PREGKMED,QMED                   MEDIA CODE                       
         MVC   PREGKDIV,QDIV                   DIVISION                         
         MVI   PREGKRCD,X'04'                  ID                               
         MVC   PREGKREG,SVREG                                                   
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE     TEST THROUGH CLIENT                           
         BNE   LR900                                                            
         CLI   KEY+3,X'04'                                                      
         BNE   LR900                                                            
         OC    SVDIV,SVDIV         SEE IF DIVISION GIVEN                        
         BZ    LR040                                                            
         CLC   KEY+7(3),SVDIV                                                   
         BNE   LR900                                                            
LR040    GOTO1 GETREC              GET THE REG RECORD                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(14),=C'DIV  REG  NAME'                                    
         MVC   0(3,R5),PREGKDIV                                                 
         MVC   5(3,R5),PREGKREG                                                 
         MVC   10(20,R5),PREGNAME                                               
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR090    DS    0H                  **NB- PRINTREP NOT FULLY CODED               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR020                                                            
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PRINT REGION REPORT                                                           
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING PREGREC,R4                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PREGKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREGKCLT,QCLT                   CLIENT                           
         MVC   PREGKMED,QMED                   MEDIA CODE                       
         MVC   PREGKDIV,QDIV                   DIVISION                         
         MVI   PREGKRCD,X'04'                  ID                               
         MVC   PREGKREG,SVREG                                                   
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(7),KEYSAVE     CHECK THRU CLIENT                             
         BNE   PR110                                                            
         CLI   KEY+3,X'04'                                                      
         BNE   PR110                                                            
         OC    SVDIV,SVDIV          SEE IF DIVISION GIVEN                       
         BZ    PR30C                                                            
         CLC   KEY+7(3),SVDIV                                                   
         BNE   PR110                                                            
*                                                                               
PR30C    CLC   KEY(10),KEYSAVE      SEE IF NEW DIVISION                         
         BE    PR31                                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,NEWDIV                                                        
         MVC   AIO,AIO1             RESET AIO TO AIO1 - REGION REC              
*                                                                               
PR31     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         DROP  R4                                                               
         L     R6,AIO1                                                          
         USING PREGREC,R6                                                       
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND REGION REC                        
         MVC   P1(3),PREGKREG                                                   
         MVC   P1+6(20),PREGNAME                                                
*                                                                               
         OC    PREGREP,PREGREP     DO I HAVE A BILLING REP?                     
         BZ    PR100                                                            
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         MVC   PREPKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREPKMED,QMED                   MEDIA CODE                       
         MVI   PREPKRCD,X'11'                  ID                               
         MVC   PREPKREP,PREGREP                REP CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    PR50                GET RECORD FOR DISPLAY                       
         MVC   P1+30(12),=C'BILLING REP='                                       
         MVC   P1+43(4),PREGREP                                                 
         MVC   P1+49(24),=C'*REP RECORD NOT ON FILE*'                           
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     PR90                                                             
PR50     DS    0H                                                               
         MVC   AIO,AIO2            USE AIO2 FOR REP RECORD                      
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         MVC   AIO,AIO1       MUST RESET AIO - REGION RECORD                    
         MVC   P1+30(12),=C'BILLING REP='                                       
         MVC   P1+43(4),PREGREP                                                 
         MVC   P1+49(L'PREPNAME),PREPNAME                                       
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
*                                                                               
PR90     GOTO1 HIGH     RESTORE SEQ READ                                        
*                                                                               
         MVI   SPACING,2                                                        
*                                                                               
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVC   P1(16),=C'NO RECORDS FOUND'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
CLRNAME  XC    DIVDIVN,DIVDIVN                                                  
         XC    DIVREGN,DIVREGN                                                  
         FOUT  DIVDIVNH                                                         
         FOUT  DIVREGNH                                                         
         BR    RE                  RETURN                                       
*                                                                               
         SPACE 2                                                                
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+15(20),CLTNM                                                  
         MVC   H3+10(3),QCLT                                                    
         MVC   H4(8),=C'DIVISION'                                               
         MVC   H4+15(20),DIVNM                                                  
         L     R6,AIO                                                           
         USING PREGREC,R6                                                       
         MVC   H4+10(3),PREGKDIV                                                
         DROP  R6                                                               
*                                                                               
*                                                                               
HOOKX    B     EXIT                                                             
         EJECT                                                                  
NEWDIV   NTR1                                                                   
         MVC   SAVEKEY,KEY                                                      
         MVI   KEY+3,X'03'                                                      
         XC    KEY+10(10),KEY+10                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'               MISSING DIVISION                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PDIVREC,R6                                                       
         MVC   DIVNM,PDIVNAME                                                   
         DROP  R6                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H1,52,C' PRINT REGION REPORT'                                    
         SSPEC H2,52,C' -------------------'                                    
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CODE  REGION NAME'                                        
         SSPEC H8,1,C'----  -----------'                                        
         DC    X'00'                                                            
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFME8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF8D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
SVCOMP   DS    X                                                                
SVDIV    DS    CL3                                                              
SVREG    DS    CL3                                                              
X        DS    XL100                                                            
SAVEKEY  DS    CL32                                                             
*                                                                               
         EJECT                                                                  
DIVHDRD  DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
REGHDRD  DSECT                                                                  
       ++INCLUDE PREGREC                                                        
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049PRSFM08   02/25/15'                                      
         END                                                                    
