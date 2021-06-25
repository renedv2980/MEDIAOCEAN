*          DATA SET PRSFM07    AT LEVEL 034 AS OF 02/25/15                      
*PHASE T41C07A                                                                  
         TITLE 'T41C07  DIVISION RECORDS'                                       
*                                                                               
**** CHANGE LOG                                                                 
*                                                                               
*  BPLA  02/15     ERREX2 BUG FIX                                               
*                                                                               
*  BPLA  03/10     DISPLAY BILLING REP WHEN REPORTING                           
*                                                                               
*  SMYE  06/05     ADD REP CODE TO RECORD                                       
*                                                                               
*  SMYE  09/97     TREAT ALL IN DIVISION LIKE NO ENTRY FOR LST AND REP          
*                                                                               
*  SMYE  05/01/96  ALTERED ELEM LENGTH AND RECORD LENGTH IN VR PROC             
*                                                                               
*                                                                               
T41C07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C07                                                         
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
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,DIVMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,DIVCLIH          CLIENT                                       
         GOTO1 VALICLT                                                          
         LA    R6,SVKEY                                                         
         USING PDIVKEY,R6                                                       
         XC    PDIVKEY,PDIVKEY        CLEAR                                     
         MVC   PDIVKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDIVKCLT,QCLT                   CLIENT                           
         MVC   PDIVKMED,QMED                   MEDIA CODE                       
         MVI   PDIVKRCD,X'03'                  ID                               
*                                                                               
         XC    SVDIV,SVDIV                                                      
         XC    DIVDIVN,DIVDIVN                                                  
         FOUT  DIVDIVNH                                                         
         LA    R2,DIVDIVH          DIVISION                                     
         CLC   8(3,R2),=C'ALL'     ALL INPUT ?                                  
         BE    VK3D                YES - TREAT LIKE NO INPUT                    
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK4                                                              
VK3D     CLI   ACTNUM,ACTLIST      NO, OK IF LIST                               
         BE    VK6                                                              
         CLI   ACTNUM,X'0C'        OK IF REPORTING                              
         BE    VK6                                                              
         CLI   ACTNUM,ACTADD       SEE IF ADD                                   
         BE    VKMISS              MISSING                                      
*                                                                               
VK4      DS    0H                                                               
         GOTO1 VALIDIV                                                          
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VK5                                                              
         CLI   DIVNM,0             MUST NOT FIND RECORD                         
         BNE   VKADDE              DUPLICATE KEY ON ADD                         
         B     VK5E                                                             
VK5      CLI   DIVNM,0                                                          
         BE    VKINVD              IF NOT ADD MUST EXIST                        
VK5E     FOUT  DIVDIVNH,DIVNM,20                                                
         MVC   SVDIV,QDIV                                                       
         MVC   PDIVKDIV,QDIV                                                    
*                                                                               
VK6      DS    0H                                                               
         MVC   KEY(25),SVKEY       SET KEY                                      
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKMISS   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
VKADDE   MVI   ERROR,DUPLICAT                                                   
         B     TRAPERR                                                          
VKINVD   MVI   ERROR,INVDIV                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PDIVREC,R6                                                       
         FOUT  DIVDIVH,PDIVKDIV,3                                               
         FOUT  DIVDIVNH,PDIVNAME,20                                             
*                                                                               
         LA    R2,DIVDIVH                                                       
         MVC   DIVDIV,SPACES                                                    
         MVC   DIVDIV(3),PDIVKDIV                                               
         MVI   DIVDIVH+5,3                                                      
*        MVC   X(4),AIO1           SAVE AIO1 POINTER                            
*        MVC   AIO1,AIO2           LET VALIDIV USE AIO2                         
*        GOTO1 VALIDIV                                                          
*        MVC   AIO1,X              RESTORE AIO1 POINTER                         
*        MVC   AIO,X               RESTORE AIO                                  
*                                                                               
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
         USING PDIVREC,R6                                                       
         CLI   ACTNUM,ACTADD      SEE IF ADDING                                 
         BNE   VR20                                                             
         XC    PDIVREC(200),PDIVREC                                             
         MVC   PDIVREC(10),KEY                                                  
         MVC   PDIVLEN,=H'173'    140+33                                        
         MVC   PDIVELEM(2),=X'038C'                                             
*****    MVC   PDIVLEN,=H'65'      32+33      NEW RECORD AND                    
*****    MVC   PDIVELEM(2),=X'0320'             ELEMENT LENGTHS                 
*                                                                               
VR20     LA    R2,DIVDNAMH                                                      
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0             MUST BE INPUT                                
         BE    VR99                                                             
         MVC   PDIVNAME,DIVDNAM                                                 
         OC    PDIVNAME,SPACES                                                  
*                                                                               
         XC    PDIVREP,PDIVREP     CLEAR REP FIELD                              
         LA    R2,DIVREPH                                                       
         CLI   5(R2),0             ANYTHING ?                                   
         BE    VR900               NO - DONE                                    
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         NUMERIC                                      
         BNO   VR99                NO - ERROR                                   
*   MOVE NUMERIC SCREEN FLD TO RIGHT-JUSTIFIED, ZERO-FILLED RECORD FLD          
         MVC   PDIVREP,=C'0000'    ZERO FILL                                    
         LA    R3,PDIVREP                                                       
         LA    R4,4                MAX LENGTH OF SCREEN FIELD                   
         ZIC   R5,5(R2)            ACTUAL LENGTH OF SCREEN FIELD                
         SR    R4,R5               DISPLACEMENT INTO PDIVREP                    
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
         MVC   PREPKREP,PDIVREP                REP CODE                         
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VR99                ERROR - REP NOT FOUND                        
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     VR900                                                            
*                                                                               
VR99     DS    0H                                                               
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
         MVC   X(4),AIO            SAVE AIO POINTER                             
         XC    DIVDNAM,DIVDNAM                                                  
         FOUT  DIVDNAMH                                                         
*                                                                               
         L     R6,AIO                                                           
         USING PDIVREC,R6                                                       
*                                                                               
DR20     DS    0H                                                               
         MVC   DIVDNAM,PDIVNAME                                                 
         FOUT  DIVDNAMH                                                         
         FOUT  DIVREPNH,SPACES,30                                               
         XC    DIVREP,DIVREP                                                    
         FOUT  DIVREPH                                                          
         CLI   PDIVREP,C' '        ANY REP CODE ?                               
         BNH   DR900               NO                                           
         MVC   DIVREP,PDIVREP                                                   
*                                  READ FOR REP NAME                            
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         MVC   PREPKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREPKMED,QMED                   MEDIA CODE                       
         MVI   PREPKRCD,X'11'                  ID                               
         MVC   PREPKREP,PDIVREP                REP CODE                         
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
         USING PDIVREC,R6                                                       
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PDIVKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDIVKCLT,QCLT                   CLIENT                           
         MVC   PDIVKMED,QMED                   MEDIA CODE                       
         MVI   PDIVKRCD,X'03'                  ID                               
         MVC   PDIVKDIV,SVDIV                                                   
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(7),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
         GOTO1 GETREC              GET THE DIV RECORD                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    LR090                                                            
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(11),=C'CODE  NAME '                                       
         MVC   0(3,R5),PDIVKDIV                                                 
         MVC   6(20,R5),PDIVNAME                                                
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
* PRINT DIVISION REPORT                                                         
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+15(20),CLTNM                                                  
         MVC   H3+10(3),QCLT                                                    
*                                                                               
*                                                                               
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING PDIVREC,R4                                                       
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   PDIVKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PDIVKCLT,QCLT                   CLIENT                           
         MVC   PDIVKMED,QMED                   MEDIA CODE                       
         MVI   PDIVKRCD,X'03'                  ID                               
         MVC   PDIVKDIV,SVDIV                                                   
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
PR30     CLC   KEY(7),KEYSAVE      CHECK THRU CLIENT                            
         BNE   PR110                                                            
*                                                                               
PR31     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING PDIVREC,R6                                                       
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND DIVISION REC                      
         MVC   P1(3),PDIVKDIV                                                   
         MVC   P1+6(20),PDIVNAME                                                
         OC    PDIVREP,PDIVREP     DO I HAVE A BILLING REP?                     
         BZ    PR100                                                            
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4                                                      
         MVC   PREPKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PREPKMED,QMED                   MEDIA CODE                       
         MVI   PREPKRCD,X'11'                  ID                               
         MVC   PREPKREP,PDIVREP                REP CODE                         
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    PR50                GET RECORD FOR DISPLAY                       
         MVC   P1+30(12),=C'BILLING REP='                                       
         MVC   P1+43(4),PDIVREP                                                 
         MVC   P1+49(24),=C'*REP RECORD NOT ON FILE*'                           
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     PR90                                                             
PR50     DS    0H                                                               
         MVC   AIO,AIO2            USE AIO2 FOR REP RECORD                      
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         MVC   P1+30(12),=C'BILLING REP='                                       
         MVC   P1+43(4),PDIVREP                                                 
         MVC   P1+49(L'PREPNAME),PREPNAME                                       
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         MVI   SPACING,2                                                        
*                                                                               
PR90     MVC   AIO,AIO1            RESET AIO FOR DIV RECS                       
         GOTO1 HIGH                RESET FOR SEQ READ                           
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
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
*                                                                               
HOOKX    B     EXIT                                                             
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
HEDSPECS SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H1,52,C' PRINT DIVISION REPORT'                                  
         SSPEC H2,52,C' ---------------------'                                  
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,1,CLIENT                                                      
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H7,1,C'CODE  DIVISION NAME'                                      
         SSPEC H8,1,C'----  -------------'                                      
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
       ++INCLUDE PRSFME7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMF7D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
SVCOMP   DS    X                                                                
SVDIV    DS    CL3                                                              
X        DS    XL100                                                            
*                                                                               
         EJECT                                                                  
DIVHDRD  DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034PRSFM07   02/25/15'                                      
         END                                                                    
