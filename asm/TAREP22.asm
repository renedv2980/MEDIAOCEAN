*          DATA SET TAREP22    AT LEVEL 071 AS OF 02/20/13                      
*PHASE T70322C,*                                                                
         TITLE 'T70322 - CHECKS CASHED FROM BANK TAPE'                          
T70322   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70322                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T703FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   APERVERT,TPERVERT   SAVE A(PERVERT)                              
         SPACE 1                                                                
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE                                                      
         BAS   RE,PREP                                                          
         BAS   RE,CLOSTAPE                                                      
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   LISTOPT,C'N'                                                     
         MVI   DETOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK),0                                   
         ZIC   R0,4(R1)                                                         
         LA    R4,BLOCK                                                         
         LTR   R0,R0                                                            
         BZ    FLDINV                                                           
         SPACE 1                                                                
OPT2     CLC   12(4,R4),=C'LIST'   LIST OPTION                                  
         BNE   OPT4                                                             
         MVI   LISTOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPT4     CLC   12(6,R4),=C'DETAIL' DETAIL REPORT OPTION                         
         BNE   FLDINV                                                           
         MVI   DETOPT,C'Y'                                                      
         B     OPTEND                                                           
         SPACE 1                                                                
OPTEND   LA    R4,32(R4)                                                        
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              UPDATE CHECK RECORDS & PRINT REPORT                              
         SPACE 3                                                                
PREP     NTR1                                                                   
         XC    TOTALS(TOTALSL),TOTALS   CLEAR TOTALS                            
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,1                                                       
         CLI   LISTOPT,C'Y'                                                     
         BE    PREP10                                                           
         MVI   RCSUBPRG,10                                                      
         SPACE 1                                                                
PREP10   BAS   RE,GETTAPE          GET FIRST(/NEXT) RECORD                      
         SPACE 1                                                                
         LA    R5,BANKREC                                                       
         USING BANKRECD,R5                                                      
         SPACE 1                                                                
         CLI   BANKTYPE,X'FF'      IF END OF TAPE                               
         BE    XIT                 EXIT                                         
*                                                                               
         CLI   BANKTYPE,BANKTYPT   IF TRAILER RECORD                            
         BNE   PREP15                                                           
         BAS   RE,PRTTOTS          PRINT BANK ACCOUNT/EMP TOTALS                
         MVI   FORCEHED,C'Y'       FORCE TO A NEW PAGE                          
         B     PREP10                                                           
*                                                                               
PREP15   CLI   BANKTYPE,BANKTYPD   IF CHECK DETAIL RECORD                       
         BNE   PREP10                                                           
*                                                                               
         CLI   LISTOPT,C'Y'        IF OPTION TO LIST ONLY                       
         BNE   PREP20                                                           
         MVC   P(L'BANKREC),BANKREC MOVE INFO TO PRINT LINE                     
         BAS   RE,SPLAT                                                         
         B     PREP10              AND GET NEXT                                 
*                                                                               
PREP20   BAS   RE,UPDCHK           ELSE, UPDATE CHECK RECORD & REPORT           
         B     PREP10              AND GET NEXT                                 
         EJECT                                                                  
*              UPDATE CHECK WITH DATE & PRINT REPORT                            
         SPACE 2                                                                
UPDCHK   NTR1                                                                   
         MVI   CHKERR,0            PRE-CLEAR CHECK ERROR FLAG                   
         MVC   BACCNO,BANKDACC     SET CURRENT BANK ACCOUNT                     
         GOTO1 DATCON,DMCB,(9,BANKDPDT),(0,MYBKDATE)                            
*                                  READ CHECK PASSIVE FOR UPDATE                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCKPD,4                                                         
         MVI   TLCKPCD,TLCKCCDQ                                                 
         MVC   TLCKCCHK,BANKDCHK+2                                              
         XC    TLCKCCHK,=8X'FF'                                                 
         MVC   FILENAME,=CL8'CHKDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(TLCKCBNK-TLCKPD),KEYSAVE                                     
         BE    *+12                                                             
         MVI   CHKERR,CHKERR1      CHECK RECORD NOT FOUND                       
         B     UPDCHKX             AND PRINT OUT ERROR DETAILS                  
*                                                                               
         MVC   FILENAME,=CL8'CHKFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         L     R4,AIO              SAVE THIS CHECKS INFO                        
         USING TLCKD,R4                                                         
         MVC   THISSSN,TLCKSSN                                                  
         MVC   THISAGY,TLCKAGY                                                  
         MVC   THISINV,TLCKINV                                                  
*                                                                               
         BAS   RE,XTRAOI           EXTRACT TAOID INFO                           
         BAS   RE,XTRATI           EXTRACT CORP INFO                            
         BAS   RE,XTRACD           EXTRACT CHECK DETAILS INFO                   
         BNE   UPDCHKX             IF NONE, PRINT ERROR DETAILS                 
         BAS   RE,WRITCHK          ELSE, WRITE CHECK BACK TO FILE               
         SPACE 1                                                                
         BAS   RE,XTRBILL          EXTRACT BILL INFORMATION                     
         BAS   RE,XTRW4            EXTRACT W4 INFO                              
*                                                                               
UPDCHKX  BAS   RE,ADDTOTS          ADD TO TOTALS                                
         BAS   RE,PRNTCHK          PRINT CHECK DETAILS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EXTRACTS INFO FROM TAOID                                 
         SPACE 1                                                                
XTRAOI   NTR1                                                                   
         MVI   ELCODE,TAOIELQ      CHECK THIS IS AN ADJUSTMENT                  
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         USING TAOID,R4                                                         
         MVC   THISAGY,TAOIAGY                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE EXTRACTS DATA FROM TATID ELEMENT                         
         SPACE 1                                                                
XTRATI   NTR1                                                                   
         MVI   ELCODE,TATIELQ      PAYING A CORP?                               
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         USING TATID,R4                                                         
         MVC   THISSSN,TATIID                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT INFO FROM TACDD ELEMENT                       
         SPACE 1                                                                
XTRACD   NTR1                                                                   
         MVI   ELCODE,TACDELQ      FIND CHECK DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   CHKERR,CHKERR2      SET BAD CHECK                                
         B     NO                                                               
*                                                                               
         USING TACDD,R4                                                         
         EDIT  TACDNET,(10,WORK),FILL=0                                         
         CLC   BANKDAMT,WORK       MAKE SURE AMOUNTS ARE EQUAL                  
         BE    *+12                                                             
         MVI   CHKERR,CHKERR3      SET AMOUNTS DON'T MATCH                      
         B     NO                                                               
*                                                                               
         OC    TACDCSH,TACDCSH                                                  
         BZ    *+12                                                             
         MVI   CHKERR,CHKERR4      SET PREVIOUSLY CASHED                        
         B     NO                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,TACDDTE),(0,CDATE)  EXTRACT CHK DATE              
*                                  COMPUTE # OF DAYS TAKEN TO CASH              
         GOTO1 APERVERT,DMCB,CDATE,MYBKDATE                                     
         LH    R1,DMCB+8           N'DAYS (INCLUSIVE)                           
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                -1 FOR N'DAYS                                
         ST    R1,NDAYS                                                         
         B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO WRITE CHECK WITH CASHED DATE                          
         SPACE 1                                                                
WRITCHK  NTR1                                                                   
         MVI   ELCODE,TACDELQ      FIND CHECK DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                JUST GOT IT                                  
         SPACE 1                                                                
         USING TACDD,R4                                                         
         GOTO1 DATCON,DMCB,(0,MYBKDATE),(1,TACDCSH)                             
*                                  UPDATE RECORD WITH CASHED DATE               
         MVC   FILENAME,=CL8'CHKFIL'                                            
         GOTO1 PUTREC                                                           
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT INVOICE INFO                                  
         SPACE 1                                                                
XTRBILL  NTR1                                                                   
         MVC   BDAYS,NDAYS         DEFAULT BILLED DAYS = CASHED                 
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING TLIND,R4                                                         
         MVI   TLINCD,TLINCDQ      READ INVOICE FOR BILLED DATE                 
         MVC   TLINAGY,THISAGY                                                  
         MVC   TLININV,THISINV                                                  
         XC    TLININV,=8X'FF'                                                  
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAIND,R4                                                         
         SPACE 1                                                                
         XC    BDATE,BDATE                                                      
         OC    TAINBDTE,TAINBDTE                                                
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(0,BDATE)                               
*                                  COMPUTE # OF DAYS SINCE BILLING              
         GOTO1 APERVERT,DMCB,BDATE,MYBKDATE                                     
         LH    R1,DMCB+8           N'DAYS (INCLUSIVE)                           
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                -1 FOR N'DAYS                                
         ST    R1,BDAYS                                                         
         B     XIT                                                              
         EJECT                                                                  
*              EXTRACT PAYEE NAME                                               
         SPACE 3                                                                
XTRW4    NTR1                                                                   
         MVC   W4NAME,SPACES                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLW4D,R4                                                         
         MVI   TLW4CD,TLW4CDQ      READ W4 FOR PAYEE NAME                       
         MVC   TLW4SSN,THISSSN                                                  
         GOTO1 HIGH                                                             
         CLC   TLW4KEY,KEYSAVE                                                  
         BNE   XIT                                                              
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAW4D,R4                                                         
         MVC   W4NAME(L'TAW4CRPN),TAW4CRPN      CORP NAME                       
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    XIT                                                              
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    XIT                                                              
         MVC   W4NAME(16),TAW4NAM1  INDIV NAME                                  
         MVI   W4NAME+16,C' '                                                   
         MVC   W4NAME+17(16),TAW4NAM2                                           
         GOTO1 SQUASHER,DMCB,W4NAME,33                                          
         B     XIT                                                              
         EJECT                                                                  
*              ADD TO TOTALS                                                    
         SPACE 1                                                                
ADDTOTS  NTR1                                                                   
         CLI   CHKERR,CHKERR1                                                   
         BNE   ADDTOT5                                                          
         L     R1,TOTMISS                                                       
         AH    R1,=H'1'                                                         
         ST    R1,TOTMISS                                                       
         B     ADDTOTX                                                          
*                                                                               
ADDTOT5  CLI   CHKERR,CHKERR2                                                   
         BNE   ADDTOT8                                                          
         L     R1,TOTBAD                                                        
         AH    R1,=H'1'                                                         
         ST    R1,TOTBAD                                                        
         B     ADDTOTX                                                          
*                                                                               
ADDTOT8  CLI   CHKERR,CHKERR3                                                   
         BNE   ADDTOT9                                                          
         L     R1,TOTNEQ                                                        
         AH    R1,=H'1'                                                         
         ST    R1,TOTNEQ                                                        
         B     ADDTOTX                                                          
*                                                                               
ADDTOT9  CLI   CHKERR,CHKERR4                                                   
         BNE   ADDTOT10                                                         
         L     R1,TOTPREV                                                       
         AH    R1,=H'1'                                                         
         ST    R1,TOTPREV                                                       
         B     ADDTOTX                                                          
*                                                                               
ADDTOT10 L     R1,TOTCHK                                                        
         AH    R1,=H'1'                                                         
         ST    R1,TOTCHK                                                        
         PACK  DUB,BANKDAMT                                                     
         CVB   R3,DUB                                                           
*                                                                               
         LR    RF,R3                                                            
         A     R3,TOTCASH                                                       
         ST    R3,TOTCASH                                                       
         LR    R3,RF                                                            
         M     RE,NDAYS            SAVE DAYS*CASH FOR WEIGHTED AVE              
         A     RF,TOTDAY                                                        
         ST    RF,TOTDAY                                                        
         LR    RF,R3                                                            
         M     RE,BDAYS            SAVE DAYS*CASH FOR WEIGHTED AVE              
         A     RF,TOTBILL                                                       
         ST    RF,TOTBILL                                                       
ADDTOTX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT CHECK & INVOICE DETAILS                         
         SPACE 1                                                                
PRNTCHK  NTR1                                                                   
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
*                                                                               
         CLI   DETOPT,C'Y'         IF NOT RUNNING DETAIL                        
         BE    *+12                                                             
         CLI   CHKERR,0            ONLY PRINT IF ERROR                          
         BE    XIT                                                              
*                                                                               
         MVC   PCHECK,BANKDCHK+2   PRINT CHECK NUMBER                           
         EDIT  (C10,BANKDAMT),(12,PAMOUNT),2,MINUS=YES & AMOUNT                 
         GOTO1 DATCON,DMCB,(0,MYBKDATE),(8,PCSHDATE)  & CASH DATE               
*                                                                               
         MVC   PNAME,=CL33'*** CHECK NOT FOUND ***'                             
         CLI   CHKERR,CHKERR1      IF CHECK NOT FOUND                           
         BE    PRNTCHKX            EXIT                                         
         SPACE 1                                                                
         CLC   THISAGY,=C'999999'  PRINT AGENCY                                 
         BE    *+10                                                             
         MVC   PAGENCY,THISAGY                                                  
         OC    THISINV,THISINV           INVOICE                                
         BZ    PRNTCHK5                                                         
         GOTO1 TINVCON,DMCB,THISINV,PINVOICE                                    
*                                                                               
PRNTCHK5 MVC   PNAME,=CL33'*** BAD CHECK RECORD ***'                            
         CLI   CHKERR,CHKERR2      IF BAD CHECK RECORD                          
         BE    PRNTCHKX            EXIT                                         
         MVC   PNAME,=CL33'*** AMOUNTS NOT EQUAL ***'                           
         CLI   CHKERR,CHKERR3      IF AMOUNTS DON'T MATCH                       
         BE    PRNTCHKX            EXIT                                         
         MVC   PNAME,=CL33'*** PREVIOUSLY CASHED ***'                           
         CLI   CHKERR,CHKERR4      IF PREVIOUSLY CASHED                         
         BE    PRNTCHKX                                                         
*                                                                               
         MVC   PSSN,THISSSN        PRINT SS#                                    
         GOTO1 DATCON,DMCB,(0,CDATE),(8,PCKDATE) PRINT CHECK DATE               
         EDIT  NDAYS,(4,PDAYS)                   N'DAYS TAKEN TO CASH           
*                                                                               
         OC    BDATE,BDATE         IF INVOICE BILLED                            
         BZ    PRNTCHK8                                                         
         GOTO1 DATCON,DMCB,(0,BDATE),(8,PBLDATE) PRINT BILLED DATE              
         EDIT  BDAYS,(4,PBDAYS)                  N'DAYS SINCE BILLED            
*                                                                               
PRNTCHK8 MVC   PNAME,W4NAME        PRINT PAYEE NAME                             
*                                                                               
PRNTCHKX BAS   RE,SPLAT            PRINT LINE                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT OUT TOTALS                                      
         SPACE 1                                                                
PRTTOTS  NTR1                                                                   
         LA    R2,P                                                             
         USING PRINTD,R2                                                        
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
         EDIT  TOTCHK,(8,PCHECK)    # OF GOOD CHECKS                            
         MVC   PSSN,=CL9'CHECKS'                                                
         MVC   PNAME,=CL33'*** TOTALS ***'                                      
         EDIT  TOTCASH,(12,PAMOUNT),2,MINUS=YES                                 
         CLI   DETOPT,C'Y'         IF RUNNING DETAIL                            
         BNE   PRNTTOT6                                                         
         OC    TOTDAY,TOTDAY       COMPUTE AVERAGE DAYS                         
         BZ    PRNTTOT5                                                         
         L     RF,TOTDAY                                                        
         M     RE,=F'10'                                                        
         D     RE,TOTCASH                                                       
         EDIT  (RF),(4,PDAYS),1                                                 
         SPACE 1                                                                
PRNTTOT5 OC    TOTBILL,TOTBILL     COMPUTE AVERAGE BILLED                       
         BZ    PRNTTOT6                                                         
         L     RF,TOTBILL                                                       
         M     RE,=F'10'                                                        
         D     RE,TOTCASH                                                       
         EDIT  (RF),(4,PBDAYS),1                                                
PRNTTOT6 BAS   RE,SPLAT            PRINT GOOD TOTAL LINE                        
         SPACE 1                                                                
         OC    TOTMISS,TOTMISS     IF MISSING CHECK RECORDS                     
         BZ    PRNTTOT7                                                         
         EDIT  TOTMISS,(8,PCHECK)  # OF CHECKS NOT FOUND                        
         MVC   PSSN,=CL9'NOT FOUND'                                             
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTTOT7 OC    TOTBAD,TOTBAD       IF BAD CHECK RECORDS                         
         BZ    PRNTTOT8                                                         
         EDIT  TOTBAD,(8,PCHECK)   # OF CHECKS WITH NO TACDD                    
         MVC   PSSN,=CL9'BAD'                                                   
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTTOT8 OC    TOTNEQ,TOTNEQ       IF CHECK AMOUNTS DON'T MATCH                 
         BZ    PRNTTOT9                                                         
         EDIT  TOTNEQ,(8,PCHECK)                                                
         MVC   PSSN,=CL9'AMT NEQ'                                               
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTTOT9 OC    TOTPREV,TOTPREV     IF CHECK PREVIOUSLY CASHED                   
         BZ    PRNTTOTX                                                         
         EDIT  TOTPREV,(8,PCHECK)                                               
         MVC   PSSN,=CL9'PREV CASH'                                             
         BAS   RE,SPLAT                                                         
*                                                                               
PRNTTOTX XC    TOTALS(TOTALSL),TOTALS                                           
         B     XIT                                                              
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 3                                                                
OPENTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         CLI   RECNUM,B1                                                        
         BNE   *+8                                                              
         L     R2,=A(TAPEIN2)                                                   
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         CLI   RECNUM,B1                                                        
         BNE   *+8                                                              
         L     R2,=A(TAPEIN2)                                                   
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETTAPE  NTR1                                                                   
         LA    R0,BANKREC                                                       
         L     R1,=A(TAPEIN)                                                    
         CLI   RECNUM,B1                                                        
         BNE   *+8                                                              
         L     R1,=A(TAPEIN2)                                                   
         GET   (1),(0)                                                          
         B     XIT                                                              
         SPACE 1                                                                
TAPEEOF  MVI   BANKREC+BANKTYPE-BANKRECD,X'FF'  SET END OF TAPE                 
         B     XIT                                                              
         EJECT                                                                  
*              VARIOUS ROUTINES                                                 
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              HEADLINES ETC                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H1+52(24),=CL24'CHECKS CASHED'                                   
         GOTO1 CENTER,DMCB,H1+52,24                                             
         GOTO1 UNDERLIN,DMCB,(24,H1+52),(X'BF',H2+52)                           
         MVC   H4+10(L'BACCNO),BACCNO                                           
*                                                                               
         BAS   RE,INITBOX          SET BOX COLS & ROWS                          
         XIT1                                                                   
         SPACE 2                                                                
INITBOX  NTR1                                                                   
         L     R6,ABOX                                                          
         USING BOXD,R6                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+60,C'B'                                                  
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRINTD,R2                                                        
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BC7,C'C'                                                         
         MVI   BC8,C'C'                                                         
         MVI   BR,C'R'                                                          
         B     XIT                                                              
         EJECT                                                                  
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              LTORG ETC                                                        
         SPACE 2                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=TAPEEOF,        X        
               RECFM=FB,BUFNO=2,LRECL=40                                        
TAPEIN2  DCB   DDNAME=TAPEIN2,DSORG=PS,MACRF=(GM),EODAD=TAPEEOF,       X        
               RECFM=FB,BUFNO=2,LRECL=40                                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 1,10                                                             
         SSPEC H1,1,RUN                                                         
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H4,1,C'BANK ACC'                                                 
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SPACE 1                                                                
         SPROG 10                                                               
         SSPEC H7,9,C'CHECK              ERROR MESSAGE '                        
         SSPEC H8,9,C'NUMBER'                                                   
         SPACE 1                                                                
         SSPEC H7,65,C'CHECK'                                                   
         SSPEC H8,65,C'AMOUNT'                                                  
         SPACE 1                                                                
         SSPEC H7,76,C'CASH'                                                    
         SSPEC H8,76,C'DATE'                                                    
         SPACE 1                                                                
         SSPEC H7,85,C'   CHECK     AGENCY INVOICE     BILLED'                  
         SSPEC H8,85,C'DATE   DAYS         NUMBER    DATE   DAYS'               
         DC    H'0'                                                             
         SPACE 1                                                                
         EJECT                                                                  
*              DSECT TO COVER WORKING STORAGE                                   
         SPACE 1                                                                
MYD      DSECT                                                                  
         DS    0A                                                               
APERVERT DS    A                   A(PERVERT)                                   
NDAYS    DS    F                                                                
BDAYS    DS    F                                                                
*                                                                               
TOTALS   DS    0F                                                               
TOTMISS  DS    F                   # OF CHECKS NOT FOUND                        
TOTBAD   DS    F                   # OF BAD CHECKS                              
TOTNEQ   DS    F                   # OF CHECKS NOT MATCHED                      
TOTPREV  DS    F                   # OF CHECKS PREVIOUSLY CASHED                
TOTCHK   DS    F                   # OF GOOD CHECKS                             
TOTCASH  DS    F                   CASH FOR GOOD CHECKS                         
TOTDAY   DS    F                   DAYS*CASH FOR WEIGHTED AVE                   
TOTBILL  DS    F                   DAYS*CASH FOR WEIGHTED AVE                   
TOTALSL  EQU   *-TOTALS                                                         
*                                                                               
LISTOPT  DS    CL1                 Y=LIST ONLY OPTION                           
DETOPT   DS    CL1                 Y=DETAIL REPORT OPTION                       
*                                                                               
CHKERR   DS    XL1                                                              
CHKERR1  EQU   1                   CHECK RECORD NOT FOUND                       
CHKERR2  EQU   2                   CHECK DETAILS ELEMENT NOT FOUND              
CHKERR3  EQU   3                   AMOUNTS NOT EQUAL                            
CHKERR4  EQU   4                   PREVIOUSLY CASHED                            
*                                                                               
THISSSN  DS    CL9                                                              
THISAGY  DS    CL6                                                              
THISINV  DS    CL6                                                              
CDATE    DS    CL6                 INTERNAL YYMMDD                              
BDATE    DS    CL6                          YYMMDD                              
MYBKDATE DS    CL6                          YYMMDD                              
BACCNO   DS    CL10                CURRENT BANK ACCOUNT                         
W4NAME   DS    CL33                PAYEE NAME                                   
*                                                                               
BANKREC  DS    0CL(BANKLNQ)        BANK RECORD                                  
MYEND    DS    0D                                                               
         EJECT                                                                  
*              INPUT TAPE/DATASET FORMAT                                        
         SPACE 1                                                                
BANKRECD DSECT                                                                  
BANKTYPE DS    CL1                 TRANSACTION TYPE H,D,T                       
BANKTYPH EQU   C'H'                HEADER RECORD                                
BANKTYPD EQU   C'D'                DETAIL RECORD                                
BANKTYPT EQU   C'T'                TRAILER RECORD                               
*                                                                               
BANKHDR  EQU   *                                                                
*                                                                               
         DS    CL9                                                              
BANKHCDT DS    CL7                 CUTOFF DATE                                  
         DS    CL23                                                             
*                                                                               
         ORG   BANKHDR                                                          
BANKDACC DS    CL10                ACCOUNT NUMBER                               
BANKDCHK DS    CL10                CHECK NUMBER                                 
BANKDAMT DS    CL10                AMOUNT                                       
BANKDPDT DS    CL8                 PAID DATE                                    
         DS    CL1                                                              
*                                                                               
         ORG   BANKHDR                                                          
         DS    CL9                                                              
BANKTCNT DS    CL10                RECORD COUNT                                 
BANKTAMT DS    CL10                TOTAL AMOUNT                                 
         DS    CL10                                                             
BANKLNQ  EQU   (*-BANKRECD)                                                     
         SPACE 2                                                                
*              PRINT LINE                                                       
         SPACE 1                                                                
PRINTD   DSECT                                                                  
         DS    CL7                                                              
BL       DS    CL1                                                              
PCHECK   DS    CL8                                                              
BC1      DS    CL1                                                              
PSSN     DS    CL9                                                              
BC2      DS    CL1                                                              
PNAME    DS    CL33                                                             
BC3      DS    CL1                                                              
PAMOUNT  DS    CL12                                                             
BC4      DS    CL1                                                              
PCSHDATE DS    CL8                                                              
BC5      DS    CL1                                                              
PCKDATE  DS    CL8                                                              
         DS    CL1                                                              
PDAYS    DS    CL4                                                              
BC6      DS    CL1                                                              
PAGENCY  DS    CL6                                                              
BC7      DS    CL1                                                              
PINVOICE DS    CL6                                                              
         DS    CL1                                                              
BC8      DS    CL1                                                              
PBLDATE  DS    CL8                                                              
         DS    CL1                                                              
PBDAYS   DS    CL4                                                              
BR       DS    CL1                                                              
         DS    CL14                                                             
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071TAREP22   02/20/13'                                      
         END                                                                    
