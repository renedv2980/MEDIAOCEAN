*          DATA SET TAGEN49    AT LEVEL 027 AS OF 12/26/12                      
*PHASE T70249C                                                                  
         TITLE 'T70249 - CHECK LIST'                                            
T70249   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70249,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         MVC   SCKLHED(7),=C'Pid Num'                                           
         OI    SCKLHEDH+6,X'80'                                                 
         SPACE 1                                                                
         MVC   SCKCUH,=C'Curr.'                                                 
         OI    SCKCUHH+6,X'80'                                                  
         SPACE 1                                                                
         CLI   MODE,SETFILE        SET FILE FOR PAGING                          
         BNE   CHKL10                                                           
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         B     XIT                                                              
         SPACE 3                                                                
CHKL10   CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   CHKL20                                                           
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         BAS   RE,INIT             INITIALIZE INFO FOR SYSIO                    
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         B     XIT                                                              
         SPACE 3                                                                
CHKL20   CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   CHKL30                                                           
         LA    R2,LISTAR                                                        
         B     CHKL40                                                           
         SPACE 3                                                                
CHKL30   CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   XIT                                                              
         LA    R1,MYSPECS          SET UP SPECS FOR REPORT                      
         LA    R1,MYSPECSP                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK             HEADLINE HOOK                                
         ST    R1,HEADHOOK                                                      
         ZAP   COUNTER,=P'0'       INIT RECORD COUNTER                          
         XC    KEY,KEY             CLEAR KEY FOR REPORT                         
         XC    TOTTAB,TOTTAB       CLEAR TOTAL TABLE                            
         LA    R2,P+1                                                           
         SPACE 1                                                                
CHKL40   DS    0H                                                               
         EJECT                                                                  
*              SET SOME VALUES BASED ON LIST TYPE / CALL SYSIO                  
         SPACE 1                                                                
         TWAXC SCKHEADH,PROT=Y     CLEAR FROM HEADINGS UNTIL END                
         SPACE 1                                                                
         CLI   LTYPE,LTYSSN        FOR EMPLOYEE                                 
         BNE   CHKL50                                                           
         LA    R0,HOOKSSN          A(SYSIO HOOK)                                
         MVI   NLISTS,NLSTSSN+1    N'RECORDS/PAGE+1                             
         MVI   RCSUBPRG,SPRGSSN        SPROG                                    
         MVC   SCKHEAD,HDSSN           HEADINGS                                 
         MVC   SCKFOOT(L'FTSSN),FTSSN  FOOTINGS                                 
         B     CHKL70                                                           
         SPACE 1                                                                
CHKL50   CLI   LTYPE,LTYINV        FOR INVOICE                                  
         BNE   CHKL60                                                           
         LA    R0,HOOKINV                                                       
         MVI   NLISTS,NLSTINV+1                                                 
         MVI   RCSUBPRG,SPRGINV                                                 
         MVC   SCKHEAD,HDINV                                                    
         MVC   SCKFOOT(L'FTINV),FTINV                                           
         NI    TIQFLAG2,X'FF'-TIQFSUB                                           
         TM    SVPDSTA2,TAPDSSUB   IF SUBSIDIARY INVOICE                        
         BNO   *+8                                                              
         OI    TIQFLAG2,TIQFSUB    READ SUBSIDIARY CHECK RECORDS                
         B     CHKL70                                                           
         SPACE 1                                                                
CHKL60   LA    R0,HOOKCAST         MUST BE FOR CAST                             
         MVI   NLISTS,NLSTCAST+1                                                
         MVI   RCSUBPRG,SPRGCAST                                                
         MVC   SCKHEAD,HDCAST                                                   
         MVC   SCKFOOT(L'FTCAST),FTCAST                                         
         SPACE 1                                                                
CHKL70   ST    R0,TIHOOK           SET A(SYSIO HOOK)                            
         MVC   TIKHOOK,SETLSTK     SET OTHER SYSIO FIELDS                       
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         XC    TOT1(TOTLNQ),TOT1   CLEAR ACCUMULATORS TO START OVER             
         SPACE 1                                                                
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   *+8                                                              
         BAS   RE,SETSCRN          SET UP SCREENS ACCORDING TO TYPE             
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO HANDLE I/O                
         ZIC   R1,NLISTS                                                        
         BCTR  R1,0                                                             
         STC   R1,NLISTS           RESET NLISTS AT END OF LIST                  
         SPACE 1                                                                
         MVI   TOTTYPE,TOTTYEND    END TOTALS                                   
         BAS   RE,DISPTOT          DISPLAY THE LAST PAGES TOTALS                
         CLI   MODE,PRINTREP       IF NOT PRINTING REPORT                       
         BE    CHKL80                                                           
         SPACE 1                                                                
         L     R3,ATHISLST                                                      
         LA    R4,SCKLSTH                                                       
         CR    R3,R4               AND NOT AT BOTTOM OF SCREEN                  
         BNL   CHKLX                                                            
         TWAXC (R3),(R4),PROT=Y    CLEAR REST OF SCREEN                         
         B     CHKLX                                                            
         SPACE 1                                                                
CHKL80   BAS   RE,PRNTIT           SKIP A LINE                                  
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(13,R1),=C'CHECK RECORDS'                                       
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         TM    WHEN,X'40'          IF REQUESTED NOW                             
         BZ    CHKLX                                                            
         XC    CONSERV,CONSERV     SET AUTO $DQU                                
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
CHKLX    MVC   TGSSN,SVTGSSN       RESTORE GLOBAL VALUES                        
         MVC   TGCOM,SVTGCOM                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND                                 
*              INITIALIZES INFO NECESSARY FOR SYSIO                             
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVI   ALLVAL,C'Y'         INIT ALL FIELDS PREV VALIDATED FLAG          
         XC    SCKSSNN,SCKSSNN     CLEAR AND TRANSMIT NAMES                     
         OI    SCKSSNNH+6,X'80'                                                 
         XC    SCKCIDN,SCKCIDN                                                  
         OI    SCKCIDNH+6,X'80'                                                 
         XC    SCKUSEN,SCKUSEN                                                  
         OI    SCKUSENH+6,X'80'                                                 
         SPACE 1                                                                
         BRAS  RE,INITSSN          VALIDATE SSN FIELD                           
         BAS   RE,INITAGY          VALIDATE AGENCY FIELD                        
         CLI   LTYPE,LTYINV        IF NOT LIST TYPE FOR INVOICE                 
         BE    *+8                                                              
         BAS   RE,INITCID          VALIDATE COMMERCIAL FIELD                    
         BAS   RE,INITINV          VALIDATE INVOICE NUMBER                      
         BAS   RE,INITCAT          VALIDATE CATEGORY FIELD                      
         BAS   RE,INITEMP          VALIDATE EMPLOYER FIELD                      
         BAS   RE,INITCLG          VALIDATE CLIENT GROUP FIELD                  
         BAS   RE,INITPD           VALIDATE PERIOD FIELD                        
         BAS   RE,INITCUR          VALIDATE CAN$ FIELD                          
         SPACE 1                                                                
         LA    R3,PFTSSN           SET PFKEY TABLE BASED ON TYPE                
         CLI   LTYPE,LTYSSN                                                     
         BE    INIT40                                                           
         LA    R3,PFTINV                                                        
         CLI   LTYPE,LTYINV                                                     
         BE    INIT40                                                           
         LA    R3,PFTCAST                                                       
INIT40   GOTO1 INITIAL,DMCB,(R3)   HANDLE PFKEY INPUT                           
         SPACE 1                                                                
         XC    TIABUFF,TIABUFF                                                  
         OC    TIFCLG,TIFCLG       IF CLIENT GROUP REQUESTED                    
         BZ    INIT50                                                           
         MVC   TIABUFF,AIO2        USE IO2+IO3 FOR NEEDDATA BUFFER              
         LA    RF,2000+2000                                                     
         ST    RF,TILBUFF                                                       
         SPACE 1                                                                
INIT50   DS    0H                                                               
**NO-OP* CLI   LTYPE,LTYCAST       IF LIST TYPE FOR CAST                        
**NO-OP* BNE   *+10                                                             
**NO-OP* XC    TIFAGY,TIFAGY       CLEAR AGENCY FILTER - DON'T NEED             
         SPACE 1                                                                
         CLI   ALLVAL,C'Y'         IF NOT ALL FIELDS PREV VALIDATED             
         BE    XIT                                                              
         SPACE 1                                                                
         XC    TOT1(TOTLNQ),TOT1   CLEAR ACCUMULATORS                           
         XC    TOTTAB,TOTTAB       CLEAR TOTAL TABLE                            
         XC    KEY,KEY             SET TO START FROM BEGINNING                  
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF   GLOBAL STORAGE                               
         MVI   TIREAD,TLCKCDQ      LIST CHECK RECORDS                           
         MVC   SVTGSSN,TGSSN       SAVE GLOBAL VALUES - XNAME CREAMS            
         MVC   SVTGCOM,TGCOM                                                    
         OI    TIQFLAGS,TIQFPDUM   SET TO PASS DUMMY RECORDS                    
         OI    TIQFLAG2,TIQFPGRY   AND TO PASS GREY RECORDS                     
         NI    TIQFLAG2,X'FF'-TIQFNLIM                                          
         OC    TIFCLG,TIFCLG       IF CLIENT GROUP REQUESTED                    
         BZ    *+8                                                              
         OI    TIQFLAG2,TIQFNLIM   SKIP LIMIT ACCESS CHECK                      
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*              ROUTINE TO VALIDATE AGENCY FIELD                                 
         SPACE 1                                                                
INITAGY  NTR1                                                                   
         TM    SCKAGYH+4,X'20'     IF AGENCY NOT PREV VALIDATED                 
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIFAGY,TIFAGY       CLEAR AGENCY FILTER                          
         CLI   LTYPE,LTYINV        IF LIST TYPE NOT FOR INVOICE                 
         BE    *+12                                                             
         CLI   SCKAGYH+5,0         TEST FOR AGENCY INPUT                        
         BE    INITAGYX                                                         
         OC    TGCLGACC,TGCLGACC   AND ADDITIONAL ACCESS BY CGROUP              
         BZ    INITAGY5                                                         
         BAS   RE,CHKINCLG         CHECK AGENCY IN CLIENT GROUP                 
         BNE   INITAGY5                                                         
         MVI   BYTE,X'80'          SKIP LIMIT ACCESS RESTRICTION TEST           
         B     *+8                                                              
INITAGY5 MVI   BYTE,0              ELSE, ENFORCE LIMIT ACCESS                   
         GOTO1 RECVAL,DMCB,(BYTE,TLAYCDQ),(X'20',SCKAGYH) VALIDATE AGY          
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         GOTO1 CHAROUT,DMCB,(X'80',TASNELQ),0                                   
         MVC   AGYNAME,TGNAME      SAVE AGENCY NAME                             
         MVC   TIFAGY,TGAGY        SET AGENCY FILTER                            
         CLI   LTYPE,LTYINV        IF LIST TYPE FOR INVOICE                     
         BNE   INITAGYX                                                         
         CLC   TGAGY,=C'999999'    AND DISPLAYING ADJUSTMENT INVOICE            
         BNE   INITAGYX                                                         
         OI    TIFINS2Y,TAINSADJ   TURN ON FILTER BIT                           
         XC    TIFAGY,TIFAGY       AND CLEAR AGENCY FILTER                      
INITAGYX OI    SCKAGYH+4,X'20'     SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS AGENCY IN CLIENT GROUP ACCESSED TO                
*              VIA STAFF RECORD                                                 
         SPACE 1                                                                
CHKINCLG NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCLPD,R4                                                        
         MVI   TLCLPCD,TLCLGCDQ                                                 
         MVC   TLCLGCLG,TGCLGACC                                                
         MVC   TLCLGAGY,SCKAGY                                                  
         OC    TLCLGAGY,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(TLCLGCLI-TLCLPD),KEYSAVE                                     
         BNE   NO                                                               
**NO-OP  MVC   SCKCLG,TGCLGACC     SET CLIENT GROUP TO SCREEN                   
**NO-OP  MVI   SCKCLGH+5,L'TGCLGACC                                             
**NO-OP  NI    SCKCLGH+4,X'DF'                                                  
**NO-OP  OI    SCKCLGH+6,X'80'                                                  
         B     YES                 AND SKIP AGENCY LIMIT RESTRICTION            
         EJECT                                                                  
*              ROUTINE VALIDATES COMMERCIAL FIELD                               
*                                                                               
INITCID  NTR1                                                                   
         LA    R2,SCKCIDH                                                       
         TM    4(R2),X'20'         IF COMMERCIAL ID NOT PREV VALIDATED          
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIFCOM,TIFCOM                                                    
         CLI   5(R2),0             IF THERE'S COMM ID INPUT                     
         BE    INITCIDX                                                         
         SPACE 1                                                                
         LA    R3,TLCOICDQ                                                      
         CLI   SCKAGYH+5,0         MUST BE AGENCY INPUT                         
         BNE   INITCID5                                                         
         OC    TGCLGACC,TGCLGACC   UNLESS CLIENT GROUP ACCESS                   
         BZ    AGYMISS                                                          
         LA    R3,TLCOGCDQ                                                      
         CLI   SCKCLGH+5,0         IN WHICH CASE MUST BE CLIENT GROUP           
         BE    AGYMISS                                                          
*                                                                               
INITCID5 GOTO1 RECVAL,DMCB,(X'10',(R3)),(X'08',SCKCIDH),SCKCIDNH                
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TIFCOM,TLCOCOM      FILTER ON INT. COMM #                        
         MVI   LTYPE,LTYCAST       SET LIST TYPE FOR CAST                       
         CLI   SCKAGYH+5,0         IF NO AGENCY INPUT                           
         BNE   INITCIDX                                                         
         MVC   TGAGY,TLCOAGY       SET GLOBAL AGENCY AND                        
         MVC   SCKAGY,TLCOAGY      DISPLAY AGENCY CODE TO SCREEN                
         OI    SCKAGYH+6,X'80'                                                  
*                                                                               
INITCIDX OI    SCKCIDH+4,X'20'     SET PREV VALIDATED                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE INVOICE NUMBER                               
         SPACE 1                                                                
INITINV  NTR1                                                                   
         LA    R2,SCKINVH                                                       
         TM    4(R2),X'20'         IF INVOICE NOT PREV VALIDATED                
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIFINV,TIFINV       CLEAR INVOICE FILTER                         
         CLI   5(R2),0             IF THERE'S NO INVOICE INPUT                  
         BNE   INITINV5                                                         
         CLI   LTYPE,LTYINV                                                     
         BE    FLDMISS             ERROR IF LIST TYPE FOR INVOICE               
         B     INITINVX                                                         
         SPACE 1                                                                
INITINV5 CLI   LTYPE,LTYINV        IF THERE IS INVOICE INPUT                    
         BNE   NOINPUT             ERROR IF NOT CORRES. LIST TYPE               
         MVI   SVPDSTA2,0                                                       
         GOTO1 TINVCON,DMCB,8(R2),TIFINV,DATCON  CONVERT FOR ACTIVE KEY         
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         MVC   TGINV,TIFINV        MOVE INTO GLOBAL                             
         XC    TGINV,HEXFFS        COMPLEMENT FOR INVOICE RECORD                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  VALIDATE RECORD EXISTS            
         BNE   THEEND                                                           
         L     R4,AIO                                                           
         USING TAPDD,R4                                                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVPDSTA2,TAPDSTA2   SAVE PAYMENT STATUS 2                        
*&&DO                                                                           
         OC    TGCLGACC,TGCLGACC   IF ACCESSED BY CLIENT GROUP                  
         BZ    INITINVX                                                         
         L     R4,AIO                                                           
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   *+14                                                             
         OC    TACOCLG,TACOCLG     IF NO CLIENT GROUP ON INV                    
         BNZ   *+14                                                             
         MVC   TIFCLG,TGCLGACC     HAVE SYSIO FILTER CLIENT GROUP               
         B     INITINVX                                                         
         CLC   TACOCLG,TGCLGACC    ELSE, DO IT NOW                              
         BNE   RECNTFND                                                         
*&&                                                                             
INITINVX OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE CATEGORY FIELD                           
         SPACE 1                                                                
INITCAT  NTR1                                                                   
         LA    R2,SCKCATH                                                       
         TM    4(R2),X'20'         IF CATEGORY NOT PREV VALIDATED               
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIFCAT,TIFCAT       CLEAR CATEGORY FILTER                        
         SPACE 1                                                                
         CLI   5(R2),0             IF THERE'S CATEGORY INPUT                    
         BE    INITCATX                                                         
         OC    8(3,R2),SPACES                                                   
         GOTO1 CATVAL,DMCB,8(R2)                                                
         BNE   FLDINV                                                           
         MVC   TIFCAT,TGCAT        SET CATEGORY FILTER                          
*                                                                               
INITCATX OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE EMPLOYER FIELD                               
         SPACE 1                                                                
INITEMP  NTR1                                                                   
         LA    R2,SCKEMPH                                                       
         TM    4(R2),X'20'         IF EMPLOYER NOT PREV VALIDATED               
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIFEMP,TIFEMP       CLEAR EMPLOYER FILTER                        
         CLI   5(R2),0             IF THERE'S NO INPUT                          
         BNE   INITEMP5                                                         
         CLI   LTYPE,LTYSSN        AND IF LIST TYPE IS FOR SSN                  
         BNE   INITEMPX                                                         
         OC    TGEMP,TGEMP         IF THERE WAS AN EMP USED                     
         BZ    *+14                                                             
         MVC   8(3,R2),TGEMP       USE IT - ELSE                                
         B     *+10                                                             
         MVC   8(3,R2),TGTPEMP     SET EMPLOYER TO BE TALENT PARTNERS           
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
INITEMP5 CLI   LTYPE,LTYSSN        IF THERE IS EMPLOYER INPUT                   
         BNE   NOINPUT             ERROR IF LIST TYPE NOT FOR SSN               
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'20',(R2))  VALIDATE EMPLOYER              
         GOTO1 CHAROUT,DMCB,(X'80',TASNELQ),0                                   
         MVC   EMPNAME,TGNAME      SAVE EMPLOYER NAME                           
         MVC   TIFEMP,TGEMP        SET EMPLOYER FILTER                          
*                                                                               
INITEMPX OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE CANADIAN DOLLAR FIELD                      
         SPACE 1                                                                
INITCUR  NTR1                                                                   
         LA    R2,SCKCURH                                                       
         XC    TIFCUR,TIFCUR                                                    
         CLI   LTYPE,LTYSSN        ONLY DEFAULT FOR EMPLOYEE'S CKS              
         BNE   *+8                                                              
         MVI   TIFCUR,C'U'                                                      
         TM    4(R2),X'20'         IF CURRENCY FIELD NOT PREV VALIDATED         
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         CLI   5(R2),0                                                          
         BE    INITCURX                                                         
*                                                                               
         CLI   LTYPE,LTYINV        INPUT ONLY VALID FOR EMP/CAST CKS            
         BE    NOINPUT                                                          
*                                                                               
         MVC   TIFCUR,8(R2)                                                     
         CLI   8(R2),C'U'          TEST US$                                     
         BE    INITCURX                                                         
         CLI   8(R2),C'C'          CANADIAN$                                    
         BE    INITCURX                                                         
         CLI   8(R2),C'E'          OR EUROS                                     
         BNE   FLDINV                                                           
*                                                                               
INITCURX OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD FIELD                                 
         SPACE 1                                                                
INITPD   NTR1                                                                   
         LA    R2,SCKPERH                                                       
         TM    4(R2),X'20'         IF START AT FIELD NOT PREV VALIDATED         
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         XC    TIQPSTR,TIQPSTR     INITIALIZE DEFAULT DATES                     
         XC    TIQPEND,TIQPEND                                                  
         MVI   TIFPDSN,0           CLEAR PAYMENT STATUS NO BITS                 
         SPACE                                                                  
         CLI   5(R2),0             IF THERE'S START INPUT                       
         BE    INITPDX                                                          
         CLI   LTYPE,LTYSSN        LIST TYPE MUST BE SSN                        
         BE    INITPD5                                                          
         CLI   LTYPE,LTYCAST       OR CAST                                      
         BNE   NOINPUT                                                          
         OI    TIFPDSN,TAPDSCNL    SET NO CANCELLED BIT FOR CAST                
INITPD5  BAS   RE,VALPD            VALIDATE PERIOD                              
*                                                                               
INITPDX  OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE REPORT PERIOD                                
         SPACE 1                                                                
VALPD    NTR1                                                                   
         SPACE 1                                                                
         LA    R2,SCKPERH          R2=A(PERIOD FIELD)                           
         MVI   TIQDTYPE,TIQDCHK    SET FILTER ON CHECK DATE                     
         LA    R4,BLOCK            R4=A(PERVAL BLOCK)                           
         USING PERVALD,R4                                                       
         MVC   BYTE,5(R2)          SET LENGTH INTO BYTE                         
         OI    BYTE,X'40'          SET VALIDATE AS MM/DD                        
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),('PVINSGLS+PVIN1DYL',(R4))              
         SPACE 1                                                                
         TM    4(R1),PVRCINV1      TEST START DATE INVALID                      
         BO    STRTINV                                                          
         TM    4(R1),PVRCINV2      TEST END DATE INVALID                        
         BO    ENDINV                                                           
         TM    4(R1),PVRCONE       ALLOW ONE DATE INPUT                         
         BO    *+12                                                             
         CLI   4(R1),PVRCOK        TEST GOOD RETURN CODE                        
         BNE   FLDINV                                                           
         SPACE 1                                                                
         CLI   8(R2),C'-'          TEST ONLY END DATE INPUT                     
         BNE   VP2                                                              
         MVC   PVALPEND,PVALPSTA   ROUTINE RETURNS DATES IN START,              
         XC    PVALPSTA,PVALPSTA                                                
         MVC   DUB,PVALCPER        INSURE DISPLAYED END DATE IS                 
         MVI   PVALCPER,C'-'       PRECEDED BY HYPHEN                           
         MVC   PVALCPER+1(8),DUB                                                
         SPACE 1                                                                
VP2      MVC   TIQPEND,PVALPEND    SET END PWOS DATE                            
         OC    TIQPEND,TIQPEND     IF NO END DATE INPUT                         
         BNZ   VP4                                                              
         MVC   TIQPEND,TGTODAY1    SET IT TO TODAY                              
         SPACE 1                                                                
VP4      CLI   PVALPSTA,0          IF WE HAVE START DATE                        
         BE    VP6                                                              
         MVC   TIQPSTR,PVALPSTA    SET START PWOS DATE                          
         SPACE 1                                                                
VP6      TM    PVALASSM,X'77'      IF ANY ASSUMPTIONS MADE                      
         BZ    VPX                                                              
         MVC   8(17,R2),PVALCPER   RE-DISPLAY DATES ON SCREEN                   
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'                                                      
VPX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE CLIENT GROUP FIELD                       
         SPACE 1                                                                
INITCLG  NTR1                                                                   
         LA    R2,SCKCLGH                                                       
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         MVI   ALLVAL,C'N'                                                      
         XC    TIFCLG,TIFCLG                                                    
         CLI   5(R2),0             IF INPUT                                     
         BE    INITCLGX                                                         
         OC    SCKCLG,SPACES                                                    
         OC    TGCLGACC,TGCLGACC   AND CLIENT GROUP ACCESS DEFINED              
         BZ    FLDINV                                                           
         CLC   SCKCLG,TGCLGACC     MUST BE EQUAL                                
         BNE   FLDINV                                                           
         GOTO1 RECVAL,DMCB,TLCGCDQ,(R2)                                         
         MVC   TIFCLG,TGCLG                                                     
*                                                                               
INITCLGX OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS OUT SUBTOTAL FOR SCREENS                            
*                                                                               
         SPACE                                                                  
DISPTOT  NTR1                                                                   
         USING LSTTOTD,R2                                                       
         CLI   LTYPE,LTYINV        IF INVOICE'S CHECKS                          
         BE    DTX                 EXIT                                         
         LA    R2,SCKTOT2H         PUT LITERALS ON 2ND LINE                     
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+22             POINT TO PRINT LINE                          
         MVC   LSTTHED(6),ENDTOT   IF END TOTALS                                
         MVI   BYTE,C'E'           SET BYTE FOR TOTCNTL                         
         CLI   TOTTYPE,TOTTYEND                                                 
         BE    DT05                                                             
         MVC   LSTTHED(13),RUNTOT  ELSE RUNNING TOTALS                          
         XC    BYTE,BYTE           - CLEAR BYTE                                 
         SPACE 1                                                                
DT05     GOTO1 TOTCNTL,DMCB,(BYTE,TOT1)                                         
         SPACE 1                                                                
         CLI   LTYPE,LTYSSN        FOR EMPLOYEE                                 
         BNE   DT10                                                             
         LA    R2,SCKTOT1H                                                      
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+38             POINT TO PRINT LINE                          
         SPACE 1                                                                
         L     R3,TOT1                                                          
         LA    R5,LSTSTOT1                                                      
         BAS   RE,EDITAM2          DISPLAY GROSS                                
         L     R3,TOT3                                                          
         LA    R5,LSTSTOT2                                                      
         BAS   RE,EDITAM2          DISPLAY MISC DEDUCTIONS                      
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         BAS   RE,PRNTIT           PRINT OUT LINE                               
         SPACE 1                                                                
         LA    R2,SCKTOT2H         POINT TO NEXT LINE                           
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+27             POINT TO PRINT LINE                          
         SPACE 1                                                                
         L     R3,TOT2                                                          
         LA    R5,LSTTOT1                                                       
         BAS   RE,EDITAM2          DISPLAY MISC PAYMENTS                        
         L     R3,TOT4                                                          
         LA    R5,LSTTOT2                                                       
         BAS   RE,EDITAM2          DISPLAY NET                                  
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         BAS   RE,PRNTIT           PRINT OUT LINE                               
         B     DTX                                                              
         SPACE 1                                                                
DT10     CLI   LTYPE,LTYCAST       FOR CAST                                     
         BNE   DTX                                                              
         LA    R2,SCKTOT2H         POINT TO 2ND LINE OF TOTALS                  
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         LA    R2,P+27             POINT TO PRINT LINE                          
         SPACE 1                                                                
         L     R3,TOT3                                                          
         LA    R5,LSTCTOT1                                                      
         BAS   RE,EDITAMTZ         DISPLAY WAGES TOTAL                          
         L     R3,TOT4                                                          
         LA    R5,LSTCTOT2                                                      
         BAS   RE,EDITAMTZ         DISPLAY PNH TOTAL                            
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   *+8                                                              
         BAS   RE,PRNTIT           PRINT OUT LINE                               
         SPACE 1                                                                
DTX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INITIALIZES SELECT FIELDS BASED ON TYPE                  
         SPACE 1                                                                
SETSCRN  NTR1                                                                   
         ZIC   R3,NLISTS                                                        
         BCTR  R3,0                R3=N'RECORDS/PAGE                            
         LA    R1,NLSTMAX                                                       
         XR    R0,R0                                                            
         DR    R0,R3                                                            
         LR    R0,R1               R0=N'LINES/RECORD                            
         SPACE 1                                                                
         LA    R2,SCKSELH          R2=A(FIRST SELECT FIELD)                     
         SPACE 1                                                                
SETS10   NI    1(R2),X'DF'         SET SEL FIELD UNPROTECTED                    
         BAS   RE,BUMP2            BUMP 2 FIELDS TO NEXT SEL                    
         BE    SETSX                                                            
         SPACE 1                                                                
         LR    RF,R0               CALC N'NON-SELECT LINES TO PROTECT           
         BCTR  RF,0                                                             
         LTR   RF,RF               FOR EACH ONE                                 
         BZ    SETS20                                                           
         OI    1(R2),X'20'         SET SEL FIELD PROTECTED                      
         BAS   RE,BUMP2            BUMP 2 FIELDS TO NEXT SEL                    
         BE    SETSX                                                            
         BCT   RF,*-12             LOOP FOR ANY OTHERS                          
         SPACE 1                                                                
SETS20   BCT   R3,SETS10           LOOP FOR N'RECORDS TO BE DISPLAYED           
         SPACE 1                                                                
         OI    1(R2),X'20'         SET SEL FIELD PROTECTED                      
         BAS   RE,BUMP2            BUMP 2 FIELDS TO NEXT SEL                    
         BNE   *-8                 KEEP LOOPING TILL END OF SCREEN              
         SPACE 1                                                                
SETSX    B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS FOR SOCIAL SECURITY NO.                    
         SPACE 1                                                                
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
HOOKSSN  NTR1                                                                   
         MVI   BYTE,NLSTSSN        SET MAX N'LINES FOR SCREEN                   
         BAS   RE,HKINIT           INITIALIZE                                   
         SPACE 1                                                                
         OC    TIINV,TIINV                                                      
         BZ    HKS01                                                            
         MVC   TGINV,TIINV         SAVE COMP. INV NO. IN GLOBAL                 
         XC    TGINV,HEXFFS                                                     
         GOTO1 TINVCON,DMCB,TIINV,LSTSINV,DATCON INVOICE NUMBER                 
         SPACE 1                                                                
HKS01    MVC   LSTSUN,TIUN         UNION                                        
         MVC   LSTSAGNT,TIAGT      AGENT                                        
         SPACE 1                                                                
         XC    SVNET,SVNET         CLEAR SAVED VALUES                           
         XC    SVCRUN,SVCRUN                                                    
         SPACE 1                                                                
         MVI   PPLFLAG,0                                                        
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMNT                      
         BAS   RE,GETEL                                                         
         BNE   HKS01A                                                           
         USING TACAD,R4                                                         
         TM    TACASTA3,TACASPPL   P+ AGENCY?                                   
         BZ    *+8                                                              
         MVI   PPLFLAG,YESQ                                                     
         SPACE 1                                                                
HKS01A   L     R4,TIAREC                                                        
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMNT                     
         BAS   RE,GETEL                                                         
         BNE   HKS02                                                            
         USING TACDD,R4                                                         
         MVC   LSTSCHK,TACDCHK     CHECK NUMBER                                 
         MVC   SVNET,TACDNET       SAVE NET AMOUNT                              
         MVC   SVCRUN,TACDRUN      CHECK PROCESSED                              
         MVC   EARNINGS,TACDEARN   INDIVIDUAL EARNINGS                          
         MVC   NONTAXBL,TACDNTAX   CORPORATE EARNINGS                           
         SPACE 1                                                                
         OC    TACDDTE,TACDDTE                                                  
         BZ    HKS02                                                            
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,LSTSCDTE)  CHECK DATE                 
         SPACE 1                                                                
HKS02    MVI   BYTE,TAODTYPD       ADD DIRECT DEPOSIT AMOUNT TO NET             
         BRAS  RE,ADDOD                                                         
         SPACE 1                                                                
         MVI   BYTE,TAODTYPW       ADD WIRE AMOUNT TO NET                       
         BRAS  RE,ADDOD                                                         
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   LSTSACDE,TAPDACDE   APPLIED CODE                                 
         MVC   LSTSICDE,TAPDICDE   INCLUDE CODE                                 
         SPACE 1                                                                
         MVC   ADJNAME,SPACES                                                   
         CLI   TAPDADJS,0          IF THIS IS AN ADJUSTMENT                     
         BE    HKS07                                                            
         GOTO1 ADJOUT,DMCB,(TAPDADJS,ADJNAME)                                   
         SPACE 1                                                                
HKS07    L     R3,EARNINGS         SET EARNING TO = TACDEARN                    
         OC    SVCRUN,SVCRUN       IF CHECK NOT PROCESSED                       
         BNZ   *+8                                                              
         L     R3,TAPDPAYI         USE INDIV. PAYMENT AMOUNT                    
         LA    R5,LSTSGR                                                        
         BAS   RE,EDITAMTZ         DISPLAY GROSS                                
         L     R1,TOT1             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT1                                                          
         SPACE 1                                                                
         OC    SVCRUN,SVCRUN       IF CHECK PROCESSED - BACK INTO DEDS.         
         BZ    HKS10                                                            
         L     R3,NONTAXBL         NON TAXABLE                                  
         A     R3,EARNINGS         PLUS TAXABLE                                 
         S     R3,SVNET            - NET                                        
         LA    R5,LSTSNET                                                       
         BAS   RE,EDITAMTZ         DISPLAY TOTAL DEDUCTIONS                     
         L     R1,TOT3             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT3                                                          
         SPACE 1                                                                
HKS10    GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE  GET USE INFO               
         MVI   LSTSSR,C'S'         INIT S/R TO S                                
         TM    TGUSSTAT,SESSION    IF NOT LIVE PAYMENT                          
         BO    *+8                                                              
         MVI   LSTSSR,C'R'         SET TO R                                     
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   *+12                                                             
         LA    R2,132(R2)          BUMP TO NEXT LINE                            
         B     HKS20                                                            
         SPACE 1                                                                
         L     R2,ATHISLST         MOVE NEXT LINE TO SCREEN W/O LISTMON         
         BAS   RE,BUMP2            BUMP PAST SEL TO NEXT DETAIL FIELD           
         LA    R2,8(R2)            BUMP PAST FIELD HEADER                       
         SPACE 1                                                                
HKS20    MVC   LSTSAGY,TIAGY       AGENCY                                       
         MVC   TGAGY,TIAGY         SAVE IN GLOBAL                               
         BAS   RE,INVCID           GET COMML ID FROM INVOICE                    
         MVC   LSTSCID,TGCID       MOVE TO SCREEN                               
         SPACE 1                                                                
         MVC   LSTSCLI,TAPDCLI     CLIENT                                       
         SPACE 1                                                                
         L     R3,NONTAXBL         NON TAXABLE                                  
         OC    SVCRUN,SVCRUN       IF CHECK PROCESSED                           
         BNZ   HSK21                                                            
         L     R3,TAPDPAYC         CORPORATE PAYMENT                            
         A     R3,TAPDREXP         + REIMBURSED EXPENSES                        
         B     HSK22                                                            
         SPACE 1                                                                
HSK21    CLI   PPLFLAG,YESQ        PAYROLL PLUS AGENCY?                         
         BNE   HSK22                                                            
         ICM   RF,15,TAPDTXNW                                                   
         AR    R3,RF               ADD IN TAXABLE REIMBURSEMENTS                
         SPACE 1                                                                
HSK22    LA    R5,LSTSMPAY                                                      
         BAS   RE,EDITAMTZ         DISPLAY MISC PAYMENTS                        
         L     R1,TOT2             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT2                                                          
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      IF THERE'S CAST DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   HKS25                                                            
         USING TACAD,R4                                                         
         OC    TACAFRST,TACAFRST                                                
         BZ    HKS25                                                            
         GOTO1 DATCON,DMCB,(1,TACAFRST),(8,LSTSFRST)  1ST SERVICES DATE         
         SPACE 1                                                                
HKS25    MVC   LSTSADJ,ADJNAME     ADJUSTMENT NAME                              
         OC    SVCRUN,SVCRUN       IF CHECK PROCESSED                           
         BZ    HKS30                                                            
         L     R3,SVNET                                                         
         LA    R5,LSTSNET                                                       
         BAS   RE,EDITAMTZ         DISPLAY NET                                  
         L     R1,TOT4             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT4                                                          
         SPACE 1                                                                
HKS30    CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   *+12                                                             
         LA    R2,132(R2)          BUMP TO NEXT LINE                            
         B     HKS40                                                            
         SPACE 1                                                                
         SH    R2,=H'8'            MOVE NEXT LINE TO SCREEN W/O LISTMON         
         BAS   RE,BUMP2            BUMP PAST SEL TO NEXT DETAIL FIELD           
         LA    R2,8(R2)            BUMP PAST FIELD HEADER                       
         SPACE 1                                                                
HKS40    BAS   RE,EPICOMM          EPISODE NUMBERS & CHECK COMMENT              
         LA    RE,SCKSL2H          IF LAST RECORD                               
         CR    RE,R2                                                            
         BH    *+14                                                             
         MVC   LSTSCMTL,BLOCK      SET UP TO DISPLAY PARTIAL COMMENT            
         B     *+10                                                             
         MVC   LSTSCMNT,BLOCK      OTHERWISE DISPLAY WHOLE COMMENT              
         B     HKEND               COMMON END HOOK ROUTINE                      
         EJECT                                                                  
*              SET COMMENT IN BLOCK. COMMENT IS COMPOSED OF                     
*              EPISODE NUMBER INFORMATION AND CHECK COMMENT                     
         SPACE 1                                                                
EPICOMM  NTR1                                                                   
         XC    BLOCK(80),BLOCK                                                  
         LA    R3,BLOCK            R3 = COMMENT TO DISPLAY                      
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASOELQ                                                   
         BAS   RE,GETEL            GET SOAP EPISODE ELEMENT                     
         BNE   EC60                                                             
         USING TASOD,R4                                                         
         ZIC   R0,TASONUM          R0 = N'EPISODE SUB-ELEMENTS                  
         LTR   R0,R0                                                            
         BZ    EC60                                                             
         LA    R2,TASOSEPI         R2 = A(EPISODE SUB-ELEMENT)                  
         USING TASOSEPI,R2                                                      
         B     *+12                                                             
EC10     MVI   0(R3),C','          SEPARATE DISPLAYED ENTRIES W/COMMA           
         LA    R3,1(R3)                                                         
*                                                                               
         LH    R1,TASOEPI          TASOEPI IS BINARY                            
         CVD   R1,DUB                                                           
         UNPK  TGEPI,DUB+5(3)      SET TGEPI FOR RECVAL LATER                   
         OI    TGEPI+4,X'F0'                                                    
         MVC   0(L'TGEPI,R3),TGEPI                                              
         LA    R3,5(R3)                                                         
         TM    TASOSTAT,TASOSH+TASOSB+TASOSS IF WRITER ROLES DEFINED            
         BZ    EC40                                                             
         MVI   0(R3),C':'          DISPLAY THEM                                 
         LA    R3,1(R3)                                                         
         LA    RE,WROLTAB                                                       
         USING WROLTABD,RE                                                      
EC20     CLC   TASOSTAT,WROLEQU    TEST WRITER ROLE                             
         BE    EC30                                                             
         LA    RE,WROLTABL(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   EC20                                                             
         DC    H'0'                                                             
EC30     MVC   0(L'WROLCHAR,R3),WROLCHAR                                        
         LA    R3,L'WROLCHAR(R3)                                                
         CLI   WROLCHAR+1,C' '     IF ONE CHAR WRITER ROLE                      
         BNE   EC40                                                             
         BCTR  R3,0                BUMP BY ONE ONLY                             
*                                                                               
EC40     GOTO1 RECVAL,DMCB,TLEPCDQ,(X'A0',0)                                    
         BNE   EC50                                                             
         L     R4,AIO              GET DATE FOR EPISODE                         
         MVI   ELCODE,TAEIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   EC50                                                             
         USING TAEID,R4                                                         
         MVC   TDATE,TAEIADT       SET TO DISPLAY AIR DATE                      
         TM    SVTACOST,TACOSWDT   IF THIS COMML REQUIRES WORK DATE             
         BZ    *+10                                                             
         MVC   TDATE,TAEIWDT       SET TO DISPLAY WORK DATE                     
         OC    TDATE,TDATE         IF DATE DEFINED                              
         BZ    EC50                                                             
         MVI   0(R3),C'-'          PRINT IT                                     
         GOTO1 DATCON,DMCB,(1,TDATE),(10,1(R3))                                 
         MVC   6(3,R3),SPACES      DON'T PRINT YEAR                             
         LA    R3,6(R3)                                                         
*                                                                               
EC50     LA    R2,L'TASOSEPI(R2)   BUMP TO NEXT EPISODE SUB-ELEMENT             
         BCT   R0,EC10             LOOP                                         
*                                                                               
EC60     MVC   AIO,TIAREC          SET AIO FOR CHAROUT                          
         LA    R2,BLOCK+100        GET CHECK COMMENT INTO FAKE FIELD            
         XC    0(88,R2),0(R2)                                                   
         MVI   0(R2),8+80                                                       
         GOTO1 CHAROUT,DMCB,TACMELQ,(R2),TACMTYPC                               
         MVC   AIO,AIO1            RESTORE AIO                                  
         MVC   1(80,R3),8(R2)                                                   
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS SYSIO RECORDS FOR INVOICE                                
         SPACE 1                                                                
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
HOOKINV  NTR1                                                                   
         MVI   BYTE,NLSTINV        SET MAX N'LINES FOR SCREEN                   
         BAS   RE,HKINIT           INITIALIZE                                   
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMNT                     
         BAS   RE,GETEL                                                         
         BNE   HKI10                                                            
         USING TACDD,R4                                                         
         TM    TACDSTAT,TACDSLIN   IF CHECK GENERATED BY A LIEN                 
         BO    XIT                 SKIP IT                                      
         TM    TACDSTAT,TACDSTRS   IF CHECK GENERATED BY W4 TRUSTEE             
         BO    XIT                 SKIP IT                                      
         SPACE 1                                                                
HKI10    L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
         SPACE 1                                                                
         OC    SCKUSEN,SCKUSEN     IF FIRST TIME IN                             
         BNZ   *+8                                                              
         BAS   RE,INV1ST           DO SOME INITIALIZATION                       
         SPACE 1                                                                
         TM    TGUSSTAT,SESSION    IF THIS IS A SESSION PAYMENT                 
         BZ    *+8                                                              
         BAS   RE,SESSOUT          DISPLAY SESSION DETAILS                      
         SPACE 1                                                                
         TM    TGUSSTA2,BSSTYPE    UNLESS THIS IS TV SESSION SCREEN             
         BO    HKI20                                                            
         MVC   LSTIACDE,TAPDACDE   APPLIED CODE                                 
         ICM   R3,15,TAPDAPPL      APPLIED AMOUNT                               
         BNZ   *+8                                                              
         L     R3,TAPDGUAR         OR GUARANTEE APPLIED                         
         LCR   R3,R3                                                            
         LA    R5,LSTIAPPL                                                      
         BAS   RE,EDITAMT                                                       
         SPACE 1                                                                
HKI20    MVC   LSTIICDE,TAPDICDE   REIMBURSED EXPENSES INCLUDE CODE             
         L     R3,TAPDREXP         REIMBURSED EXPENSES                          
         LA    R5,LSTIREIM                                                      
         BAS   RE,EDITAMT                                                       
         SPACE 1                                                                
         L     R3,TAPDPAYI         PAYMENT AMOUNT                               
         A     R3,TAPDPAYC                                                      
         LA    R5,LSTIPAY                                                       
         BAS   RE,EDITAMTZ                                                      
         SPACE 1                                                                
         L     R3,TAPDSPNH         SUBJECT TO P&H                               
         LA    R5,LSTISPNH                                                      
         BAS   RE,EDITAMTZ                                                      
         SPACE 1                                                                
         L     R3,TAPDMDED         MISC. DEDUCTION = (MDED+DUES)                
         A     R3,TAPDDUES                                                      
         LA    R5,LSTIMDED                                                      
         BAS   RE,EDTAMT9          FOR OUTPUT LENGTH OF 9                       
         SPACE 1                                                                
         MVC   LSTIAGT,TIAGT       AGENT                                        
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   *+12                                                             
         LA    R2,132(R2)          BUMP TO NEXT LINE                            
         B     HKI40               AND CONTINUE                                 
         SPACE 1                                                                
         L     R2,ATHISLST         MOVE NEXT LINE TO SCREEN W/O LISTMON         
         BAS   RE,BUMP2            BUMP PAST SEL TO NEXT DETAIL FIELD           
         LA    R2,8(R2)            BUMP PAST FIELD HEADER                       
         SPACE 1                                                                
HKI40    MVC   TGSSN,TISSN         MOVE SSN INTO GLOBAL FOR XNAME               
         GOTO1 XNAME,DMCB,(X'80',TLW4CDQ),LSTINAME,TIKEY  GET SSN NAME          
         SPACE 1                                                                
         MVC   LSTICAT,TICAT       CATEGORY                                     
         OC    TIONOF,TIONOF                                                    
         BZ    *+8                                                              
         MVI   LSTIONOF,C'Y'                                                    
         CLC   TIONOF,=C'OFF'      ON/OFF CAMERA                                
         BNE   *+8                                                              
         MVI   LSTIONOF,C'N'                                                    
         MVC   LSTIYR,TIYEAR       CONTRACT YEAR                                
         SPACE 1                                                                
         BAS   RE,DISOPT           DISPLAY OPTIONAL INFORMATION                 
         SPACE 1                                                                
         MVC   AIO,TIAREC                       SET AIO FOR CHAROUT             
         GOTO1 CHAROUT,DMCB,TACMELQ,0,TACMTYPC  GET CHECK COMMENT               
         MVC   AIO,AIO1                         RESTORE AIO                     
         MVC   LSTICMNT,TGNAME                  DISPLAY CHECK COMMENT           
         SPACE 1                                                                
         B     HKEND               COMMON END HOOK ROUTINE                      
         EJECT                                                                  
*              1ST TIME ROUTINES FOR INVOICE CHECK LIST                         
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
INV1ST   NTR1                                                                   
         GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE  GET USE INFO               
         SPACE 1                                                                
         TM    TGUSSTA2,BSSTYPE    SPECIAL HEADS FOR TV SESSION SCREEN          
         BZ    *+10                                                             
         MVC   SCKHEAD+4(L'HDINVBSS),HDINVBSS                                   
*                                                                               
         CLI   TGUSEQU,UBSM                             BSM                     
         BE    *+12                                                             
         CLI   TGUSEQU,UIMS                             IMS                     
         BNE   *+10                                                             
         MVC   SCKHEAD+4(L'HDINVBSM),HDINVBSM                                   
*                                                                               
         CLI   TGUSEQU,UBSR                             BSR                     
         BNE   *+10                                                             
         MVC   SCKHEAD+4(L'HDINVBSR),HDINVBSR                                   
         SPACE 1                                                                
         BRAS  RE,SETDETS          BUILD USE DETAILS                            
         MVC   SCKUSEN,BLOCK       RETURNED IN BLOCK - MOVE TO TOP              
         SPACE 1                                                                
         BAS   RE,INVCID           GET COMML ID/TITLE FROM INVOICE              
         SPACE 1                                                                
         MVC   SCKCID,TGCID        MOVE CID TO SCREEN                           
         OI    SCKCIDH+6,X'80'                                                  
         MVC   SCKCIDN,TGNAME      MOVE TITLE TO SCREEN                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS COMMERCIAL ID AND TITLE FROM INVOICE RECORD         
         SPACE 1                                                                
INVCID   NTR1                                                                   
         MVI   SVTACOST,0          PRE-CLEAR STATUS                             
         MVC   TGCID,SPACES        AND CID                                      
         SPACE 1                                                                
         OC    TIINV,TIINV         IF THERE IS NO INVOICE DO NOT                
         BZ    INVCIDX             TRY TO READ INVOICE RECORD                   
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'20',0)  GET INVOICE RECORD                
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMNT                
         BAS   RE,GETEL                                                         
         BNE   INVCID10                                                         
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       SAVE IN GLOBAL                               
         MVC   SVTACOST,TACOSTAT   SAVE IN STORAGE                              
         SPACE 1                                                                
INVCID10 GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTTTL  GET COMMERCIAL TITLE            
INVCIDX  B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SESSION DETAILS                                          
         SPACE 1                                                                
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
SESSOUT  NTR1                                                                   
         L     R4,TIAREC                                                        
         MVI   ELCODE,TASDELQ      GET SESSION DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BNE   SESSX                                                            
         USING TASDD,R4            R4=A(SESSION DETAILS EL.)                    
         SPACE 1                                                                
         TM    TGUSSTA2,BSSTYPE    TV SESSION SCREENS                           
         BZ    SESS4                                                            
         GOTO1 EDIT1,DMCB,TASDSP,LBSSSPOT   SPOTS                               
         GOTO1 (RF),(R1),TASDDAY,LBSSDAY    DAYS                                
         GOTO1 (RF),(R1),TASDOT,LBSSOT      OVERTIME                            
         GOTO1 (RF),(R1),TASDDT,LBSSDT      DOUBLETIME                          
         GOTO1 EDIT2,DMCB,TASDTRV,LBSSTRAV  TRAVEL TIME                         
         GOTO1 (RF),(R1),TASDPDW,LBSSPDW    PRIOR-DAY WARD TIME                 
         GOTO1 EDIT1,DMCB,TASDTAG,LBSSTAG   TAGS                                
         B     SESSX                                                            
         SPACE 1                                                                
SESS4    CLI   TGUSEQU,UBSM        MUSIC SESSIONS                               
         BE    SESS5                                                            
         CLI   TGUSEQU,UIMS                                                     
         BNE   SESS6                                                            
SESS5    GOTO1 EDIT1,DMCB,TASDMSP,LBSMSPOT  SPOTS                               
         GOTO1 EDIT2,DMCB,TASDMHM,LBSMHM    HOURS/MINUTES                       
         B     SESSX                                                            
         SPACE 1                                                                
SESS6    CLI   TGUSEQU,UBSR        RADIO SESSIONS                               
         BNE   SESSX                                                            
         GOTO1 EDIT1,DMCB,TASDRSP,LBSRSPOT  SPOTS                               
         GOTO1 (RF),(R1),TASDRTG,LBSRTAGS   TAGS                                
         GOTO1 EDIT2,DMCB,TASDRHM,LBSRHM    HOURS/MINUTES                       
         SPACE 1                                                                
SESSX    B     XIT                                                              
         SPACE 3                                                                
EDIT1    NTR1  ,                   EDIT 1 BYTE TO 2 BYTES                       
         LM    RE,RF,0(R1)         RE=A(AMOUNT)  RF=A(OUTPUT AREA)              
         EDIT  (1,0(RE)),(2,(RF)),ZERO=BLANK                                    
         B     XIT                                                              
         SPACE 2                                                                
EDIT2    NTR1  ,                   EDIT 2 BYTES TO 5 BYTES W/ 2 DEC.            
         LM    RE,RF,0(R1)         RE=A(AMOUNT)  RF=A(OUTPUT AREA)              
         EDIT  (2,0(RE)),(5,(RF)),2,ZERO=BLANK                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY OPTIONAL PAY INFO FOR INVOICE CHECKS LIST                
         SPACE 1                                                                
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
DISOPT   NTR1                                                                   
         MVC   BLOCK(40),SPACES    DISPLAY FIRST IN BLOCK                       
         LA    R3,BLOCK+1                                                       
         SPACE 1                                                                
         CLI   TAPDW4TY,TAW4TYIN   IF THIS ISN'T AN INDIVIDUAL                  
         BE    DOPT2                                                            
         MVC   0(1,R3),TAPDW4TY    DISPLAY W4 TYPE                              
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
DOPT2    XC    TGDUB,TGDUB                                                      
         MVC   TGDUB(4),TAPDOV1    SAVE 1ST OVERSCALE RATE                      
         SPACE 1                                                                
         L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMNT                      
         BAS   RE,GETEL                                                         
         BNE   DOPT8                                                            
         USING TACAD,R4            R4=A(CAST DETAILS EL.)                       
         SPACE 1                                                                
         OC    TACAGUA,TACAGUA     IF ON A GUARANTEE                            
         BZ    DOPT4                                                            
         MVC   0(3,R3),TACAGUA+1   DISPLAY LAST 3 CHARS OF CODE                 
         MVI   3(R3),C','                                                       
         LA    R3,4(R3)                                                         
         SPACE 1                                                                
DOPT4    CLI   TACADBL,C' '        IF DOUBLES PRESENT                           
         BNH   DOPT6                                                            
         MVC   0(1,R3),TACADBL     DISPLAY THEM                                 
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
DOPT6    MVC   TGDUB+4(4),TACAOV2  SAVE 2ND OVERSCALE RATE                      
         SPACE 1                                                                
DOPT8    OC    TGDUB,TGDUB         IF THERE'S ANY OVERSCALE                     
         BZ    DOPTX                                                            
         MVC   0(2,R3),=C'O='      DISPLAY TAG                                  
         LA    R3,2(R3)                                                         
         L     R1,TGDUB            1ST OVERSCALE RATE                           
         TM    TGDUB,X'80'         PERCENT SCALE USED?                          
         BZ    DOPT8A                                                           
         MVI   0(R3),C'%'                                                       
         LA    R3,1(R3)                                                         
         N     R1,=X'7FFFFFFF'     STRIP OFF HOB                                
DOPT8A   BAS   RE,EDITOV                                                        
         ICM   R1,15,TGDUB+4       2ND OVERSCALE RATE IF PRESENT                
         BZ    *+8                                                              
         BAS   RE,EDITOV                                                        
         SPACE 1                                                                
DOPTX    BCTR  R3,0                SHUFFLE BACK ONE                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '          ERASE TRAILING COMMA                         
         SPACE 1                                                                
         MVC   LSTIOPTS,BLOCK+1    MOVE TO SCREEN                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EDIT OVERSCALE RATES                                  
         SPACE 1                                                                
*                                  R1=RATE   R3=A(OUTPUT AREA)                  
EDITOV   DS    0H                                                               
         SR    R0,R0                                                            
         D     R0,=F'100'          TRUNCATE FRACTION                            
         EDIT  (R1),(4,(R3)),ALIGN=LEFT                                         
         AR    R3,R0               BUMP PAST AMOUNT                             
         MVI   0(R3),C','          ADD COMMA                                    
         LA    R3,1(R3)                                                         
         BR    RE                  RETURN R3=A(NEXT SLOT)                       
         EJECT                                                                  
*              PROCESS SYSIO RECORDS FOR CAST                                   
         SPACE                                                                  
         USING LISTD,R2            R2=A(OUTPUT AREA)                            
HOOKCAST NTR1                                                                   
         MVI   BYTE,NLSTCAST       SET MAX N'LINES FOR SCREEN                   
         BAS   RE,HKINIT           INITIALIZE                                   
         SPACE 1                                                                
         GOTO1 TINVCON,DMCB,TIINV,LSTCINV,DATCON INVOICE NUMBER                 
         MVC   LSTCCAT,TICAT       CATEGORY                                     
         OC    TICKDATE,TICKDATE                                                
         BZ    HKC03                                                            
         GOTO1 DATCON,DMCB,(1,TICKDATE),(8,LSTCCDTE) CHECK DATE                 
         SPACE 1                                                                
HKC03    L     R4,TIAREC                                                        
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMNT                      
         BAS   RE,GETEL                                                         
         BNE   HKC05                                                            
         USING TACAD,R4                                                         
         MVC   LSTCAGNT,TIAGT      AGENT                                        
         MVC   LSTCONOF,TACAONOF   ON/OFF CAMERA                                
         CLI   TACADBL,C'0'                                                     
         BE    *+10                                                             
         MVC   LSTCDBL,TACADBL     DOUBLES                                      
         MVC   LSTCYR,TACAYEAR     CONTRACT YEAR                                
         SPACE 1                                                                
         ICM   R1,15,TACAOV2       2ND OVERSCALE RATE                           
         BZ    HKC05                                                            
         SR    R0,R0                                                            
         D     R0,=F'100'          DIV BY 100 TO GET RID OF DEC PLACES          
         EDIT  (R1),(3,LSTCOV2)                                                 
         SPACE 1                                                                
HKC05    L     R4,TIAREC                                                        
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMNT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         MVC   LSTCACDE,TAPDACDE   APPLIED CODE                                 
         MVC   LSTCICDE,TAPDICDE   INCLUDE CODE                                 
         SPACE 1                                                                
         L     R3,TAPDPAYI                                                      
         A     R3,TAPDPAYC                                                      
         LA    R5,LSTCWAGE                                                      
         BAS   RE,EDITAMTZ         DISPLAY WAGES                                
         L     R1,TOT3             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT3                                                          
         SPACE 1                                                                
         L     R3,TAPDPNH                                                       
         LA    R5,LSTCPNH                                                       
         BAS   RE,EDITAMTZ         DISPLAY P&H                                  
         L     R1,TOT4             RUNNING SUBTOTAL OF THIS PAGE                
         AR    R1,R3                                                            
         ST    R1,TOT4                                                          
         SPACE 1                                                                
         ICM   R1,15,TAPDOV1       1ST OVERSCALE RATE                           
         BZ    HKC10                                                            
         SR    R0,R0                                                            
         LA    RF,LSTCOV1                                                       
         TM    TAPDOV1,X'80'       PERCENT SCALE USED?                          
         BZ    HKC07                                                            
         MVI   LSTCOV1,C'%'                                                     
         LA    RF,1(RF)                                                         
         N     R1,=X'7FFFFFFF'     STRIP OFF HOB                                
HKC07    D     R0,=F'100'          DIV BY 100 TO GET RID OF DEC PLACES          
         EDIT  (R1),(3,(RF))                                                    
HKC10    CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   *+12                                                             
         LA    R2,132(R2)          BUMP TO NEXT LINE                            
         B     HKC20                                                            
         SPACE 1                                                                
         L     R2,ATHISLST         MOVE NEXT LINE TO SCREEN W/O LISTMON         
         BAS   RE,BUMP2            BUMP PAST SEL TO NEXT DETAIL FIELD           
         LA    R2,8(R2)            BUMP PAST FIELD HEADER                       
         SPACE 1                                                                
HKC20    GOTO1 USEVAL,DMCB,(X'20',TAPDUSE),TAPDTYPE  GET USE INFO               
         BNE   HKC50                                                            
         BRAS  RE,SETDETS          BUILD USE DETAILS                            
         MVC   LSTCDETS,BLOCK      RETURNED IN BLOCK                            
         SPACE 1                                                                
HKC50    B     HKEND               COMMON END HOOK ROUTINE                      
         EJECT                                                                  
*              COMMON HOOK INITIALIZATION ROUTINE                               
         SPACE 1                                                                
*                                  BYTE=MAX N'SCREEN LINES                      
HKINIT   DS    0H                                                               
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   *+14                                                             
         CLC   LISTNUM,BYTE        AND ALREADY FILLED PAGE                      
         BE    ENDPAGE             GET OUT                                      
         SPACE 1                                                                
         TM    TIQFLAG2,TIQFNLIM   IF SKIPPING LIMIT ACCESS CHECK               
         BZ    *+14                                                             
         CLC   TIAGY,=CL6'999999'  MUST SUPPRESS 999999 AGY MYSELF              
         BE    XIT                                                              
         SPACE 1                                                                
         MVC   SYSDIR,SVSYSDIR     SET DEFAULT FILES                            
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   LISTAR,SPACES       CLEAR PREV LINE PUT OUT BY LISTMON           
         SPACE 1                                                                
         CLC   TIAGT,=C'NONE'      IF AGENT IS NONE                             
         BNE   *+10                                                             
         MVC   TIAGT,SPACES        CLEAR IT                                     
         BR    RE                                                               
         SPACE 3                                                                
*              COMMON HOOK END ROUTINE                                          
         SPACE 1                                                                
*                                  R2=A(OUTPUT AREA)                            
HKEND    DS    0H                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BNE   HKE10                                                            
         BAS   RE,PRNTIT           SPOOL IT                                     
         AP    COUNTER,=P'1'       COUNT RECORDS OUTPUT                         
         GOTO1 CATCHIOS            CATCH IO OVERUSE                             
         B     HKEX                                                             
         SPACE 1                                                                
HKE10    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
         SH    R2,=H'8'            RESTORE R2 TO FIELD HEADER                   
         BAS   RE,BUMP2            BUMP PAST SEL TO NEXT DETAIL FIELD           
         ST    R2,ATHISLST         UPDATE ATHISLST                              
         SPACE 1                                                                
         MVC   SYSDIR,=CL8'CHKDIR' SET FILES FOR PAGING                         
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE 1                                                                
HKEX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUMPS TWO SCREEN FIELDS                                  
         SPACE 1                                                                
BUMP2    DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ICM   R1,1,0(R2)                                                       
         BZ    *+10                                                             
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                  RETURN CC EQ IF END OF SCREEN                
         SPACE 3                                                                
*              ROUTINE EDITS AMOUNT IN R3 TO ADDRESS IN R5 FOR L'11             
         SPACE 1                                                                
EDITAMT  DS    0H                                                               
         LTR   R3,R3               IF AMOUNT NOT 0                              
         BZR   RE                                                               
EDITAMTZ CLI   LTYPE,LTYINV                                                     
         BNE   EDITAM2                                                          
         EDIT  (R3),(11,(R5)),2,FLOAT=-                                         
         BR    RE                                                               
EDITAM2  EDIT  (R3),(11,(R5)),2,MINUS=YES                                       
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE EDITS AMOUNT IN R3 TO ADDRESS IN R5 FOR L'9              
         SPACE 1                                                                
EDTAMT9  DS    0H                                                               
         LTR   R3,R3               IF AMOUNT NOT 0                              
         BZR   RE                                                               
EDTAMT9Z CLI   LTYPE,LTYINV                                                     
         BNE   EDTAM92                                                          
         EDIT  (R3),(9,(R5)),2,FLOAT=-                                          
         BR    RE                                                               
EDTAM92  EDIT  (R3),(9,(R5)),2,MINUS=YES                                        
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         CLI   RCSUBPRG,SPRGSSN    EMPLOYEE'S CHECKS                            
         BNE   HK2                                                              
         MVC   HEAD4+11(6),TGPID   PID NUMBER                                   
         MVC   HEAD5+11(16),SCKSSNN   AND NAME                                  
         MVC   HEAD4+64(3),TGEMP   EMPLOYER CODE                                
         MVC   HEAD5+64(16),EMPNAME     AND NAME                                
         B     HKX                                                              
         SPACE 1                                                                
HK2      CLI   RCSUBPRG,SPRGINV    INVOICE CHECKS                               
         BNE   HK4                                                              
         MVC   HEAD4+10(6),TGAGY   AGENCY CODE                                  
         MVC   HEAD4+18(16),AGYNAME   AND NAME                                  
         MVC   HEAD5+10(6),SCKINV  INVOICE NUMBER                               
         MVC   HEAD5+18(42),SCKUSEN    AND USE DETAILS                          
         MVC   HEAD4+62(12),TGCID  COMMERCIAL ID                                
         MVC   HEAD5+62(16),SCKCIDN       AND NAME                              
         B     HKX                                                              
         SPACE 1                                                                
HK4      CLI   RCSUBPRG,SPRGCAST   CAST CHECKS                                  
         BNE   HKX                                                              
         MVC   HEAD4+8(6),TGAGY    AGENCY CODE                                  
         MVC   HEAD4+21(16),AGYNAME   AND NAME                                  
         MVC   HEAD5+8(12),TGCID   COMMERCIAL ID                                
         MVC   HEAD5+21(16),SCKCIDN       AND NAME                              
         MVC   HEAD4+64(6),TGPID   PID NUMBER                                   
*        MVC   HEAD4+64(9),TGSSN   S/S NUMBER                                   
         MVC   HEAD5+64(16),SCKSSNN   AND NAME                                  
         SPACE 1                                                                
HKX      MVC   HEAD7+1(L'SCKHEAD-4),SCKHEAD+4  DETAILS HEADINGS                 
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
STRTINV  MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
AGYMISS  LA    R2,SCKAGYH                                                       
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
         SPACE 1                                                                
ENDPAGE  MVC   TGSSN,SVTGSSN       RESTORE GLOBAL VALUES                        
         MVC   TGCOM,SVTGCOM                                                    
         MVC   MYMSGNO1,OKNO       SET MESSAGE - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SCKSELH                                                       
         MVI   TOTTYPE,TOTTYRUN    RUNNING TOTALS                               
         BAS   RE,DISPTOT          DISPLAY THE TOTALS                           
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE                                                                  
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
NLSTMAX  EQU   15                  MAX N'RECORDS/SCREEN                         
NLSTSSN  EQU   5                   N'RECORDS/SCREEN BY LIST TYPE                
NLSTINV  EQU   7                                                                
NLSTCAST EQU   7                                                                
         SPACE 1                                                                
SPRGSSN  EQU   1                   REPORT SPROG BY LIST TYPE                    
SPRGINV  EQU   2                                                                
SPRGCAST EQU   3                                                                
         SPACE 1                                                                
HDSSN    DC    CL79'Sel Inv/Agy Payroll/Comm Chk/Cli  Un-Agnt/Srv S/R AX        
               p In Gross/Misc     Ded/Net'                                     
HDINV    DC    CL79'Sel             A    Applied   I  Reimb Exp    PaymX        
               ent   Subj-p&&h Misc-ded Agnt'                                   
HDCAST   DC    CL79'Sel Inv #  Chk Date Agnt Cat Cam Dbl Yr  Ov1 Ov2 ApX        
                In      Wages         P&&H'                                     
         SPACE 1                                                                
HDINVBSS DC    C'Sp Dy Ot Dt Travl Pd-wd Tg'  USE-DEPENDENT HEADS               
HDINVBSM DC    C'Sp Hr.Mn    A    Applied  '                                    
HDINVBSR DC    C'Sp Hr.Mn Tg A    Applied  '                                    
         SPACE 1                                                                
FTSSN    DC    C'PF13=History'                                                  
*TINV    DC    C'PF13=History  PF14=Employee Check List'                        
FTINV    DC    C'PF13=History'                                                  
FTCAST   DC    C'PF13=History'                                                  
         SPACE 1                                                                
RUNTOT   DC    C'Totals so far'                                                 
ENDTOT   DC    C'Totals'                                                        
GRS      DC    C'Grs='                                                          
MSC      DC    C'Misc='                                                         
DED      DC    C'Ded='                                                          
NET      DC    C'Net='                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* WRITER ROLE TABLE                                                             
WROLTAB  DS    0CL3                                                             
         DC    AL1(TASOSH+TASOSS+TASOSB),CL2'A '                                
         DC    AL1(TASOSH+TASOSS),CL2'HS'                                       
         DC    AL1(TASOSH+TASOSB),CL2'HB'                                       
         DC    AL1(TASOSS+TASOSB),CL2'SB'                                       
         DC    AL1(TASOSH),CL2'H '                                              
         DC    AL1(TASOSS),CL2'S '                                              
         DC    AL1(TASOSB),CL2'B '                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
*              PFKEY VALIDATION TABLES                                          
         SPACE 2                                                                
PFTSSN   DS    0C                  PF KEYS TABLE FOR LIST FOR SSN               
         DC    AL1(PF13AX-*,13,0,(PF13AX-PF13A)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'DISPLAY'                               
PF13A    DC    AL1(KEYTYCUR,L'LSTSPFAY-1),AL2(LSTSPFAY-LISTD)                   
         DC    AL1(KEYTYCUR,L'LSTSINV-1),AL2(LSTSINV-LISTD)                     
PF13AX   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 3                                                                
PFTINV   DS    0C                  PF KEYS TABLE FOR LIST FOR INVOICE           
         DC    AL1(PF13BX-*,13,0,(PF13BX-PF13B)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'DISPLAY'                               
PF13B    DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
PF13BX   EQU   *                                                                
*        DC    AL1(PF14BX-*,14,0,(PF14BX-PF14B)/KEYLNQ,0)                       
*        DC    CL3'CH',CL8'        ',CL8'       '                               
*F14B    DC    AL1(KEYTYCUR,L'LSTISSN-1),AL2(LSTISSN-LISTD) NEED SSN!           
*        DC    AL1(KEYTYCOM,0),AL2(0)                                           
*        DC    AL1(KEYTYCOM,0),AL2(0)                                           
*        DC    AL1(KEYTYCOM,0),AL2(0)                                           
*F14BX   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 3                                                                
PFTCAST  DS    0C                  PF KEYS TABLE FOR LIST FOR CAST              
         DC    AL1(PF13CX-*,13,0,(PF13CX-PF13C)/KEYLNQ,0)                       
         DC    CL3'HI',CL8'HISTORY ',CL8'DISPLAY'                               
PF13C    DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LSTCINV-1),AL2(LSTCINV-LISTD)                     
PF13CX   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              SPECS FOR REPORT PRINTING                                        
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPROG 1,2,3                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'Check List'                                              
         SSPEC H2,34,C'----------'                                              
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,2,C'S/S Numb'                                                 
         SSPEC H4,56,C'Employer'                                                
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H4,2,C'Agency'                                                   
         SSPEC H4,56,C'Comml'                                                   
         SSPEC H5,2,C'Invoice'                                                  
         SPACE 1                                                                
         SPROG 3                                                                
         SSPEC H4,2,C'Agency'                                                   
         SSPEC H5,2,C'Comml'                                                    
         SSPEC H4,56,C'S/S Numb'                                                
         DC    X'00'                                                            
         SPACE 2                                                                
MYSPECSP DS    0H                                                               
         SPROG 1,2,3                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'Check List'                                              
         SSPEC H2,34,C'----------'                                              
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,2,C'PID'                                                      
         SSPEC H4,56,C'Employer'                                                
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H4,2,C'Agency'                                                   
         SSPEC H4,56,C'Comml'                                                   
         SSPEC H5,2,C'Invoice'                                                  
         SPACE 1                                                                
         SPROG 3                                                                
         SSPEC H4,2,C'Agency'                                                   
         SSPEC H5,2,C'Comml'                                                    
         SSPEC H4,56,C'PID'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*              ROUTINE TO VALIDATE SSN FIELD                                    
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
INITSSN  NTR1  BASE=*,LABEL=*                                                   
         XC    TIFSSN,TIFSSN       CLEAR SSN FILTER                             
         LA    R2,SCKSSNH                                                       
         TM    SCRSTAT,RECCHG      IF RECORD TYPE CHANGED FORCE RE-VAL          
         BO    *+12                                                             
         TM    4(R2),X'20'         IF SSN NOT PREV VALIDATED                    
         BO    *+8                                                              
         MVI   ALLVAL,C'N'         SET FLAG                                     
         CLI   5(R2),0             IF THERE'S SSN INPUT                         
         BE    INITSS50                                                         
         CLI   SCKSSNH+5,6                                                      
         BH    INITSS10                                                         
         MVC   TGPID,SCKSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   INITSS10                                                         
         MVC   SCKSSN,TGSSN                                                     
         MVI   SCKSSNH+5,9                                                      
*                                                                               
INITSS10 GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SCKSSNNH  VALIDATE IT           
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SCKSSN,SPACES                                                    
         MVC   SCKSSN(L'TGPID),TGPID                                            
         MVI   SCKSSNH+5,6                                                      
         OI    SCKSSNH+6,X'80'                                                  
*                                                                               
INITSS20 MVC   TIFSSN,TGSSN        FILTER ON SSN                                
         MVI   LTYPE,LTYSSN        SET LIST TYPE FOR SSN                        
         B     INITX                                                            
         SPACE 1                                                                
INITSS50 CLI   SCKINVH+5,0         IF THERE'S NO INVOICE INPUT                  
         BNE   *+12                                                             
         CLI   SCKAGYH+5,0                                                      
         BE    FLDMISS             SSN REQUIRED                                 
         MVI   LTYPE,LTYINV        ELSE SET LIST TYPE FOR INVOICE               
INITX    OI    SCKSSNH+4,X'20'     SET VALIDATED                                
         B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD DIRECT DEPOSIT OR WIRE AMOUNT TO NET              
*                                  NTRY - BYTE =D/W                             
         SPACE 1                                                                
ADDOD    NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              SAVE AIO                                     
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TAODELQ                                                   
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         ST    R4,AIO              RESTORE AIO                                  
         JNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TAODD,R4                                                         
         L     R1,SVNET            ADD OTHER DEDUCTION AMOUNT TO NET            
         A     R1,TAODAMT                                                       
         ST    R1,SVNET                                                         
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DISPLAYS USE DETAILS FOR CAST PAYMENTS LIST              
         SPACE 1                                                                
         USING TAPDD,R4            R4=A(PAYMENT DETAILS EL.)                    
SETDETS  NTR1  BASE=*,LABEL=*                                                   
         MVC   BLOCK(80),SPACES    BUILD IN BLOCK                               
         LA    R3,BLOCK                                                         
         MVC   0(L'TGUSNAME,R3),TGUSNAME  USE NAME                              
         LA    R3,L'TGUSNAME+1(R3)                                              
         SPACE 1                                                                
         CLI   TGUSEQU,UITN        IF USE IS ITN                                
         JE    SETD28                                                           
         TM    TGUSSTA4,INDUSTRL   OR INDUSTRIAL, JUMP AHEAD                    
         BO    SETD30                                                           
         SPACE 1                                                                
         TM    TGUSTYST,USES       TEST WE HAVE USE NUMBERS                     
         JZ    SETD10                                                           
         EDIT  (2,TAPDSTUS),(5,(R3)),ALIGN=LEFT  START USE NUMBER               
         AR    R3,R0                                                            
         CLC   TAPDUSES,=H'1'      DONE IF ONLY ONE USE PAID                    
         JE    SETD05                                                           
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LH    R1,TAPDSTUS         CALC END USE NUMBER                          
         AH    R1,TAPDUSES                                                      
         BCTR  R1,0                                                             
         EDIT  (R1),(5,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
SETD05   LA    R3,1(R3)                                                         
         SPACE 1                                                                
SETD10   TM    TGUSTYST,MAJORS     TEST WE HAVE MAJORS                          
         JZ    SETD20                                                           
         GOTO1 MAJVAL,DMCB,(X'80',TAPDMAJ)  VALIDATE MAJORS                     
         MVC   0(L'TGMACHAR,R3),TGMACHAR                                        
         LA    R3,L'TGMACHAR+1(R3)                                              
         SPACE 1                                                                
SETD20   TM    TGUSTYST,UNITS      TEST WE HAVE UNITS                           
         JZ    SETD22                                                           
         EDIT  TAPDUNIT,(5,(R3)),ALIGN=LEFT,ZERO=NOBLANK                        
         LA    R3,6(R3)                                                         
         SPACE 1                                                                
SETD22   TM    TGUSTYST,INSERTS    TEST WE HAVE INSERTS                         
         JZ    SETD24                                                           
         EDIT  TAPDINS,(5,(R3)),ALIGN=LEFT,ZERO=BLANK                           
         LA    R3,6(R3)                                                         
         J     SETD30                                                           
         SPACE 1                                                                
SETD24   CLI   TGUSEQU,UTAG        TEST TAG PAYMENT                             
         JNE   SETD26                                                           
         EDIT  TAPDTAGS,(3,(R3)),ALIGN=LEFT,ZERO=BLANK                          
         LA    R3,4(R3)                                                         
         J     SETD30                                                           
         SPACE 1                                                                
SETD26   CLI   TGUSEQU,UDEM        TEST DEMO PAYMENT                            
         JE    SETD27                                                           
         CLI   TGUSEQU,USNA                                                     
         JE    SETD27                                                           
         CLI   TGUSEQU,UCDM                                                     
         JNE   SETD28                                                           
SETD27   EDIT  TAPDDEMS,(3,(R3)),ALIGN=LEFT,ZERO=BLANK                          
         LA    R3,4(R3)                                                         
         J     SETD30                                                           
         DROP  R4                                                               
         SPACE 1                                                                
SETD28   LR    R2,R4               SAVE A(PAYMENT DETAILS ELEMENT)              
         SPACE 1                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO              FOR ITN PAYMENT                              
         MVI   ELCODE,TAUHELQ      IF USAGE HISTORY ELEMENT                     
         BRAS  RE,GETEL            IS NOT PRESENT                               
         JE    SETD28B             MUST BE AUTO PAYMENT                         
         MVC   0(4,R3),=C'AUTO'                                                 
         LA    R3,4(R3)                                                         
         J     SETD28C                                                          
         SPACE                                                                  
SETD28B  EDIT  TAUHIFUS,(3,(R3)),ZERO=NOBLANK,ALIGN=LEFT                        
         AR    R3,R0                                                            
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
         LH    RE,TAUHIFUS                                                      
         AH    RE,TAUHINUS                                                      
         SHI   RE,1                                                             
         EDIT  (RE),(3,(R3)),ZERO=NOBLANK,ALIGN=LEFT                            
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         AHI   R3,1                                                             
         DROP  R4                                                               
         SPACE                                                                  
         USING TAPDD,R4                                                         
SETD28C  LR    R4,R2               RESTORE R4=A(PAYMENT DETAILS ELEM)           
         SPACE                                                                  
SETD30   OC    TAPDCYCS,TAPDCYCS   DISPLAY CYCLE DATES IF AROUND                
         JZ    SETD40                                                           
         GOTO1 DATCON,DMCB,(1,TAPDCYCS),(8,(R3))                                
         OC    TAPDCYCE,TAPDCYCE                                                
         BZ    SETD40                                                           
         MVI   8(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,TAPDCYCE),(8,9(R3))                               
         SPACE 1                                                                
SETD40   CLI   TAPDADJS,0          IF THIS IS AN ADJUSTMENT                     
         JE    SETD50                                                           
         GOTO1 ADJOUT,DMCB,(TAPDADJS,BLOCK+67)                                  
         MVI   BLOCK+65,C'-'                                                    
         SPACE 1                                                                
SETD50   GOTO1 SQUASHER,DMCB,BLOCK,80  SQUASH EVERYTHING TOGETHER               
         J     XIT                                                              
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE FOR EMPLOYEE'S CHECKS            
         SPACE                                                                  
LISTD    DSECT                                                                  
LSTSINV  DS    CL6                 INVOICE NUMBER                               
         DS    CL2                                                              
LSTSCDTE DS    CL8                 CHECK DATE                                   
         DS    CL5                                                              
LSTSCHK  DS    CL8                 CHECK NUMBER                                 
         DS    CL1                                                              
LSTSUN   DS    CL3                 UNION                                        
         DS    CL1                                                              
LSTSAGNT DS    CL4                 AGENT                                        
         DS    CL5                                                              
LSTSSR   DS    CL1                 SESSION/RESIDUAL                             
         DS    CL3                                                              
LSTSACDE DS    CL1                 APPLIED CODE                                 
         DS    CL2                                                              
LSTSICDE DS    CL1                 REIMB. EXPENSES INCLUDE CODE                 
         DS    CL1                                                              
LSTSGR   DS    CL11                GROSS                                        
         DS    CL1                                                              
LSTSMDED DS    CL11                MISC. DEDUCTION                              
*                                                                               
         DS    CL8                 HEADER FOR SEL FIELD                         
         DS    CL3                 SEL FIELD                                    
         DS    CL8                 EXTENDED HEADER FOR SEL FIELD                
         DS    CL8                 HEADER FOR DETAIL FIELD                      
LSTSPFAY DS    CL6                 AGENCY FOR PF TABLE                          
*                                                                               
         ORG   LISTD               2ND LINE                                     
LSTSAGY  DS    CL6                 AGENCY                                       
         DS    CL2                                                              
LSTSCID  DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
LSTSCLI  DS    CL6                 CLIENT                                       
         DS    CL3                                                              
LSTSFRST DS    CL8                 FIRST SERVICES DATE                          
         DS    CL1                                                              
LSTSADJ  DS    CL12                ADJUSTMENT NAME                              
         DS    CL1                                                              
LSTSMPAY DS    CL11                MISC. PAYMENTS                               
         DS    CL1                                                              
LSTSNET  DS    CL11                NET CHECK AMOUNT                             
*                                                                               
         ORG   LSTSCDTE            3RD LINE                                     
LSTSCMNT DS    CL66                EPISODE INFO & CHECK COMMENT                 
         ORG   LSTSCMNT            3RD LINE                                     
LSTSCMTL DS    CL42                LAST RECORD (ONLY 42 AVAILABLE)              
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE FOR INVOICE CHECKS               
         SPACE 1                                                                
         ORG   LISTD                                                            
LSTIVAR  DS    CL24                VARIABLE AREA                                
*                                                                               
         ORG   LSTIVAR             BSS                                          
LBSSSPOT DS    CL2                 N'SPOTS                                      
         DS    CL1                                                              
LBSSDAY  DS    CL2                 N'DAYS                                       
         DS    CL1                                                              
LBSSOT   DS    CL2                 N'OVERTIME HOURS                             
         DS    CL1                                                              
LBSSDT   DS    CL2                 N'DOUBLE-TIME HOURS                          
         DS    CL1                                                              
LBSSTRAV DS    CL5                 N'TRAVEL TIME HOURS                          
         DS    CL1                                                              
LBSSPDW  DS    CL5                 N'PRIOR-DAY WARDROBE HOURS                   
         DS    CL1                                                              
LBSSTAG  DS    CL2                 N'TAGS                                       
*                                                                               
         ORG   LSTIVAR             BSM/IMS                                      
LBSMSPOT DS    CL2                 N'SPOTS                                      
         DS    CL1                                                              
LBSMHM   DS    CL5                 HOURS/MINUTES                                
*                                                                               
         ORG   LSTIVAR             BSR                                          
LBSRSPOT DS    CL2                 N'SPOTS                                      
         DS    CL1                                                              
LBSRHM   DS    CL5                 HOURS/MINUTES                                
         DS    CL1                                                              
LBSRTAGS DS    CL2                 N'TAGS                                       
*                                                                               
         ORG   LSTIVAR             GENERAL                                      
         DS    CL12                                                             
LSTIACDE DS    CL1                 APPLIED CODE                                 
LSTIAPPL DS    CL11                APPLIED AMOUNT                               
*                                                                               
         DS    0CL51               ALL INV SCREENS ARE SAME FROM HERE           
         DS    CL3                                                              
LSTIICDE DS    CL1                 REIMBURSED EXPENSES INDICATOR                
LSTIREIM DS    CL11                REIMBURSED EXPENSES                          
LSTIPAY  DS    CL11                PAYMENT AMOUNT                               
LSTISPNH DS    CL11                AMOUNT SUBJECT TO P&H                        
LSTIMDED DS    CL9                 MISCELLANEOUS DEDUCTION                      
         DS    CL1                                                              
LSTIAGT  DS    CL4                 AGENT                                        
*                                                                               
         ORG   LISTD               2ND LINE                                     
LSTINAME DS    CL16                NAME                                         
         DS    CL1                                                              
LSTICAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LSTIONOF DS    CL1                 Y=ON, N=OFF CAMERA                           
         DS    CL1                                                              
LSTIYR   DS    CL3                 CONTRACT YEAR                                
         DS    CL1                                                              
LSTIOPTS DS    CL17                OPTIONS                                      
         DS    CL1                                                              
LSTICMNT DS    CL30                CHECK COMMENT                                
         EJECT                                                                  
*              DSECT FOR SCREEN AND PRINT LINE FOR CAST PAYMENT HISTORY         
         SPACE 1                                                                
         ORG   LISTD                                                            
LSTCINV  DS    CL6                 INVOICE NUMBER                               
         DS    CL1                                                              
LSTCCDTE DS    CL8                 CHECK DATE                                   
         DS    CL1                                                              
LSTCAGNT DS    CL4                 AGENT                                        
         DS    CL1                                                              
LSTCCAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
LSTCONOF DS    CL3                 ON/OFF CAMERA                                
         DS    CL2                                                              
LSTCDBL  DS    CL1                 DOUBLES                                      
         DS    CL2                                                              
LSTCYR   DS    CL3                 CONTRACT YEAR                                
         DS    CL1                                                              
LSTCOV1  DS    CL3                 OVERSCALE RATE 1                             
         DS    CL1                                                              
LSTCOV2  DS    CL3                 OVERSCALE RATE 2                             
         DS    CL2                                                              
LSTCACDE DS    CL1                 APPLIED CODE                                 
         DS    CL2                                                              
LSTCICDE DS    CL1                 REIMB. EXPENSES INCLUDE CODE                 
         DS    CL1                                                              
LSTCWAGE DS    CL11                WAGES                                        
         DS    CL1                                                              
LSTCPNH  DS    CL11                PENSION & HEALTH                             
         SPACE                                                                  
         ORG   LSTCCDTE            2ND LINE                                     
LSTCDETS DS    CL(L'SCKDET-(*-LISTD)) USE DETAILS                               
         EJECT                                                                  
*        DSECT TO COVER TOTAL LINES                                             
         SPACE                                                                  
LSTTOTD  DSECT                                                                  
         DS    CL8                                                              
         DS    CL1                                                              
LSTTHED  DS    CL13                HEADING                                      
         DS    CL4                                                              
LSTTOT1  DS    CL11                WAGES                                        
         DS    CL1                                                              
LSTTOT2  DS    CL11                PENSION & HEALTH                             
         ORG   LSTTOTD                                                          
         DS    CL8                                                              
LSTSTOT1 DS    CL11                EMPLOYEE'S TOTALS                            
         DS    CL1                                                              
LSTSTOT2 DS    CL11                                                             
         ORG   LSTTOTD                                                          
         DS    CL8                                                              
         DS    CL17                                                             
LSTCTOT1 DS    CL11                WAGES                                        
         DS    CL1                                                              
LSTCTOT2 DS    CL11                PENSION & HEALTH                             
         SPACE 2                                                                
*        DSECT TO COVER WRITER ROLE TABLE                                       
         SPACE                                                                  
WROLTABD DSECT                                                                  
WROLEQU  DS    XL1                 EQUATE FOR THIS WRITER ROLE                  
WROLCHAR DS    CL2                 CHARACTER WRITER ROLE                        
WROLTABL EQU   (*-WROLTABD)                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR49D                                                       
         EJECT                                                                  
COUNTER  DS    PL4                 COUNTER OF NUM OF RECORDS OUTPUT             
ALLVAL   DS    CL1                 Y=ALL FIELDS WERE PREV. VALIDATED            
TDATE    DS    XL3                 TEMP DATE (WORK OR AIR)                      
*                                                                               
LTYPE    DS    CL1                 LIST TYPE                                    
LTYSSN   EQU   C'1'                C'1'=EMPLOYEE'S (SSN'S) CHECKS LIST          
LTYINV   EQU   C'2'                C'2'=INVOICE PAYMENT LIST                    
LTYCAST  EQU   C'3'                C'3'=CAST PAYMENT HISTORY LIST               
*                                                                               
INV      DS    CL6                 WORK SPACE FOR INVOICE                       
SVPDSTA2 DS    XL1                 SAVED PAYMENT DETAILS STATUS 2               
SVTACOST DS    XL1                 SAVED COMMERCIAL STATUS                      
SVTGSSN  DS    CL9                 SAVED GLOBAL SSN                             
SVTGCOM  DS    XL4                 SAVED GLOBAL INTERNAL COMM NUMBER            
MYNAME   DS    CL16                NAME RETURNED BY XNAME                       
AGYNAME  DS    CL16                AGENCY NAME                                  
EMPNAME  DS    CL16                EMPLOYER NAME                                
ADJNAME  DS    CL12                ADJUSTMENT NAME                              
*                                                                               
TOTTYPE  DS    CL1                                                              
TOTTYRUN EQU   C'R'                RUNNING TOTAL                                
TOTTYEND EQU   C'E'                END TOTAL                                    
*                                                                               
SVCRUN   DS    PL3                 SAVED CHK RUN DATE                           
SVNET    DS    F                   SAVED NET AMOUNT                             
*                                                                               
PPLFLAG  DS    XL1                                                              
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
*                                                                               
EARNINGS DS    F                   ADJUSTMENT AMOUNT                            
NONTAXBL DS    F                   ADJUSTMENT CORPORATE AMOUNT                  
*                                                                               
TOT1     DS    F                                                                
TOT2     DS    F                                                                
TOT3     DS    F                                                                
TOT4     DS    F                                                                
TOTLNQ   EQU   *-TOT1                                                           
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027TAGEN49   12/26/12'                                      
         END                                                                    
