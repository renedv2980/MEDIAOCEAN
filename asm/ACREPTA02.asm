*          DATA SET ACREPTA02  AT LEVEL 008 AS OF 01/06/12                      
*PHASE ACTA02A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'TIMESHEET AUDIT REPORT'                                         
**********************************************************************          
* QOPT1 = Y RUNNING IT OVER THE WEEKEND SKIP READING RECOVERY FILE   *          
*         READ ACCFILES ONLY                                         *          
* QOPT2 = Y DISPLAY THOSE TIME SHEETS ALSO THAT DOES NOT HAVE ERRORS *          
* QOPT3 = Y DISPLAY ALL LINES EVEN IF THERE ARE NO ERRORS QOPT2 MUST *          
*         BE SET TO Y FOR THIS TO WORK                               *          
* QOPT4 = Y SUPPRESS E-MAIL WHEN ERRORS ARE ENCOUNTERED              *          
**********************************************************************          
ACTA02   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 0,**ACTA**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACTAD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,REQLAST              LAST FOR REQUEST                       
         BE    REQL                                                             
         CLI   MODE,RUNLAST              LAST FOR REQUEST                       
         BE    RUNL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         LHI   R1,CPYMAX           TOTAL NO. OF ENTRIES TO CLEAR                
         LA    R2,CPYTAB           POINT TO COMPANY TABLE                       
         XC    0(CPYLNQ,R2),0(R2)                                               
         LA    R2,CPYLNQ(R2)                                                    
         BCT   R1,*-10                                                          
*                                                                               
         ZAP   PKCPYCNT,=P'0'      CPY COUNT TO ZERO                            
         ZAP   PKRUNTOT,=P'0'      RUN TOTAL ERRORS                             
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING RCVRECD,R2                                                       
REQF     DS    0H                                                               
         ZAP   PKCOUNT,=P'0'       INIT BAD RECORD ERRORS COUNTER               
         MVI   FLAG,0              INITIALIZE FLAG                              
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         MVI   ERRFLG,0            INITIALIZE ERROR FLAGS                       
         MVI   ERRFLG1,0                                                        
*                                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         GOTO1 ASETDATE,DMCB,(RC)  SET START AND END DATES                      
*                                                                               
         CLI   QOPT1,C'Y'          ARE WE RUNNING ON WEEKEND                    
         BNE   *+12                                                             
         OI    FLAG,FLGWKND                                                     
         B     REQFX                                                            
*                                                                               
         LA    RE,RECVIO                                                        
         LA    RF,L'RECVIO                                                      
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR RECVIO                                 
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
REQF10   LA    R2,RECVIO           POINT TO THE BEGINING OF RECORD              
         GET   RECVIN,(2)                                                       
*                                                                               
         CLI   RCVFILTY,RCVFAMST   IS IT ACCMST                                 
         BNE   REQF10                                                           
         CLI   RCVPRGNO,RCVPCAPQ   ARE THESE COST ALLOCATION PROGRAMS           
         BE    *+12                                                             
         CLI   RCVPRGNO,RCVPBRAQ   OR BRANDOCEAN                                
         BNE   REQF10                                                           
         CLI   RCVRECTY,RCVRADDQ   IS IT AN ADD                                 
         BNE   REQF10                                                           
*                                                                               
         USING TIMRECD,R5             R3=A(TRANSACTION KEY)                     
         LA    R5,RCVRECRD            IF THERE ARE NO TIMELS (X'8B')            
         TM    TIMRSTA,X'80'          IF RECORD IS MARKED DELETED               
         BO    REQF10                 DON'T PROCESS GET NEXT                    
         CLC   TIMKREF,=C'*TIME*'     IS IT A TIME RECORD?                      
         BNE   REQF10                                                           
*                                                                               
         USING TIMELD,R6                                                        
         LR    R6,R5                                                            
         AH    R6,=Y(TIMRFST-TIMKEY)                                            
REQF20   CLI   0(R6),0                ARE WE AT THE END OF RECORD               
         BE    REQF10                                                           
         CLI   0(R6),TIMELQ        IS IT 8B                                     
         BNE   REQF30                                                           
         CLI   TIMETYP,TIMEINP     IS IT INPUT TYPE                             
         BNE   REQF10              SHOULD BE A INPUT TYPE IF TEMPO              
         TM    TIMSTAT,TIMTEMPO    IS IT TEMPO RECORD                           
         BZ    REQF10                                                           
         B     REQF40                                                           
REQF30   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     REQF20              BUMP TO CHECK NEXT ELEMENT                   
         DROP  R6                                                               
*                                                                               
         USING RECVD,R4                                                         
REQF40   DS    0H                                                               
         LA    R4,RECVWRK          POINT TO RECOVERY TABLE TO BE BUILT          
         MVC   RECVWRK,SPACES                                                   
         MVC   RECVCULA,TIMKCULA    SAVE 1R ACCOUNT IN TABLE                    
         MVC   RECVPEDT,TIMKPEDT                                                
         GOTO1 ABINADD,DMCB,(RC),RECVWRK,ARECVTAB   ADD TABLE ENTRY             
         B     REQF10                                                           
*                                                                               
         EJECT                                                                  
ENDIN    CLOSE (RECVIN,)                                                        
         USING BIND,R1             CLEAR BIN TABLES                             
         L     R1,ARECVTAB         BIN TABLE BUILT OFF RECOVERY TAPE            
         ICM   R0,15,BININ                                                      
         BNZ   REQFX               MAKE IT REQFX                                
         MVI   FCRDACC,C'N'        DO NOT READ ACCOUNTS                         
REQFX    B     EXIT                                                             
         DROP  R1,R2,R4,R5                                                      
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
*                                                                               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
*                                                                               
         BAS   RE,GETLEVS          GET LEVELS                                   
*                                                                               
LDGFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL A FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         L     R2,ADLVANAM                                                      
         LA    R3,LEVANME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVAFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL B FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         L     R2,ADLVBNAM                                                      
         LA    R3,LEVBNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVBFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL C FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ADLVCNAM                                                      
         LA    R3,LEVCNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVCFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
PACC     DS    0H                                                               
         XC    ARECVENT,ARECVENT   CLEAR ADDRESS OF ENTRY IN TABLE              
         BAS   RE,SETCDE           SET LEVEL CODES                              
         L     R2,ADACCNAM                                                      
         LA    R3,LEVDNME                                                       
         BAS   RE,GETNME                                                        
         L     R2,ADACC                                                         
*                                                                               
         USING BIND,R1                                                          
         L     R1,AT1RATAB                                                      
         XC    BININ,BININ         GET NUMBER OF ENTRIES IN BINTABLE            
         DROP  R1                                                               
*                                                                               
         USING BIND,R1                                                          
         L     R1,ARECVTAB                                                      
         ICM   R0,15,BININ         GET NUMBER OF ENTRIES IN BINTABLE            
         BNZ   *+12                                                             
         OI    FLAG,FLGRCV         BUILD RECOVERY BIN TABLE YOURSELF            
         B     PACC40              RUNNING OVER THE WEEKEND IF ZERO             
*                                                                               
         USING RECVD,R3                                                         
         LA    R3,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
PACC20   CLC   ACTKCULA,RECVCULA   COMPARE WITH WHATS IN RECV BIN TABLE         
         BE    PACC30                                                           
         LA    R3,RECVLNQ(R3)      BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,PACC20                                                        
         B     PACCX                                                            
*                                                                               
PACC30   ST    R3,ARECVENT         SAVE OFF OFF ADD OF ENTRY IN TABLE           
*                                                                               
PACC40   DS    0H                                                               
*                                                                               
         BAS   RE,BLDTMBUK         FILL IN TIME HOURS BUCKET                    
         BAS   RE,BLDHDBUK         FILL IN HEADER HOURS BUCKET                  
         BAS   RE,BLDDTBUK         FILL IN DETAIL HOURS BUCKET                  
         BAS   RE,ACCL                                                          
*                                                                               
         TM    FLAG,FLGWKND        ARE WE DOING IT OVER THE WEEKEND             
         BNO   PACCX                                                            
         USING BIND,R1                                                          
         L     R1,ARECVTAB                                                      
         XC    BININ,BININ     CLR TABL DOING ONE ACC AT A TIME WKND            
PACCX    B     EXIT                                                             
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
ACCL     NTR1                                                                   
         BAS   RE,CLEARXP          CLEAR XP TO SPACES                           
*                                                                               
         NI    FLAG,X'FF'-FLGPRNT                                               
         LA    R5,XP                                                            
*                                                                               
         USING BIND,R1                                                          
         L     R1,AT1RATAB         R1=A(TRANSACTION TABLE)                      
         ICM   R3,15,BININ         NO. OF ENTRIES IN A TABLE                    
         BZ    ACCLX                                                            
         USING T1RAD,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         XC    SVPEDT,SVPEDT       INITIALIZE SAVE PERIOD END DATE              
ACCL10   DS    0H                                                               
         CLC   SVPEDT,T1RAPEDT                                                  
         BE    ACCL30                                                           
         OC    SVPEDT,SVPEDT       IS IT FIRST TIME WITH THIS PERSON            
         BZ    ACCL20              YES DON'T CLOSE BOX                          
         TM    FLAG,FLGPRNT        HAS ANYTHING PRINTED SO FAR                  
         BNO   ACCL20              IF NOT THEN DON'T PRINT BOX                  
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRNTLIN          PRINT A LINE TO DIVIDE END DATES             
ACCL20   MVC   PCODE,LEVDCDE                                                    
         MVC   SVPEDT,T1RAPEDT     SAVE PERIOD END DATE                         
         GOTO1 DATCON,DMCB,(1,T1RAPEDT),(8,PPEDT)                               
*                                                                               
ACCL30   EDIT  T1RXTLN#,PXLIN#     TEMPO LINE NUMBER                            
         EDIT  T1RLINE#,PLINE#     TMS LINE NUMBER                              
         LA    R4,T1RABKT          POINT TO START OF BUCKETS                    
         LA    R6,PBUCKET          POINT WHERE TO START PRINTING                
         LHI   R1,T1RABKCT         NUMBER OF BUCKETS TO LOOP                    
ACCL40   DS    0H                                                               
         EDIT  (P8,(R4)),(PBUCLEN,(R6)),2,ZERO=BLANK                            
         LA    R4,T1RABKLN(R4)                                                  
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R1,ACCL40                                                        
*                                                                               
         BAS   RE,CMPHRS           COMPARE HOURS                                
         BAS   RE,CMPHACNT         COMPARE HDR VS ALLOC LINE SWP CNT            
*                                                                               
         OC    T1RAERR,T1RAERR                                                  
         BNZ   ACCL50              NO ERRORS LIKE RECDS MISSING ETC             
         OC    T1RAERR1,T1RAERR1   IS THERE ANY WARNING/ERROR                   
         BNZ   ACCL50                                                           
*                                                                               
         CLI   QOPT2,C'Y'          WANT TO PRNT TMS WHICH HAS NO ERRS           
         BE    *+12                                                             
         BAS   RE,CLEARXP          CLEAR XP                                     
         B     ACCL80              GET NEXT TIME SHEET                          
*                                                                               
         CLI   QOPT3,C'Y'          WANT TO PRINT ALL LINES                      
         BE    ACCL70              YES                                          
         OC    T1RXTLN#(4),T1RXTLN#                                             
         BZ    ACCL70                                                           
         BAS   RE,CLEARXP          CLEAR XP TO SPACES                           
         B     ACCL80              DON'T PRINT TMS WITH LINE NUMBERS            
*                                                                               
ACCL50   OC    T1RXTLN#(4),T1RXTLN#                                             
         BNZ   ACCL60                                                           
         AP    PKCOUNT,=P'1'       INCREMENT RECORD COUNTER                     
         LA    R1,T1RAERR          PASS T1RAERR'S ADDRESS                       
         OC    ERRFLG,T1RAERR      MARK ERROR FLAG                              
         LA    R6,ERRTAB           PASS ERROR TABLE'S ADDRESS                   
         BAS   RE,FILERR           FILL APPROPRIATE ERROR IN XP                 
         LA    R1,T1RAERR1         PASS SECOND ERROR BYTE'S ADDRESS             
         OC    ERRFLG1,T1RAERR1    MARK ERROR FLAG                              
         LA    R6,ERRTAB1                                                       
         BAS   RE,FILERR                                                        
         OI    FLAG,FLGPRNT        SOMETHING WAS PRINTED                        
         B     ACCL80                                                           
ACCL60   MVC   PERROR,=CL24'    *** ERROR ***    '                              
ACCL70   BAS   RE,PRINTIT                                                       
         OI    FLAG,FLGPRNT        SOMETHING WAS PRINTED                        
*                                                                               
ACCL80   LA    R2,T1RALNQ(R2)                                                   
         BCT   R3,ACCL10                                                        
         TM    FLAG,FLGPRNT        WAS ANYTHING PRINTED                         
         BNO   ACCLX                                                            
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRNTLIN          PRINT LINE TO DIVIDE NEXT PERSON             
*                                                                               
ACCLX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
REQL     DS    0H                                                               
         LA    R5,XP                                                            
         CP    PKCOUNT,=P'0'                                                    
         BE    REQLX                                                            
*                                                                               
         AP    PKCPYCNT,=P'1'      INCREMENT NO. OF CPY IN ERROR                
         AP    PKRUNTOT,PKCOUNT    UPDATE RUN TOTAL ERRORS                      
*                                                                               
         CP    PKCPYCNT,=P'50'     CPYMAX                                       
         BE    REQL30              DON'T DIE FOR THIS SMALL THING               
         USING CPYTABD,R2                                                       
         LA    R2,CPYTAB                                                        
REQL10   CLI   CPYID,0                                                          
         BE    REQL20                                                           
         LA    R2,CPYLNQ(R2)                                                    
         B     REQL10                                                           
REQL20   MVC   CPYID,ALPHAID       SAVE ALPHAID                                 
         MVC   CPYLOGID,SVCLOGO    COMPANY LOGIN ID                             
         ZAP   CPYBAD,PKCOUNT      NO. OF BAD RECORDS                           
         MVC   CPYEFLG,ERRFLG      SAVE ERROR FLAGS                             
         MVC   CPYEFLG1,ERRFLG1                                                 
*                                                                               
REQL30   MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PERRMSG,PERRDESC    RECS IN ERROR FOR THIS CPY                   
         EDIT  PKCOUNT,PCNT                                                     
         BAS   RE,PRINTIT                                                       
REQLX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
RUNL     DS    0H                                                               
         LA    R5,XP                                                            
         CP    PKCPYCNT,=P'0'                                                   
         BE    RUNLX                                                            
*                                                                               
         CLI   QOPT4,C'Y'          SUPPRESS E-MAIL REPORTING?                   
         BE    *+8                                                              
         BAS   RE,SENDMAIL         E-MAIL ERROR REPORT                          
*                                                                               
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   PERRMSG,PERRCPY     TOTAL NO. OF CPY IN ERROR                    
         EDIT  PKCPYCNT,PCNT                                                    
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
         USING CPYTABD,R2                                                       
         LA    R2,CPYTAB           POINT TO COMPANY TABLE                       
RUNL10   CLI   CPYID,0                                                          
         BE    RUNL20                                                           
         MVC   PERRMSG+2(L'ALPHAID),CPYID     PRINT ALPHAID                     
         MVC   PERRMSG+5(L'CPYLOGID),CPYLOGID COMPANY LOGIN ID                  
         EDIT  CPYBAD,PCNT                                                      
         BAS   RE,PRINTIT                                                       
         LA    R2,CPYLNQ(R2)                                                    
         B     RUNL10                                                           
*                                                                               
RUNL20   BAS   RE,PRINTIT                                                       
         MVC   PERRMSG,PERRRUN     TOTAL NO. OF RECS IN ERROR                   
         EDIT  PKRUNTOT,PCNT                                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
RUNLX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* CLEAR XP TO SPACES                                                            
**********************************************************************          
         SPACE 1                                                                
CLEARXP  NTR1                                                                   
         LA    R5,XP                                                            
         MVI   XP,C' '             FILL XP WITH SPACES                          
         MVC   XP+1(L'XP-1),XP                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT A LINE                                                                  
**********************************************************************          
         USING PLINED,R5                                                        
         SPACE 1                                                                
PRNTLIN  NTR1                                                                   
         LA    R5,XP                                                            
         MVI   PCODE-1,X'BF'           FILL XP WITH DASHES                      
         MVC   PCODE(PLINELNQ-2),PCODE-1                                        
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* E-MAIL ERROR REPORT                                                *          
**********************************************************************          
         SPACE 1                                                                
SENDMAIL NTR1                                                                   
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   SUBJJOB,MCJOB       JOB NAME                                     
         DROP  RF                                                               
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBJDSC,SUBJDSC)                
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN1)                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN2)                                 
*                                                                               
         USING CPYTABD,R2                                                       
         LA    R2,CPYTAB           POINT TO COMPANY TABLE                       
SENDM10  CLI   CPYID,0                                                          
         BE    SENDM50                                                          
         MVC   BODYCPY(L'ALPHAID),CPYID     PRINT ALPHAID                       
         MVC   BODYID(L'CPYLOGID),CPYLOGID  COMPANY LOGIN ID                    
         LA    R6,ERRTAB                                                        
*                                                                               
SENDM20  MVC   SVERR,CPYEFLG                                                    
         CLI   0(R6),X'FF'                                                      
         BNE   *+12                                                             
         LA    R6,ERRTAB1                                                       
         B     SENDM30                                                          
         NC    SVERR,0(R6)                                                      
         BZ    SENDM25                                                          
         MVC   BODYMSG,1(R6)                                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN3)                                 
         MVC   BODYLIN3,SPACES                                                  
SENDM25  LA    R6,L'ERRTAB(R6)                                                  
         B     SENDM20                                                          
*                                                                               
SENDM30  MVC   SVERR,CPYEFLG1                                                   
         CLI   0(R6),X'FF'                                                      
         BE    SENDM40                                                          
         NC    SVERR,0(R6)                                                      
         BZ    SENDM35                                                          
         MVC   BODYMSG,1(R6)                                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN3)                                 
         MVC   BODYLIN3,SPACES                                                  
SENDM35  LA    R6,L'ERRTAB(R6)                                                  
         B     SENDM30                                                          
*                                                                               
SENDM40  LA    R2,CPYLNQ(R2)                                                    
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN3)   SKIP LINE                     
         B     SENDM10                                                          
*                                                                               
SENDM50  GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
SENDMX   B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
*                                                                               
*OWHO    DC    C'US-TEMPO SUPPORT:'                                             
TOWHO    DC    C'JSHA,DCUR,RDES:'                                               
*                                                                               
SUBJDSC  DC    0CL80                                                            
         DC    C'ATA REPORT ERROR(S) FOR JOB '                                  
SUBJJOB  DC    C'        '                      JOB NAME                        
         DC    CL(L'SUBJDSC-(*-SUBJDSC))' '     SPARE SPACES                    
BODYLIN1 DC    0CL80                                                            
BODYHEAD DC    C' COMPANY              ERRORS'  HEADING                         
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '   SPARE SPACES                    
BODYLIN2 DC    0CL80                                                            
BODYLINE DC    C'----------   '                 UNDERLINES                      
         DC    C'------------------------'                                      
         DC    CL(L'BODYLIN2-(*-BODYLIN2))' '   SPARE SPACES                    
BODYLIN3 DC    0CL80                                                            
BODYCPY  DC    C'   '                           COMPANY CODE                    
BODYID   DC    C'          '                    ID                              
BODYMSG  DC    C'                        '      ERROR MESSAGE                   
         DC    CL(L'BODYLIN3-(*-BODYLIN3))' '   SPARE SPACES                    
*                                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD TIME BUCKETS ROUTINE. READS TIME RECORDS AND FILL IN T1RATAB *          
* HOUR BUCKETS TYPE B,R,N,NC.                                        *          
* CHECKS TEMPO TIME ONLY IF RUNNING ON WEEKDAYS ELSE CHECKS TMS AND  *          
* TEMPO BOTH IF RUNNING ON WEEKEND.                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMRECD,R4                                                       
BLDTMBUK NTR1                                                                   
         L     R2,ADACC                                                         
         LA    R4,SVKEY                                                         
         MVC   TIMKEY,SPACES                                                    
         MVC   TIMKCULA,0(R2)      FILL IN ACCOUNT                              
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     BLDTM20                                                          
BLDTM10  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDTM20  CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY                                      
         BNE   BLDTMX                                                           
         LA    R4,IO                                                            
         CLC   TIMKREF,=C'*TIME*'  IS IT A TIME RECORD                          
         BNE   BLDTM10                                                          
*                                                                               
         TM    FLAG,FLGWKND        ARE WE DOING OVER THE WEEKEND                
         BNO   BLDTM30             YES                                          
         CLC   TIMKPEDT,START      IS PERIOD END <  START DATE                  
         BL    BLDTM10             YES GET NEXT RECORD                          
         CLC   TIMKPEDT,END        IS PERIOD END > END DATE                     
         BH    BLDTM10             YES GET NEXT RECORD                          
         B     BLDTM50             YES                                          
*                                                                               
         USING RECVD,R3                                                         
BLDTM30  L     R3,ARECVENT                                                      
BLDTM40  CLC   RECVCULA,TIMKCULA   JUST A DOUBLE CHECK                          
         BNE   BLDTM10                                                          
         CLC   RECVPEDT,TIMKPEDT   DOES PERIOD END DATES MATCH                  
         BE    BLDTM50                                                          
         LA    R3,RECVLNQ(R3)                                                   
         B     BLDTM40                                                          
         DROP  R3                                                               
*                                                                               
         USING T1RAD,R3                                                         
BLDTM50  DS    0H                                                               
         LA    R3,T1RAWRK                                                       
         XC    T1RAWRK,T1RAWRK                                                  
         BAS   RE,ZAP1RBUK                                                      
         MVC   T1RKCULA,TIMKCULA   ACCOUNT CODE TO BINTAB                       
         MVC   T1RAPEDT,TIMKPEDT   PERIOD END DATE TO BINTAB                    
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TIMELD,R5                                                        
         LR    R5,R4                                                            
         AH    R5,=Y(TIMRFST-TIMKEY)                                            
*        LA    R5,TIMRFST                                                       
BLDTM60  CLI   0(R5),0                                                          
         BE    BLDTM10                                                          
         CLI   0(R5),TIMELQ        IS IT X'8B' ELEMENT                          
         BNE   BLDTM70                                                          
         CLI   TIMETYP,TIMEINP     IS IT INPUT DETAIL 01                        
         BNE   BLDTM70                                                          
         TM    TIMSTAT,TIMTEMPO                                                 
         BO    BLDTM80                                                          
BLDTM70  SR    R1,R1                                                            
         IC    R1,1(R5)            GET LENGTH OF ELEMENT                        
         AR    R5,R1                                                            
         BAS   RE,ZAP1RBUK         CLEAR ALL BUCKETS                            
         B     BLDTM60                                                          
*                                                                               
BLDTM80  DS    0H                                                               
         MVC   T1RLINE#,TIMLINE#   MOVE IN TMS LINE NUMBER                      
         BAS   RE,FLTMBUK          FILL IN TIME BUCKETS                         
BLDTM90  SR    R1,R1                                                            
         IC    R1,1(R5)            GET LENGTH OF ELEMENT                        
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    BLDTM70                                                          
         CLI   0(R5),TIMELQ                                                     
         BNE   BLDTM90                                                          
         CLI   TIMETYP,TIMEXTRA    IS IT TEMPO EXTRA STATUS                     
         BNE   BLDTM90                                                          
*                                                                               
         MVC   T1RXTLN#,TIMXTLN#   MOVE IN LINE NUMBER                          
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB                               
*                                                                               
         XC    T1RXTLN#(4),T1RXTLN#  ADD TO HIGHER LEVEL                        
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB                               
*                                                                               
         TM    FLAG,FLGRCV         AM I BUILDING RECOVERY MYSELF                
         BNO   BLDTM70                                                          
*                                                                               
         MVC   RECVWRK,SPACES                                                   
         MVC   RECVWRK(T1RXTLN#-T1RAKEY),T1RAKEY                                
         GOTO1 ABINADD,DMCB,(RC),RECVWRK,ARECVTAB   ADD TABLE ENTRY             
         B     BLDTM70                                                          
*                                                                               
BLDTMX   B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE FILLS IN TIME BUCKETS IN T1RATABLE                         *          
* R3= AT1RATAB  BINTAB WORK AREA WITH KEY FILLED IN                  *          
* R5=A(8B 01 INPUT DETAIL ELEMENT)                                   *          
**********************************************************************          
         SPACE 1                                                                
         USING T1RAD,R3                                                         
         USING TIMELD,R5                                                        
FLTMBUK  NTR1                                                                   
         LA    R1,BUKDSPTB         POINT TO TABLE                               
FLTM10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         CLC   TIMTTYP,0(R1)       MATCH WITH B,R,N,NC TYPE                     
         BE    *+12                                                             
         LA    R1,L'BUKDSPTB(R1)   BUMP TO NEXT TYPE OF TIME                    
         B     FLTM10                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R1)               GET DISPLACEMENT OF BUCKET                
         LA    R3,T1RAWRK                                                       
         AR    R3,RE                  POINT TO BUCKET                           
         ZAP   0(T1RABKLN,R3),TIMHRS  ZAP HOURS TO CORRECT BUCKET               
FLTMX    B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* READ HEADER RECORDS AND GET ALL DIFFERENT TYPE OF TIME HOURS       *          
* PUT THEM IN BUCKETS OF T1RATAB TABLE FOR A LATER COMPARISON        *          
**********************************************************************          
         USING TSXRECD,R6                                                       
         USING RECVD,R3                                                         
         SPACE 1                                                                
BLDHDBUK NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ARECVTAB                                                      
         ICM   R0,15,BININ         GET NUMBER OF ENTRIES IN A TABLE             
         BZ    BLDHDX                                                           
         LA    R3,BINTAB                                                        
         TM    FLAG,FLGWKND        ARE WE DOING ON WEEKEND                      
         BO    *+8                                                              
         L     R3,ARECVENT         START FROM THIS ENTRY                        
         DROP  R1                                                               
*                                                                               
         XC    SVKEY,SVKEY         BUILD KEY TO READ 3E13 RECDS                 
         LA    R6,SVKEY                                                         
         MVI   TSXKTYP,TSXKTYPQ                                                 
         MVI   TSXKSUB,TSXKSUBQ                                                 
         MVC   TSXKCPY,RECVCPY                                                  
*                                                                               
         MVC   TSXKODS,SPACES                                                   
         LA    RF,RECVACC                                                       
         SR    R1,R1                                                            
         IC    R1,LEVC             GET OFF/DPT/SUB LEN                          
         BCTR  R1,0                                                             
         MVC   TSXKODS(0),0(RF)    ISOLATE OFF/DPT/SUB                          
         EX    R1,*-6                                                           
*                                                                               
         MVC   TSXKPER,SPACES                                                   
         LA    RF,1(R1,RF)         POINT TO PERSON                              
         IC    R1,LEVLNQD          LEVEL D INDIVIDUAL LENGTH (PERSON)           
         BCTR  R1,0                                                             
         MVC   TSXKPER(0),0(RF)    ISOLATE PERSON CODE                          
         EX    R1,*-6                                                           
BLDHD10  DS    0H                                                               
*                                                                               
         MVC   TSXKEND,RECVPEDT                                                 
         USING T1RAD,R4                                                         
         LA    R4,T1RAWRK          POINT TO WORK AREA                           
         XC    T1RAWRK,T1RAWRK                                                  
         BAS   RE,ZAP1RBUK                                                      
         MVC   T1RKCULA,RECVCULA                                                
         MVC   T1RAPEDT,RECVPEDT                                                
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     BLDHD30                                                          
*                                                                               
BLDHD20  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDHD30  CLC   SVKEY(TSXKSBR-TSXKEY),IOKEY                                      
         BE    *+12                                                             
         OI    T1RAERR,T1RAHDNF     HEADER RECORD NOT FOUND                     
         B     BLDHD70              ADD ERROR TO BIN TABLE                      
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO                                                            
         MVI   ELCODE,XTHELQ       X'BD'-TEMPO HEADER ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    BLDHD40             FOUND ELEMENT                                
*                                                                               
         LA    R6,IO                                                            
         TM    TSXRSTA1,X'10'      ARE THERE OTHER SEQ RECORDS?                 
         BO    BLDHD20             GET NEXT RECORD                              
*                                                                               
         USING XTHELD,R5                                                        
BLDHD40  LA    R6,IO                                                            
         LR    R5,R6                                                            
         AH    R5,=Y(TSXRFST-TSXKEY)                                            
BLDHD50  CLI   0(R5),0                                                          
         BE    BLDHD80                                                          
         CLI   0(R5),XTHELQ        IS IT X'BD' ELEMENT                          
         BE    BLDHD60                                                          
         SR    R1,R1                                                            
         IC    R1,1(R5)            GET LENGTH OF ELEMENT                        
         AR    R5,R1                                                            
         B     BLDHD50                                                          
*                                                                               
BLDHD60  DS    0H                                                               
         ZAP   T1RAHEDB,XTHBHRS      HEADER HRS B TIME                          
         ZAP   T1RAHEDR,XTHRHRS      HEADER HRS R TIME                          
         ZAP   T1RAHEDN,XTHNHRS      HEADER HRS N TIME                          
         ZAP   T1RAHDNC,XTHNCHRS     HEADER HRS NC TIME                         
         MVC   T1RAHLPD,XTHLDCNT     LINES PER DAY                              
         MVC   T1RAHSWC,XTHSCNT      SWIPE COUNTS                               
BLDHD70  GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD TABLE ENTRY             
*                                                                               
BLDHD80  LA    R3,RECVLNQ(R3)                                                   
*                                                                               
         LA    R6,SVKEY            POINT TO SAVED KEY                           
         LA    RF,RECVACC          POINT TO NEXT ENTRY OF TABLE                 
         ZIC   R1,LEVC             GET LENGTH OF LEVA+B+C                       
         LA    RF,0(R1,RF)         POINT TO PERSON                              
         IC    R1,LEVLNQD          PERSONS CODE LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),TSXKPER     ARE WE DOING THE SAME PERSON                 
         BNE   BLDHDX                                                           
*                                                                               
         ZIC   R1,LEVC             GET OFF+DPT+SUB DEPT LENGTH                  
         LA    RF,RECVACC          POINT TO 1R ACCOUNT IN RECOVERY TAB          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSXKODS(0),0(RF)    R V DOING THE SAME OFF/DPT/SUB DPT           
         BNE   BLDHDX              NO EXIT                                      
*                                                                               
         BCT   R0,BLDHD10                                                       
*                                                                               
BLDHDX   B     EXIT                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* READ  TIME SHEET TEMPO INFO DETAIL RECORDS AND FILL IN 1RATAB TABLE*          
* BUCKETS ACCUMULATE EACH TYPE OF TIME HOURS BY READING MINI ELEMENTS*          
* CONVERTS DIFFERENCE BETWEEN TIME INTO NUMBER OF HOURS AND FILLS    *          
* BUCKETS                                                            *          
**********************************************************************          
         SPACE 1                                                                
         USING TSIRECD,R6                                                       
BLDDTBUK NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ARECVTAB                                                      
         ICM   R0,15,BININ         GET NUMBER OF ENTRIES IN A TABLE             
         BZ    BLDDTX                                                           
         LA    R3,BINTAB           POINT TO RECOVERY BIN TABLE                  
         TM    FLAG,FLGWKND        ARE WE DOING IT OVER THE WEEKEND             
         BO    *+8                                                              
         L     R3,ARECVENT         GET ADDRESS OF CORRECT PERS IN TAB           
         DROP  R1                                                               
*                                                                               
         USING RECVD,R3                                                         
         USING T1RAD,R4                                                         
BLDDT10  DS    0H                  BUILD ENTRY TO PUT IN T1RABINTAB             
         LA    R4,T1RAWRK          BUILD ENTRY TO PUT IN T1RABINTAB             
         XC    T1RAWRK,T1RAWRK                                                  
         BAS   RE,ZAP1RBUK                                                      
         MVC   T1RKCULA,RECVCULA        ACCOUNT CODE TO BINTAB                  
         MVC   T1RAPEDT,RECVPEDT        PERIOD END DATE TO BINTAB               
         MVC   SVPERDTE,RECVPEDT                                                
*                                                                               
         XC    SVPERDTE,=X'FFFFFF'                                              
         SR    R1,R1                                                            
         ICM   R1,7,SVPERDTE                                                    
         AHI   R1,1                                                             
         STCM  R1,7,SVPERDTE                                                    
*                                                                               
         LA    R6,SVKEY                READ INFO DETAIL RECORD                  
         XC    SVKEY,SVKEY                                                      
         MVI   TSIKTYP,TSIKTYPQ        3E                                       
         MVI   TSIKSUB,TSIKSUBQ        14                                       
         MVC   TSIKCULA,RECVCULA       MOVE C/U/L/ACC FROM BIN TABLE            
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         OI    T1RAERR,T1RADTNF        DETAIL RECORD NOT FOUND                  
         B     BLDDT30                 ADD ERROR TO BIN TABLE                   
*                                                                               
BLDDT20  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDDT30  CLC   SVKEY(TSIKOFF-TSIKEY),IOKEY                                      
         BNE   BLDDT70                                                          
*                                                                               
         LA    R6,IO                                                            
         CLC   TSIKPEDT,SVPERDTE       IS THIS I NEED                           
         BNE   BLDDT20                                                          
         NI    T1RAERR,X'FF'-T1RADTNF                                           
         BAS   RE,ZAP1RBUK                                                      
*                                                                               
         MVC   LKEY,IOKEY                                                       
         OI    FLAG,FLGSEQ         TURN FLAG ON                                 
         BAS   RE,SEQBF            PUT BF ELEMENTS IN CORRECT SEQUENCE          
         NI    FLAG,X'FF'-FLGSEQ   TURN FLAG OFF                                
*                                                                               
         OI    FLAG,FLGDAY         ADDING DAY HOURS BY DAY                      
         BAS   RE,GTDAYHR          ADD HOURS BY DAY IN DAYTABLE                 
         NI    FLAG,X'FF'-FLGDAY   TURN OFF ADDING DAY HOURS BY DAY             
         NI    T1RAERR1,X'FF'-T1RAO24H                                          
         TM    FLAG,FLGO24         IS DAYHOUR OVER 24 HOURS                     
         BNO   *+8                                                              
         OI    T1RAERR1,T1RAO24H   ANOTHER ERROR                                
*                                                                               
         USING TEMPOD,R5                                                        
         USING BIND,R1                                                          
         L     R1,ATEMPTAB                                                      
         ICM   R2,15,BININ                                                      
         BZ    BLDDT20                                                          
         LA    R5,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         MVC   SVXLINNO,=H'-1'     INITALIZE TEMPO LINE NUMBER                  
         XC    SVLINNO,SVLINNO     INITALIZE TMS LINE NUMBER                    
BLDDT40  CLI   TEMPOTY,TEMPOTD     IS IT LINE INFO 01                           
         BE    BLDDT50             YES                                          
         CLC   SVXLINNO,TEMPOTL#                                                
         BE    BLDDT60                                                          
*                                                                               
         BAS   RE,ZAP1RBUK                                                      
         OI    T1RAERR,T1RALNNF    SOME LINES ARE MISSING                       
         XC    T1RXTLN#,T1RXTLN#                                                
         XC    T1RLINE#,T1RLINE#                                                
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD TABLE ENTRY             
         MVC   T1RXTLN#,TEMPOTL#                                                
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD TABLE ENTRY             
         MVI   T1RAERR,0                                                        
         B     BLDDT60                                                          
*                                                                               
BLDDT50  DS    0H                                                               
         BAS   RE,ZAP1RBUK                                                      
         MVC   SVXLINNO,TEMPOTL#   SAVE TEMPO LINE NUMBER                       
         MVC   SVLINNO,TEMPOL#     SAVE TMS LINE NUMBER                         
         MVC   SVXTMHRS,TEMPOHRS   SAVE HOURS                                   
*                                                                               
         BAS   RE,CKLIVSAL         CHECK LINE INFO VS ALLOCATION HOURS          
         BAS   RE,FLALBUK          FILL IN ALLOCATION BUCKETS                   
         BAS   RE,FLDTBUK          FILL IN DETAIL BUCKETS                       
*                                                                               
         XC    T1RXTLN#(4),T1RXTLN#                                             
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD TABLE ENTRY             
         MVI   T1RAERR1,0                                                       
         MVC   T1RXTLN#,SVXLINNO   MOVE IN TEMPO LINE NUMBER                    
         MVC   T1RLINE#,SVLINNO    MOVE IN TMS LINE NUMBER                      
         XC    T1RADLPD,T1RADLPD   DONT SAVE LIN/DAY CNT HERE                   
         XC    T1RADSWC,T1RADSWC   DONT SAVE SWIPE CNT HERE IT'S WRONG          
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD TABLE ENTRY             
         MVI   T1RAERR,0                                                        
         BAS   RE,ZAP1RBUK                                                      
*                                                                               
BLDDT60  LA    R5,TEMPLNQ(R5)      BUMP TO NEXT ENTRY                           
         BCT   R2,BLDDT40                                                       
*                                                                               
BLDDT70  BAS   RE,ZAP1RBUK                                                      
         GOTO1 ABINADD,DMCB,(RC),T1RAWRK,AT1RATAB   ADD ERROR TO TABLE          
         MVI   T1RAERR,0                                                        
*                                                                               
         LA    R3,RECVLNQ(R3)                                                   
         CLC   T1RKCULA,RECVCULA        ARE WE DOING THE SAME PERSON            
         BNE   BLDDTX                                                           
         BCT   R0,BLDDT10                                                       
*                                                                               
BLDDTX   B     EXIT                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* SEQBF - PUTS THE BF ELEMENTS IN SEQUENCE. FIRST WE HAVE TYPE 1     *          
*         FOLLOWED BY TYPE 3 ETC...                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING TEMPOD,R4                                                        
         USING TSIRECD,R6                                                       
SEQBF    NTR1                                                                   
*                                                                               
         USING BIND,R1                                                          
         L     R1,ATEMPTAB                                                      
         XC    BININ,BININ        CLR NUMBER OF ENTRIES IN BINTABLE             
         DROP  R1                                                               
*                                                                               
         LA    R4,TEMPWRK                                                       
*                                                                               
         MVC   SVKEY,LKEY                                                       
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     SEQBF20                                                          
*                                                                               
SEQBF10  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
SEQBF20  CLC   SVKEY(TSIKOFF-TSIKEY),IOKEY                                      
         BNE   SEQBFX                                                           
*                                                                               
         LA    R6,IO                                                            
         CLC   TSIKPEDT,SVPERDTE    IS THIS CORRECT RECORD                      
         BNE   SEQBF10                                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING XTMELD,R5                                                        
         LA    R5,IO                                                            
         MVI   ELCODE,XTMELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SEQBF30  BAS   RE,NEXTEL                                                        
         BNE   SEQBF10             GET NEXT RECORD                              
*                                                                               
         XC    TEMPWRK,TEMPWRK     CLEAR WORK AREA                              
         CLI   XTMTYP,XTMTNAR      IS IT BF 02?                                 
         BE    SEQBF30             YES, SO LOOK FOR ANOTHER ELEMENT             
         MVC   TEMPOEL,XTMEL       ELEMENT CODE                                 
         MVC   TEMPOTL#,XTMTLIN#   TEMPO LINE #                                 
         MVC   TEMPOTY,XTMTYP      TEMPO TYPE                                   
         MVI   TEMPOSEQ,0                                                       
*                                                                               
         CLI   TEMPOTY,TEMPOTD     IS TYPE BF01                                 
         BNE   SEQBF40             NO                                           
         MVC   TEMPOL#,XTMLIN#     TMS LINE #                                   
         MVC   TEMPOHRS,XTMHRS     HOURS                                        
         MVC   TEMPOTYP,XTMTTYP    TIME TYPE(B/R/N/NC)                          
         B     SEQBF60                                                          
*                                                                               
SEQBF40  SR    R0,R0                                                            
         ICM   R0,1,XTMMINI                                                     
         BZ    SEQBF30                                                          
         LA    R2,XTMDATA                                                       
         USING XTMDATA,R2                                                       
         LA    R3,TEMPDATA                                                      
         USING TEMPDATA,R3                                                      
         MVC   TEMPMINI,XTMMINI    NUMBER OF MINIS FOR DAY                      
SEQBF50  MVC   TEMPSTAT,XTMMSTAT   MINI STATUS                                  
         MVC   TEMPDAY,XTMDAY      DAY                                          
         MVC   TEMPBLK,XTMBLK      HOURS FOR DAY (NON-SWIPE)                    
         LA    R2,XTMLN4Q(R2)                                                   
         LA    R3,TEMPLN4Q(R3)                                                  
         BCT   R0,SEQBF50                                                       
*                                                                               
SEQBF60  GOTO1 ABINADD,DMCB,(RC),TEMPWRK,ATEMPTAB                               
         B     SEQBF30             GET NEXT ELEMENT                             
*                                                                               
SEQBFX   B     EXIT                                                             
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
**********************************************************************          
* BUILD DAYS AND HOURS TOTAL TABLE FROM TEMPTAB                      *          
* BF03 ELEMENT                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING DAYD,R4                                                          
         USING TEMPOD,R5                                                        
GTDAYHR  NTR1                                                                   
*                                                                               
         NI    FLAG,X'FF'-FLGO24   TURN FLAG OFF                                
*                                                                               
         USING BIND,R1                                                          
         L     R1,ADAYTAB                                                       
         XC    BININ,BININ        CLR NUMBER OF ENTRIES IN BINTABLE             
*                                                                               
         L     R1,ATEMPTAB                                                      
         ICM   R2,15,BININ                                                      
         BZ    GTDAYX                                                           
         LA    R5,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
GTDAY10  LA    R4,DAYWRK          DAY TABLE WORKING AREA                        
         XC    DAY,DAY                                                          
         ZAP   DAYHOURS,=P'0'                                                   
         CLI   TEMPOTY,TEMPOTL    IS IT TYPE 3                                  
         BNE   GTDAY40                                                          
         ZICM  R0,TEMPMINI         GET NUMBER OF MINIS                          
*                                                                               
         LA    R3,TEMPDATA         POINT R3 TO MINIS                            
         USING TEMPDATA,R3                                                      
*                                                                               
GTDAY20  TM    TEMPSTAT,TEMPSWPQ   IS IT A SWIPE                                
         BO    GETDAY30            NOT DAY HOURS                                
         MVC   DAY,TEMPDAY                                                      
         ZAP   DAYHOURS,TEMPBLK    ADD HOURS TO TABLE                           
         GOTO1 ABINADD,DMCB,(RC),DAYWRK,ADAYTAB                                 
GETDAY30 LA    R3,TEMPLN4Q(R3)                                                  
         BCT   R0,GTDAY20                                                       
GTDAY40  LA    R5,TEMPLNQ(R5)                                                   
         BCT   R2,GTDAY10                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,ADAYTAB                                                       
         ICM   R2,15,BININ                                                      
         BZ    GTDAYX                                                           
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
GTDAY50  CP    DAYHOURS,=PL8'2400'   DOES ANY DAY HAVE MORE THAN 24 HRS         
         BH    GTDAY60                                                          
         LA    R4,DAYLNQ(R4)                                                    
         BCT   R2,GTDAY50          CHECK NEXT DAY IN TABLE                      
         B     GTDAYX                                                           
GTDAY60  OI    FLAG,FLGO24         THIS DAY HAS MORE THAN 24 HOURS              
GTDAYX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* ROUTINE COMPARES LINE DETAIL INFO HRS WITH SUM OF MINIS(ALLOCATION *          
* SWIPE/DAY) HOURS. TURNS ON AN ERROR BIT IF A DIFFERENCE IS FOUND   *          
**********************************************************************          
         SPACE 1                                                                
         USING TEMPOD,R5                                                        
         USING T1RAD,R4                                                         
CKLIVSAL NTR1                                                                   
*                                                                               
         ZAP   ALLHRS,=P'0'                                                     
         LA    R4,T1RAWRK                                                       
         XC    T1RXTLN#(4),T1RXTLN#   CLEAR PREV ERROR IF ANY                   
*                                                                               
         LA    R5,TEMPLNQ(R5)      BUMP TO NEXT ENTRY                           
         BCTR  R2,0                DECREMENT COUNT OF ENTRYS                    
         CHI   R2,0                                                             
         BE    CKLIERX             MEANS ALLOCATION IS MISSING                  
         CLI   TEMPOTY,TEMPOTD     LINE INFO FOLLOWS LINE INFO ELEM             
         BE    CKLIERX             MEANS ALLOCATION IS MISSING                  
*                                                                               
         CLC   SVXLINNO,TEMPOTL#     DOES BF01 LINE= BF03 LINE                  
         BNE   CKLIERX               ERROR                                      
*                      IT MEANS ALLOCATION OF PREVIOUS LINE AND LINE            
*                      TO WHICH THIS ALLOCATION BELONGS IS MISSING              
*                                                                               
         XC    SVLINPD,SVLINPD     LINE PER DAY COUNT                           
         XC    SVSWCNT,SVSWCNT     NO OF SWIPES                                 
CKLIAL10 BAS   RE,GETALHRS         GET ALLOCATION HRS                           
         MVC   T1RADLPD,SVLINPD    LINE PER DAY COUNT                           
         MVC   T1RADSWC,SVSWCNT    NO OF SWIPES                                 
         LA    R5,TEMPLNQ(R5)      BUMP TO NEXT ENTRY                           
         BCTR  R2,0                DECREMENT COUNT OF ENTRYS                    
         CHI   R2,0                NO MORE                                      
         BE    CKLIALX             EXIT                                         
         CLI   TEMPOTY,TEMPOTD     ANOTHER BF 01 ELEMENT?                       
         BE    CKLIALX             YES SO EXIT                                  
         CLC   SVXLINNO,TEMPOTL#   DOES LINE MATCHES ALLOCATIION LINE           
         BNE   CKLIALX             NO SO EXIT                                   
         B     CKLIAL10            ADD HOURS                                    
*                                                                               
CKLIERX  DS    0H                                                               
         OI    T1RAERR,T1RAALNF      ALLOCATIONS MISSING                        
*                                                                               
CKLIALX  B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE CALCULATES AND ACCUMULATES TIME ALLOCATION HRS IN A FLD    *          
**********************************************************************          
         SPACE 1                                                                
         USING TEMPOD,R5                                                        
GETALHRS NTR1                                                                   
         LA    R6,TEMPDATA                                                      
         USING TEMPDATA,R6                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,TEMPMINI                                                    
         BZ    GETALX                                                           
*                                                                               
GETAL10  DS    0H                                                               
         SR    R1,R1                                                            
         TM    TEMPSTAT,TEMPSWPQ   IS IT A SWIPE                                
         BO    GETAL20                                                          
         ICM   R1,3,SVLINPD        GET LINEHOURS/DAY SO FAR                     
         AHI   R1,1                UPDATE LINE HOURS PER DAY                    
         STCM  R1,3,SVLINPD                                                     
         AP    ALLHRS,TEMPBLK                                                   
         B     GETAL40                                                          
*                                                                               
GETAL20  DS    0H                                                               
         ICM   R1,3,SVSWCNT        GET NO. OF SWIPES SO FAR                     
         AHI   R1,1                UPDATE SWIPES                                
         STCM  R1,3,SVSWCNT                                                     
*                                                                               
         SR    R4,R4                                                            
         SR    R3,R3                                                            
         ICM   R4,3,TEMPEND        GET SWIPE END HOURS                          
         ICM   R3,3,TEMPSTR        GET SWIPE START HOURS                        
         SR    R4,R3                                                            
         BP    *+8                                                              
         AHI   R4,2400          COMPENSATE IF SOMETHING LIKE 11PM-1AM           
         CVD   R4,DUB                                                           
         MP    DUB,=P'10'                                                       
*                                                                               
* CONVERT FROM 100'S TO MINUTES                                                 
*                                                                               
         CP    DUB+6(2),=P'150'                                                 
         BNE   *+14                                                             
         AP    DUB+6(2),=P'100'    CONVERT 15 MIN TO 25                         
         B     GETAL30                                                          
         CP    DUB+6(2),=P'300'                                                 
         BNE   *+14                                                             
         AP    DUB+6(2),=P'200'    CONVERT 30 MIN TO 50                         
         B     GETAL30                                                          
         CP    DUB+6(2),=P'450'                                                 
         BNE   *+14                                                             
         AP    DUB+6(2),=P'300'    CONVERT 45 MIN TO 75                         
         B     GETAL30                                                          
         CP    DUB+6(2),=P'550'    CONVERT 55 MIN TO 25                         
         BNE   *+14                                                             
         SP    DUB+6(2),=P'300'                                                 
         B     GETAL30                                                          
         CP    DUB+6(2),=P'700'    CONVERT 70 MIN TO 50                         
         BNE   *+14                                                             
         SP    DUB+6(2),=P'200'                                                 
         B     GETAL30                                                          
         CP    DUB+6(2),=P'850'    CONVERT 85 MIN TO 75                         
         BNE   *+10                                                             
         SP    DUB+6(2),=P'100'                                                 
*                                                                               
GETAL30  DS    0H                                                               
         DP    DUB,PKROUND                                                      
         AP    ALLHRS,DUB(L'DUB-L'PKROUND)                                      
GETAL40  DS    0H                                                               
         LA    R6,TEMPLN4Q(R6)                                                  
         BCT   R0,GETAL10                                                       
*                                                                               
GETALX   B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* ROUTINE FILLS IN DETAIL BUCKETS IN T1RATABLE                       *          
* R3= AT1RATAB  BINTAB WORK AREA WITH KEY FILLED IN                  *          
* R5=A(BF 01 TEMPO LINE DETAIL ELEMENT, LINE INFO)                   *          
**********************************************************************          
         SPACE 1                                                                
         USING TEMPOD,R5                                                        
FLDTBUK  NTR1                                                                   
*                                                                               
         LA    R1,BUKDSPTB         POINT TO TABLE                               
FLDT10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         CLC   TEMPOTYP,0(R1)      MATCH WITH B,R,N,NC TYPE                     
         BE    *+12                                                             
         LA    R1,L'BUKDSPTB(R1)   BUMP TO NEXT TYPE OF TIME                    
         B     FLDT10                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,2(R1)        GET DISPLACEMENT OF DETAIL BUCKET                
         LA    R3,T1RAWRK                                                       
         AR    R3,RE                  POINT TO BUCKET                           
         ZAP   0(T1RABKLN,R3),TEMPOHRS  ZAP HOURS TO CORRECT BUCKET             
*                                                                               
FLDTX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE FILLS IN ALLOCATION BUCKETS IN T1RATABLE                   *          
* R3= AT1RATAB  BINTAB WORK AREA WITH KEY FILLED IN                  *          
* R5=A(BF 01 TEMPO LINE DETAIL ELEMENT, LINE INFO)                   *          
**********************************************************************          
         SPACE 1                                                                
         USING TEMPOD,R5                                                        
FLALBUK  NTR1                                                                   
*                                                                               
         LA    R1,BUKDSPTB         POINT TO TABLE                               
FLAL10   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         CLC   TEMPOTYP,0(R1)      MATCH WITH B,R,N,NC TYPE                     
         BE    *+12                                                             
         LA    R1,L'BUKDSPTB(R1)   BUMP TO NEXT TYPE OF TIME                    
         B     FLAL10                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,3(R1)        GET DISPLACEMENT OF ALLOCTION BUCKETS            
         LA    R3,T1RAWRK                                                       
         AR    R3,RE                  POINT TO BUCKET                           
         ZAP   0(T1RABKLN,R3),ALLHRS  ZAP HOURS TO CORRECT BUCKET               
*                                                                               
FLALX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE ZAPS ALL BUCKETS OF T1RATAB BINTABLE                       *          
**********************************************************************          
         SPACE 1                                                                
         USING T1RAD,R2                                                         
ZAP1RBUK NTR1                                                                   
*                                                                               
         LA    R0,T1RABKCT         NO. OF BUCKETS                               
         LA    R2,T1RAWRK          POINT TO FIRST BUCKET                        
         LA    R1,T1RABKT                                                       
         ZAP   0(T1RABKLN,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,T1RABKLN(R1)                                                  
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE PRINTS OUT ERROR DESCRIPTION                               *          
* R1= A(T1RAERR)                                                     *          
* R6= A(ERROR TABLE)                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
FILERR   NTR1                                                                   
*                                                                               
         LA    R5,XP                                                            
FILER10  MVC   SVERR,0(R1)         0(R1) IS T1RAERR                             
         CLI   0(R6),X'FF'                                                      
         BE    FILERX              CHECK SECOND ERROR BYTE                      
         NC    SVERR,0(R6)         WHICH ERROR OCCURED                          
         BZ    *+14                                                             
         MVC   PERROR,1(R6)        MOVE ERROR DESCRIPTION                       
         BAS   RE,PRINTIT                                                       
         LA    R6,L'ERRTAB(R6)                                                  
         B     FILER10                                                          
FILERX   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE COMPARES ALL THE BUCKETS                                   *          
* R2= A(BINTABLE) AT1RATAB                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING T1RAD,R2                                                         
CMPHRS   NTR1                                                                   
*                                                                               
         LA    R4,T1RATIM          POINT TO TIME BUCKETS                        
         LA    R5,T1RAHED          POINT TO HEADER BUCKETS                      
         LA    R6,T1RADET          POINT TO DETAIL BUCKETS                      
         LA    R7,T1RAAL           POINT TO ALLOCATION BUCKETS                  
         LHI   R1,4                LOOP THROUGH 4 TYPES OF TIME                 
CMPHR10  DS    0H                                                               
         OC    T1RXTLN#(4),T1RXTLN#  HEADER RECD NOT BY LINE NUMBER             
         BNZ   CMPHR20                                                          
         CP    0(T1RABKLN,R4),0(T1RABKLN,R5)    TIME VS HEAD                    
         BE    CMPHR20                                                          
         OI    T1RAERR,T1RATMHD                 TIME DON'T MATCH HEADER         
CMPHR20  CP    0(T1RABKLN,R4),0(T1RABKLN,R6)    TIME VS DETAIL                  
         BE    CMPHR30                                                          
         OI    T1RAERR,T1RATMDT                 TIME DON'T MATCH DETAIL         
CMPHR30  CP    0(T1RABKLN,R4),0(T1RABKLN,R7)    TIME VS ALLOCATION              
         BE    CMPHR40                                                          
         OI    T1RAERR,T1RATMAL            TIME DON'T MATCH ALLOCATION          
CMPHR40  DS    0H                                                               
         LA    R4,T1RABKLN(R4)     BUMP TIME BUCKET                             
         LA    R5,T1RABKLN(R5)     BUMP HEADER BUCKET                           
         LA    R6,T1RABKLN(R6)     BUMP DETAIL BUCKET                           
         LA    R7,T1RABKLN(R7)     BUMP ALLOCATIION BUCKET                      
         BCT   R1,CMPHR10                                                       
*                                                                               
CMPHRX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE COMPARES HEADERS LINE PER DAY AND SWIPE COUNT WITH DETAIL  *          
* RECORDS ALLOCATIONS LINE AND SWIPE COUNT TOTAL                     *          
* SET T1RAALNF AND T1RATMAL TO OFF IF THE ABOVE COUNTS ARE ZERO MEANS*          
* IF HOURS ENTERED WERE NEITHER LINES/DAY NOR SWIPES                 *          
* R2= A(BINTABLE) AT1RATAB                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING T1RAD,R2                                                         
CMPHACNT NTR1                                                                   
*                                                                               
         CLC   T1RAHLPD,T1RADLPD     IS HDR LINE/DAY= DETAIL                    
         BNE   CMPHA10                                                          
         CLC   T1RAHSWC,T1RADSWC     ARE HDR SWIPES = DETAIL                    
         BNE   CMPHA10                                                          
         OC    T1RAHLPD(4),T1RAHLPD  WERE ANY LINES OR SWIPES PASSED            
         BNZ   CMPHAX                                                           
         NI    T1RAERR,X'FF'-(T1RAALNF+T1RATMAL) IT IS TOTAL HOURS              
         B     CMPHAX                                                           
CMPHA10  OI    T1RAERR,T1RAHALM      HEADER ALLOCATIONS MISMATCH                
*                                                                               
CMPHAX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
         EX    R1,*-6                                                           
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC            A(ACCOUNT RECORD)                            
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         MVC   0(0,R2),0(R1)                                                    
         EX    R4,*-6                                                           
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME ROUTINE                                                   *          
*     R2 = NAME ELEMENT (SOURCE)                                     *          
*     R3 = NAME FIELD   (DESTINATION)                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
GETNME   NTR1                                                                   
         MVC   0(L'LEVNMES,R3),SPACES                                           
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNX                                                            
         MVC   0(0,R3),NAMEREC                                                  
         EX    R1,*-6                                                           
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD3+1(10),=CL10'LOGIN ID :'                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         CLI   RCSUBPRG,2                                                       
         BNE   HEADX10                                                          
*                                                                               
         USING HEADD,R3                                                         
         LA    R3,XHEAD6                                                        
         USING HEADD,R3                                                         
         MVC   HEADSUM,=C'BAD RECORDS SUMMARY PAGE'                             
         LA    R3,XHEAD7                                                        
         MVC   HEADSUM,=C'------------------------'                             
         B     HEADX                                                            
*                                                                               
HEADX10  LA    R3,XHEAD5                                                        
         MVC   HEADDESC,LEVADSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVACDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVANME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD6                                                        
         MVC   HEADDESC,LEVBDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVBCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVBNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD7                                                        
         MVC   HEADDESC,LEVCDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVCCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVCNME      LEVEL NAME                                 
*                                                                               
HEADX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ACCMST   DC    CL8'ACCMST'                                                      
PKROUND  DC    XL2'010C'                                                        
PERRDESC DC    CL(L'PERRMSG)'RECS IN ERROR FOR THIS CPY'                        
PERRCPY  DC    CL(L'PERRMSG)'TOTAL NO. OF CPYS IN ERROR'                        
PERRRUN  DC    CL(L'PERRMSG)'TOTAL NO. OF RECS IN ERROR'                        
         SPACE 2                                                                
**********************************************************************          
* ERRORS TABLE ERRORS LIKE RECORD NOT FOUND ETC                      *          
**********************************************************************          
         SPACE 1                                                                
ERRTAB   DS    0CL(ERRLNQ)                                                      
ERRLN    DS    0X                                                               
         DC    AL1(T1RAHDNF),CL(L'PERROR)'HEADER RECORD NOT FOUND '             
ERRLNQ   EQU   *-ERRLN                                                          
         DC    AL1(T1RADTNF),CL(L'PERROR)'DETAIL RECD NOT FOUND   '             
         DC    AL1(T1RALNNF),CL(L'PERROR)'LINE INFOS ARE MISSING  '             
         DC    AL1(T1RAALNF),CL(L'PERROR)'ALLOCATIONS MISSING     '             
         DC    AL1(T1RATMHD),CL(L'PERROR)'TIME HRS <> HEADER HRS  '             
         DC    AL1(T1RATMDT),CL(L'PERROR)'TIME HRS <> DETAIL HRS  '             
         DC    AL1(T1RATMAL),CL(L'PERROR)'TIME HRS <> ALLOCATION  '             
         DC    AL1(T1RAHALM),CL(L'PERROR)'HDR LIN/SWP CNT <> ALLOC'             
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
**********************************************************************          
* ERRORS TABLE FOR MORE ERRORS                                       *          
**********************************************************************          
         SPACE 1                                                                
ERRTAB1  DS    0CL(ERRLNQ1)                                                     
ERRLN1   DS    0X                                                               
         DC    AL1(T1RAO24H),CL(L'PERROR)'SOME DAY/S EXCEED 24 HRS'             
ERRLNQ1  EQU   *-ERRLN1                                                         
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
**********************************************************************          
* TABLES  BUKDSPTB 1R TIME HOURS DISPLACEMENT TABLE                  *          
**********************************************************************          
         SPACE 1                                                                
BUKDSPTB DS    0CL4                                                             
         DC    AL1(TIMTCB),AL1(T1RATIMB-T1RAKEY),AL1(T1RADETB-T1RAKEY)          
         DC    AL1(T1RAALB-T1RAKEY)                                             
         DC    AL1(TIMTCR),AL1(T1RATIMR-T1RAKEY),AL1(T1RADETR-T1RAKEY)          
         DC    AL1(T1RAALR-T1RAKEY)                                             
         DC    AL1(TIMTCN),AL1(T1RATIMN-T1RAKEY),AL1(T1RADETN-T1RAKEY)          
         DC    AL1(T1RAALN-T1RAKEY)                                             
         DC    AL1(TIMTNC),AL1(T1RATMNC-T1RAKEY),AL1(T1RADTNC-T1RAKEY)          
         DC    AL1(T1RAALNC-T1RAKEY)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
         DC    A(RECVTAB)          1R ACC TABLE BUILT FROM RECOVERY             
         DC    A(T1RATAB)          1R ACC TABLE BUILT FROM FILE                 
         DC    A(TEMPTAB)          TABLE BUILT OF TEMPO ELEMENTS                
         DC    A(DAYTAB)           TABLE BUILT OF TEMPO ELEMENTS                
         DC    A(SETDATE)          SET START AND END DATES                      
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    V(PRNTBL)           PRINT DATA                                   
         SPACE 2                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        +        
               MACRF=GM,EODAD=ENDIN                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,56,ELCODE                                                     
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET START AND END DATES                                             *         
***********************************************************************         
         SPACE 1                                                                
SETDATE  DS    0D                                                               
         NMOD1 0,*SETDAT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   PRTSTART,SPACES                                                  
         MVC   PRTEND,SPACES                                                    
         XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    SETDT10                                                          
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
         GOTO1 DATCON,DMCB,(0,WORK),(5,PRTSTART)                                
*                                                                               
SETDT10  MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    SETDTX                                                           
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   SETDT20                                                          
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
SETDT20  GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
         GOTO1 DATCON,DMCB,(0,WORK),(5,PRTEND) FOR PRINTING ON REPORT           
SETDTX   XIT1                                                                   
**********************************************************************          
* LITERALS SETDATE NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         CLI   RCSUBPRG,2                                                       
         BNE   BXHK10                                                           
*                                                                               
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         MVI   BOXCOLS+(PCNT-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXXIT                                                            
*                                                                               
BXHK10   MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PPEDT-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PXLIN#-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PLINE#-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PBHEAD-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PRHEAD-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNHEAD-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNCHEAD-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PBTIME-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PRTIME-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNTIME-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNCTIME-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PBDET-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PRDET-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PNDET-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PNCDET-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PBALOC-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PRALOC-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNALOC-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PNCALOC-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PERROR-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         BAS   RE,GRPCOLS                                                       
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-DIVIDE GROUP CODE COLUMN                                        *         
***********************************************************************         
         SPACE 1                                                                
GRPCOLS  NTR1                                                                   
         USING PLINED,R5                                                        
         LA    R5,XHEAD11                                                       
         LA    R6,PBUCKET-1                                                     
         LA    R4,XHEAD9                                                        
         LA    R4,PBUCKET-PRTLINE-1(R4)                                         
         LHI   R1,4                                                             
GRPCOL10 DS    0H                                                               
         LHI   R0,4                                                             
         MVI   0(R6),X'EB'                       LEFT T DASH                    
GRPCOL20 MVI   1(R6),X'BF'                       MOVE IN DASH                   
         MVC   2(PRHEAD-PBHEAD-2,R6),1(R6)       PROPAGATE DASHES               
         MVI   L'PBUCKET(R6),X'CC'               TOP T DASH                     
         MVI   L'PBUCKET(R4),X'BF'          OVERRIDE TOP T WITH DASH            
         LA    R6,L'PBUCKET(R6)                                                 
         LA    R4,L'PBUCKET(R4)                                                 
         BCT   R0,GRPCOL20                                                      
*                                                                               
         MVI   0(R4),X'CC'                                                      
         MVI   0(R6),X'EC'                                                      
         BCT   R1,GRPCOL10                                                      
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS BXHOOK  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
BINA10   GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         C     R5,ARECVTAB         ARE WE ADDING TO RECVTAB                     
         BE    BINXIT              NOTHING AFTER KEY: NO BUCKETS                
*                                                                               
         TM    FLAG,FLGSEQ                                                      
         BNO   BINA20                                                           
         ZIC   RE,TEMPOSEQ-TEMPOD(R3)                                           
         LA    RE,1(RE)                                                         
         STC   RE,TEMPOSEQ-TEMPOD(R3)                                           
         B     BINA10                                                           
*                                                                               
BINA20   TM    FLAG,FLGDAY         ARE WE ADDING TO DAYTAB                      
         BO    BINA35                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LR    RE,R3                                                            
         LHI   R6,T1RAERR-T1RAD    DISPLACEMENT TO ERROR                        
         AR    R4,R6               BUMP TO ERROR BYTE IN TABLE                  
         AR    RE,R6               BUMP TO ERROR BYTE IN NEW ITEM               
         OC    0(L'T1RAERR,R4),0(RE)                                            
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LR    RE,R3                                                            
         LHI   R6,T1RAERR1-T1RAD   DISPLACEMENT TO ERROR 1                      
         AR    R4,R6               BUMP TO ERROR BYTE IN TABLE                  
         AR    RE,R6               BUMP TO ERROR BYTE IN NEW ITEM               
         OC    0(L'T1RAERR1,R4),0(RE)                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LR    RE,R3                                                            
         LHI   R6,T1RALNCT-T1RAD   DISPLACEMENT TO HEADER LIN/DAY               
         AR    R4,R6               BUMP TO LINE COUNTER IN TABLE                
         AR    RE,R6               BUMP TO ERROR BYTE IN NEW ITEM               
         LHI   R0,T1RACTQ          NO. OF COUNTERS TO BE UPDATED                
BINA30   SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R1,3,0(R4)                                                       
         ICM   R2,3,0(RE)                                                       
         AR    R1,R2               UPDATE NEW COUNT                             
         STCM  R1,3,0(R4)                                                       
         LA    R4,L'T1RALNCT(R4)   BUMP TO NEXT COUNTER                         
         LA    RE,L'T1RALNCT(RE)   BUMP TO NEXT COUNTER                         
         BCT   R0,BINA30                                                        
*                                                                               
BINA35   L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA40   AP    0(L'RECVBKT,R4),0(L'RECVBKT,R3)   ADD TO BUCKET                  
         LA    R3,L'RECVBKT(R3)    BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,L'RECVBKT(R4)    BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA40                                                        
*                                                                               
         B     BINXIT                                                           
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  BINADD NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 APRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        +        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  DUMP NMOD1                                               *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
*                                                                               
* BINTABLE 1 - 1R ACCOUNT TABLE BUILT OFF RECOVERY TAPE                         
*                                                                               
         DC    C'***RECV***'                                                    
RECVTAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(RECVLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(RECVKLNQ)           KEY LENGTH                               
         DC    AL4(RECVMAX)            MAX IN TABLE                             
         DC    AL1(RECVBKCT)           NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(RECVBKT-RECVD)      DISPLACEMENT TO FIRST BUCKET             
         DS    (RECVMAX*RECVLNQ)XL1    TABLE                                    
*                                                                               
RECVMAX  EQU   20000                                                            
*                                                                               
* BINTABLE 2 - 1R ACCOUNT TABLE BUILT IN PROCESS ACCOUNT                        
*                                                                               
         DC    C'**T1RACCT*'                                                    
T1RATAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 2               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(T1RALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(T1RAKLNQ)           KEY LENGTH                               
         DC    AL4(T1RAMAX)            MAX IN TABLE                             
         DC    AL1(T1RABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(T1RABKT-T1RAD)      DISPLACEMENT TO FIRST BUCKET             
         DS    (T1RAMAX*T1RALNQ)XL1    TABLE                                    
*                                                                               
T1RAMAX  EQU  16000                                                             
*                                                                               
* BINTABLE 3 - TEMPO ELEMENTS TABLE                                             
*                                                                               
         DC    C'**TEMPOEL*'                                                    
TEMPTAB  DS    0D                      BINTABLE CONSTANTS                       
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TEMPLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TEMPKLNQ)           KEY LENGTH                               
         DC    AL4(TEMPOMAX)           MAX IN TABLE                             
         DC    AL1(TEMPBKCT)           NUMBER OF BUCKETS                        
         DC    AL1(TEMPOBKT-TEMPOD)    DISPLACEMENT TO FIRST BUCKET             
         DS    (TEMPOMAX*TEMPLNQ)XL1   TABLE                                    
*                                                                               
TEMPOMAX EQU   400                                                              
*                                                                               
* BINTABLE 4 - DAY TABLE BULIT OF BF03 ELEMENT                                  
*                                                                               
         DC    C'***DAY****'                                                    
DAYTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 4               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(DAYLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(DAYKLNQ)            KEY LENGTH                               
         DC    AL4(DAYMAX)             MAX IN TABLE                             
         DC    AL1(DAYBKCT)            NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(DAYBKT-DAYD)        DISPLACEMENT TO FIRST BUCKET             
         DS    (DAYMAX*DAYLNQ)XL1      TABLE                                    
*                                                                               
DAYMAX   EQU   40                                                               
*                                                                               
         SPACE 1                                                                
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACTAD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
VTYPES   DS    0A                                                               
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
ARECVTAB DS    A                   1R ACC TABLE BUILT FROM RECOVERY             
AT1RATAB DS    A                   1R ACC TABLE BUILT FROM FILE                 
ATEMPTAB DS    A                   TABLE BUILT OF TEMPO ELEMENTS                
ADAYTAB  DS    A                   TABLE BUILT OF BF03 ELEMENTS DAY/HR          
ASETDATE DS    A                   SETS START AND END DATES                     
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
APRNTBL  DS    V                   PRINT DATA                                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
PKCOUNT  DS    PL4                 TOTAL RECORD IN A COMPANY IN ERROR           
PKCPYCNT DS    PL4                 TOTAL NO. OF COMPANIES IN ERROR              
PKRUNTOT DS    PL4                 TOTAL RECORDS FOR RUN IN ERROR               
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
ELCODE   DS    XL1                                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGRCV   EQU   X'80'               BUILDING RECOVERY MYSELF                     
FLGWKND  EQU   X'40'               RUNNING ON WEEKEND                           
FLGPRNT  EQU   X'20'               MARK SOMETHING WAS PRINTED                   
FLGSEQ   EQU   X'10'               ADDING SEQUENCE ELEMENTS TO BINTABLE         
FLGDAY   EQU   X'08'               ADDING DAY HOURS TO BINTABLE                 
FLGO24   EQU   X'04'               SOME DAY/S HAVE OVER 24 HOURS                
*                                                                               
ERRFLG   DS    XL1                 ERROR FLAGS FOR E-MAIL                       
ERRFLG1  DS    XL1                                                              
*                                                                               
SVKEY    DS    CL42                                                             
LKEY     DS    CL42                                                             
SVPEDT   DS    XL3                                                              
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
SVXLINNO DS    XL2                 SAVED TEMPO LINE NUMBER                      
SVLINNO  DS    XL2                 SAVED TMS LINE NO. FROM XTMELEM              
SVXTMHRS DS    CL3                 SAVED HOURS FROM XTMELEM                     
SVPERDTE DS    XL3                 SAVED PERIOD END DATE IN COMPRESSED          
SVERR    DS    XL1                 SAVED ERROR T1RAERR                          
ALLHRS   DS    PL4                 ALLOCATION HRS                               
SVSWCNT  DS    XL2                 SWIPE COUNTS ALLOCATION LINES                
SVLINPD  DS    XL2                 LINE PER DAY COUNT ALLOCATIONS               
*                                                                               
ARECVENT DS    A               ADD OF CURNT MTCH IN RECOVERY BIN TAB            
LASTKEY  DS    CL(TIMKEND)                                                      
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
PRTSTART DS    CL8                                                              
PRTEND   DS    CL8                                                              
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVNMES  DS    0CL36               LEVEL NAMES                                  
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVDNME  DS    CL36                LEVEL D NAME                                 
*                                                                               
CPYTAB   DS    (CPYMAX)CL(CPYLNQ)  COMPANIES IN ERROR TABLE                     
CPYMAX   EQU   50                                                               
*                                                                               
RECVWRK  DS    CL(RECVLNQ)         BINSEARCH WORK AREA - RECV TABLE             
T1RAWRK  DS    CL(T1RALNQ)         BINSEARCH WORK AREA - 1R ACCT TABLE          
TEMPWRK  DS    CL(TEMPLNQ)         BINSEARCH WORK AREA - TEMP TABLE             
DAYWRK   DS    CL(DAYLNQ)          BINSEARCH WORK AREA - DAY TABLE              
*                                                                               
RECVIO   DS    CL(RCVRECRD-RCVRECD+2000)                                        
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
         EJECT                                                                  
*---------------------------------------------------------------                
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
***********************************************************************         
* DSECT FOR CPYTAB                                                    *         
***********************************************************************         
         SPACE 1                                                                
CPYTABD  DSECT                                                                  
CPYID    DS    CL2                                                              
CPYLOGID DS    CL7                 CPY LOGIN ID                                 
CPYBAD   DS    PL4                 NO. OF BAD RECORDS                           
CPYEFLG  DS    X                   ERROR FLAGS FOR E-MAIL                       
CPYEFLG1 DS    X                                                                
CPYLNQ   EQU   *-CPYTABD                                                        
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PCODE    DS    CL8                 PERSON CODE                                  
         DS    CL1                                                              
PPEDT    DS    CL8                 PERIOD END DATE                              
         DS    CL2                                                              
PXLIN#   DS    CL4                 TEMPO LINE NUMBER                            
         DS    CL2                                                              
PLINE#   DS    CL4                 LINE NUMBER                                  
         DS    CL2                                                              
*                                                                               
PBUCKET  DS    0CL8                                                             
*                                                                               
PBHEAD   DS    CL7                 B  TIME HEAD REC                             
PBUCLEN  EQU   *-PBUCKET                                                        
         DS    CL1                                                              
PRHEAD   DS    CL7                 R  TIME HEAD REC                             
         DS    CL1                                                              
PNHEAD   DS    CL7                 N  TIME HEAD REC                             
         DS    CL1                                                              
PNCHEAD  DS    CL7                 NC  TIME HEAD REC                            
         DS    CL1                                                              
*                                                                               
PBTIME   DS    CL7                 B TIME TIME REC                              
         DS    CL1                                                              
PRTIME   DS    CL7                 R TIME TIME REC                              
         DS    CL1                                                              
PNTIME   DS    CL7                 N  TIME TIME REC                             
         DS    CL1                                                              
PNCTIME  DS    CL7                 NC  TIME TIME REC                            
         DS    CL1                                                              
*                                                                               
*                                                                               
PBDET    DS    CL7                 B  TIME DETAIL RECORD                        
         DS    CL1                                                              
PRDET    DS    CL7                 R  TIME DETAIL RECORD                        
         DS    CL1                                                              
PNDET    DS    CL7                 N  TIME DETAIL RECORD                        
         DS    CL1                                                              
PNCDET   DS    CL7                 NC TIME DETAIL RECORD                        
         DS    CL1                                                              
PBALOC   DS    CL7                 ALLOCATION SUM OF HOURS B                    
         DS    CL1                                                              
PRALOC   DS    CL7                 ALLOCATION SUM OF HOURS R                    
         DS    CL1                                                              
PNALOC   DS    CL7                 ALLOCATION SUM OF HOURS N                    
         DS    CL1                                                              
PNCALOC  DS    CL7                 ALLOCATION SUM OF HOURS NC                   
         DS    CL1                                                              
PERROR   DS    CL24                ROOM FOR ERROR                               
*                                                                               
         ORG   PRTLINE                                                          
         DS    CL30                                                             
PERRMSG  DS    CL27                NO. OF RECORDS IN ERROR                      
PCNT     DS    CL6                                                              
         ORG                                                                    
PLINELNQ EQU   *-PRTLINE                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADLINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADDESC DS    CL15                                                             
         DS    CL1                                                              
HEADCODE DS    CL6                 1R OFFICE                                    
         DS    CL1                                                              
HEADNAME DS    CL36                DECSRIPTION                                  
*                                                                               
         ORG   HEADD                                                            
         DS    CL85                                                             
HEADSUM  DS    CL24                BAD RECORDS SUMMARY PAGE                     
         DS    CL1                                                              
         ORG                                                                    
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT 1R ACCOUNT TABLE BUILT OFF OF RECOVERY TAPE                   *         
***********************************************************************         
         SPACE 1                                                                
RECVD    DSECT                                                                  
RECVCULA DS    0CL15               1R ACC CODE                                  
RECVCPY  DS    XL1                 COMPANY CODE                                 
RECVUL   DS    CL2                 1R UNIT LEDGER                               
RECVACC  DS    CL12                1R ACC CODE                                  
RECVPEDT DS    XL3                 PERIOD END DATE                              
RECVKLNQ EQU   *-RECVD                                                          
RECVBKT  DS    0PL8                BUCKET                                       
RECVBKLN EQU   *-RECVBKT           BUCKET LENGTH                                
RECVBKCT EQU   (*-RECVBKT)/RECVBKLN NUMBER OF BUCKETS                           
RECVLNQ  EQU   *-RECVD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN 1R ACCOUNT TABLE                                 *         
***********************************************************************         
         SPACE 1                                                                
T1RAD    DSECT                                                                  
T1RAKEY  DS    0CL20                                                            
T1RKCULA DS    0CL15               ACCOUNT C/U/L/A                              
T1RACPY  DS    CL1                 COMPANY CODE                                 
T1RAUL   DS    CL2                 UNIT LEDGER                                  
T1RACCT  DS    CL12                1R ACCOUNT CODE                              
T1RAPEDT DS    XL3                 PERIOD END DATE                              
T1RXTLN# DS    XL2                 TEMPO LINE NUMBER                            
T1RLINE# DS    XL2                 TMS LINE NUMBER                              
T1RAKLNQ EQU   *-T1RAD             LENGTH OF KEY                                
*                                                                               
T1RALNCT DS    0XL2    COUNTERS FOR SWIPES AND HOURS BY DAY                     
T1RAHLPD DS    XL2                 TOT NO OF LINS/DAY HEADER                    
T1RAHSWC DS    XL2                 TOTAL NO. OF SWIPES SENT VIA HEADER          
T1RADLPD DS    XL2                 TOT NO OF LINS/DAY DETAIL                    
T1RADSWC DS    XL2                 TOTAL NO. OF SWIPES SENT VIA DETAIL          
T1RACTLQ EQU   (*-T1RALNCT)                                                     
T1RACTQ  EQU   (*-T1RALNCT)/L'T1RALNCT TOTAL NUMBER OF COUNTERS                 
*                                                                               
T1RAERR  DS    XL1                 POSSIBLE ERRORS                              
T1RAHDNF EQU   X'80'               HEADER RECORD NOT FOUND                      
T1RADTNF EQU   X'40'               DETAIL RECORD NOT FOUND                      
T1RALNNF EQU   X'20'               SOME LINES ARE MISSING                       
T1RAALNF EQU   X'10'               SOME ALLOCATIONS ARE MISSING                 
T1RATMHD EQU   X'08'               TIME DON'T MATCH HEADER                      
T1RATMDT EQU   X'04'               TIME DON'T MATCH DETAIL                      
T1RATMAL EQU   X'02'               TIME DON'T MATCH ALLOCATION                  
T1RAHALM EQU   X'01'    HEADER VS ALLOCATION LINE/DAY, SWP MISMATCH             
*                                                                               
T1RAERR1 DS    XL1                 POSSIBLE ERRORS                              
T1RAO24H EQU   X'80'               DAY HAS MORE THAN 24 HOURS                   
*                                                                               
T1RABKT  DS    0PL8                BUCKET                                       
*                                                                               
T1RAHED  DS    0PL8                HEAD BUCKETS                                 
T1RAHEDB DS    PL8                 B  TYPE TIME HEADER REC                      
T1RABKLN EQU   *-T1RABKT           BUCKET LENGTH                                
T1RAHEDR DS    PL8                 R  TYPE TIME HEADER REC                      
T1RAHEDN DS    PL8                 N  TYPE TIME HEADER REC                      
T1RAHDNC DS    PL8                 NC TYPE TIME HEADER REC                      
*                                                                               
T1RATIM  DS    0PL8                TIME BUCKETS                                 
T1RATIMB DS    PL8                 B  TYPE TIME TIME REC                        
T1RATIMR DS    PL8                 R  TYPE TIME TIME REC                        
T1RATIMN DS    PL8                 N  TYPE TIME TIME REC                        
T1RATMNC DS    PL8                 NC TYPE TIME TIME REC                        
*                                                                               
T1RADET  DS    0PL8                DETAIL BUCKETS                               
T1RADETB DS    PL8                 B  TYPE TIME DETAIL REC                      
T1RADETR DS    PL8                 R  TYPE TIME DETAIL REC                      
T1RADETN DS    PL8                 N  TYPE TIME DETAIL REC                      
T1RADTNC DS    PL8                 NC TYPE TIME DETAIL REC                      
*                                                                               
T1RAAL   DS    0PL8                ALLOCATION HOUR BUCKETS                      
T1RAALB  DS    PL8                 ALLOCATION LINES HRS B TYPE                  
T1RAALR  DS    PL8                 ALLOCATION LINES HRS R TYPE                  
T1RAALN  DS    PL8                 ALLOCATION LINES HRS N TYPE                  
T1RAALNC DS    PL8                 ALLOCATION LINES HRS NC TYPE                 
*                                                                               
T1RABKCT EQU   (*-T1RABKT)/T1RABKLN NUMBER OF BUCKETS                           
T1RALNQ  EQU   *-T1RAD             LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR TDETAIL ELEMENT FOLLOWED BY TLINE ELEMENTS                *         
***********************************************************************         
         SPACE 1                                                                
TEMPOD   DSECT                                                                  
TEMPOEL  DS    XL1                 ELEMENT CODE                                 
TEMPOTL# DS    XL2                 TEMPO LINE NUMBER                            
TEMPOTY  DS    XL1                 LINE INFO TYPE                               
TEMPOTD  EQU   1                   LINE INFO (TDETAIL)                          
TEMPOTL  EQU   3                   ALLOCATION LINE (TLINE)                      
TEMPOSEQ DS    XL1                 SEQUENCE NUMBER (ONLY FOR BF03)              
TEMPKLNQ EQU   *-TEMPOD                                                         
TEMPOINP DS    0C                                                               
TEMPOL#  DS    XL2                 TMS LINE NUMBER                              
TEMPOHRS DS    PL3                 HOURS                                        
TEMPOTYP DS    XL1                 TIME TYPE(B/R/N/NC)                          
         ORG   TEMPOINP                                                         
TEMPMINI DS    XL1                 NUMBER OF MINIS FOR DAY                      
TEMPDATA DS    0C                  DAY AND HOURS                                
TEMPSTAT DS    XL1                 MINI STATUS                                  
TEMPSWPQ EQU   X'80'               MINI IS A SWIPE                              
TEMPDAY  DS    XL1                 DAY                                          
TEMPBLK  DS    0PL4                HOURS FOR DAY(NON-SWIPE)                     
TEMPSTR  DS    XL2                 START OF SWIPE                               
TEMPEND  DS    XL2                 END OF SWIPE                                 
TEMPLN4Q EQU   *-TEMPDATA                                                       
TEMPOBKT DS    0PL8                BUCKET                                       
TEMPBKLN EQU   *-TEMPOBKT          BUCKET LENGTH                                
TEMPBKCT EQU   (*-TEMPOBKT)/TEMPBKLN NUMBER OF BUCKETS                          
TEMPLNQ  EQU   255                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT DAY HOURS BUILT OFF FROM XTMELD TYPE 3 (ALLOCATION) ELEMENT   *         
***********************************************************************         
         SPACE 1                                                                
DAYD     DSECT                                                                  
DAY      DS    XL1                 DAY 1,2,3, ETC                               
DAYKLNQ  EQU   *-DAYD                                                           
DAYBKT   DS    0PL8                BUCKET                                       
DAYHOURS DS    PL8                 DAY HOURS                                    
DAYBKLN  EQU   *-DAYBKT            BUCKET LENGTH                                
DAYBKCT  EQU   (*-DAYBKT)/DAYBKLN  NUMBER OF BUCKETS                            
DAYLNQ   EQU   *-DAYD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPTA02 01/06/12'                                      
         END                                                                    
