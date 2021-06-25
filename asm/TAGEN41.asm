*          DATA SET TAGEN41    AT LEVEL 192 AS OF 10/18/16                      
*PHASE T70241B,*                                                                
         TITLE 'T70241 - TALENT MULTIPLE COMMERCIAL ESTIMATES'                  
T70241   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MAXRECLQ,T70241,RA,R6,R5,CLEAR=YES,RR=R2                         
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=LOCAL WORKING STORAGE                     
         USING ESTD,R7                                                          
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0      SYSTEM INITIALIZATION                        
         SPACE 2                                                                
         ST    R3,AESTREC          SAVE A(ESTIMATE RECORD)                      
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
         ST    RC,SAVRC            SAVE RC FOR NMOD'S                           
         SPACE 2                                                                
         CLI   MODE,PRINTREP       RECOGNIZE ONLY PRINT REPORT MODE             
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         SPACE 3                                                                
YES      XR    RC,RC               EXITS                                        
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
YESR4    XR    RC,RC                                                            
NOR4     LTR   RC,RC                                                            
XITR4    XIT1  REGS=(R4)                                                        
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINES TO CONTROL GENERATION OF REPORT                         
         SPACE 1                                                                
         USING TAESD,R4                                                         
PREP     NTR1                                                                   
         CLI   FORMAT,TAEPFDWN     IF FORMAT FOR DOWNLOAD                       
         BE    *+8                                                              
         CLI   FORMAT,TAEPFRCD     OR RECAP DOWNLOAD                            
         BNE   *+8                                                              
         MVI   PRTDET,C'N'         YES, DON'T WANT DETAILS PRINTED              
*                                                                               
         GOTO1 =A(INIT),DMCB,RR=RELO                                            
         SPACE 1                                                                
         OC    ESTKEY2(L'ESTKEY2*3),ESTKEY2 IF MORE THAN ONE ESTIMATE           
         BZ    PREP2                                                            
         MVC   BLOCK(L'ESTKEY*4),ESTKEY     PRE-READ ESTIMATES TO FIND          
         LA    R0,4                         OUT ABOUT MISC. ADDITIONS           
         LA    R4,BLOCK                                                         
PREP1    BAS   RE,GETEREC                                                       
         LA    R4,L'ESTKEY(R4)                                                  
         BCT   R0,PREP1                                                         
         SPACE 1                                                                
PREP2    LA    R4,ESTKEY           R4=A(PRIMARY ESTIMATE KEY)                   
         BAS   RE,GETEREC          GET THE RECORD                               
         SPACE 1                                                                
PREP3    L     R4,AESTREC          R4=A(ESTIMATE RECORD)                        
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   PREP4                                                            
         CLI   PRTDET,C'Y'         AND ASKED TO PRINT DETAILS                   
         BNE   PREP4                                                            
         MVC   AIO,AESTREC                                                      
         GOTO1 AESTPRT,DMCB,(TRACEOPT,(RC)),0 PRINT THE RECORD                  
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
PREP4    BAS   RE,PROCCLI          PROCESS A CLIENT                             
         BNE   PREP20                                                           
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,NEXTEL           GET NEXT ESTIMATE ELEMENT                    
         BNE   PREP4                                                            
         CLI   TAESTYPE,TAESTPRD   IF IT'S A PRODUCT, GO PROCESS                
         BE    PREP6                                                            
         BH    PREP7               IF HIGHER THEN MUST BE COMMERCIAL            
         B     PREP4               ELSE IT MUST BE A CLIENT                     
         SPACE 1                                                                
PREP6    BAS   RE,PROCPRD          PROCESS A PRODUCT                            
         BNE   PREP4                                                            
         SPACE 1                                                                
PREP7    BAS   RE,PROCCOM          PROCESS AN ACTUAL COMMERCIAL                 
         BNE   PREP9                                                            
PREP7A   BAS   RE,PROCHPRF         PROCESS HYPOTHETICAL PERFORMERS              
         BAS   RE,PROCPRF          PROCESS ACTUAL PERFORMERS                    
         BAS   RE,PROCHPRF         PROCESS HYPOTHETICAL PERFORMERS              
         SPACE 1                                                                
         CLI   PROFSUPP,C'Y'       IF SUPPRESSING INACTIVE COMMLS               
         BNE   *+12                                                             
         TM    STATUS2,COMLACTV    THEN TEST WHETHER THERE WAS ACTIVITY         
         BZ    PREP8                                                            
         MVI   ESTMODE,PROCCOML    SET COMMERCIAL RECORD                        
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         SPACE 1                                                                
PREP8    BRAS  RE,PRO04ACN         IF CANADIAN CAST STILL NEEDS TO BE           
         BE    PREP7A              PROCESSED FOR 2404A, LOOP BACK               
         SPACE 1                                                                
         MVC   FLUSHCOM,TGCOM      SET COMMERCIAL FILTER                        
         BAS   RE,FLUSHCST         PROCESS CAST RECORDS WAITING FOR IT          
         B     PREP7                                                            
         SPACE 1                                                                
PREP9    CLI   TAESTYPE,TAESTHCO   DID WE GET A HYPO COMMERCIAL                 
         BNE   PREP6               NO, SO TRY FOR ANOTHER PRODUCT               
PREP10   BAS   RE,PROCHCOM         PROCESS A HYPOTHETICAL COMMERCIAL            
         BNE   PREP14                                                           
PREP11   BAS   RE,PROCHPRF         PROCESS HYPOTHETICAL PERFORMERS              
         SPACE 1                                                                
         ICM   R2,15,AHCOMENT      R2=A(CURRENT HYPO COMML TAB ENTRY)           
         BZ    PREP12                                                           
         USING HCOMD,R2                                                         
         MVC   HCOMNAME,COMNAME    SET HYPOTHETICAL COMMERCIAL NAME             
         DROP  R2                                                               
         SPACE 1                                                                
PREP12   CLI   PROFSUPP,C'Y'       IF SUPPRESSING INACTIVE COMMLS               
         BNE   *+12                                                             
         TM    STATUS2,COMLACTV    THEN TEST WHETHER THERE WAS ACTIVITY         
         BZ    PREP10                                                           
         MVI   ESTMODE,PROCCOML    SET COMMERCIAL RECORD                        
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         SPACE 1                                                                
         BRAS  RE,PRO04ACN         IF CANADIAN CAST STILL NEEDS TO BET          
         BE    PREP11              PROCESSED FOR 2404A, LOOP BACK               
         BNE   PREP10                                                           
         SPACE 1                                                                
PREP14   CLI   TAESTYPE,TAESTCOM   DID WE GET AN ACTUAL COMMERCIAL              
         BE    PREP7               YES, SO PROCESS IT                           
         B     PREP6               ELSE TRY FOR ANOTHER PRODUCT                 
         SPACE 1                                                                
PREP20   XC    FLUSHCOM,FLUSHCOM   CLEAR COMMERCIAL FILTER SO THAT WE           
         BAS   RE,FLUSHCST         PROCESS REMAINING CAST RECORDS               
         SPACE 1                                                                
         BAS   RE,MISCADD          PROCESS MISCELLANEOUS ADDITIONS              
         BAS   RE,ANOTHER          TEST FOR ANOTHER ESTIMATE KEY                
         BE    PREP3                                                            
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   PREP40                                                           
         L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'40',ERPESTH)                              
         MVC   ESTKEY,KEY                                                       
         LA    R4,ESTKEY                                                        
         BAS   RE,GETEREC                                                       
         BAS   RE,CVRSHEET         PRINT COVER SHEET IF DEFINED                 
         XC    ESTKEY,ESTKEY                                                    
         SPACE 1                                                                
PREP40   GOTO1 =A(CALLDRV),DMCB,GLOUTPUT,RR=RELO  PRINT THE REPORT              
         BAS   RE,FINAL            WRAP-UP                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FLUSH TABLE OF HELD CAST MEMBERS                      
         SPACE 1                                                                
FLUSHCST NTR1                                                                   
         OI    STATUS,FLUSHING     SET FLUSHING HOLD TABLE                      
         SPACE 1                                                                
         L     R2,HOLDTAB          R2=A(HOLD TABLE)                             
         L     RF,MAXHOLD          RF=MAXIMUM NUMBER OF TABLE ENTRIES           
         MHI   RF,HOLDLNQ                                                       
         GOTO1 =A(MYTRACE2),DMCB,=C'HTAB',(R2),(RF),RR=RELO                     
         SPACE 1                                                                
         USING HOLDD,R2                                                         
         L     R0,MAXHOLD          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
FCST10   OC    HOLDD(HOLDLNQ),HOLDD SKIP NULL ENTRIES                           
         BZ    FCST70                                                           
         SPACE 1                                                                
         OC    FLUSHCOM,FLUSHCOM   IF COMMERCIAL FILTER DEFINED                 
         BZ    *+14                                                             
         CLC   FLUSHCOM,HOLDCOM    THEN LOOK FOR ENTRIES WAITING FOR IT         
         BNE   FCST70                                                           
         SPACE 1                                                                
         L     R4,HOLDACLI         R4=A(CLIENT ESTIMATE DETAILS EL.)            
         USING TAESD,R4                                                         
         C     R4,ACLIEL           IF DON'T HAVE CLIENT                         
         BE    *+8                                                              
         BAS   RE,PROCCLI          PROCESS CLIENT                               
         SPACE 1                                                                
         L     R4,HOLDAPRD         R4=A(PRODUCT ESTIMATE DETAILS EL.)           
         SPACE 1                                                                
         C     R4,APRDEL           IF DON'T HAVE PRODUCT                        
         BE    FCST30                                                           
         LTR   R4,R4               AND PRODUCT EL. DEFINED                      
         BZ    *+12                                                             
         BAS   RE,PROCPRD          PROCESS PRODUCT                              
         B     FCST30                                                           
         SPACE 1                                                                
         ST    R4,APRDEL           ELSE CLEAR PRODUCT VALUES                    
         XC    TGPRD,TGPRD                                                      
         SPACE 1                                                                
FCST30   L     R4,HOLDACOM         R4=A(COMML. ESTIMATE DETAILS EL.)            
         SPACE 1                                                                
         OI    STATUS,SEENCOML     ASSUME WE'VE SEEN COMMERCIAL                 
         C     R4,ACOMEL           IF DON'T HAVE COMMERCIAL                     
         BE    FCST40                                                           
         CLI   TAESTYPE,TAESTCOM   THEN IF ACTUAL COMMERCIAL TYPE               
         BNE   *+12                                                             
         BAS   RE,PROCCOM          PROCESS COMMERCIAL                           
         B     *+8                                                              
         BAS   RE,PROCHCOM         ELSE PROCESS HYPO. COMMERCIAL                
         SPACE 1                                                                
FCST40   XR    R4,R4               CLEAR R4                                     
         OC    HOLDSEQ,HOLDSEQ     IF CAST SEQUENCE NUMBER IS ZERO              
         BNZ   FCST50                                                           
         L     R4,HOLDAPRF         SET R4=A(PERF. ESTIMATE DETAILS EL.)         
         SPACE 1                                                                
         CLI   TAESTYPE,TAESTHPE   IF HYPO. PERFORMER TYPE                      
         BNE   *+12                                                             
         BAS   RE,PROCHPRF         THEN PROCESS HYPOTHETICAL PERFORMER          
         B     FCST60                                                           
         SPACE 1                                                                
FCST50   LA    R3,CASTKEY          R3=A(CAST KEY)                               
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     BUILD IT                                     
         MVI   TLCACD,TLCACDQ      RECORD CODE                                  
         MVC   TLCACOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLCASORT,HOLDSORT   SORT KEY FROM HOLD TABLE                     
         LTR   R4,R4               IF A(PERF. ELEMENT DEFINED)                  
         BZ    *+10                                                             
         MVC   TLCASORT,TAESSORT   TAKE SORT KEY FROM THERE                     
         SPACE 1                                                                
         BAS   RE,PROCPRF          PROCESS PERFORMER                            
         SPACE 1                                                                
FCST60   XC    HOLDD(HOLDLNQ),HOLDD CLEAR PROCESSED ENTRIES                     
         SPACE 1                                                                
FCST70   LA    R2,HOLDNEXT         BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,FCST10           AND PROCESS                                  
         SPACE 1                                                                
FCSTX    NI    STATUS,ALL-FLUSHING TURN OFF FLUSH STATUS                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SEARCHES FOR ADDITIONAL ESTIMATES                        
         SPACE 1                                                                
ANOTHER  NTR1                                                                   
         LA    R4,ESTKEY2                                                       
         BAS   RE,GETEREC                                                       
         BE    YES                                                              
         LA    R4,ESTKEY3                                                       
         BAS   RE,GETEREC                                                       
         BE    YES                                                              
         LA    R4,ESTKEY4                                                       
         BAS   RE,GETEREC                                                       
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO GET AN ESTIMATE RECORD                                
         SPACE 1                                                                
*                                  R4=A(KEY)                                    
GETEREC  NTR1                                                                   
         OC    0(L'ESTKEY,R4),0(R4)  TEST KEY DEFINED                           
         BZ    NO                                                               
         MVC   KEY(L'ESTKEY),0(R4)   SET KEY                                    
         XC    0(L'ESTKEY,R4),0(R4)  CLEAR PASSED KEY SO DON'T REPEAT           
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R3,KEY                                                           
         USING TLESD,R3                                                         
         CLC   TLESKEY,KEYSAVE     DID WE GET THE RECORD                        
         BNE   NO                                                               
         SPACE 1                                                                
         MVC   AIO,AESTREC                                                      
         GOTO1 GETREC              GET RECORD INTO AESTREC                      
         MVC   AIO,AIO1                                                         
         NI    STATUS,ALL-NOMORE   SET WE'VE GOT A NEW RECORD                   
         SPACE 1                                                                
GREC4    GOTO1 SEQ                             LOOK FOR ANOTHER RECORD          
         LA    R3,KEY                                                           
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE  WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO IO1                      
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R3,AESTREC                             R3=A(MAIN RECORD)         
         LH    RF,TLESLEN-TLESD(RE)  L'NEW RECORD                               
         SH    RF,DATADISP           LESS DATADISP                              
         SPACE 1                                                                
         LH    R1,TLESLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
         SPACE 1                                                                
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLESLEN          IS L'COMBINED RECORD                         
         SPACE 1                                                                
         AR    R0,R3               R0=A(END OF MAIN RECORD)                     
         AH    RE,DATADISP         RE=A(1ST EL. IN NEW RECORD)                  
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
         SPACE 1                                                                
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
         SPACE 1                                                                
GRECX    BRAS  RE,ADPCCOMS         ADD PER CYCLE COMM'LS TO ESTIMATE            
         SPACE 1                                                                
         MVC   AIO,AESTREC                                                      
         L     R4,AESTREC                                                       
         MVI   ELCODE,TAEMELQ      LOOK FOR ESTIMATE MISC ADDITION ELS          
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    STATUS3,MISCAFLG    SET HAVE MISC. ADDITIONS                     
         GOTO1 =A(MYTRACE),DMCB,=C'EST',RR=RELO                                 
         MVC   AIO,AIO1                                                         
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT COVER SHEET                                     
         SPACE 1                                                                
CVRSHEET NTR1                                                                   
         CLI   FORMAT,TAEPFDWN     IF FORMAT FOR DOWNLOAD                       
         BE    CVRX                                                             
         CLI   FORMAT,TAEPFRCD     OR RECAP DOWNLOAD                            
         BE    CVRX                DON'T WANT COVER SHEET                       
         MVI   FORCEHED,C'Y'                                                    
         L     R4,AESTREC                                                       
         MVI   ELCODE,TAENELQ      LOOK FOR ESTIMATE NARRATIVE ELS.             
         BAS   RE,GETEL                                                         
         BNE   CVRX                                                             
         USING TAEND,R4            R4=A(ESTIMATE NARRATIVE EL.)                 
         SPACE 1                                                                
         CLI   BOXOPT,C'N'         UNLESS BOXES DISABLED                        
         BE    CVR10                                                            
         L     R3,ABOX             SET UP BOX AROUND TEXT                       
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+13,C'T'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+35,C'L'                                                  
         MVI   BOXCOLS+96,C'R'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         SPACE 1                                                                
CVR10    MVI   SPACING,11          SKIP A BUNCH OF LINES                        
         BRAS  RE,PRNTIT                                                        
         SPACE 1                                                                
CVR20    ZIC   R1,TAENLEN                                                       
         SH    R1,=AL2(TAENLNQ+1)                                               
         BM    CVR30               DUMMY EL - SKIP A LINE                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+36(0),TAENNARR    TEXT TO PRINT LINE                           
         SPACE 1                                                                
CVR30    BRAS  RE,PRNTIT           PRINT THE LINE                               
         SPACE 1                                                                
         BAS   RE,NEXTEL           LOOK FOR ANOTHER ELEMENT                     
         BE    CVR20                                                            
         BAS   RE,BXBOT            NO MORE - END BOX                            
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       SET TO START NEW PAGE                        
         SPACE 1                                                                
CVRX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES MISCELLANEOUS ADDITIONS TO ESTIMATE            
         SPACE 1                                                                
MISCADD  NTR1                                                                   
         ZIC   R3,SUMSVIND         R3=LAST INDEX USED FOR MISCADDS              
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,NALL+1           SET ONE LARGER THAN REGULAR ACCUM            
         SPACE 1                                                                
         L     R4,AESTREC                                                       
         MVI   ELCODE,TAEMELQ      LOOK FOR ESTIMATE MISC ADDITION ELS          
         BRAS  RE,GETEL                                                         
         BNE   MISCX                                                            
         USING TAEMD,R4            R4=A(ESTIMATE MISC. ADDTION EL.)             
         SPACE 1                                                                
MISC10   STC   R3,SUMINDEX         SET DUMMY INDEX                              
         SPACE 1                                                                
         MVC   SUMLIT,SPACES       CLEAR SUMMARY LITERAL                        
         ZIC   R1,TAEMLEN                                                       
         SH    R1,=AL2(TAEMLNQ+1)                                               
         BM    MISC20              NO TEXT PRESENT                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUMLIT(0),TAEMTXT   TEXT TO PRINT LINE                           
         SPACE 1                                                                
MISC20   MVC   SUMAMT,TAEMAMT      SET AMOUNT                                   
         GOTO1 =A(CNVAMTS),DMCB,SUMAMT,1,RR=RELO   CONVERT AMOUNT               
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  OFF TO DRIVER                  
         SPACE 1                                                                
         LA    R3,1(R3)            BUMP INDEX NUMBER                            
         BRAS  RE,NEXTEL                                                        
         BE    MISC10                                                           
         STC   R3,SUMSVIND         SAVE NEXT INDEX NUMBER FOR MISCADDS          
         MVI   SUMINDEX,0          CLEAR SUMMARY INDEX                          
         SPACE 1                                                                
MISCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO WRAP-UP REPORT                                        
         SPACE 1                                                                
FINAL    NTR1                                                                   
         CLI   FORMAT,TAEPFDWN     IF FORMAT FOR DOWNLOAD                       
         BE    FINX                                                             
         CLI   FORMAT,TAEPFRCD     OR RECAP DOWNLOAD                            
         BE    FINX                EXIT NOW                                     
*                                                                               
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    FIN1                                                             
         TM    WHEN,X'40'          AND IF SPOOLING NOW                          
         BZ    FINX                                                             
         L     R3,AREPBLK          SET SPOOLED MESSAGE                          
         USING FAREPBLKD,R3                                                     
         TM    REPIND1,REPIPUT     IF NO REPORT GENERATED                       
         BZ    FINX                LET GENCON GIVE 'NOTHING GEN' MSG            
         B     REPSPOOL            ELSE, SET $DQU (UNLESS STEREO ACTV)          
*                                  AND MESSAGE ON RETURN                        
FIN1     BAS   RE,BXBOT            SET TO END BOX NOW                           
         GOTO1 =A(PRTEND),DMCB,RR=RELO                                          
         SPACE 1                                                                
FINX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CLIENTS                                       
         SPACE 1                                                                
PROCCLI  NTR1                                                                   
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BO    PCLI2                                                            
         BAS   RE,CLIEL            GET CLIENT ELEMENT                           
         BNE   NOR4                                                             
         SPACE 1                                                                
PCLI2    ST    R4,ACLIEL           R4=A(CLIENT ESTIMATE DETAILS EL.)            
         USING TAESD,R4                                                         
         MVC   TGCLI,TAESCLI       SET GLOBAL CLIENT                            
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',0)  READ CLIENT RECORD                
         BRAS  RE,CLIADDL          SET ADDITIONAL CLIENT INFORMATION            
*                                                                               
PCLI5    LA    R3,PRDOPTS          IF NO PRODUCT DEFINED OPTS ARE HERE          
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         SPACE 1                                                                
         XC    TGPRD,TGPRD         RESET PRODUCT                                
         GOTO1 =A(CALLDRV),DMCB,GLINIT,RR=RELO  INITIALIZE DRIVER               
         B     YESR4                                                            
         SPACE 3                                                                
*              ROUTINE TO PROCESS PRODUCTS                                      
         SPACE 1                                                                
PROCPRD  NTR1                                                                   
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BO    PPRD2                                                            
         TM    STATUS,NOMORE       TEST SO DON'T CLEAR APRDEL AT END            
         BO    NOR4                                                             
         XC    APRDEL,APRDEL                                                    
         BAS   RE,PRDEL            GET PRODUCT ELEMENT                          
         BNE   NOR4                                                             
         SPACE 1                                                                
PPRD2    ST    R4,APRDEL           R4=A(PRODUCT ESTIMATE DETAILS EL.)           
         USING TAESD,R4                                                         
         MVC   TGPRD,TAESPRD       SET GLOBAL PRODUCT                           
         SPACE 1                                                                
         BAS   RE,PRDADDL          GET ADDITIONAL PRODUCT INFO                  
         SPACE 1                                                                
         LA    R3,PRDOPTS                                                       
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         B     YESR4                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS COMMERCIALS                                   
         SPACE 1                                                                
PROCCOM  NTR1                                                                   
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BO    PCOM2                                                            
**NO-OP  NI    CSFSTAT,X'FF'-CSFUNI  RESET CSF UNI STATUS FOR EACH COMM         
         NI    CSFSTAT,X'FF'-CSFUNI-COMSSMW RESET UNI,SMW FOR EACH COMM         
         BAS   RE,COMEL            GET COMMERICIAL ELEMENT                      
         BNE   NOR4                                                             
         SPACE 1                                                                
PCOM2    ST    R4,ACOMEL           R4=A(COMMERCIAL ESTIMATE DETAILS EL)         
         USING TAESD,R4                                                         
         MVC   TGCOM,TAESCOM       SET GLOBAL COMMERCIAL                        
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'24',0)  READ COMML RECORD                
         BNE   NOR4                                                             
         GOTO1 =A(MYTRACE),DMCB,=C'COML',RR=RELO                                
         BAS   RE,COMADDL          SET SOME ADD'L INFORMATION                   
         SPACE 1                                                                
         LA    R3,COMOPTS                                                       
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         SPACE 1                                                                
         L     RE,CASTTAB          CLEAR CAST TABLE                             
         L     RF,MAXCAST                                                       
         SLA   RF,1                MULTIPLY MAX N'ENTRIES*2                     
         XCEFL                                                                  
         SPACE 1                                                                
         BRAS  RE,ADDCOM           ADD TO COMMERCIAL TABLE                      
         BE    PCOMX               SKIP IF ALREADY PROCESSED THIS COMML         
         SPACE 1                                                                
         CLI   ACTLOPT,C'Y'        IF ACTUALS REQUESTED                         
         BE    PCOM6                                                            
         CLI   ACTLOPT,C'P'        OR PAYMENTS REQUESTED                        
         BE    PCOM6                                                            
         CLI   ACTLOPT,C'U'        OR ACTUALS REQ/NO TNH ON UNBILLED            
         BE    PCOM6                                                            
         CLI   ACTLOPT,C'Z'        OR ACTUALS REQUESTED/UNBILLED EXCL.          
         BE    PCOM6                                                            
         CLI   ACTLOPT,C'O'        OR ONLY ACTUALS REQUESTED                    
         BNE   PCOMX                                                            
PCOM6    BAS   RE,ACTLOPTS         EXTRACT OPTIONS FOR ACTUALS                  
         BAS   RE,ACTUALS          GET ACTUAL PAYMENTS                          
         SPACE 1                                                                
PCOMX    B     YESR4                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS PERFORMERS                                    
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
PROCPRF  NTR1                                                                   
         NI    TCSTAT2,X'FF'-TCVNR1U                                            
         TM    STATUS,FLUSHING     SKIP IF FLUSHING HOLD TABLE                  
         BO    PPRF3                                                            
         L     R4,ATHISEST                                                      
         ST    R4,ALASTEST                                                      
         MVI   ELCODE,TAESELQ                                                   
PPRF1    BAS   RE,NEXTEL           LOOK FOR ANY PERFORMER ESTIMATE EL.          
         BNE   PPRF2                                                            
         CLI   TAESTYPE,TAESTPER                                                
         BNE   PPRF2                                                            
         ST    R4,ALASTEST         SAVE A(LAST ONE)                             
         B     PPRF1                                                            
         SPACE 1                                                                
PPRF2    XC    KEY,KEY             INITIALIZE KEY                               
         SPACE 1                                                                
PPRF3    BAS   RE,GETCAST          GET (FIRST/)NEXT CAST MEMBER                 
         BNE   PPRFX                                                            
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BZ    *+14                                                             
         LTR   R4,R4               TEST WHETHER WE HAVE A(EST EL.)              
         BZ    PPRF4               NO - SET DUMMY                               
         B     PPRF6               ELSE CONTINUE                                
         SPACE 1                                                                
         XC    APRFEL,APRFEL                                                    
         BAS   RE,PEREL            LOOK FOR CORRESPONDING EST. EL.              
         BE    PPRF6                                                            
         NI    STATUS,ALL-NOMORE   NOT FOUND - INSURE WE KEEP ON TRYING         
PPRF4    XC    WORK,WORK                                                        
         LA    R4,WORK             SET DUMMY FOR SETOPTS                        
         B     *+8                                                              
PPRF6    ST    R4,APRFEL           R4=A(PERFORMER ESTIMATE DETAILS EL.)         
         LA    R3,PEROPTS                                                       
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         BAS   RE,PERADDL          SET ADDITIONAL PERFORMER INFO.               
         SPACE 1                                                                
         CLI   XPERF,C'Y'          EXCLUDE THIS PERFORMER                       
         BE    PPRF3                                                            
         BAS   RE,CHKCAST          TEST WHETHER CAST HAD ACTUAL USAGE           
         SPACE 1                                                                
         BRAS  RE,GUARBLD          INIT GUAR CONTROL BLOCK                      
         BNE   PPRF8               SKIP - WILL PROCESS LATER                    
         SPACE 1                                                                
         BAS   RE,ESTIMATE         PROCESS ESTIMATED PAYMENTS                   
         SPACE 1                                                                
PPRF8    TM    STATUS,SEENCOML     IF WE'VE SEEN THIS COMML BEFORE SKIP         
         BO    PPRF10                                                           
         CLI   PROFSUPP,C'Y'       OR IF SUPPRESSING INACTIVE COMMLS            
         BNE   *+12                                                             
         TM    STATUS2,CASTACTV    THEN TEST WHETHER THERE WAS ACTIVITY         
         BZ    PPRF10                                                           
         MVI   ESTMODE,PROCCAST    SET CAST RECORD                              
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         SPACE 1                                                                
PPRF10   TM    STATUS,FLUSHING     DONE IF FLUSHING HOLD TABLE                  
         BZ    PPRF3               ELSE LOOK FOR ANOTHER PERFORMER              
         SPACE 1                                                                
PPRFX    MVC   ATHISEST,ALASTEST   SET EL. POINTER TO LAST PERF. EL.            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS HYPOTHETICAL COMMERCIALS                      
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
PROCHCOM NTR1                                                                   
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BO    PHCOM2                                                           
**NO-OP  NI    CSFSTAT,X'FF'-CSFUNI  RESET CSF UNI STATUS FOR EACH COMM         
         NI    CSFSTAT,X'FF'-CSFUNI-COMSSMW RESET UNI,SMW FOR EACH COMM         
         BAS   RE,HYCOMEL          GET HYPOTHETICAL COMMERCIAL ELEMENT          
         BNE   NOR4                                                             
         SPACE 1                                                                
PHCOM2   ST    R4,ACOMEL           R4=A(HYPO COMML ESTIMATE DETAILS EL)         
         GOTO1 =A(MYTRACE2),DMCB,=C'HCOM',(R4),16,RR=RELO                       
         SPACE 1                                                                
         LA    R3,COMOPTS                                                       
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         SPACE 1                                                                
         BAS   RE,HCOMADDL         SET ADDITIONAL HYPO. COMM'L. INFO.           
         BAS   RE,ADDHCOM          ADD TO HYPO. COMMERCIAL TABLE                
         SPACE 1                                                                
         XC    KEY,KEY             SET TO DO AN I/O TO RESET CPU TIMER          
         MVC   KEY(1),IOBYTE       (NECESSARY FOR SOON JOBS WITH LARGE          
         GOTO1 HIGH                                  HYPO COMMERCIALS.)         
         XI    IOBYTE,X'F0'        TOGGLE HOB TO ENSURE WE DO THE I/O           
         B     YESR4                                                            
         EJECT                                                                  
*              ROUTINE TO PROCESS HYPOTHETICAL PERFORMERS                       
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
PROCHPRF NTR1                                                                   
         OI    STATUS,HYPOPERF     SET PROCESSING HYPO. PERFORMER               
         SPACE 1                                                                
         TM    STATUS,FLUSHING     IF FLUSHING HOLD TABLE R4 IS SET             
         BO    PHPRF3                                                           
PHPRF2   BAS   RE,HYPEREL          GET HYPOTHETICAL PERFORMER ELEMENT           
         BNE   PHPRFX                                                           
         SPACE 1                                                                
PHPRF3   ST    R4,APRFEL           R4=A(HYPO PERF. ESTIMATE DETAILS EL)         
         LA    R3,PEROPTS                                                       
         BAS   RE,SETOPTS          SET OPTIONS FOR THIS LEVEL                   
         SPACE 1                                                                
         ZIC   R2,TAESHNPR         R2=N'PERFORMERS IN THIS ELEMENT              
         SPACE 1                                                                
PHPRF4   GOTO1 =A(MYTRACE2),DMCB,=C'HPRF',(R4),16,RR=RELO                       
         SPACE 1                                                                
         BAS   RE,HPERADDL         SET ADDITIONAL HYPO. PERFORMER INFO.         
         SPACE 1                                                                
         TM    STATUS3,US2404A     IF PROCESSING 2404A US$ CAST                 
         BZ    PHPRF6                                                           
         CLI   TGUNEQU,ACT         ONLY PROCESS US$ CAST AT THIS TIME           
         BE    PHPRF10                                                          
         SPACE 1                                                                
PHPRF6   TM    STATUS3,CAN2404A    IF PROCESSING 2404A CAN$ CAST                
         BZ    PHPRF6A                                                          
         CLI   TGUNEQU,ACT         ONLY PROCESS CAN$ CAST AT THIS TIME          
         BNE   PHPRF10                                                          
         SPACE 1                                                                
PHPRF6A  TM    STATUS3,PRO2404     IF PROCESSING 2404 CAST                      
         BZ    PFPRF7                                                           
         CLI   TGUNEQU,NON         EXCLUDE NON UNION PERFORMERS                 
         BE    PHPRF10                                                          
         SPACE 1                                                                
PFPRF7   BAS   RE,PERADDL          SET ADDITIONAL PERFORMER INFO.               
         SPACE 1                                                                
         CLI   XPERF,C'Y'          EXCLUDE THIS PERFORMER                       
         BE    PHPRF10                                                          
         SPACE 1                                                                
         CLI   TGUNEQU,SAG         IF UNION IS SAG                              
         BE    PFPRF7A                                                          
         CLI   TGUNEQU,AFT         OR AFTRA,                                    
         BNE   *+8                                                              
PFPRF7A  OI    CSFSTAT,CSFUNI      SET STATUS FOR CSF                           
         SPACE 1                                                                
         BRAS  RE,GUARBLD          INIT GUAR CONTROL BLOCK                      
         BNE   PHPRF8              SKIP - WILL PROCESS LATER                    
         SPACE 1                                                                
         BAS   RE,ESTIMATE         GET ESTIMATED PAYMENTS                       
         SPACE 1                                                                
PHPRF8   TM    STATUS,SEENCOML     IF WE'VE SEEN THIS COMML BEFORE SKIP         
         BO    PHPRF10                                                          
         CLI   PROFSUPP,C'Y'       OR IF SUPPRESSING INACTIVE COMMLS            
         BNE   *+12                                                             
         TM    STATUS2,CASTACTV    THEN TEST WHETHER THERE WAS ACTIVITY         
         BZ    PHPRF10                                                          
         MVI   ESTMODE,PROCCAST    SET CAST RECORD                              
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         SPACE 1                                                                
PHPRF10  BCT   R2,PHPRF4           LOOP FOR N'PERFORMERS                        
         SPACE 1                                                                
         TM    STATUS,FLUSHING     IF NOT FLUSHING HOLD TABLE                   
         BZ    PHPRF2              LOOK FOR ANOTHER HYPO. PERF. EL.             
         SPACE 1                                                                
PHPRFX   NI    STATUS,ALL-HYPOPERF TURN OFF PROCESSING HYPO. PERFORMER          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR FINDING ESTIMATE ELEMENTS                           
         SPACE 1                                                                
         USING TAESD,R4                                                         
CLIEL    DS    0H                                                               
         MVI   BYTE,TAESTCLI       SET CLIENT TYPE                              
         B     ALLEL                                                            
         SPACE 1                                                                
PRDEL    MVI   BYTE,TAESTPRD       SET PRODUCT TYPE                             
         B     ALLEL                                                            
         SPACE 1                                                                
COMEL    MVI   BYTE,TAESTCOM       SET COMMERCIAL TYPE                          
         B     ALLEL                                                            
         SPACE 1                                                                
PEREL    MVI   BYTE,TAESTPER       SET PERFORMER TYPE                           
         B     ALLEL                                                            
         SPACE 1                                                                
HYCOMEL  MVI   BYTE,TAESTHCO       SET HYPOTHETICAL COMMERCIAL TYPE             
         B     ALLEL                                                            
         SPACE 1                                                                
HYPEREL  MVI   BYTE,TAESTHPE       SET HYPOTHETICAL PERFORMER TYPE              
         SPACE 1                                                                
ALLEL    NTR1                                                                   
         TM    STATUS,NOMORE       NO MORE TO CHECK                             
         BO    NOR4                                                             
         MVI   ELCODE,TAESELQ      SET ESTIMATE ELEMENT CODE                    
         C     R4,AESTREC          IF WE'RE POINTING AT KEY                     
         BNE   *+12                                                             
ALLEL2   BAS   RE,GETEL            START AT BEGINNING                           
         B     ALLEL3                                                           
         L     R4,ATHISEST         START AT LAST ELEMENT                        
ALLEL2D  BAS   RE,NEXTEL                                                        
ALLEL3   BE    *+12                                                             
         OI    STATUS,NOMORE       SET NO MORE ESTIMATE ELEMENTS                
         B     NOR4                                                             
         SPACE 1                                                                
         CLC   TAESTYPE,BYTE       DID WE GET CORRECT TYPE                      
         BNE   NOR4                                                             
         CLI   BYTE,TAESTPER       SPECIAL FOR PERFORMERS                       
         BNE   ALLELX                                                           
         CLC   TAESSORT,TGCSORT    MATCH TO CAST SORT KEY                       
         BE    YESR4                                                            
         B     ALLEL2D             ELSE TRY AGAIN                               
         SPACE 1                                                                
ALLELX   ST    R4,ATHISEST         SAVE THIS ADDRESS                            
         B     YESR4               RETURN ADDRESS, CC EQUAL                     
         EJECT                                                                  
*              ROUTINE SETS ADDITIONAL CLIENT INFORMATION                       
         SPACE 1                                                                
*              ROUTINE SETS ADDITIONAL PRODUCT INFORMATION                      
         SPACE 1                                                                
PRDADDL  NTR1                                                                   
         NI    CSFSTAT,X'FF'-CSFPRDY-CSFPRDN   TURN OFF PRD CSF STATUS          
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'20',0)  READ PRODUCT REC.                 
         SPACE 1                                                                
**NO-OP**GOTO1 =A(MYTRACE),DMCB,=C'PROD',RR=RELO                                
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,0         GET NAME                          
         MVC   PRDNAME,TGNAME                 AND SAVE IN W/S                   
*                                                                               
         L     R4,AIO              GET PRODUCT INFO ELEMENT                     
         USING TAPID,R4                                                         
         MVI   ELCODE,TAPIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         TM    TAPISTAT,TACIJPCY   CAN CHARGE JPC CSF?                          
         BZ    *+8                                                              
         OI    CSFSTAT,CSFPRDY     CLIENT CSF YES                               
         TM    TAPISTAT,TAPIJPCN   DON'T CHARGE JPC CSF?                        
         BZ    *+8                                                              
         OI    CSFSTAT,CSFPRDN     CLIENT CSF NO                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE SETS ADDITIONAL COMMERCIAL INFORMATION                   
         SPACE 1                                                                
COMADDL  NTR1                                                                   
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLCOD,R4                                                         
         CLC   TGPRD,TLCOPRD       IF WE HAVE NEW PRODUCT                       
         BE    COMAD2                                                           
         MVC   TGPRD,TLCOPRD       SAVE PRODUCT                                 
         MVC   AIO,AIO2                                                         
         BAS   RE,PRDADDL          GET ADDITIONAL PRODUCT INFO                  
         ST    R4,AIO                                                           
         SPACE 1                                                                
COMAD2   GOTO1 CHAROUT,DMCB,TANAELQ,0  GET NAME                                 
         MVC   COMNAME,TGNAME          AND SAVE IN W/S                          
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            FIND COMMERCIAL DETAILS EL.                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELTACO,0(R4)        SAVE ENTIRE ELEMENT                          
         SPACE 1                                                                
         USING TACOD,R4                                                         
         CLI   TACOLEN,TACOLNQ2                                                 
         BL    COMAD10                                                          
         TM    TACOSTA3,TACOSSMW   IF COMM HAS SOCIAL MEDIA WAIVER,             
         BZ    COMAD10                                                          
         OI    CSFSTAT,COMSSMW     SET SOCIAL MEDIA WAIVER STATUS               
         SPACE 1                                                                
COMAD10  GOTO1 MEDVAL,DMCB,TACOMED    VALIDATE MEDIA                            
         MVI   CANDOLS,C'N'                                                     
         TM    TACOSTAT,TACOSCAN                                                
         BZ    *+8                                                              
         MVI   CANDOLS,C'Y'        SET CANADIAN DOLLAR INVOICE                  
         MVC   SVCANDOL,CANDOLS                                                 
         SPACE 1                                                                
         NI    STATUS3,X'FF'-US2404A-CAN2404A-PRO2404B-PRO2404                  
         SPACE 1                                                                
         CLI   TACOCTYP,CCTY04A    IF ACTRA TYPE IS 2404A                       
         BNE   *+8                                                              
         OI    STATUS3,US2404A     SET TO PROCESS US CAST FIRST                
         SPACE 1                                                                
         CLI   TACOCTYP,CCTY2404   IF ACTRA TYPE IS 2404                        
         BNE   *+8                 SET TO PROCESS US CAST FIRST                
         OI    STATUS3,US2404A+PRO2404                                          
         SPACE 1                                                                
         CLI   TACOCTYP,CCTY04B    IF ACTRA TYPE IS 2404B                       
         BNE   *+8                                                              
         OI    STATUS3,PRO2404B    SET TO PROCESS 2404B COMMERCIAL              
         DROP  R4                                                               
         SPACE 1                                                                
         XC    ELTALF,ELTALF                                                    
         XC    ELTALF2,ELTALF2                                                  
         MVI   TGVER,0                                                          
         SPACE 1                                                                
         MVI   ELCODE,TALFELQ      LOOK FOR LIFT DETAILS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ELTALF,0(R4)        SAVE ENTIRE ELEMENT                          
         SPACE 1                                                                
         MVI   ELCODE,TAL2ELQ      LOOK FOR LIFT DETAILS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ELTALF2,0(R4)       SAVE ENTIRE ELEMENT                          
         SPACE 1                                                                
         BRAS  RE,COMADVER         LOOK FOR VERSIONS                            
         SPACE 1                                                                
         XC    CSDATES,CSDATES                                                  
         MVI   ELCODE,TACSELQ      LOOK UP STUDIO ELEMENTS                      
         USING TACSD,R4                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPR))                                     
         BNE   *+14                                                             
         L     R4,TGELEM                                                        
         MVC   RECDATE,TACSDATE    SAVE RECORD DATE                             
         GOTO1 (RF),(R1),(1,=AL1(TACSTYPM))                                     
         BNE   *+14                                                             
         L     R4,TGELEM                                                        
         MVC   MUSDATE,TACSDATE    AND MUSIC DATE                               
         GOTO1 (RF),(R1),(1,=AL1(TACSTYPF))                                     
         BNE   *+14                                                             
         L     R4,TGELEM                                                        
         MVC   FLMDATE,TACSDATE    AND FILM DATE                                
         SPACE 1                                                                
         GOTOR TRN2TACC,DMCB,AIO,AIO                                            
         SPACE 1                                                                
         USING TACCD,R4            LOOK FOR COMMERCIAL CONTRACT EL.             
         MVC   CONTRCTS,SPACES                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TACCELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COMADX                                                           
         ZIC   R1,TACCNCON         R1=N'CONTRACT NUMBERS IN EL.                 
         MH    R1,=AL2(L'TACCCON)                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONTRCTS(0),TACCCON  SAVE MUSIC CONTRACT NUMBERS                 
         SPACE 1                                                                
COMADX   BRAS  RE,SETCOML          SET COMMERCIAL DETAILS                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS ADDITIONAL HYPOTHETICAL COMMERCIAL INFO.            
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
HCOMADDL NTR1                                                                   
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         XC    ELTACO,ELTACO                                                    
         MVC   TACOCID,SPACES                SET COMMERCIAL ID                  
         MVC   TACOCID(L'TAESHCOM),TAESHCOM                                     
         CLI   TAESHCOM+9,0                                                     
         BNE   HCOMAD0                                                          
         GOTO1 =A(CHPACK),DMCB,(C'U',TAESHCOM),TACOCID,RR=RELO                  
HCOMAD0  MVC   TACOSEC,TAESHLEN              COMMERCIAL LENGTH                  
         GOTO1 MEDVAL,DMCB,(X'80',TAESHMED)  LOOK UP MEDIA AND                  
         MVC   TACOMED,TGMENAME              MOVE TO ELEMENT                    
         MVC   TACOFCYC,TGTODAY1   ASSUME FIRST FIXED CYCLE IS TODAY            
         MVI   TACOAFM,C'5'        DEFAULT AFM 5 AND OVER RATE                  
         MVI   TACOCTYP,CCTYNATL   DEFAULT TO NATIONAL ACTRA TYPE               
         SPACE 1                                                                
         MVI   CANDOLS,C'X'        FIGURE OUT TNH BY UNION                      
         SPACE 1                                                                
         NI    STATUS3,X'FF'-US2404A-CAN2404A-PRO2404B-PRO2404                  
         SPACE 1                                                                
         CLI   TAESSBNO,0          IF COMMERCIAL HAS SUB-ELEMENTS ...           
         BE    HCOMAD1B                                                         
         SPACE 1                                                                
         CLI   TAESSBCD,OPATYP     HAS TO BE COMML ACTRA TYPE SUB ELEM          
         BNE   HCOMAD1B                                                         
         CLI   TAESSBDT,CCTY04A    ... AND ACTRA TYPE IS SET AS 2404A           
         BNE   HCOMAD1                                                          
         MVI   TACOCTYP,CCTY04A    SET ACTRA TYPE AS 2404A                      
         OI    STATUS3,US2404A     AND SET TO PROCESS US CAST FIRST            
         MVI   CANDOLS,C'N'                                                     
         SPACE 1                                                                
HCOMAD1  CLI   TAESSBDT,CCTY2404   ... AND ACTRA TYPE IS SET AS 2404            
         BNE   HCOMAD1A                                                         
         MVI   TACOCTYP,CCTY2404   SET ACTRA TYPE AS 2404                       
         OI    STATUS3,US2404A+PRO2404                                          
         MVI   CANDOLS,C'N'                                                     
         SPACE 1                                                                
HCOMAD1A CLI   TAESSBDT,CCTY04B    ... AND ACTRA TYPE IS SET AS 2404B           
         BNE   HCOMAD1B                                                         
         OI    STATUS3,PRO2404B    SET TO PROCESS 2404B COMMERCIAL              
         SPACE 1                                                                
*                                  NOP AS OF 8/12/92 AS PER TF                  
* NO-OP  GOTO1 EXPIRE,DMCB,(R3),0,TACOEXP  CALC. EXPIRATION DATE                
         SPACE 1                                                                
HCOMAD1B TM    TGMEEQU,RADIO       IF MEDIA IS RADIO                            
         BZ    HCOMAD6                                                          
         XR    R0,R0                                                            
         MVI   ELCODE,TAESELQ                                                   
HCOMAD2  BAS   RE,NEXTEL           SCAN THROUGH HYPO PERFORMER ELS.             
         BNE   HCOMAD4                                                          
         CLI   TAESTYPE,TAESTHPE                                                
         BNE   HCOMAD4                                                          
         GOTO1 CATVAL,DMCB,(X'80',TAESHCAT)  VALIDATE CATEGORY CODE             
         SPACE 1                                                                
*        TM    TGCAUNI,AFM         IGNORE MUSICIANS                             
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BO    HCOMAD2                                                          
         CLI   TAESHCAT,CTANN      IF THIS IS AN ANNOUNCER                      
         BNE   HCOMAD6                                                          
         CLI   TAESHNPR,1          AND THERE'S ONLY ONE                         
         BH    HCOMAD6                                                          
         LTR   R0,R0               AND HAVEN'T SEEN ANY OTHER ANN'S             
         BNZ   HCOMAD6                                                          
         LA    R0,1                THEN SET WE'VE SEEN ONE ANNOUNCER            
         B     HCOMAD2             AND KEEP ON LOOKING                          
         SPACE 1                                                                
HCOMAD4  LTR   R0,R0               DID WE HAVE AN ANNOUNCER ALONE               
         BZ    *+8                                                              
         OI    TACOSTA2,TACOSANO   YES - SET STATUS BIT IN COMML EL.            
         SPACE 1                                                                
HCOMAD6  MVC   COMNAME,SPACES      CLEAR NAME                                   
         XC    ELTALF,ELTALF             LIFT DETAILS                           
         XC    ELTALF2,ELTALF2           LIFT DETAILS                           
         MVI   TGVER,0                   VERSION                                
         MVC   CONTRCTS,SPACES           CONTRACT NUMBERS                       
         XC    CSDATES,CSDATES           STUDIO DATES                           
         SPACE 1                                                                
         BRAS  RE,SETCOML          SET COMMERCIAL DETAILS                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE ADDS HYPO COMMLS TO TABLE & SETS PREV. SEEN BIT          
ADDHCOM  NTR1                                                                   
         XC    AHCOMENT,AHCOMENT   CLEAR A(CURRENT ENTRY IN HCOMTAB)            
         L     R1,HCOMTAB          R1=A(HYPOTHETICAL COMMLS TABLE)              
         L     R0,MAXHCOM          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING HCOMD,R1                                                         
         LA    RE,1                RE=RELATIVE COMML NUMBER IN TABLE            
         SPACE 1                                                                
AHCOM10  ST    RE,TGCOM            SET INTERNAL COMMERCIAL NUMBER               
         OC    0(HCOMLNQ,R1),0(R1) TEST LOGICAL END OF TABLE                    
         BZ    AHCOM20                                                          
         CLC   HCOMCLI,TGCLI       LOOKING FOR SAME CLIENT                      
         BNE   AHCOM14                                                          
         CLC   HCOMPRD,TGPRD                        PRODUCT                     
         BNE   AHCOM14                                                          
         CLC   HCOMCID,TGCID                        COMMERCIAL ID               
         BE    AHCOM30                                                          
         SPACE 1                                                                
AHCOM14  LA    R1,HCOMNEXT         BUMP TO NEXT SPOT                            
         LA    RE,1(RE)            ADD TO COUNT                                 
         BCT   R0,AHCOM10          AND TRY AGAIN                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   NOTNOW              GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE - INCREASE NHCOMS!                  
         SPACE 1                                                                
AHCOM20  MVC   HCOMCLI,TGCLI       SAVE THIS CLIENT                             
         MVC   HCOMPRD,TGPRD                 PRODUCT                            
         MVC   HCOMCID,TGCID                 COMMERCIAL ID                      
         ST    R1,AHCOMENT                   A(CURRENT ENTRY)                   
         NI    STATUS,ALL-SEENCOML SET HAVEN'T SEEN IT BEFORE                   
         B     NO                  RETURN CC NE                                 
         SPACE 1                                                                
AHCOM30  OI    STATUS,SEENCOML     SET WE'VE SEEN IT BEFORE                     
         B     YES                 RETURN CC EQ                                 
         EJECT                                                                  
*              ROUTINE EXTRACTS OPTIONS RELEVENT TO ACTUAL PAYMENTS             
         SPACE 1                                                                
ACTLOPTS NTR1                                                                   
         MVC   OVERIDES(DEFLNQ),DEFAULTS  INIT. DEFAULT RATES                   
         XC    OTHERS(OTHLNQ),OTHERS      CLEAR OTHER OPTIONS                   
         XC    USEOPTS,USEOPTS     CLEAR ACTUAL USAGE FILTER AREA               
         SPACE 1                                                                
         ZIC   R0,COMOPTS          RECOGNIZE COMMERCIAL LEVEL OR HIGHER         
         LTR   R0,R0               ANYTHING TO DO                               
         BZ    AOPTX                                                            
         XR    R2,R2               R2=A(SAVED USE TYPE EQUATES)                 
         LA    R3,USEOPTS          R3=A(USE TYPES TO FILTER)                    
         LA    R4,COMOPTS+1        R4=A(1ST OPTION)                             
         USING TAESSBCD,R4                                                      
         SPACE 1                                                                
AOPT1    MVC   BYTE,TAESSBCD       SET OPTION CODE                              
         BAS   RE,LOOKUP           LOOK UP OPTION IN TABLE                      
         USING OPTD,R1             RETURNS R1=A(TABLE ENTRY)                    
         SPACE 1                                                                
         TM    OPTSTAT,OPOKACTL    IGNORE IF NOT ACTUAL-RELATED                 
         BZ    AOPT9                                                            
         SPACE 1                                                                
         CLI   OPTCODE,OPUSE       IF THIS IS A USE TYPE                        
         BNE   AOPT2                                                            
         OC    0(2,R3),0(R3)       THEN IF CURRENT USEOPTS ENTRY USED           
         BZ    *+12                                                             
         LA    R3,5(R3)            BUMP TO NEXT SPOT IN USEOPTS                 
         B     *+10                                                             
         OC    0(5,R3),0(R3)       ELSE INSURE ENTIRE ENTRY IS CLEAR            
         LA    R2,TAESSBDT         SAVE A(USE TYPE EQUATES IN EL.)              
         B     AOPT9                                                            
         SPACE 1                                                                
AOPT2    CLI   OPTCODE,OPASOF      IF THIS IS AS/OF DATE OPTION                 
         BNE   *+10                                                             
         XR    R2,R2               CLEAR A(USE TYPE EQUATES)                    
         B     AOPT9                                                            
         SPACE 1                                                                
         CLI   OPTCODE,OPCYC       IF THIS IS CYCLE DATES OPTION                
         BE    *+12                                                             
         CLI   OPTCODE,OPDATE      OR DATE OPTION                               
         BNE   AOPT7                                                            
         LTR   R2,R2               AND WE HAVE A(USE TYPE)                      
         BZ    AOPT9                                                            
         MVC   2(3,R3),TAESSBDT    SAVE START DATE IN USEOPTS                   
         B     AOPT9                                                            
         SPACE 1                                                                
AOPT7    CLI   OPTCODE,OPNO        IF THIS IS EXCLUDE USE OPTION                
         BNE   AOPT8                                                            
         LTR   R2,R2               AND WE HAVE A(USE TYPE)                      
         BZ    AOPT9                                                            
         MVC   0(2,R3),0(R2)       SAVE USE TYPE EQUATES IN TABLE               
         B     AOPT9                                                            
         SPACE 1                                                                
AOPT8    XR    RF,RF               AT THIS POINT SIMPLY EXECUTE I/P RTN         
         ICM   RF,3,OPTDISP                                                     
         BNZ   *+6                                                              
         DC    H'0'                MISSING ROUTINE DISPLACEMENT                 
         AR    RF,RB                                                            
         LA    RE,AOPT9            SET RETURN ADDRESS FOR COMMON NTR1           
         NTR1                                                                   
         BR    RF                  GO OFF TO ROUTINE                            
         SPACE 1                                                                
AOPT9    ZIC   RE,OPTSUBLN         RE=DISP TO NEXT SUB-EL                       
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,TAESSBDT         LENGTH IS IN 1ST BYTE OF DATA                
         LA    R4,1(RE,R4)                                                      
         BCT   R0,AOPT1                                                         
         SPACE 1                                                                
AOPTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GENERATES ACTUAL PAYMENTS FROM INVOICE RECORDS           
         SPACE 1                                                                
ACTUALS  NTR1                                                                   
         MVI   ESTMODE,PROCPAY     SET ACTUAL PAYMENT MODE                      
         L     RE,CLATAB           CLEAR CLASS A TABLE                          
         L     RF,MAXCLA                                                        
         MHI   RF,CLALNQ           MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         L     RE,PAXTAB           CLEAR PAX TABLE                              
         L     RF,MAXCLA                                                        
         MHI   RF,CLALNQ           MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         L     RE,WSPTAB           CLEAR USAGE HISTORY TABLE                    
         L     RF,MAXWSP                                                        
         MHI   RF,3                MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         SPACE 1                                                                
         L     R2,=A(SYSIOD)       R2=A(SYSIO WORKING STORAGE)                  
         A     R2,RELO                                                          
         USING TASYSIOD,R2                                                      
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         LA    R1,IOHOOK           A(I/O HOOK)                                  
         ST    R1,TIHOOK                                                        
         L     R3,ATWA             R3=A(TWA)                                    
         USING T702FFD,R3                                                       
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTHORIZATION                                
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIQSTAFF,TGCTSTAF   SET STAFF ID                                 
         MVI   TIREAD,TLINHCDQ     READ INVOICE HISTORY RECORDS                 
         MVI   TISUBRD,TLCKCDQ     SUBREAD CHECK RECORDS                        
         MVC   TIFCOM,TGCOM        SET INTERNAL COMMERCIAL NUMBER               
         MVI   TIFINS2N,TAINSRTH   IGNORE RETRO HOLD INVOICES                   
         OI    TIQFLAG2,TIQFSUB    PASS SUBSIDIARY INVOICE RECORDS              
         OI    TIQFLAG2,TIQFNLIM   DO NOT CHECK LIMITS                          
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  LET SYSIO HANDLE I/O                      
         SPACE 1                                                                
         MVC   CANDOLS,SVCANDOL    RESTORE COMM'L CAN$ STATUS                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE HANDLES I/O HOOK FROM SYSIO                              
         SPACE 1                                                                
         USING TASYSIOD,R2         R2=A(SYSIO WORKING STORAGE)                  
IOHOOK   NTR1                                                                   
         L     R4,TIAREC           R4=A(RECORD)                                 
         ST    R4,AIO                                                           
         CLI   TIMODE,PROCREC      IF THIS IS CHECK RECORD                      
         BNE   IOH6                                                             
**NO-OP**GOTO1 =A(MYTRACE),DMCB,=C'CHK',RR=RELO                                 
         USING TLCKD,R4                                                         
         MVC   TGCSORT,TLCKSORT    EXTRACT CAST SORT SEQ. NUMBER                
         MVC   TGSSN,TISSN                 SSN                                  
         GOTO1 CATVAL,DMCB,TICAT           CATEGORY                             
         GOTO1 UNIVAL,DMCB,TIUN            UNION                                
         MVC   TGLCL,TILOCL                LOCAL                                
         MVC   TCCAONOF,TIONOF             CAMERA                               
         BAS   RE,GETTACA          GET TACAD DETAILS                            
         BNE   XIT                                                              
         B     IOH20                                                            
IOH6     CLI   TIMODE,PROCINV      ELSE IF THIS IS INVOICE RECORD               
         BNE   IOHX                                                             
         USING TLIND,R4                                                         
         CLI   TLINCD,TLINCDQ      INSURE THIS NOT A COMMENT RECORD             
         BNE   IOH22                                                            
**NO-OP**GOTO1 =A(MYTRACE),DMCB,=C'INV',RR=RELO                                 
         MVI   FRSTIME,0           SET FIRST TIME IND. FOR MERGECLA/PAX         
         XC    EXCHANGE,EXCHANGE                                                
         SPACE 1                                                                
         GOTO1 =A(FILTACT),DMCB,(RC),RR=RELO  DO SOME FILTERING                 
         BNE   IOH22                                                            
         SPACE 1                                                                
         XC    TGUSDATA,TGUSDATA                                                
         GOTO1 USEVAL,DMCB,TIUSE,TIUTYPE  LOOK UP USE TYPE                      
         SPACE 1                                                                
         LA    RF,USEOPTS          RF=A(TABLE OF FILTERABLE USE TYPES)          
IOH14    CLI   0(RF),0                                                          
         BE    IOH16                                                            
         CLC   TGUSEQU,0(RF)       IF MATCH ON USE CODE EQUATE                  
         BNE   IOH15                                                            
         CLI   TGUSEQU,ULFT                                                     
         BE    IOH14A                                                           
         CLI   1(RF),0             AND NO TYPE IN FILTER                        
         BE    IOH14A                                                           
         CLC   TGUSTYP,1(RF)       OR IF MATCH ON USE TYPE EQUATE               
         BNE   IOH15                                                            
IOH14A   OC    2(3,RF),2(RF)       THEN IF DATE FILTER NOT SPECIFIED            
         BZ    IOH22                    THEN BYPASS THIS PAYMENT                
         CLC   TICSDATE,2(RF)      ELSE IF MATCH ON CYCLE START                 
         BE    IOH22                    THEN BYPASS THIS PAYMENT                
IOH15    LA    RF,5(RF)                                                         
         B     IOH14               ELSE TRY NEXT TABLE ENTRY                    
         SPACE 1                                                                
IOH16    MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         SPACE 1                                                                
         MVC   CANDOLS,SVCANDOL    DEFAULT TO COMM'L CAN$ STATUS                
         TM    TAPDSTAT,TAPDSCAN   IF CAN$ STATUS IS ON THE INVOICE             
         BZ    *+8                                                              
         MVI   CANDOLS,C'Y'        OVERRIDE THE COMMERCIAL SETTING              
         SPACE 1                                                                
         MVC   INVESTPD,TAPDESPD   SAVE ESTIMATE PERIOD FROM INVOICE            
         LH    R1,TAPDSTUS         STARTING USE NUMBER                          
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                LESS ONE                                     
         STH   R1,INVNUSES         IS N'USES PREV. PAID                         
         MVC   INVTUSES,TAPDUSES   N'USES                                       
         XC    ACLAENT,ACLAENT     CLEAR A(THIS CLASS A ENTRY)                  
         BAS   RE,GETCSF           GET CSFAMT                                   
         XC    TAXAMTS,TAXAMTS     INITIALIZE T&H AMOUNTS                       
         OC    TIBIDATE,TIBIDATE   IF INVOICE NOT BILLED YET                    
         BZ    IOHX                SKIP INV - WILL CALC T&H FROM CHECKS         
         SPACE 1                                                                
         MVI   ELCODE,TABDELQ                                                   
         CLC   TGAGY,=CL6'8800'                                                 
         BE    *+8                                                              
         CLI   TGOFF,C'O'                                                       
         BE    *+8                                                              
         CLI   TGAGY,C'Q'                                                       
         BNE   *+8                                                              
         MVI   ELCODE,TABDELQ2                                                  
         L     R4,AIO                                                           
         BAS   RE,GETEL            GET BILLING DETAILS ELEMENT                  
         BNE   IOH18                                                            
         USING TABDD,R4                                                         
*                                                                               
         L     RE,TABDCSF          IF COMMERCIAL SERVICE FEE WAS                
         LTR   RE,RE               CALCULATED                                   
         BZ    *+8                                                              
         OI    SFCMSTAT,SFCMCSF    SET CSF CALCULATED                           
*                                                                               
         CLI   TABDLEN,TABDLN2Q    OLD STYLE ELEMENT WON'T HAVE COMM            
         BL    IOH16A                                                           
*                                                                               
         L     RE,TABDACOM         IF AGENCY COMMISSION WAS                     
         LTR   RE,RE               CALCULATED                                   
         BZ    *+8                                                              
         OI    SFCMSTAT,SFCMCOMM   SET COMMISSION CALCULATED                    
*                                                                               
         L     RF,TABDSIGN         IF SIGNATORY FEE WAS                         
         LTR   RF,RF               CALCULATED                                   
         BZ    *+8                                                              
         OI    SFCMSTAT,SFCMSFEE   SET SIGNATORY FEE CALCULATED                 
*                                                                               
         AR    RE,RF               ADD AGENCY COMMISSION AND                    
         ST    RE,COMMAMT          SIGNATORY FEE AND SAVE                       
*                                                                               
IOH16A   OC    TABDCCVT,TABDCCVT   SET CONVERSION RATE FOR THIS                 
         BZ    IOH16C              INVOICE FROM HISTORY                         
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         ST    R1,EXCHANGE                                                      
*                                                                               
IOH16C   L     R1,TABDTAX          R1 = PAYROLL TAXES                           
         A     R1,TABDFICR                                                      
         A     R1,TABDGST                                                       
         A     R1,TABDPST                                                       
         L     R0,TABDHND          R0 = INDIV. HANDLING                         
         L     RF,TABDHNDC         RF = CORP. HANDLING                          
*                                                                               
         CLI   PROFBTYP,TABRTYE    IF EMS                                       
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY23   OR TYPE 23                                   
         BNE   IOH17                                                            
         AR    RF,R0                                                            
         ST    R1,TAXAMT                                                        
         ST    RF,HNDAMT                                                        
         B     IOH18                                                            
*                                                                               
IOH17    CLI   PROFBTYP,TABRTY12   IF TYPE 12                                   
         BE    IOH17A                                                           
         CLI   PROFBTYP,TABRTY22   OR TYPE 22                                   
         BE    IOH17A                                                           
         CLI   PROFBTYP,TABRTY24   OR TYPE 24                                   
*        BE    IOH17A                                                           
*        CLI   PROFBTYP,TABRTY25   OR TYPE 25                                   
*        BE    IOH17A                                                           
*        CLI   PROFBTYP,TABRTY26   OR TYPE 26                                   
         BNE   IOH17B                                                           
IOH17A   AR    RF,R0               ADD BOTH HANDLING CHARGES TOGETHER           
         B     *+6                                                              
IOH17B   AR    R1,R0               ELSE ADD INDIV. HANDLING TO TAXES            
         ST    R1,TAXAMT           SAVE PAYROLL TAXES AMOUNT                    
         ST    RF,HNDAMT           SAVE HANDLING AMOUNT                         
         SPACE 1                                                                
IOH18    CLI   FORMAT,TAEPFQTR     IF QUARTERLY FORMAT BY CAST                  
         BE    IOHX                   DON'T PROCESS INVOICE REC.                
         CLI   FORMAT,TAEPFCAT     OR IF FORMAT BY CATEGORY                     
         BE    IOHX                   DON'T PROCESS INVOICE REC.                
         CLI   RECAPOPT,TAEPRWC    OR IF RECAP BY WORK-CODE                     
         BE    IOHX                   DON'T PROCESS INVOICE REC.                
         CLI   PROFSUPP,C'Y'       OR IF SUPPRESSING INACTIVE COMMLS            
         BE    IOHX                   DON'T PROCESS INVOICE REC.                
         CLI   ACTLOPT,C'P'        OR IF REQUESTING CHECKS                      
         BE    IOHX                   DON'T PROCESS INVOICE REC.                
         SPACE 1                                                                
IOH20    BAS   RE,GETDTLS          EXTRACT PAYMENT DETAILS                      
         CLI   TIMODE,PROCREC      IF PROCESSING CHECK RECORD                   
         BNE   IOH21                                                            
         GOTO1 =A(TNHCALC),DMCB,RR=RELO CALC TAX & HANDL. IF NEC.               
*                                                                               
IOH21    GOTO1 =A(CLCAMTS),DMCB,RR=RELO  SET COMM AMOUNTS                       
         GOTO1 =A(CNVAMTS),DMCB,TCTOTS,NTCTOTS,RR=RELO  CONVERT AMOUNTS         
         GOTO1 =A(CNVAMTS),DMCB,COMMAMT,4,RR=RELO                               
         SPACE 1                                                                
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         BNE   *+16                                                             
         CLI   TIMODE,PROCREC      IF PROCESSING CHECK RECORD                   
         BNE   *+8                                                              
         BAS   RE,ADDCAST          ADD TO CAST TABLE                            
         XC    TAXAMTS,TAXAMTS     CLEAR T&H AMOUNTS AFTER FIRST CHECK          
         XC    CSFAMT,CSFAMT       & CONTRACT SERVICE FEE                       
         CLI   TIMODE,PROCINV      IF PROCESSING INVOICE RECORD                 
         BNE   IOHX                                                             
         SPACE 1                                                                
IOH22    MVI   TIMODE,PROCNOCK     SET TO NOT PASS CHECK RECORDS                
         SPACE 1                                                                
IOHX     MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         TM    STATUS3,RERDKEY     IF NEED TO RE-READ KEY                       
         BZ    XIT                                                              
         MVC   KEY,TIKEY           RESTORE SYSIO'S READ SEQUENCE                
         GOTO1 HIGH                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET SOME CAST DETAIL INFO                             
*              IF CSF CHECK AND CANADIAN $ PAYMENT - CHECK SKIPPED              
         SPACE 1                                                                
GETTACA  NTR1                                                                   
         MVI   TCCASTA2,0                                                       
         MVI   TCCASTA3,0                                                       
         MVI   CSFCHK,C'N'                                                      
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
*                                                                               
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         MVC   TCCASTA2,TACASTA2   STATUS BYTE 2 (CELEBRITY)                    
         MVC   TCCASTA3,TACASTA3   STATUS BYTE 3                                
         BAS   RE,CHKCSF           IF CSF CHECK                                 
         BNE   YES                                                              
         MVI   CSFCHK,C'Y'         SET FLAG                                     
         CLI   TICUR,C'C'          IF CANADIAN PAYMENT                          
         BE    NO                  SKIP                                         
         B     YES                 ELSE, PROCESS                                
         SPACE 2                                                                
*              SET CC EQUAL IF UNION ACTRA CSF CHECK                            
         SPACE 1                                                                
CHKCSF   NTR1                                                                   
         CLC   =C'ACT',TGUNI                                                    
         BNE   NO                                                               
         CLC   TGCSORT,HEXFFS                                                   
         BNE   NO                                                               
         CLC   TGSSN,=X'000001553' TOR                                          
         BE    YES                                                              
         CLC   TGSSN,=X'000004137' MON                                          
         BE    YES                                                              
         CLC   TGSSN,=X'000002533' VAN                                          
         BE    YES                                                              
         CLC   TGSSN,=X'000007108' NEWFOUNDLAND                                 
         BE    YES                                                              
         CLC   TGSSN,=X'000008213' EDMONTON                                     
         BE    YES                                                              
         CLC   TGSSN,=X'000115472' MARITIMES                                    
         BE    YES                                                              
         CLC   TGSSN,=X'000006777' WINNIPEG                                     
         BE    YES                                                              
         CLC   TGSSN,=X'000000202' CAL                                          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET CONTRACT SERVICE FEE AMOUNT                       
         SPACE 1                                                                
GETCSF   NTR1                                                                   
         XC    CSFAMT,CSFAMT                                                    
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         USING TAPDD,R4                                                         
         BAS   RE,GETEL            GET PAYMENT DETAILS ELEMENT                  
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAPDOPT4,TAPDOCSF   GENERATED CONTRACT SERVICE FEE CHK           
         BO    XIT                 YES, DON'T NEED IT AGAIN                     
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TABDELQ                                                   
         CLC   TGAGY,=CL6'8800'                                                 
         BNE   *+8                                                              
         MVI   ELCODE,TABDELQ2                                                  
         USING TABDD,R4                                                         
         BAS   RE,GETEL            GET BILLING DETAILS ELEMENT                  
         BNE   XIT                                                              
         MVC   CSFAMT,TABDCSF      SET CONTRACT SERVICE FEE AMOUNT              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXTRACT ACTUAL PAYMENT DETAILS FOR REPORT             
         SPACE 1                                                                
         USING TASYSIOD,R2         R2=A(SYSIO WORKING STORAGE)                  
GETDTLS  NTR1                                                                   
         BAS   RE,ESTINIT          INITIALIZE PAYMENT-RELATED AREAS             
         SPACE 1                                                                
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAPDD,R4                                                         
         XC    TGUSDATA,TGUSDATA                                                
         GOTO1 USEVAL,DMCB,TAPDUSE,TAPDTYPE  SET GLOBAL USE TYPE VALUES         
         BE    GDTL1                                                            
         OI    TGUSXUNI,AFM        IF USE NOT DEFINED SET NOT MUSIC USE         
         OI    TGUSXUN1,AFM                                                     
         SPACE 1                                                                
GDTL1    TM    TGUSTYST,UPGRADE    IF THIS IS AN UPGRADE                        
         BZ    GDTL2                                                            
         GOTO1 UPGRVAL,DMCB,TGUSCDE,TGUSTYP  SET GLOBAL VALUES                  
         SPACE 1                                                                
GDTL2    CLI   TIMODE,PROCINV      IF READING CHECKS                            
         BE    GDTL2X                                                           
         CLC   TGCSORT,HEXFFS      AND THIS IS UNION CHECK                      
         BNE   GDTL2X                                                           
         OC    TCPCYC,TCPCYC       AND NO CYCLE DATES SET YET                   
         BNZ   GDTL2X                                                           
         OC    TAPDCYCS(L'TCPCYC),TAPDCYCS AND NONE ON UNION CHK                
         BNZ   GDTL2X                                                           
         LR    R0,R4                                                            
         L     R4,TIAMAIN          SET CYCLE DATE FROM INVOICE                  
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TCPCYC,TAPDCYCS-TAPDD(R4)                                        
         LR    R4,R0               RESET R4=A(TAPDD) ON CHECK                   
         MVC   TAPDCYCS(L'TCPCYC),TCPCYC                                        
*                                                                               
GDTL2X   MVC   TCPCYC,TAPDCYCS     CYCLE DATES                                  
         MVI   TCUSETAB+3,0                                                     
         TM    TAPDSTAT,TAPDSLFT   PAYMENT MADE TO LIFT                         
         BZ    *+8                                                              
         OI    TCUSETAB+3,TCLFTPRO                                              
         SPACE 1                                                                
         MVC   TCGUAR,TAPDGUAR     GUARANTEE CREDITS                            
         CLI   TAPDACDE,APPLOTH    IF APPLIED CODE IS NOT OTHER                 
         BE    *+16                                                             
         MVC   TCAPPLCD,TAPDACDE   APPLIED CODE                                 
         MVC   TCAPPLCR,TAPDAPPL   APPLIED CREDITS                              
         SPACE 1                                                                
         MVC   TCNUSES,INVNUSES    N'USES PREV. PAID                            
         MVC   TCTUSES,INVTUSES    N'USES                                       
         CLI   FORMAT,TAEPFW       IF FORMAT W                                  
         BNE   *+8                                                              
         BAS   RE,CHKWSPM          ADJUST TAPDUNIT & TAPDMAJ IF NECESS.         
         MVC   TCUNITS,TAPDUNIT    N'UNITS                                      
         MVC   TCMAJORS,TAPDMAJ    MAJORS                                       
         MVC   TCINSRTS,TAPDINS    INSERTS                                      
         MVC   TCTAGS,TAPDTAGS     TAGS                                         
         MVC   TCDEMO,TAPDDEMS     DEMOS                                        
         MVC   TCOV1,TAPDOV1       OVERSCALE RATE 1                             
         MVC   TCW4TYPE,TAPDW4TY   W4 TYPE                                      
         SPACE 1                                                                
         MVC   TCGROSS,TAPDGRS     GROSS                                        
         MVC   TCPAYI,TAPDPAYI     INDIVIDUAL PAYMENT AMOUNT                    
         CLI   TICUR,C'U'          IF US$ PAYMENT                               
         BNE   GDTL3                                                            
         CLI   CSFCHK,C'Y'         AND THIS IS CSF UNION CHECK                  
         BNE   GDTL3                                                            
         CLI   ACTLOPT,C'U'        AND ACTUALS REQ./UNBILLED EXCL.              
         BE    GDTL4               DON'T SET PAYMENT AMOUNT                     
         CLI   OFFLINE,C'Y'        OR RUNNING NOW                               
         BNE   GDTL4               DON'T SET PAYMENT AMOUNT                     
GDTL3    MVC   TCPAYC,TAPDPAYC     ELSE, CORPORATION PAYMENT AMOUNT             
         SPACE 1                                                                
GDTL4    MVC   TCEXP,TAPDREXP      REIMBURSED EXPENSES                          
         MVC   TCPNH,TAPDPNH       PENSION & HEALTH                             
         MVC   TCHNW,TAPDHNW       HEALTH & WELFARE                             
         OC    TCHNW,TCHNW         IF HNW PAID TO FUND                          
         BZ    *+8                                                              
         BAS   RE,CHGHNW           SHOW IT SEPERATELY                           
         SPACE 1                                                                
         MVC   TCINR,TAPDINR       SHOW INR SEPERATELY                          
         SPACE 1                                                                
         XC    APPLYDTE,APPLYDTE                                                
         CLI   PROFFILT,TAEPFCYC   IF FILTER ACTUALS ON CYCLE START             
         BE    *+12                                                             
         CLI   PROFFILT,TAEPFCES                                                
         BNE   *+10                                                             
         MVC   APPLYDTE,TAPDCYCS   TAKE CYCLE START DATE                        
         SPACE 1                                                                
         OC    APPLYDTE,APPLYDTE   IF WE DON'T HAVE APPLY DATE YET              
         BNZ   *+10                                                             
         MVC   APPLYDTE,TIBIDATE   USE BILLED DATE                              
         OC    APPLYDTE,APPLYDTE   IF WE DON'T HAVE APPLY DATE YET              
         BNZ   *+10                                                             
         MVC   APPLYDTE,TIPADATE   USE PAYMENT DATE                             
         OC    TCPCYCS,TCPCYCS     IF WE DON'T HAVE CYCLE START DATE            
         BNZ   *+10                                                             
         MVC   TCPCYCS,APPLYDTE    USE APPLY DATE                               
         SPACE 1                                                                
         CLI   PROFFILT,TAEPFCES   IF GROUPING ACTUALS BY ESTIMATE PD           
         BE    *+12                                                             
         CLI   PROFFILT,TAEPFBES                                                
         BNE   GDTL10                                                           
         MVC   APPLYMTH,APPLYDTE   DEFAULT TO SAME AS APPLY DATE                
         ZIC   R1,INVESTPD         NOW EXTRACT MONTH FROM ESTIMATE PD           
         LTR   R1,R1                                                            
         BZ    GDTL10                                                           
         TM    BRSTAT,TABRSQTR     IF BY QUARTER                                
         BZ    *+12                                                             
         MHI   R1,3                CONVERT TO 1ST MONTH IN QTR                  
         SH    R1,=H'2'                                                         
         CVD   R1,DUB                                                           
         MP    DUB,=P'10'                                                       
         MVC   APPLYMTH+1(1),DUB+6 SAVE PWOS MONTH FROM ESTIMATE PD             
         SPACE 1                                                                
GDTL10   L     R1,TCPAYC           SUBTRACT INR FROM PAYC                       
         S     R1,TCINR                                                         
         CLI   TIMODE,PROCINV      IF PROCESSING INVOICE                        
         BNE   GDTL20                                                           
         TM    TGUSSTAT,SESSION    RESIDUALS HAVE COMML SERV FEE                
         BNO   GDTL20                                                           
         CLI   TICUR,C'C'          CANADIAN HAS CONTRACT SERVICE FEE            
         BNE   GDTL20              DON'T SUBTRACT IF COMML SERV FEE             
         S     R1,CSFAMT           ALSO SUBTRACT CSF                            
GDTL20   ST    R1,TCPAYC                                                        
*                                                                               
         MVI   ELCODE,TAVRELQ                                                   
         MVI   BYTE,3                                                           
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BNE   XIT                                                              
         OI    TCUSETAB+3,TCLFTPR2                                              
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
CHKWSPM  NTR1                                                                   
         CLI   TGUSEQU,UWSP        IF WILDSPOT                                  
         BE    CHKWSPM1                                                         
         CLI   TGUSEQU,UWSC                                                     
         BE    CHKWSPM1                                                         
         CLI   TGUSEQU,UWSM                                                     
         BE    CHKWSPM1                                                         
         CLI   TGUSEQU,UADW                                                     
         BNE   XIT                                                              
*                                                                               
CHKWSPM1 L     R1,WSPTAB           R1=A(WILDSPOT TABLE)                         
         L     R0,MAXWSP           R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         SPACE 1                                                                
CHKWSPM3 OC    0(3,R1),0(R1)       TEST LOGICAL END OF TABLE                    
         BZ    CHKWSPM8                                                         
         CLC   TAPDCYCS,0(R1)      TEST CYCLE ALREADY IN TABLE                  
         BE    CHKWSPM5                                                         
         LA    R1,3(R1)            BUMP TO NEXT ENTRY                           
         BCT   R0,CHKWSPM3                                                      
         DC    H'0'                DIE - THIS FORMAT NOT FOR ONLINE             
         SPACE 1                                                                
CHKWSPM5 XC    TAPDUNIT,TAPDUNIT   IN TABLE - CLEAR UNITS & MAJORS              
         MVI   TAPDMAJ,0           (ALREADY ACCOUNTED FOR)                      
*                                                                               
CHKWSPM8 MVC   0(3,R1),TAPDCYCS    NOT IN TABLE - SAVE CYCLE                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE ADDS HNW TO APPROPRIATE PAY AMOUNT                       
         SPACE 1                                                                
CHGHNW   NTR1                                                                   
         TM    TAPDINV+5,X'0F'     IF OLD SYSTEM INVOICES                       
         BZ    *+12                                                             
         CLI   TAPDINV+5,3                                                      
         BE    XIT                 EXIT                                         
*                                                                               
         CLI   TCW4TYPE,TAW4TYCO   OR IF THIS IS A CORPORATION                  
         BE    CHGH2                                                            
         CLI   TCW4TYPE,TAW4TYCA   OR A CANADIAN                                
         BE    CHGH2                                                            
         CLI   TCW4TYPE,TAW4TYFO   OR A FOREIGNER                               
         BNE   CHGH5                                                            
CHGH2    L     R1,TCPAYC           SUBTRACT HNW FROM CORP PAYMENT               
         S     R1,TCHNW                                                         
         ST    R1,TCPAYC                                                        
         B     CHGH8                                                            
CHGH5    L     R1,TCPAYI                                                        
         S     R1,TCHNW            ELSE, SUBTRACT HNW FROM INDIVIDUAL           
         ST    R1,TCPAYI                                                        
CHGH8    ST    R1,TCPAY            SET PAYMENT AMOUNT OVERRIDE                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AN ACTUAL CAST MEMBER TO CAST TABLE               
         SPACE 1                                                                
ADDCAST  NTR1                                                                   
         L     R1,CASTTAB          R1=A(CAST TABLE)                             
         L     R0,MAXCAST          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         SPACE 1                                                                
ACST10   OC    0(2,R1),0(R1)       TEST LOGICAL END OF TABLE                    
         BZ    ACST20                                                           
         CLC   TGCSORT+4(2),0(R1)  TEST THIS CAST ALREADY IN TABLE              
         BE    ACSTX                                                            
         LA    R1,2(R1)            BUMP TO NEXT SPOT                            
         BCT   R0,ACST10           AND TRY AGAIN                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   NOTNOW              GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE  - INCREASE NCAST!                  
         SPACE 1                                                                
ACST20   MVC   0(2,R1),TGCSORT+4   SAVE THIS INTERNAL SEQUENCE NUMBER           
         SPACE 1                                                                
ACSTX    B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO CHECK IF CAST MEMBER HAD ACTUAL PAYMENTS              
         SPACE 1                                                                
CHKCAST  NTR1                                                                   
         L     R1,CASTTAB          R1=A(CAST TABLE)                             
         L     R0,MAXCAST          R0=MAXIUM NUMBER OF TABLE ENTRIES            
         SPACE 1                                                                
CCST10   OC    0(2,R1),0(R1)       TEST LOGICAL END OF TABLE                    
         BZ    CCSTX                                                            
         CLC   TGCSORT+4(2),0(R1)  TEST THIS CAST MEMBER IN TABLE               
         BE    CCST20                                                           
         LA    R1,2(R1)            NO - BUMP TO NEXT SPOT                       
         BCT   R0,CCST10           AND TRY AGAIN                                
         B     CCSTX                                                            
         SPACE 1                                                                
CCST20   OI    STATUS2,CASTACTV+COMLACTV  SET CAST MEMBER/COMML ACTIVE          
         SPACE 1                                                                
CCSTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ACTUAL CAST MEMBER (SAVE REC IN IO3)           
         SPACE 1                                                                
GETCAST  NTR1                                                                   
         LA    R3,KEY              R3=A(CAST KEY)                               
         USING TLCAD,R3                                                         
         TM    STATUS,FLUSHING     SKIP IF FLUSHING HOLD TABLE                  
         BO    GETC2                                                            
         OC    TLCAKEY,TLCAKEY     TEST THIS IS FIRST TIME FOR COMM'L           
         BNZ   GETC1                                                            
         MVI   TLCACD,TLCACDQ      YES - INITIALIZE KEY                         
         MVC   TLCACOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                READ HIGH FOR FIRST CAST MEMBER              
         B     GETC4                                                            
         SPACE 1                                                                
GETC1    CLI   TLCACD,TLCACDQ      IF LAST RECORD READ NOT CAST                 
         BE    GETC3                                                            
GETC2    MVC   KEY,CASTKEY         SET KEY OF LAST CAST RECORD READ             
         GOTO1 HIGH                AND RE-READ                                  
         TM    STATUS,FLUSHING     DONE IF FLUSHING HOLD TABLE                  
         BO    GETC4                                                            
GETC3    GOTO1 SEQ                 GET NEXT CAST MEMBER                         
         SPACE 1                                                                
GETC4    CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    GETC4A                                                           
         GOTO1 CATCHIOS            CHECK IO COUNT                               
GETC4A   CLC   TLCAKEY(TLCASORT-TLCAD),KEYSAVE  TEST STILL SAME COMM'L          
         BNE   NO                                                               
         MVC   CASTKEY,TLCAKEY     SAVE THIS KEY                                
         SPACE 1                                                                
         MVC   AIO,AIO3            USING I/O 3                                  
         GOTO1 GETREC              GET THE RECORD                               
         GOTO1 =A(MYTRACE),DMCB,=C'CAST',RR=RELO                                
         GOTO1 EXTRACT             EXTRACT GLOBAL KEY DETAILS                   
         CLI   ACTLOPT,C'X'        IF USER WANTS TO IGNORE ACTUAL USAGE         
         BNE   GETC5                                                            
         MVI   ELCODE,TACRELQ                                                   
         GOTO1 REMELEM             REMOVE APPLIED CREDIT HISTORY ELS.           
         SPACE 1                                                                
GETC5    MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         GOTO1 CATVAL,DMCB,TLCACAT  SET CATEGORY INFO                           
         BNE   GETC3                                                            
         SPACE 1                                                                
         TM    STATUS3,PRO2404                                                  
         BZ    GETC5A              IF PROCESSING 2404 CAST                      
         BRAS  RE,NONUNION         EXCLUDE NON-UNION PERFORMERS                 
         BE    GETC3                                                            
         SPACE 1                                                                
GETC5A   TM    STATUS3,US2404A     IF PROCESSING 2404A US$ CAST                 
         BZ    GETC5B                                                           
         BRAS  RE,CANCST           ONLY PROCESS US$ CAST AT THIS TIME           
         BE    GETC3                                                            
         SPACE 1                                                                
GETC5B   TM    STATUS3,CAN2404A    IF PROCESSING 2404A CAN$ CAST                
         BZ    GETC6                                                            
         BRAS  RE,CANCST           ONLY PROCESS CAN$ CAST AT THIS TIME          
         BNE   GETC3                                                            
         SPACE 1                                                                
GETC6    XC    TCCAST(TCCSTLNQ),TCCAST  INITIALIZE RATE CALC. FIELDS            
         XC    TCCSTBRK,TCCSTBRK                                                
         XC    MYCAENT(MYCALNQ),MYCAENT  MY CAST DETAILS                        
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         L     R4,AIO3                                                          
         ST    R4,TCACAST          SAVE A(CAST RECORD)                          
         BAS   RE,GETEL                                                         
         BNE   GETC3                                                            
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
GETC6A   MVC   TCCAONOF,TACAONOF        ON/OFF CAMERA                           
         MVC   TCCADBL,TACADBL          N'DOUBLES                               
         MVC   TCCASTA2,TACASTA2        2ND STATUS BYTE (CELEBRITY)             
         MVC   TCCASTA3,TACASTA3        3RD STATUS BYTE                         
**NO-OP**MVC   TCOV2,TACAOV2            2ND OVERSCALE RATE                      
         MVC   TCCASTAT,TACASTAT        STATUS BYTE                             
         MVC   TCCAFRST,TACAFRST        1ST SERVICES DATE                       
         MVC   TCCAFCYC,TACAFCYC        FIRST FIXED CYCLE DATE                  
         MVC   MYCALAST,TACALAST        LAST SERVICE DATE                       
         MVC   TGGUA,TACAGUA            GUARANTEE RECORD CODE                   
         MVC   TGLCL,TACALOCL           LOCAL                                   
         MVC   TCCAYEAR,TACAYEAR        UNION YEAR                              
         MVC   TCCASTA4,TACASTA4        4TH STATUS BYTE                         
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,TACAUN       UNION INFORMATION                       
         BNE   GETC3                                                            
         CLC   TACAUN,=C'SAG'           IF UNION IS SAG                         
         BE    GETC7                                                            
         CLC   TACAUN,=C'AFT'           OR AFT,                                 
         BNE   *+8                                                              
GETC7    OI    CSFSTAT,CSFUNI           SET CSF UNION                           
*                                                                               
         GOTO1 YRVAL,DMCB,TACAYEAR      YEAR                                    
         BNE   GETC3                                                            
         XC    TGTACODE,TGTACODE                                                
         GOTO1 TAXVAL,DMCB,(3,TACAUNIT) UNIT                                    
         BE    GETC8                                                            
         MVC   TGCTRY,=C'CA'       CANADIAN                                     
         GOTO1 TAXVAL,DMCB,(X'FF',TACAUNIT) UNIT                                
         BNE   GETC3                                                            
         MVC   TGTACODE,=C'CN '    NOT SET FOR CANADIAN PROVINCES               
*                                                                               
GETC8    BRAS  RE,GETCVER               GET VERSION INFORMATION                 
         SPACE 1                                                                
GETC200  GOTO1 =A(GETPSTR),DMCB,TACAUNIT,RR=RELO                                
         BAS   RE,W4DET            GET W4 DETAILS                               
*                                                                               
         L     RE,HISTTAB          CLEAR USAGE HISTORY TABLE                    
         L     RF,MAXHIST                                                       
         MHI   RF,HISTLNQ          MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         XC    HISTDLR,HISTDLR     CLEAR USAGE HIST ACTUAL FOR DLR              
         XC    LASTS,LASTS         CLEAR SELECTED LAST CYCLE ENDS               
         NI    STATUS,ALL-HAVESESS ASSUME WE DON'T HAVE SESSION                 
         SPACE 1                                                                
         CLI   ACTLOPT,C'X'        USER WANTS TO IGNORE ACTUAL USAGE            
         BE    GETCX                                                            
         TM    TGCATYPE,EXTRA      IF PERFORMER IS AN EXTRA                     
         BZ    *+8                                                              
         OI    STATUS,HAVESESS     THEN ASSUME SESSION'S BEEN PAID              
         SPACE 1                                                                
         L     R2,HISTTAB          R2=A(USAGE HISTORY TABLE)                    
         USING HISTD,R2                                                         
         MVI   ELCODE,TACRELQ      SCAN APPLIED CREDIT HISTORY ELS.             
         L     R4,TCACAST                                                       
         BAS   RE,GETEL                                                         
         BNE   GETC300                                                          
         OI    STATUS,HAVESESS     PERFORMER HAS BEEN PAID A SESSION            
         LR    R3,R4                                                            
         BAS   RE,NEXTEL           FIND LAST EL.                                
         BE    *-6                                                              
         LR    R4,R3               R4=A(LAST APPLIED CREDIT HIST EL.)           
         USING TACRD,R4                                                         
         MVC   LASTHLD,TACREND     SAVE LAST END DATE                           
         GOTO1 USEVAL,DMCB,(X'40',TACRUSE)                                      
         TM    TGUSSTA2,HLDTYPE    ONLY IF THIS IS HLD-TYPE USE                 
         BZ    GETC300                                                          
         MVI   HISTUSE,UHLD        ALWAYS SAVE AS HLD (CAN'T USE HLDUSE         
*                                  BECAUSE HAVEN'T CALLED PERADDL YET!)         
         MVC   HISTCYC,TACRSTRT    SAVE CYCLE DATES                             
         SPACE 1                                                                
*ETC300  TM    TGUNEQU,AFM         IF THIS IS A MUSICIAN                        
GETC300  GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    GETCX                                                            
         MVI   HALF,UMUS           SET TO LOOK UP LAST MUSIC PAYMENT            
         MVI   HALF+1,0                                                         
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1  SET MUS GLOBAL VALUES           
         XC    ELEMENT,ELEMENT                                                  
         LA    R2,ELEMENT          R2=A(TEMP AREA FOR HISTORY ENTRY)            
         USING HISTD,R2                                                         
         BAS   RE,BLDHIST                                                       
******   BE    GETC500                                                          
*DH ON   MVI   HALF,UFMU           DIDN'T FIND MUS - LOOK FOR FMU               
*11/19   GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1                                  
******   BAS   RE,BLDHIST                                                       
         BNE   *+8                                                              
         MVI   HISTUSE,UMUS        FOUND FMU - ALWAYS SAVE AS MUS               
GETC50   MVC   LASTMUS,HISTCYCE    SAVE END DATE FROM LAST MUSIC PMT            
GETCX    B     YES                 RETURN CC EQ - HAVE A CAST MEMBER            
         EJECT                                                                  
*              ROUTINE EXTRACTS DETAILS FROM W4 RECORD                          
         SPACE 1                                                                
*                                  R4=A(CAST DETAILS ELEMENT)                   
W4DET    NTR1                                                                   
         CLI   0(R4),TACAELQ                                                    
         BE    *+16                                                             
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         L     R4,TCACAST                                                       
         BAS   RE,GETEL                                                         
         LR    R3,R4               SAVE IT                                      
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',0)  GET W4 RECORD                     
         SPACE 1                                                                
         GOTO1 =A(MYTRACE),DMCB,=C'W4',RR=RELO                                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      LOOK FOR W4 DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         MVC   TCW4TYPE,TAW4TYPE   SAVE W4 TYPE                                 
         SPACE 1                                                                
         CLI   TACACORP-TACAD(R3),C' '  IF PAYING INDIV. AS CORP.               
         BNH   *+8                                                              
         MVI   TCW4TYPE,TAW4TYCO        SET CORPORATION TYPE                    
*                                                                               
         CLI   TCW4TYPE,TAW4TYCA   IF CANADIAN                                  
         BE    *+12                                                             
         CLI   TCW4TYPE,TAW4TYCO   OR CORPORATION                               
         BNE   XIT                                                              
         MVI   ELCODE,TANUELQ      CHECK FOR GST NUMBER                         
         GOTO1 GETL,DMCB,(1,=AL1(TANUTGST))                                     
         BNE   *+8                                                              
         OI    MYCASTAT,MYCASGST                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS ADDITIONAL PERFORMER INFORMATION                    
         SPACE 1                                                                
PERADDL  NTR1                                                                   
         MVC   OVERIDES(DEFLNQ),DEFAULTS INIT. DEFAULT RATES                    
         XC    OTHERS(OTHLNQ),OTHERS     CLEAR OTHER OPTIONS                    
         XC    USEOPTS,USEOPTS                 USES FOR THIS PERFORMER          
         SPACE 1                                                                
         L     R2,GUARTAB          CLEAR ANY HYPO SINGLE COMML GUARS            
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING GUARD,R2            R2=A(GUARANTEE TABLE)                        
PERA1    OC    GUARGUA,GUARGUA     INDICATED BY NULL GUARANTEE CODE             
         BNZ   *+10                                                             
         XC    GUARD(GUARLNQ),GUARD  CLEAR ENTRY                                
         LA    R2,GUARNEXT         BUMP TO NEXT ENTRY                           
         BCT   R0,PERA1                                                         
         SPACE 1                                                                
         L     RE,ASOFCB            CLEAR AS/OF DATE CONTROL BLOCK              
         L     RF,MAXASOF                                                       
         MHI   RF,ASOFLNQ                                                       
         XCEFL                                                                  
         XC    ASESSENT,ASESSENT         CLEAR A(SESSION ENTRY TO EST.)         
*                                        INIT EST. USAGE STATUS/CAST            
         NI    STATUS2,ALL-ESTGRT-ESTCBL-CASTACTV               ACTIVE          
         SPACE 1                                                                
         ZIC   R0,PEROPTS                                                       
         LTR   R0,R0               ANYTHING TO DO                               
         BZ    PERA8                                                            
         LA    R3,USEOPTS          R3=A(USE TYPE POINTERS)                      
         LA    R4,PEROPTS+1        R4=A(1ST OPTION)                             
         USING TAESSBCD,R4                                                      
PERA2    MVC   BYTE,TAESSBCD       SET OPTION CODE                              
         BAS   RE,LOOKUP           LOOK UP OPTION IN TABLE                      
         USING OPTD,R1             RETURNS R1=A(TABLE ENTRY)                    
         SPACE 1                                                                
         CLI   OPTCODE,OPUSE       CHECK FOR USE TYPES                          
         BNE   PERA3                                                            
         CLI   TAESSBDT,UGRT       IF WILL ESTIMATE GRT PAYMENT                 
         BNE   *+8                                                              
         OI    STATUS2,ESTGRT      SET THIS PERFORMER HAS GUARANTEE             
         CLI   TAESSBDT,UCBL       IF WILL ESTIMATE CBL PAYMENT                 
         BNE   *+8                                                              
         OI    STATUS2,ESTCBL      SET THIS PERFORMER HAS CBL                   
         LA    RE,PEROPTS                                                       
         SR    RE,R4                                                            
         LPR   RE,RE                                                            
         STH   RE,0(R3)            SAVE DISP. TO THIS USE TYPE OPTION           
         LR    R2,R1                                                            
         GOTO1 USEVAL,DMCB,(X'80',TAESSBDT),TAESSBDT+1  VAL. USE TYPE           
         LR    R1,R2                                                            
         TM    TGUSSTA2,APPREUSE   IF THIS USE CAN ADD FTRACKS                  
         BZ    PERA2D                                                           
         TM    TGUSSTA2,HLDTYPE    AND IT IS NOT A HOLDING FEE                  
         BO    PERA2D                                                           
         ST    R3,ASESSENT         SAVE ITS ADDRESS IN USEOPTS                  
         OI    STATUS,HAVESESS     AND SET SESSION HAS BEEN PAID                
PERA2D   LA    R3,2(R3)            BUMP TO NEXT ENTRY IN USEOPTS TABLE          
         LA    RF,USEOPTS+L'USEOPTS                                             
         CR    R3,RF                                                            
         BL    PERA4               GET NEXT OPTION                              
         DC    H'0'                NEED TO MAKE USEOPTS BIGGER                  
         SPACE 1                                                                
PERA3    TM    OPTSTAT,OPFORUSE+OPIGNORE IGNORE USE-RELATED FOR NOW             
         BNZ   PERA4                                                            
         XR    RF,RF                                                            
         ICM   RF,3,OPTDISP                                                     
         BNZ   *+6                                                              
         DC    H'0'                MISSING SET ROUTINE DISPLACEMENT             
         AR    RF,RB                                                            
         LA    RE,PERA4            SET RETURN ADDRESS FOR COMMON NTR1           
         NTR1                                                                   
         BR    RF                  GO OFF TO ROUTINE                            
         SPACE 1                                                                
PERA4    ZIC   RE,OPTSUBLN         RE=DISP TO NEXT SUB-EL                       
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,TAESSBDT         LENGTH IS IN 1ST BYTE OF DATA                
         LA    R4,1(RE,R4)                                                      
         BCT   R0,PERA2                                                         
         SPACE 1                                                                
PERA8    CLI   YEAR,0              IF YEAR OVERRIDE NOT DEFINED                 
         BNE   *+10                                                             
         MVC   YEAR,TGYREQU        SAVE DEFAULT YEAR IN OVERRIDE BYTE           
         GOTO1 YRVAL,DMCB,(X'80',YEAR)  SET CHAR FOR CAST REPORT                
         SPACE 1                                                                
         OC    LASTSRV,LASTSRV     LAST SERVICES DATE OVERRIDE                  
         BZ    *+10                                                             
         MVC   MYCALAST,LASTSRV                                                 
         SPACE 1                                                                
         MVC   TCOV1,OVERSCL       SET OVERALL OVSCL RATE FOR CAST RPT          
         MVC   TCOV2,OVERSCL2      SET OVERALL OVSCL2 RATE FOR CAST RPT         
         SPACE 1                                                                
         LA    R3,ELTALF           R3=A(LIFT DETAILS EL.)                       
         USING TALFD,R3                                                         
         CLI   LLIFT,0             L'LIFT OVERRIDE                              
         BE    *+14                                                             
         MVI   TALFEL,TALFELQ      INSURE LIFT DETAILS EL. INITIALIZED          
         MVC   TALFSEC,LLIFT       SAVE L'LIFT                                  
         SPACE 1                                                                
         LA    R3,ELTALF2          R3=A(2ND LIFT DETAILS EL.)                   
         CLI   LLIFT2,0            L'LIFT OVERRIDE                              
         BE    *+14                                                             
         MVI   TALFEL,TAL2ELQ      INSURE LIFT DETAILS EL. INITIALIZED          
         MVC   TALFSEC,LLIFT2      SAVE L'LIFT                                  
         SPACE 1                                                                
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         OC    EXPIRY,EXPIRY       EXPIRATION DATE OVERRIDE                     
         BZ    *+10                                                             
         MVC   TACOEXP,EXPIRY                                                   
         SPACE 1                                                                
         OC    FRSTDATE,FRSTDATE   IF FIRST FIXED CYCLE OVERRIDE                
         BZ    *+10                                                             
         MVC   TACOFCYC,FRSTDATE   USE IT                                       
         SPACE 1                                                                
         OC    ADSTATE,ADSTATE     ADDENDUM STATE                               
         BZ    *+14                                                             
         MVC   TACOADST,ADSTATE                                                 
         MVI   TACOTYPE,CTYADD     SET ADDENDUM COMML TYPE                      
         SPACE 1                                                                
         CLI   CTYP,0              COMMERCIAL TYPE OVERRIDE                     
         BE    *+10                                                             
         MVC   TACOTYPE,CTYP                                                    
         GOTO1 CTYPVAL,DMCB,TACOTYPE  VALIDATE COMMERCIAL TYPE                  
         SPACE 1                                                                
         TM    STATUS3,CAN2404A    IF PAYING CANADIANS ON A 2404A               
         BZ    *+8                 COMMERCIAL                                   
         OI    TGCATYPE,NOHLD      SET NOT ELIGIBLE FOR HOLDING FEES            
         SPACE 1                                                                
         MVI   BSSUSE,UBSS         SET AUTO USE TYPE CODES                      
         MVI   BSSUTYPE,0                                                       
         MVI   BSRUSE,UBSR                                                      
         MVI   BSRUTYPE,0                                                       
         MVI   HLDUSE,UHLD                                                      
         SPACE 1                                                                
         CLI   TGCTEQU,CTYSPAN     SPANISH COMMERCIALS ARE DIFFERENT            
         BNE   *+12                                                             
         MVI   BSSUSE,USSS                                                      
         MVI   HLDUSE,USHL                                                      
         SPACE 1                                                                
         CLI   TGCTEQU,CTYADD      ADDENDUM COMMERCIALS ARE DIFFERENT           
         BNE   PERA14                                                           
         MVI   BSSUSE,UADT                                                      
         MVI   BSSUTYPE,UADT13W                                                 
         MVI   BSRUSE,UADO                                                      
         MVI   BSRUTYPE,UADO13W                                                 
         MVI   HLDUSE,UADH                                                      
         SPACE 1                                                                
PERA14   L     R2,HISTTAB          IF WE HAVE USAGE HISTORY TABLE ENTRY         
         USING HISTD,R2                                                         
         CLI   HISTUSE,UHLD        FOR HLD ALREADY (FROM GETCAST)               
         BNE   *+10                                                             
         MVC   HISTUSE,HLDUSE      THEN RESET TO BE CORRECT TYPE                
         SPACE 1                                                                
         CLI   AFMRATE,0           AFM RATE OVERRIDE                            
         BE    *+10                                                             
         MVC   TACOAFM,AFMRATE                                                  
         SPACE 1                                                                
         OC    TCCASTAT,CASTSTAT   SET CAST STATUS OVERRIDES                    
         OC    TCCASTA4,CASTSTA2   SET CAST STATUS OVERRIDES                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS ADDITIONAL HYPOTHETICAL PERFORMER INFO.             
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
HPERADDL NTR1                                                                   
         XC    TGCSORT,TGCSORT     CLEAR CAST SORT KEY                          
         XC    TCCAFRST,TCCAFRST         1ST SERVICES DATE (SEE SETUSE)         
         XC    MYCAENT(MYCALNQ),MYCAENT  MY CAST DETAILS                        
**NO-OP**XC    TCOV2,TCOV2               2ND OVERSCALE RATE                     
         XC    TGSSN,TGSSN               SOCIAL SECURITY NUMBER                 
         XC    TGGUA,TGGUA               GUARANTEE RECORD CODE                  
         L     RE,HISTTAB                USAGE HISTORY TABLE                    
         L     RF,MAXHIST                                                       
         MHI   RF,HISTLNQ          MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         XC    HISTDLR,HISTDLR           USAGE HIST ACTUAL FOR DLR              
         XC    LASTS,LASTS               SELECTED LAST CYCLE ENDS               
         NI    STATUS,ALL-HAVESESS                                              
         SPACE 1                                                                
         MVC   TGNOPERF,TAESHNPR   NUMBER OF PERFORMERS                         
         MVC   TGLCL,HEXFFS                                                     
         CLI   PROFHNW,C'Y'        IF NOT INCLUDING HNW IN NET                  
         BE    *+10                                                             
         MVC   TGLCL,TGHWLCLS      SET LOCAL WITH A FUND                        
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,(X'80',TAESHCAT)  VALIDATE CATEGORY CODE             
         SPACE 1                                                                
         TM    TGCATYPE,EXTRA      SPECIAL SORT BIT FOR EXTRA                   
         BZ    *+8                                                              
         OI    TGCSORT,X'20'                                                    
         MVC   TGCSORT+2(1),TGCASORT  SET CATEGORY SORT NUMBER                  
         SPACE 1                                                                
         MVC   TCCADBL,TAESHDBL    SET N'DOUBLES                                
         SPACE 1                                                                
         MVC   TCCAONOF,=C'ON '                                                 
         TM    TAESHSTA,X'80'      SET CAMERA STATUS                            
         BO    *+14                                                             
         MVC   TCCAONOF,=C'OFF'                                                 
         OI    TGCSORT,X'08'                                                    
         SPACE 1                                                                
         MVI   TCCASTA2,0          NO CELEBRITIES                               
         MVI   TCCASTA3,0                                                       
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,(X'20',TAESHUNI)  VALIDATE UNION                     
         SPACE 1                                                                
         CLI   TGUNEQU,AFM         IF UNION IS AFM                              
         BNE   *+8                                                              
         OI    TGCSORT,X'88'       FORCE MUSICIAN/OFF CAMERA STATUS             
         SPACE 1                                                                
         LA    R1,TGUNYR+L'TGUNYR-1  R1=A(VALID YEARS FOR THIS UNION)           
         CLI   0(R1),0                                                          
         BNE   *+8                 SHUFFLE BACK TO FIND LATEST ONE              
         BCT   R1,*-8                                                           
         MVC   TGYREQU,0(R1)       SAVE CONTRACT YEAR                           
         SPACE 1                                                                
         MVI   TCCASTAT,0          INITIALIZE STATUS                            
         MVI   TCCASTA4,0          INITIALIZE STATUS                            
         CLI   TAESHLFT,C'O'       PERFORMER ONLY ON 1ST LIFT?                  
         BNE   *+12                                                             
         OI    TCCASTAT,TACASTLO                                                
         B     *+12                                                             
         CLI   TAESHLFT,C'Y'       PERFORMER ON MASTER + 1ST LIFT?              
         BNE   *+12                                                             
         OI    TCCASTAT,TACASTLF                                                
         B     *+16                                                             
         CLI   TAESHLFT,C'S'       PERFORMER ONLY ON 2ND LIFT?                  
         BNE   *+12                                                             
         OI    TCCASTA4,TACAST2O                                                
         B     *+16                                                             
         CLI   TAESHLFT,C'X'       PERFORMER ON MASTER + 2ND LIFT?              
         BNE   *+12                                                             
         OI    TCCASTA4,TACAS2LF                                                
         B     *+16                                                             
         CLI   TAESHLFT,C'A'       PERFORMER ON MASTER+1ST+2ND LIFTS?           
         BNE   *+12                                                             
         OI    TCCASTA4,TACASALL                                                
         B     *+16                                                             
         CLI   TAESHLFT,C'E'       PERFORMER ON 1ST + 2ND LIFT?                 
         BNE   *+8                                                              
         OI    TCCASTA4,TACASL2O                                                
         SPACE 1                                                                
         MVI   TCW4TYPE,TAW4TYIN   SET INDIVIDUAL TYPE                          
         TM    TAESHSTA,X'40'      IF CORP = Y                                  
         BZ    *+8                                                              
         MVI   TCW4TYPE,TAW4TYCO   SET THIS IS CORP                             
         L     R3,AIO3             SET UP DUMMY CAST RECORD                     
         USING TLCAD,R3            FOR CREDITTING PURPOSES                      
         XC    TLCAKEY(50),TLCAKEY                                              
         MVC   TLCALEN,DATADISP                                                 
         ST    R3,TCACAST          SAVE ITS ADDRESS FOR RATE CALC.              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS OPTION VALUES                                       
         SPACE 1                                                                
*                                  R3=A(OPTIONS AREA FOR THIS LEVEL)            
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
SETOPTS  NTR1                                                                   
         LR    RE,R3                                                            
         LA    RF,ENDOPTS          DETERMINE HOW MUCH TO CLEAR                  
         SR    RF,RE                                                            
         XCEFL ,                   CLEAR OPTIONS AREA(S)                        
         SPACE 1                                                                
         ZIC   R2,TAESSBNO         R2=N'SUB-ELEMENTS AT THIS LEVEL              
         LTR   R2,R2               ANYTHING TO SAVE                             
         BZ    SETO4                                                            
         ZIC   R1,TAESLEN          DETERMINE HOW MUCH                           
         SH    R1,=AL2((TAESSBNO-TAESD)+1)                                      
         BNM   *+6                                                              
         DC    H'0'                BAD ELEMENT                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAESSBNO    SAVE SUB-ELEMENT DATA                        
         SPACE 1                                                                
SETO4    LA    RF,1(R1,R3)         SAVE A(NEXT SLOT AT THIS LEVEL)              
         ST    RF,ANXTSLOT                                                      
         LR    RF,R3               IS THERE A HIGHER LEVEL TO MERGE             
         S     RF,=AL4(L'ALLOPTS)  RF=A(PREVIOUS LEVEL'S OPTIONS)               
         LA    R1,ALLOPTS                                                       
         CR    RF,R1               GET OUT IF PREVIOUS IS BEFORE FIRST          
         BL    XIT                                                              
         ZIC   R0,0(RF)            R0=N'SUB ELS. AT PREVIOUS LEVEL              
         LTR   R0,R0                                                            
         BZ    XIT                 NOTHING TO MERGE                             
         SPACE 1                                                                
         USING OPTD,R1                                                          
         LA    RF,1(RF)            RF=A(1ST OPTION AT PREVIOUS LEVEL)           
         SPACE 1                                                                
SETO6    MVC   BYTE,0(RF)                                                       
         BAS   RE,LOOKUP           RETURNS R1=A(OPTION TABLE ENTRY)             
         ST    R1,FULL                                                          
         TM    OPTSTAT,OPIGNORE    IGNORE THIS OPTION                           
         BO    SETO10                                                           
         TM    OPTSTAT,OPFORUSE    USE TYPE RELATED OPTIONS                     
         BZ    SETO8                                                            
         CLI   OPTCODE,OPUSE       IS THIS THE USE TYPE                         
         BE    SETO7                                                            
         TM    STATUS,SKIPPING     NO - ARE WE SKIPPING TO NEXT                 
         BO    SETO10              YES, SO GET NEXT                             
         L     R4,ANXTSLOT         NO, SO POINT TO END                          
         B     SETO9               AND ADD THIS ONE                             
         SPACE 1                                                                
SETO7    NI    STATUS,ALL-SKIPPING                                              
         BAS   RE,MATCH            LOOK FOR A MATCH                             
         BNE   SETO9               NOT FOUND - ADD TO THIS LEVEL                
         OI    STATUS,SKIPPING     FOUND - SKIP IT                              
         B     SETO10                                                           
         SPACE 1                                                                
SETO8    BAS   RE,MATCH            LOOK FOR A MATCH                             
         BE    SETO10                                                           
         SPACE 1                                                                
SETO9    L     R4,ANXTSLOT         NO MATCH - R4=A(END OF THIS LEVEL)           
         L     R1,FULL             R1=A(OPTION TABLE ENTRY)                     
         ZIC   RE,OPTSUBLN                                                      
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,1(RF)            LENGTH IS IN 1ST BYTE OF DATA                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(RF)       ADD TO END OF THIS LEVEL                     
         LA    R4,1(RE,R4)         BUMP BEYOND                                  
         ST    R4,ANXTSLOT         AND SAVE A(NEXT SLOT)                        
         LA    RE,L'ALLOPTS(R3)                                                 
         CR    R4,RE               CHECK PAST END OF CURRENT LEVEL              
         BNH   *+6                                                              
         DC    H'0'                TABLE TOO SMALL                              
         ZIC   RE,0(R3)            ADD TO N'ENTRIES AT THIS LEVEL               
         AHI   RE,1                                                             
         STC   RE,0(R3)                                                         
         SPACE 1                                                                
SETO10   L     R1,FULL             R1=A(PREVIOUS OPTION ENTRY)                  
         ZIC   RE,OPTSUBLN         BUMP TO NEXT ENTRY AT PREVIOUS LEVEL         
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,1(RF)            LENGTH IS IN 1ST BYTE OF DATA                
         LA    RF,1(RE,RF)                                                      
         BCT   R0,SETO6                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOOK UP AN OPTION IN TABLE                            
         SPACE 1                                                                
         USING OPTD,R1                                                          
LOOKUP   DS    0H                                                               
         L     R1,=A(OPTTAB)                                                    
         A     R1,RELO                                                          
LOOK2    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OPTCODE,BYTE        MATCH ON CONTENTS OF BYTE                    
         BER   RE                                                               
         LA    R1,OPTNEXT                                                       
         B     LOOK2                                                            
         SPACE 3                                                                
*              ROUTINE TO MATCH OPTION ENTRIES                                  
         SPACE 1                                                                
         USING OPTD,R1                                                          
*                                  R3=A(OPTIONS AREA FOR THIS LEVEL)            
*                                  RF=A(OPTIONS AREA FOR PREV LEVEL)            
MATCH    NTR1                                                                   
         LTR   R2,R2               R2=N'SUB ELS. ORIG. AT THIS LEVEL            
         BZ    NO                  NOTHING TO MATCH                             
         LA    R4,1(R3)            R4=A(THIS LEVEL'S OPTIONS)                   
         SPACE 1                                                                
         USING TAESSBCD,R4                                                      
MAT2     CLC   TAESSBCD,0(RF)      LOOK FOR MATCH AT THIS LEVEL                 
         BNE   MAT4                                                             
         CLI   0(RF),OPUSE         IF THIS IS USE TYPE                          
         BNE   YES                                                              
         CLC   TAESSBDT,TAESSBDT-TAESSBCD(RF) USE CODE MUST ALSO MATCH          
         BE    YES                                                              
         SPACE 1                                                                
MAT4     MVC   BYTE,TAESSBCD                                                    
         BAS   RE,LOOKUP           LOOK UP THIS ENTRY - RETURNS IN R1           
         ZIC   RE,OPTSUBLN                                                      
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,TAESSBDT         LENGTH IS IN 1ST BYTE OF DATA                
         LA    R4,1(RE,R4)         BUMP TO NEXT ENTRY                           
         BCT   R2,MAT2                                                          
         B     NO                  NOT FOUND - RETURN CC NOT EQUAL              
         DROP  R4                                                               
         EJECT                                                                  
*              OPTION SETTING ROUTINES                                          
         SPACE 1                                                                
         USING TAESSBCD,R4         R4=A(OPTION DATA ENTRY)                      
CANIN    DS    0H                                                               
         LA    RF,CANTAX                                                        
         B     STORIT                                                           
         SPACE 1                                                                
INCIN    DS    0H                                                               
         CLI   PROFBTYP,TABRTY12   IF TYPE 12                                   
         BE    INCIN05                                                          
         CLI   PROFBTYP,TABRTY22   OR TYPE 22                                   
         BNE   INCIN10                                                          
INCIN05  LA    RF,TNHRT2           OVERRIDE INDIVIDUAL HANDLING RATE            
         BAS   RE,STORITN                                                       
*                                                                               
INCIN10  LA    RF,CORPRT           AND CORPORATE HANDLING RATE                  
         B     STORIT                                                           
         SPACE 1                                                                
EORIN    DS    0H                                                               
         LA    RF,TNHRT                                                         
         B     STORIT                                                           
         SPACE 1                                                                
OVERIN   DS    0H                                                               
         OI    OTHOPT,OTHOV1                                                    
         LA    RF,OVERSCL                                                       
         XC    OVERASOF,OVERASOF                                                
         B     STORIT                                                           
         SPACE 1                                                                
OVER2IN  DS    0H                                                               
         OI    OTHOPT,OTHOV2                                                    
         LA    RF,OVERSCL2                                                      
         XC    OVERASOF,OVERASOF                                                
         B     STORIT                                                           
         SPACE 1                                                                
OVER12IN DS    0H                                                               
         OI    OTHOPT,OTHOV1+OTHOV2                                             
         MVC   OVERSCL,TAESSBDT    SET OV1                                      
         MVC   OVERSCL2,TAESSBDT+4 SET OV2                                      
         XC    OVERASOF,OVERASOF   CLEAR AS OF DATE                             
         B     XIT                                                              
         SPACE 1                                                                
OVASIN   DS    0H                                                               
         OI    OTHOPT,OTHOV1                                                    
         MVC   OVERASOF,TAESSBDT   SET AS OF DATE                               
         MVC   OVERSCL,TAESSBDT+3  AND RATE                                     
         B     XIT                                                              
         SPACE 1                                                                
OV2ASIN  DS    0H                                                               
         OI    OTHOPT,OTHOV2                                                    
         MVC   OVERASOF,TAESSBDT   SET AS OF DATE                               
         MVC   OVERSCL2,TAESSBDT+3 AND OV2 RATE                                 
         B     XIT                                                              
         SPACE 1                                                                
OV12ASIN DS    0H                                                               
         OI    OTHOPT,OTHOV1+OTHOV2                                             
         MVC   OVERASOF,TAESSBDT   SET AS OF DATE                               
         MVC   OVERSCL,TAESSBDT+3  AND OVERSCALE 1 RATE                         
         MVC   OVERSCL2,TAESSBDT+7 AND SECOND OVERSCALE RATE                    
         B     XIT                                                              
         SPACE 1                                                                
INTIN    DS    0H                                                               
         LA    RF,INTFEE                                                        
         B     STORIT                                                           
         SPACE 1                                                                
MULTIN   DS    0H                                                               
         LA    RF,MULTIPLY                                                      
         B     STORIT                                                           
         SPACE 1                                                                
CTYPIN   DS    0H                  COMMERCIAL TYPE                              
         LA    RF,CTYP                                                          
         B     STORIT                                                           
         SPACE 1                                                                
ATYPIN   DS    0H                  COMMERCIAL ACTRA TYPE                        
         L     R3,TCATACO                                                       
         USING TACOD,R3                                                         
         MVC   TACOCTYP,TAESSBDT                                                
         B     XIT                                                              
         SPACE 1                                                                
COMMIN   DS    0H                                                               
         LA    RF,COMM                                                          
         B     STORIT                                                           
         SPACE 1                                                                
EXPIN    DS    0H                                                               
         LA    RF,EXPIRY                                                        
         B     STORIT                                                           
         SPACE 1                                                                
YEARIN   DS    0H                                                               
         LA    RF,YEAR                                                          
         B     STORIT                                                           
         SPACE 1                                                                
YRASIN   DS    0H                                                               
         MVC   YRASOF,TAESSBDT      SET AS OF DATE                              
         MVC   YRASOFYR,TAESSBDT+3  AND YEAR                                    
         B     XIT                                                              
         EJECT                                                                  
*              OPTION SETTING ROUTINES  (CONT'D)                                
         SPACE 1                                                                
         USING TAESSBCD,R4         R4=A(OPTION DATA ENTRY)                      
GRTNOIN  DS    0H                                                               
         XR    R1,R1                                                            
         ICM   R1,3,TAESSBDT                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GUA,DUB                                                          
         B     XIT                                                              
         SPACE 1                                                                
SSNIN    DS    0H                                                               
         ICM   R1,15,TAESSBDT                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TGSSN,DUB                                                        
         B     XIT                                                              
         SPACE 1                                                                
PRIIN    DS    0H                                                               
         MVI   PRIMARY,C'Y'                                                     
         OI    STATUS2,ESTGRT      SET THIS COMMERCIAL HAS GUARANTEE            
         B     XIT                                                              
         SPACE 1                                                                
GUARIN   DS    0H                                                               
         LA    RF,GUARSW                                                        
         B     STORIT                                                           
         SPACE 1                                                                
LLIFTIN  DS    0H                                                               
         LA    RF,LLIFT                                                         
         B     STORIT                                                           
         SPACE 1                                                                
LLIFT2IN DS    0H                                                               
         LA    RF,LLIFT2                                                        
         B     STORIT                                                           
         SPACE 1                                                                
PNHRIN   DS    0H                                                               
         MVC   PNHRATE,TAESSBDT+2  SAVE BOTTOM 2 BYTES                          
         OC    PNHRATE,PNHRATE     IF RATE IS ZERO                              
         BNZ   *+10                                                             
         MVC   PNHRATE,=X'FFFF'    SET TO FF'S SO WE KNOW                       
         B     XIT                                                              
         SPACE 1                                                                
XIN      DS    0H                                                               
         MVI   XPERF,C'Y'                                                       
         B     XIT                                                              
         SPACE 1                                                                
EXCHIN   DS    0H                                                               
         LA    RF,DEFEXCH                                                       
         OI    OTHOPT,OTHEXCH                                                   
         B     STORIT                                                           
         SPACE 1                                                                
LASTIN   DS    0H                                                               
         LA    RF,LASTSRV                                                       
         B     STORIT                                                           
         EJECT                                                                  
*              OPTION SETTING ROUTINES  (CONT'D)                                
         SPACE 1                                                                
         USING TAESSBCD,R4         R4=A(OPTION DATA ENTRY)                      
         SPACE 1                                                                
NAPIN    DS    0H                                                               
         LA    RF,NAPDATE                                                       
         B     STORIT                                                           
         SPACE 1                                                                
COMNMIN  DS    0H                                                               
         ZIC   RE,TAESSBDT                                                      
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   COMNAME(0),TAESSBDT+1                                            
         SPACE 1                                                                
STATEIN  DS    0H                                                               
         LA    RF,ADSTATE                                                       
         B     STORIT                                                           
         SPACE 1                                                                
AFMIN    DS    0H                                                               
         LA    RF,AFMRATE                                                       
         B     STORIT                                                           
         SPACE 1                                                                
LIFTIN   DS    0H                                                               
         OI    CASTSTAT,TACASTLF                                                
         B     XIT                                                              
         SPACE 1                                                                
SLIFTIN  DS    0H                                                               
         OI    CASTSTA2,TACAS2LF                                                
         B     XIT                                                              
         SPACE 1                                                                
ALLIN    DS    0H                                                               
         OI    CASTSTAT,TACASTLF                                                
         OI    CASTSTA2,TACASALL                                                
         B     XIT                                                              
         SPACE 1                                                                
LONLYIN  DS    0H                                                               
         OI    CASTSTAT,TACASTLO+TACASTLF                                       
         B     XIT                                                              
         SPACE 1                                                                
SONLYIN  DS    0H                                                               
         OI    CASTSTA2,TACAST2O                                                
         B     XIT                                                              
         SPACE 1                                                                
FSONLYIN DS    0H                                                               
         OI    CASTSTA2,TACAST2O+TACASTLO                                       
         B     XIT                                                              
         SPACE 1                                                                
FRSTIN   DS    0H                                                               
         LA    RF,FRSTDATE                                                      
         B     STORIT                                                           
         SPACE 1                                                                
HLDRIN   DS    0H                                                               
         MVI   ELCODE,TAOPELQ      SET ELEMENT CODE - OVERSCALE PCT             
         B     HLDIN5                                                           
         SPACE 1                                                                
HLDIN    DS    0H                                                               
         MVI   ELCODE,TAOAELQ      SET ELEMENT CODE - OVERSCALE AMT             
HLDIN5   XR    R1,R1               SAVE USE CODE                                
         ICM   R1,7,TGUSCDE                                                     
         XC    HLDASOF,HLDASOF     CLEAR AS OF DATE                             
         MVC   TGUSCDE,=C'HLD'     SET USE                                      
         MVC   TGFULL,TAESSBDT     SET AMOUNT                                   
         BRAS  RE,UPDOAOP          UPDATE OVERSCALE AMT/PCT ELEMENT             
         BRAS  RE,CLROAOP          CLEAR OPP. OV AMT/PCT ELEMENT                
         STCM  R1,7,TGUSCDE        RESTORE USE CODE                             
         OI    OTHOPT,OTHHLD       SET HLD OVERRIDE                             
         B     XIT                                                              
         SPACE 1                                                                
HLDASIN  LA    R1,X'80'                                                         
         B     *+6                                                              
HLDRASIN XR    R1,R1                                                            
         MVC   HLDASOF,TAESSBDT     SET AS OF DATE                              
         MVC   HLDASAMT,TAESSBDT+3  AND AMT/RATE                                
         STC   R1,HLDASAMT          AMT OR RATE INDICATOR                       
         B     XIT                                                              
         EJECT                                                                  
*              OPTION SETTING ROUTINES  (CONT'D)                                
         SPACE 1                                                                
         USING TAESSBCD,R4         R4=A(OPTION DATA ENTRY)                      
         SPACE 1                                                                
         USING OPTD,R1             R1=A(OPTION TABLE ENTRY)                     
STORITN  NTR1                                                                   
*                                                                               
STORIT   DS    0H                  RF=A(STORAGE AREA)                           
         ZIC   RE,OPTSUBLN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   0(0,RF),TAESSBDT    SAVE TABLE DATA IN LOCAL STORAGE             
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE GENERATES ESTIMATED PAYMENTS                             
         SPACE 1                                                                
ESTIMATE NTR1                                                                   
         CLI   ACTLOPT,C'O'        IF ONLY ACTUALS REQUESTED                    
         BE    ESTX                   GET OUT                                   
         CLI   PROFREL,C'N'        IF DON'T WANT TO EST ON REL COMMLS           
         BNE   EST0                                                             
         L     R3,TCATACO                                                       
         USING TACOD,R3                                                         
         TM    TACOSTAT,TACOSTRL   THEN IF COMMERCIAL IS RELEASED               
         BO    ESTX                   GET OUT                                   
         DROP  R3                                                               
         SPACE 1                                                                
EST0     MVI   ESTMODE,PROCESTP    SET ESTIMATED PAYMENTS MODE                  
         SPACE 1                                                                
         ICM   R3,15,ASESSENT      IF HAVE A(SESSION PAYMENT)                   
         BNZ   EST2                THEN START WITH IT                           
EST1     LA    R3,USEOPTS          ELSE START AT BEGINNING OF TABLE             
         SPACE 1                                                                
EST2     OC    0(2,R3),0(R3)       ANY MORE USES                                
         BZ    EST18                                                            
         CLC   0(2,R3),HEXFFS      TEST PROCESSED THIS ENTRY ALREADY            
         BE    EST17                                                            
         BAS   RE,ESTINIT          INITIALIZE SOME AREAS                        
         SPACE 1                                                                
         LH    R4,0(R3)            USE DISPLACEMENT TO GET -                    
         LA    R4,PEROPTS(R4)      R4=A(USE OPTION ENTRY)                       
         USING TAESSBCD,R4                                                      
         SPACE 1                                                                
         CLI   TAESSBDT,UCAB       IF USE CODE IS CAB                           
         BNE   EST4                                                             
         CLI   TAESSBDT+1,UCABTV   AND TYPE IS TV                               
         BNE   EST4                                                             
         TM    STATUS2,ESTCBL      AND WILL ESTIMATE CBL                        
         BO    EST16                   THEN REJECT                              
         MVI   TAESSBDT,UCBL       ELSE SWITCH USE TYPE TO CBL                  
         MVI   TAESSBDT+1,0                                                     
         MVC   TCUNITS,=H'172'     AND SET MAXIMUM UNITS                        
         SPACE 1                                                                
EST4     GOTO1 USEVAL,DMCB,(X'80',TAESSBDT),TAESSBDT+1  VAL. USE TYPE           
         BNE   EST16                                                            
         SPACE 1                                                                
         USING TACOD,RE                                                         
         L     RE,TCATACO                                                       
         TM    TACOSTA2,TACOPCYC   IF COMMERCIAL IS A PER CYCLE                 
         BZ    EST5                                                             
         TM    TGUSSTA2,HLDTYPE    ONLY PROCESS HOLDING FEES                    
         BZ    EST16                                                            
         DROP  RE                                                               
         SPACE 1                                                                
EST5     TM    TGUSTYST,UPGRADE              IF THIS IS AN UPGRADE              
         BZ    EST7                                                             
         GOTO1 UPGRVAL,DMCB,TGUSCDE,TGUSTYP  SET GLOBAL VALUES                  
         SPACE 1                                                                
EST7     MVC   BYTE,TAESSBCD       SET USE TYPE OPTION CODE FOR LOOKUP          
         BAS   RE,LOOKUP           LOOK UP USE IN OPTION TABLE                  
         USING OPTD,R1             RETURNS R1=A(TABLE ENTRY)                    
         SPACE 1                                                                
EST8     ZIC   RE,OPTSUBLN         BUMP TO TO NEXT USER OPTION ENTRY            
         CLI   OPTCODE,OPCOMNM     FOR HYPO COMML NAME                          
         BNE   *+8                                                              
         IC    RE,TAESSBDT         LENGTH IS IN 1ST BYTE OF DATA                
         LA    R4,1(RE,R4)                                                      
         CLI   0(R4),0             CHECK FOR END-OF-LIST                        
         BE    EST12                                                            
         MVC   BYTE,TAESSBCD       LOOK UP THIS OPTION IN TABLE                 
         BAS   RE,LOOKUP           RETURNS R1=A(TABLE ENTRY)                    
         TM    OPTSTAT,OPFORUSE    MUST BE USE-RELATED                          
         BZ    EST12                                                            
         CLI   OPTCODE,OPUSE       STOP IF WE'VE REACHED NEXT USE TYPE          
         BE    EST12                                                            
         XR    RF,RF                                                            
         ICM   RF,3,OPTDISP                                                     
         BNZ   *+6                                                              
         DC    H'0'                MISSING SET ROUTINE DISPLACEMENT             
         AR    RF,RB                                                            
         LA    RE,EST10            SET RETURN ADDRESS FOR COMMON NTR1           
         NTR1                                                                   
         BR    RF                  GO OFF TO ROUTINE                            
EST10    B     EST8                                                             
         SPACE 1                                                                
EST12    CLI   MYUSASOF,C'Y'       IS THIS AN AS-OF ENTRY                       
         BNE   EST14                                                            
         L     RE,ATHSASOF                                                      
         USING ASOFD,RE                                                         
         MVC   ASOFUEQU,TGUSEQU    SET ADDITIONAL VALUES                        
         MVC   ASOFYORN,MYUSYORN                                                
         MVC   ASOFAMT,TCPAY                                                    
         MVC   ASOFOVER,MYUSOVER                                                
         OC    TCPCYCS,TCPCYCS     IF NON-ASOF DATE INPUT                       
         BZ    *+14                                                             
         MVC   ASOFDATE,TCPCYCS    USE IT                                       
         OI    ASOFSTAT,ASOFONLY   AND SET ENTRY IS FOR THIS DATE ONLY          
         TM    TGUSSTA2,HLDTYPE    IF ENTRY IS FOR HOLDING FEE                  
         BO    EST16               THEN DON'T MAKE PAYMENT NOW                  
         MVC   ASOFUEQU,HLDUSE     ELSE SET ASOF ENTRY FOR HLD                  
         MVC   TCPCYCS,ASOFDATE    AND SET TO MAKE PYMT FOR CURRENT USE         
         BRAS  RE,SENDMAIL                                                      
         SPACE 1                                                                
EST14    BAS   RE,PAYIT            ** MAKE THE PAYMENT **                       
         SPACE 1                                                                
EST16    C     R3,ASESSENT         IF POINTING TO SESSION ENTRY                 
         BNE   EST17                                                            
         MVC   0(2,R3),HEXFFS      SET PROCESSED THIS ENTRY ALREADY             
         B     EST1                AND START OVER AT BEG. OF TABLE              
         SPACE 1                                                                
EST17    LA    R3,2(R3)            ELSE BUMP TO NEXT ENTRY IN TABLE             
         B     EST2                AND LOOP                                     
         SPACE 2                                                                
EST18    CLC   PEND,HEXFFS         IF USER INPUT AN END DATE                    
         BE    ESTX                                                             
         TM    TGMEEQU,LIKETV+CABLE AND MEDIA IS TV,INT,NEW OR CABLE            
         BZ    ESTX                                                             
*        TM    TGUNEQU,AFM         AND THIS ISN'T A MUSICIAN                    
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    ESTX                                                             
         SPACE 1                                                                
         USING TACOD,RE                                                         
         L     RE,TCATACO          RE=A(COMMERCIAL DETAILS ELEMENT)             
         TM    TACOSTA2,TACOPCYC   IF COMMERCIAL IS PER CYCLE                   
         BO    EST20               SKIP AHEAD                                   
         DROP  R4                                                               
         SPACE 1                                                                
         TM    STATUS,HAVESESS     AND SESSION HASN'T BEEN PAID                 
         BO    EST20                                                            
         TM    AUTOOPT,AUTOKBSS    AND USER WANTS AUTO SESSIONS                 
         BZ    EST20                                                            
         MVC   AUTOUSE,BSSUSE      SET TO PAY AUTOMATIC BSS                     
         MVC   AUTOTYPE,BSSUTYPE                                                
         B     EST22                                                            
         SPACE 1                                                                
EST20    TM    AUTOOPT,AUTOKHLD    OR USER WANTS AUTO HLDS                      
         BZ    ESTX                                                             
         MVC   AUTOUSE,HLDUSE      SET TO PAY AUTOMATIC HLD                     
         MVI   AUTOTYPE,0                                                       
         SPACE 1                                                                
EST22    CLC   LASTHLD,PEND        IF LAST END DATE BEFORE PERIOD END           
         BNL   ESTX                                                             
         XC    AUTOCYCS,AUTOCYCS   CLEAR AUTO CYCLE DATES                       
         BAS   RE,HLDAUTO          GENERATE ADDITIONAL PAYMENT                  
         BE    EST20               TRY ANOTHER                                  
         SPACE 1                                                                
ESTX     B     XIT                                                              
         EJECT                                                                  
*              USE-RELATED OPTION SETTING ROUTINES                              
         SPACE 1                                                                
         USING TAESSBCD,R4         R4=A(THIS ESTIMATE SUB-ELEMENT)              
         SPACE 1                                                                
LIN      OI    TCUSETAB+3,TCLFTPRO                                              
         MVC   TCTUSESL,TCTUSES                                                 
         B     XIT                                                              
         SPACE 1                                                                
SIN      OI    TCUSETAB+3,TCLFTPR2                                              
         MVC   TCTUSESL,TCTUSES                                                 
         B     XIT                                                              
         SPACE 2                                                                
MAJIN    LA    RF,TCMAJORS                                                      
         B     STORIT                                                           
         SPACE 2                                                                
UNITSIN  LA    RF,TCUNITS                                                       
         B     STORIT                                                           
         SPACE 2                                                                
INSIN    LA    RF,TCINSRTS                                                      
         B     STORIT                                                           
         SPACE 2                                                                
TAGIN    LA    RF,TCTAGS                                                        
         B     STORIT                                                           
         SPACE 2                                                                
DEMOIN   LA    RF,TCDEMO                                                        
         B     STORIT                                                           
         SPACE 2                                                                
USESIN   LA    RF,TCTUSES                                                       
         OI    TCOPTS,TCONUMUS                                                  
         B     STORIT                                                           
         SPACE 2                                                                
USES2IN  MVC   FULL,TAESSBDT                                                    
         LH    RE,FULL             RE=START USE NUMBER                          
         BCTR  RE,0                                                             
         STH   RE,TCNUSES          SET N'USES ALREADY PAID                      
         LH    RF,FULL+2           RF=END USE NUMBER                            
         SR    RF,RE                                                            
         STH   RF,TCTUSES          SAVE N'USES TO PAY                           
         OI    TCOPTS,TCONUMUS                                                  
         MVI   MYUSSTUS,C'Y'       SET WE HAVE STARTING USE NUMBER              
         B     XIT                                                              
         SPACE 2                                                                
PCTIN    XR    RF,RF                                                            
         ICM   RF,3,TAESSBDT                                                    
         MHI   RF,100                                                           
         ST    RF,MYUSOVER                                                      
         B     XIT                                                              
         SPACE 2                                                                
CYCLEIN  LA    RF,TCPCYCS                                                       
         B     STORIT                                                           
         SPACE 2                                                                
DATEIN   LA    RF,MYUSDATE                                                      
         B     STORIT                                                           
         SPACE 2                                                                
HISTIN   MVI   MYUSHIST,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              USE-RELATED OPTION SETTING ROUTINES  (CONT'D)                    
         SPACE 2                                                                
CSFIN    DS    0H                  CONTRACT SERVICE FEE                         
         XC    CSFAMT,CSFAMT                                                    
         TM    STATUS2,COMLCSF     IF NO CSF FOR COMMERCIAL YET                 
         BO    CSFINX                                                           
         CLI   TAESSBDT,C'N'       CHECK IF N,Y, OR AMOUNT INPUT                
         BE    CSFINX                                                           
         CLI   TAESSBDT,C'Y'       IF WANT TABLE AMOUNT                         
         BNE   *+12                                                             
         BAS   RE,SETCSF           GET CSF AMOUNT FROM TABLE                    
         B     *+10                                                             
         MVC   CSFAMT,TAESSBDT     ELSE, SET AMOUNT PASSED                      
CSFINX   OI    STATUS2,COMLCSF     SET PROCESSED CSF                            
         B     XIT                                                              
         SPACE 1                                                                
PAY1IN   OI    TCPAYST2,TCVNR1U    PAY 1ST VARIATION                            
         B     XIT                                                              
         SPACE 2                                                                
APPLYIN  CLI   TAESSBDT,C'H'                                                    
         BNE   *+8                                                              
         OI    TCOPTS,TCOAPPLH                                                  
         CLI   TAESSBDT,C'S'                                                    
         BNE   *+8                                                              
         OI    TCOPTS,TCOAPPLS                                                  
         CLI   TAESSBDT,C'N'                                                    
         BNE   *+8                                                              
         OI    TCOPTS,TCONOAPP                                                  
         B     XIT                                                              
         SPACE 1                                                                
PNHIN    MVC   MYUSPNH,TAESSBDT    P&H AMOUNT                                   
         B     XIT                                                              
         SPACE 1                                                                
SPNHIN   MVC   TCSUBPNH,TAESSBDT   SUBJECT TO P&H AMOUNT                        
         OI    TCINPUT,TCINPNH     SET CORRES. STATUS BIT                       
         B     XIT                                                              
         SPACE 1                                                                
HNDIN    MVC   MYUSHND,TAESSBDT    INCORPORATED HANDLING AMT OVERRIDE           
         B     XIT                                                              
         SPACE 1                                                                
AMTIN    DS    0H                  * PAYMENT AMOUNT *                           
         CLI   PRIMARY,C'Y'        IF PRIMARY COMML OF TYPE 6 GUAR              
         BNE   AMTIX                                                            
         TM    TGUSSTA2,APPREUSE   IF USE ADDS FTRACKS                          
         BZ    AMTIX                                                            
         MVC   TGFULL,TAESSBDT     SET AMOUNT                                   
         MVI   ELCODE,TAOAELQ      UPDATE OVERSCALE AMOUNT ELEMENT              
         BRAS  RE,UPDOAOP                                                       
         B     XIT                                                              
         SPACE 1                                                                
AMTIX    LA    RF,TCPAY            ELSE SET TO SAVE PAYMENT AMOUNT              
         OI    TCINPUT,TCINPAY     INSURE WE PAY ZERO IF ZERO INPUT             
         B     STORIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              USE-RELATED OPTION SETTING ROUTINES  (CONT'D)                    
         SPACE 2                                                                
NIN      MVI   MYUSYORN,C'N'       SET TO NOT PAY THIS USE                      
         SPACE 1                                                                
*        TM    TGUNEQU,AFM         FOR NON-MUSICIANS                            
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    NIN1                                                             
         CLC   TGUSEQU,BSSUSE      IF THIS IS BSS                               
         BNE   *+16                                                             
         TM    AUTOOPT,AUTOKBSS    MUST BE AUTO CANDIDATE                       
         BO    NIN2                                                             
         B     NINX                                                             
         CLC   TGUSEQU,BSRUSE      IF THIS IS BSR                               
         BNE   *+16                                                             
         TM    AUTOOPT,AUTOKBSR    MUST BE AUTO CANDIDATE                       
         BO    NIN2                                                             
         B     NINX                                                             
         TM    TGUSSTA2,HLDTYPE    IF THIS IS HLD                               
         BZ    NINX                                                             
         TM    AUTOOPT,AUTOKHLD    MUST BE AUTO CANDIDATE                       
         BO    NIN2                                                             
         B     NINX                                                             
NIN1     CLI   TGUSEQU,UMUS        OR FOR MUSICIANS, IF MUS                     
         BNE   *+16                                                             
         TM    AUTOOPT,AUTOKMUS    MUST BE AUTO CANDIDATE                       
         BO    NIN2                                                             
         B     NINX                                                             
         CLI   TGUSEQU,UBSM        IF THIS IS BSM                               
         BNE   NINX                                                             
         TM    AUTOOPT,AUTOKBSM    MUST BE AUTO CANDIDATE                       
         BZ    NINX                                                             
NIN2     BAS   RF,GTASOF           GET A(NEXT ASOFCB ENTRY) - RETURN RE         
         BNE   NINX                NO MORE                                      
         MVI   MYUSASOF,C'Y'       SET WE'RE PROCESSING AN AS/OF ENTRY          
         ST    RE,ATHSASOF         SAVE A(THIS ENTRY)                           
NINX     B     XIT                                                              
         EJECT                                                                  
*              USE-RELATED OPTION SETTING ROUTINES  (CONT'D)                    
         SPACE 1                                                                
ASOFIN   DS    0H                                                               
         BAS   RF,GTASOF           GET A(NEXT ASOFCB ENTRY) - RETURN RE         
         BNE   XIT                 NO MORE                                      
         MVI   MYUSASOF,C'Y'       SET WE'RE PROCESSING AN AS/OF ENTRY          
         ST    RE,ATHSASOF         SAVE A(THIS ENTRY)                           
         USING ASOFD,RE                                                         
         LA    RF,ASOFDATE         SAVE THIS DATE                               
         B     STORIT                                                           
         SPACE 2                                                                
*              ROUTINE LOOKS UP NEXT ENTRY IN ASOFCB                            
         SPACE 1                                                                
GTASOF   DS    0H                                                               
         L     RE,ASOFCB           RE=A(AS/OF CONTROL BLOCK)                    
         L     R0,MAXASOF          R0=MAXIMUM NUMBER OF BLOCK ENTRIES           
         CLI   0(RE),0                                                          
         BER   RF                  FOUND IT - RETURN CC EQUAL                   
         LA    RE,ASOFNEXT                                                      
         BCT   R0,*-10                                                          
         LTR   RF,RF               NONE LEFT - RETURN CC NOT EQUAL              
         BR    RF                                                               
         EJECT                                                                  
*              ROUTINE TO MAKE AN ESTIMATED PAYMENT                             
         SPACE 1                                                                
PAYIT    NTR1                                                                   
         CLI   MYUSYORN,C'N'       DON'T PAY THIS USE TYPE                      
         BE    NO                                                               
         XC    EXCHANGE,EXCHANGE   CLEAR BILLED EXCHANGE RATE                   
         BAS   RE,SETUSE           SET REMAINING USE DETAILS                    
         SPACE 1                                                                
         CLI   ESTMODE,PROCAUTO    IF NOT ALREADY MAKING AUTO PAYMENT           
         BE    *+8                                                              
         BAS   RE,NEEDAUTO         TEST WHETHER NEED HLD/MUS PAYMENT            
         SPACE 1                                                                
         GOTO1 =A(DECIDE),DMCB,RR=RELO DECIDE WHETHER TO PAY THIS USE           
         BNE   NO                  NO - GET OUT                                 
         SPACE 1                                                                
         BAS   RE,ASOFSET          NOW LOOK FOR AN AS-OF BLOCK ENTRY            
         BNE   NO                                                               
         SPACE 1                                                                
         BRAS  RE,HLDSET           SET AS-OF HOLDING FEE OVERSCALE              
         SPACE 1                                                                
         BAS   RE,OVERSET          SET OVERSCALE RATES                          
         SPACE 1                                                                
         TM    TGUSSTA2,HLDTYPE    IF THIS IS A HOLDING FEE PAYMENT             
         BZ    *+8                                                              
         BAS   RE,CHKDLR           CHECK TO SEE IF IT'S COVERED BY DLR          
         SPACE 1                                                                
         TM    STATUS,DATEONLY     IF SET DATES ONLY THEN SKIP THIS             
         BO    PAY10                                                            
         OI    TCOPTS,TCRESCRS     SET TO RESOLVE CREDIT AMOUNTS                
         CLI   TGUSEQU,UITN        FAKE OUT SYSCALC FOR ITN USES                
         BNE   PAY03                                                            
         LH    R1,TCUNITS                                                       
         SH    R1,TCTUSES                                                       
         STH   R1,TCUNITS                                                       
         SPACE 1                                                                
         USING TACOD,RE                                                         
PAY03    TM    STATUS3,PRO2404B+PRO2404                                         
         BZ    PAY04               IF PROCESSING 2404B OR 2404 COMM'L           
         CLI   TGUSEQU,UBSC        AND PAYING CANADIAN SESSION                  
         BNE   PAY04                                                            
         L     RE,TCATACO                                                       
         MVI   TACOCTYP,CCTYNATL   TREAT LIKE ACTRA TYPE NATIONAL               
         DROP  RE                                                               
         SPACE 1                                                                
PAY04    GOTO1 ARATECLC,DMCB,(RC),TCD,SYSCOMM  ** GET THE RATES **              
         MVC   WORK(3),TGUSCDE                                                  
         MVC   WORK+3(12),=C' PAY - CC EQ'                                      
         BE    *+10                                                             
         MVC   WORK+13(2),=C'NE'                                                
         SPACE 1                                                                
         USING TACOD,RE                                                         
         TM    STATUS3,PRO2404B    IF PROCESSING 2404B COMMERCIAL               
         BZ    PAY04A                                                           
         L     RE,TCATACO                                                       
         MVI   TACOCTYP,CCTY04B    RESTORE ACTRA TYPE                           
         DROP  RE                                                               
         SPACE 1                                                                
         USING TACOD,RE                                                         
PAY04A   TM    STATUS3,PRO2404     IF PROCESSING 2404 COMMERCIAL                
         BZ    PAY05                                                            
         L     RE,TCATACO                                                       
         MVI   TACOCTYP,CCTY2404   RESTORE ACTRA TYPE                           
         DROP  RE                                                               
         SPACE 1                                                                
PAY05    CLI   TGUSEQU,UITN        RESTORE FROM FAKE OUT                        
         BNE   PAY06                                                            
         LH    R1,TCUNITS                                                       
         AH    R1,TCTUSES                                                       
         STH   R1,TCUNITS                                                       
PAY06    GOTO1 =A(MYTRACE2),DMCB,(15,WORK),TCD,TCEND-TCD,RR=RELO                
         CLC   WORK+13(2),=C'NE'                                                
         BE    NO                  GET OUT NOW IF RATE CALC. REJECTED           
*                                                                               
         XC    AOSAMT,AOSAMT                                                    
         XC    AOSPST,AOSPST                                                    
         OC    TCACTAOS,TCACTAOS   IF HAVE ACTRA AOS CONTR FOR THIS INV         
         BZ    PAY08                                                            
         L     R3,TCACTAOS         CALC 1% FOR AOS                              
         CVD   R3,DUB                                                           
         SRP   DUB,62,5                                                         
         CVB   R3,DUB                                                           
         STCM  R3,15,AOSAMT           R3=ACTRA AOS                              
         L     RF,TCPAYC           ADD AOS AMT TO CORP AMOUNT                   
         AR    RF,R3                                                            
         ST    RF,TCPAYC                                                        
*                                                                               
         CLC   TCAOSUNT,SPACES                                                  
         BNH   PAY08                                                            
         LA    R1,800              8% GST                                       
         CLC   TCAOSUNT,=C'ON '    ONTARIO                                      
         BE    PAY07                                                            
         LA    R1,900              9% QST                                       
         CLC   TCAOSUNT,=C'QC '    QUEBEC                                       
         BNE   PAY08                                                            
PAY07    XR    R2,R2                                                            
         MR    R2,R1               MULTIPLY BY PST/QST RATE                     
         ZAP   DUB(8),=P'0'                                                     
         CVD   R3,DUB                                                           
         SRP   DUB,60,5                                                         
         CVB   R0,DUB                                                           
         STCM  R0,15,AOSPST        SAVE PST/QST AMOUNT                          
*                                                                               
PAY08    OC    MYUSPNH,MYUSPNH     IF P&H AMOUNT OVERRIDDEN                     
         BZ    *+10                                                             
         MVC   TCPNH,MYUSPNH       SET IT NOW                                   
         MVC   CORPAMT,MYUSHND     SET INCORP. HANDLING AMT OVERRIDE            
         SPACE 1                                                                
         BRAS  RE,GUARUPD               UPDATE GRT BLOCK IF NECESSARY           
         L     RF,MAXGUAR                                                       
         MHI   RF,GUARLNQ                                                       
         GOTO1 =A(MYTRACE2),DMCB,=C'GUTAB',GUARTAB,(RF),RR=RELO                 
         SPACE 1                                                                
PAY10    GOTO1 =A(HISTUPD),DMCB,RR=RELO UPDATE UH TABLE IF NEC.                 
         SPACE 1                                                                
         TM    STATUS,DATEONLY     IF SET DATES ONLY THEN SKIP THIS             
         BO    PAY20                                                            
         GOTO1 =A(CLCAMTS),DMCB,RR=RELO  SET TNH & COMM AMOUNTS                 
         GOTO1 =A(CNVAMTS),DMCB,TCTOTS,NTCTOTS,RR=RELO  CONVERT AMOUNTS         
         GOTO1 =A(CNVAMTS),DMCB,COMMAMT,4,RR=RELO                               
         SPACE 1                                                                
PAY20    TM    TGUSSTAT,SESSION    IF THIS IS A SESSION PAYMENT                 
         BO    *+12                                                             
         TM    TGUSSTA2,APPREUSE   OR THIS USE CAN ADD FTRACKS                  
         BZ    *+8                                                              
         OI    STATUS,HAVESESS     SET WE'VE PAID SESSION                       
         TM    STATUS,DATEONLY     IF SET DATES ONLY THEN SKIP THIS             
         BO    PAYX                                                             
         GOTO1 =A(CALLDRV),DMCB,GLINPUT,RR=RELO  ADD TO DRIVER TABLES           
         SPACE 1                                                                
PAYX     B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SET USE DETAILS FOR ESTIMATED PAYMENTS                
         SPACE 1                                                                
SETUSE   NTR1                                                                   
         XC    TCTAUHEL,TCTAUHEL   CLEAR USAGE HISTORY ELEMENT                  
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF NO CYCLE START DEFINED                    
         BNZ   SETU4                                                            
         MVC   TCPCYCS,MYUSDATE    USE START DATE IF INPUT                      
         OC    TCPCYCS,TCPCYCS     IF STILL NO CYCLE START DEFINED              
         BNZ   SETU4                                                            
         TM    TGUSCYCK,OPTCYC     AND CYCLE DATES OPTIONAL                     
         BZ    SETU4                                                            
         MVC   TCPCYCS,TGTODAY1    USE TODAY'S DATE                             
         SPACE 1                                                                
SETU4    CLI   TGUSEQU,UDOR        UNLESS DORMANCY FEE USE                      
         BE    *+12                                                             
         TM    TGUSCYCK,OPTCYC     SKIP IF CYCLE DATES OPTIONAL                 
         BO    SETU10                                                           
         SPACE 1                                                                
         BAS   RE,GETHIST          LOOK UP USAGE HISTORY FOR USE                
         L     R2,AHISTENT                                                      
         USING HISTD,R2            R2=A(HISTORY TABLE ENTRY)                    
         SPACE 1                                                                
         BAS   RE,CYCSTART         CALCULATE CYCLE START DATE                   
         LA    R4,PVIN1DYL         SET PARAMETER FOR PERVAL                     
         GOTO1 CYCEND,DMCB,(R4)    CALCULATE END DATE                           
         SPACE 1                                                                
         MVC   HISTCYC,TCPCYC      SET CYCLE DATES IN USAGE HISTORY TAB         
         OC    TCIPCYC,TCIPCYC     IF SPECIAL CYCLE DATES SET                   
         BZ    *+10                                                             
         MVC   HISTCYC,TCIPCYC     USE THESE FOR HISTORY TABLE                  
         SPACE 1                                                                
         L     RF,MAXHIST                                                       
         MHI   RF,HISTLNQ                                                       
         GOTO1 =A(MYTRACE2),DMCB,=C'HISTAB',HISTTAB,(RF),RR=RELO                
         SPACE 1                                                                
SETU10   MVC   APPLYDTE,TCPCYCS    INIT APPLY DATE TO CYCLE START               
         SPACE 1                                                                
         OC    MYUSDATE,MYUSDATE   IF WE HAVE USE DATE (I/P BY USER)            
         BZ    *+10                                                             
         MVC   APPLYDTE,MYUSDATE   THEN APPLY PERIOD S/B BASED ON IT            
         SPACE 1                                                                
         MVC   TGYREQU,YEAR        SET DEFAULT CONTRACT YEAR                    
         OC    YRASOF,YRASOF       IF AS OF DATE DEFINED                        
         BZ    SETU12                                                           
         CLC   TCPCYCS,YRASOF      AND IF CYCLE START NOT BEFORE IT             
         BL    SETU12                                                           
         MVC   TGYREQU,YRASOFYR    SET AS/OF DATE YEAR OVERRIDE                 
SETU12   GOTO1 YRVAL,DMCB,(X'80',TGYREQU)  GET CHARACTER YEAR                   
         SPACE 1                                                                
         OC    TGGUA,TGGUA         IF PERFORMER ON GUARANTEE                    
         BZ    *+8                                                              
         BRAS  RE,SETPRI           SET PRIMARY COMMERCIAL IF NECESSARY          
         SPACE 1                                                                
         CLI   FREEOPT,C'N'        IF FIRST MUSIC PAYMENT S/B FREE              
         BE    SETU20                                                           
         CLI   TGUSEQU,UMUS        AND THIS IS A MUSIC PAYMENT                  
         BNE   SETU20                                                           
         CLI   TGUSTYP,UMUS13W     AND REUSE TYPE                               
         BE    *+12                                                             
         CLI   TGUSTYP,UMUS8W                                                   
         BNE   SETU20                                                           
         OC    LASTMUS,LASTMUS     AND THIS IS FIRST ONE                        
         BNZ   SETU20                                                           
         CLI   ACTLOPT,C'X'        SKIP 1ST AIR DATE CHECKS IF IGNORING         
         BE    SETU16                                   ACTUAL PAYMENTS         
*                                         AND NO FIRST AIR DATE ON COML         
         OC    ELTACO+TACOAIR-TACOD(3),ELTACO+TACOAIR-TACOD                     
         BZ    SETU16                           (AS PER TF 04/30/93)            
         CLC   TCPCYCS,ELTACO+TACOAIR-TACOD OR CYCLE START=FIRST AIR            
         BNE   SETU20                           (AS PER TF 05/06/93)            
SETU16   XC    TCPAY,TCPAY         SET PAYMENT AMOUNT TO ZERO                   
         OI    TCINPUT,TCINPAY     SET WE HAVE PAY AMOUNT                       
         XC    TCSUBPNH,TCSUBPNH   SUBJECT TO P&H AMOUNT = ZERO                 
         OI    TCINPUT,TCINPNH     SET WE HAVE SUBJ TO P&H AMOUNT               
         SPACE 1                                                                
SETU20   XC    TAXAMTS,TAXAMTS     INITIALIZE T&H AMOUNTS                       
         SPACE 1                                                                
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         NI    TACOSTAT,ALL-TACOSCRT                                            
         TM    TGUSSTAT,CANUSE     IF CANADIAN USE                              
         BZ    *+8                                                              
         OI    TACOSTAT,TACOSCRT   SET PAY CANADIAN RATES BIT ON                
         DROP  R3                                                               
         SPACE 1                                                                
         TM    STATUS2,COMLCSF     IF HAVEN'T PROCESSED CSF                     
         BO    SETU25                                                           
         CLI   TGUSEQU,UBSC        AND ACTRA SESSION (BSC OR  OTC)              
         BE    SETU22                                                           
         CLI   TGUSEQU,UOTC                                                     
         BNE   SETU25                                                           
SETU22   BAS   RE,SETCSF           SET CSF AMOUNT FROM TABLE                    
         OI    STATUS2,COMLCSF     AND SET NOW PROCESSED                        
         SPACE 1                                                                
SETU25   MVC   TCLFTSEC,ELTALF+TALFSEC-TALFD  SET L'LIFT FOR RATE CALC.         
         TM    TCUSETAB+3,TCLFTPR2 2ND LIFT?                                    
         BZ    *+10                                                             
         MVC   TCLFTSEC,ELTALF2+TALFSEC-TALFD                                   
         MVC   TCFLMDTE,FLMDATE                   FILM DATE                     
         MVC   TCRECDTE,RECDATE                   RECORD DATE                   
         SPACE 1                                                                
         TM    STATUS,HYPOPERF     IF PROCESSING HYPO. PERFORMER                
         BZ    SETU30                                                           
         OC    TCCAFRST,TCCAFRST   AND 1ST SERVICE DATE NOT DEFINED YET         
         BNZ   SETU30                                                           
         TM    TGUSSTA2,HLDTYPE    AND THIS ISN'T A HOLDING FEE                 
         BO    SETU30                                                           
         OC    FRSTDATE,FRSTDATE   IF FIRST FIXED CYLE ON COMML                 
         BZ    SETU28                                                           
         MVC   TCCAFRST,FRSTDATE   SET CAST FIRST SERVICE                       
         MVC   TCCAFCYC,FRSTDATE   AND CAST FIRST FIXED CYCLE                   
         B     SETU30                                                           
SETU28   MVC   TCCAFRST,TCPCYCS    ELSE, USE PWOS CYCLE START                   
         MVC   TCCAFCYC,TCPCYCS                                                 
         SPACE 1                                                                
SETU30   OC    PNHRATE,PNHRATE     IF WE HAVE P&H RATE OVERRIDE                 
         BZ    XIT                                                              
         OI    TCOPTS,TCOPNHR      LET RATE CALC. KNOW                          
         MVC   TCPNHR,PNHRATE      SET IT NOW                                   
         CLC   TCPNHR,=X'FFFF'     IF FF'S                                      
         BNE   XIT                                                              
         XC    TCPNHR,TCPNHR       SET TO ZERO NOW                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS UP CURRENT USE IN USAGE HISTORY TABLE              
         SPACE 1                                                                
GETHIST  NTR1                                                                   
         ICM   R2,15,AHISTENT      R2=A(SAVED HISTORY TABLE ENTRY)              
         BNZ   GHST10                                                           
         L     R2,HISTTAB          R2=A(USAGE HISTORY TABLE)                    
         USING HISTD,R2                                                         
         XR    R3,R3               CLEAR A(SAVED ENTRY)                         
         SPACE 1                                                                
GHST2    OC    0(HISTLNQ,R2),0(R2) TEST END OF TABLE                            
         BZ    GHST8                                                            
         CLC   TGUSEQU,HISTUSE     MATCH ON USE EQUATE                          
         BNE   GHST6                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,UCLA        FOR CLASS A'S                                
         BE    GHST3                                                            
         CLI   TGUSEQU,UPAX        FOR PAX                                      
         BE    GHST3                                                            
         CLI   TGUSEQU,UITN        FOR ITN                                      
         BE    GHST3                                                            
         TM    TGUSSTA3,UPGRDBLE   AND FOR UPGRADABLE USE TYPES                 
         BO    GHST3                                                            
         CLI   TGUSEQU,UDLR        & DEALER(NEED TO KEEP MULT ENTRIES)          
         BNE   GHST10                                                           
         SPACE 1                                                                
GHST3    OC    TCPCYCS,TCPCYCS     IF WE HAVE A NEW CYCLE START                 
         BZ    GHST5                                                            
         CLC   TCPCYCS,HISTCYCS    AND IT'S WITHIN OLD CYCLE DATES              
         BL    GHST6                                                            
         CLC   TCPCYCS,HISTCYCE                                                 
         BH    GHST6                                                            
         CLI   TGUSEQU,UITN        THEN IF THIS IS ITN                          
         BE    *+8                                                              
         CLI   TGUSEQU,UPAX        THEN IF THIS IS PAX                          
         BE    *+8                                                              
         CLI   TGUSEQU,UCLA        OR IF THIS IS CLASS A                        
         BNE   GHST4                                                            
         OC    MYUSDATE,MYUSDATE   AND IT'S NOT ALREADY AROUND                  
         BNZ   *+10                                                             
         MVC   MYUSDATE,TCPCYCS    SET EFFECTIVE USE DATE                       
         MVC   TCPCYC,HISTCYC      SET CYCLE DATES FOR CREDITTING               
         B     GHST10                                                           
         SPACE 1                                                                
GHST4    TM    TGUSTYST,UPGRADE    OR IF THIS IS AN UPGRADE                     
         BZ    GHST6                                                            
         MVC   TCIPCYC,HISTCYC     SET CYCLE DATES FOR CREDITTING               
         OC    TCPCYCE,TCPCYCE     IF WE DON'T HAVE CYCLE END DATE              
         BNZ   *+10                                                             
         MVC   TCPCYCE,HISTCYCE    THEN USE END DATE FROM THIS CYCLE            
         B     GHST10                                                           
         SPACE 1                                                                
GHST5    LTR   R3,R3               IF NO SAVED ENTRY YET                        
         BZ    *+14                                                             
         CLC   HISTCYCE,HISTCYCE-HISTD(R3) OR THIS CYC ENDS AFTER SAVED         
         BNH   *+6                                                              
         LR    R3,R2               SAVE A(THIS ENTRY)                           
         SPACE 1                                                                
GHST6    LA    R2,HISTNEXT         TRY NEXT ENTRY                               
         B     GHST2                                                            
         SPACE 1                                                                
GHST8    LTR   R3,R3               IF WE HAVE A(SAVED ENTRY)                    
         BZ    GHST9                                                            
         TM    TGUSTYST,UPGRADE    AND THIS IS AN UPGRADE                       
         BZ    *+16                                                             
         LR    R2,R3               SET TO USE SAVED ENTRY                       
         MVC   TCPCYC,HISTCYC      USE SAME CYCLE DATES AS WELL                 
         B     GHST10              NOW BUILD USAGE HISTORY EL.                  
         SPACE 1                                                                
         MVC   HISTCYCE,HISTCYCE-HISTD(R3)  ELSE USE ITS END FOR NEXT           
         B     GHSTX                                                            
         SPACE 1                                                                
GHST9    BAS   RE,BLDHIST          ELSE LOOK FOR ACTUAL HIST FOR CYCLE          
         BNE   GHSTX                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,UDLR        IF DEALER                                    
         BNE   GHST10                                                           
         MVC   HISTDLR,HISTD       SAVE ACTUAL HIST CYCLE                       
         SPACE 1                                                                
GHST10   BRAS  RE,BLDTAUH          FOUND HIST - BUILD USAGE HISTORY EL          
         SPACE 1                                                                
GHSTX    MVC   HISTUSE,TGUSEQU     SET USE EQUATE FOR NEW ENTRY                 
         SPACE 1                                                                
         ST    R2,AHISTENT         SAVE A(ENTRY)                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUILD USAGE HISTORY FOR USE                              
         SPACE 1                                                                
         USING HISTD,R2            R2=A(NEXT AVAIL. SLOT IN TABLE)              
BLDHIST  NTR1                                                                   
         CLI   ACTLOPT,C'X'        USER WANTS TO IGNORE ACTUAL USAGE            
         BE    NO                                                               
         TM    STATUS,HYPOPERF     DON'T BOTHER FOR HYPO. PERFORMERS            
         BO    NO                                                               
         TM    TGUSSTA2,APPREUSE   IF USE TYPES CAN CREATE TACRELS              
         BO    NO                  GET OUT (HANDLED BY GETCAST)                 
         SPACE 1                                                                
         NI    STATUS2,ALL-MUSTOFMU  INIT. MUS/FMU SWITCH INDICATOR             
         SPACE 1                                                                
BHST1    XC    KEY,KEY             FIND LAST USAGE HISTORY REC. FOR USE         
         LA    R3,KEY              R3=A(USAGE HISTORY KEY)                      
         USING TLUHD,R3                                                         
         MVI   TLUHCD,TLUHCDQ      RECORD EQUATE                                
         MVC   TLUHCOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         GOTO1 HIGH                                                             
         B     BHST4                                                            
         SPACE 1                                                                
BHST2    GOTO1 SEQ                 TRY NEXT USAGE HISTORY RECORD                
         XC    HISTDATA,HISTDATA                                                
         SPACE 1                                                                
BHST4    CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE  DID WE FIND REC. FOR USE         
         BE    BHST5                                                            
         CLI   TGUSEQU,UMUS        IF LOOKING FOR MUS AND DIDN'T FIND           
         BE    *+12                GO TRY FMU                                   
         LA    R4,1                ELSE SET TO RETURN CC NE                     
         B     BHST14                                                           
         SPACE 1                                                                
         MVC   SVMUSTYP,TGUSTYP    SAVE MUS TYPE SO CAN RESTORE LATER           
         MVI   HALF,UFMU           SET FOR FMU                                  
         MVI   HALF+1,0                                                         
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1                                  
         OI    STATUS2,MUSTOFMU    SET WE SWITCHED                              
         B     BHST1                                                            
         SPACE 1                                                                
BHST5    GOTO1 GETREC                                                           
         GOTO1 =A(MYTRACE),DMCB,=C'HISTTAB',RR=RELO                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   BHST2                                                            
         USING TAUHD,R4            BUILD HISTORY TABLE ENTRY FOR USE            
         SPACE 1                                                                
         CLI   TGUSEQU,UCBL        SPECIAL FOR CABLE                            
         BE    *+12                                                             
         CLI   TGUSEQU,USCB        OR SPANISH CABLE                             
         BNE   *+14                                                             
         MVC   HISTUNIT,TAUHCBUN   UNITS                                        
         B     BHST6                                                            
         SPACE                                                                  
         CLI   TGUSEQU,ULCB        SPECIAL FOR LOCAL CABLE                      
*        BNE   *+14                                                             
*        MVC   HISTMAJ,TAUHLCBM    MAJORS                                       
*        B     BHST6                                                            
         BE    BHST6                                                            
         SPACE                                                                  
         CLI   TGUSEQU,URRN        SPECIAL FOR RADIO REGIONAL NETWORK           
         BNE   *+14                                                             
         MVC   HISTMAJ,TAUHRMAJ    MAJORS                                       
         B     BHST6                                                            
         SPACE 1                                                                
         TM    TGUSTYST,INSERTS    SPECIAL FOR INSERTS FOR BOOKENDS             
         BZ    *+10                                                             
         MVC   HISTINS,TAUHINS     INSERTS                                      
         TM    TGUSTYST,MAJORS                                                  
         BZ    *+10                                                             
         MVC   HISTMAJ,TAUHMAJ     MAJORS                                       
         TM    TGUSTYST,UNITS                                                   
         BZ    *+10                                                             
         MVC   HISTUNIT,TAUHUNT    UNITS                                        
         SPACE 1                                                                
BHST6    CLI   TGUSEQU,UPAX        OR FOR PAX                                   
         BE    BHST7                                                            
         CLI   TGUSEQU,UCLA        OR FOR CLASS A                               
         BE    BHST7                                                            
         CLI   TGUSEQU,UITN        OR FOR ITN                                   
         BE    BHST7                                                            
         TM    TGUSSTA3,UPGRDBLE   OR UPGRADABLE USE TYPES                      
         BZ    BHST12                                                           
         SPACE 1                                                                
BHST7    OC    TCPCYCS,TCPCYCS     IF CYCLE START NOT DEFINED                   
         BNZ   BHST10                                                           
         TM    TGUSTYST,UPGRADE    AND WE'RE PAYING AN UPGRADE                  
         BZ    *+10                                                             
         MVC   TCPCYC,TAUHSTRT     USE SAME CYCLE DATES                         
         B     BHST12                                                           
         SPACE 1                                                                
BHST10   CLC   TCPCYCS,TAUHSTRT    IF EST CYCLE START IS WITHIN                 
         BL    BHST2                                                            
         CLC   TCPCYCS,TAUHEND     THIS ACTUAL CYCLE                            
         BH    BHST2                                                            
         CLI   TGUSEQU,UITN        AND THIS IS ITN                              
         BE    *+8                                                              
         CLI   TGUSEQU,UPAX        OR THIS IS PAX                               
         BE    *+8                                                              
         CLI   TGUSEQU,UCLA        OR THIS IS A CLASS A                         
         BNE   BHST11                                                           
         OC    MYUSDATE,MYUSDATE   AND IT'S NOT ALREADY AROUND                  
         BNZ   *+10                                                             
         MVC   MYUSDATE,TCPCYCS    SET EFFECTIVE USE DATE                       
         MVC   TCPCYC,TAUHSTRT     SET CYCLE DATES FOR CREDITTING               
         MVC   HISTUSN,TAUHUSN     SAVE N'USES PAID ALREADY                     
         MVC   HISTUSNL,TAUHUSNL                                                
         B     BHST12                                                           
         SPACE 1                                                                
BHST11   TM    TGUSTYST,UPGRADE    OR IF THIS IS AN UPGRADE                     
         BZ    BHST2                                                            
         MVC   TCIPCYC,TAUHSTRT    SET CYCLE DATES FOR CREDITTING               
         OC    TCPCYCE,TCPCYCE     IF WE DON'T HAVE CYCLE END DATE              
         BNZ   *+10                                                             
         MVC   TCPCYCE,TAUHEND     USE END DATE FROM EXISTING CYCLE             
         SPACE 1                                                                
BHST12   MVC   HISTCYC,TAUHSTRT    SAVE LAST CYCLE DATES IN TABLE               
         SPACE 1                                                                
         XR    R4,R4               SET TO RETURN CC EQ                          
         SPACE 1                                                                
BHST14   TM    STATUS2,MUSTOFMU    IF SWITCHED FROM MUS TO FMU                  
         BZ    BHSTX                                                            
         MVI   HALF,UMUS           RESTORE MUS BEFORE EXITING                   
         MVC   HALF+1(1),SVMUSTYP                                               
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1                                  
         SPACE 1                                                                
BHSTX    LTR   R4,R4                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES CYCLE START DATES                             
         SPACE 1                                                                
         USING HISTD,R2            R2=A(HISTORY TABLE ENTRY)                    
CYCSTART NTR1                                                                   
         OC    TCPCYCS,TCPCYCS     GET OUT IF WE HAVE START ALREADY             
         BNZ   CYCSTX                                                           
         SPACE 1                                                                
CYCST2   OC    HISTCYCE,HISTCYCE   IF WE HAVE A PREV. END DATE                  
         BZ    CYCST4                                                           
         GOTO1 DATCON,DMCB,(1,HISTCYCE),(0,WORK) ELSE ADD 1 DAY TO PREV         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,TCPCYCS)   TO CREATE NEW START         
         B     CYCSTX                                                           
         SPACE 1                                                                
*YCST4   TM    TGUNEQU,AFM         IF THIS IS MUSICIAN                          
CYCST4   GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    *+10                                                             
         MVC   TCPCYCS,MUSDATE     USE MUSIC DATE                               
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     TEST WE STILL NEED CYCLE START DATE          
         BNZ   CYCST8                                                           
         TM    TGMEEQU,RADIO       IF MEDIA IS RADIO                            
         BZ    *+10                                                             
         MVC   TCPCYCS,RECDATE     USE RECORD DATE                              
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF WE STILL DON'T HAVE START DATE            
         BNZ   CYCST8                                                           
         MVC   TCPCYCS,TCCAFCYC    USE CAST FIRST FIXED CYCLE                   
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF WE STILL DON'T HAVE START DATE            
         BNZ   CYCST8                                                           
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         MVC   TCPCYCS,TACOFCYC    USE COMMERCIAL FIRST FIXED CYCLE             
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF WE STILL DON'T HAVE START DATE            
         BNZ   CYCST8                                                           
         MVC   TCPCYCS,FLMDATE     USE FILM DATE                                
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF WE STILL DON'T HAVE START DATE            
         BNZ   CYCST8                                                           
         MVC   TCPCYCS,TCCAFRST    USE CAST FIRST SERVICES DATE                 
         SPACE 1                                                                
         OC    TCPCYCS,TCPCYCS     IF WE STILL DON'T HAVE START DATE            
         BNZ   CYCST8                                                           
         MVC   TCPCYCS,TGTODAY1    USE TODAY'S DATE (WHEW!)                     
         SPACE 1                                                                
CYCST8   TM    TGUSSTA2,HLDTYPE    IF PAYING HLD TYPE PAYMENT                   
         BZ    CYCSTX                                                           
         LA    R4,PVIN1DYL         SET PARAMETER FOR PERVAL                     
         GOTO1 CYCEND,DMCB,(R4)    CALCULATE END DATE OF SESSION                
         MVC   HISTCYCE,TCPCYCE    USE THIS TO CALC START OF HLD                
         XC    TCPCYCE,TCPCYCE     CLEAR END                                    
         B     CYCST2              GO START OVER                                
         SPACE 1                                                                
CYCSTX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES CYCLE END DATES                               
*              P1 BYTE 3 = PARAMETER FOR PERVAL                                 
         SPACE 1                                                                
         USING HISTD,R2            R2=A(HISTORY TABLE ENTRY)                    
CYCEND   NTR1                                                                   
         L     R4,0(R1)            R4=PARAMETER FOR PERVAL                      
         OC    TCPCYCE,TCPCYCE     DON'T BOTHER IF WE HAVE IT ALREADY           
         BNZ   CYCEX                                                            
         OC    TCPCYCS,TCPCYCS     OR IF WE DON'T HAVE START                    
         BZ    CYCEX                                                            
         SPACE 1                                                                
**NO-OP* CLI   TGUSEQU,UGRT        SPECIAL FOR GUARANTEES                       
**NO-OP* BNE   *+12                                                             
**NO-OP* BAS   RE,EXTEND           EXTEND CORRESPONDING TO PREV. PERIOD         
**NO-OP* B     CYCEX                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,UGRT        EXTEND IS NO-OP'D SO THESE STATMENTS         
         BE    CYCEX               DO SAME AS ABOVE FOUR.                       
         SPACE 1                                                                
         CLI   TGUSWKS,0           GET OUT IF L'CYCLE = 0                       
         BE    CYCEX                                                            
         MVC   WORK(17),SPACES                                                  
         GOTO1 DATCON,DMCB,(1,TCPCYCS),(8,WORK)                                 
         MVI   WORK+8,C'-'         SET HYPHEN (REQUIRED NOW)                    
         SPACE 1                                                                
         MVC   BYTE,TGUSWKS        SET L'CYCLE FROM USE TABLE                   
         SPACE 1                                                                
         CLI   BYTE,13             IF CYCLE DEFINED AS 13 WEEKS                 
         BNE   *+16                                                             
         TM    AGYSTAT,TAAYS13W    TEST THIS AGENCY USES 13 WEEK CYCLES         
         BO    *+8                                                              
         MVI   BYTE,X'80'+3        ELSE SET TO 3 MONTHS                         
         SPACE 1                                                                
         ZIC   RF,BYTE             ISOLATE NUMBER                               
         CLI   TGUSEQU,UDOR                                                     
         BE    CYCE10                                                           
         SLL   RF,26                                                            
         SRL   RF,26                                                            
         SPACE 1                                                                
CYCE10   MVI   WORK+9,C'('         BUILD DISPLAY FORMAT                         
         SPACE 1                                                                
         EDIT  (RF),(3,WORK+10),ALIGN=LEFT,WRK=BLOCK                            
         LR    R1,R0                                                            
         LA    R1,WORK+10(R1)      BUMP PAST NUMBER                             
         SPACE 1                                                                
         CLI   TGUSEQU,UDOR                                                     
         BNE   CYCE20                                                           
         MVI   0(R1),C'D'                                                       
         B     CYCE30                                                           
         SPACE 1                                                                
CYCE20   MVI   0(R1),C'W'          SET WEEKS OR MONTHS                          
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'M'                                                       
         TM    BYTE,X'40'                                                       
         BZ    *+8                                                              
         MVI   0(R1),C'D'                                                       
         SPACE 1                                                                
CYCE30   MVI   1(R1),C')'          END WITH TRAILING PARENTHESIS                
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         LA    RF,14                                                            
         CLI   TGUSEQU,UDOR                                                     
         BNE   *+8                                                              
         LA    RF,15                                                            
         GOTO1 PERVAL,DMCB,((RF),WORK),((R4),(R3))                              
         SPACE 1                                                                
         MVC   TCPCYCE,PVALPEND    SAVE END DATE                                
CYCEX    B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET CONTRACT SERVICE FEE AMOUNT FROM TABLE            
         SPACE 1                                                                
SETCSF   NTR1                                                                   
         L     R3,TCATACO                                                       
         USING TACOD,R3                                                         
         L     R1,=A(CSFTBL)                                                    
         A     R1,RELO                                                          
SETCSF5  CLI   0(R1),X'FF'                                                      
         BE    SETCSFX                                                          
         CLC   TACOCTYP,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,L'CSFTBL(R1)                                                  
         B     SETCSF5                                                          
         SPACE 1                                                                
         MVC   CSFAMT,1(R1)                                                     
         CLI   TACOMED,TACOMEDT                                                 
         BE    SETCSFX                                                          
         CLI   TACOMED,TACOMEDI                                                 
         BE    SETCSFX                                                          
         CLI   TACOMED,TACOMEDN                                                 
         BE    SETCSFX                                                          
         MVC   CSFAMT,5(R1)                                                     
SETCSFX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE EXTENDS GUARANTEES CORRESPONDING TO PREV PERIOD          
         SPACE 1                                                                
*&&UK                                                                           
         USING HISTD,R2            R2=A(HISTORY TABLE ENTRY)                    
EXTEND   NTR1                                                                   
         OC    HISTCYCS,HISTCYCS   MUST HAVE PREVIOUS PERIOD START              
         BZ    EXTX                                                             
         OC    HISTCYCE,HISTCYCE                         AND END                
         BZ    EXTX                                                             
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(1,HISTCYCS),(0,WORK)  CONVERT TO EBCDIC             
         GOTO1 (RF),(R1),(1,HISTCYCE),(0,WORK+6)                                
         GOTO1 (RF),(R1),(1,TCPCYCS),(0,WORK+12)  ALSO CVT NEW START            
         SPACE 1                                                                
         GOTO1 PERVERT,DMCB,WORK,WORK+6  CALCULATE N'DAYS IN PERIOD             
         LH    R3,8(R1)                                                         
         SPACE 1                                                                
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,(R3)  CALC APPROX. NEW END            
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,WORK+18),(1,TCPCYCE)  SAVE AS PWOS                
         SPACE 1                                                                
         PACK  DUB,WORK+8(2)       CVT OLD END MONTH TO BINARY                  
         CVB   R1,DUB                                                           
         LA    RF,DAYTAB-1(R1)     RF=PWOS LAST DAY OF MONTH                    
         SPACE 1                                                                
         CLC   HISTCYCE+2(1),0(RF) IF PREV END WAS ON LAST DAY OF MTH           
         BL    EXT30                                                            
         SPACE 1                                                                
EXT30    DS    0H                                                               
         SPACE 1                                                                
EXTX     B     XIT                                                              
         SPACE 1                                                                
DAYTAB   DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
*&&                                                                             
         EJECT                                                                  
*              ROUTINE DETERMINES IF AUTO PAYMENT IS NECESSARY                  
         SPACE 1                                                                
NEEDAUTO NTR1                                                                   
         CLI   AUTOOPT,0           TEST USER WANTS THEM                         
         BE    NAUTX                                                            
         TM    TGUSSTAT,SESSION    GET OUT IF THIS IS SESSION PAYMENT           
         BO    NAUTX                                                            
         TM    TGUSTYST,UPGRADE    OR IF THIS IS AN UPGRADE                     
         BO    NAUTX                                                            
         CLI   TGUSEQU,UGRT        ALSO IF THIS IS GUARANTEE                    
         BE    NAUTX                                                            
         MVI   AUTOTYPE,0          INITIALIZE                                   
         XC    AUTOCYCS,AUTOCYCS                                                
         SPACE 1                                                                
         TM    STATUS,HAVESESS     SKIP IF SESSION HAS BEEN PAID                
         BO    NAUT20                                                           
         SPACE 1                                                                
         MVI   AUTOUSE,UBSM        SET TO PAY MUSIC SESSION                     
*        TM    TGUNEQU,AFM         IF THIS IS A MUSICIAN                        
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    *+16                                                             
         TM    AUTOOPT,AUTOKBSM    AND USER WANTS MUSIC SESSIONS                
         BZ    NAUT20                                                           
         B     NAUT10              GO PAY IT                                    
         SPACE 1                                                                
         MVC   AUTOUSE,BSRUSE      SET TO PAY RADIO SESSION                     
         MVC   AUTOTYPE,BSRUTYPE                                                
         TM    TGMEEQU,RADIO       IF MEDIA IS RADIO                            
         BZ    *+16                                                             
         TM    AUTOOPT,AUTOKBSR    AND USER WANTS RADIO SESSIONS                
         BZ    NAUT20                                                           
         B     NAUT10              GO PAY IT                                    
         SPACE 1                                                                
         MVC   AUTOUSE,BSSUSE      ELSE SET TO PAY TV SESSION                   
         MVC   AUTOTYPE,BSSUTYPE                                                
         TM    AUTOOPT,AUTOKBSS    IF USER WANTS TV SESSIONS                    
         BO    NAUT10              GO DO IT                                     
         OI    STATUS,DATEONLY     ELSE SET DATES ONLY                          
         SPACE 1                                                                
NAUT10   BAS   RE,AUTO             PAY AUTOMATIC SESSION                        
         NI    STATUS,ALL-DATEONLY                                              
         SPACE 1                                                                
NAUT20   TM    TGUSSTA2,APPREUSE   GET OUT IF USE CAN ADD FTRACKS               
         BO    NAUTX                                                            
         CLI   TGUSEQU,UOTH                          OTHER                      
         BE    NAUTX                                                            
*        TM    TGUSXUNI,AFM                          MUSIC USE                  
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    NAUTX                                                            
         SPACE 1                                                                
*        TM    TGUNEQU,AFM         IF THIS IS A MUSICIAN                        
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BZ    NAUT30                                                           
         TM    AUTOOPT,AUTOKMUS    AND OK TO PAY AUTO MUS REUSE                 
         BZ    NAUTX                                                            
         MVI   AUTOUSE,UMUS        SET TO PAY MUSIC CYCLE                       
         MVI   AUTOTYPE,UMUS13W                                                 
         TM    TGMEEQU,RADIO       IF MEDIA IS RADIO                            
         BZ    *+16                                                             
         CLI   TGUSWKS,8           AND CURRENT CYCLE IS 8 WEEKS                 
         BH    *+8                                                              
         MVI   AUTOTYPE,UMUS8W     THEN GENERATE 8 WEEK MUSIC CYCLE             
         SPACE 1                                                                
NAUT26   CLC   TCPCYCE,LASTMUS     DONE IF THIS CYCLE END DATE COVERED          
         BNH   NAUTX                                                            
         XC    AUTOCYCS,AUTOCYCS   SET TO START CONTIGUOUS TO LAST MUS          
         CLC   TCPCYCS,LASTMUS     IF THIS CYCLE STARTS AFTER LAST MUS          
         BNH   *+10                                                             
         MVC   AUTOCYCS,TCPCYCS    SET START DATE FOR MUSIC CYCLE               
         BAS   RE,AUTO             MAKE AUTOMATIC MUSIC PAYMENT                 
         BE    NAUT26                                                           
         B     NAUTX                                                            
         SPACE 1                                                                
NAUT30   TM    TGMEEQU,LIKETV+CABLE IF MEDIA IS TV,INT,NEW OR CABLE             
         BZ    NAUTX                                                            
         TM    AUTOOPT,AUTOKHLD    AND OK TO PAY AUTO HOLDING FEES              
         BZ    NAUTX                                                            
         MVC   AUTOUSE,HLDUSE      SET TO PAY HOLDING FEE                       
         MVI   AUTOTYPE,0                                                       
         CLC   TCPCYCS,LASTHLD     IF CURRENT CYCLE STARTS AFTER LAST           
         BNH   *+12                HOLDING FEE PAYMENT                          
         BAS   RE,AUTO             MAKE AN AUTOMATIC HLD PAYMENT                
         BE    *-14                AND TRY AGAIN                                
         SPACE 1                                                                
NAUTX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINES CONTROL GENERATING AUTOMATIC PAYMENTS                   
         SPACE 1                                                                
HLDAUTO  NTR1                                                                   
         LA    R3,1                SET PROCESSING ADD'L HLDS AT END             
         B     AUT2                                                             
         SPACE 1                                                                
AUTO     NTR1                                                                   
         XR    R3,R3               SET PROCESSING NORMAL AUTO PAYMENT           
         SPACE 1                                                                
AUT2     MVI   ESTMODE,PROCAUTO    SET AUTO. ESTIMATED PAYMENTS MODE            
         SPACE 1                                                                
         BAS   RE,ESTSAVE          SAVE CURRENT VALUES                          
         BAS   RE,ESTINIT          CLEAR THEM                                   
         SPACE 1                                                                
         MVC   AUSVUSE,TGUSEQU     SAVE CURRENT USE CODE EQUATE                 
         MVC   AUSVTYP,TGUSTYP     AND CURRENT USE TYPE EQUATE                  
         SPACE 1                                                                
         CLC   AUTOUSE,HLDUSE      IF PROCESSING AUTO HOLDING FEES              
         BNE   AUT6                                                             
         DROP  R3                  DROP PREVIOUS USING                          
         L     R1,TCATACO          DON'T PROCESS FOR CERTAIN COMMLS             
         USING TACOD,R1                                                         
         TM    TACOSTAT,TACOSCRT   DON'T WANT CANADIAN RATES                    
         BO    AUT5                                                             
         CLI   TACOTYPE,CTYSEAS    DON'T WANT SEASONAL                          
         BE    AUT5                                                             
         CLI   TACOTYPE,CTYSEAS2                                                
         BE    AUT5                                                             
         L     RE,=A(NAHLDTAB)     RE=A(COMM'L TYPE TAB GET NO HFN)             
         A     RE,RELO                                                          
AUT4     CLI   0(RE),0             TEST END OF TABLE                            
         BE    AUT6                                                             
         CLC   TACOTYPE,0(RE)      TEST MATCH ON COMMERCIAL TYPE                
         BE    AUT5                                                             
         LA    RE,1(RE)            NO-BUMP TO NEXT COMML TYPE IN TAB            
         B     AUT4                                                             
AUT5     LA    R2,1                YES-DON'T PROCESS AUTO HOLDING FEE           
         B     AUTR                                                             
         DROP  R1                                                               
         SPACE 1                                                                
AUT6     GOTO1 USEVAL,DMCB,(X'80',AUTOUSE),AUTOTYPE                             
         MVC   TCPCYCS,AUTOCYCS    SET CYCLE START IF DEFINED                   
         SPACE 1                                                                
         XR    R2,R2                                                            
         BAS   RE,PAYIT            PAY IT                                       
         BE    *+8                                                              
         LA    R2,1                                                             
         SPACE 1                                                                
AUTR     LTR   R3,R3               IF THIS IS ADDL HLD AT END                   
         BNZ   AUTX                DON'T BOTHER WITH RESTORES                   
         SPACE 1                                                                
         BAS   RE,ESTREST          RESTORE CURRENT VALUES                       
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,(X'80',AUSVUSE),AUSVTYP                              
         SPACE 1                                                                
         BAS   RE,SETUSE           RESET USE DETAILS                            
         SPACE 1                                                                
AUTX     MVI   ESTMODE,PROCESTP    RESET ESTIMATED PAYMENTS MODE                
         SPACE 1                                                                
         LTR   R2,R2               RETURN CC BASED ON PAYIT                     
         B     XIT                                                              
         EJECT                                                                  
*              ESTIMATED PAYMENT INITIALIZATION ROUTINES                        
         SPACE 1                                                                
ESTINIT  DS    0H                  INITIALIZE AREAS                             
         XC    MYUSENT(MYUSLNQ),MYUSENT   MY USE ENTRY                          
         XC    TCTOTS(TCTOTLNQ),TCTOTS    ACCUMULATORS                          
         XC    TCUDETS(TCUDTLNQ),TCUDETS  USE DETAILS                           
         XC    TCPAY,TCPAY                PAYMENT AMOUNT OVERRIDE               
         XC    TCNUSES(4),TCNUSES         PREVIOUS N'USES                       
         MVI   TCINPUT,0                  RATE CALC. INPUT STATUS               
         MVI   TCAPPLCD,0                 APPLIED CODE                          
         XC    TCIPCYC,TCIPCYC            INDIVIDUAL CYCLE DATES                
         XC    AHISTENT,AHISTENT          A(USAGE HISTORY TABLE ENTRY)          
         XC    GROSS,GROSS                INTERNAL TOTAL FOR PAYMENT            
         XC    APPLYMTH,APPLYMTH          OVERRIDE APPLY YEAR/MONTH             
         CLI   ESTMODE,PROCPAY            IF NOT ACTUAL PAYMENT MODE            
         BE    *+10                                                             
         XC    CSFAMT,CSFAMT              CLEAR CONTRACT SERV. FEE TOO          
         BR    RE                                                               
         SPACE 3                                                                
ESTSAVE  DS    0H                  SAVE AREAS                                   
         MVC   AUSVMYUS,MYUSENT    MY USE ENTRY                                 
         MVC   AUSVTOTS,TCTOTS     ACCUMULATORS                                 
         MVC   AUSVDETS,TCUDETS    USE DETAILS                                  
         MVC   AUSVPAY,TCPAY       PAYMENT AMOUNT OVERRIDE                      
         MVC   AUSVNUSE,TCNUSES    PREVIOUS N'USES                              
         MVC   AUSVINPT,TCINPUT    RATE CALC. INPUT STATUS                      
         MVC   AUSVAPCD,TCAPPLCD   APPLIED CODE                                 
         MVC   AUSVICYC,TCIPCYC    INDIVIDUAL CYCLE DATES                       
         MVC   AUSVAHST,AHISTENT   A(USAGE HISTORY TABLE ENTRY)                 
         MVC   AUSVCSF,CSFAMT      CONTRACT SERVICE FEE AMOUNT                  
         BR    RE                                                               
         SPACE 3                                                                
ESTREST  DS    0H                  RESTORE AREAS                                
         MVC   MYUSENT(MYUSLNQ),AUSVMYUS  MY USE ENTRY                          
         MVC   TCTOTS(TCTOTLNQ),AUSVTOTS  ACCUMULATORS                          
         MVC   TCUDETS(TCUDTLNQ),AUSVDETS USE DETAILS                           
         MVC   TCPAY,AUSVPAY              PAYMENT AMOUNT OVERRIDE               
         MVC   TCNUSES(4),AUSVNUSE        PREVIOUS N'USES                       
         MVC   TCINPUT,AUSVINPT           RATE CALC. INPUT STATUS               
         MVC   TCAPPLCD,AUSVAPCD          APPLIED CODE                          
         MVC   TCIPCYC,AUSVICYC           INDIVIDUAL CYCLE DATES                
         MVC   AHISTENT,AUSVAHST          A(USAGE HISTORY TABLE ENTRY)          
         XC    GROSS,GROSS                INTERNAL TOTAL FOR PAYMENT            
         MVC   CSFAMT,AUSVCSF             CONTRACT SERVICE FEE AMOUNT           
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO SET AS-OF BLOCK ENTRIES                               
         SPACE 1                                                                
         USING ASOFD,RE                                                         
ASOFSET  NTR1                                                                   
         XR    R1,R1                                                            
         L     RE,ASOFCB           RE=A(AS/OF CONTROL BLOCK)                    
         L     R0,MAXASOF          R0=MAXIMUM NUMBER OF BLOCK ENTRIES           
         SPACE 1                                                                
ASOFS2   CLI   0(RE),0             END OF TABLE                                 
         BE    ASOFS6                                                           
         CLC   ASOFUEQU,TGUSEQU    MATCH ON USE EQUATE                          
         BNE   ASOFS4                                                           
         CLC   TCPCYCS,ASOFDATE    CYCLE START MUST BE GE AS/OF DATE            
         BL    ASOFS4                                                           
         BH    *+16                                                             
         TM    ASOFSTAT,ASOFONLY   DATES EQ-IS ENTRY FOR THIS DATE ONLY         
         BO    ASOFS8              YES - USE IT                                 
         B     *+12                                                             
         TM    ASOFSTAT,ASOFONLY   DATES NOT EQ-IF FOR DATE ONLY, SKIP          
         BO    ASOFS4                                                           
         LTR   R1,R1               IF WE ALREADY HAVE A VALID DATE              
         BZ    *+14                                                             
         CLC   ASOFDATE,ASOFDATE-ASOFD(R1) THIS ONE MUST BE GE PREV.            
         BL    ASOFS4                                                           
         LR    R1,RE               SAVE A(THIS ENTRY)                           
         SPACE 1                                                                
ASOFS4   LA    RE,ASOFNEXT         AND KEEP ON TRYING                           
         BCT   R0,ASOFS2                                                        
         SPACE 1                                                                
ASOFS6   LTR   R1,R1               ANYTHING FOUND                               
         BZ    ASOFSX                                                           
         LR    RE,R1               YES - SET SAVED A(APPLICABLE ENTRY)          
         SPACE 1                                                                
ASOFS8   MVC   MYUSYORN,ASOFYORN   MOVE IN SAVED VALUES                         
         MVC   TCPAY,ASOFAMT                                                    
         MVC   MYUSOVER,ASOFOVER                                                
         OC    MYUSOVER,MYUSOVER   IF NOT OVERSCALE OVERRIDE                    
         BNZ   *+8                                                              
         OI    TCINPUT,TCINPAY     INSURE WE PAY ZERO IF ZERO INPUT             
         SPACE 1                                                                
         CLI   MYUSYORN,C'N'       TEST DON'T PAY THIS USE TYPE                 
         BE    NO                  SO RETURN CC NE                              
         SPACE 1                                                                
         TM    TCINPUT,TCINPRI     IF THIS IS FIXED CYC PMT ON PRI COML         
         BZ    ASOFSX                                                           
         OC    TCPAY,TCPAY         AND PAYMENT AMOUNT SET                       
         BZ    ASOFSX                                                           
         MVC   TGFULL,TCPAY        SET AMOUNT                                   
         TM    TGUSSTA2,APPREUSE   TEST THIS USE CAN ADD FTRACKS                
         BZ    *+12                                                             
         MVI   ELCODE,TAOAELQ      UPDATE OVERSCALE AMT ELEMENT                 
         BRAS  RE,UPDOAOP                                                       
         XC    TCPAY,TCPAY         CLEAR PAYMENT AMOUNT                         
         NI    TCINPUT,ALL-TCINPAY AND PAYMENT OVERRIDE STATUS                  
ASOFSX   B     YES                 ELSE RETURN CC EQ                            
         EJECT                                                                  
*              ROUTINE TO INITIALIZE OVERSCALE FIELDS                           
         SPACE 1                                                                
OVERSET  NTR1                                                                   
         XC    TCOV1,TCOV1         CLEAR OVERSCALE FIELDS                       
         XC    TCOV2,TCOV2                                                      
         XC    TCOVAMT,TCOVAMT                                                  
         NI    TCCASTST,ALL-TCCAOVAM  TURN OFF OVERSCALE AMNT FOUND BIT         
         SPACE 1                                                                
         MVI   DUB+7,0                                                          
         OC    OVERASOF,OVERASOF   IF AS OF DATE DEFINED                        
         BZ    OVERS2                                                           
         CLC   APPLYDTE,OVERASOF   AND APPLY DATE BEFORE AS OF DATE             
         BNL   OVERS2                                                           
         MVI   DUB+7,1             SET NOT APPLYING OVERRIDES                   
         B     *+16                                                             
OVERS2   MVC   TCOV1,OVERSCL       ELSE, SET OVERALL OVSCALE OVERRIDES          
         MVC   TCOV2,OVERSCL2                                                   
         SPACE 1                                                                
         MVC   AIO,TCACAST         GET OV RATES FROM CAST RECORD                
         XC    DUB(4),DUB                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         USING TACAD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   OVERS3                                                           
         GOTO1 SETOV2,DMCB,(R4),AIO,TGUSCDE                                     
         MVC   DUB(L'TACAOV2),TACAOV2                                           
OVERS3   GOTO1 GETOV1,DMCB,TGUSCDE,FULL                                         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   0(R1),X'FF'         IF PAYMENT AMOUNT DEFINED ON CAST            
         BNE   OVERS4                                                           
         TM    TCINPUT,TCINPAY     AND WE DON'T ALREADY HAVE PAY AMOUNT         
         BO    *+14                                                             
         MVC   TCPAY,FULL          SET PAY AMOUNT FROM CAST REC.                
         OI    TCINPUT,TCINPAY+TCINOVSC                                         
         B     OVERS6                                                           
         SPACE 1                                                                
OVERS4   CLC   TGUSCDE,=C'HLD'     IF LOOKING FOR HLD                           
         BNE   OVERS5                                                           
         TM    OTHOPT,OTHHLD       AND HOLDING FEE AMT OR PCT ESTIMATED         
         BZ    OVERS5                                                           
         MVC   TCOV1,FULL          USE IT                                       
         XC    TCOV2,TCOV2         AND CLEAR OV2                                
         B     OVERS6                                                           
         SPACE 1                                                                
OVERS5   TM    OTHOPT,OTHOV1       ELSE IF OVSCAL RATE OVERRIDE NOT DEF         
         BZ    *+12                                                             
         CLI   DUB+7,1             OR IF DEFINED, BUT NOT USED                  
         BNE   *+10                                                             
         MVC   TCOV1,FULL          SET OVERSCALE RATES FROM CAST RECORD         
         TM    OTHOPT,OTHOV2                                                    
         BZ    *+12                                                             
         CLI   DUB+7,1                                                          
         BNE   *+10                                                             
         MVC   TCOV2,DUB                                                        
         SPACE 1                                                                
OVERS6   OC    MYUSOVER,MYUSOVER   IF OVSCALE OVERRIDE FOR USE DEFINED          
         BZ    *+10                                                             
         MVC   TCOV1,MYUSOVER      SET IT                                       
         SPACE 1                                                                
         TM    TGUSSTA2,APPREUSE   IF THIS USE CAN ADD FTRACKS                  
         BZ    OVERSX                                                           
         TM    TCINPUT,TCINPAY     AND PAYMENT AMOUNT IS ALREADY SET            
         BZ    OVERSX                                                           
         MVC   TCOVAMT,TCPAY       THEN SET OVERSCALE AMOUNT                    
         OI    TCCASTST,TCCAOVAM   AND TURN ON OVERSCALE AMNT FOUND BIT         
OVERSX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF HLD PAYMENT COVERED BY DLR CYCLE           
         SPACE 1                                                                
CHKDLR   NTR1                                                                   
         OI    TCOPTS,TCNOCKDL     SET RATE CALC. TO NOT CHECK THIS             
         SPACE 1                                                                
         CLI   DLROPT,C'Y'         OPTION TO PAY HLDS IN DLR CYCLE              
         BE    CDLRX                                                            
         MVI   BYTE,C'N'           SET WE HAVEN'T FOUND A DLR ENTRY             
         SPACE 1                                                                
         L     R2,HISTTAB          R2=A(USAGE HISTORY TABLE)                    
         USING HISTD,R2                                                         
CDLR2    OC    0(HISTLNQ,R2),0(R2) TEST END OF TABLE                            
         BNZ   CDLR4                                                            
         OC    HISTDLR,HISTDLR     TEST ACTUAL HIST CYCLE                       
         BZ    *+12                                                             
         LA    R2,HISTDLR          PROCESS IT                                   
         B     CDLR10                                                           
*                                                                               
         CLI   BYTE,C'Y'           IF WE SAW A DLR ENTRY THEN GET OUT           
         BE    CDLRX                                                            
         ZIC   R0,TGUSEQU          ELSE SAVE CURRENT USE CODE                   
         MVI   HALF,UDLR           AND SET TO LOOK UP DLR VALUES                
         MVI   HALF+1,0                                                         
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1  SET DLR GLOBAL VALUES           
         BAS   RE,BLDHIST          LOOK UP DLR HISTORY                          
         MVI   HISTUSE,UDLR        SET WE HAVE TABLE ENTRY FOR DLR              
         STC   R0,HALF             RESTORE ORIGINAL USE CODE                    
         MVI   HALF+1,0                                                         
         GOTO1 USEVAL,DMCB,(X'80',HALF),HALF+1  RESTORE HLD VALUES              
         SPACE 1                                                                
CDLR4    CLI   HISTUSE,UDLR        LOOK FOR DLR USE TYPE                        
         BE    CDLR8                                                            
CDLR6    LA    R1,HISTDLR                                                       
         CR    R2,R1               IF ALREADY CHECKED HISTDLR                   
         BE    CDLRX               THEN DONE                                    
         LA    R2,HISTNEXT                                                      
         B     CDLR2                                                            
*                                                                               
CDLR8    MVI   BYTE,C'Y'           SET WE'VE SEEN A DLR CYCLE                   
CDLR10   OC    HISTCYC,HISTCYC     DON'T BOTHER IF DUMMY ENTRY                  
         BZ    CDLR6                                                            
         CLC   HISTCYCS,TCPCYCS    MUST START BEFORE HLD                        
         BNL   CDLR6                                                            
         CLC   HISTCYCE,TCPCYCS    MUST END ON/AFTER HLD STARTS                 
         BL    CDLR6                                                            
         SPACE 1                                                                
         ICM   R3,7,TCPCYCS        SAVE HLD CYCLE DATES                         
         ICM   R4,7,TCPCYCE                                                     
         MVC   TCPCYCS,HISTCYCS    SET DLR START                                
         XC    TCPCYCE,TCPCYCE     AND CLEAR END DATE                           
         SPACE 1                                                                
         GOTO1 CYCEND,DMCB,0       CALC HLD END DATE W/O PVIN1DYL BASED         
         MVC   WORK(3),TCPCYCE     ON DLR.  WORK=CALCULATED END DATE            
         SPACE 1                                                                
         STCM  R3,7,TCPCYCS        RESTORE HLD CYCLE DATES                      
         STCM  R4,7,TCPCYCE                                                     
         SPACE 1                                                                
         CLC   TCPCYCS,WORK        IF THIS HLD STARTS AFTER CALC. DATE          
         BH    CDLRX               PAY HOLDING FEE NORMALLY                     
         SPACE 1                                                                
         TM    TCINPUT,TCINPAY+TCINOVSC+TCINPRI IF PAYMENT FROM OVSC &          
         BNO   *+12                FIXED CYCLE PMT ON PRIMARY COMML             
         OI    TCCASTST,TCCAAPP0   SET TO MAKE PAYMENT BUT APPLY ZERO           
         B     CDLRX                                                            
         SPACE 1                                                                
         XC    TCPAY,TCPAY         ELSE SET PAYMENT AMOUNT TO ZERO              
         OI    TCINPUT,TCINPAY     SET WE HAVE PAY AMOUNT                       
CDLRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POP IN A BOTTOM AT END OF REPORT                      
         SPACE 1                                                                
BXBOT    NTR1                                                                   
         CLI   BOXOPT,C'N'         GET OUT IF BOXES DISABLED                    
         BE    XIT                                                              
         CLC   LINE,MAXLINES       OR IF LAST LINE                              
         BE    XIT                                                              
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'B'                                                       
         MVI   BOXINIT,0                                                        
         BRAS  RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 3                                                                
         EJECT                                                                  
*              ERRORS, CONSTANTS, ETC.                                          
         SPACE 1                                                                
REPSPOOL TM    PRGSTAT,EASY32      IF CONNECTED THROUGH EASYEST32               
         BO    REPSPL5             JUST DISPLAY REPORT MESSAGE                  
         L     R3,ATWA             ELSE, GO STRAIGHT TO $DQU                    
         USING T702FFD,R3                                                       
         LA    R2,CONRECH                                                       
         XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
REPSPL5  L     R3,AREPBLK                                                       
         USING FAREPBLKD,R3                                                     
         MVI   BLOCK,4                                                          
         MVC   BLOCK+1(3),REPSUBID SET REPORT SUB ID,REPORT NUMBER              
         MVI   BLOCK+4,6                                                        
         EDIT  REPREPNO,(5,BLOCK+5),ALIGN=LEFT                                  
         MVI   BLOCK+10,0                                                       
         OI    GENSTAT2,USGETTXT   SET CALL FOR GETTXT                          
         MVI   MYMTYP,GTMINF       INFORMATION                                  
         MVI   MYMSGNO1,241        MESSAGE NUMBER                               
         B     THEEEND                                                          
         SPACE 1                                                                
NOTNOW   L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         LA    R2,CONWHENH         REQUEST TOO BIG FOR ONLINE PROC.             
         MVI   ERROR,NOTONLIN                                                   
THEEEND  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
MAXRECLQ EQU   6000                MAXIMUM SIZE OF ESTIMATE RECORD              
RELO     DS    F                   RELOCATION FACTOR                            
HEXFFS   DC    6X'FF'                                                           
IOBYTE   DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RA,R6,R5            DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*---------------------------------------------------------------------          
* E-MAIL ERROR REPORT                                                           
*---------------------------------------------------------------------          
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
         CLI   SENTEML,C'Y'                                                     
         JE    XIT                                                              
         MVI   SENTEML,C'Y'                                                     
*                                                                               
         MVC   SBJDSCAY,TGAGY                                                   
         MVC   SBJDSCES,TGEST                                                   
         MVC   SBJDSCST,TGSTAF                                                  
         OC    SBJDSCST,SPACES                                                  
*                                                                               
         GOTO1 SQUASHER,DMCB,NOPROD,NOPRODLN   SQUASH IT                        
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(NOPRODLN),NOPROD)                   
         J     XIT                                                              
*                                                                               
NOPROD   DC    C'AUTONOTE*US-TALENT_MF_DEV_TEAM@MEDIAOCEAN.COM:'                
         DC    C'Trap Estimate '                                                
SBJDSCAY DC    C'123456'                                                        
         DC    C' / '                                                           
SBJDSCES DC    C'12345678901234567890'                                          
         DC    C' by '                                                          
SBJDSCST DC    C'12345678'                                                      
NOPRODLN EQU   *-NOPROD                                                         
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1  BASE=*,LABEL=*                                                   
         TM    WHEN,X'80'          DON'T BOTHER IF ON SCREEN                    
         JO    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         LTORG                                                                  
*----------------------------------------------------------------------         
*        SETUP FOR HST CALCULATIONS                                             
*           0(R1) = X'80', SEARCH FOR HIGHEST PST RATE                          
*           P1 = ADDRESS OF PROVINCE                                            
*----------------------------------------------------------------------         
GETPSTR  NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)               CANADIAN PROVINCE                         
         MVC   TGGSTRAT,=AL4(GSTRT)   DEFAULT 5.00% GST                         
         NI    TGPRVST,X'FF'-TGPSTHST-TGPSTQUE                                  
         XC    TGPSTRAT,TGPSTRAT      NO PST                                    
         MVI   GETPSTHI,C'N'                                                    
         TM    0(R1),X'80'            SEARCH FOR HIGHEST PST                    
         BZ    GETP050                                                          
         MVI   GETPSTHI,C'Y'          YES                                       
*                                                                               
GETP050  MVC   AIO,AIO2               GET SYSTEM RECORD FOR CAN PROV            
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'20',0)                                    
         BE    *+6                    RATE                                      
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO2                                                          
         USING TAPCD,R4                                                         
         MVI   ELCODE,TAPCELQ         GET CANADIAN PROVINCE ELEMENT             
         BRAS  RE,GETEL                                                         
         BNE   GETPX                                                            
         MVC   TGGSTRAT,TAPCGSTR      GET GST RATE                              
*                                                                               
         CLI   GETPSTHI,C'Y'          SEARCH FOR HIGHEST PST RATE               
         BE    GETP080                YES, DON'T CHECK ON PROVINCE              
         CLC   0(3,R5),=C'   '        ANY PROVINCE?                             
         BNH   GETPX                  NO, LEAVE                                 
*                                                                               
GETP080  ZIC   R1,TAPCNSUB                                                      
         LA    R3,TAPCPROV                                                      
E1       USING TAPCPROV,R3                                                      
GETP100  DS    0H                                                               
         CLI   GETPSTHI,C'Y'                                                    
         BE    GETP180                                                          
         CLC   E1.TAPCPROV,0(R5)      MATCH ON PROVINCE                         
         BE    GETP200                                                          
GETP150  AHI   R3,TAPCSLNQ            BUMP TO NEXT PROVINCE                     
         BCT   R1,GETP100                                                       
         B     GETPX                  LEAVE, PROVINCE NOT IN TABLE              
*                                                                               
GETP180  DS    0H                                                               
         CLC   TGPSTRAT,E1.TAPCPSTR   COMPARE PST RATES                         
         BNL   GETP150                EQUAL OR HIGHER ALREADY, SKIP             
         MVC   TGPRVST,E1.TAPCSTAT    SET HST INDICATOR                         
         MVC   TGPSTRAT,E1.TAPCPSTR   SET PST RATE                              
         CLC   =C'QC ',E1.TAPCPROV    QUEBEC                                    
         BE    GETP190                                                          
         CLC   =C'PE ',E1.TAPCPROV    OR PRINCE EDWARD ISLAND                   
         BNE   GETP150                                                          
GETP190  OI    TGPRVST,TGPSTQUE       USE QUEBEC CALCULATION                    
         B     GETP150                                                          
*                                                                               
GETP200  MVC   TGPRVST,E1.TAPCSTAT    SET HST INDICATOR                         
         TM    TGPRVST,TGPSTHST       FOR NOW, ONLY CALC PST WHEN HST           
         BZ    GETPX                                                            
         MVC   TGPSTRAT,E1.TAPCPSTR   SET PST RATE                              
         CLC   0(3,R5),=C'QC '        QUEBEC                                    
         BE    GETP210                                                          
         CLC   0(3,R5),=C'PE '        OR PRINCE EDWARD ISLAND                   
         BNE   GETPX                                                            
GETP210  OI    TGPRVST,TGPSTQUE       USE QUEBEC CALCULATION                    
*                                                                               
GETPX    XIT1                                                                   
         DROP  R4                                                               
GETPSTHI DC    C'N'                   GET HIGHEST PST RATE                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE GETS CLIENT INFORMATION                                        
***********************************************************************         
*                                                                               
CLIADDL  NTR1  BASE=*,LABEL=*                                                   
         NI    CSFSTAT,X'FF'-CSFCLIY-CSFCLIN   TURN OFF CLI CSF STATUS          
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0  GET NAME                                 
         MVC   CLINAME,TGNAME          AND SAVE IN W/S                          
*                                                                               
         MVC   PROFILES,AGYPROFS   SET PROFILES = AGENCY LVL                    
         MVC   DEFAULTS(DEFLNQ),AYVALS                                          
         MVC   OVERIDES(DEFLNQ),AYVALS                                          
*                                                                               
         L     R4,AIO              GET CLIENT INFORMATION ELEMENT               
         USING TACID,R4                                                         
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CLIAD20                                                          
         TM    TACISTAT,TACIJPCY   CAN CHARGE JPC CSF?                          
         BZ    *+8                                                              
         OI    CSFSTAT,CSFCLIY     CLIENT CSF YES                               
         TM    TACISTA2,TACIJPCN   DON'T CHARGE JPC CSF?                        
         BZ    *+8                                                              
         OI    CSFSTAT,CSFCLIN     CLIENT CSF NO                                
         DROP  R4                                                               
*                                                                               
CLIAD20  MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         B     CLIAD25                                                          
CLIAD23  BRAS  RE,NEXTEL                                                        
CLIAD25  BNE   CLIAD80                                                          
         USING TABRD,R4                                                         
         TM    TABRSTAT,TABRSACP                                                
         BO    CLIAD23                                                          
         OC    DFBRSTAT,TABRSTAT   OR IN CLIENT STATUS BYTE                     
         CLI   TABRHRLS,0                                                       
         BE    *+10                                                             
         MVC   PROFHRUL,TABRHRLS                                                
*                                                                               
         OC    TABROEOR,TABROEOR   IF EMPLOYER OVERRIDE DEFINED,                
         BZ    *+10                                                             
         MVC   TGEMP,TABROEOR      MOVE TO GLOBAL STORAGE                       
*                                                                               
         CLI   TABRTYPE,0          IF BILLING TYPE DEFINED                      
         BE    CLIAD80                                                          
         GOTO1 BTYPVAL,DMCB,TABRTYPE  LOOK UP STANDARD RATE CARD                
*                                                                               
         MVC   PROFBTYP,TGBTYPE    SAVE BILLING TYPE                            
*                                                                               
         OC    TABRRATE,TABRRATE   IF RATE OVERRIDES DEFINED IN ELEMENT         
         BZ    *+10                                                             
         MVC   TGBSRC,TABRRATE     MOVE THEM GLOBAL STORAGE                     
*                                                                               
         BAS   RE,ADDLBR                                                        
*                                                                               
         CLI   PROFBTYP,TABRTY1    IF THIS IS TYPE 1                            
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY2            OR TYPE 2                            
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY7            OR TYPE 7                            
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY8            OR TYPE 8                            
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY18           OR TYPE 18                           
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY20           OR TYPE 20                           
         BE    CLIAD40                                                          
         CLI   PROFBTYP,TABRTY11           OR TYPE 11                           
         BNE   *+14                                                             
         XC    DFTNHRT,DFTNHRT         (TYPE 11 HAS NO TAX AMOUNT)              
         B     CLIAD50                                                          
         CLI   PROFBTYP,TABRTY21           OR TYPE 21                           
         BE    CLIAD30                 (USES SUTA FOR SESS. HANDLING)           
         CLI   PROFBTYP,TABRTY23           OR TYPE 23                           
         BE    CLIAD30                 (USES SUTA FOR SESS. HANDLING)           
         CLI   PROFBTYP,TABRTY6            OR TYPE 6                            
         BNE   CLIAD60                                                          
CLIAD30  OC    TGBSUTA,TGBSUTA         (TYPE 6 USES SUTA RATE TOO)              
         BZ    *+10                                                             
         MVC   DFTNHRT2+2(2),TGBSUTA                                            
CLIAD40  OC    TGBFUTA,TGBFUTA         AND WE HAVE HIGHEST T&H RATE             
         BZ    *+10                                                             
         MVC   DFTNHRT+2(2),TGBFUTA    USE IT                                   
CLIAD50  OC    TGBHAND,TGBHAND         IF WE HAVE HANDLING RATE                 
         BZ    *+10                                                             
         MVC   DFCORPRT+2(2),TGBHAND   USE IT                                   
         B     CLIAD80                                                          
*                                                                               
CLIAD60  CLI   PROFBTYP,TABRTY12     IF BILLING TYPE 12                         
         BE    CLIAD61                                                          
         CLI   PROFBTYP,TABRTY22     OR BILLING TYPE 22                         
         BE    CLIAD61                                                          
         CLI   PROFBTYP,TABRTY25     OR BILLING TYPE 25                         
         BE    CLIAD61                                                          
         CLI   PROFBTYP,TABRTY26     OR BILLING TYPE 26                         
         BNE   CLIAD69                                                          
CLIAD61  MVC   DFTNHRT+2(2),TGBFUTA  SET FUTA & HANDLING SEPERATELY             
         MVC   DFTNHRT2+2(2),TGBHAND                                            
         B     CLIAD70                                                          
*                                                                               
CLIAD69  LH    R1,TGBFUTA          ELSE TAKE HIGHEST T&H RATE                   
         AH    R1,TGBHAND          + HANDLING RATE                              
         BZ    *+8                                                              
         STH   R1,DFTNHRT+2        SAVE NEW T&H RATE                            
CLIAD70  OC    TGBCORP,TGBCORP     IF WE HAVE CORP RATE                         
         BZ    *+10                                                             
         MVC   DFCORPRT+2(2),TGBCORP   USE IT                                   
*                                                                               
CLIAD80  DS    0H                                                               
*LIAD80  OC    TGBCAN,TGBCAN       IF WE HAVE CANADIAN HANDLING RATE            
*        BZ    *+10                                                             
*        MVC   DFCANTAX+2(2),TGBCAN                                             
*                                                                               
CLIAD90  GOTO1 RECVAL,DMCB,TLCTCDQ,(X'20',0)  IF HAVE CNTL REC FOR CLI          
         BNE   CLIAD100                                                         
         L     R4,AIO                                                           
         USING TLCTD,R4                                                         
         OC    TLCTCLI,TLCTCLI     INSURE THIS IS CLIENT LEVEL RECORD           
         BZ    CLIAD100                                                         
**NO-OP**GOTO1 =A(MYTRACE),DMCB,=C'CNTRL',RR=RELO                               
*                                                                               
         MVI   ELCODE,TAEPELQ      GET ESTIMATE PROFILE ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   CLIAD100                                                         
         USING TAEPD,R4            SET OPTIONS                                  
         MVC   PROFFILT,TAEPFILT   ACTUALS FILTER BASIS                         
         MVC   PROFCOMM,TAEPCOMM   COMMISSION                                   
         MVC   PROFMTH,TAEPMTH     START FISCAL MONTH                           
         MVC   PROFEXP,TAEPEXP     EXPIRY EXTEND                                
         MVC   PROFMUS,TAEPMUS     SEPARATE MUSIC                               
         MVC   PROFPAGE,TAEPPAGE   NEW PAGE EACH COMMERCIAL                     
         MVC   PROFSUPP,TAEPSUPP   SUPPRESS INACTIVE COMMERCIALS                
         MVC   PROFREL,TAEPREL     ESTIMATE ON RELEASED COMMERCIALS             
         MVC   PROFTNH,TAEPTNH     TNH RATE FOR CORPS                           
         MVC   PROFHNW,TAEPHNW     INCLUDE H&W WITH TALENT NET                  
         MVC   PROFTNHC,TAEPTNHC   FORCE T&H COMBINED                           
         MVC   DFCOMM+2(2),TAEPRATE  COMMISSION RATE                            
*                                                                               
CLIAD100 CLI   RECAPOPT,TAEPRWC    IF RECAP IS BY WORK-CODE                     
         BNE   CLIADX                                                           
         XC    IFEL,IFEL           ASSUME PROD CLIENT SAME AS TALENT            
         MVC   TGPCLI(L'TGCLI),TGCLI                                            
         OC    TGPCLI,SPACES                                                    
         GOTO1 RECVAL,DMCB,TLIFCDQ,(X'20',0) AND READ INTERFACE REC             
         BNE   CLIADX                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAIFELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   CLIADX                                                           
*                                                                               
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFEL(0),0(R4)       SAVE CLIENT LEVEL INTERFACE ELEMENT          
*                                                                               
CLIADX   XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------          
*              SET ADDITIONAL BILLING RULES FOR OFFICE O                        
*---------------------------------------------------------------------          
ADDLBR   NTR1                                                                   
         L     R4,AIO              GET BILLING RULES ELEMENT                    
         USING TABRD,R4                                                         
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         B     ADDLBR7                                                          
ADDLBR5  BRAS  RE,NEXTEL                                                        
ADDLBR7  BNE   ADDLBR8                                                          
         TM    TABRSTAT,TABRSACP   SPECIAL ACOPY BILLING RULES                  
         BZ    ADDLBR5                                                          
*                                                                               
         MVC   PROFBTYP,TABRTYPE   SAVE BILLING TYPE                            
         MVC   ADRBTYPE,TABRTYPE                                                
         MVC   ADRFUTA(L'TABRRATE),TABRFUTA   BILLING RATES                     
*                                                                               
ADDLBR8  OC    ADRFUTA(L'TABRRATE),ADRFUTA  ANY RATES TO ACCUMULATE?            
         BZ    ADDLBRX                      NO, LEAVE                           
         LA    RE,TMRATEN          N'BILLING RATES                              
         LA    R3,TGBSRC                                                        
         LA    R4,ADRFUTA                                                       
         XR    R1,R1                                                            
         XR    RF,RF                                                            
*                                                                               
ADDLBR9  ICM   RF,3,0(R3)          ACCUMULATE THE RATES                         
         ICM   R1,3,0(R4)                                                       
         AR    RF,R1                                                            
         STCM  RF,3,0(R3)                                                       
                                                                                
         AHI   R3,2                BUMP TO NEXT RATES                           
         AHI   R4,2                                                             
         BCT   RE,ADDLBR9                                                       
ADDLBRX  XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
***********************************************************************         
*        ROUTINE AUTOMATICALLY ADDS PER CYCLE COMMERCIALS TO          *         
*        THE ESTIMATE                                                 *         
***********************************************************************         
                                                                                
ADPCCOMS NTR1  BASE=*,LABEL=*                                                   
         TM    AUTOOPT,AUTOKHLD    IF USER WANTS AUTOMATIC HOLDING              
         BZ    APCCX               FEE PAYMENTS                                 
                                                                                
         USING APCD,R2                                                          
         LA    R2,BLOCK            INITIALIZE ROUTINE'S WORKING STORAGE         
         XC    APCD(APCLNQ),APCD                                                
         MVC   APCOIO,AIO          SAVE ORIGINAL VALUE OF AIO                   
                                                                                
         LA    R3,KEY              R3=A(KEY)                                    
         MVC   AIO,AIO3            PREPARE TO USE AIO3 FOR I/O                  
                                                                                
         USING TLESD,R4                                                         
         L     R4,AESTREC          R4=A(COMBINED ESTIMATE RECORD)               
         MVC   APCAGY,TLESAGY                                                   
         LA    R4,TLESELEM                                                      
         DROP  R4                                                               
                                                                                
         USING TAESD,R4                                                         
APC10    CLI   0(R4),0                                                          
         BE    APCCX                                                            
         CLI   0(R4),TAESELQ       READ ALL ESTIMATE ELEMENTS                   
         BNE   APC20                                                            
                                                                                
         CLI   TAESTYPE,TAESTCOM   IF COMMERCIAL TYPE ELEMENT                   
         BE    APC30               GO PROCESS IT                                
                                                                                
APC20    ZIC   RE,1(R4)            ELSE, GO GET NEXT ESTIMATE                   
         AR    R4,RE               ELEMENT                                      
         B     APC10                                                            
                                                                                
         USING TLCAPD,R3                                                        
APC30    XC    KEY,KEY             SEE IF COMMERCIAL IS ELIGIBLE                
         MVI   TLCAPCD,TLCAHCDQ    FOR HOLDING FEES                             
         MVC   TLCAHCOM,TAESCOM                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLCAHDTE-TLCAPD),KEYSAVE                                     
         BNE   APC20                                                            
         ST    R4,APCAEL           SAVE A(ELEMENT)                              
         DROP  R3,R4                                                            
                                                                                
APC40    GOTO1 SEQ                 READ ALL ATTACHED CAST RECORDS               
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   APC90                                                            
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO              R4=A(CAST RECORD)                            
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   APC40                                                            
         OC    TACAGUA,TACAGUA     IF CAST IS ON GUARANTEE ...                  
         BZ    APC40                                                            
         MVC   APCSVCA,KEY         SAVE CAST KEY                                
                                                                                
         USING TLGUD,R3                                                         
         XC    KEY,KEY             READ GUARANTEE RECORD                        
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,APCSVCA+TLCAHSSN-TLCAPD                                  
         MVC   TLGUGUA,TACAGUA                                                  
         XC    TLGUGUA,APCCHXFF                                                 
         GOTO1 HIGH                                                             
         CLC   TLGUKEY,KEYSAVE                                                  
         BNE   APC80                                                            
         GOTO1 GETREC                                                           
         DROP  R3,R4                                                            
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   APC80                                                            
         OC    TAGUCOM,TAGUCOM     IF THIS IS A PER CYCLE GUARANTEE ...         
         BZ    APC80                                                            
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY             READ THE PRIMARY COMMERCIAL                  
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TAGUCOM                                                 
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         BNE   APC80                                                            
         GOTO1 GETREC                                                           
         DROP  R3,R4                                                            
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         CLC   APCAGY,TLCOAGY      IF AGENCY MATCHES THE ESTIMATE ...           
         BNE   APC80                                                            
         MVC   APCCID,TLCOCID      SAVE COMMERCIAL ID                           
         MVC   APCCOM,TLCOCOM      AND INTERNAL COMMERCIAL NUMBER               
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         BNE   APC80                                                            
         TM    TACOSTA2,TACOPCYC   IF THIS IS A PER CYCLE COMM'L ...            
         BZ    APC80                                                            
         DROP  R4                                                               
                                                                                
         USING TLESD,R4                                                         
         L     R4,AESTREC          R4=A(COMBINED ESTIMATE RECORD)               
         LA    R4,TLESELEM                                                      
         DROP  R4                                                               
                                                                                
         USING TAESD,R4                                                         
APC50    CLI   0(R4),0                                                          
         BE    APC70                                                            
         CLI   0(R4),TAESELQ       READ ALL ESTIMATE ELEMENTS                   
         BNE   APC60                                                            
         CLI   TAESTYPE,TAESTCOM   IF COMMERCIAL TYPE ELEMENT                   
         BNE   APC60               ALREADY EXISTS FOR THE PER CYCLE             
         CLC   APCCOM,TAESCOM      COMMERCIAL, NO NEED TO ADD IT                
         BE    APC80                                                            
APC60    ZIC   RE,1(R4)            ELSE, GO GET NEXT ESTIMATE                   
         AR    R4,RE               ELEMENT                                      
         B     APC50                                                            
         DROP  R4                                                               
                                                                                
APC70    MVC   AIO,AESTREC                                                      
                                                                                
         USING TAESD,R4                                                         
         LA    R4,ELEMENT          ADD ESTIMATE ELEMENT FOR                     
         XC    ELEMENT,ELEMENT     THE PER CYCLE COMMERCIAL                     
         MVI   TAESEL,TAESELQ                                                   
         MVI   TAESLEN,TAESLNQ                                                  
         MVI   TAESTYPE,TAESTCOM                                                
         MVC   TAESCID,APCCID                                                   
         MVC   TAESCOM,APCCOM                                                   
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         MVC   AIO,AIO3                                                         
                                                                                
APC80    MVC   KEY,APCSVCA         RESTORE CAST READ SEQUENCE                   
         GOTO1 HIGH                                                             
         B     APC40                                                            
                                                                                
APC90    L     R4,APCAEL           RESET R4 TO THE LAST READ ESTIMATE           
         B     APC20               ELEMENT, AND GO READ THE NEXT                
                                                                                
APC100   MVC   AIO,APCOIO          RESTORE I/O AREA                             
         DROP  R2                                                               
                                                                                
APCCX    XIT1                                                                   
                                                                                
APCCHXFF DC    6X'FF'                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE LOOKS FOR VERSION ELEMENTS AND SAVES AS LIFT         *         
***********************************************************************         
                                                                                
COMADVER NTR1  BASE=*,LABEL=*                                                   
         OC    ELTALF,ELTALF       IF LIFT ELEMENT NOT ALREADY FOUND            
         BNZ   CAVX                                                             
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CAV10    BRAS  RE,NEXTEL                                                        
         BNE   CAVX                                                             
                                                                                
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   CAV20               IS FOUND                                     
         MVI   TGVER,2             SAVE VERSION NUMBER                          
                                                                                
         USING TALFD,RE                                                         
         LA    RE,ELTALF           AND SAVE THE VERSION 2 INFO                  
         MVI   TALFEL,TALFELQ      AS A LIFT ELEMENT                            
         MVI   TALFLEN,TALFLNQ                                                  
         MVC   TALFLID,TAVRCID                                                  
         MVC   TALFSEC,TAVRSEC                                                  
                                                                                
CAV20    CLI   TAVRVERS,3          IF ELEMENT FOR VERSION 3                     
         BNE   CAV10               IS FOUND                                     
         MVI   TGVER,3             SAVE VERSION NUMBER                          
                                                                                
         LA    RE,ELTALF2          AND SAVE THE VERSION 2 INFO                  
         MVI   TALFEL,TAL2ELQ      AS A LIFT ELEMENT                            
         MVI   TALFLEN,TALFLNQ                                                  
         MVC   TALFLID,TAVRCID                                                  
         MVC   TALFSEC,TAVRSEC                                                  
         B     CAV10                                                            
                                                                                
CAVX     XIT1                                                                   
         DROP  RE,R4                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSLATES CAST-LEVEL VERSION INFORMATION INTO LIFT  *         
***********************************************************************         
                                                                                
GETCVER  NTR1  BASE=*,LABEL=*                                                   
         CLI   TGVER,0             IF TREATING COMMERCIAL WITH                  
         BE    GCVX                VERSIONS LIKE COMMERCIAL WITH LIFT           
                                                                                
         MVI   TCCASTAT,0                                                       
         MVI   TCCASTA4,0                                                       
                                                                                
         MVC   TGFULL,AIO                                                       
         MVC   AIO,TCACAST                                                      
         BRAS  RE,TRNTATR                                                       
         BNE   GCV10                                                            
         MVC   KEY,CASTKEY                                                      
         GOTO1 HIGH                                                             
GCV10    MVC   AIO,TGFULL                                                       
                                                                                
         USING TAFND,R4                                                         
         L     R4,TCACAST                                                       
         MVI   ELCODE,TAFNELQ      GET CAST'S VERSION ELEMENTS                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GCV20    BRAS  RE,NEXTEL                                                        
         BNE   GCV45                                                            
         CLI   TAFNTYPE,TAFNTVER                                                
         BNE   GCV20                                                            
                                                                                
         CLI   TAFNNAME,251        IF CAST IS ON ALL VERSIONS                   
         BNE   *+12                                                             
         OI    TCCASTA4,TACASALL                                                
         B     GCVX                CAST IS ON MASTER AND LIFT                   
                                                                                
         MVI   TGBYTE,0                                                         
                                                                                
         ZIC   R3,TAFNLEN                                                       
         SHI   R3,3                                                             
         LA    RF,TAFNNAME                                                      
GCV30    CLI   0(RF),1                                                          
         BNE   *+12                                                             
         OI    TGBYTE,MASTERQ      ON MASTER                                    
         B     GCV40                                                            
         CLI   0(RF),2                                                          
         BNE   *+12                                                             
         OI    TGBYTE,FIRSTQ       ON 1ST LIFT                                  
         B     GCV40                                                            
         CLI   0(RF),3                                                          
         BNE   *+8                                                              
         OI    TGBYTE,SECONDQ      ON 2ND LIFT                                  
GCV40    AHI   RF,1                                                             
         BCT   R3,GCV30                                                         
                                                                                
GCV45    TM    TGBYTE,MASTERQ+FIRSTQ+SECONDQ ON MASTER+1ST+2ND LIFT?            
         BNO   *+12                                                             
         MVI   TCCASTA4,TACASALL   ON MASTER AND ALL LIFTS                      
         B     GCVX                                                             
                                                                                
         TM    TGBYTE,MASTERQ      ON MASTER?                                   
         BZ    GCV50                                                            
         TM    TGBYTE,FIRSTQ       AND 1ST LIFT?                                
         BZ    *+12                                                             
         MVI   TCCASTAT,TACASTLF   ON MASTER + 1ST LIFT                         
         B     GCVX                                                             
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   TCCASTA4,TACAS2LF   ON MASTER + 2ND LIFT                         
         B     GCVX                                                             
                                                                                
GCV50    TM    TGBYTE,FIRSTQ       ON 1ST LIFT?                                 
         BZ    GCV60                                                            
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   TCCASTA4,TACASL2O   ON BOTH                                      
         B     *+8                                                              
         MVI   TCCASTAT,TACASTLO+TACASTLF   ONLY ON 1ST LIFT                    
         B     GCVX                                                             
                                                                                
GCV60    TM    TGBYTE,SECONDQ      ON 2ND LIFT?                                 
         BZ    *+8                                                              
         OI    TCCASTA4,TACAST2O   ON 2ND LIFTONLY                              
                                                                                
GCVX     XIT1                                                                   
                                                                                
MASTERQ  EQU   X'80'               ON MASTER                                    
FIRSTQ   EQU   X'40'               ON 1ST LIFT                                  
SECONDQ  EQU   X'20'               ON 2ND LIFT                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET AS-OF HOLDING FEE OVERSCALE                       
         SPACE 1                                                                
HLDSET   NTR1  BASE=*,LABEL=*                                                   
         TM    TGUSSTA2,HLDTYPE    IF PAYING HLD TYPE PAYMENT                   
         BZ    HLDSETX                                                          
         OC    HLDASOF,HLDASOF     AND AS OF DATE DEFINED                       
         BZ    HLDSETX                                                          
         CLC   APPLYDTE,HLDASOF    AND IF APPLY DATE NOT BEFORE IT              
         BL    HLDSETX                                                          
         MVC   TGFULL,HLDASAMT     SET HOLDING FEE AMT OR RATE                  
         TM    TGFULL,X'80'        IF HLDAMT IS AMT                             
         BZ    *+12                                                             
         MVI   ELCODE,TAOAELQ      CHANGE CAST OVERSCALE AMT                    
         B     *+8                                                              
         MVI   ELCODE,TAOPELQ      ELSE CHANGE CAST OVERSCALE PCT               
         MVI   TGFULL,0            CLEAR FLAG                                   
         BRAS  RE,UPDOAOP                                                       
         BRAS  RE,CLROAOP                                                       
         OI    OTHOPT,OTHHLD       SET HLD OVERRIDE                             
HLDSETX  XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE UPDATES OVERSCALE AMOUNT EL. WHEN PAYMENT AMOUNT         
*              OVERRIDES ARE INPUT FOR PRIMARY COMMLS OF TYPE 6 GUARS.          
*              ROUTINE ALSO TAKES CARE OF HANDLING AMT OR RATE OVERRIDE         
         SPACE 1                                                                
*                                  TGFULL=PAYMENT AMOUNT                        
*                                  ELCODE=TAOAELQ OR TAOPELQ                    
UPDOAOP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,TCACAST                                                       
         ST    R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   UOAP4                                                            
         GOTO1 REMELEM             FOUND IT, SO DELETE IT                       
         SPACE 1                                                                
         LA    R4,ELEMENT          R4=A(ELEMENT) AFTER REMELEM                  
         USING TAOAD,R4                                                         
         ZIC   R0,TAOANUM          R0=N'SUB-ELEMENTS                            
         LA    RF,TAOASBEL         RF=A(1ST SUB-ELEMENT)                        
         USING TAOASBEL,RF                                                      
         CLC   TAOAUSE,TGUSCDE     LOOK FOR THIS USE CODE                       
         BE    UOAP8               FOUND IT - GO UPDATE                         
         LA    RF,L'TAOASBEL(RF)                                                
         BCT   R0,*-14                                                          
         B     UOAP6               NOT FOUND - ADD SUB-ELEMENT                  
         DROP  RF                                                               
         SPACE 1                                                                
UOAP4    LA    R4,ELEMENT          BUILD A NEW ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
         MVC   TAOAEL,ELCODE                                                    
         MVI   TAOALEN,TAOALNQ                                                  
         LA    RF,TAOASBEL         SET RF=A(1ST SUB-ELEMENT)                    
         SPACE 1                                                                
UOAP6    ZIC   R1,TAOANUM          NOT IN ELEMENT, SO ADD NEW SUB-EL            
         LA    R1,1(R1)            BUMP N'SUB-ELEMENTS                          
         STC   R1,TAOANUM                                                       
         IC    R1,TAOALEN                                                       
         LA    R1,L'TAOASBEL(R1)   BUMP L'ELEMENT                               
         STC   R1,TAOALEN                                                       
         SPACE 1                                                                
         USING TAOASBEL,RF         RF=A(NEXT AVAILABLE SUB-ELEMENT)             
UOAP8    MVC   TAOAUSE,TGUSCDE     SET NEW SUB-ELEMENT                          
         MVC   TAOAAMT,TGFULL      WITH AMOUNT INPUT                            
         DROP  RF                                                               
         GOTO1 ADDELEM             ADD UPDATED ELEMENT BACK TO CAST REC         
         MVC   AIO,AIO1            RESET DEFAULT I/O AREA                       
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CLEAR OPPOSITE HLD AMT/PCT IN ELEMENT                 
         SPACE 1                                                                
CLROAOP  NTR1  BASE=*,LABEL=*                                                   
         CLI   ELCODE,TAOAELQ      IF REPLACED HLD OVERSCALE AMT                
         BNE   *+12                                                             
         MVI   ELCODE,TAOPELQ      MAKE SURE TO CLEAR HLD OV PCT                
         B     *+8                                                              
         MVI   ELCODE,TAOAELQ      ELSE, CLEAR HLD OV AMT                       
         SPACE 1                                                                
         L     R4,TCACAST                                                       
         ST    R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   CLROAOPX                                                         
         ZIC   R0,TAOANUM          R0=N'SUB-ELEMENTS                            
         LA    RF,TAOASBEL         RF,=A(FIRST SUB-ELEMENT)                     
         USING TAOASBEL,RF                                                      
         SPACE 1                                                                
         CLC   TAOAUSE,TGUSCDE     FIND HLD                                     
         BE    *+16                                                             
         LA    RF,L'TAOASBEL(RF)                                                
         BCT   R0,*-14                                                          
         B     CLROAOPX                                                         
         SPACE 1                                                                
         XC    TAOAUSE,TAOAUSE     AND CLEAR IT                                 
         XC    TAOAAMT,TAOAAMT                                                  
         SPACE 1                                                                
CLROAOPX MVC   AIO,AIO1                                                         
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DETERMINES IS CANADIAN CAST NEEDS TO BE                  
*              PROCESSED ON A 2404A COMMERCIAL                                  
         SPACE 1                                                                
PRO04ACN NTR1  BASE=*,LABEL=*                                                   
         TM    STATUS3,US2404A     IF JUST PROCESSED 2404A US$ CAST             
         BZ    P04ACNO                                                          
         SPACE 1                                                                
         NI    STATUS3,X'FF'-US2404A                                            
         SPACE 1                                                                
         MVI   CANDOLS,C'Y'        NOW SET TO PROCESS CAN$ CAST                 
         OI    STATUS3,CAN2404A                                                 
         SPACE 1                                                                
         MVC   ATHISEST,ACOMEL     AND RESTORE ELEMENT POINTER                  
         NI    STATUS,ALL-NOMORE   TO CURRENT COMMERCIAL                        
         SPACE 1                                                                
P04ACYES XR    RC,RC                                                            
P04ACNO  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TESTS IF PERFORMER'S UNION IS NON                        
         SPACE 1                                                                
NONUNION NTR1  BASE=*,LABEL=*                                                   
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAUN,=C'NON'      IF UNION IS NON                              
         J     XIT                 RETURN POSITIVE CONDITION CODE               
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO TEST IF PERFORMER ON 2404A COMMERCIALS                
*              QUALIFIES AS A CANADIAN                                          
         SPACE 1                                                                
CANCST   NTR1  BASE=*,LABEL=*                                                   
         USING TACAD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         CLC   TACAUN,=C'ACT'      IF (UNION IS ACTRA)                          
         BE    CCSTYES                                                          
         CLC   TACALOCL,=C'CAN'    OR (LOCAL IS CAN)                            
         BE    CCSTYES                                                          
         CLC   TACAUN,=C'AFM'      OR (UNION IS AFM                             
         BNE   CCSTNO                                                           
         CLC   TACAUNIT(2),=C'CN'  AND TAX UNIT IS CANADIAN)                    
         BNE   CCSTNO              PERFORMER BELONGS ON CANADIAN INV            
         DROP  R4                                                               
         SPACE 1                                                                
CCSTYES  XR    RC,RC                                                            
CCSTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES AGENCY COMMISSION                             
         SPACE 1                                                                
CLCCOMM  NTR1  BASE=*,LABEL=*                                                   
         CLI   PROFBTYP,TABRTYE    IF AGY/CLI GETS EMS FEE                      
         BE    CCOMMX                                                           
         OC    AYSGNINF,AYSGNINF   OR SIGNATORY FEE                             
         BNZ   CCOMMX              DON'T CALULCATE COMMISSION                   
         SPACE 1                                                                
         XC    COMMAMT,COMMAMT     CLEAR AMOUNT                                 
         CLI   PROFCOMM,TAEPCNO    DON'T CALCULATE                              
         BE    CCOMMX                                                           
         CLI   PROFCOMM,TAEPCCLI   CALC AT CLIENT LEVEL ONLY                    
         BE    CCOMMX                                                           
         XR    R1,R1                                                            
         L     R4,TCPAYI           PAYMENT AMOUNT                               
         A     R4,TCPAYC                                                        
         CLI   PROFBTYP,TABRTY20                                                
         BE    CCOM10                                                           
         A     R4,TCEXP                                                         
         A     R4,CSFAMT                                                        
         CLI   PROFCOMM,TAEPCPAY   BASE COMMISSION ON PAY ONLY                  
         BE    *+16                                                             
         A     R4,TCPNH            PLUS P&H                                     
         A     R4,TCINR            PLUS I&R                                     
         A     R4,TCHNW            PLUS H&W                                     
         CLI   PROFCOMM,TAEPCHND                                                
         BE    *+16                                                             
         CLI   PROFCOMM,TAEPCTNH                                                
         BNE   *+12                                                             
         A     R4,TAXAMT           ADD IN TAX                                   
         A     R4,HNDAMT           AND HANDLING                                 
         L     R0,COMM                                                          
         B     CCOM20                                                           
CCOM10   A     R4,TCINR            PLUS I&R                                     
         L     R0,COMM             TALENT MASTER FEE OVERRIDE?                  
         LTR   R0,R0               YES, DON'T USE DEFAULTS                      
         BNZ   CCOM20                                                           
         LHI   R0,750                                                           
         CLI   TGUSEQU,UGRT                                                     
         BNE   CCOM20                                                           
         LHI   R0,500                                                           
CCOM20   LR    R1,R4                                                            
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                RETURN ANSWER IN R1                          
         ST    R1,COMMAMT                                                       
         OI    SFCMSTAT,SFCMCOMM   SET COMMISSION CALCULATED                    
CCOMMX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES SIGNATORY FEE                                 
         SPACE 1                                                                
CLCSIGN  NTR1  BASE=*,LABEL=*                                                   
         OC    AYSGNINF,AYSGNINF   EXIT IF AGENCY DOES NOT HAVE                 
         BZ    CSIGNX              SIGNATORY FEE INFO                           
         SPACE 1                                                                
         XC    COMMAMT,COMMAMT     CLEAR AMOUNT                                 
         SPACE 1                                                                
         TM    REPOPT,PRNTSESS     ALSO EXIT IF NOT PRINTING                    
         BO    CSIGN05             SESSIONS                                     
         TM    TGUSSTAT,SESSION    AND THIS IS A SESSION                        
         BO    CSIGNX                                                           
         SPACE 1                                                                
CSIGN05  L     R1,TCPAYI           INDIDIVIDUAL PAYMENT AMOUNT                  
         A     R1,TCPAYC           PLUS CORPORATE PAYMENT AMOUNT                
         CLI   AYSRULE,1                                                        
         BE    CSIGN900                                                         
                                                                                
CSIGN10  CLI   AYSRULE,0                                                        
         BE    CSIGN15                                                          
         CLI   AYSRULE,3                                                        
         BE    CSIGN15                                                          
         CLI   AYSRULE,5                                                        
         BE    CSIGN15                                                          
         CLI   AYSRULE,6                                                        
         BNE   CSIGN20                                                          
                                                                                
CSIGN15  A     R1,TCPNH            PLUS P&H                                     
         A     R1,TCINR            PLUS I&R                                     
         A     R1,TCHNW            PLUS H&W                                     
                                                                                
CSIGN20  CLI   AYSRULE,0                                                        
         BE    CSIGN25                                                          
         CLI   AYSRULE,4                                                        
         BE    CSIGN25                                                          
         CLI   AYSRULE,6                                                        
         BE    CSIGN25                                                          
         CLI   AYSRULE,7                                                        
         BNE   CSIGN30                                                          
CSIGN25  A     R1,HNDAMT           PLUS HANDLING                                
                                                                                
CSIGN30  CLI   AYSRULE,0                                                        
         BE    CSIGN35                                                          
         CLI   AYSRULE,2                                                        
         BE    CSIGN35                                                          
         CLI   AYSRULE,5                                                        
         BE    CSIGN35                                                          
         CLI   AYSRULE,7                                                        
         BNE   CSIGN900                                                         
CSIGN35  A     R1,TAXAMT           PLUS TAXES                                   
                                                                                
CSIGN900 XR    R0,R0               IS MULTIPLIED BY SIGNATORY RATE              
         LH    RE,AYSGNRTE                                                      
         MR    R0,RE                                                            
         LHI   RF,5000                                                          
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         SPACE 1                                                                
         ICM   RE,15,AYSGNCAP                                                   
         CR    R1,RE               IF PRODUCT IS GREATER THAN                   
         BNH   *+6                 SIGNATORY CAP, USE SIGNATORY CAP             
         LR    R1,RE                                                            
         SPACE 1                                                                
         L     RE,COMMAMT                                                       
         AR    RE,R1                                                            
         ST    RE,COMMAMT          ADD TO AGENCY COMMISSION                     
         OI    SFCMSTAT,SFCMSFEE   AND SET SIGNATORY FEE CALCULATED             
CSIGNX   XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES COMMERCIAL SERVICE FEE                        
         SPACE 1                                                                
CLCCSF   NTR1  BASE=*,LABEL=*                                                   
         XC    CSFAMT,CSFAMT       CLEAR AMOUNT                                 
*                                                                               
**NO-OP 09/13                                                                   
*&&DO                                                                           
         OC    TCPCYC,TCPCYC       BLANK CYCLE START DATE?                      
         BZ    CCSFX               NO CSF CHARGED                               
         CLC   TCPCYC,=X'B30401'   CYCLE START APR01/13 OR AFTER?               
         BNL   CCSFX               NO CSF CHARGED                               
*                                                                               
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         TM    TACOSTA2,TACOSJPC   ALREADY PAID CSF?                            
         BNZ   CCSFX                                                            
*                                                                               
         TM    TACOSTA2,TACOSNCS   SKIP IF COMM SAYS DON'T CHARGE               
         BO    CCSFX                                                            
*                                                                               
         TM    CSFSTAT,CSFPRDN     IF PRODUCT STATUS IS NOCSF,                  
         BO    CCSFX               SKIP                                         
         TM    CSFSTAT,CSFPRDY     IF PRODUCT STATUS IS CSF,                    
         BO    CCSF10              SEE IF USE IS VALID FOR CSF                  
         TM    CSFSTAT,CSFCLIN     IF CLIENT STATUS IS NOCSF,                   
         BO    CCSFX               SKIP                                         
         TM    CSFSTAT,CSFCLIY     IF CLIENT STATUS IS CSF,                     
         BO    CCSF10              SEE IF USE IS VALID FOR CSF                  
         TM    CSFSTAT,CSFAGYY     IF AGENCY STATUS IS NOT CSF,                 
         BZ    CCSFX               SKIP                                         
*                                                                               
CCSF10   CLC   TACOFCYC,=X'A90701' FFC MUST BE ON OR AFTER JUL01/09             
         BL    CCSFX                                                            
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TV                             
         BE    CCSF20                                                           
         CLI   TACOMED,TACOMEDC    OR CABLE                                     
         BNE   CCSFX                                                            
*                                                                               
CCSF20   TM    TGUSSTAT,SESSION+CANUSE IF THIS IS RE-USE PAYMENT                
         BNZ   CCSFX                   AND NOT CANADIAN                         
         TM    TGUSSTA2,HLDTYPE    BUT NOT A HOLDING FEE TYPE                   
         BO    CCSFX                                                            
*                                                                               
         CLI   TACOTYPE,0          COMMERCIAL TYPE CAN BE BLANK                 
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYASIAN   ASIAN                                        
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYADD     ADDENDUM                                     
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYANIM    ANIMATICS                                    
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYPUB     PUBLIC SERVICE                               
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYSEAS2   SEASONAL                                     
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYSTRM    SHORT TERM                                   
         BE    CCSF30                                                           
         CLI   TACOTYPE,CTYSPAN    OR SPANISH                                   
         BNE   CCSFX                                                            
*                                                                               
CCSF30   TM    CSFSTAT,CSFUNI      MUST HAVE UNIONS SAG OR AFT                  
         BZ    CCSFX                                                            
*                                                                               
         LA    R1,NOCSFTAB         AND NOT IN EXCLUDED USE TABLE                
CCSF40   CLI   0(R1),X'FF'              TEST END OF TABLE                       
         BE    CCSF50                                                           
         CLC   TGUSEQU,0(R1)            TEST FOR MATCH                          
         BE    CCSFX                                                            
         LA    R1,1(R1)                 BUMP TO NEXT ENTRY                      
         B     CCSF40                                                           
*                                                                               
CCSF50   LHI   R1,CSFRATE          CSF RATE                                     
         ST    R1,CSFAMT                                                        
         OI    SFCMSTAT,SFCMCSF    SET CSF CALCULATED                           
         OI    TACOSTA2,TACOSJPC   SET CSF CALCULATED                           
         DROP  R3                                                               
*&&                                                                             
CCSFX    XIT1                                                                   
         SPACE 2                                                                
*              TABLE OF USES TO EXCLUDE WHEN ADDING CSF FEE                     
*                                                                               
NOCSFTAB DC    AL1(UDWN,UPPF,UPRR,UPNH,UOTH,USOT,USRE,USPN,USLF,UALF)           
         DC    AL1(UARN,UGRT,ULFT,UPEN,UREN,UGRR,UPRT,USCS,USFS)                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDS GUARANTEE TABLE ENTRIES FOR A PERFORMER             
*                                                                               
*   IT TGSSN <> ZERO, IT WILL ALWAYS TRY FOR A GUARANTEE RECORD                 
*   IF IT DOESN'T FIND ONE, IT ASSUMES IT IS A HYPO GUARANTEE                   
*                                                                               
*  TGSSN     TGGUA      GUA        DESCRIPTION                                  
*  --------  --------   ---------  -------------------                          
*  NON-ZERO  NON-ZERO   ZERO       REAL GRT                                     
*            NON-ZERO   NZ=TGGUA   REAL GRT                                     
*            NON-ZERO   NZ<>TGGUA  CAN BE HYPO OR REAL GRT                      
*            ZERO       NON-ZERO   CAN BE HYPO OR REAL GRT                      
*  ZERO      ZERO       NON-ZERO   HYPO GRT                                     
         SPACE 2                                                                
         DS    0D                                                               
GUARBLD  NTR1  BASE=*,LABEL=*                                                   
         OC    TGGUA,TGGUA         DON'T BOTHER IF NOT ON GUARANTEE             
         BNZ   *+14                                                             
         OC    GUA,GUA             AND IF GUAR. NOT SPECIFIED ON EST.           
         BZ    GUARBDYS                                                         
         SPACE 1                                                                
         OC    GUA,GUA             IF GUAR ON EST RECORD                        
         BZ    *+10                                                             
         MVC   TGGUA,GUA           SET TO USE GUAR FROM EST RECORD              
         SPACE 1                                                                
         XC    TGFULL,TGFULL                                                    
         OC    TGSSN,TGSSN         IF NO SSN, MUST BE HYPO PERFORMER            
         BZ    GUARBD2                                                          
         PACK  DUB,TGSSN           CONVERT S/S NUMBER TO BINARY                 
         CVB   R1,DUB                                                           
         ST    R1,TGFULL                                                        
         SPACE 1                                                                
GUARBD2  BAS   RE,CKGUARTB         IF NOT ALREADY IN GUARTAB                    
         USING GUARD,R2                                                         
         BE    GUARBD12            (RETURNS R2)                                 
         SPACE 1                                                                
         OC    TGSSN,TGSSN         IF ACTUAL PERFORMER                          
         BZ    GUARBD3                                                          
         BAS   RE,RDGRT            TRY FOR GUARANTEE REC (SETS TGELEM)          
         BNE   GUARBD3             NOT FOUND - ASSUME HYPOTHETICAL              
         SPACE 1                                                                
         L     R4,TGELEM           R4=A(GUARANTEE DETAILS ELEMENT)              
         USING TAGUD,R4                                                         
         OC    TAGUCOM,TAGUCOM     IF PRIMARY COMMERCIAL NOT DEFINED            
         BNZ   GUARBD6                                                          
         MVC   TGFULL,TAGUBAL      SET GUARANTEE BALANCE                        
         MVC   TGDUB,TAGUPD        PERIOD                                       
         BAS   RE,GUARADD          ADD TO TABLE - RETURNS R2=A(ENTRY)           
         B     GUARBD12                                                         
         SPACE 1                                                                
GUARBD3  TM    STATUS2,ESTGRT      IF WON'T ESTIMATE GRT PAYMENT                
         BZ    GUARBD16            ADD TO HOLDTAB - NEED TO WAIT                
         CLI   PRIMARY,C'Y'        IF THIS ISN'T A TYPE 6 GUARANTEE             
         BNE   GUARBDX             THEN DONE                                    
         XC    ELEMENT,ELEMENT     ELSE BUILD DUMMY GUAR. DETAILS EL.           
         LA    R4,ELEMENT                                                       
         ST    R4,TGELEM                                                        
         USING TAGUD,R4                                                         
         MVC   TAGUCOM,TGCOM       SET HYPO INTERNAL COMML NUMBER               
         B     GUARBD7             ADD DUMMY GUARANTEE TABLE ENTRY              
         SPACE 1                                                                
GUARBD6  MVI   ELCODE,TAGCELQ      ELSE LOOK FOR GUAR. CYCLE ELEMENTS           
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    GUARBD8                                                          
GUARBD7  XC    TGFULL,TGFULL       NONE FOUND - SET UP DUMMY ENTRY              
         XC    TGDUB,TGDUB                                                      
         BAS   RE,GUARADD          ADD TO TABLE - RETURNS R2=A(ENTRY)           
         B     GUARBD12                                                         
         SPACE 1                                                                
         USING TAGCD,R4            R4=A(GUARANTEE CYCLE ELEMENT)                
GUARBD8  MVC   TGFULL,TAGCBAL      SET GUARANTEE BALANCE                        
         MVC   TGDUB,TAGCPD        PERIOD                                       
         BAS   RE,GUARADD          ADD TO TABLE - RETURNS R2=A(ENTRY)           
         BRAS  RE,NEXTEL           LOOK FOR ANOTHER GUAR. CYCLE EL.             
         BE    GUARBD8                                                          
         SPACE 1                                                                
GUARBD12 OC    GUARCOM,GUARCOM     IF PRIMARY COMMERCIAL DEFINED                
         BZ    GUARBDX                                                          
         CLC   TGCOM,GUARCOM       AND WE'RE NOT PROCESSING PRIMARY NOW         
         BE    GUARBDX                                                          
         BAS   RE,CKSEEN           CHK IF WE'VE PROC'D PRIMARY ALREADY          
         BE    GUARBDX             YES - EXIT                                   
*                                  NO  - NEED TO WAIT FOR PRIMARY               
GUARBD16 TM    STATUS,FLUSHING     IF WE'RE NOT FLUSHING HELD PERFS.            
         BO    GUARBDX                                                          
         BAS   RE,ADDHOLD          ADD CAST MEMBER TO HOLD TABLE                
         OI    STATUS2,CASTACTV+COMLACTV SET ACTIVE (CANT DURING FLUSH)         
         B     GUARBDNO            AND RETURN CC NE - WILL PROC LATER           
         SPACE 1                                                                
GUARBDX  B     GUARBDYS            RETURN CC EQ - OK TO CONTINUE                
         EJECT                                                                  
*              ROUTINE TO CHECK PERFORMER/GUARANTEE ALREADY IN TABLE            
*              RETURNS R2=A(GUARTAB ENTRY)                                      
         SPACE                                                                  
CKGUARTB NTR1                                                                   
         L     R2,GUARTAB          R2=A(GUARANTEE TABLE)                        
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
*                                                                               
CKGRTB2  CLC   GUARSSN,TGFULL      MATCH ON S/S NUMBER                          
         BNE   CKGRTB10                                                         
         CLC   GUARGUA,TGGUA       MATCH ON GUARANTEE                           
         BE    GRBDYSR2                                                         
*                                                                               
CKGRTB10 LA    R2,GUARNEXT         BUMP TO NEXT ENTRY                           
         BCT   R0,CKGRTB2                                                       
         B     GRBDNOR2                                                         
         SPACE 2                                                                
*              ROUTINE TO READ GUARANTEE RECORD                                 
*              SETS TGELEM WITH A(GUARANTEE DETAILS ELEMENT)                    
         SPACE                                                                  
RDGRT    NTR1                                                                   
         XC    TGGUA,HEXFFS2       ELSE COMPLEMENT GUARANTEE CODE               
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'20',0)  READ GUARANTEE RECORD             
         BNE   GUARBDNO                                                         
         GOTOR GTRACE,DMCB,=C'GUAR'                                             
         XC    TGGUA,HEXFFS2       UN-COMPLEMENT GUARANTEE CODE                 
*                                                                               
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   GUARBDNO                                                         
*                                                                               
         LR    R3,R4                                                            
         XR    R0,R0                                                            
*                                                                               
         USING TAGUD,R3            R3=A(GUARANTEE DETAILS ELEMENT)              
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         BNZ   RDGRT10                                                          
         TM    TGCTSTST,TGCTSCLI                                                
         BO    GUARBDNO                                                         
*                                                                               
RDGRT10  OC    TAGUAGY,TAGUAGY     IF GUARANTEE IS AGENCY-SPECIFIC              
         BZ    RDGRT20                                                          
         CLC   TGAGY,TAGUAGY       INSURE THIS IS CORRECT AGENCY                
         BE    RDGRT20                                                          
         LHI   R0,1                                                             
*                                                                               
RDGRT20  OC    TAGUCLI,TAGUCLI     IF GUARANTEE IS CLIENT-SPECIFIC              
         BZ    RDGRT30                                                          
         CLC   TGCLI,TAGUCLI       INSURE THIS IS CORRECT CLIENT                
         BE    RDGRT30                                                          
         LHI   R0,1                                                             
         DROP  R3                                                               
*                                                                               
RDGRT30  LTR   R0,R0               IF USING NEW GUARANTEE SYSTEM                
         BZ    RDGRT60             AND AGY/CLI MATCH NOT YET FOUND              
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ      GET FIRST VALID AGENCY/CLIENT                
         BRAS  RE,GETEL            ELEMENT                                      
         B     *+8                                                              
RDGRT40  BRAS  RE,NEXTEL                                                        
         BNE   GUARBDNO            IF NOT FOUND, ERROR                          
*                                                                               
         CLC   TAVAAGY,TGAGY       IF AGENCY IS FOUND IN GRT LIMITS             
         BNE   RDGRT40                                                          
         CLI   TAVALEN,TAVALNQ     AND CLIENT LIMITS ARE NOT DEFINED            
         BE    RDGRT60             ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
RDGRT50  CLC   TGCLI,0(RF)         IF CLIENT IS FOUND IN GRT LIMITS             
         BE    RDGRT60             ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   RDGRT50                                                          
         B     RDGRT40                                                          
         DROP  R4                                                               
*                                                                               
RDGRT60  ST    R3,TGELEM           SAVE A(ELEMENT)                              
         B     GUARBDYS                                                         
         EJECT                                                                  
*              ROUTINE CHECKS COMMERCIAL TABLE TO SEE IF                        
*              PRIMARY COMMERCIAL HAS ALREADY BEEN PROCESSED                    
         SPACE                                                                  
CKSEEN   NTR1                                                                   
         L     R4,ACOMEL           R4=A(COMMERCIAL ELEMENT)                     
         USING TAESD,R4                                                         
         CLI   TAESTYPE,TAESTCOM   IF PROCESSING HYPO COMMERCIAL                
         BE    CKSEEN40                                                         
         L     R1,HCOMTAB          R1=A(HYPOTHETICAL COMMLS TABLE)              
         L     R0,MAXHCOM          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING HCOMD,R1                                                         
*                                                                               
CKSEEN10 OC    0(HCOMLNQ,R1),0(R1) TEST LOGICAL END OF TABLE                    
         BZ    GUARBDNO                                                         
         CLC   HCOMCLI,TGCLI       LOOK FOR SAME CLIENT                         
         BNE   CKSEEN20                                                         
         CLC   HCOMPRD,TGPRD                     PRODUCT                        
         BNE   CKSEEN20                                                         
         CLC   HCOMCID,TGCID                     COMMERCIAL ID                  
         BE    GUARBDYS                                                         
CKSEEN20 LA    R1,HCOMNEXT         TRY NEXT SPOT IN TABLE                       
         BCT   R0,CKSEEN10                                                      
         B     GUARBDNO                                                         
*                                                                               
CKSEEN40 L     R1,COMLTAB          R1=A(ACTUAL COMMERCIAL TABLE)                
         L     R0,MAXCOML          R0=MAXIUM NUMBER OF TABLE ENTRIES            
*                                                                               
CKSEEN50 OC    0(4,R1),0(R1)       TEST LOGICAL END OF TABLE                    
         BZ    GUARBDNO                                                         
         CLC   GUARCOM,0(R1)       LOOK FOR SAME INTERNAL COMML NUMBER          
         BE    GUARBDYS                                                         
         LA    R1,4(R1)            TRY NEXT SPOT IN TABLE                       
         BCT   R0,CKSEEN50                                                      
         B     GUARBDNO                                                         
         EJECT                                                                  
*              ROUTINE TO ADD A GUARANTEE TO GUARANTEE TABLE                    
         SPACE 1                                                                
*                                  TGFULL=BALANCE                               
*                                  TGDUB=CYCLE DATES                            
GUARADD  NTR1                                                                   
         L     R2,GUARTAB          LOOK FOR EMPTY SLOT IN TABLE                 
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING GUARD,R2            R2=A(GUARANTEE TABLE)                        
         OC    GUARD(GUARLNQ),GUARD  TAKE FIRST EMPTY ENTRY                     
         BZ    GADD5                                                            
         LA    R2,GUARNEXT         BUMP TO NEXT ENTRY                           
         BCT   R0,*-14                                                          
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   NOTNOW1             GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE - INCREASE NGUARS                   
         SPACE 1                                                                
GADD5    L     R4,TGELEM                                                        
         USING TAGUD,R4            R4=A(GUARANTEE DETAILS ELEMENT)              
         SPACE 1                                                                
         OC    TGSSN,TGSSN         SKIP SSN IF NONE DEFINED                     
         BZ    GADD10                                                           
         PACK  DUB,TGSSN           BUILD ENTRY - S/S NUMBER                     
         CVB   R1,DUB                                                           
         ST    R1,GUARSSN                                                       
GADD10   MVC   GUARGUA,TGGUA                     GUARANTEE CODE                 
         MVC   GUARCOM,TAGUCOM                   PRI COMML (IF DEFINED)         
         MVC   GUARSTAT,TAGUSTAT                 STATUS                         
         NI    GUARSTAT,GUARSPNH+GUARSOVR        TURN OFF ALL BUT THESE         
         TM    TAGUSTA2,TAGUSIGP                 IF IGNORE PAY OVERAGE          
         BZ    *+8                               IS SET, TURN ON                
         OI    GUARSTAT,GUARSIGP                                                
         MVC   GUARBAL,TGFULL                    BALANCE REMAINING              
* NO-OP  TM    GUARBAL,X'80'                     PROT AGAINST BAD RECS          
* 5/13   BZ    *+10                                                             
* OKNEG  XC    GUARBAL,GUARBAL                                                  
         MVC   GUARPD,TGDUB                      PERIOD                         
         B     GRBDX1R2            RETRUNS R2=A(ENTRY)                          
         EJECT                                                                  
*              ROUTINE ADDS CAST MEMBERS TO HOLD TABLE                          
         SPACE 1                                                                
         USING GUARD,R2            R2=A(GUARANTEE TABLE ENTRY)                  
ADDHOLD  NTR1                                                                   
         MVC   FULL,GUARCOM        SAVE PRIMARY COMMERCIAL NUMBER               
         SPACE 1                                                                
         L     R2,HOLDTAB          R2=A(HOLD TABLE)                             
         L     R0,MAXHOLD          R0=MAXIUM NUMBER OF TABLE ENTRIES            
         USING HOLDD,R2                                                         
         SPACE 1                                                                
AHOLD10  OC    HOLDD(HOLDLNQ),HOLDD TAKE FIRST EMPTY ENTRY                      
         BZ    AHOLD20                                                          
         LA    R2,HOLDNEXT         BUMP TO NEXT SPOT                            
         BCT   R0,AHOLD10          AND TRY AGAIN                                
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   NOTNOW1             GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE - INCREASE NHOLD!                   
         SPACE 1                                                                
AHOLD20  MVC   HOLDCOM,FULL        SAVE COMML NUMBER WE'RE WAITING FOR          
         MVC   HOLDACLI,ACLIEL     SAVE A(CLIENT ESTIMATE DETAILS EL.)          
         MVC   HOLDAPRD,APRDEL            PRODUCT                               
         MVC   HOLDACOM,ACOMEL            COMMERCIAL                            
         MVC   HOLDAPRF,APRFEL            PERFORMER                             
         SPACE 1                                                                
         OC    HOLDAPRF,HOLDAPRF   IF DON'T HAVE A(PERFORMER EL.)               
         BNZ   *+10                                                             
         MVC   HOLDSORT,TGCSORT    THEN SAVE CAST SORT KEY                      
         B     GUARBDX1                                                         
         SPACE                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
         DS    0D                                                               
GTRACE   NTR1                                                                   
         CLI   TRACEOPT,C'N'       IF TRACING                                   
         BE    GTRACEX                                                          
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,AIO3                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'3'                                                       
         TM    STATUS,FLUSHING                                                  
         BZ    *+14                                                             
         MVC   7(8,R4),=C' - FLUSH'                                             
         LA    R3,8(R3)                                                         
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
GTRACEX  B     GUARBDX1                                                         
         SPACE 2                                                                
GUARBDYS XR    RC,RC                                                            
GUARBDNO LTR   RC,RC                                                            
GUARBDX1 XIT1                                                                   
         SPACE 2                                                                
GRBDYSR2 XR    RC,RC                                                            
GRBDNOR2 LTR   RC,RC                                                            
GRBDX1R2 XIT1  REGS=(R2)                                                        
         SPACE                                                                  
HEXFFS2  DC    6X'FF'                                                           
         SPACE                                                                  
NOTNOW1  L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         LA    R2,CONWHENH         REQUEST TOO BIG FOR ONLINE PROC.             
         MVI   ERROR,NOTONLIN                                                   
         GOTO1 EXIT,DMCB,0                                                      
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE UPDATES GUARANTEE TABLE IF NECESSARY                     
         SPACE 1                                                                
         DS    0D                                                               
GUARUPD  NTR1  BASE=*,LABEL=*                                                   
         XR    R2,R2                                                            
         SPACE 1                                                                
         XC    TGFULL,TGFULL                                                    
         OC    TGSSN,TGSSN         IF NO SSN, MUST BE HYPO                      
         BZ    GUPD2                                                            
         PACK  DUB,TGSSN           CONVERT S/S NUMBER TO BINARY                 
         CVB   R1,DUB                                                           
         ST    R1,TGFULL                                                        
         SPACE 1                                                                
GUPD2    CLI   TGUSEQU,UGRT        IF THIS IS A GUARANTEE                       
         BE    *+12                                                             
         TM    TCINPUT,TCINPRI     OR FIXED CYCLE PAYMENT ON PRI COMML          
         BZ    GUPD4                                                            
         L     R2,GUARTAB          LOOP THROUGH TABLE                           
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING GUARD,R2            R2=A(GUARANTEE TABLE)                        
         OC    GUARD(GUARLNQ),GUARD  LOOKING FOR EMPTY SLOT                     
         BZ    GUARD3                                                           
         LA    R2,GUARNEXT                                                      
         BCT   R0,*-14                                                          
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BNE   NOTNOW2             GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE  - INCREASE NGUARS!                 
         SPACE 1                                                                
GUARD3   MVC   GUARSSN,TGFULL      S/S NUMBER                                   
         MVC   GUARGUA,TGGUA       GUARANTEE CODE                               
         L     R1,TCPAY            BALANCE REMAINING IS PAYMENT AMOUNT          
         A     R1,TCGUAR           LESS ANY CREDITS (FOR TYPE 6)                
         ST    R1,GUARBAL                                                       
         MVC   GUARSTRT(6),TCPCYC  PERIOD IS CYCLE DATES                        
         TM    TCINPUT,TCINPRI     IF FIXED CYCLE PAYMENT ON PRI COMML          
         BZ    *+10                                                             
         MVC   GUARCOM,TGCOM       SAVE PRIMARY COMMERCIAL NUMBER               
         OI    GUARSTAT,GUARSOVR   SET PAYING OVERAGE                           
*NO-OP*  MVI   GUARSTAT,GUARSOVR   SET PAYING OVERAGE                           
         B     GUPD15                                                           
         SPACE 1                                                                
GUPD4    CLI   GUARSW,C'N'         GET OUT IF GUAR CREDITTING DISABLED          
         BE    GUPD15                                                           
         LA    R3,TCPAYI           R3=A(PAYMENT AMOUNT)                         
         CLI   TCW4TYPE,TAW4TYCO                                                
         BE    GUPD4A                                                           
         CLI   TCW4TYPE,TAW4TYCA                                                
         BE    GUPD4A                                                           
         CLI   TCW4TYPE,TAW4TYFO                                                
         BNE   GUPD5                                                            
GUPD4A   LA    R3,TCPAYC                                                        
         SPACE 1                                                                
GUPD5    OC    0(4,R3),0(R3)       IF THERE'S (STILL) A PAYMENT AMOUNT          
         BZ    GUPD15                                                           
         SPACE 1                                                                
         L     R2,GUARTAB          LOOP THROUGH TABLE                           
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
GUPD6    CLC   GUARSSN,TGFULL      LOOKING FOR THIS S/S NUMBER                  
         BNE   GUPD8                                                            
         OC    GUARGUA,GUARGUA     IF THIS IS AN ACTUAL GUARANTEE               
         BZ    *+14                                                             
         CLC   GUARGUA,TGGUA       MUST MATCH GUARANTEE CODE                    
         BNE   GUPD8                                                            
         SPACE 1                                                                
         TM    GUARSTAT,GUARSIGP   IF IGNORING NOT PAYING OVERAGE               
         BO    *+12                                                             
         TM    GUARSTAT,GUARSOVR   IF PAYING OVERAGE                            
         BZ    *+12                                                             
         ICM   R1,15,GUARBAL       ENTRY MUST HAVE REMAINING BALANCE            
         BNP   GUPD8                                                            
         SPACE 1                                                                
         MVC   TGTHREE,TCIPCYCS    USE INDIV. CYCLE START IF AROUND             
         OC    TGTHREE,TGTHREE                                                  
         BNZ   *+10                                                             
         MVC   TGTHREE,TCPCYCS     ELSE USE GENERAL CYCLE START                 
         CLC   TGTHREE,GUARSTRT    PAYMENT MUST FALL WITHIN PERIOD              
         BL    GUPD8                                                            
         CLC   TGTHREE,GUAREND                                                  
         BNH   GUPD10                                                           
         SPACE 1                                                                
GUPD8    LA    R2,GUARNEXT         TRY NEXT ENTRY IN TABLE                      
         BCT   R0,GUPD6                                                         
         B     GUPD15              NONE FOUND                                   
         SPACE 1                                                                
GUPD10   L     R4,0(R3)            R4=ORIGINAL PAYMENT AMOUNT                   
         L     R0,GUARBAL          R0=REMAINING BALANCE                         
         TM    GUARSTAT,GUARSIGP   IF IGNORING NOT PAYING OVERAGE               
         BO    *+12                                                             
         TM    GUARSTAT,GUARSOVR   IF PAYING OVERAGE                            
         BZ    *+12                                                             
         C     R0,0(R3)            COMPARE BALANCE TO PAYMENT AMOUNT            
         BNH   *+8                 IF NOT LARGER THEN SKIP                      
         L     R0,0(R3)               ELSE AMT CREDITTED = PAY AMOUNT           
         LCR   R0,R0               REVERSE SIGN                                 
         L     R1,TCGUAR                                                        
         AR    R1,R0               UPDATE GUARANTEE CREDITS TAKEN               
         ST    R1,TCGUAR                                                        
         L     R1,0(R3)                                                         
         AR    R1,R0               UPDATE PAYMENT AMOUNT                        
         ST    R1,0(R3)                                                         
         A     R0,GUARBAL          UPDATE PREVIOUS BALANCE                      
         ST    R0,GUARBAL                                                       
         SPACE 1                                                                
         TM    GUARSTAT,GUARSPNH   IF NOT PAYING P&H ON USE                     
         BO    GUPD12                                                           
         L     R0,0(R3)            NEW PAYMENT AMOUNT                           
         CVD   R0,DUB                                                           
         ZAP   PL16,DUB                                                         
         MP    PL16,=P'10'         * 10 FOR ROUNDING                            
         L     R0,TCPNH                                                         
         CVD   R0,DUB                                                           
         MP    PL16,DUB            * OLD P&H                                    
         CVD   R4,DUB                                                           
         DP    PL16,DUB            / OLD PAYMENT AMOUNT                         
         SRP   PL16(8),63,5        ROUNDED                                      
         CVB   R1,PL16                                                          
         ST    R1,TCPNH            = NEW P&H                                    
GUPD12   DS    0H                                                               
*        GOTOR SPCL18                                                           
         B     GUPD5               LOOK FOR ANOTHER GUAR IF PAY REMAINS         
         SPACE 1                                                                
GUPD15   BRAS  RE,GRTHAND          CALCULATE APP GUAR HANDLING                  
         XIT1                                                                   
         SPACE                                                                  
NOTNOW2  L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         LA    R2,CONWHENH         REQUEST TOO BIG FOR ONLINE PROC.             
         MVI   ERROR,NOTONLIN                                                   
         GOTO1 EXIT,DMCB,0                                                      
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CALCULATE APPLIED GUARANTEE HANDLING CHARGE           
*              ON ENTRY ... R2=A(GUARANTEE TABLE)                               
         SPACE 1                                                                
         USING GUARD,R2            R2=A(GUARANTEE TABLE)                        
GRTHAND  NTR1  BASE=*,LABEL=*                                                   
         LTR   R2,R2                                                            
         BZ    GHX                                                              
         CLI   TCW4TYPE,TAW4TYIN   ONLY EXECUTE ROUTINE IF                      
         BE    GH10                PERFORMER IS INDIVIDUAL                      
         CLI   TCW4TYPE,TAW4TYCO   OR CORPORATION                               
         BE    GH05                                                             
         CLI   TCW4TYPE,TAW4TYCA   OR CANADIAN                                  
         BE    GH05                                                             
         CLI   TCW4TYPE,TAW4TYES   OR ESTATE                                    
         BE    GH05                                                             
         CLI   TCW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         BNE   GHX                                                              
         SPACE 1                                                                
GH05     CLI   PROFHRUL,1          IF PERORMER IS CORPORATION                   
         BE    GH20                HANDLING RULE MUST BE SET                    
         CLI   PROFHRUL,2          BETWEEN 1 AND 4                              
         BE    GH20                                                             
         SPACE 1                                                                
GH10     CLI   PROFHRUL,3          IF PERFORMER IS INDIVIDUAL                   
         BE    GH20                HANDLING RULE MUST BE SET                    
         CLI   PROFHRUL,4          AS 3 OR 4                                    
         BNE   GHX                                                              
         SPACE 1                                                                
GH20     OC    GUARCOM,GUARCOM     EXIT IF GUARANTEE IS TYPE 6                  
         BNZ   GHX                                                              
         DROP  R2                                                               
         SPACE 1                                                                
         TM    REPOPT,PRNTSESS     ALSO EXIT IF NOT PRINTING SESSIONS           
         BO    GH30                                                             
         TM    TGUSSTAT,SESSION    AND THIS IS A SESSION PAYMENT                
         BO    GHX                                                              
         SPACE 1                                                                
GH30     L     R1,TCGUAR           COMPLEMENT APPLIED GUARANTEE                 
         LCR   R1,R1                                                            
         SPACE 1                                                                
         LH    RE,TGBHAND          MULTIPLY APPLIED GUARANTEE                   
         CLI   PROFBTYP,TABRTY21   IF TYPE 21                                   
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY23   IF TYPE 23                                   
         BNE   GH35                                                             
         TM    TGUSSTAT,SESSION    AND SESSION PAYMENT                          
         BNO   GH35                                                             
         LH    RE,TGBSUTA          USE SUTA FOR HANDLING RATE                   
GH35     XR    R2,R2               AMOUNT BY HANDLING RATE                      
         LR    R3,R1                                                            
         MR    R2,RE                                                            
         LHI   RF,5000                                                          
         DR    R2,RF                                                            
         LTR   R3,R3                                                            
         BM    *+8                                                              
         AHI   R3,1                                                             
         SRA   R3,1                                                             
         A     R3,GRTHND           ADD TO PREVIOUSLY CALCULATED                 
         ST    R3,GRTHND           APPLIED GUARANTEE HANDLING                   
         SPACE 1                                                                
         CLI   PROFHRUL,1          DONE IF USING HANDLING RULE                  
         BE    GHX                 1 OR 3                                       
         CLI   PROFHRUL,3                                                       
         BE    GHX                                                              
         SPACE 1                                                                
*&&DO                                                                           
**NO-OP  07/2014                                                                
         USING YEARD,RF                                                         
         LA    RF,YEARTAB          RF=A(YEAR TABLE)                             
GH40     CLI   0(RF),0                                                          
         BE    GHX                                                              
         CLC   YEARYR,TGYREQU      FIND ENTRY FOR CAST YEAR                     
         BE    GH50                                                             
         LA    RF,YEARNEXT                                                      
         B     GH40                                                             
*&&                                                                             
         SPACE 1                                                                
         LR    R3,R1               SAVE APPLIED GRT AMOUNT                      
         GOTO1 YRVAL,DMCB,(X'80',TGYREQU)                                       
         BNE   GHX                                                              
         L     RF,TGAYEAR                                                       
         USING YRTABD,RF                                                        
         LR    R1,R3               RESTORE APPLIED GRT AMOUNT                   
                                                                                
GH50     SR    R0,R0                                                            
**NO-OP  MVC   DUB(2),YEARPNHT     MULTIPLY APPLIED GUARANTEE                   
         MVC   DUB(2),YRPNHT       MULTIPLY APPLIED GUARANTEE                   
         DROP  RF                                                               
         MH    R1,DUB              AMOUNT BY THE P&H RATE                       
         LHI   RF,5000                                                          
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         SPACE 1                                                                
         LH    RE,TGBHAND          MULTIPLY PHANTOM P&H                         
         CLI   PROFBTYP,TABRTY21   IF TYPE 21                                   
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY23   IF TYPE 23                                   
         BNE   GH60                                                             
         TM    TGUSSTAT,SESSION    AND SESSION PAYMENT                          
         BNO   GH60                                                             
         LH    RE,TGBSUTA          USE SUTA FOR HANDLING RATE                   
GH60     XR    R0,R0               AMOUNT BY HANDLING RATE                      
         MR    R0,RE                                                            
         LHI   RF,5000                                                          
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         A     R1,GRTHND           ADD TO PREVIOUSLY CALCULATED                 
         ST    R1,GRTHND           APPLIED GUARANTEE HANDLING                   
GHX      XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* THIS ROUTINE DETERMINES THE TAXABLE STATE.  USUALLY IT IS THE STATE           
* OF WORK.  IF THE SOW IS NOT DEFINED ON THE EMPLOYER RECORD AND IF THE         
* STATE OF RESIDENCE IS DEFINED ON THE EMPLOYER RECORD THEN THE SOR IS          
* THE TAXABLE STATE FOR PURPOSES OF DISABILITY, UNEMPLOYMENT, ETC.              
* IF NEITHER ARE DEFINED ON THE EMPLOYER RECORD THEN THE ROUTINE                
* ATTEMPTS TO ASSIGN THE DEFAULT TAXABLE STATE FROM THE EMPLOYER                
* RECORD.  THE STATE U.C.# FOR THE TAXABLE STATE IS SET ACCORDINGLY.            
* TAXABLE STATE IN TMUNIT IS SENT TO TALIM FOR ACTUAL PAYMENTS                  
*----------------------------------------------------------------------         
*                                                                               
         USING TASYSIOD,R2                                                      
         USING TMD,R3                                                           
SETTAXBL NTR1  BASE=*,LABEL=*                                                   
         XC    CASTSOR,CASTSOR                                                  
         TM    STATUS,HYPOPERF     SKIP IF PROCESSING HYPO PERFORMER            
         JO    XIT                                                              
                                                                                
         CLI   TMWUNIT+2,C' '      IF THIS IS A CITY                            
         BNH   SETT03                                                           
         GOTO1 TAXVAL,DMCB,(3,TMWUNIT)                                          
         MVC   TMUNIT,TGTASTCY     FIND THE STATE IT BELONGS TO                 
                                                                                
SETT03   L     R0,AIO                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',0)  GET W4 RECORD                     
         JNE   SETTXX                                                           
                                                                                
         MVI   ELCODE,TAWHELQ      GET WITHHOLDING ELEMENT                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SETT05   BRAS  RE,NEXTEL                                                        
         JNE   SETTXX                                                           
         USING TAWHD,R4                                                         
         CLC   TAWHEMP,TGEMP       MATCH ON EMPLOYER                            
         JNE   SETT05                                                           
         CLC   TAWHUNIT,=C'FD '    SKIP IF FEDERAL                              
         JE    SETT05                                                           
         CLI   TAWHUNIT+2,C' '     FIND STATE OF RESIDENCE                      
         JNE   SETT05                                                           
         MVC   CASTSOR,TAWHUNIT    STATE OF RESIDENCE                           
         DROP  R4                                                               
                                                                                
*                                  SEARCH EMPLOYER RECORD FOR TAX ID            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A4',TGEMP)                                
         JNE   SETTXX                                                           
                                                                                
         MVC   TGFULL,SPACES                                                    
         MVC   TGFULL(L'TMUNIT),TMUNIT    STATE OF WORK                         
         BRAS  RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         JE    SETTXX              IF FOUND - DONE                              
*                                                                               
         MVC   TGFULL,CASTSOR      STATE OF RESIDENCE                           
         BRAS  RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         JNE   SETT10              IF FOUND                                     
         MVC   TMUNIT,CASTSOR      SET TAXABLE STATE IS STATE OF RES.           
         J     SETTXX                                                           
*                                                                               
SETT10   L     R4,AIO1                                                          
         MVI   ELCODE,TAEDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   SETTXX                                                           
         USING TAEDD,R4                                                         
         MVC   TMUNIT,TAEDDST       IF NOT USE DEFAULT STATE                    
         DROP  R4                                                               
*                                                                               
SETTXX   ST    R0,AIO                                                           
         J     XIT                                                              
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
* THIS ROUTINE DETERMINES THE TAXABLE STATE.  USUALLY IT IS THE STATE           
* OF WORK.  IF THE SOW IS NOT DEFINED ON THE EMPLOYER RECORD AND IF THE         
* STATE OF RESIDENCE IS DEFINED ON THE EMPLOYER RECORD THEN THE SOR IS          
* THE TAXABLE STATE FOR PURPOSES OF DISABILITY, UNEMPLOYMENT, ETC.              
* IF NEITHER ARE DEFINED ON THE EMPLOYER RECORD THEN THE ROUTINE                
* ATTEMPTS TO ASSIGN THE DEFAULT TAXABLE STATE FROM THE EMPLOYER                
* RECORD.  THE STATE U.C.# FOR THE TAXABLE STATE IS SET ACCORDINGLY.            
* TAXABLE STATE IS SAVED IN CASTTXST FOR ESTIMATED PAYMENTS                     
*----------------------------------------------------------------------         
*                                                                               
         USING TASYSIOD,R2                                                      
SETTAXB2 NTR1  BASE=*,LABEL=*                                                   
         MVC   CASTTXST,TGTACODE   DEFAULT IS STATE OF WORK                     
         TM    STATUS,HYPOPERF     SKIP IF PROCESSING HYPO PERFORMER            
         JO    XIT                                                              
                                                                                
         L     R0,AIO                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',0)  GET W4 RECORD                     
         JNE   SETT2X                                                           
                                                                                
         MVI   ELCODE,TAWHELQ      GET WITHHOLDING ELEMENT                      
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SET205   BRAS  RE,NEXTEL                                                        
         JNE   SETT2X                                                           
         USING TAWHD,R4                                                         
         CLC   TAWHEMP,TGEMP       MATCH ON EMPLOYER                            
         JNE   SET205                                                           
         CLC   TAWHUNIT,=C'FD '    SKIP IF FEDERAL                              
         JE    SET205                                                           
         CLI   TAWHUNIT+2,C' '     FIND STATE OF RESIDENCE                      
         JNE   SET205                                                           
         MVC   CASTTXST,TAWHUNIT   STATE OF RESIDENCE                           
         DROP  R4                                                               
                                                                                
*                                  SEARCH EMPLOYER RECORD FOR TAX ID            
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A4',TGEMP)                                
         JNE   SETT2X                                                           
                                                                                
         MVC   TGFULL,TGTACODE     STATE OF WORK                                
         CLI   TGFULL+2,C' '       IF THIS IS A CITY                            
         BNH   SET207                                                           
         GOTO1 TAXVAL,DMCB,(3,TGFULL)                                           
         MVC   TGFULL,TGTASTCY     FIND THE STATE IT BELONGS TO                 
SET207   BRAS  RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         JNE   SET210                                                           
         MVC   CASTTXST,TGTASTCY   SET STATE OF WORK AS TAXABLE STATE           
         J     SETT2X              IF FOUND - DONE                              
*                                                                               
SET210   MVC   TGFULL,CASTTXST     STATE OF RESIDENCE                           
         BRAS  RE,GETSTATE         LOOK UP STATE OF WORK ON EMP RECORD          
         JE    SETT2X              IF FOUND - DONE                              
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,TAEDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   SETT2X                                                           
         USING TAEDD,R4                                                         
         MVC   CASTTXST,TAEDDST    IF NOT USE DEFAULT STATE                     
         DROP  R4                                                               
*                                                                               
SETT2X   ST    R0,AIO                                                           
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        FIND STATE IN EMPLOYER RECORD                                          
*                      TGFULL - STATE                                           
*                                                                               
GETSTATE NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   ELCODE,TATIELQ          ELEMENT FOR STATE OF WORK                
         MVI   FULL,TATITYUN                                                    
         MVC   FULL+1(3),TGFULL                                                 
         GOTO1 GETL,DMCB,(4,FULL)  FIRST TRY TO FIND UNEMPLOYMENT ID            
         JE    YES                                                              
*                                                                               
         MVI   FULL,TATITYTX       THEN TRY TO FIND TAX ID                      
         GOTO1 GETL,DMCB,(4,FULL)                                               
         JE    YES                 SET CC                                       
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*        SPECIAL CALCULATION FOR BILLING TYPE 18                                
*           EXTRA HANDLING                                                      
*======================================================================         
*&&DO                                                                           
SPCL18   NTR1  BASE=*,LABEL=*                                                   
         CLI   TGOFF,C'7'          OFFICE 7 / G ONLY                            
         BE    *+8                                                              
         CLI   TGOFF,C'8'                                                       
         BNE   SPCLHX                                                           
*                                                                               
         CLI   PROFBTYP,TABRTY7    BILLING TYPE 7 AND                           
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY18   IF BILLING TYPE 18 AND                       
         BNE   SPCL18X                                                          
         OC    TCGUAR,TCGUAR       APPLIED GUAR NOT A ZERO AMOUNT               
         BZ    SPCL18X                                                          
*                                                                               
         L     R1,TCGUAR           CREDITS ARE NEGATIVE NUMBERS                 
         LPR   R1,R1               MAKE IT POSITIVE                             
         LR    R2,R1                                                            
         MVC   DUB(2),TGBHAND                                                   
         MH    R1,DUB                                                           
         CVD   R1,DUB              PACKED NUMBER                                
         SRP   DUB,60,5            DIVIDE BY 10 AND ROUND                       
         CVB   R1,DUB                                                           
         LR    RF,R1                                                            
         A     R1,HNDAMT           CHANGE HANDLING                              
         ST    R1,HNDAMT                                                        
*                                                                               
         LR    R1,R2                                                            
         MVC   WORK(2),TCCAYEAR                                                 
         XC    WORK(2),=X'F0F0'    STRIP OFF HIGH NIBBLE                        
         SR    R2,R2               CALCULATE YEAR                               
         IC    R2,WORK                                                          
         MH    R2,=H'10'                                                        
         SR    RF,RF                                                            
         IC    RF,WORK+1                                                        
         AR    R2,RF                                                            
         CH    R2,=H'90'                                                        
         BH    *+8                                                              
         LA    R2,100(R2)                                                       
         STC   R2,WORK             SAVE IT IN WORK                              
*                                                                               
         LA    RF,YEARTAB                                                       
         USING YEARD,RF                                                         
SPCL18A  CLI   0(RF),0             EOT, DON'T GET P&H CALC                      
         BE    SPCL18X                                                          
         CLC   YEARYR,WORK         MAKE SURE YEARS MATCH                        
         BE    SPCL18B                                                          
         LA    RF,YEARNEXT                                                      
         B     SPCL18A                                                          
*                                                                               
SPCL18B  SR    R0,R0                                                            
         ICM   R0,3,YEARPNHT       P&H RATE                                     
         MR    R0,R0               PHANTOM P&H AMOUNT                           
         CVD   R1,DUB                                                           
         SRP   DUB,60,5                                                         
         CVB   R1,DUB                                                           
         MVC   DUB(2),TGBHAND                                                   
         MH    R1,DUB                                                           
         CVD   R1,DUB                                                           
         SRP   DUB,60,5                                                         
         CVB   R1,DUB                                                           
         A     R1,HNDAMT                                                        
         ST    R1,HNDAMT                                                        
*                                                                               
SPCL18X  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*&&                                                                             
*NO-OP ++INCLUDE TAYRTABLE                                                      
         EJECT                                                                  
*              ROUTINE SETS FIXED CYCLE PAYMENT TO PRIMARY COMML BIT            
         SPACE 1                                                                
SETPRI   NTR1  BASE=*,LABEL=*                                                   
         TM    TGUSSTA2,APPREUSE   IF THIS USE CAN ADD FTRACKS                  
         BZ    SPRIX                                                            
*                                                                               
         XC    TGFULL,TGFULL       THEN IT'S A CANDIDATE                        
         OC    TGSSN,TGSSN                                                      
         BZ    SPRI4                                                            
         PACK  DUB,TGSSN           CONVERT S/S NUMBER TO BINARY                 
         CVB   R1,DUB                                                           
         ST    R1,TGFULL                                                        
*                                                                               
SPRI4    L     R2,GUARTAB          LOOP THROUGH GUARANTEE TABLE                 
         L     R0,MAXGUAR          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
         USING GUARD,R2            R2=A(GUARANTEE TABLE)                        
*                                                                               
SPRI6    CLC   GUARSSN,TGFULL      LOOKING FOR THIS S/S NUMBER                  
         BNE   *+14                                                             
         CLC   GUARGUA,TGGUA       MUST MATCH GUARANTEE CODE                    
         BE    SPRI8                                                            
         LA    R2,GUARNEXT                                                      
         BCT   R0,SPRI6                                                         
         B     SPRIX               NOT FOUND                                    
*                                                                               
SPRI8    CLC   GUARCOM,TGCOM       IF WE'RE PAYING PRIMARY COMML                
         BNE   SPRIX                                                            
         OI    TCINPUT,TCINPRI     SET FIXED CYCLE PMT ON PRIMARY COMML         
*                                                                               
SPRIX    XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET SOME COMMERCIAL DETAILS                           
         SPACE 1                                                                
SETCOML  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         MVC   TGCID,TACOCID       SET GLOBAL COMMERCIAL ID                     
         SPACE 1                                                                
         MVC   ACTLEXP,TACOEXP     SAVE ACTUAL EXPIRATION DATE                  
         SPACE 1                                                                
         CLI   PROFEXP,C'Y'        ESTIMATE BEYOND EXPIRY DATE                  
         BNE   *+10                                                             
         MVC   TACOEXP,HEXFFS3                                                  
         SPACE 1                                                                
         NI    STATUS2,ALL-COMLACTV-CASTACTV  SET COMML/CAST INACTIVE           
         NI    STATUS2,ALL-COMLCSF            SET NO CSF FOR COMMERCIAL         
         J     XIT                                                              
HEXFFS3  DC    6X'FF'                                                           
         SPACE 2                                                                
*              ROUTINE ADDS COMMLS TO TABLE & SETS PREVIOUSLY SEEN BIT          
ADDCOM   NTR1  BASE=*,LABEL=*                                                   
         L     R1,COMLTAB          R1=A(ACTUAL COMMERCIAL TABLE)                
         L     R0,MAXCOML          MAXIMUM NUMBER OF ENTRIES IN TABLE           
         SPACE 1                                                                
ACOM10   OC    0(4,R1),0(R1)       TEST LOGICAL END OF TABLE                    
         BZ    ACOM20                                                           
         CLC   TGCOM,0(R1)         TEST THIS COMML ALREADY IN TABLE             
         BE    ACOM30                                                           
         LA    R1,4(R1)            BUMP TO NEXT SPOT                            
         BCT   R0,ACOM10           AND TRY AGAIN                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         JNE   NOTNOW              GIVE ERROR MESSAGE                           
         DC    H'0'                ELSE DIE - INCREASE NCOMLS!                  
         SPACE 1                                                                
ACOM20   MVC   0(4,R1),TGCOM       SAVE THIS COMMERCIAL                         
         NI    STATUS,ALL-SEENCOML SET HAVEN'T SEEN IT BEFORE                   
         J     NO                  RETURN CC NE                                 
         SPACE 1                                                                
ACOM30   OI    STATUS,SEENCOML     SET WE'VE SEEN IT BEFORE                     
         J     YES                 RETURN CC EQ                                 
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         DS    0D                                                               
INIT     NMOD1 0,INIT                                                           
         L     RC,SAVRC                                                         
         SPACE 1                                                                
         MVC   ARATECLC,TASYSCLC   SET A(RATE CALCULATION ROUTINES)             
         LA    R3,ELTACO           SAVE A(COMMERCIAL DETAILS EL.) FOR           
         ST    R3,TCATACO                                     SYSCALC           
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    *+12                                                             
         BAS   RE,INITON           INITIALIZE FOR ONLINE PROCESSING             
         B     *+8                                                              
         BAS   RE,INITOFF          ELSE, INITIALIZE FOR OFFLINE                 
         SPACE 1                                                                
INITX    XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO INITIALIZE OFFLINE PROGRAMS                           
         SPACE                                                                  
INITOFF  NTR1                                                                   
         MVI   OVERLAY,X'09'       GET A(TABLES) FOR OFFLINE PROCESSING         
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,FULL                                                          
*                                                                               
         LR    RE,R3               RE=A(START OF TAGEN09)                       
         LA    R0,NADDTABS         R0=NUMBER OF TABLE ADDRESSES TO SET          
         LA    RF,TAESTABS         RF=A(LIST OF TABLE ADDRESSES)                
         LA    R2,TABMAXES         R2=A(LIST OF TABLE MAXIMUMS)                 
INITOFF2 L     R1,FULL                                                          
         A     R1,0(RE)                                                         
         MVC   0(4,R2),0(R1)       SET TABLE MAXIMUM                            
         LA    R1,4(R1)                                                         
         ST    R1,0(RF)            SET ADDRESS OF TABLE START                   
         LA    RE,4(RE)            BUMP TO NEXT DISPLACEMENT IN TAGEN09         
         LA    RF,4(RF)            BUMP TO NEXT TABLE ADDRESS TO SET            
         LA    R2,4(R2)            BUMP TO NEXT TABLE MAXIMUM TO SET            
         BCT   R0,INITOFF2         LOOP                                         
         SPACE 1                                                                
         MVC   TAINTER,0(RE)       SET ADDRESS OF TAINTER                       
         MVC   TALIM,4(RE)         SET ADDRESS OF TALIM                         
         SPACE 1                                                                
         MVI   OVERLAY,X'9F'       GET A(GLOBAL STORGAE)                        
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,AGLOBAL                                                       
         LR    R2,R3                                                            
         USING GLOBALD,R2                                                       
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     RF,DMCB                                                          
         ST    RF,DRIVER                                                        
         SPACE 1                                                                
         MVI   OVERLAY,X'9E'       GET A(EST SYSDRIVE)                          
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,GLASYSDR                                                      
         SPACE 1                                                                
         MVC   OVERLAY,FORMDPG     LOAD DPG PHASE FOR REPORT FORMAT             
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,GLAPROG                                                       
         SPACE 1                                                                
         LA    R3,OFFSPECS         SET A(REPORT SPECS)                          
         ST    R3,SPECS                                                         
         SPACE 1                                                                
         MVI   OVERLAY,X'06'       GET A(TAESTPRT)                              
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,AESTPRT                                                       
         B     INITX                                                            
         EJECT                                                                  
*              ROUTINE TO INITIALIZE ONLINE PROGRAMS                            
         SPACE                                                                  
INITON   NTR1                                                                   
         MVI   OVERLAY,X'08'       GET A(TABLES) FOR ONLINE PROCESSING          
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,FULL                                                          
*                                                                               
         LR    RE,R3               RE=A(START OF TAGEN08)                       
         LA    R0,NADDTABS         R0=NUMBER OF TABLE ADDRESSES TO SET          
         LA    RF,TAESTABS         RF=A(LIST OF TABLE ADDRESSES)                
         LA    R2,TABMAXES         R2=A(LIST OF TABLE MAXIMUMS)                 
INITON2  L     R1,FULL                                                          
         OC    0(4,RE),0(RE)                                                    
         BZ    INITON5                                                          
         A     R1,0(RE)                                                         
         MVC   0(4,R2),0(R1)       SET TABLE MAXIMUM                            
         LA    R1,4(R1)                                                         
         ST    R1,0(RF)            SET ADDRESS OF TABLE START                   
INITON5  LA    RE,4(RE)            BUMP TO NEXT DISPLACEMENT IN TAGEN09         
         LA    RF,4(RF)            BUMP TO NEXT TABLE ADDRESS TO SET            
         LA    R2,4(R2)            BUMP TO NEXT TABLE MAXIMUM TO SET            
         BCT   R0,INITON2          LOOP                                         
         SPACE 1                                                                
         L     R1,FULL                                                          
         A     R1,0(RE)                                                         
         ST    R1,AREPBLK          A(FAREPORT BLOCK)                            
         SPACE 1                                                                
         L     R1,FULL                                                          
         A     R1,4(RE)                                                         
         ST    R1,AGLOBAL          A(GLOBALD)                                   
         LR    R2,R1                                                            
         USING GLOBALD,R2                                                       
*                                                                               
         L     RE,ATWA                                                          
         AHI   RE,TWAENDLQ                                                      
         ST    RE,GLADTAB          A(DRIVER TABLE)                              
         L     RF,=AL4(DRVTABLQ)                                                
         ST    RF,GLSIZE                                                        
         XCEFL                                                                  
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A8B'   LOAD T00A8B (DROOL)                  
         L     RF,DMCB                                                          
         ST    RF,DRIVER                                                        
         SPACE 1                                                                
         MVI   OVERLAY,X'07'       GET A(ESTIMATE ONLINE DRIVER)                
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,GLASYSDR                                                      
         SPACE 1                                                                
         MVC   OVERLAY,FORMDPG     LOAD DPG PHASE FOR REPORT FORMAT             
         GOTO1 LOADSOPH,DMCB,0     RETURNS ADDRESS IN R3                        
         ST    R3,GLAPROG                                                       
         SPACE 1                                                                
         LA    R3,ONSPECS          SET A(REPORT SPECS)                          
         ST    R3,GLASPECS                                                      
         MVC   GLCOMFAC,ACOMFACS   SET A(COMFACS)                               
*                                                                               
         L     R2,AREPBLK          SET FAREPBLK FOR DROOL USE ONLY              
         USING FAREPBLKD,R2                                                     
         L     R3,ATWA             R3=A(TWA)                                    
         USING T702FFD,R3                                                       
         XC    REPBLK,REPBLK                                                    
         MVC   REPACOM,ACOMFACS    A(COMFACS)                                   
         MVC   REPAPQB,ATIA        A(PRINT QUEUE BUFFER)                        
         LA    R1,HEAD1                                                         
         ST    R1,REPABUF          A(PRINT BUFFER)                              
         OI    REPHEADI,REPHCLRA   CLEAR HEADLINE AFTER PRINTING                
         MVI   REPPRNTI,REPPCLRA   CLEAR PRINTLINE AFTER PRINTING               
         MVI   REPHEADH,1          INORDER TO GET A HOOK                        
         MVI   REPHEADN,10         NUMBER OF HEADLINES                          
         MVI   REPPRNTN,2          NUMBER OF PRINT LINES                        
         MVI   REPWIDTH,REPWREGQ   SET REGULAR REPORT WIDTH                     
         MVC   REPSYSID,RCPROG     SYSTEM ID                                    
         MVC   REPPRGID,RCPROG+2   PROGRAM ID                                   
         MVC   REPUSRID,TWAORIG    USER ID NUMBER                               
         MVC   REPSUBID,REMUSER    REPORT SUB-ID                                
         MVC   REPCTRY,CTRY        ACTUAL COUNTRY                               
         MVC   REPLANG,LANG        ACTUAL LANGUAGE                              
         GOTO1 DATCON,DMCB,(1,TGTODAY1),(3,REPDATE)  BINARY YMD                 
         B     INITX                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS FOR OFFLINE PRINTING                                       
         SPACE 1                                                                
OFFSPECS DS    0C                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,58,C'TALENT ESTIMATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SSPEC H1,97,REPORT                                                     
         SSPEC H1,111,PAGE                                                      
         SSPEC H2,97,REQUESTOR                                                  
         DC    X'00'                                                            
         SPACE 2                                                                
*              SPECS FOR ONLINE PRINTING                                        
         SPACE 1                                                                
ONSPECS  DS    0C                                                               
         SPEC  H1,2,RUN                                                         
         SPEC  H1,32,C'TALENT ESTIMATE'                                         
         SPEC  H2,32,15X'BF'                                                    
         SPEC  H1,52,REPORT                                                     
         SPEC  H1,65,PAGE                                                       
         SPEC  H2,52,REQUESTOR                                                  
         SPEC  END                                                              
         EJECT                                                                  
*              ROUTINE TO FILTER ACTUALS                                        
*              EXCLUDE UNBILLED INV & FILTERS ACTUALS BY ESTIMATE               
         SPACE 1                                                                
         DS    0D                                                               
         USING TASYSIOD,R2                                                      
FILTACT  NMOD1 0,FLTACT                                                         
         L     RC,SAVRC                                                         
*                                                                               
         OC    TIBIDATE,TIBIDATE   IF INVOICE NOT MARKED BILLED                 
         BNZ   *+12                                                             
         CLI   ACTLOPT,C'Z'        AND DON'T WANT UNBILLED INVOICES             
         BE    CKFNO               SKIP                                         
*                                                                               
         CLI   FILTOPT,C'N'        FILTER ACTUALS ON ESTIMATE                   
         BE    CKFYES                                                           
*                                                                               
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTEST  SET ESTIMATE IN TGNAME          
         BNE   CKFNO                                                            
         OC    ERPEST,SPACES       PAD ESTIMATES REPORTING ON W/SPACES          
         OC    ERPES2,SPACES                                                    
         OC    ERPES3,SPACES                                                    
         OC    ERPES4,SPACES                                                    
*                                  CHECK FOR A FULL MATCH                       
         CLC   ERPEST,TGNAME                                                    
         BE    CKFYES                                                           
         CLC   ERPES2,TGNAME                                                    
         BE    CKFYES                                                           
         CLC   ERPES3,TGNAME                                                    
         BE    CKFYES                                                           
         CLC   ERPES4,TGNAME                                                    
         BE    CKFYES                                                           
*                                                                               
         CLI   FILTOPT,C'P'        TEST PARTIAL MATCH OKAY                      
         BNE   CKFNO                                                            
*                                  CHECK FOR A PARTIAL MATCH                    
         LA    R1,ERPEST                                                        
         BAS   RE,PMATCH                                                        
         BE    CKFYES                                                           
         LA    R1,ERPES2                                                        
         CLC   0(L'ERPEST,R1),SPACES                                            
         BE    *+12                                                             
         BAS   RE,PMATCH                                                        
         BE    CKFYES                                                           
         LA    R1,ERPES3                                                        
         CLC   0(L'ERPEST,R1),SPACES                                            
         BE    *+12                                                             
         BAS   RE,PMATCH                                                        
         BE    CKFYES                                                           
         LA    R1,ERPES4                                                        
         CLC   0(L'ERPEST,R1),SPACES                                            
         BE    *+12                                                             
         BAS   RE,PMATCH                                                        
         BE    CKFYES                                                           
         B     CKFNO                                                            
*                                                                               
CKFYES   XR    RC,RC               EXITS                                        
CKFNO    LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TESTS IF ESTIMATE NUMBER IN TGNAME AND TANU EL           
*              AT TGELEM MATCHES THE RIGHT SIDE OF REPORTING ESTIMATES          
*              SETS CC EQUAL IF IT DOES, ELSE CC NOT EQUAL                      
         SPACE                                                                  
*                                  NTRY - R1=REPORTING ESTIMATE                 
         USING TANUD,R4                                                         
PMATCH   NTR1                                                                   
         LR    R2,R1               R2=START OF REPORTING ESTIMATE               
*                                                                               
         L     R4,TGELEM                                                        
         ZIC   RE,TANULEN                                                       
         SH    RE,=Y(TANULNQ)      RE=LENGTH OF TANUMBER                        
         SPACE                                                                  
         LA    R3,L'ERPEST         SET ACTUAL LENGTH OF ESTIMATE IN R3          
         LR    R1,R2                                                            
         AR    R1,R3               START FROM END OF ESTIMATE                   
PM5      BCTR  R1,0                GO FORWARD & LOOK FOR NON-SPACE CHAR         
         CLI   0(R1),X'40'         IF A SPACE                                   
         BNE   PM10                                                             
         BCT   R3,PM5              DECREMENT LENGTH                             
         DC    H'0'                ESTIMATE ALL SPACES                          
         SPACE                                                                  
PM10     SR    R3,RE               L'ESTIMATE - L'TANUMBER IS N'CHARS           
         LR    R1,R2                                                            
         AR    R1,R3               TO SKIP IN EST FOR PARTIAL MATCH             
*                                  R1=A(CHAR IN EST TO START MATCH)             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),TGNAME                                                   
         BE    CKFYES              SET CC EQUAL IF MATCH                        
         B     CKFNO               ELSE SET CC NOT EQUAL                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE PRINTS NARRATIVE & SIGNATURE LINES                       
         SPACE 1                                                                
         DS    0D                                                               
PRTEND   NMOD1 0,PRTEND                                                         
         L     RC,SAVRC                                                         
*                                                                               
         CLC   NARR,SPACES         PRINT NARRATIVE AT END OF REPORT             
         BNH   PRTEND5                                                          
         LA    RF,L'NARR                                                        
         GOTO1 CHOPPER,DMCB,((RF),NARR),(60,P+1),(C'P',2)                       
         BAS   RE,PRNTIT2                                                       
         SPACE 1                                                                
PRTEND5  CLI   SIGOPT,C'Y'         IF REQUEST SIG LINES                         
         BNE   PRTENDX                                                          
         BAS   RE,PRTSIG           PRINT THEM                                   
PRTENDX  XIT1                                                                   
         SPACE 2                                                                
*              ROUTINE TO PRINT SIGNATURE LINES                                 
         SPACE 1                                                                
PRTSIG   NTR1                                                                   
         XC    HEADHOOK,HEADHOOK   CLEAR A(HEADLINE HOOK)                       
         ZIC   R1,LINE             IF FOOTLNS DON'T FIT ON CURRENT PG           
         AHI   R1,9                                                             
         CLM   R1,1,MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         BAS   RE,PRNTIT2          ELSE, JUST SKIP A LINE                       
*                                                                               
         MVC   P+2(L'LTAPPBY),LTAPPBY                                           
         BAS   RE,PRNTIT2                                                       
         BAS   RE,PRNTIT2                                                       
         MVC   P+2(L'LTACEXEC),LTACEXEC                                         
         BAS   RE,PRNTIT2                                                       
         MVC   P+19(36),=47X'BF'                                                
         BAS   RE,PRNTIT2                                                       
         MVC   P+2(L'LTCLIENT),LTCLIENT                                         
         BAS   RE,PRNTIT2                                                       
         MVC   P+8(47),=47X'BF'                                                 
         BAS   RE,PRNTIT2                                                       
         MVC   P+26(L'LTDATE),LTDATE                                            
         BAS   RE,PRNTIT2                                                       
         MVC   P+30(25),=47X'BF'                                                
         BAS   RE,PRNTIT2                                                       
         B     PRTENDX                                                          
         SPACE 2                                                                
PRNTIT2  NTR1                                                                   
         TM    WHEN,X'80'          DON'T BOTHER IF ON SCREEN                    
         BO    PRTENDX                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTENDX                                                          
         SPACE 2                                                                
LTAPPBY  DC    CL12'APPROVED BY:'                                               
LTACEXEC DC    CL17'ACCOUNT EXECUTIVE:'                                         
LTCLIENT DC    CL6'CLIENT'                                                      
LTDATE   DC    CL4'DATE'                                                        
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DECIDES WHETHER TO PAY A USE                             
         SPACE 1                                                                
         DS    0D                                                               
DECIDE   NMOD1 0,DECIDE                                                         
         L     RC,SAVRC                                                         
*                                                                               
         TM    STATUS,HYPOPERF     IF PROCESSING REAL PERFORMER ...             
         BO    DEC1Z                                                            
*                                                                               
         CLI   TGUSEQU,UVRE        ... AND USE IS VRE                           
         BE    DEC1A                                                            
         CLI   TGUSEQU,UVNR        OR VRN                                       
         BNE   DEC1B                                                            
DEC1A    CLC   TCCAYEAR,=C'13 '    EXCLUDE CONTRACT YEARS PRIOR TO 13           
         BL    DECNO                                                            
         B     DEC1Z                                                            
*                                                                               
DEC1B    CLI   TGUSEQU,UINU        IF MAKING INU                                
         BE    DEC1C                                                            
         CLI   TGUSEQU,UNMU        OR NMU                                       
         BE    DEC1C                                                            
         CLI   TGUSEQU,USIU        OR SIU                                       
         BE    DEC1C                                                            
         CLI   TGUSEQU,USNU        OR SNU                                       
         BNE   DEC1D                                                            
DEC1C    CLI   TGUSTYP,UINUI4W     4 WEEK PAYMENT                               
         BL    DEC1Z                                                            
         CLC   TCCAYEAR,=C'09 '    SKIP PERFORMERS FROM EARLIER THAN            
         BL    DECNO               CONTRACT YEAR 09                             
         B     DEC1Z                                                            
*                                                                               
DEC1D    CLI   TGUSEQU,UMVI        IF MAKING MVI                                
         BE    DEC1E                                                            
         CLI   TGUSEQU,USMI        OR SMI                                       
         BE    DEC1E                                                            
         CLI   TGUSEQU,UMVN        OR MVN                                       
         BE    DEC1E                                                            
         CLI   TGUSEQU,USMN        OR SMN                                       
         BNE   DEC1F                                                            
DEC1E    CLI   TGUSTYP,UMVII4W     4 WEEK PAYMENT                               
         BL    DEC1Z                                                            
         CLC   TCCAYEAR,=C'09 '    SKIP PERFORMERS FROM EARLIER THAN            
         BL    DECNO               CONTRACT YEAR 09                             
         B     DEC1Z                                                            
*                                                                               
DEC1F    CLI   TGUSEQU,USOM        IF MAKING SOM PYMT (SOCIAL MEDIA)            
         BNE   DEC1Z                                                            
         CLC   TCCAYEAR,=C'16 '    SKIP PERFORMERS FROM EARLIER THAN            
         BL    DECNO               CONTRACT YEAR 16                             
*                                                                               
DEC1Z    MVC   BYTE,TGUSXCAT       TEST WHETHER CAT EXCLUDED FROM USE           
         NC    BYTE,TGCATYPE                                                    
         BNZ   DECNO               YES - DON'T PAY                              
         SPACE 1                                                                
         MVC   BYTE,TGUSMEDS       TEST WHETHER USE VALID FOR MEDIA             
         NC    BYTE,TGMEEQU                                                     
         BZ    DECNO               NO - DON'T PAY                               
         SPACE 1                                                                
         CLI   TGUNEQU,NON         IF UNION NON                                 
         BNE   DEC2                                                             
*        MVC   BYTE,TGCAUNI        AND IF ANY ALLOWABLE UNIONS FOR CAST         
*        XI    BYTE,NON            (NOT INCLUDING NON)                          
*        MVC   TGBYTE,TGUSXUNI                                                  
*        XI    TGBYTE,X'FF'                                                     
*        NC    TGBYTE,BYTE         IS EXCLUDED FROM THE USE                     
*        BZ    DECNO               THEN DON'T PAY                               
         MVC   DUB(4),TGCAUNIS                                                  
         XI    DUB,NON                                                          
         MVC   DUB+4(4),TGUSXUNS                                                
         XC    DUB+4(4),=4X'FF'                                                 
         GOTO1 UNITEST,DMCB,(X'80',DUB),DUB+4                                   
         BZ    DECNO                                                            
         B     DEC4                                                             
         SPACE 1                                                                
*EC2     MVC   BYTE,TGUSXUNI       TEST WHETHER UNION EXCLUDED FROM USE         
*        NC    BYTE,TGUNEQU                                                     
DEC2     GOTO1 UNITEST,DMCB,(X'80',TGUNEQUS),TGUSXUNS                           
         BNZ   DECNO               YES - DON'T PAY                              
         SPACE 1                                                                
DEC4     CLI   ESTMODE,PROCAUTO    IF THIS ISN'T AN AUTO PAYMENT                
         BE    DEC5                                                             
         TM    TGUSSTA2,HLDTYPE    AND NOT A HOLDING FEE PAYMENT                
         BO    DEC5                                                             
         TM    TCUSETAB+3,TCLFTPRO 1ST LIFT?                                    
         BZ    DEC4A                                                            
         TM    TCCASTAT,TACASTLF                                                
         BO    DEC6                                                             
         TM    TCCASTAT,TACASTLO                                                
         BO    DEC6                                                             
         TM    TCCASTA4,TACASALL                                                
         BO    DEC6                                                             
         TM    TCCASTA4,TACASL2O                                                
         BO    DEC6                                                             
         B     DECNO                                                            
DEC4A    TM    TCUSETAB+3,TCLFTPR2 2ND LIFT?                                    
         BZ    DEC4B                                                            
         TM    TCCASTA4,TACAST2O                                                
         BO    DEC6                                                             
         TM    TCCASTA4,TACAS2LF                                                
         BO    DEC6                                                             
         TM    TCCASTA4,TACASALL                                                
         BO    DEC6                                                             
         TM    TCCASTA4,TACASL2O                                                
         BO    DEC6                                                             
         B     DECNO                                                            
DEC4B    TM    TCCASTA4,TACASALL   ALL LIFTS                                    
         BO    DEC6                                                             
         TM    TCCASTAT,TACASTLO                                                
         BNZ   DECNO                                                            
         TM    TCCASTA4,TACAST2O+TACASL2O                                       
         BNZ   DECNO                                                            
         B     DEC6                                                             
         SPACE 1                                                                
DEC5     OC    NAPDATE,NAPDATE     FOR AUTO PAYMENTS - IF NO AUTO PYMTS         
         BZ    DEC6                AS/OF DATE DEFINED                           
         CLC   TCPCYCS,NAPDATE     REJECT IF CYCLE START EQ/AFTER DATE          
         BNL   DECNO                                                            
         SPACE 1                                                                
DEC6     TM    TGUSSTA2,HLDTYPE    IF PAYING HOLDING FEE                        
         BZ    DEC10                                                            
         TM    CSFSTAT,COMSSMW     IF SOCIAL MEDIA WAIVER STATUS ON             
         BO    DECNO               REJECT                                       
         CLC   TCCAONOF,=C'OFF'    EXCLUDE SOME OFF-CAMERA PERFS.               
         BNE   DEC8                                                             
         TM    TGCATYPE,NOHLDOFF   NO HOLDS IF OFF-CAMERA                       
         BO    DECNO                                                            
         CLI   TGYREQU,CN88        AS OF '88 CONTRACT                           
         BL    DEC8                                                             
         TM    TGCATYPE,NHLDOF88   EXCLUDE OTHERS                               
         BO    DECNO                                                            
DEC8     B     DEC12               SKIP NO REUSE TEST                           
         SPACE 1                                                                
DEC10    TM    TGUSSTAT,SESSION    IF THIS ISN'T A SESSION (OR HLD)             
         BO    *+12                                                             
         TM    TGCATYPE,NOREUSE    EXCLUDE SOME CATEGORIES                      
         BO    DECNO                                                            
         SPACE 1                                                                
DEC12    OC    MYCALAST,MYCALAST   IF DATE OF LAST SERVICE DEFINED              
         BZ    *+14                                                             
         CLC   TCPCYCS,MYCALAST    REJECT IF CYCLE STARTS AFTER                 
         BH    DECNO                                                            
         SPACE 1                                                                
         LA    R3,ELTACO           R3=A(COMMERCIAL DETAILS EL.)                 
         USING TACOD,R3                                                         
         OC    TACOEXP,TACOEXP     IF WE HAVE EXPIRATION DATE                   
         BZ    *+14                                                             
         CLC   TCPCYCS,TACOEXP     REJECT IF CYCLE STARTS AFTER EXPIRY          
         BH    DECNO                                                            
         SPACE 1                                                                
         L     RE,ACOMEL           RE=A(COMMERCIAL ELEMENT)                     
         USING TAESD,RE                                                         
         CLI   TAESTYPE,TAESTCOM   IF ACTUAL COMMERCIAL,                        
         BNE   DEC15                                                            
         DROP  RE                                                               
         CLI   TGUSEQU,USOM        AD IF SOCIAL MEDIA PAYMENT                   
         BNE   DEC15                                                            
         TM    CSFSTAT,COMSSMW     SOCIAL MEDIA STATUS MUST BE ON               
         BZ    DECNO                                                            
         SPACE 1                                                                
DEC15    CLI   TGUSEQU,UBSS        IF USE IS BSS                                
         BNE   DECYES              AND MEDIA IS INTERNET OR NEW MEDIA           
         TM    TGMEEQU,INTERNET+NEWMEDIA                                        
         BZ    DECYES                                                           
         TM    TGCATYPE,EXTRA      AND PAYING AN EXTRA                          
         BZ    DECYES                                                           
         CLC   TGCAT,=C'EXB'       EXLCUDE ALL EXCEPT EXB                       
         BE    DECYES                                                           
         CLC   TGCAT,=C'HMB'       AND HMB                                      
         BNE   DECNO                                                            
         DROP  R3                                                               
DECYES   XR    RC,RC               YES EXIT                                     
DECNO    LTR   RC,RC               NO EXIT                                      
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE UPDATES USAGE HISTORY TABLE AFTER PAYMENT                
         SPACE 1                                                                
         DS    0D                                                               
HISTUPD  NMOD1 0,HSTUPD                                                         
         L     RC,SAVRC                                                         
*                                                                               
         L     R2,AHISTENT         R2=A(USAGE HISTORY TABLE)                    
         LTR   R2,R2                                                            
         BZ    HUPDX                                                            
         USING HISTD,R2                                                         
         SPACE 1                                                                
         CLI   HISTUSE,UITN        IF ITN                                       
         BE    *+8                                                              
         CLI   HISTUSE,UPAX        IF PAX                                       
         BE    *+8                                                              
         CLI   HISTUSE,UCLA        IF CLASS A                                   
         BNE   HUPD4                                                            
         XR    R1,R1                                                            
         ICM   R1,3,HISTUSN        N'USES PAID PREVIOUSLY                       
         AH    R1,TCTUSES          + N'USES PAID THIS TIME                      
         STCM  R1,3,HISTUSN        = TOTAL N'USES PAID SO FAR                   
         SPACE 1                                                                
         TM    TCUSETAB+3,TCLFTPRO+TCLFTPR2 AND IF PAYMENT TO LIFT              
         BNO   HUPDX                                                            
         ICM   R1,3,HISTUSNL       N'USES PAID TO LIFT PREVIOUSLY               
         AH    R1,TCTUSES          + N'USES PAID TO LIFT THIS TIME              
         STCM  R1,3,HISTUSNL       = TOTAL N'USES PAID TO LIFT SO FAR           
         B     HUPDX                                                            
         SPACE 1                                                                
HUPD4    TM    TGUSTYST,INSERTS    IF INSERTS FOR BOOKENDS                      
         BZ    *+10                                                             
         MVC   HISTINS,TCINSRTS    SAVE N'INSERTS                               
         TM    TGUSTYST,MAJORS                                                  
         BZ    *+10                                                             
         MVC   HISTMAJ,TCMAJORS    SAVE MAJORS                                  
         TM    TGUSTYST,UNITS                                                   
         BZ    *+10                                                             
         MVC   HISTUNIT,TCUNITS         UNITS                                   
         SPACE 1                                                                
         TM    TGMEEQU,LIKETV+CABLE IF MEDIA IS TV,INT,NEW OR CABLE             
         BZ    HUPD6                                                            
         TM    TGUSSTA2,APPREUSE   AND USE ADDS FTRACKS                         
         BZ    HUPD6                                                            
         MVC   HISTUSE,HLDUSE      SAVE IN TABLE AS FIRST HLD                   
         SPACE 1                                                                
HUPD6    CLC   HISTUSE,HLDUSE      IF HOLDING FEE                               
         BNE   HUPD8                                                            
         CLC   LASTHLD,HISTCYCE    AND SAVED LAST END IS LESS THEN THIS         
         BNL   *+10                                                             
         MVC   LASTHLD,HISTCYCE    SAVE LAST END DATE                           
         SPACE 1                                                                
HUPD8    CLI   HISTUSE,UMUS        IF MUSIC PAYMENT                             
         BE    *+12                                                             
         CLI   HISTUSE,UFMU        OR FIRST MUSIC PAYMENT                       
         BNE   HUPDX                                                            
         MVI   HISTUSE,UMUS        FORCE SAVE AS MUSIC PAYMENT                  
         CLC   LASTMUS,HISTCYCE    IF SAVED LAST END IS LESS THEN THIS          
         BNL   *+10                                                             
         MVC   LASTMUS,HISTCYCE    THEN SAVE NEW LAST END DATE                  
         SPACE 1                                                                
HUPDX    XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CALCULATES TAX AND HANDLING                              
*              LOOK AT CLCTNH ROUTINE FOR ESTIMATED PAYMENTS                    
         SPACE 1                                                                
         USING TASYSIOD,R2         R2=A(SYSIO WORKING STORAGE)                  
         DS    0D                                                               
TNHCALC  NMOD1 0,TNHCALC                                                        
         L     RC,SAVRC                                                         
         OC    TIBIDATE,TIBIDATE   GET OUT IF INVOICE MARKED BILLED             
         BNZ   TXIT                                                             
         CLI   ACTLOPT,C'U'        OR IF DON'T WANT TO CALCULATE TNH            
         BE    TXIT                                                             
         CLI   OFFLINE,C'Y'        OR IF ONLINE                                 
         BNE   TXIT                                                             
*                                                                               
         LA    R3,TALIMD           R3=A(TALIM BLOCK)                            
         USING TMD,R3                                                           
         XCEF  (R3),'TMLNQ'        CLEAR IT                                     
                                                                                
         LA    RF,TXDSCT                                                        
         XC    0(256,RF),0(RF)     CLEAR IT                                     
         ST    RF,TMXTDSCT                                                      
*                                                                               
         ST    RC,TMRC             A(GEND)                                      
         MVC   TMTRACE,TRACEOPT    TRACE OPTION                                 
         MVC   TMEFDTE0,TGTODAY0   EFFECTIVE DATE                               
         MVC   TMCURR,TICUR        CURRENCY                                     
         MVC   TMEMP,TIEMP         EMPLOYER                                     
         MVI   TMSTAT,TMSBIL       CALC. BASED ON BILLING RATES                 
         MVC   TMBILTYP,PROFBTYP   SET BILLING TYPE                             
         MVC   TMHNDRLS,PROFHRUL   SET HANDLING RULES INFO                      
         MVC   TMOFCDE,TGOFF       OFFICE CODE                                  
*                                                                               
         LA    R1,SYSCOMM          A(SYSTEM COMMON ROUTINES)                    
         ST    R1,TMASYCOM                                                      
*                                                                               
         LA    R1,TGBSRC           MOVE HALFWORD BILLING RATES TO               
         LA    RF,TMBRATES         FULLWORD BILLING RATES IN TALIMD             
         LA    R0,TMRATEN          R0=N'BILLING RATES                           
         MVC   2(2,RF),0(R1)                                                    
         LA    R1,2(R1)            BUMP TO NEXTS                                
         LA    RF,4(RF)                                                         
         BCT   R0,*-14             AND LOOP                                     
*                                  INITIALIZE CAST-SPECIFIC FIELDS              
TNH4     MVC   TMW4TYPE,TCW4TYPE   W4 TYPE                                      
         MVC   TMSSN,TISSN         S/S NUMBER                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT IN CHECK            
         BAS   RE,GETEL2                                                        
         BNE   TNH5                                                             
         USING TACAD,R4                                                         
*                                                                               
         MVC   WORK(2),TACAYEAR    SAVE CAST YEAR                               
         XC    WORK(2),=X'F0F0'                                                 
         ZIC   RE,WORK                                                          
         MHI   RE,10                                                            
         ZIC   R1,WORK+1                                                        
         AR    RE,R1                                                            
         CHI   RE,90                                                            
         BH    *+8                                                              
         AHI   RE,100                                                           
         STC   RE,TGYREQU                                                       
         MVC   TGGUA,TACAGUA       AND GUARANTEE CODE                           
*                                                                               
         MVC   TMUNIT,TACAUNIT     SET STATE OF WORK                            
         MVC   TMWUNIT,TACAUNIT    SET STATE OF WORK                            
         GOTO1 =A(GETPSTR),DMCB,TACAUNIT                                        
         CLI   TACACORP,0          IF CORP CODE DEFINED                         
         BE    TNH5                                                             
         MVI   ELCODE,TATIELQ      SET TO LOOK UP CORP ID EL IN CHECK           
         MVI   HALF,TATITYCO       SCAN FOR CORP ID ELEMENT TYPE                
         MVC   HALF+1(1),TACACORP  AND FOR THIS CORP CODE                       
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   TNH5                                                             
         L     R4,TGELEM           R4=A(TAX ID EL. FOR CORP)                    
         USING TATID,R4                                                         
         MVC   TMSSN,TATIID        SET CORP ID NUMBER IN LOCAL W/S              
         SPACE 1                                                                
TNH5     BRAS  RE,GUARBLD                                                       
         BRAS  RE,GUARUPD                                                       
         SPACE 1                                                                
         MVC   TMTXEARN,TCPAYI     TAXABLE EARNINGS                             
         MVC   TMNTEARN,TCPAYC     NON-TAXABLE EARNINGS                         
         SPACE 1                                                                
         CLI   TMCURR,C'C'         IF CANADIAN                                  
         BNE   TNH10                                                            
         L     R1,TMNTEARN                                                      
         A     R1,CSFAMT           ADD CONTRACT SERVICE FEE                     
         A     R1,TCEXP            ADD REIMB EXP (TALIM WILL DEDUCT)            
         ST    R1,TMNTEARN         TO NON-TAXABLE EARNINGS                      
         MVC   TMINR,TCINR         AND SET I&R AMOUNT                           
         SPACE 1                                                                
*                                                                               
         L     R1,TMNTEARN         ADD INR BACK INTO EARNINGS                   
         A     R1,TCINR                                                         
         ST    R1,TMNTEARN                                                      
*                                                                               
         BAS   RE,CHKGSTNO         IF PERFORMER HAS GST # ON W4                 
         BNE   TNH25                                                            
         CLI   TGUSEQU,UCPN        AND IF NOT CPN PAYMENT                       
         BE    TNH25                                                            
         OI    TMSTAT2,TMSGSTU     CALCULATE GST ON EARNINGS                    
         B     TNH25                                                            
         SPACE 1                                                                
TNH10    CLC   TGUNI,=C'ACT'       IF US$ ACTRA PAYMENT                         
         BE    TNH11                                                            
         CLC   TGUNI,=C'UDA'       OR UDA                                       
         BE    TNH11                                                            
         CLC   TGUNI,=C'NON'       OR NON CANADA                                
         BNE   TNH12                                                            
         CLC   TGLCL,=C'CAN'                                                    
         BNE   TNH12                                                            
TNH11    CLI   TMW4TYPE,TAW4TYCA   IF CANADIAN                                  
         BE    *+12                                                             
         CLI   TMW4TYPE,TAW4TYCO   OR CORP WITH STATE OF CN                     
         BNE   TNH12                                                            
***      CLC   TMUNIT,=C'CN '                                                   
***      BNE   TNH12                                                            
         XC    TMBRWCRP,TMBRWCRP                                                
                                                                                
         L     R1,TMNTEARN         ADD INR BACK INTO EARNINGS                   
         A     R1,TCINR                                                         
         ST    R1,TMNTEARN                                                      
         MVC   TMINR,TCINR         AND SET I&R AMOUNT                           
         BAS   RE,CHKGSTNO         IF PERFORMER HAS GST # ON W4                 
         BNE   TNH12                                                            
         CLI   TGUSEQU,UCPN        AND IF NOT CPN PAYMENT                       
         BE    *+8                                                              
         OI    TMSTAT2,TMSGSTU     CALCULATE GST ON EARNINGS                    
         SPACE 1                                                                
TNH12    CLI   TMW4TYPE,TAW4TYCA   IF CANADIAN                                  
         BE    *+12                                                             
         CLI   TMW4TYPE,TAW4TYCO   OR CORPORATION                               
         BNE   TNH15                                                            
         MVC   TGCTRY,=C'CA'       OR WORKING IN CANADA                         
         GOTO1 TAXVAL,DMCB,(X'FF',TMWUNIT)                                      
         BE    TNH13                                                            
         CLC   TGCAT,=C'ZZZ'       AND CATEGORY IS ZZZ                          
         BNE   TNH15                                                            
TNH13    XC    TMBRWCRP,TMBRWCRP   DON'T CALCULATE WORKMEN'S COMP               
         SPACE 1                                                                
TNH15    CLI   TMW4TYPE,TAW4TYIN   IF INDIVIDUAL OR ESTATE                      
         BE    *+12                                                             
         CLI   TMW4TYPE,TAW4TYES                                                
         BNE   *+14                                                             
         MVC   TMNTEARN,TCEXP      CALC HANDLING ON REIMBURSED                  
         B     TNH20                                                            
         L     R1,TMNTEARN         FOR CORP CALC TAX/HANDLING ON                
         A     R1,TCEXP            ENTIRE AMOUNT                                
         ST    R1,TMNTEARN                                                      
TNH20    OC    TMINR,TMINR         CANNOT HAVE I&R AND P&H                      
         BNZ   *+10                                                             
         MVC   TMPNH,TCPNH         P&H (NEEDED FOR SOME BILL TYPES)             
         MVC   TMHNW,TCHNW         H&W                                          
*                                                                               
         CLI   TMW4TYPE,TAW4TYCO   IF THIS IS A CORPORATION                     
         BE    TNH21                                                            
         CLI   TMW4TYPE,TAW4TYCA   OR A CANADIAN                                
         BE    TNH21                                                            
         CLI   TMW4TYPE,TAW4TYFO   OR A FOREIGNER                               
         BNE   TNH22                                                            
TNH21    L     R1,TMNTEARN                                                      
         A     R1,TCHNW            ADD HNW BACK INTO TAXABLE                    
         ST    R1,TMNTEARN                                                      
         B     TNH25                                                            
TNH22    L     R1,TMTXEARN         ADD HNW BACK INTO TAXABLE                    
         A     R1,TCHNW            FOR TALIM                                    
         ST    R1,TMTXEARN                                                      
         SPACE 1                                                                
TNH25    XC    TMREXP,TMREXP                                                    
**NO-OP  CLI   TGUSEQU,UBSC        TALIM NEEDS REIMB EXP TO SUBTRACT            
**MAR15  BNE   *+10                FROM GST/PST & WC FOR ALL PAY TYPES          
         MVC   TMREXP,TCEXP                                                     
         SPACE 1                                                                
         BRAS  RE,SETTAXBL         SET TAXABLE STATE (TMUNIT)                   
         BAS   RE,GETYTD           GET YTD FOR THIS S/S NUMBER                  
*        GOTO1 =A(GETPSTR),DMCB,TABSTATE   GET PST RATE AND HST INDICAT         
         GOTO1 TALIM,DMCB,TMD      CALC. TAX AND HANDLING                       
                                                                                
         BAS   RE,INRHND18         CALC HANDLING ON I&R FOR BTYPE 18            
         BAS   RE,CKAOSPST         SEE IF THERE IS AOS PST                      
                                                                                
         CLI   TRACEOPT,C'N'       OPTIONAL TRACE                               
         BE    TNH60                                                            
         GOTO1 TRACE,DMCB,TMD,TMLNQ,=C'TALIM',5                                 
         SPACE 1                                                                
TNH60    CLI   TICUR,C'U'          IF US$ PAYMENT                               
         BNE   TNH65                                                            
         CLI   CSFCHK,C'Y'         AND CSF UNION CHECK                          
         BNE   TNH65                                                            
         XC    TCPAYC,TCPAYC       DON'T SET PYMT AMT (USED FOR HNDLNG)         
         SPACE 1                                                                
TNH65    L     R1,TMXTOTAL         R1 = PAYROLL TAXES                           
         S     R1,TMXFICR                                                       
         TM    TGSYSTA2,TASYSNCS   ALLOW TO SUI SURCHARGE IT?                   
         BZ    TNH67                                                            
         A     R1,TMSUISUR         ADD IN SUI SURCHARGE                         
TNH67    OC    TMOGST,TMOGST       IF GST CALCULATED                            
         BNZ   TNH68                                                            
         OC    TMOPST,TMOPST       IF PST CALCULATED                            
         BZ    TNH69                                                            
TNH68    L     R1,TMOGST           USE THAT AS PAYROLL TAXES                    
         A     R1,TMOPST                                                        
TNH69    L     R0,TMOHAND          R0 = INDIV. HANDLING                         
         L     RF,TMOCORP          RF = CORP. HANDLING                          
         A     RF,TMOCAN                                                        
         CLI   PROFBTYP,TABRTY8    IF BILLING TYPE 8 OR,                        
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY21   IF BILLING TYPE 21 OR,                       
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY7    IF BILLING TYPE 7 OR,                        
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY6    IF BILLING TYPE 6 OR,                        
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY1    IF BILLING TYPE 1 OR,                        
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY2    IF BILLING TYPE 2 OR,                        
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY18   IF BILLING TYPE 18 OR,                       
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY12   IF TYPE 12 OR,                               
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY22   IF TYPE 22                                   
         BE    TNH70                                                            
         CLI   PROFBTYP,TABRTY23   IF TYPE 23                                   
         BNE   TNH80                                                            
TNH70    AR    RF,R0               ADD BOTH HANDLING CHARGES TOGETHER           
         B     *+6                                                              
TNH80    AR    R1,R0               ELSE ADD INDIV. HANDLING TO TAXES            
         ST    R1,TAXAMT           SAVE PAYROLL TAXES AMOUNT                    
         A     RF,GRTHND                                                        
         ST    RF,HNDAMT           SAVE HANDLING AMOUNT                         
         XC    GRTHND,GRTHND                                                    
         SPACE 1                                                                
         BRAS  RE,CLCCOMM                                                       
         BRAS  RE,CLCSIGN          CALCULATE SIGNATORY FEE                      
         BRAS  RE,CLCCSF           CALCULATE COMMERCIAL SERVICE FEE             
         SPACE 1                                                                
         L     R0,AIO                                                           
         MVC   AIO,TIAMAIN                                                      
         MVI   ELCODE,TAPAELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAPATHND))                                     
         ST    R0,AIO                                                           
         BNE   TNH90                                                            
         L     R4,TGELEM                                                        
         USING TAPAD,R4                                                         
         MVC   HNDAMT,TAPADATA                                                  
         XC    TAPADATA(4),TAPADATA   CLEAR AMT - ONLY ONCE FOR INVOICE         
         SPACE 1                                                                
TNH90    MVC   KEY,TIKEY           SET TO RE-ESTABLISH SYSIO'S READ SEQ         
         MVC   FILENAME,=CL8'CHKDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         SPACE 1                                                                
TXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE CHECKS IF GST NUMBER ON W4 RECORD                        
CHKGSTNO NTR1                                                                   
         OI    STATUS3,RERDKEY     SET TO RE-READ SYSIO'S KEY                   
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'20',0)  GET W4 RECORD                     
         BNE   CGSTNOX                                                          
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTGST))                                     
CGSTNOX  MVC   AIO,TIAREC                                                       
         B     TXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO FIND AOS PST IN THE CHECK RECORD                  *         
***********************************************************************         
CKAOSPST NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAMAELQ      GET CAST DETAILS ELEMENT IN CHECK            
         BAS   RE,GETEL2                                                        
         BNE   TXIT                                                             
         USING TAMAD,R4                                                         
         BNE   TXIT                                                             
*                                                                               
         ICM   R1,15,TAMAASPA      ADD IN AOS PST                               
         A     R1,TMOPST           PST CALCULATED AMOUNT                        
         ST    R1,TMOPST                                                        
         B     TXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE, FOR BILLING TYPE 18, TO CALCULATE HANDLING          *         
*        ALSO FOR BILLING TYPE 23                                     *         
*        AMOUNT ON I&R                                                *         
***********************************************************************         
INRHND18 NTR1                                                                   
         CLI   PROFBTYP,TABRTY18   IF BILLING TYPE 18                           
         BE    INRH1810                                                         
         CLI   PROFBTYP,TABRTY23   OR IF BILLING TYPE 23                        
         BNE   TXIT                                                             
INRH1810 BRAS  RE,CKCAN            AND THIS IS AN I&R CHECK                     
         BNE   TXIT                                                             
*                                                                               
         L     R1,TMNTEARN         TAKE CANADIAN HANDLING RATE                  
         MVC   DUB(2),TMBRCAN+2    ON THE GROSS                                 
         MH    R1,DUB                                                           
         CVD   R1,DUB                                                           
         SRP   DUB,60,5                                                         
         CVB   R1,DUB                                                           
         A     R1,TMOCORP          AND ADD IT TO CORP HANDLING                  
         ST    R1,TMOCORP                                                       
         B     TXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CHECK IF CANADIAN I&R ACCOUNT CHECK                                    
*                R3 = CHECK TABLE                                               
*----------------------------------------------------------------------         
CKCAN    NTR1                                                                   
         LA    R4,IRCAN            TABLE OF SSN'S FOR I&R CANADA                
*                                                                               
CCAN10   CLI   0(R4),X'FF'         END OF TABLE                                 
         JE    CKCANNO                                                          
         CLC   TMSSN,0(R4)         IF THIS IS I&R ACCOUNT                       
         JE    CKCANYES                                                         
         LA    R4,L'IRCAN(R4)      BUMP TABLE                                   
         B     CCAN10                                                           
*                                                                               
CKCANYES XR    RE,RE                                                            
CKCANNO  LTR   RE,RE                                                            
         B     TXIT                                                             
*                                                                               
*        TABLE OF I&R SSN'S                                                     
IRCAN    DS    0CL9                                                             
         DC    CL9'000000055'                                                   
         DC    CL9'000003755'                                                   
         DC    CL9'000003855'                                                   
         DC    CL9'000003876'                                                   
         DC    CL9'000007106'                                                   
         DC    CL9'000008213'                                                   
         DC    CL9'000000066'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE GETS BILLING YTD FOR CURRENT SSN                         
         SPACE 1                                                                
         USING TMD,R3              R3 = A(TALIM BLOCK)                          
GETYTD   NTR1                                                                   
         LA    R2,BLOCK            R2 = A(YTD PARAMETER BLOCK)                  
         USING TYD,R2                                                           
         XC    0(TYLNQ,R2),0(R2)   CLEAR TAYTD PARAMETER BLOCK                  
         MVC   TYASYSIO,TASYSIO    A(SYSIO)                                     
         MVC   TYCUR,TMCURR        SET CURRENCY                                 
         MVC   TYEMP,TMEMP         EMPLOYER                                     
         MVC   TYSSN,TMSSN         SOCIAL SECURITY NUMBER                       
         MVC   TYPEND,TGTODAY1     AS OF DATE                                   
         MVC   TYTRACE,TMTRACE     TRACE OPTION                                 
         OI    TYSTAT,TYSTBILL     SET WANT BILLING YTD                         
         SPACE 1                                                                
         GOTO1 TGTAYTD,DMCB,(RC),SYSCOMM,TYD   GET YTD DATA                     
         SPACE 1                                                                
         MVC   TMYTD(TYBYLNQ),TYBYTD  SET YTD AMOUNTS IN TALIM BLOCK            
         B     TXIT                                                             
         SPACE 3                                                                
         GETEL2 (R4),DATADISP,ELCODE                                            
         SPACE 2                                                                
         LTORG                                                                  
         DS    0D                  TALIM BLOCK                                  
         DC    C'*TALIMD*'                                                      
TALIMD   DC    (TMLNQ)X'00'                                                     
         DS    0D                  TALIM BLOCK                                  
         DC    C'*TXDSCT*'                                                      
TXDSCT   DC    (255)X'00'                                                       
         EJECT                                                                  
*              ROUTINE CALCULATES TAX, HANDLING & AGENCY COMMISSION             
         SPACE 1                                                                
         DS    0D                                                               
CLCAMTS  NMOD1 0,CLCAMTS                                                        
         L     RC,SAVRC                                                         
         RELOC RELO2                                                            
*                                                                               
         CLI   ESTMODE,PROCPAY     IF NOT ACTUAL PAYMENT MODE                   
         BE    CLC10                                                            
         BAS   RE,CLCTNH           CALCULATE TAX AND HANDLING                   
         ICM   RF,15,AOSPST                                                     
         L     R1,TAXAMT           ADD PST TO TAX AMOUNT                        
         AR    R1,RF                                                            
         ST    R1,TAXAMT                                                        
*                                                                               
CLC10    CLI   PROFBTYP,TABRTY20   IF BILLING TYPE 20                           
         BNE   CLC20                                                            
         CLI   ESTMODE,PROCPAY     AND ACTUAL PAYMENT MODE                      
         BE    CLC30               DO NOT CALCULATE AGENCY COMMISSION           
CLC20    BRAS  RE,CLCCOMM          ELSE, CALCULATE AGENCY COMMISSION            
*                                                                               
CLC30    CLI   ESTMODE,PROCPAY     IF NOT ACTUAL PAYMENT MODE                   
         BE    CLCX                                                             
         BRAS  RE,CLCSIGN          CALCULATE SIGNATORY FEE                      
         LA    RE,ELTACO                                                        
         USING TACOD,RE                                                         
         TM    TACOSTA2,TACOSJPC   WAS CSF ALREADY PAID?                        
         BNZ   CLCX                                                             
         DROP  RE                                                               
***      TM    SFCMSTAT,SFCMCSF    WAS CSF CALCULATED ALREADY?                  
***      BNZ   *+8                                                              
         BRAS  RE,CLCCSF           CALCULATE COMMERCIAL SERVICE FEE             
CLCX     XIT1                                                                   
         SPACE 1                                                                
*              ROUTINE TO CALC TAX & HANDLING FOR ESTIMATED PAYMENT             
         SPACE 1                                                                
CLCTNH   NTR1                                                                   
         CLI   PROFBTYP,TABRTY99   IF NO CHARGE                                 
         BE    CLCX                GET OUT                                      
         CLI   CANDOLS,C'N'                                                     
         BE    CLCTNH1                                                          
         SPACE 1                                                                
         CLI   CANDOLS,C'Y'        IF CANADIAN DOLLARS                          
         BE    CLCTNH0                                                          
*        TM    TGUNEQU,ACT+UDA     OR IF UNION IS ACT OR UDA                    
         GOTO1 UNITEST,DMCB,TGUNEQUS,ACT+UDA,0,0,0                              
         BZ    *+12                                                             
CLCTNH0  BAS   RE,INTNHCAN         CALCUATE TNH FOR CANADIANS                   
         B     CLCTNH6                                                          
*                                                                               
CLCTNH1  CLI   PROFBTYP,TABRTY12   IF TYPE 12                                   
         BE    CLCTNH1A                                                         
         CLI   PROFBTYP,TABRTY22   OR TYPE 22                                   
         BE    CLCTNH1A                                                         
         CLI   PROFBTYP,TABRTY24   OR TYPE 24                                   
         BE    CLCTNH1A                                                         
         CLI   PROFBTYP,TABRTY25   OR TYPE 25                                   
         BE    CLCTNH1A                                                         
         CLI   PROFBTYP,TABRTY26   OR TYPE 26                                   
         BNE   CLCTNH2                                                          
CLCTNH1A BAS   RE,INTNH12          CALCULATE PAYROLL TAXES & HANDLING           
         B     CLCTNH6                                                          
*                                                                               
CLCTNH2  CLI   PROFBTYP,TABRTYE    IF TYPE E (EMS)                              
         BNE   *+12                                                             
         BAS   RE,INTNHE           CALCULATE PAYROLL TAXES & HANDLING           
         B     CLCTNH6                                                          
*                                                                               
         BAS   RE,SETRATE          R0=(INDIVIDUAL TAX RATE)                     
         L     R4,TCPAYI                                                        
*                                  (OLD WAY)                                    
*        CLC   TGTACODE,=C'HI '    HAWAII HAS 4.5% SURCHARGE                    
*        BNE   CLCTNH2A                                                         
*        AH    R0,=H'450'                                                       
*                                                                               
CLCTNH2A BAS   RE,MULT                                                          
         ST    R1,TAXAMT                                                        
                                                                                
         BAS   RE,WCTAX            CALCULATE WC ON CORPS                        
         BAS   RE,XTRATAX          CALC EXTRA TAXES, ACCUM TO TAXAMT            
         BRAS  RE,SETTAXB2         SET TAXABLE STATE                            
         BAS   RE,XTRASUI          CALC EXTRA SUI STATE SURCHARGE               
                                                                                
         BAS   RE,CALCGSTU         RE-CALCULATE TAX IF REALLY US$ GST           
                                                                                
         CLI   PROFTNH,C'Y'        IF USING TNH RATE FOR CORPS                  
         BE    *+8                                                              
         L     R0,CORPRT           CHARGE CORP. RATE                            
         L     R4,TCPAYC                                                        
         A     R4,TCEXP                                                         
         CLI   PROFBTYP,TABRTY6    BILLING TYPES 6                              
         BE    CLCTNH3                                                          
         CLI   PROFBTYP,TABRTY7              AND 7                              
         BE    CLCTNH3                                                          
         CLI   PROFBTYP,TABRTY8              AND 8                              
         BE    CLCTNH3                                                          
         CLI   PROFBTYP,TABRTY21             AND 21                             
         BE    CLCTNH3                                                          
         CLI   PROFBTYP,TABRTY11             AND 11                             
         BE    CLCTNH3                                                          
         CLI   PROFBTYP,TABRTY18             AND 18                             
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY23             AND 23                             
         BNE   CLCTNH4                                                          
         L     R0,CORPRT           (ALWAYS USE CORP. RATE FOR 18 TOO)           
CLCTNH3  A     R4,TCPAYI           ADD IN INDIV PAYMENTS                        
         CLI   PROFBTYP,TABRTY7    IF NOT TYPE 7                                
         BE    CLCTNH4                                                          
         A     R4,TCPNH            ADD IN P&H                                   
         CLI   PROFBTYP,TABRTY18                                                
         BE    CLCTNH4                                                          
         CLI   PROFBTYP,TABRTY23                                                
         BE    *+8                                                              
         A     R4,TAXAMT           ADD IN TAXES                                 
         CLI   PROFBTYP,TABRTY21   IF TYPE 21                                   
         BE    *+8                                                              
         CLI   PROFBTYP,TABRTY23                                                
         BNE   CLCTNH4                                                          
         TM    TGUSSTAT,SESSION    AND THIS IS SESSION PAYMENT                  
         BZ    CLCTNH4                                                          
         L     R0,TNHRT2           SET TO CHARGE SESS HANDLING RATE             
*LCTNH4  TM    TGUNEQU,ACT         IF NOT US$ ACTRA PAYMENT                     
CLCTNH4  GOTO1 UNITEST,DMCB,TGUNEQUS,ACT,0,0,0                                  
*        BO    *+8                                                              
         A     R4,TCINR            ADD IN INR (FOR CANADIANS)                   
         A     R4,TCHNW            ADD IN HNW (SET IF SEP FUND)                 
                                                                                
CLCTNH5  BAS   RE,MULT                                                          
         A     R1,HNDAMT                                                        
         ST    R1,HNDAMT                                                        
         SPACE 1                                                                
CLCTNH6  OC    CORPAMT,CORPAMT     IF CORP HANDLING AMT OVERRIDE                
         BZ    *+10                                                             
         MVC   HNDAMT,CORPAMT      USE IT                                       
         SPACE 1                                                                
         CLI   PROFBTYP,TABRTY20                                                
         BNE   CLCTNH8                                                          
         L     R4,TCINR                                                         
         L     R0,CORPRT                                                        
                                                                                
CLCTNH7  BAS   RE,MULT                                                          
         A     R1,HNDAMT                                                        
         ST    R1,HNDAMT                                                        
*                                                                               
CLCTNH8  L     R1,HNDAMT           ADD APPLIED GUARANTEE HANDLING               
         A     R1,GRTHND           TO HANDLING                                  
         ST    R1,HNDAMT                                                        
         XC    GRTHND,GRTHND                                                    
         B     CLCX                                                             
         EJECT                                                                  
*              ROUTINE CALCULATES EXTRA TAXES FOR WA / HI                       
         SPACE 1                                                                
XTRATAX  NTR1                                                                   
         TM    STATUS,HYPOPERF     SKIP IF PROCESSING HYPO PERFORMER            
         JO    XIT                                                              
                                                                                
         LA    RF,XTRAUNIT                                                      
XTT10    CLI   0(RF),X'FF'         EOT, NONE LEAVE                              
         JE    CLCX                                                             
         CLC   TGTACODE,0(RF)      MATCH ON UNIT                                
         JE    XTT20                                                            
         AHI   RF,5                                                             
         J     XTT10                                                            
                                                                                
XTT20    XR    R0,R0                                                            
         ICM   R0,3,3(RF)          GET RATE (N.NNN%)                            
         L     R4,TCPAYI                                                        
         A     R4,TCPAYC                                                        
         A     R4,TCEXP                                                         
         A     R4,TCPNH                                                         
         A     R4,TCINR                                                         
         A     R4,TCHNW                                                         
         A     R4,TAXAMT                                                        
                                                                                
         BAS   RE,MULT5            RESULT IN R1                                 
         L     RF,TAXAMT           ACCUM TO TAXAMT                              
         AR    RF,R1                                                            
         ST    RF,TAXAMT                                                        
                                                                                
         J     CLCX                                                             
                                                                                
XTRAUNIT DC    C'HI ',X'1268'      4.712%                                       
         DC    C'WA ',X'05DC'      1.500%                                       
         DC    X'FFFFFF'                                                        
         EJECT                                                                  
*              ROUTINE CALCULATES EXTRA TAXES FOR NY / CA                       
         SPACE 1                                                                
XTRASUI  NTR1                                                                   
         TM    TGSYSTA2,TASYSNCS   ARE WE CHARGING THE SURCHARGE?               
         JZ    XIT                                                              
         TM    STATUS,HYPOPERF     SKIP IF PROCESSING HYPO PERFORMER            
         JO    XIT                                                              
         CLI   TCW4TYPE,TAW4TYIN   INDIVIDUALS                                  
         JE    *+12                                                             
         CLI   TCW4TYPE,TAW4TYES   OR ESTATES ONLY                              
         JNE   XIT                                                              
                                                                                
         LA    RF,XSUIUNIT                                                      
XSUI10   CLI   0(RF),X'FF'         EOT, NONE LEAVE                              
         JE    XIT                                                              
         CLC   CASTTXST,0(RF)      MATCH ON TAXABLE STATE                       
         JE    XSUI20                                                           
         AHI   RF,5                                                             
         J     XSUI10                                                           
                                                                                
XSUI20   XR    R0,R0                                                            
         ICM   R0,3,3(RF)          GET RATE (N.NNN%)                            
         L     R4,TCPAYI                                                        
                                                                                
         BAS   RE,MULT5            RESULT IN R1                                 
         L     RF,TAXAMT           ACCUM TO TAXAMT                              
         AR    RF,R1                                                            
         ST    RF,TAXAMT                                                        
                                                                                
         J     XIT                                                              
                                                                                
XSUIUNIT DS    0H                                                               
**DUMMY  DC    CL3'NY ',XL2'0FA0'  4.000%                                       
**DUMMY  DC    CL3'CA ',XL2'0BB8'  3.000%                                       
         DC    X'FF'                                                            
         EJECT                                                                  
*        ROUTINE TO CALCULATE TAX & HANDLING FOR CANADIAN UNIONS                
*                                                                               
INTNHCAN NTR1                                                                   
         L     R0,CANTAX           CHARGE CANADIAN TAX RATE                     
*                                                                               
         CLI   PROFBTYP,TABRTY25   IF BILLING TYPE IS 25                        
         BE    INTNHCA5                                                         
         CLI   PROFBTYP,TABRTY26   IF BILLING TYPE IS 26                        
         BE    INTNHCA5                                                         
*                                                                               
         CLI   PROFBTYP,TABRTY24   IF BILLING TYPE IS 24                        
         BNE   INTNHCA1                                                         
         L     R0,TNHRT2           USE INDIVIDUAL HANDLING RATE                 
         TM    TGUSSTAT,SESSION    FOR SESSION PAYMENTS                         
         BO    INTNHCA1            ELSE USE CORP HANDLING RATE                  
         L     R0,CORPRT           FOR REUSE PAYMENTS                           
*                                                                               
INTNHCA1 L     R4,TCPAYC                                                        
         A     R4,TCPAYI                                                        
         A     R4,CSFAMT                                                        
         A     R4,TCEXP            (TCEXP=0 MISC ADDS USED INSTEAD)             
         A     R4,TCHNW            HNW (SET IF SEP FUND)                        
         CLI   PROFBTYP,23         IF BILLING TYPE 23 ADD IN I&R                
         BNE   *+8                                                              
         A     R4,TCINR                                                         
         BAS   RE,MULT                                                          
         ST    R1,HNDAMT           AND SET IN HANDLING                          
*                                                                               
         GOTO1 =A(GETPSTR),DMCB,(X'80',0),RR=RELO2   HIGHEST PST RATE           
*                                                                               
         ICM   R0,15,TGGSTRAT      CHARGE GST RATE                              
         L     R4,TCPAYC                                                        
         A     R4,TCPAYI                                                        
**NO-OP  A     R4,TCEXP            BILLING EXCLUDES REIMB EXP                   
         A     R4,TCINR                                                         
         A     R4,CSFAMT                                                        
         A     R4,TCHNW            HNW (SET IF SEP FUND)                        
*        A     R4,HNDAMT           2010 JUL 08, BILLING, NO HANDLING            
         LR    RF,R4               SAVE AMOUNT                                  
         BAS   RE,MULT                                                          
         ST    R1,TAXAMT           AND SET IN PAYROLL TAXES                     
*                                                                               
         LR    R4,RF               RESTORE AMOUNT                               
**NO-OP  TM    TGPRVST,TGPSTQUE    QST?                                         
**03/15  BZ    *+6                                                              
**NO-OP  AR    R4,R1               USES GST IN TAXABLE AMOUNT                   
         ICM   R0,15,TGPSTRAT                                                   
         BAS   RE,MULT                                                          
         L     RF,TAXAMT           ADD PST INTO PAYROLL TAXES                   
         AR    R1,RF                                                            
         ST    R1,TAXAMT           SET IN PAYROLL TAXES                         
         B     CLCX                                                             
*                                                                               
INTNHCA5 L     R0,TNHRT            FUTA RATE OR EOR= OVERRIDE                   
         L     R4,TCPAYC                                                        
         A     R4,TCPAYI                                                        
*                                                                               
         LR    RF,R4               SAVE AMOUNT                                  
         BAS   RE,MULT                                                          
         ST    R1,TAXAMT           AND SET IN PAYROLL TAXES                     
*                                                                               
         L     R0,TNHRT2                                                        
         LR    R4,RF                                                            
         CLI   PROFBTYP,TABRTY26   IF BILLING TYPE IS 26                        
         BNE   *+8                                                              
         A     R4,TAXAMT           PLUS THE TAX                                 
         BAS   RE,MULT                                                          
         ST    R1,HNDAMT           IS FIRST HALF OF HANDLING                    
*                                                                               
         B     CLCX                                                             
         EJECT                                                                  
*              ROUTINE SETS R0 TO INDIVIDUAL TAX RATE                           
         SPACE 1                                                                
SETRATE  DS    0H                                                               
         L     R0,TNHRT            SET TO CHARGE INDIV. TAX RATE                
         CLI   PROFBTYP,TABRTY6    IF TYPE 6                                    
         BNE   SETRATE5                                                         
         TM    TGUSSTAT,SESSION    AND THIS IS SESSION PAYMENT                  
         BZ    *+8                                                              
         L     R0,TNHRT2           SET TO CHARGE SPECIAL RATE                   
         B     SETRATEX                                                         
*                                                                               
SETRATE5 CLI   PROFBTYP,TABRTY14   IF TYPE 14                                   
         BNE   SETRATE7                                                         
         TM    TGUSSTAT,SESSION    AND IT IS A RE-USE PAYMENT -                 
         BZ    *+14                                                             
         CLC   TCPAYI,=F'100000000' OR A SESSION > $100,000                     
         BNH   SETRATEX                                                         
         SH    R0,TGBWCRP          SUBTRACT WORKMEN'S COMP THAT IS              
         B     SETRATEX                                                         
*                                                                               
SETRATE7 DS    0H                                                               
*ETRATE7 CLI   TGOFF,C'7'          IF OFFICE IS 7 OR G                          
*        BE    *+12                                                             
*        CLI   TGOFF,C'G'                                                       
*        BNE   SETRATEX                                                         
*        CLC   TGTACODE,=C'CA '    AND CALIFORNIA WORK                          
*        BNE   SETRATEX                                                         
*        SH    R0,TGBFUTA                                                       
*        AHI   R0,1643             USE SPECIAL FUTA RATE OF 16.43%              
SETRATEX BR    RE                                                               
         EJECT                                                                  
*              ROUTINE CALCULATES WORKMEN'S COMP ON CORPS                       
         SPACE 1                                                                
WCTAX    NTR1                                                                   
         CLI   TCW4TYPE,TAW4TYCA   IF CANADIAN                                  
         BE    *+12                                                             
         CLI   TCW4TYPE,TAW4TYCO   OR CORPORATION                               
         BNE   WCTAXX                                                           
         CLC   TGTACODE,=C'CN '    AND NOT FOR CN WORK                          
         BE    WCTAXX                                                           
         MVC   TGCTRY,=C'CA'                                                    
         GOTO1 TAXVAL,DMCB,(X'FF',TGTACODE)                                     
         BE    WCTAXX                                                           
         XR    R0,R0                                                            
         CLC   TGCAT,=C'ZZZ'       AND CATEGORY NOT ZZZ                         
         BE    WCTAX10                                                          
*                                                                               
         ICM   R0,3,TGBWCRP        R0=(WC CORP RATE)                            
         CLI   PROFBTYP,TABRTY14   IF BILLING TYPE 14                           
         BNE   WCTAX5                                                           
         TM    TGUSSTAT,SESSION    AND IT IS A RE-USE PAYMENT -                 
         BZ    WCTAX8              DON'T CHARGE WORKMEN'S COMP.                 
         CLC   TMNTEARN,=F'100000000' ELSE, ONLY ON FIRST 100,000               
         BH    WCTAX8                                                           
         B     WCTAX10                                                          
*                                                                               
WCTAX5   CLI   PROFBTYP,TABRTY6    IF BILLING TYPE 6                            
         BNE   WCTAX10                                                          
         CLI   TGOFF,C'O'          OFFICE O CHARGES WC                          
         BE    WCTAX10                                                          
         TM    TGUSSTAT,SESSION    AND IT IS A RE-USE PAYMENT                   
         BO    WCTAX10                                                          
*                                                                               
WCTAX8   XR    R0,R0               DON'T CHANGE WORKMEN'S COMP.                 
*                                                                               
WCTAX10  CLI   PROFBTYP,TABRTY23   IF BILLING TYPE 23                           
         BNE   WCTAX30                                                          
*                                  (OLD WAY)                                    
*        CLC   TGTACODE,=C'HI '    HAWAII HAS 4.5% SURCHARGE                    
*        BNE   WCTAX30                                                          
*        AH    R0,=H'450'                                                       
*                                                                               
WCTAX30  L     R4,TCPAYC           TIMES NON TAX AMOUNTS                        
**NO-OP  A     R4,TCEXP            BILLING EXCLUDES REIMB EXP                   
         BAS   RE,MULT                                                          
         CLI   PROFBTYP,TABRTY11                                                
         BNE   WCTAX50                                                          
         A     R1,HNDAMT                                                        
         ST    R1,HNDAMT                                                        
         B     WCTAXX                                                           
*                                                                               
WCTAX50  A     R1,TAXAMT                                                        
         ST    R1,TAXAMT                                                        
WCTAXX   B     CLCX                                                             
         EJECT                                                                  
*        ROUTINE TO CALCULATE TAX & HANDLING FOR BILLING TYPE 12                
*        PAYROLL TAXES = FUTA X PAYI                                            
*        HANDLING = (IND HAND X PAYI)+ ((CORP HAND X (PAYC+REXP))               
*        IF EOR= OVERRIDE APPLIES TO PAYROLL TAXES ONLY (EOR X PAYI)            
*        IF INC= OVERRIDE APPLIES TO HAND ONLY (INC X PAYI+PAYC+REXP)           
*                                                                               
INTNH12  NTR1                                                                   
         L     R0,TNHRT            FUTA RATE OR EOR= OVERRIDE                   
         L     R4,TCPAYI           TIMES INDIVIDUAL PAYMENT                     
         BAS   RE,MULT                                                          
         ST    R1,TAXAMT           IS PAYROLL TAXES                             
         BRAS  RE,SETTAXB2         SET TAXABLE STATE                            
         BAS   RE,XTRASUI          CALC EXTRA SUI STATE SURCHARGE               
         BAS   RE,WCTAX            CALCULATE WC ON CORPS                        
*                                                                               
         BAS   RE,CALCGSTU         RE-CALCULATE TAX IF REALLY US$ GST           
*                                                                               
         L     R0,TNHRT2           IND. HANDLING FEE (OR INC= OVERRIDE)         
*                                                                               
         CLI   PROFBTYP,TABRTY24   IF BILLING TYPE IS 24                        
         BNE   INTNH12A                                                         
         TM    TGUSSTAT,SESSION    AND PAYMENT IS REUSE                         
         BO    INTNH12A                                                         
         L     R0,CORPRT           USE CORP HANDLING RATE                       
*                                                                               
INTNH12A L     R4,TCPAYI           TIMES INDIVIDUAL PAYMENT                     
         CLI   PROFBTYP,TABRTY26   IF BILLING TYPE IS 26                        
         BNE   *+8                                                              
         A     R4,TAXAMT           PLUS THE TAX                                 
         BAS   RE,MULT                                                          
         ST    R1,HNDAMT           IS FIRST HALF OF HANDLING                    
*                                                                               
         CLI   PROFBTYP,TABRTY24   IF BILLING TYPE IS 24                        
         BNE   INTNH12B                                                         
         L     R0,TNHRT2           USE INDIVIDUAL HANDLING RATE                 
         TM    TGUSSTAT,SESSION    FOR SESSION PAYMENTS                         
         BO    INTNH12C            ELSE USE CORP HANDLING RATE                  
         L     R0,CORPRT           FOR REUSE PAYMENTS                           
*                                                                               
INTNH12B CLI   PROFTNH,C'Y'        IF NOT USING TNH RATE FOR CORPS              
         BE    *+8                                                              
         L     R0,CORPRT           CORP HANDLING FEE (OR INC=OVERRIDE)          
INTNH12C L     R4,TCPAYC           TIMES CORP PAYMENT                           
         A     R4,TCEXP            (TCEXP=0 MISC ADDS USED INSTEAD)             
         BAS   RE,MULT             IS SECOND HALF OF HANDLING                   
         A     R4,TCHNW            ADD IN HNW (SET IF SEP FUND)                 
         A     R1,HNDAMT           FIRST HALF + SECOND HALF IS                  
         ST    R1,HNDAMT           TOTAL HANDLING AMOUNT                        
         B     CLCX                                                             
         SPACE 2                                                                
*        ROUTINE TO CALCULATE TAX & HANDLING FOR BILLING TYPE E                 
*        PAYROLL TAXES = FUTA/SUTA X (PAYI + PAYC)                              
*        HANDLING = FICA (PAYI + PAYC + PAYROLL TAXES)                          
*                                                                               
INTNHE   NTR1                                                                   
         LH    R0,TGBFUTA                                                       
         TM    TGUSSTAT,SESSION    IF THIS IS A SESSION PAYMENT                 
         BO    *+8                                                              
         LH    R0,TGBSUTA          SET TO REUSE RATE                            
         L     R4,TCPAYI           INDIVIDUAL AND CORP                          
         A     R4,TCPAYC                                                        
         BAS   RE,MULT                                                          
         ST    R1,TAXAMT           PAYROLL TAXES                                
*                                                                               
         LH    R0,TGBFICA          HANDLING RATE                                
         AR    R4,R1               ADD PAYROLL TAXES TO SUBTOTAL                
         A     R4,TCPNH            PNH                                          
         A     R4,TCHNW            HNW                                          
         A     R4,TCINR            INR                                          
         BAS   RE,MULT                                                          
         ST    R1,HNDAMT           HANDLING AMOUNT                              
*                                                                               
         LH    R0,TGBOFIC          EMS FEE RATE                                 
         AR    R4,R1               ADD HANDLING AMOUNT TO SUBTOTAL              
         BAS   RE,MULT                                                          
         ST    R1,COMMAMT          EMS FEE                                      
*                                                                               
         LTR   R1,R1               IF EMS FEE IS NOT ZERO                       
         BZ    *+8                                                              
         OI    SFCMSTAT,SFCMCOMM   SET COMMISSION CALCULATED                    
*                                                                               
***      BAS   RE,CALCGSTU         RE-CALCULATE TAX IF REALLY US$ GST           
*                                                                               
         B     CLCX                                                             
         EJECT                                                                  
*              ROUTINE CALCULATES US$ GST IF NECESSARY                          
         SPACE 1                                                                
CALCGSTU NTR1                                                                   
         TM    MYCASTAT,MYCASGST   IF CORP HAS GST NUMBER                       
         BZ    CLCX                                                             
         CLI   TGUSEQU,UCPN        AND IF NOT CPN PAYMENT                       
         BE    CLCX                                                             
***      CLC   TGTACODE,=C'CN '    AND FOR CN WORK                              
***      BNE   CLCX                                                             
*        TM    TGUNEQU,ACT         AND UNION IS ACT                             
         GOTO1 UNITEST,DMCB,TGUNEQUS,ACT+UDA+NON,0,0,0                          
         BZ    CLCX                (HYPOS DON'T HAVE LOCALS)                    
CGSTU10  ICM   R0,15,TGGSTRAT      CALCULATE GST                                
         L     R4,TCPAYC                                                        
**NO-OP  A     R4,TCEXP            BILLING EXCLUDES REIMB EXP                   
         A     R4,TCINR                                                         
         OC    AOSAMT,AOSAMT       REMOVE AOS AMOUNT FROM WAGES                 
         BZ    CGSTU20                                                          
         ICM   RF,15,AOSAMT                                                     
         SR    R4,RF                                                            
*                                                                               
CGSTU20  LR    RF,R4                                                            
         BAS   RE,MULT                                                          
         ST    R1,TAXAMT                                                        
*                                                                               
         LR    R4,RF               RESTORE AMOUNT                               
**NO-OP  TM    TGPRVST,TGPSTQUE    QST?                                         
**03/15  BZ    *+6                                                              
**NO-OP  AR    R4,R1               USES GST IN TAXABLE AMOUNT                   
         ICM   R0,15,TGPSTRAT                                                   
         BAS   RE,MULT                                                          
         L     RF,TAXAMT           ADD PST INTO PAYROLL TAXES                   
         AR    R1,RF                                                            
         ST    R1,TAXAMT           SET IN PAYROLL TAXES                         
         B     CLCX                                                             
         EJECT                                                                  
*              ROUTINE MULTIPLIES A BINARY AMOUNT BY A PERCENTAGE RATE          
         SPACE 1                                                                
*                                  R4=AMOUNT (2 DEC. PLACES)                    
*                                  R0=RATE   (NNN.NN PCT.)                      
MULT     NTR1                                                                   
         LR    R1,R4                                                            
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                RETURN ANSWER IN R1                          
         XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*              ROUTINE MULTIPLIES A BINARY AMOUNT BY A PERCENTAGE RATE          
         SPACE 1                                                                
*                                  R4=AMOUNT (2 DEC. PLACES)                    
*                                  R0=RATE   (NN.NNN PCT.)                      
MULT5    NTR1                                                                   
         LR    R1,R4                                                            
         MR    R0,R0                                                            
         D     R0,=F'50000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                RETURN ANSWER IN R1                          
         XIT1  REGS=(R1)                                                        
         SPACE 3                                                                
RELO2    DS    F                   RELOCATION FACTOR                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE CONVERTS CANADIAN DOLLAR PAYMENTS TO US DOLLARS          
*              & APPLIES MULTIPLIER                                             
*                                   NTRY  P1=AMOUNT(S), P2=N'AMOUNTS            
         SPACE 1                                                                
         DS    0D                                                               
CNVAMTS  NMOD1 0,CNVAMTS                                                        
         L     RC,SAVRC                                                         
         L     R3,4(R1)            R3=N'AMTS TO CONVERT                         
         L     R2,0(R1)            R2=A(AMOUNTS)                                
CNVAMT5  BAS   RE,CONVERT                                                       
         BAS   RE,MULTPLR                                                       
         LA    R2,4(R2)                                                         
         BCT   R3,CNVAMT5                                                       
*                                                                               
CNVAMTX  XIT1                                                                   
         SPACE 2                                                                
CONVERT  NTR1                                                                   
         TM    OTHOPT,OTHEXCH      IF OVERRIDE CONVERSION RATE                  
         BO    CONVERT1            SET, ALWAYS CONVERT                          
         CLI   CANDOLS,C'Y'        OTHERWISE, ONLY CONVERT IF                   
         BNE   CONVERTX            COMMERCIAL IS CANADIAN DOLLARS               
CONVERT1 ICM   R1,15,EXCHANGE      IF THERE'S AN ACTUAL EXCHANGE                
         BNZ   CONVERT2            RATE                                         
         ICM   R1,15,DEFEXCH       OR SYSTEM/OVERRIDE EXCHANGE RATE             
         BZ    CONVERTX                                                         
CONVERT2 CVD   R1,WORK             CONVERT TO PACKED FORMAT                     
*                                                                               
         L     R1,0(R2)                                                         
         CVD   R1,DUB                                                           
*                                                                               
         ZAP   WORK+8(12),DUB                                                   
*                                                                               
*        MP    DUB,WORK+4(4)       MULTIPLY BY AMOUNT                           
         MP    WORK+8(12),WORK+4(4)       MULTIPLY BY AMOUNT                    
         MVC   DUB,WORK+12                                                      
*                                                                               
         SRP   DUB,60,5            DIVIDE BY 10000 AND ROUND                    
         CVB   R1,DUB                                                           
         ST    R1,0(R2)            RESTORE TO FIELD                             
CONVERTX B     CNVAMTX                                                          
         SPACE 2                                                                
MULTPLR  NTR1                                                                   
         ICM   R0,15,MULTIPLY      IF MULTIPLIER PRESENT                        
         BZ    MULTPLRX                                                         
         L     R4,0(R2)                                                         
*                                  CALCULATE ADDITIONAL AMOUNT                  
         LR    R1,R4                                                            
         MR    R0,R0                                                            
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
         AR    R4,R1               AND ADD IT IN                                
         ST    R4,0(R2)                                                         
MULTPLRX B     CNVAMTX                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MERGE CLASS A PAYMENTS                                
         SPACE 1                                                                
         DS    0D                                                               
MERGECLA NMOD1 0,*MERGE*                                                        
         L     RC,SAVRC                                                         
         RELOC RELO3                                                            
*                                                                               
         TM    FRSTIME,X'40'       IF NOT FIRST TIME FOR MERGECLA               
         BNO   *+14                                                             
         XC    TCTUSES,TCTUSES     CLEAR TOTAL NUMBER USES PAID                 
         B     MRGCLAX             AND EXIT                                     
         OI    FRSTIME,X'40'       ELSE, TURN ON FIRST CHECK CLA FLAG           
         SPACE 1                                                                
MRGCLA5  BAS   RE,SETUSENM         SET START & END USE NUMBER IN FULL           
         SPACE 1                                                                
         L     R3,CLATAB           R3=A(CLASS A TABLE)                          
         USING CLATABD,R3                                                       
         L     R0,MAXCLA           R0=MAXIMUM NUMBER OF TABLE ENTRIES           
*                                                                               
MRGCLA10 OC    0(CLALNQ,R3),0(R3)  WHILE NOT END OF TABLE                       
         BZ    MRGCLA30                                                         
         CLC   CLACYCS,TCPCYCS     AND WITHIN CYCLE                             
         BNE   MRGCLA40                                                         
         CLC   CLACYCE,TCPCYCE                                                  
         BNE   MRGCLA40                                                         
*                                  CHECK START & END USE W/IN USE RANGE         
         CLC   FULL(2),CLASTRT                                                  
         BL    MRGCLA20                                                         
         CLC   FULL+2(2),CLAEND                                                 
         BH    MRGCLA20                                                         
         XC    TCTUSES,TCTUSES     ALREADY WITHIN RANGE                         
         B     MRGCLAX3                                                         
*                                                                               
MRGCLA20 LH    R1,FULL+2           CHECK USES ARE CONTINOUS                     
         LA    R1,1(R1)                                                         
         CH    R1,CLASTRT          (END USE + 1 = START USE IN TABLE)           
         BNE   MRGCLA35                                                         
         MVC   CLASTRT,FULL        REPLACE NEW START USE IN TABLE               
         B     MRGCLAX3                                                         
*                                                                               
MRGCLA30 MVC   CLACYCS,TCPCYCS     SET CYCLE DATES                              
         MVC   CLACYCE,TCPCYCE                                                  
MRGCLA35 MVC   CLASTRT,FULL        SET START USE NUMBER                         
         MVC   CLAEND,FULL+2       SET END USE NUMBER                           
         B     MRGCLAX3                                                         
*                                                                               
MRGCLA40 LA    R3,CLALNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,MRGCLA10         LOOP                                         
         DC    H'0'                DIE - INCREASE NCLA!                         
*                                                                               
MRGCLAX3 ST    R3,ACLAENT          SAVE ADDRESS OF THIS ENTRY                   
*                                                                               
         L     R3,CLATAB           R3=A(CLASS A TABLE)                          
         L     RF,MAXCLA           RF=MAXIMUM NUMBER OF CLA ENTRIES             
         MHI   RF,CLALNQ                                                        
         GOTO1 =A(MYTRACE2),DMCB,=C'CLATAB',(R3),(RF),RR=RELO3                  
*                                                                               
MRGCLAX  XIT1                                                                   
         SPACE 2                                                                
*              ROUTINE TO MERGE PAX PAYMENTS                                    
*              THIS ROUTINE USES THE SAME DSECT AS MERGECLA                     
         SPACE 1                                                                
         DS    0D                                                               
MERGEPAX NMOD1 0,*MERGP*                                                        
         L     RC,SAVRC                                                         
         RELOC RELO3                                                            
*                                                                               
         TM    FRSTIME,X'20'       IF NOT FIRST TIME FOR MERGEPAX               
         BNO   *+14                                                             
         XC    TCTUSES,TCTUSES     CLEAR TOTAL NUMBER USES PAID                 
         B     MRGPAXX             AND EXIT                                     
         OI    FRSTIME,X'20'       ELSE, TURN ON FIRST CHECK PAX FLAG           
         SPACE 1                                                                
MRGPAX5  BAS   RE,SETUSENM         SET START & END USE NUMBER IN FULL           
         SPACE 1                                                                
         L     R3,PAXTAB           R3=A(CLASS A TABLE)                          
         USING CLATABD,R3                                                       
         L     R0,MAXCLA           R0=MAXIMUM NUMBER OF TABLE ENTRIES           
*                                                                               
MRGPAX10 OC    0(CLALNQ,R3),0(R3)  WHILE NOT END OF TABLE                       
         BZ    MRGPAX30                                                         
         CLC   CLACYCS,TCPCYCS     AND WITHIN CYCLE                             
         BNE   MRGPAX40                                                         
         CLC   CLACYCE,TCPCYCE                                                  
         BNE   MRGPAX40                                                         
*                                  CHECK START & END USE W/IN USE RANGE         
         CLC   FULL(2),CLASTRT                                                  
         BL    MRGPAX20                                                         
         CLC   FULL+2(2),CLAEND                                                 
         BH    MRGPAX20                                                         
         XC    TCTUSES,TCTUSES     ALREADY WITHIN RANGE                         
         B     MRGPAXX3                                                         
*                                                                               
MRGPAX20 LH    R1,FULL+2           CHECK USES ARE CONTINOUS                     
         LA    R1,1(R1)                                                         
         CH    R1,CLASTRT          (END USE + 1 = START USE IN TABLE)           
         BNE   MRGPAX35                                                         
         MVC   CLASTRT,FULL        REPLACE NEW START USE IN TABLE               
         B     MRGPAXX3                                                         
*                                                                               
MRGPAX30 MVC   CLACYCS,TCPCYCS     SET CYCLE DATES                              
         MVC   CLACYCE,TCPCYCE                                                  
MRGPAX35 MVC   CLASTRT,FULL        SET START USE NUMBER                         
         MVC   CLAEND,FULL+2       SET END USE NUMBER                           
         B     MRGPAXX3                                                         
*                                                                               
MRGPAX40 LA    R3,CLALNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,MRGPAX10         LOOP                                         
         DC    H'0'                DIE - INCREASE NCLA!                         
*                                                                               
MRGPAXX3 ST    R3,ACLAENT          SAVE ADDRESS OF THIS ENTRY                   
*                                                                               
         L     R3,PAXTAB           R3=A(CLASS A TABLE)                          
         L     RF,MAXCLA           RF=MAXIMUM NUMBER OF CLA ENTRIES             
         MHI   RF,CLALNQ                                                        
         GOTO1 =A(MYTRACE2),DMCB,=C'PAXTAB',(R3),(RF),RR=RELO3                  
*                                                                               
MRGPAXX  XIT1                                                                   
         SPACE 2                                                                
*              ROUTINE RETURNS START AND END USE NUMBER IN                      
*              FULL AND FULL+2 RESPECTIVELY                                     
         SPACE 1                                                                
SETUSENM NTR1                                                                   
         LH    R1,TCNUSES          PREV USES PAID + 1                           
         LA    R1,1(R1)                                                         
         STH   R1,FULL             = START USE NUMBER                           
*                                                                               
         LH    RE,TCTUSES          USES PAID NOW PLUSSTART USE NUMBER           
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         BCTR  RE,0                                                             
         AR    R1,RE                                                            
         STH   R1,FULL+2           = END USE NUMBER                             
         B     MRGPAXX                                                          
         SPACE                                                                  
RELO3    DS    F                   RELOCATION FACTOR                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DRIVER INTERFACE ROUTINES                                        
         SPACE 1                                                                
         DS    0D                                                               
CALLDRV  NMOD1 0,*CDRIV*                                                        
         L     RC,SAVRC                                                         
         L     R2,AGLOBAL            SET INITIALIZATION CONDITIONALS            
         USING GLOBALD,R2                                                       
*                                                                               
         MVC   GLMODE,3(R1)        DRIVER MODE                                  
*                                                                               
         CLI   GLMODE,GLINIT       IF INITIALIZING                              
         BNE   *+12                                                             
         BAS   RE,INTDRIVE                                                      
         B     CALLDRVX                                                         
*                                                                               
         CLI   GLMODE,GLINPUT      IF INPUT                                     
         BNE   *+12                                                             
         BAS   RE,INDRIVE                                                       
         B     CALLDRVX                                                         
*                                                                               
         CLI   GLMODE,GLOUTPUT     IF OUTPUT                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ALLDRIVE                                                      
         B     CALLDRVX                                                         
*                                                                               
CALLDYES XR    RC,RC               EXITS                                        
CALLDNO  LTR   RC,RC                                                            
CALLDRVX XIT1                                                                   
         SPACE 2                                                                
INTDRIVE NTR1                                                                   
         TM    STATUS2,DRINITED      IF DRIVER HASN'T BEEN INITIALIZED          
         BO    CALLDRVX                                                         
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         LA    R1,HDHOOK             SET A(HEADLINE HOOK)                       
         ST    R1,GLAHOOK                                                       
         MVI   GLTWORKD,GLTSPOOL   SPOOL APPLICATION                            
         MVI   GLFHEADL,8          FIRST ROW HEADING LINE                       
         MVC   GLSPACE,SPACOPT     PASS THRU SPACING OPT                        
         MVC   GLBOXOPT,BOXOPT               BOX OPTION                         
         MVC   GLLFTOPT,LEFTOPT          AND LEFT OPTION                        
         CLI   TRACEOPT,C'G'       IF GLOBAL TRACE ONLY                         
         BNE   *+16                                                             
         MVI   GLTRACE,C'Y'        SET GLOBAL TRACE ON                          
         MVI   TRACEOPT,C'N'       SET LOCAL TRACE OFF                          
         B     *+10                                                             
         MVC   GLTRACE,TRACEOPT    OPTION TO TRACE                              
         MVI   GLNORBOX,X'40'      TURN OFF ROW BOXES FOR TOTALS                
         CLI   FORMAT,TAEPFDWN     IF DOWNLOAD FORMAT                           
         BE    *+8                                                              
         CLI   FORMAT,TAEPFRCD     OR RECAP DOWNLOAD                            
         BNE   *+12                                                             
         MVI   GLDOWNLD,X'80'      SET DOWNLOAD SWITCH ACTIVE                   
         MVI   GLBOXOPT,C'N'       AND TURN OFF BOXES                           
         SPACE 1                                                                
         MVC   GLINDS,DRINDS       PASS THROUGH DRIVER INDICATORS               
         OI    GLINDS,GLISDONT     SUPPRESS SPACING FOR REJECTED LINES          
         CLI   FORMAT,TAEPFQTR     IF NOT QUARTERLY REPORT                      
         BE    INTDRV5                                                          
         CLI   FORMAT,TAEPFV1      AND NOT VARIANCE FORMATS                     
         BE    INTDRV5                                                          
         CLI   FORMAT,TAEPFV2                                                   
         BE    INTDRV5                                                          
         OI    GLINDS,GLPALDET     SET WE WANT ALL DETAILS                      
         SPACE 1                                                                
INTDRV5  MVC   GLOPTS(1),PROFMUS     SEPARATE MUSIC FEES/P&H                    
         MVC   GLOPTS+1(1),PROFCOMM  CALC. AGENCY COMMISSION                    
         CLI   PROFTNHC,C'Y'         IF FORCING T&H COMBINED                    
         BE    *+12                                                             
         TM    TGBTSTAT,BTYPSTNH     OR CERTAIN BILLING TYPE                    
         BZ    *+8                                                              
         MVI   GLOPTS+2,C'Y'         REPORT T&H TOGETHER                        
         CLI   PROFBTYP,TABRTY20                                                
         BNE   INTDRV10                                                         
         MVI   GLOPTS+2,C'A'         SPECIAL LIT FOR BILLING TYPE 20            
         CLC   TGAGY,=CL6'0585'      IF GRUPOS GALLEGOS, SET SPECIAL            
         BNE   INTDRV10              LITERAL, FOR HORIZ TOTALS TO PRINT         
         MVI   GLOPTS+2,C'L'         LUNA PROD INSTEAD OF TALENT MASTER         
INTDRV10 CLI   PROFBTYP,TABRTY9                                                 
         BNE   *+8                                                              
         MVI   GLOPTS+2,C'9'         SPECIAL LIT FOR BILLING TYPE 9             
                                                                                
*        CLI   PROFBTYP,TABRTY23     TYPE 23 NEEDS TAX AND HAND SEPRTD          
*        BNE   *+8                                                              
*        MVI   GLOPTS+2,C' '         SPECIAL LIT FOR BILLING TYPE 9             
                                                                                
         MVC   GLOPTS+3(1),COMLOPT   PRINT COMMLS INCLUDED SUMMARY              
         MVC   GLOPTS+4(1),RECAPOPT  PRINT RECAP                                
         MVC   GLOPTS+5(1),CASTOPT   PRINT CAST LIST                            
         MVC   GLOPTS+6(1),HORZOPT   PRINT DETAIL TOTALS HORIZONTALLY           
         MVC   GLOPTS+7(1),ONLYOPT   PRINT RECAP ONLY                           
         MVC   GLOPTS+8(1),PROFPAGE  NEW PAGE EACH COMMERCIAL                   
         MVC   GLOPTS+9(1),FORMAT    REPORT FORMAT                              
         MVC   GLOPTS+10(1),PROFHNW  COMBINE H&W WITH NET PAYMENT               
         MVC   GLOPTS+11(1),CLAOPT   MERGE CLASS A PAYMENTS W/IN CYCLE          
         SPACE 1                                                                
         BAS   RE,ALLDRIVE                                                      
         OI    STATUS2,DRINITED    SET DRIVER INITIALIZED                       
         B     CALLDRVX                                                         
         EJECT                                                                  
*              HOOK ROUTINE FROM DRIVER                                         
HDHOOK   NTR1                                                                   
         CLI   GLHOOK,GLPRINT      IF ABOUT TO PRINT A LINE                     
         BNE   HK5                                                              
         TM    STATUS2,NOPRINT     THEN IF NO PRINT STATUS ON                   
         BZ    HKX                                                              
         XI    STATUS2,NOPRINT     TURN IT OFF                                  
         B     HKDONT              TELL DRIVER TO SKIP LINE                     
         SPACE 1                                                                
HK5      CLI   GLHOOK,GLINREC      IF ABOUT TO PROCESS A REC                    
         BNE   HK10                                                             
         CLC   =C'WREC',GLRECLAB   AND IT IS RECAP W                            
         BNE   HK8                                                              
         CLI   GLOPTS+19,PROCWC    IF NOT PROCESSING WORK CODES                 
         BE    HKX                                                              
         B     HKDONT              TELL DRIVER TO SKIP REC                      
HK8      CLC   =C'RECAPREC',GLRECLAB OR IT IS ANY RECAP                         
         BNE   HK9                                                              
         CLI   GLOPTS+17,0         MUST BE CORRECT SUMMARY INDEX                
         BNE   HKDONT                                                           
         CLI   GLOPTS+18,C'Y'      AND DETAIL RECORD                            
         BNE   HKDONT              TELL DRIVER TO SKIP REC                      
         B     HKX                                                              
HK9      CLC   =C'MISCREC',GLRECLAB IF ABOUT TO PROCESS MISC. RECAP             
         BNE   HKX                                                              
         TM    STATUS3,MISCAFLG    AND NO MISC ADDITIONS IN REC                 
         BZ    HKDONT              TELL DRIVER TO SKIP REC                      
         B     HKX                                                              
         SPACE 1                                                                
HK10     CLI   GLHOOK,GLHEAD       IF WE'RE PROCESSING HEADINGS                 
         BNE   HKX                                                              
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    *+14                                                             
         MVC   HEAD3+30(17),DPERIOD JUST SET PERIOD                             
         B     HKX                 AND EXIT                                     
         SPACE 1                                                                
         ZIC   R4,GLFHEADL         R4=FIRST ROW HEADING LINE NUMBER             
         SH    R4,=H'3'            R4=N'POSSIBLE HEADINGS TO ADJUST             
         LA    R2,HEAD3+1+L'LABLAREA  R2=A(CODE ON FIRST LINE)                  
         LA    R3,L'CODEAREA       R3=MAX L'CODE                                
         BAS   RE,GETLONG                                                       
         LA    R3,L'CODEAREA(R2)   BUMP TO WHERE NAME STARTS                    
         A     R2,FULL                                                          
         LA    R2,2(R2)            R2=A(NEW POSITION FOR NAME)                  
         LA    R1,L'NAMEAREA       R1=MAX L'NAME                                
         BAS   RE,SHUFFLE                                                       
         LA    R2,HEAD3+1          R2=A(LABEL ON FIRST LINE)                    
         LA    R3,L'LABLAREA       R3=MAX L'LABEL                               
         BAS   RE,GETLONG                                                       
         LA    R3,L'LABLAREA(R2)   BUMP TO WHERE CODE STARTS                    
         A     R2,FULL                                                          
         LA    R2,2(R2)            R2=A(NEW POSITION FOR CODE)                  
         LA    R1,L'CODEAREA+2+L'NAMEAREA  R1=MAX L'CODE+L'NAME                 
         BAS   RE,SHUFFLE                                                       
         SPACE 1                                                                
         MVC   HEAD3+56(17),DPERIOD DISPLAY PERIOD IN CENTER OF REPORT          
         SPACE 1                                                                
         CLC   MYHEAD5,SPACES      IF HEAD5 OVERRIDE DEFINED                    
         BNH   HKX                                                              
         LA    RE,HEAD5+(132-L'MYHEAD5)/2-2                                     
         LA    RF,MYHEAD5                                                       
         LA    R1,L'MYHEAD5-1                                                   
HK20     CLC   0(10,RE),SPACES     FIND 1ST AVAILABLE SLOT                      
         BE    HK30                                                             
         LA    RE,1(RE)                                                         
         CLI   0(RF),C' '          IF LITERAL HASN'T BEGUN YET                  
         BH    *+10                                                             
         LA    RF,1(RF)            OK TO MOVE LESS                              
         BCTR  R1,0                                                             
         B     HK20                                                             
HK30     EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,RE),0(RF)       DISPLAY IT                                   
         MVC   MYHEAD5,SPACES      AND CLEAR IT FOR NEXT TIME                   
         B     HKX                                                              
         SPACE 1                                                                
HKDONT   MVI   GLHOOK,GLDONT       TELL DRIVER TO NOT PRINT                     
HKX      B     CALLDRVX                                                         
         EJECT                                                                  
*              ROUTINE FINDS WIDEST FIELD FOR COLUMN (HOOK)                     
         SPACE 1                                                                
*                                  R2=A(FIELD ON FIRST LINE)                    
*                                  R3=MAX WIDTH                                 
*                                  R4=N'LINES TO ADJUST                         
GETLONG  NTR1                                                                   
GLNG2    ST    R3,FULL             RETURN FULL=WIDEST FOUND                     
         LTR   R3,R3                                                            
         BZ    HKX                                                              
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4               R0=N'LINES TO ADJUST                         
         SPACE 1                                                                
         CLI   0(R1),C' '          IF SOMETHING HERE THEN DONE                  
         BH    HKX                                                              
         AH    R1,PWIDTH           BUMP TO NEXT LINE                            
         BCT   R0,*-12             AND CHECK IT AS WELL                         
         SPACE 1                                                                
         BCTR  R3,0                NOTHING IN THIS POSITION -                   
         B     GLNG2               SLIDE BACK ONE AND TRY AGAIN                 
         SPACE 3                                                                
*              ROUTINE ADJUSTS HEADLINES BASED ON GETLONG (HOOK)                
         SPACE 1                                                                
*                                  R1=MAX L'DATA TO MOVE                        
*                                  R2=A(NEW BEG. OF DATA ON FIRST LINE)         
*                                  R3=A(CURRENT BEG. OF DATA)                   
*                                  R4=N'LINES TO ADJUST                         
SHUFFLE  NTR1                                                                   
SHF2     XR    RE,RE               DETERMINE L'DATA TO MOVE                     
         LR    RF,R3                                                            
         SPACE 1                                                                
SHF4     CLC   0(10,RF),SPACES     SCAN FOR FIRST EMPTY AREA                    
         BNH   SHF6                                                             
         LA    RF,1(RF)            BUMP TO NEXT DATA AREA                       
         LA    RE,1(RE)            ADD 1 TO LENGTH                              
         CR    RE,R1               DETERMINE IF REACHED MAXIMUM LENGTH          
         BNE   SHF4                                                             
         SPACE 1                                                                
SHF6     LTR   RE,RE               HAVE SOMETHING TO MOVE ON THIS LINE          
         BZ    SHF8                                                             
         BCTR  RE,0                                                             
         EX    RE,SHFSAVE          SAVE DATA IN WORK                            
         EX    RE,SHFCLEAR         CLEAR IT FROM EXISTING LOCATION              
         EX    RE,SHFMOVE          MOVE IT TO NEW SPOT                          
         SPACE 1                                                                
SHF8     AH    R2,PWIDTH           BUMP TO NEXTS                                
         AH    R3,PWIDTH                                                        
         BCT   R4,SHF2             AND CONTINUE                                 
         B     HKX                                                              
         EJECT                                                                  
*              DRIVER INTERFACE ROUTINES  (CONT'D)                              
         SPACE                                                                  
INDRIVE  NTR1                                                                   
         MVI   TGBYTE,0                                                         
         TM    ESTMODE,PROCPAY+PROCESTP+PROCAUTO  IF THIS IS A PAYMENT          
         BZ    INDR20                                                           
*                                                                               
         CLI   MYUSHIST,C'Y'       HISTORY ONLY - DON'T REPORT                  
         BE    CALLDNO                                                          
         CLC   APPLYDTE,PSTART     FILTER BASED ON PERIOD                       
         BL    CALLDNO                                                          
         CLC   APPLYDTE,PEND                                                    
         BH    CALLDNO                                                          
         TM    REPOPT,PRNTSESS     IF DON'T WANT SESSIONS PRINTED               
         BO    *+12                                                             
         TM    TGUSSTAT,SESSION    THEN EXCLUDE THEM NOW                        
         BO    CALLDNO                                                          
         MVI   TGBYTE,C'Y'         SET DETAIL RECORD                            
         SPACE 1                                                                
         CLI   CLAOPT,C'Y'         IF MERGING CLASS A PAYMENTS                  
         BNE   INDR20                                                           
         CLI   ESTMODE,PROCPAY     & IF ACTUAL CLASS A USAGE PAYMENT            
         BNE   INDR20                                                           
         CLI   SUMINDEX,0          & DETAIL REPORT                              
         BNE   INDR20                                                           
         CLI   TGUSEQU,UCLA                                                     
         BNE   INDR15                                                           
         GOTO1 =A(MERGECLA)        TRY TO MERGE CLA'S WITHIN A CYCLE            
         B     INDR20                                                           
INDR15   CLI   TGUSEQU,UPAX        PAX PAYMENT?                                 
         BNE   INDR17                                                           
         GOTO1 =A(MERGEPAX)        TRY TO MERGE PAX'S WITHIN A CYCLE            
         B     INDR20                                                           
INDR17   CLI   TGUSEQU,UITN        ITN PAYMENT?                                 
         BNE   INDR20                                                           
*        GOTO1 =A(MERGEITN)        DON'T KNOW ABOUT THIS YET                    
*                                                                               
INDR20   CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    INDR30                                                           
         CLI   SUMINDEX,NALL       INTERESTED IN MISC ADDITIONS                 
         BH    INDR30                                                           
         CLI   SUMINDEX,0          AND IN DETAILS ONLY                          
         BNE   CALLDNO                                                          
         CLI   TGBYTE,C'Y'                                                      
         BNE   CALLDNO                                                          
*                                                                               
INDR30   CLI   TGBYTE,C'Y'         IF DETAIL                                    
         BNE   *+8                                                              
         OI    STATUS2,CASTACTV+COMLACTV  SET CAST MEMBER/COMML ACTIVE          
         SPACE 1                                                                
         MVC   GLOPTS+17(1),SUMINDEX PASS RUNTIME OPTION - PROC. SUMM.          
         MVC   GLOPTS+18(1),TGBYTE                       - DETAIL REC.          
         MVC   GLOPTS+19(1),ESTMODE                      - RECORD TYPE          
         BAS   RE,ALLDRIVE                                                      
         B     CALLDYES                                                         
         EJECT                                                                  
ALLDRIVE NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    ALLDRV5                                                          
*                                                                               
         GOTO1 CATCHIOS            CHECK IO COUNT                               
         LA    R3,WORK             R3=A(DROOL PARM BLOCK)                       
         USING DROOLD,R3                                                        
         XC    DROOLBLK,DROOLBLK                                                
         MVC   DROGLOBL,AGLOBAL    A(DRIVER GLOBALD)                            
         MVC   DRORPBLK,AREPBLK    A(FAREPBLK)                                  
         LA    RE,TSARBLK          A(TSARD)                                     
         STCM  RE,7,DROTSARD                                                    
         GOTO1 DRIVER,(R3)         OFF TO DROOL                                 
         CLI   GLMODE,GLINIT       IF INITIALIZING                              
         BNE   *+16                                                             
         CLI   DROERROR,DROERRAF   CHECK TEMPEST END-OF-FILE                    
         BE    NOTNOWD             MUST RUN OFFLINE                             
         B     CALLDRVX                                                         
         CLI   GLMODE,GLINPUT      IF INPUT MODE                                
         BNE   CALLDRVX                                                         
         CLI   DROERROR,DROERREF   CHECK ALLOCATION FAILURE                     
         BE    NOTNOWD             MUST RUN OFFLINE                             
         B     CALLDRVX                                                         
*                                                                               
ALLDRV5  GOTO1 DRIVER,DMCB,(R2)    OFF TO DRIVER                                
         CLI   GLMODE,GLINPUT      IF IN INPUT MODE                             
         BNE   CALLDRVX                                                         
         TM    ESTMODE,PROCPAY+PROCESTP+PROCAUTO AND THIS IS A PAYMENT          
         BZ    CALLDRVX                                                         
         CLI   SUMINDEX,0          AND NOT ALREADY PROCESSING SUMMARIES         
         BNE   CALLDRVX                                                         
         BAS   RE,SUMDRIVE         THEN NEED TO PROCESS SUMMARIES               
         CLI   RECAPOPT,TAEPRWC    IF RECAP IS BY WORK-CODE                     
         BNE   *+8                                                              
         BAS   RE,WCDRIVE          PROCESS WORK-CODES                           
         B     CALLDRVX                                                         
         EJECT                                                                  
*              ROUTINE HANDLES DRIVER HOOKS FOR SUMMARIES                       
         SPACE 1                                                                
SUMDRIVE NTR1                                                                   
         XC    SUBTACC,SUBTACC     CLEAR SUBTOTAL ACCUMULATOR                   
         LA    R3,NALL             R3=N'POSSIBLE ACCUMS                         
         SPACE 1                                                                
SUMDRV5  CH    R3,=Y(NSUBT)        IF NET+P&H+I&R+CSF ACCUM                     
         BNE   SUMDRV7                                                          
         CLI   FORMAT,TAEPFA       SHOW FOR FORMAT A/B ONLY                     
         BE    SUMDRV8                                                          
         CLI   FORMAT,TAEPFB                                                    
         BE    SUMDRV8                                                          
         B     SUMDRV9                                                          
         SPACE 1                                                                
SUMDRV7  CH    R3,=Y(NNETT)        IF NET SUBTOTAL ACCUMULATOR                  
         BNE   *+12                                                             
         CLI   GLOPTS+6,C'S'       SHOW ONLY IF HORIZ TOTS=S                    
         BNE   SUMDRV9                                                          
         SPACE 1                                                                
SUMDRV8  STC   R3,SUMINDEX         SAVE INDEX NUMBER FOR SUMMARY ACC.           
         BAS   RE,INDRIVE          OFF TO DRIVER                                
         SPACE 1                                                                
SUMDRV9  BCT   R3,SUMDRV5          LOOP FOR EACH ACCUM                          
         MVI   SUMINDEX,0          CLEAR SUMMARY INDEX                          
         B     CALLDRVX                                                         
         EJECT                                                                  
*              ROUTINE HANDLES DRIVER HOOKS FOR WORK-CODE RECAP                 
         SPACE 1                                                                
WCDRIVE  NTR1                                                                   
         MVC   SVMODE,ESTMODE                                                   
         MVI   ESTMODE,PROCWC      SET PROCESSING WORK-CODES                    
         SPACE 1                                                                
         L     RE,WCTAB            CLEAR WORK-CODE TABLE                        
         L     RF,MAXWKCD                                                       
         MHI   RF,WCLNQ            MULTIPLY MAX N'ENTRIES*L'ENTRY               
         XCEFL                                                                  
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(TAINTER PARAMETER BLOCK)                
         USING TND,R3                                                           
         XC    0(TNLNQ,R3),0(R3)                                                
         ST    RC,TNRC             A(GEND)                                      
         LA    R1,SYSCOMM          A(SYSTEM COMMON ROUTINES)                    
         ST    R1,TNASYCOM                                                      
         LA    R1,IFEL             INTERFACE ELEMENT                            
         ST    R1,TNAIFEL                                                       
         MVC   TGONOF,TCCAONOF     CAMERA STATUS                                
         MVC   TNCASTA2,TCCASTA2   CAST STATUS BYTE 2 (FOR CELEBRITY)           
         SPACE 1                                                                
         L     R1,TCPAYI           PAYMENT AMOUNT                               
         A     R1,TCPAYC                                                        
         A     R1,TCHNW                                                         
         A     R1,TCINR                                                         
         A     R1,TCEXP                                                         
         A     R1,CSFAMT                                                        
         ST    R1,TNPAY                                                         
         MVC   TNPNH,TCPNH         P&H                                          
         MVC   TNTAX,TAXAMT        PAYROLL TAXES                                
         MVC   TNHAND,HNDAMT       HANDLING                                     
         MVC   TNCOMM,COMMAMT      AGENCY COMMISSION                            
         L     R4,WCTAB            R4=A(WORK-CODE TABLE)                        
         ST    R4,TNAWCTAB                                                      
         SPACE 1                                                                
         GOTO1 TAINTER,DMCB,(R3)   BUILD WORK-CODE TABLE                        
         SPACE 1                                                                
         USING WCD,R4              R4=A(WORK-CODE TABLE)                        
         L     R0,MAXWKCD          R0=MAXIMUM NUMBER OF TABLE ENTRIES           
WCD20    OC    WCD(WCLNQ),WCD                                                   
         BZ    WCDX                                                             
         ST    R4,ATHSWC           SET A(CURRENT WORK-CODE TABLE ENTRY)         
         BAS   RE,INDRIVE          OFF TO DRIVER                                
         LA    R4,WCNEXT                                                        
         BCT   R0,WCD20                                                         
         SPACE 1                                                                
WCDX     MVC   ESTMODE,SVMODE      RESTORE SAVED MODE                           
         B     CALLDRVX                                                         
         SPACE 2                                                                
NOTNOWD  L     R3,ATWA                                                          
         USING T702FFD,R3                                                       
         LA    R2,CONWHENH         REQUEST TOO BIG FOR ONLINE PROC.             
         MVI   ERROR,NOTONLIN                                                   
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
SHFSAVE  MVC   WORK(0),0(R3)       SAVE IT IN WORK                              
SHFCLEAR MVC   0(0,R3),SPACES      CLEAR IT IN HEADS                            
SHFMOVE  MVC   0(0,R2),WORK        MOVE IT TO NEW POSITION                      
         SPACE 2                                                                
PWIDTH   DC    H'132'              CURRENT PRINT LINE WIDTH                     
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES RECORD TRACES                                    
         SPACE 1                                                                
         DS    0D                                                               
MYTRACE  NMOD1 0,*TRACE*                                                        
         L     RC,SAVRC                                                         
*                                                                               
         CLI   TRACEOPT,C'N'       IF TRACING                                   
         BE    MYTRACEX                                                         
         L     R2,0(R1)                                                         
         ZIC   R3,0(R1)                                                         
         MVC   WORK,0(R2)          MOVE LITERAL TO WORK                         
         LA    R4,WORK(R3)                                                      
         MVC   0(7,R4),=C' (AIO?)' ADD I/O AREA LITERAL                         
         LA    R3,7(R3)            BUMP L'LITERAL                               
         CLC   AIO,AIO1                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'1'          SET CURRENT I/O AREA LITERAL                 
         CLC   AIO,AIO2                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'2'                                                       
         CLC   AIO,AIO3                                                         
         BNE   *+8                                                              
         MVI   5(R4),C'3'                                                       
         TM    STATUS,FLUSHING                                                  
         BZ    *+14                                                             
         MVC   7(8,R4),=C' - FLUSH'                                             
         LA    R3,8(R3)                                                         
         GOTO1 TRACE,DMCB,AIO,0,WORK,(R3)                                       
MYTRACEX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE HANDLES STORAGE TRACES                                   
         SPACE 1                                                                
         DS    0D                                                               
MYTRACE2 NMOD1 0,*TRAC2*                                                        
         L     RC,SAVRC                                                         
*                                                                               
         CLI   TRACEOPT,C'N'       IF TRACING                                   
         BE    MYTRAC2X                                                         
         LM    R2,R4,0(R1)                                                      
         ZIC   RF,0(R1)                                                         
         MVC   WORK,0(R2)                                                       
         LA    R2,WORK(RF)                                                      
         TM    STATUS,FLUSHING                                                  
         BZ    *+14                                                             
         MVC   0(8,R2),=C' - FLUSH'                                             
         LA    RF,8(RF)                                                         
         GOTO1 TRACE,DMCB,(R3),(R4),WORK,(RF)                                   
MYTRAC2X XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENEOPTS                                                     
         EJECT                                                                  
*              VARIOUS TABLES, ETC.                                             
         SPACE 1                                                                
*              OPTION TABLE                                                     
OPTTAB   DS    0C                                                               
         DC    AL1(OPADDL,LOPADDL,OPIGNORE)                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(OPCANTX,LOPCANTX,0)                                          
         DC    AL2(CANIN-T70241)                                                
*                                                                               
         DC    AL1(OPCOMM,LOPCOMM,OPOKACTL)                                     
         DC    AL2(COMMIN-T70241)                                               
*                                                                               
         DC    AL1(OPEORTX,LOPEORTX,0)                                          
         DC    AL2(EORIN-T70241)                                                
*                                                                               
         DC    AL1(OPEXP,LOPEXP,0)                                              
         DC    AL2(EXPIN-T70241)                                                
*                                                                               
         DC    AL1(OPLAST,LOPLAST,0)                                            
         DC    AL2(LASTIN-T70241)                                               
*                                                                               
         DC    AL1(OPGUAR,LOPGUAR,0)                                            
         DC    AL2(GUARIN-T70241)                                               
*                                                                               
         DC    AL1(OPGRTNO,LOPGRTNO,0)                                          
         DC    AL2(GRTNOIN-T70241)                                              
*                                                                               
         DC    AL1(OPSSN,LOPSSN,0)                                              
         DC    AL2(SSNIN-T70241)                                                
*                                                                               
         DC    AL1(OPPRI,LOPPRI,0)                                              
         DC    AL2(PRIIN-T70241)                                                
*                                                                               
         DC    AL1(OPI,LOPI,OPIGNORE)                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(OPINCTX,LOPINCTX,0)                                          
         DC    AL2(INCIN-T70241)                                                
*                                                                               
         DC    AL1(OPOVER,LOPOVER,0)                                            
         DC    AL2(OVERIN-T70241)                                               
*                                                                               
         DC    AL1(OPOVER2,LOPOV2,0)                                            
         DC    AL2(OVER2IN-T70241)                                              
*                                                                               
         DC    AL1(OPOV12,LOPOV12,0)                                            
         DC    AL2(OVER12IN-T70241)                                             
*                                                                               
         DC    AL1(OPINTEG,LOPINTEG,0)                                          
         DC    AL2(INTIN-T70241)                                                
*                                                                               
         DC    AL1(OPLLIFT,LOPLLIFT,0)                                          
         DC    AL2(LLIFTIN-T70241)                                              
*                                                                               
         DC    AL1(OPLLIF2,LOPLLIF2,0)                                          
         DC    AL2(LLIFT2IN-T70241)                                             
         EJECT                                                                  
*              OPTION TABLE  (CONT'D)                                           
         SPACE 1                                                                
         DC    AL1(OPMULT,LOPMULT,OPOKACTL)                                     
         DC    AL2(MULTIN-T70241)                                               
*                                                                               
         DC    AL1(OPEXCH,LOPEXCH,OPOKACTL)                                     
         DC    AL2(EXCHIN-T70241)                                               
*                                                                               
         DC    AL1(OPCTYP,LOPCTYP,0)                                            
         DC    AL2(CTYPIN-T70241)                                               
*                                                                               
         DC    AL1(OPNO,LOPNO,OPFORUSE+OPOKACTL)                                
         DC    AL2(NIN-T70241)                                                  
*                                                                               
         DC    AL1(OPX,LOPX,0)                                                  
         DC    AL2(XIN-T70241)                                                  
*                                                                               
         DC    AL1(OPPERF,LOPPERF,OPIGNORE)                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(OPYES,LOPYES,OPIGNORE)                                       
         DC    AL2(0)                                                           
*                                                                               
         DC    AL1(OPYEAR,LOPYEAR,0)                                            
         DC    AL2(YEARIN-T70241)                                               
*                                                                               
         DC    AL1(OPUSE,LOPUSE,OPFORUSE+OPOKACTL)                              
         DC    AL2(0)              INPUT OF USE TYPES HANDLED MANUALLY          
*                                                                               
         DC    AL1(OPMAJOR,LOPMAJOR,OPFORUSE)                                   
         DC    AL2(MAJIN-T70241)                                                
*                                                                               
         DC    AL1(OPUNITS,LOPUNITS,OPFORUSE)                                   
         DC    AL2(UNITSIN-T70241)                                              
*                                                                               
         DC    AL1(OPINS,LOPINS,OPFORUSE)                                       
         DC    AL2(INSIN-T70241)                                                
*                                                                               
         DC    AL1(OPPAY1,LOPPAY1,OPFORUSE)                                     
         DC    AL2(PAY1IN-T70241)                                               
*                                                                               
         DC    AL1(OPUSES,LOPUSES,OPFORUSE)                                     
         DC    AL2(USESIN-T70241)                                               
*                                                                               
         DC    AL1(OPUSES2,LOPUSES2,OPFORUSE)                                   
         DC    AL2(USES2IN-T70241)                                              
*                                                                               
         DC    AL1(OPPCT,LOPPCT,OPFORUSE)                                       
         DC    AL2(PCTIN-T70241)                                                
*                                                                               
         DC    AL1(OPAMT,LOPAMT,OPFORUSE)                                       
         DC    AL2(AMTIN-T70241)                                                
*                                                                               
         DC    AL1(OPCYC,LOPCYC,OPFORUSE+OPOKACTL)                              
         DC    AL2(CYCLEIN-T70241)                                              
*                                                                               
         DC    AL1(OPDATE,LOPDATE,OPFORUSE+OPOKACTL)                            
         DC    AL2(DATEIN-T70241)                                               
         EJECT                                                                  
*              OPTION TABLE  (CONT'D)                                           
         SPACE 1                                                                
         DC    AL1(OPASOF,LOPASOF,OPFORUSE+OPOKACTL)                            
         DC    AL2(ASOFIN-T70241)                                               
*                                                                               
         DC    AL1(OPL,LOPL,OPFORUSE)                                           
         DC    AL2(LIN-T70241)                                                  
*                                                                               
         DC    AL1(OPS,LOPS,OPFORUSE)                                           
         DC    AL2(SIN-T70241)                                                  
*                                                                               
         DC    AL1(OPHIST,LOPHIST,OPFORUSE)                                     
         DC    AL2(HISTIN-T70241)                                               
*                                                                               
         DC    AL1(OPAPPLY,LOPAPPLY,OPFORUSE)                                   
         DC    AL2(APPLYIN-T70241)                                              
*                                                                               
         DC    AL1(OPCOMNM,LOPCOMNM,0)                                          
         DC    AL2(COMNMIN-T70241)                                              
*                                                                               
         DC    AL1(OPOVAS,LOPOVAS,0)                                            
         DC    AL2(OVASIN-T70241)                                               
*                                                                               
         DC    AL1(OPOV2AS,LOPOV2AS,0)                                          
         DC    AL2(OV2ASIN-T70241)                                              
*                                                                               
         DC    AL1(OPOV12AS,LOPOV12A,0)                                         
         DC    AL2(OV12ASIN-T70241)                                             
*                                                                               
         DC    AL1(OPPNH,LOPPNH,OPFORUSE)                                       
         DC    AL2(PNHIN-T70241)                                                
*                                                                               
         DC    AL1(OPSPNH,LOPSPNH,OPFORUSE)                                     
         DC    AL2(SPNHIN-T70241)                                               
*                                                                               
         DC    AL1(OPPNHR,LOPPNHR,0)                                            
         DC    AL2(PNHRIN-T70241)                                               
*                                                                               
         DC    AL1(OPYRAS,LOPYRAS,0)                                            
         DC    AL2(YRASIN-T70241)                                               
*                                                                               
         DC    AL1(OPSTATE,LOPSTATE,0)                                          
         DC    AL2(STATEIN-T70241)                                              
*                                                                               
         DC    AL1(OPAFM,LOPAFM,0)                                              
         DC    AL2(AFMIN-T70241)                                                
*                                                                               
         DC    AL1(OPLIFT,LOPLIFT,0)                                            
         DC    AL2(LIFTIN-T70241)                                               
*                                                                               
         DC    AL1(OPSLIFT,LOPSLIFT,0)                                          
         DC    AL2(SLIFTIN-T70241)                                              
*                                                                               
         DC    AL1(OPALLIN,LOPALLIN,0)                                          
         DC    AL2(ALLIN-T70241)                                                
*                                                                               
         DC    AL1(OPLONLY,LOPLONLY,0)                                          
         DC    AL2(LONLYIN-T70241)                                              
*                                                                               
         DC    AL1(OPSONLY,LOPSONLY,0)                                          
         DC    AL2(SONLYIN-T70241)                                              
*                                                                               
         DC    AL1(OPFSONLY,LOPFSNLY,0)                                         
         DC    AL2(FSONLYIN-T70241)                                             
*                                                                               
         DC    AL1(OPNAP,LOPNAP,0)                                              
         DC    AL2(NAPIN-T70241)                                                
         EJECT                                                                  
*              OPTION TABLE  (CONT'D)                                           
         SPACE 1                                                                
         DC    AL1(OPATYP,LOPATYP,0)                                            
         DC    AL2(ATYPIN-T70241)                                               
*                                                                               
         DC    AL1(OPCSF,LOPCSF,OPFORUSE)                                       
         DC    AL2(CSFIN-T70241)                                                
*                                                                               
         DC    AL1(OPHLD,LOPHLD,0)                                              
         DC    AL2(HLDIN-T70241)                                                
*                                                                               
         DC    AL1(OPHLDR,LOPHLDR,0)                                            
         DC    AL2(HLDRIN-T70241)                                               
*                                                                               
         DC    AL1(OPHLDAS,LOPHLDAS,0)                                          
         DC    AL2(HLDASIN-T70241)                                              
*                                                                               
         DC    AL1(OPHLDRAS,LOPHLDRA,0)                                         
         DC    AL2(HLDRASIN-T70241)                                             
*                                                                               
         DC    AL1(OP1ST,LOP1ST,0)                                              
         DC    AL2(FRSTIN-T70241)                                               
*                                                                               
         DC    AL1(OPHNDTX,LOPHNDTX,OPFORUSE)                                   
         DC    AL2(HNDIN-T70241)                                                
*                                                                               
         DC    AL1(OPTAG,LOPTAG,OPFORUSE)                                       
         DC    AL2(TAGIN-T70241)                                                
*                                                                               
         DC    AL1(OPDEMO,LOPDEMO,OPFORUSE)                                     
         DC    AL2(DEMOIN-T70241)                                               
*                                                                               
         DC    AL1(OPLCLAU,LOPLCLAU,OPFORUSE)                                   
         DC    AL2(UNITSIN-T70241)                  !!! FOR NOW                 
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              ROUTINE BUILDS USAGE HISTORY ELEMENT                             
         SPACE 1                                                                
         USING HISTD,R2            R2=A(NEXT AVAIL. SLOT IN TABLE)              
BLDTAUH  NTR1  BASE=*,LABEL=*                                                   
         XC    TCTAUHEL,TCTAUHEL   BUILD LOCAL USAGE HISTORY EL.                
         LA    R4,TCTAUHEL                                                      
         USING TAUHD,R4                                                         
         MVI   TAUHEL,TAUHELQ      CODE                                         
         MVI   TAUHLEN,TAUHLNQ     LENGTH                                       
         MVC   TAUHTYPE,TGUSTYP    USE TYPE                                     
         MVC   TAUHSTRT(6),TCPCYC  CYCLE DATES                                  
         SPACE 1                                                                
         CLI   TGUSEQU,UITN        SPECIAL FOR ITN                              
         JE    *+8                                                              
         CLI   TGUSEQU,UPAX        SPECIAL FOR PAX                              
         JE    *+8                                                              
         CLI   TGUSEQU,UCLA        SPECIAL FOR CLASS A                          
         JNE   BTAUH4                                                           
         CLI   MYUSSTUS,C'Y'       IF NOT OVERRIDDEN                            
         JE    *+10                                                             
         MVC   TCNUSES,HISTUSN     N'USES                                       
         MVC   TCNUSESL,HISTUSNL   N'USES TO LIFT ONLY                          
         J     XIT                                                              
         SPACE 1                                                                
BTAUH4   CLI   TGUSEQU,UCBL        SPECIAL FOR CABLE                            
         JE    *+12                                                             
         CLI   TGUSEQU,USCB        OR SPANISH CABLE                             
         JNE   *+14                                                             
         MVC   TAUHCBUN,HISTUNIT   UNITS                                        
         J     XIT                                                              
         SPACE 1                                                                
         TM    TGUSTYST,INSERTS    SPECIAL FOR INSERTS FOR BOOKENDS             
         JZ    *+10                                                             
         MVC   TAUHINS,HISTINS     INSERTS                                      
         CLI   TGUSEQU,ULCB        IF LOCAL CABLE                               
         JE    XIT                                                              
         CLI   TGUSEQU,URRN        IF RRN                                       
         JNE   *+14                                                             
         MVC   TAUHRMAJ,HISTMAJ    MAJORS                                       
         J     XIT                                                              
         TM    TGUSTYST,MAJORS                                                  
         JZ    *+10                                                             
         MVC   TAUHMAJ,HISTMAJ     MAJORS                                       
         TM    TGUSTYST,UNITS                                                   
         JZ    *+10                                                             
         MVC   TAUHUNT,HISTUNIT    UNITS                                        
         J     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TASYSNAHLD                                                     
         EJECT                                                                  
       ++INCLUDE TATRNTR                                                        
       ++INCLUDE TAT2TACC                                                       
       ++INCLUDE TASYSCSF                                                       
         SPACE 2                                                                
         DS    0D                  SYSIO BLOCK                                  
         DC    C'*SYSIOD*'                                                      
SYSIOD   DC    (TIEND-TASYSIOD)X'00'                                            
         SPACE 2                                                                
       ++INCLUDE TAGENESTD                                                      
         EJECT                                                                  
       ++INCLUDE TAESTDSECT                                                     
         EJECT                                                                  
*              DSECT TO COVER OPTION TABLE                                      
         SPACE 1                                                                
OPTD     DSECT                                                                  
OPTCODE  DS    XL1                 EQUATE IDENTIFYING THIS OPTION               
OPTSUBLN DS    XL1                 L'SUB-ELEMENT DATA                           
OPTSTAT  DS    XL1                                                              
OPIGNORE EQU   X'80'               IGNORE THIS SUB-ELEMENT                      
OPFORUSE EQU   X'40'               USE TYPE-RELATED SUB-ELEMENT                 
OPOKACTL EQU   X'20'               RECOGNIZE FOR ACTUAL PAYMENTS                
OPTDISP  DS    AL2                 DISPLACEMENT TO SET ROUTINE                  
OPTNEXT  EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER ADPCCOMS ROUTINE VARIABLES                        
         SPACE 1                                                                
APCD     DSECT                                                                  
APCOIO   DS    F                   ORIGINAL VALUE OF AIO                        
APCAGY   DS    CL(L'TLESAGY)       ESTIMATE AGENCY                              
APCAEL   DS    F                   A(LAST READ ESTIMATE ELEMENT)                
APCSVCA  DS    XL(L'KEY)           SAVED CAST KEY                               
APCCID   DS    XL(L'TAESCID)       COMMERCIAL ID                                
APCCOM   DS    XL(L'TAESCOM)       INTERNAL COMMERCIAL NUMBER                   
APCLNQ   EQU   *-APCD                                                           
         DSECT                                                                  
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
       ++INCLUDE TAYTDD                                                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR40D                                                       
         EJECT                                                                  
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TASYSIOD                                                                      
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* FAGETTXTD                                                                     
* FAREPBLK                                                                      
* TAGENFILE                                                                     
* DRGLOBAL                                                                      
* DDDROOLD                                                                      
* DDBIGBOX                                                                      
* DDPERVALD                                                                     
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAGETTXTD                                                      
FAREPBLKD DSECT                                                                 
       ++INCLUDE FAREPBLK                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDDROOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'192TAGEN41   10/18/16'                                      
         END                                                                    
