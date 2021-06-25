*          DATA SET TAGEND3    AT LEVEL 034 AS OF 08/27/13                      
*PHASE T702D3E,*                                                                
         TITLE 'T702D3 - ESTIMATE REBUILD'                                      
T702D3   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYAREAL,T702D3,R7,CLEAR=YES                                      
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R5,TWAHOLE          R5=LOCAL WORKING STORAGE                     
         USING REBD,R5                                                          
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         ST    R3,ACOMTAB          A(COMMERCIAL TABLE)                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         LA    RE,WKIO1                                                         
         AHI   RE,L'WKIO1                                                       
         ST    RE,AWKIO2                                                        
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         ZAP   COUNTER,=P'0'       COUNT OF ESTIMATE RECORDS PROCESSED          
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R3,ACOMTAB                                                       
         AH    R3,=Y(TABLNQ)                                                    
         ST    R3,AOLDEST          ADDRESS OF OLD ESTIMATE RECORD               
         L     R3,AIO1             AIO1(/AIO2/AIO3)                             
         ST    R3,ANEWEST          ADDRESS OF NEW ESTIMATE RECORD               
*                                                                               
         BAS   RE,REBUILD          REBUILD ESTIMATE RECORD(S)                   
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    WHEN,X'40'          IF NOT REQUESTED NOW                         
         BO    *+12                                                             
         CLI   TGCTSTTY,TASTTYPP   MUST BE PROGRAMMER                           
         BNE   ERRPRNT                                                          
*                                                                               
         LA    R1,WKIO1            SET ADDRESS OF WORK IO                       
         ST    R1,AIO                                                           
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SERAGYH),SERAGYNH                     
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,SERESTH                                                       
         XC    TIQSTART,TIQSTART   CLEAR SYSIO START ESTIMATE                   
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)          IF PROCESSING ALL ESTIMATES                  
         BNZ   VK15                                                             
         CLI   TGCTSTTY,TASTTYPP   MUST BE A PROGRAMMER                         
         BNE   ERRMISS                                                          
         TM    WHEN,X'40'                                                       
         BO    NOTNOW              REQUEST NOT AVAILABLE 'NOW'                  
         B     VK20                                                             
*                                                                               
VK15     BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)   SET SYSIO ESTIMATE FILTER                    
         GOTO1 RECVAL,DMCB,TLESCDQ,(X'08',SERESTH),SERESTNH                     
*                                                                               
VK20     XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM SCREEN           
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLESCDQ      SET TO READ ESTIMATE RECORD                  
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL RECORDS                                       
         SPACE 1                                                                
REBUILD  NTR1                                                                   
         LA    R0,IOHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   PASS SOME MORE THINGS                        
         MVC   TIACCS,TWAACCS      LIMIT ACCESS                                 
         MVC   TIAUTH,TWAAUTH      AUTORIZATION                                 
         XC    SVCLI,SVCLI         CLEAR SAVED VALUES                           
         XC    SVPRD,SVPRD                                                      
*                                                                               
         GOTO1 TASYSIO,DMCB,TASYSIOD  PASS CONTROL TO SYSIO FOR LIST            
*                                                                               
         CP    COUNTER,=P'0'       AND WE REPORTED SOMETHING                    
         BE    REBX                                                             
         BAS   RE,PRNT             SKIP A LINE                                  
         SPACE 1                                                                
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT  AND PRINT TOTALS          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(6,R1),=C'RECORD'                                               
         LA    R1,6(R1)                                                         
         CP    COUNTER,=P'1'                                                    
         BE    *+12                                                             
         MVI   0(R1),C'S'                                                       
         LA    R1,1(R1)                                                         
         MVC   1(7,R1),=C'CHANGED'                                              
         BAS   RE,PRNT                                                          
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    REBX                                                             
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
REBX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS SYSIO HOOKS                                   
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         L     R3,TIAREC           R3=A(ESTIMATE RECORD)                        
         USING TLESD,R3                                                         
*                                                                               
         CLI   TLESSEQ,0           REJECT IF NOT PRIMARY RECORD                 
         BNE   XIT                                                              
*                                                                               
         BAS   RE,CLRTAB           CLEAR COMTAB                                 
         MVC   CUREST,TLESEST      SET CURRENT ESTIMATE NUMBER                  
         BAS   RE,GETEREC          GET COMPLETE ESTIMATE RECORD                 
         BE    *+6                                                              
         DC    H'0'                PROBLEM WITH ESTIMATE RECORD                 
         BAS   RE,SETNREC          SET NEW ESTIMATE RECORD AREA                 
         BAS   RE,RDCOMML          READ COMMERCIAL RECORDS                      
         OC    TABCNT,TABCNT       IF DETAILS IN TABLE                          
         BZ    IOHOOK20                                                         
         BAS   RE,SORTTAB          XSORT THE COMMERCIAL TABLE                   
         BAS   RE,FINNREC          FINISH SETTING NEW ESTIMATE RECORD           
         BAS   RE,CHKCHG           CHECK ANY CHANGE MADE TO ESTIMATE            
         CLI   ANYCHNG,C'N'        IF CHANGES TO ESTIMATE RECORD                
         BE    IOHOOK20                                                         
*                                                                               
         MVI   AIOTYPE,C'O'                                                     
         BAS   RE,PRINTIT          PRINT OLD ESTIMATE INFORMATION               
         MVI   AIOTYPE,C'N'        THEN,                                        
         BAS   RE,PRINTIT          PRINT NEW ESTIMATE INFORMATION               
         BAS   RE,BXBOT            PRINT BOTTOM LINE                            
**NO-OP**GOTO1 ACTVIN,DMCB,(X'C0',0)                                            
**NO-OP**LA    R4,ELEMENT                                                       
**NO-OP**BAL   RE,MYADDL           ADD SCRN ACTIVITY ELE TO NEW EST             
         BAS   RE,SPLIT            SPLIT/WRITE NEW ESTIMATE RECORDS             
         AP    COUNTER,=P'1'       ADD TO ESTIMATE RECORD COUNT                 
*                                                                               
IOHOOK20 MVC   KEY,TIKEY           RESTORE READ SEQUENCE                        
         LA    R3,KEY                                                           
         OC    TIQSTART,TIQSTART   IF SPECIFIC ESTIMATE REQUESTED               
         BZ    *+8                                                              
         MVI   TLESAGY,X'FF'       FORCE SYSIO TO STOP                          
         GOTO1 HIGH                                                             
         B     XIT                 RETURN TO SYSIO                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GET AN ESTIMATE RECORD                                
*                                  RETURNS WITH RECORD IN AOLDEST               
GETEREC  NTR1                                                                   
         MVC   KEY,TIKEY           SET KEY                                      
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R3,KEY                                                           
         USING TLESD,R3                                                         
         CLC   TLESKEY,KEYSAVE     DID WE GET THE RECORD                        
         BNE   NO                                                               
         SPACE 1                                                                
         MVC   KEYSAVE,TLESKEY     SAVE ENTIRE DIR. REC. IN KEYSAVE             
         MVC   AIO,AOLDEST                                                      
         GOTO1 GETREC              GET RECORD INTO AOLDEST                      
         LA    RE,WKIO1                                                         
         ST    RE,AIO                                                           
         SPACE 1                                                                
GREC4    GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         LA    R3,KEY                                                           
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE  WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO WKIO1                    
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R3,AOLDEST                             R3=A(MAIN RECORD)         
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
GRECX    MVC   KEY,KEYSAVE         RESTORE ORIG. DIR. REC. TO KEY               
         B     YES                 AOLDEST HAS A(RECORD)                        
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SET PRELIMINARY NEW ESTIMATE REC INFO                 
*                                                                               
SETNREC  NTR1                                                                   
         L     R4,AOLDEST          R4=A(OLD ESTIMATE RECORD)                    
         USING TLESD,R4                                                         
         L     R3,ANEWEST          R3=A(NEW ESTIMATE RECORD)                    
         ST    R3,AIO                                                           
         USING TLRCD,R3                                                         
*                                                                               
         MVC   0(L'TLRCKEY,R3),TLESKEY SET KEY IN NEW RECORD                    
         MVC   TLRCLEN,DATADISP        SET LENGTH                               
         XC    TLRCSTAT(10),TLRCSTAT   CLEAR STATUS                             
*                                                                               
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SETNR10  BAS   RE,NEXTEL           GET AN ELEMENT                               
         BNE   SETNRX                                                           
*                                                                               
         CLI   0(R4),TAESELQ       IF ESTIMATE ELEMENT                          
         BE    SETNR10             SKIP FOR NOW                                 
         MVC   ELEMENT,0(R4)                                                    
         GOTO1 ADDELEM             OTHERWISE, ADD ELEM TO NEW EST REC           
         B     SETNR10                                                          
         SPACE 1                                                                
SETNRX   B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO READ COMMERCIALS SET IN COMTAB                        
RDCOMML  NTR1                                                                   
         XC    SVCLI,SVCLI         CLEAR SAVED VALUES                           
         XC    SVPRD,SVPRD                                                      
         XC    SVCOM,SVCOM                                                      
         XC    SVCIDI,SVCIDI                                                    
         SR    R2,R2               R2=TABLE COUNTER                             
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
         L     R4,AOLDEST          R4=A(OLD ESTIMATE RECORD)                    
         USING TAESD,R4                                                         
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RDCOM10  BAS   RE,NEXTEL           GET AN ESTIMATE ELEMENT                      
         BNE   RDCOMX                                                           
*                                                                               
         CLI   TAESTYPE,TAESTCLI   IF CLIENT                                    
         BNE   RDCOM15                                                          
         MVC   SVCLI,TAESCLI       SAVE CLIENT CODE                             
         XC    SVPRD,SVPRD         CLEAR PRODUCT CODE                           
         B     RDCOM50                                                          
*                                                                               
RDCOM15  CLI   TAESTYPE,TAESTPRD   IF PRODUCT                                   
         BNE   *+14                                                             
         MVC   SVPRD,TAESPRD       SAVE PRODUCT CODE                            
         B     RDCOM50                                                          
*                                                                               
         TM    TAESTYPE,TAESTCOM   IF COMMERCIAL                                
         BNO   RDCOM30                                                          
         TM    TAESTYPE,TAESTHYP   AND IF HYPOTHETICAL                          
         BNO   RDCOM18                                                          
         MVC   COMCTYP,TAESTYPE    SET COMMERCIAL TYPE                          
         MVC   COMCID,TAESDATA     SAVE COMMERCIAL INFO                         
         MVC   SVCOM,COMCOM        SAVE TYPE(1),CID(8),INT COM(4)               
         MVC   SVCCLI,SVCLI        RESET TO GLOBAL CLIENT                       
         MVC   SVCPRD,SVPRD        AND PRODUCT                                  
         B     RDCOM50             ADD TO TABLE                                 
*                                                                               
RDCOM18  LA    RE,WKIO1            READ REAL COMMERCIAL RECORD                  
         ST    RE,AIO                                                           
         MVC   SVCOMI,TAESCOM      SAVE INT COMM'L NUMBER                       
         OC    TAESCOM,TAESCOM     IF INTERNAL COMML                            
         BZ    RDCOM20                                                          
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAESCOM)                             
         MVI   ELCODE,TAESELQ      RESET ELEMENT CODE                           
         BNE   RDCOM20             DON'T ADD TO TABLE                           
         L     R1,AIO                                                           
         USING TLCOD,R1                                                         
         CLC   TIFAGY,TLCOAGY      IF AGENCY CHANGED                            
         BE    *+14                                                             
RDCOM20  MVC   SVCIDI,TAESCOM      SAVE BAD INTERNAL COMM #                     
         B     RDCOM10             SKIP IT                                      
*                                                                               
         MVC   SVCCLI,TLCOCLI      SAVE COMMERCIAL CLIENT FOR CASTS             
         MVC   SVCPRD,TLCOPRD      SAVE COMMERCIAL PRODUCT FOR CASTS            
         MVC   COMCLI,SVCCLI       SET CLIENT                                   
         MVC   COMPRD,SVCPRD       SET PRODUCT                                  
         MVC   COMCTYP,TAESTYPE    SET COMMERCIAL TYPE                          
         MVC   COMCID,TLCOCID      AND SET IN TABLE                             
         MVC   SVCOM,COMCOM        SAVE TYPE(1),CID(8),INT COM(4)               
         B     RDCOM60                                                          
*                                                                               
RDCOM30  TM    TAESTYPE,TAESTPER   IF CAST                                      
         BO    *+6                                                              
         DC    H'0'                UNDEFINED TAESD ELEMENT                      
         TM    TAESTYPE,TAESTHYP   IF REAL CAST                                 
         BO    *+14                                                             
         CLC   SVCIDI,SVCOMI                                                    
         BE    RDCOM10             SKIP IF BAD INTERNAL COMM'L NUMBER           
         MVC   COMCOM,SVCOM        ELSE, SET COMMERCIAL INFO                    
         OC    COMCID,COMCID                                                    
         BZ    RDCOM10             SKIP IF NO COMML INFO AT ALL                 
         MVC   COMCLI,SVCCLI       SET CLIENT                                   
         MVC   COMPRD,SVCPRD       SET PRODUCT                                  
         B     RDCOM60                                                          
*                                                                               
RDCOM50  MVC   COMCLI,SVCLI        SAVE CLIENT                                  
         MVC   COMPRD,SVPRD        SAVE PRODUCT                                 
RDCOM60  MVC   COMTYPE,TAESTYPE    SAVE ELEMENT TYPE                            
         MVC   COMSEQ,TAESSEQ      SAVE ELEMENT SEQUENCE NUMBER                 
         LA    R3,COMTABL(R3)      BUMP TO NEXT COMTAB ENTRY                    
         LA    R2,1(R2)            INCREMENT TABLE COUNTER                      
         B     RDCOM10             LOOP                                         
         SPACE 1                                                                
RDCOMX   ST    R2,TABCNT           SAVE COUNTER OF TABLE ENTRIES                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FINISH SETTING UP TAESD ELEMENTS IN                   
*              NEW ESTIMATE RECORD                                              
         SPACE                                                                  
FINNREC  NTR1                                                                   
*                                                                               
         BAS   RE,FIXTAB           FIX THE TABLE                                
         OC    TABCNT,TABCNT                                                    
         BZ    FINNRX                                                           
*                                                                               
         LA    R2,MAXCOM           R2=MAXIMUM NUMBER OF TABLE ENTRIES           
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
*                                                                               
         MVI   CURSEQ,0            START AT SEQUENCE ZERO FOR NEW REC           
*                                                                               
FINNR10  OC    0(COMTABL,R3),0(R3) TEST END OF TABLE                            
         BZ    FINNRX                                                           
         CLI   0(R3),X'FF'                                                      
         BE    FINNRX                                                           
         BAS   RE,SETELEM          MOVE ELEMENT TO NEW EST RECORD               
         LA    R3,COMTABL(R3)                                                   
         BCT   R2,FINNR10                                                       
FINNRX   B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE DELETES OLD ENTRIES, ADDES NEW ENTRIES              
*                           AND RESORTS THE TABLE                               
         SPACE                                                                  
FIXTAB   NTR1                                                                   
         BAS   RE,DELNTRY          DELETE ENTRIES AS NECESARY                   
         BAS   RE,ADDNTRY          ADDS ENTRIES AS NECESSARY                    
         BAS   RE,CNTTAB           COUNT ACTUAL ENTRIES IN TABLE                
         BAS   RE,SORTTAB          RESORT THE TABLE                             
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO XSORT THE COMMERCIAL TABLE                            
         SPACE                                                                  
SORTTAB  NTR1                                                                   
         LA    R2,COMKEYL          LENGTH OF KEY IN TABLE ENTRY                 
         LA    R3,COMTABL          LENGTH OF TABLE ENTRY                        
         L     R4,TABCNT           COUNT OF TABLE ENTRIES                       
         GOTO1 XSORT,DMCB,ACOMTAB,(R4),(R3),(R2),0                              
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE DELETES ANY CLI/PRD THAT HAS NO DETAILS             
*                                       AND DUPLICATE COMMERCIALS               
         SPACE                                                                  
DELNTRY  NTR1                                                                   
         L     R2,TABCNT           R2=NUMBER OF TABLE ENTRIES                   
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
*                                                                               
DELNT10  OC    0(COMTABL,R3),0(R3) IF NOT END OF TABLE                          
         BZ    DELNTX                                                           
         CLI   0(R3),X'FF'         AND VALID ENTRY                              
         BE    DELNT40                                                          
*                                                                               
         CLI   COMTYPE,TAESTCLI                                                 
         BNE   *+12                                                             
         BAS   RE,CHKCDTL          CHECK TO DELETE CLIENT ENTRY                 
         B     DELNT30                                                          
*                                                                               
         CLI   COMTYPE,TAESTPRD                                                 
         BNE   *+12                                                             
         BAS   RE,CHKPDTL          CHECK TO DELETE PRODUCT ENTRY                
         B     DELNT30                                                          
*                                                                               
         CLI   COMTYPE,TAESTCOM                                                 
         BNE   DELNT40                                                          
         BAS   RE,DUPCOM           MARK DUPLICATE COMML ELES DELETED            
         B     DELNT40                                                          
*                                                                               
DELNT30  BNE   *+8                                                              
         MVI   0(R3),X'FF'         MARK ENTRY DELETED                           
*                                                                               
DELNT40  LA    R3,COMTABL(R3)                                                   
         BCT   R2,DELNT10                                                       
DELNTX   B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE CHECKS IF ANY DETAIL FOR THIS CLIENT                
*                                  NTRY - R3=A(COMTAB),R2=(N'ENTRIES)           
*                                  XIT - NONE FOUND, IF CC NEQ                  
         SPACE                                                                  
CHKCDTL  NTR1                                                                   
         ZAP   DTLCNTR,=P'0'       DETAIL ENTRY CNTR (COMML OR CAST)            
         MVC   SVCLI,COMCLI        SAVE CLIENT CODE                             
         B     CHKC10              BUMP PAST CURRENT CLIENT ENTRY               
*                                                                               
CHKC5    OC    0(COMTABL,R3),0(R3) IF NOT EMPTY ENTRY                           
         BZ    CHKC15                                                           
         CLI   COMTYPE,TAESTCLI    SEARCH TILL NEXT CLIENT ENTRY                
         BE    CHKC15                                                           
         CLI   COMTYPE,TAESTPRD                                                 
         BE    CHKC10              SKIP PRODUCT ENTRIES                         
         CLC   SVCLI,COMCLI        IF DETAIL FOR CLIENT                         
         BNE   CHKC15                                                           
         AP    DTLCNTR,=P'1'       ADD TO DETAIL RECORD COUNT                   
         B     CHKC15                                                           
*                                                                               
CHKC10   LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,CHKC5                                                         
*                                                                               
CHKC15   CP    DTLCNTR,=P'0'                                                    
         B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE CHECKS IF ANY DETAIL FOR THIS PRODUCT               
*                                  NTRY - R3=A(COMTAB),R2=(N'ENTRIES)           
*                                  XIT - NONE FOUND, IF CC NEQ                  
         SPACE                                                                  
CHKPDTL  NTR1                                                                   
         ZAP   DTLCNTR,=P'0'       DETAIL ENTRY CNTR (COMML OR CAST)            
         MVC   SVCLI,COMCLI        SAVE CLIENT CODE                             
         MVC   SVPRD,COMPRD        SAVE PRODUCT CODE                            
         B     CHKP10              BUMP PAST CURRENT CLIENT ENTRY               
*                                                                               
CHKP5    OC    0(COMTABL,R3),0(R3) IF NOT EMPTY ENTRY                           
         BZ    CHKP15                                                           
         CLI   COMTYPE,TAESTCLI    SEARCH TILL NEXT CLIENT                      
         BE    CHKP15                                                           
         CLI   COMTYPE,TAESTPRD    OR NEXT PRODUCT ENTRY                        
         BE    CHKP15                                                           
         CLC   SVCLI,COMCLI        IF DETAIL FOR PRODUCT                        
         BNE   CHKP15                                                           
         CLC   SVPRD,COMPRD                                                     
         BNE   CHKP15                                                           
         AP    DTLCNTR,=P'1'       ADD TO DETAIL RECORD COUNT                   
         B     CHKP15                                                           
*                                                                               
CHKP10   LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,CHKP5                                                         
*                                                                               
CHKP15   CP    DTLCNTR,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE DELETES DUPLICATE COMMERCIAL INFO                   
*                                  NTRY - R3=A(COMTAB),R2=(N'ENTRIES)           
*                                  XIT - NONE FOUND, IF CC NEQ                  
         SPACE                                                                  
DUPCOM   NTR1                                                                   
         MVC   SVCIDI,COMCIDI      SAVE INTERNAL COMMERCIAL NUMBER              
         B     DUPC10              BUMP PAST CURRENT COMML ELEMENT              
*                                                                               
DUPC5    OC    0(COMTABL,R3),0(R3) IF NOT EMPTY ENTRY                           
         BZ    DUPCX                                                            
         CLI   0(R3),X'FF'         AND VALID ENTRY                              
         BE    DUPC10                                                           
         CLI   COMTYPE,TAESTCOM    SEARCH TILL NEXT COMMERCIAL                  
         BNE   DUPC10                                                           
         CLC   SVCIDI,COMCIDI      IF MATCH ON INTERNAL COMM                    
         BNE   DUPC10                                                           
         MVI   0(R3),X'FF'         MARK COMML FOR DELETION                      
         BAS   RE,DELMISC          MARK MISC ELEMENTS ATTACHED TO COMML         
*                                                                               
DUPC10   LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,DUPC5                                                         
DUPCX    B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE DELETES ALL PERF ELEMENTS ATTACHED                  
*              TO COMMERCIAL DELETED                                            
         SPACE                                                                  
DELMISC  NTR1                                                                   
         B     DELM10                                                           
*                                                                               
DELM5    OC    0(COMTABL,R3),0(R3) IF NOT EMPTY                                 
         BZ    DELMX                                                            
         TM    TAESTYPE,TAESTPER   IF CAST AND                                  
         BZ    DELMX                                                            
         CLC   SVCIDI,COMCIDI      MATCH ON INTERNAL COMM                       
         BNE   DELM10                                                           
         MVI   0(R3),X'FF'         MARK DETAIL FOR DELETION                     
*                                                                               
DELM10   LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,DELM5                                                         
DELMX    B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE ADDS CLI/PRD ELEMENTS                               
         SPACE                                                                  
ADDNTRY  NTR1                                                                   
         XC    SVCLI,SVCLI         CLEAR SAVED VALUES                           
         XC    SVPRD,SVPRD                                                      
         L     R2,TABCNT           R2=NUMBER TABLE ENTRIES                      
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
ADDNT10  OC    0(COMTABL,R3),0(R3) IF NOT END OF TABLE                          
         BZ    ADDNTX                                                           
         CLI   0(R3),X'FF'         AND IF VALID ENTRY                           
         BE    ADDNT50                                                          
*                                                                               
         CLI   COMTYPE,TAESTCLI    IF CLIENT ENTRY                              
         BNE   ADDNT20                                                          
         MVC   SVCLI,COMCLI        SAVE CLIENT CODE                             
         XC    SVPRD,SVPRD         CLEAR PRODUCT CODE                           
         B     ADDNT50                                                          
*                                                                               
ADDNT20  CLI   COMTYPE,TAESTPRD    IF PRODUCT ENTRY                             
         BNE   ADDNT25                                                          
         MVC   SVPRD,COMPRD        SAVE PRODUCT CODE                            
         B     ADDNT50                                                          
*                                                                               
ADDNT25  CLI   COMTYPE,TAESTCOM    IF COMMERCIAL ENTRY                          
         BNE   ADDNT50                                                          
         CLC   SVCLI,COMCLI        IF CLIENT CHANGED                            
         BE    ADDNT30                                                          
         MVC   SVCLI,COMCLI                                                     
         XC    SVPRD,SVPRD                                                      
         MVI   ELETYPE,TAESTCLI    ADD NEW CLIENT ELEMENT                       
         BAS   RE,ADDCLPR                                                       
*                                                                               
ADDNT30  CLC   SVPRD,COMPRD        IF PRODUCT CHANGED                           
         BE    ADDNT50                                                          
         OC    COMPRD,COMPRD       IF ANY PRODUCT CODE                          
         BZ    ADDNT50                                                          
         MVC   SVPRD,COMPRD                                                     
         MVI   ELETYPE,TAESTPRD    ADD NEW PRODUCT ELEMENT                      
         BAS   RE,ADDCLPR          ADD IT                                       
*                                                                               
ADDNT50  LA    R3,COMTABL(R3)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R2,ADDNT10                                                       
ADDNTX   B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE ADDS A CLIENT OR PRODUCT ENTRY TO TABLE             
ADDCLPR  NTR1                                                                   
         LA    R2,MAXCOM           R2=MAXIMUM NUMBER OF TABLE ENTRIES           
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
ADDCLPR5 OC    0(COMTABL,R3),0(R3) LOOK FOR EMPTY ENTRY                         
         BZ    ADDCLPR9                                                         
         CLI   0(R3),X'FF'                                                      
         BE    ADDCLPR9                                                         
         LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,ADDCLPR5         LOOP                                         
         DC    H'0'                TABLE FULL                                   
*                                                                               
ADDCLPR9 XC    0(COMTABL,R3),0(R3) MAKE SURE ITS CLEAR                          
         MVC   COMCLI,SVCLI        SET CLIENT                                   
         MVC   COMPRD,SVPRD        SET PRODUCT                                  
         MVC   COMTYPE,ELETYPE     SET ENTRY TYPE                               
         MVI   COMSEQ,COMNEWQ      INDICATE NEW ELEMENT                         
         B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE COUNTS CURRENT NUMBER OF TABLE ENTRIES              
CNTTAB   NTR1                                                                   
         XR    RE,RE               ACTUAL ENTRIES                               
         LA    R2,MAXCOM           R2=MAXIMUM NUMBER OF TABLE ENTRIES           
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
CNTTAB5  OC    0(COMTABL,R3),0(R3) IF VALID TABLE ENTRY                         
         BZ    CNTTABX                                                          
         LA    RE,1(RE)            ADD TO COUNT                                 
         LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R2,CNTTAB5          LOOP                                         
*                                                                               
CNTTABX  ST    RE,TABCNT           STORE TABLE COUNT                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              THIS ROUTINE SETS ELEMENT INTO NEW RECORD AREA                   
         USING COMTABD,R3                                                       
SETELEM  NTR1                                                                   
         MVC   ELETYPE,COMTYPE                                                  
         CLI   COMTYPE,TAESTCLI    IF CLIENT ENTRY                              
         BE    *+12                                                             
         CLI   COMTYPE,TAESTPRD    OR IF PRODUCT ENTRY                          
         BNE   SETEL30             MOVE IT FROM OLDEST                          
*                                                                               
         CLI   COMSEQ,COMNEWQ      AND NEW ENTRY                                
         BNE   SETEL30                                                          
         BAS   RE,ADDCPEL          ADD ELEMENT TO NEW                           
         B     *+8                                                              
*                                                                               
SETEL30  BAS   RE,MVCELEM          ELSE, MOVE ELEMENT FROM OLD TO NEW           
         B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE MOVES AN ELEMENT TO NEW EST RECORD                  
         SPACE                                                                  
MVCELEM  NTR1                                                                   
         MVC   AIO,AOLDEST         SET OLD ESTIMATE RECORD                      
         MVI   ELCODE,TAESELQ                                                   
         GOTO1 GETL,DMCB,(1,COMSEQ)  IF FIND ELEMEMT IN OLD EST REC             
         BE    *+6                                                              
         DC    H'0'                ELEMENT MUST BE IN OLDEST                    
*                                                                               
         L     R2,TGELEM                                                        
         MVC   ELEMENT,0(R2)                                                    
         LA    R4,ELEMENT                                                       
         USING TAESD,R4                                                         
         MVC   TAESSEQ,CURSEQ      SET NEW SEQUENCE NUMBER                      
         CLI   COMTYPE,TAESTCOM    IF COMMERCIAL                                
         BNE   *+10                                                             
         MVC   TAESCID,COMCID      RESET CID (MIGHT HAVE CHANGED)               
         BAS   RE,MYADDL           ADD ELEMENT AND INCREMENT CURSEQ             
         B     XIT                                                              
         SPACE 2                                                                
*              THIS ROUTINE ADDS CLT/PRD ELEMENT TO NEW EST RECORD              
         SPACE                                                                  
ADDCPEL  NTR1                                                                   
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         LA    R4,ELEMENT                                                       
         USING TAESD,R4                                                         
         MVI   TAESEL,TAESELQ      SET ELEMENT EQUATE                           
         MVI   TAESLEN,TAESLNQ     SET ELEMENT LENGTH                           
         MVC   TAESSEQ,CURSEQ      SET NEW SEQUENCE NUMBER                      
         MVC   TAESTYPE,ELETYPE    SET ELEMENT TYPE                             
         CLI   ELETYPE,TAESTCLI    IF ADDING CLIENT ELEMENT                     
         BNE   *+14                                                             
         MVC   TAESCLI,COMCLI      SET CLIENT                                   
         B     *+10                                                             
         MVC   TAESPRD,COMPRD      ELSE, SET PRODUCT                            
         BAS   RE,MYADDL           ADD ELEMENT AND INCREMENT CURSEQ             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD ELEMENT TO NEW ESTIMATE RECORD                    
*                                  NTRY- R4=A(ELEMENT)                          
               SPACE                                                            
MYADDL   NTR1                                                                   
         MVC   AIO,ANEWEST         SET ADDRESS OF NEW ESTIMATE RECORD           
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,(R4),0                              
         CLI   12(R1),5                                                         
         BE    MYADDL10                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
MYADDL10 BAS   RE,INCSEQ           INCREMENT SEQUENCE NUMBER                    
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO INCREMENT SEQUENCE NUM FOR NEW ELEMENTS               
               SPACE                                                            
INCSEQ   ZIC   R1,CURSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,CURSEQ                                                        
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE CHECKS IF ESTIMATE RECORD WAS CHANGED                    
*                                  IF NO CHANGE, ANYCHNG=N                      
         SPACE 1                                                                
CHKCHG   NTR1                                                                   
         MVI   ANYCHNG,C'Y'        RECORD CHANGED                               
         L     R2,AOLDEST          R2=A(OLD ESTIMATE RECORD)                    
         LH    R3,TLRCLEN-TLRCKEY(R2)                                           
         L     RE,ANEWEST          R3=A(NEW ESTIMATE RECORD)                    
         LH    RF,TLRCLEN-TLRCKEY(RE)                                           
         CR    R3,RF                                                            
         BNE   CHKCHGX             DIFFERENT LENGTHS => RECORD CHANGED          
*                                                                               
         CLCL  R2,RE               CHECK RECORD                                 
         BNE   *+8                                                              
         MVI   ANYCHNG,C'N'        NO CHANGE IN RECORD                          
CHKCHGX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SPLITS 'BIG' RECORDS INTO SMALLER ONES                   
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL THIS WILL KEEP IT ON UNTIL END             
         L     R4,ANEWEST          R4=A(NEW ESTIMATE RECORD)                    
         L     R3,AWKIO2                                                        
         ST    R3,AIO              R3=A(RECORD TO BE WRITTEN BACK)              
         USING TLESD,R3                                                         
         MVC   TLESKEY,0(R4)       SET KEY IN RECORD                            
         SPACE 1                                                                
         LA    R4,TLESELEM-TLESD(R4)  R4=A(FIRST ELEMENT)                       
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
         SPACE 1                                                                
SPL2     MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,TLESLEN             + L'RECORD                                
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL6                YES - GO RELEASE IT                          
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    SPL4                                                             
         SPACE 1                                                                
SPL6     BAS   RE,RELEASE          WRITE BACK RECORD IN IO2                     
         SPACE 1                                                                
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         SPACE 1                                                                
         BAS   RE,DELETE           DELETE ANY REMAINING RECORDS                 
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         OI    GENSTAT1,RDUPAPPL   RESET GENCON CONTROL SWITCH                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE RELEASES SPLIT-UP ESTIMATE RECORD                        
         SPACE 1                                                                
         USING TLESD,R3            R3=A(RECORD TO BE WRITTEN BACK)              
RELEASE  NTR1                                                                   
         MVC   KEY,TLESKEY         MOVE KEY FROM RECORD TO KEY                  
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         SPACE 1                                                                
         LA    R3,KEY              R3=A(DIRECTORY RECORD)                       
         CLC   TLESKEY,KEYSAVE     IS RECORD ALREADY ON FILE                    
         BE    REL8                                                             
         MVC   TLESKEY,KEYSAVE     NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD NEW RECORD TO FILE                   
         B     RELX                                                             
         SPACE 1                                                                
         USING TLDRD,R3                                                         
REL8     TM    TLDRSTAT,X'80'      IF DIRECTORY MARKED DELETED                  
         BZ    REL10                                                            
         XI    TLDRSTAT,X'80'      UNMARK IT                                    
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
REL10    LA    RE,WKIO1            RECORD EXISTS -                              
         ST    RE,AIO              READ IT INTO WKIO1                           
         GOTO1 GETREC                                                           
         L     RE,AWKIO2           RESET AIO TO A(NEW RECORD)                   
         ST    RE,AIO                                                           
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
         SPACE 1                                                                
RELX     NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         L     R3,AIO              R3=A(RECORD WE JUST ADDED/WROTE)             
         USING TLESD,R3                                                         
         ZIC   R1,TLESSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,2(R1)                                                         
         STC   R1,TLESSEQ                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DELETES SUBSEQUENT ESTIMATE RECORDS                      
         SPACE 1                                                                
DELETE   NTR1                                                                   
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLESD,R3                                                         
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   XIT                                                              
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLESSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
         EJECT                                                                  
*              ROUTINE TO PRINT NEW ESTIMATE RECORD INFORMATION                 
         SPACE                                                                  
PRINTIT  NTR1                                                                   
         BAS   RE,PRNTLBL          PRINT OLD/NEW                                
*                                                                               
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         L     R4,AOLDEST          R4=A(OLD/NEW ESTIMATE RECORD)                
         CLI   AIOTYPE,C'O'                                                     
         BE    *+8                                                              
         L     R4,ANEWEST                                                       
         MVI   ELCODE,TAESELQ      GET ESTIMATE ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRINT10  BAS   RE,NEXTEL                                                        
         BNE   PRINT50                                                          
*                                                                               
         CLI   TAESTYPE,TAESTCLI   IF CLIENT ELEMENT                            
         BNE   PRINT20                                                          
         CLC   P,SPACES            IF ANYTHING TO PRINT                         
         BE    *+8                                                              
         BAS   RE,PRNT             PRINT IT                                     
         MVC   LINCLI,TAESCLI      MOVE CLIENT TO PRINT LINE                    
         MVC   LINPRD,=CL6'ALL'    MOVE DEFAULT PRODUCT                         
         B     PRINT10                                                          
*                                                                               
PRINT20  CLI   TAESTYPE,TAESTPRD   IF PRODUCT ELEMENT                           
         BNE   PRINT25                                                          
         CLC   LINPRD,=CL6'ALL'    IF SOMETHING TO PRINT                        
         BNE   *+14                                                             
         CLC   LINCOM,SPACES                                                    
         BE    *+8                                                              
         BAS   RE,PRNT             PRINT IT                                     
         MVC   LINPRD,TAESPRD      MOVE PRODUCT TO PRINT LINE                   
         B     PRINT10                                                          
*                                                                               
PRINT25  TM    TAESTYPE,TAESTCOM   IF COMMERCIAL                                
         BNO   PRINT10                                                          
         CLC   LINCOM,SPACES       IF FIRST COMMERCIAL FOR LINE                 
         BNE   *+12                                                             
         LA    R3,LINCOM           R3=A(FIRST COMMERCIAL)                       
         B     PRINT35                                                          
*                                                                               
         MVI   0(R3),C','          ADD TO COMML OF CURRENT LINE                 
         LA    R3,1(R3)                                                         
*                                                                               
PRINT35  BAS   RE,PRCOM            PRINT THE COMMERCIAL                         
         B     PRINT10             LOOP                                         
*                                                                               
PRINT50  CLC   P,SPACES            IF LINE LEFT TO PRINT                        
         BE    *+8                                                              
         BAS   RE,PRNT             PRINT IT                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT FULL CID                                        
*                                  R2=A(PRINT LINE)                             
PRCOM    NTR1                                                                   
         BAS   RE,RERDCOM          REREAD COMML FOR TGCID                       
         CLC   TGCID,SPACES        IF NO CID FOUND                              
         BE    PRCOM20             PRINT ,,                                     
*                                                                               
PRCOM10  LR    R1,R3               R1=A(BEGINNING OF CURRENT COMML)             
         TM    TAESTYPE,TAESTHYP   IF HYPOTHETICAL                              
         BO    *+8                                                              
         TM    TAESTYPE,TAESTHP    IF HYPOTHETICAL PACKED                       
         BZ    PRCOM15                                                          
         MVC   0(2,R3),=C'H='                                                   
         AHI   R3,2                                                             
*                                                                               
PRCOM15  MVC   0(L'TGCID,R3),TGCID                                              
         LA    R3,L'TGCID-1(R3)    MOVE TO PRINT LINE                           
         CLI   0(R3),C' '          SQUASH IT                                    
         BH    *+8                                                              
         BCT   R3,*-8                                                           
*                                                                               
PRCOM20  LA    R3,1(R3)                                                         
         LA    RE,LINLNQ(R2)       RE=A(END OF LINE)                            
         CR    RE,R3               TEST PAST END OF LINE                        
         BNL   PRCOMX                                                           
         BCTR  R1,0                GRAB COMMA                                   
         MVC   0(L'TGCID+1,R1),SPACES                                           
         BAS   RE,PRNT             PRINT CURRENT LINE                           
         LA    R3,LINCOM           PT TO NEXT LINE                              
         B     PRCOM10             GO PRINT TGCID ON NEXT LINE                  
*                                                                               
PRCOMX   XIT1  REGS=(R3)           PASS BACK R3                                 
         EJECT                                                                  
*              THIS ROUTINE RE-READS COMMERCIAL RECORD FOR FULL CID             
*                                  TGCID= FULL CID                              
RERDCOM  NTR1                                                                   
         MVC   TGCID,SPACES                                                     
         TM    TAESTYPE,TAESTHYP   IF HYPOTHETICAL                              
         BNO   RERDC10                                                          
         CLI   TAESHCOM+9,0                                                     
         BE    RERDC05                                                          
         MVC   TGCID(2),=C'H='                                                  
         MVC   TGCID+2(L'TAESHCOM),TAESHCOM                                     
         B     RERDCX                                                           
*                                                                               
RERDC05  GOTO1 CHPACK,DMCB,(C'U',TAESHCOM),TGCID                                
         B     RERDCX                                                           
*                                                                               
RERDC10  MVC   TGCID(L'TAESCID),TAESCID DEFAULT TO 8 CHAR CID                   
         CLI   AIOTYPE,C'O'             IF OLD RECORD                           
         BE    RERDCX                   USE CID IN RECORD                       
         OC    TAESCOM,TAESCOM          ELSE, READ RECORD                       
         BNZ   *+6                                                              
         DC    H'0'                     MUST BE VALID INT COMML NUM             
         LA    RE,WKIO1                                                         
         ST    RE,AIO                                                           
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TAESCOM)                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   TGCID,TACOCID       SET FULL 12 CHAR CID                         
*                                                                               
         MVC   AIO,AOLDEST         RESET IOAREA                                 
         CLI   AIOTYPE,C'O'                                                     
         BE    *+10                                                             
         MVC   AIO,ANEWEST                                                      
         MVI   ELCODE,TAESELQ      RESET ELCODE                                 
*                                                                               
RERDCX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INDICATE NEW/OLD                                      
PRNTLBL  NTR1                                                                   
         LA    R2,P                                                             
         USING LINED,R2                                                         
*                                                                               
         MVC   LINEST,CUREST       PRINT ESTIMATE NUMBER                        
         MVC   LINCLI(5),=C'*OLD*'                                              
         CLI   AIOTYPE,C'O'                                                     
         BE    *+10                                                             
         MVC   LINCLI(5),=C'*NEW*'                                              
         BAS   RE,PRNT                                                          
         B     XIT                                                              
         DROP  R2                                                               
         SPACE                                                                  
*              ROUTINE TO CLEAR COMMERCIAL TABLE                                
CLRTAB   NTR1                                                                   
         LA    R2,MAXCOM           R2=MAXIMUM NUMBER OF TABLE ENTRIES           
         L     R3,ACOMTAB          R3=A(COMMERCIAL TABLE)                       
         USING COMTABD,R3                                                       
CLRTAB5  XC    0(COMTABL,R3),0(R3) CLEAR ENTRY                                  
         LA    R3,COMTABL(R3)      BUMP TO NEXT ENTRY                           
         BCT   R2,CLRTAB5          LOOP                                         
         B     XIT                                                              
         DROP  R3                                                               
         SPACE                                                                  
*              ROUTINE TO PRINT A LINE                                          
PRNT     NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)  PRINT A LINE                                    
         B     XIT                                                              
         EJECT                                                                  
*              HEADHOOK ROUTINE FOR BOXES                                       
HOOK     NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   HKX                                                              
         L     R4,ABOX             SET UP BOXES                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES      SET ROWS                                     
         MVI   BOXROWS+3,C'T'      TOP                                          
         MVI   BOXROWS+5,C'M'      MIDDLE                                       
         MVI   BOXROWS+66,C'B'     BOTTOM                                       
         SPACE                                                                  
         MVC   BOXCOLS,SPACES      SET COLUMNS                                  
         LA    R2,BOXCOLS                                                       
         USING LINED,R2            USE PRINT LINE DSECT                         
         MVI   BXL,C'L'                                                         
         MVI   BC0,C'C'                                                         
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BXR,C'R'                                                         
         MVI   BOXYORN,C'Y'        INITIALIZE REMAINING FLDS                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
HKX      B     XIT                                                              
         DROP  R4                                                               
         SPACE                                                                  
*              ROUTINE TO PRINT MIDDLE LINE                                     
BXBOT    NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   BXBOTX                                                           
         L     R4,ABOX             PRINT MIDDLE LINE                            
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         ZIC   RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,PRNT                                                          
BXBOTX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
NOTNOW   MVI   ERROR,ERNOTOLN      ALL ESTIMATES NOT 'NOW'                      
         LA    R2,CONWHENH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
ERRMISS  MVI   ERROR,MISSING       EST REQUIRED(EXCEPT FOR PROGRAMMERS)         
         B     THEEND                                                           
         SPACE 1                                                                
ERRPRNT  MVI   ERROR,INVPRINT      INVALID PRINT FIELD                          
         LA    R2,CONWHENH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,30,C'ESTIMATE REBUILD'                                        
         SSPEC H2,30,16X'BF'                                                    
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SSPEC H5,2,C'ESTIMATE             CLIENT PROD'                         
         SSPEC H5,37,C'COMMERCIALS'                                             
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
REBD     DSECT                                                                  
AOLDEST  DS    A                   ADDRESS OF OLD ESTIMATE RECORD               
ANEWEST  DS    A                   ADDRESS OF NEW ESTIMATE RECORD               
ACOMTAB  DS    A                   ADDRESS OF COMTAB                            
TABCNT   DS    F                   TABLE COUNTER                                
COUNTER  DS    PL4                 LINE COUNTER                                 
DTLCNTR  DS    PL4                 COUNTER OF DETAIL ENTRIES IN TABLE           
ELETYPE  DS    CL1                 ELEMENT TYPE TO ADD                          
ANYCHNG  DS    CL1                 ESTIMATE RECORD CHANGED                      
AIOTYPE  DS    CL1                 O=OLD ESTIMATE/N=NEW ESTIMATE                
*                                                                               
CURSEQ   DS    XL1                 SEQUENCE COUNT FOR NEW ELEMENTS              
CUREST   DS    CL(L'TLESEST)                                                    
*                                                                               
SVCLI    DS    CL(L'TAESCLI)       SAVED CLIENT                                 
SVCCLI   DS    CL(L'TAESCLI)       SAVED COMMERCIAL CLIENT                      
SVPRD    DS    CL(L'TAESPRD)       SAVED PRODUCT                                
SVCPRD   DS    CL(L'TAESPRD)       SAVED COMMERCIAL PRODUCT                     
SVCOM    DS    0CL(L'COMCOM)        SAVED COMMERCIAL INFO                       
SVCOMTYP DS    XL1                                                              
SVCOMID  DS    0CL12                                                            
SVCOMIDD DS    CL8                 COMMERCIAL ID                                
SVCOMI   DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
*                                                                               
SVCIDI   DS    CL(L'COMCIDI)       SAVED COMMERCIAL INFO                        
*                                                                               
AWKIO2   DS    A                                                                
*                                                                               
         DS    0D                  LOCAL WORK IOAREAS                           
WKIO1    DS    XL4000                                                           
WKIO2    DS    XL4000                                                           
*                                                                               
COMNEWQ  EQU   X'FF'               INDICATES NEW ELEMENT                        
MAXCOM   EQU   256                                                              
ESTRECL  EQU   6000                                                             
*                                                                               
TABLNQ   EQU   (MAXCOM*COMTABL)                                                 
MYAREAL  EQU   (TABLNQ+ESTRECL)    COMTAB AND OLDEST                            
         EJECT                                                                  
*              COMMERCIAL TABLE                                                 
*                                                                               
COMTABD  DSECT                                                                  
COMCLI   DS    CL(L'TAESCLI)       CLIENT                                       
COMPRD   DS    CL(L'TAESPRD)       PRODUCT OR ZEROS                             
COMCOM   DS    0CL13               COMMERCIAL INFO                              
COMCTYP  DS    XL1                                                              
COMCID   DS    0CL12                                                            
COMCIDD  DS    CL8                 COMMERCIAL ID                                
COMCIDI  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
COMTYPE  DS    XL1                 ELEMENT TYPE                                 
COMKEYL  EQU   *-COMTABD                                                        
         SPACE                                                                  
COMSEQ   DS    XL1                 SEQUENCE NUMBER                              
COMTABL  EQU   *-COMTABD                                                        
         SPACE 2                                                                
         EJECT                                                                  
*              PRINT LINE DSECT                                                 
*                                                                               
LINED    DSECT                                                                  
BXL      DS    CL1                                                              
LINEST   DS    CL(L'TLESEST)       ESTIMATE                                     
BC0      DS    CL1                                                              
LINCLI   DS    CL(L'TAESCLI)       CLIENT                                       
BC1      DS    CL1                                                              
LINPRD   DS    CL(L'TAESPRD)       PRODUCT OR ZEROS                             
BC2      DS    CL1                                                              
LINCOM   DS    CL44                COMMERCIAL IDS (COM1,COM2,ETC)               
LINLNQ   EQU   *-LINED                                                          
BXR      DS    CL1                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD3D                                                       
         EJECT                                                                  
* TASYSIOD                         MUST FOLLOW LAST SCREEN                      
* DDGENTWA                         MUST FOLLOW LAST SCREEN                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034TAGEND3   08/27/13'                                      
         END                                                                    
